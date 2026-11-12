-- Synthetic CIA test database.
--
-- Mirrors the production structure (data.hauls, data.haul_catch, data.hex_8km,
-- data.hex_20km) with made-up, deterministic data. Contains NO real data.
--
-- Built-in situations the tests rely on:
--   * GPS hauls (10 m buffered tracks) and AREA hauls (whole stat-area squares)
--   * "fringe" vessels fishing alone, so some grid cells have < 3 vessels
--   * a fishing region that extends past the eastern edge of the hex grids,
--     so some hauls are only partly covered (exercises p renormalisation)
--   * catch rows with a species but no species group, and a species group
--     but no species (unspeciated)
--
-- Coordinates are EPSG:3338 (Alaska Albers) metres; the region is arbitrary.

CREATE EXTENSION IF NOT EXISTS postgis;
CREATE SCHEMA IF NOT EXISTS data;

SELECT setseed(0.42);

-- Fishing region: x 0–400 km, y 1000–1300 km. Hex grids stop at x = 380 km.

-- Stat areas (50 km squares). Only used to build AREA haul polygons.
CREATE TABLE data.stat_areas AS
SELECT row_number() OVER (ORDER BY i, j)::int AS stat_area_id,
       geom::geometry(Polygon, 3338)        AS shape
FROM ST_SquareGrid(50000, ST_MakeEnvelope(0, 1000000, 400000, 1300000, 3338));
ALTER TABLE data.stat_areas ADD PRIMARY KEY (stat_area_id);

-- Hex grids. ST_HexagonGrid's size is the edge length; edge = d / sqrt(3)
-- gives a flat-to-flat width of d.
CREATE TABLE data.hex_8km AS
SELECT row_number() OVER (ORDER BY i, j)::int AS hexid,
       geom::geometry(Polygon, 3338)        AS shape
FROM ST_HexagonGrid(8000 / sqrt(3), ST_MakeEnvelope(0, 1000000, 380000, 1300000, 3338));
ALTER TABLE data.hex_8km ADD PRIMARY KEY (hexid);
CREATE INDEX hex_8km_shape_gix ON data.hex_8km USING gist (shape);

CREATE TABLE data.hex_20km AS
SELECT row_number() OVER (ORDER BY i, j)::int AS hexid,
       geom::geometry(Polygon, 3338)        AS shape
FROM ST_HexagonGrid(20000 / sqrt(3), ST_MakeEnvelope(0, 1000000, 380000, 1300000, 3338));
ALTER TABLE data.hex_20km ADD PRIMARY KEY (hexid);
CREATE INDEX hex_20km_shape_gix ON data.hex_20km USING gist (shape);

-- Vessels: 36 "core" vessels around the middle of the region, plus 4 "fringe"
-- vessels near the corners/edges with a small spread, fishing mostly alone.
-- Fringe vessels 38 and 40 sit past the eastern grid edge (x > 380 km).
CREATE TEMP TABLE vessels AS
SELECT v AS vessel_id,
       CASE WHEN v <= 36 THEN 120000 + random() * 160000
            ELSE (ARRAY[15000, 392000, 20000, 395000])[v - 36] END     AS home_x,
       CASE WHEN v <= 36 THEN 1080000 + random() * 140000
            ELSE (ARRAY[1015000, 1020000, 1285000, 1280000])[v - 36] END AS home_y,
       CASE WHEN v <= 36 THEN 60000 ELSE 6000 END                       AS spread,
       CASE WHEN v <= 36 THEN 40 + floor(random() * 40)::int
            ELSE 6 + floor(random() * 6)::int END                       AS n_hauls,
       (ARRAY['TRW', 'TRW', 'HAL', 'POT', 'JIG'])[1 + floor(random() * 5)::int]          AS gear_code,
       (ARRAY['AFA', 'A80', 'CDQ', 'OA', 'IFQ'])[1 + floor(random() * 5)::int]           AS management_program_code,
       (ARRAY['CV', 'CV', 'CP'])[1 + floor(random() * 3)::int]                           AS harvest_sector
FROM generate_series(1, 40) AS v;

CREATE TABLE data.hauls (
    haul_id                 bigint PRIMARY KEY,
    vessel_id               integer  NOT NULL,
    haul_type               text     NOT NULL,
    year                    integer  NOT NULL,
    gear_code               text     NOT NULL,
    management_program_code text     NOT NULL,
    trip_target_code        text     NOT NULL,
    harvest_sector          text     NOT NULL,
    processing_sector       text     NOT NULL,
    haul_polygon            geometry(Geometry, 3338) NOT NULL
);

INSERT INTO data.hauls
WITH h AS (
    SELECT v.*, k,
           random() AS r_type, random() AS r_dx, random() AS r_dy,
           random() AS r_head, random() AS r_len, random() AS r_year,
           random() AS r_tgt,  random() AS r_proc
    FROM vessels v
    CROSS JOIN LATERAL generate_series(1, v.n_hauls) AS k
), p AS (
    SELECT h.*,
           ST_SetSRID(ST_MakePoint(
               LEAST(GREATEST(home_x + (r_dx - 0.5) * 2 * spread, 1000), 399000),
               LEAST(GREATEST(home_y + (r_dy - 0.5) * 2 * spread, 1001000), 1299000)
           ), 3338) AS p0,
           5000 + r_len * 25000 AS len_m
    FROM h
)
SELECT row_number() OVER (ORDER BY vessel_id, k)                      AS haul_id,
       vessel_id,
       CASE WHEN r_type < 0.15 THEN 'AREA'
            WHEN r_type < 0.50 THEN 'VMS'
            WHEN r_type < 0.80 THEN 'ELB'
            ELSE 'OBS' END                                             AS haul_type,
       2018 + floor(r_year * 7)::int                                   AS year,
       gear_code,
       management_program_code,
       (ARRAY['P', 'C', 'B', 'W'])[1 + floor(r_tgt * 4)::int]          AS trip_target_code,
       harvest_sector,
       CASE WHEN harvest_sector = 'CP' THEN 'CP'
            WHEN r_proc < 0.2 THEN 'M' ELSE 'S' END                    AS processing_sector,
       CASE WHEN r_type < 0.15 THEN
                (SELECT s.shape FROM data.stat_areas s
                 WHERE ST_Intersects(s.shape, p0)
                 ORDER BY s.stat_area_id LIMIT 1)
            ELSE
                ST_Buffer(ST_MakeLine(p0, ST_Translate(p0,
                    cos(r_head * 2 * pi()) * len_m,
                    sin(r_head * 2 * pi()) * len_m)), 10)
       END                                                             AS haul_polygon
FROM p;

CREATE INDEX hauls_polygon_gix ON data.hauls USING gist (haul_polygon);
CREATE INDEX hauls_vessel_idx  ON data.hauls (vessel_id);
CREATE INDEX hauls_year_idx    ON data.hauls (year);

-- Species catalogue. Codes are placeholders that mimic the real quirks:
--   POLL, HLBT : species with no species group
--   (NULL, AKFL): unspeciated catch identified only to group
CREATE TEMP TABLE species (species_code text, species_group_code text, target text, p_bycatch float8);
INSERT INTO species VALUES
    ('POLL', NULL,   'P', 0.30),
    ('PCOD', 'RNDF', 'C', 0.35),
    ('ATF',  'AKFL', 'B', 0.25),
    ('KAM',  'AKFL', NULL, 0.15),
    (NULL,   'AKFL', NULL, 0.10),
    ('FSOL', 'FLAT', 'W', 0.25),
    ('RSOL', 'FLAT', NULL, 0.20),
    ('HLBT', NULL,   NULL, 0.15);

CREATE TABLE data.haul_catch (
    haul_id            bigint  NOT NULL REFERENCES data.hauls (haul_id),
    species_code       text,
    species_group_code text,
    weight             numeric NOT NULL,   -- metric tons
    count              numeric NOT NULL
);

INSERT INTO data.haul_catch
SELECT h.haul_id, s.species_code, s.species_group_code,
       round(w.weight::numeric, 3),
       round((w.weight * 1000 / (0.5 + random() * 2.5))::numeric, 0)
FROM data.hauls h
CROSS JOIN species s
CROSS JOIN LATERAL (
    SELECT CASE WHEN s.target = h.trip_target_code
                THEN 5 + random() * 40
                ELSE 0.01 + random() * 2 END AS weight,
           random() AS r
) w
WHERE s.target = h.trip_target_code OR w.r < s.p_bycatch;

CREATE INDEX haul_catch_haul_idx ON data.haul_catch (haul_id);

ANALYZE;

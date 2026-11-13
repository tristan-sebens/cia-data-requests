-- Synthetic spatial filter areas, mirroring the production dbt model
-- (data.management_areas: source, area_code, shape). Contains NO real data.
--
--   * NMFS reporting areas: four 100 km wide strips (610, 620, 630, 640)
--     across the fishing region. Their boundaries fall on stat-area edges, so
--     AREA hauls next to an area only touch it (they must not match).
--   * ADF&G stat areas: the 50 km stat-area squares, current definitions,
--     plus a retired (end_date set) definition of one code that the view
--     must leave out.
--   * The two hex grids.

CREATE TABLE data.nmfs_reporting_areas AS
SELECT (600 + 10 * k)::int AS rep_area,
       ST_Multi(ST_MakeEnvelope((k - 1) * 100000, 1000000, k * 100000, 1300000, 3338))::geometry(MultiPolygon, 3338) AS shape
FROM generate_series(1, 4) AS k;
CREATE INDEX nmfs_reporting_areas_gix ON data.nmfs_reporting_areas USING gist (shape);

CREATE TABLE data.adfg_stat_areas AS
SELECT (500000 + stat_area_id)::text AS code,
       DATE '2000-01-01' AS begin_date,
       NULL::date        AS end_date,
       ST_Multi(shape)::geometry(MultiPolygon, 3338) AS shape
FROM data.stat_areas;
-- A retired definition of code 500001 (shifted far away): must not be used.
INSERT INTO data.adfg_stat_areas
SELECT '500001', DATE '1990-01-01', DATE '1999-12-31',
       ST_Multi(ST_Translate(shape, 0, 2000000))
FROM data.stat_areas WHERE stat_area_id = 1;
CREATE INDEX adfg_stat_areas_gix ON data.adfg_stat_areas USING gist (shape);

-- Same shape as the production model (UNION ALL, constant source per branch),
-- without the row_number() area_id, which the app does not use.
CREATE VIEW data.management_areas AS
SELECT 'ADFG_STAT_AREA' AS source, code::int AS area_code, shape
FROM data.adfg_stat_areas WHERE end_date IS NULL
UNION ALL
SELECT 'NMFS_REPORTING_AREA', rep_area::int, shape FROM data.nmfs_reporting_areas
UNION ALL
SELECT 'HEX_8KM', hexid::int, shape FROM data.hex_8km
UNION ALL
SELECT 'HEX_20KM', hexid::int, shape FROM data.hex_20km;

ANALYZE;

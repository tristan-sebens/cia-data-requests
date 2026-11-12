# Checks that the synthetic database contains the situations later tests rely on.

test_that("tables exist with plausible sizes", {
  con <- test_con()
  expect_gt(q1(con, "SELECT count(*) FROM data.hauls"), 1000)
  expect_gt(q1(con, "SELECT count(*) FROM data.haul_catch"),
            q1(con, "SELECT count(*) FROM data.hauls"))
  expect_gt(q1(con, "SELECT count(*) FROM data.hex_8km"), 100)
  expect_gt(q1(con, "SELECT count(*) FROM data.hex_20km"), 10)
})

test_that("geometries are valid polygons in EPSG:3338", {
  con <- test_con()
  expect_equal(q1(con, "SELECT count(*) FROM data.hauls WHERE NOT ST_IsValid(haul_polygon)"), 0)
  expect_equal(q1(con, "SELECT count(DISTINCT ST_SRID(haul_polygon)) FROM data.hauls"), 1)
  expect_equal(q1(con, "SELECT min(ST_SRID(haul_polygon)) FROM data.hauls"), 3338)
  expect_equal(q1(con, "SELECT count(*) FROM data.hauls
                        WHERE GeometryType(haul_polygon) NOT IN ('POLYGON', 'MULTIPOLYGON')"), 0)
})

test_that("both GPS and AREA hauls are present", {
  con <- test_con()
  types <- DBI::dbGetQuery(con, "SELECT haul_type, count(*) AS n FROM data.hauls GROUP BY 1")
  expect_true("AREA" %in% types$haul_type)
  expect_gt(sum(types$n[types$haul_type != "AREA"]), sum(types$n[types$haul_type == "AREA"]))
})

test_that("some hauls extend past the grid edge (exercises p renormalisation)", {
  con <- test_con()
  n <- q1(con, "
    SELECT count(*) FROM data.hauls h
    WHERE ST_XMax(h.haul_polygon) > (SELECT max(ST_XMax(shape)) FROM data.hex_8km)
      AND ST_XMin(h.haul_polygon) < (SELECT max(ST_XMax(shape)) FROM data.hex_8km)")
  expect_gt(n, 0)
})

test_that("grid cells span both sides of the 3-vessel threshold", {
  con <- test_con()
  counts <- DBI::dbGetQuery(con, "
    SELECT g.hexid, count(DISTINCT h.vessel_id) AS n_vessels
    FROM data.hex_20km g
    JOIN data.hauls h ON ST_Intersects(h.haul_polygon, g.shape)
    GROUP BY g.hexid")
  expect_gt(sum(counts$n_vessels < 3), 0)
  expect_gt(sum(counts$n_vessels >= 3), 0)
})

test_that("species / species group quirks are present", {
  con <- test_con()
  expect_gt(q1(con, "SELECT count(*) FROM data.haul_catch
                     WHERE species_code IS NULL AND species_group_code IS NOT NULL"), 0)
  expect_gt(q1(con, "SELECT count(*) FROM data.haul_catch
                     WHERE species_code IS NOT NULL AND species_group_code IS NULL"), 0)
})

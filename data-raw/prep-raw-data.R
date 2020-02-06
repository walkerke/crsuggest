# Step 1: read in and convert .mdb database
mdb_path <- "data-raw/EPSG_v9_8_6.mdb"

tables <- Hmisc::mdb.get(mdb_path, tables = TRUE)

names(tables) <- tables

table_list <- purrr::map_dfc(tables, ~{
  Hmisc::mdb.get(mdb_path, tables = .x)
})

crs_df <- table_list$`Coordinate Reference System` %>%
  dplyr::select(area_code = AREA.OF.USE.CODE,
                crs_type = COORD.REF.SYS.KIND,
                crs_code = COORD.REF.SYS.CODE,
                crs_name = COORD.REF.SYS.NAME) %>%
  dplyr::mutate_each(as.character)

crs_sf <- sf::st_read("data-raw/EPSG_Polygons.shp") %>%
  dplyr::transmute(area_code = as.character(AREA_CODE)) %>%
  dplyr::left_join(crs_df, by = "area_code")

usethis::use_data(crs_sf, compress = "xz")

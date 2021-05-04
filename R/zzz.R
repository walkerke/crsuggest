.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Using the EPSG Dataset v10.019, a product of the International Association of Oil & Gas Producers. \nPlease view the terms of use at https://epsg.org/terms-of-use.html.")
}

utils::globalVariables(c("crs_gcs", "crs_units", ".", "hausdist", "crs_code",
                         "crs_type", "crs_proj4", ".x", "dist_km"))

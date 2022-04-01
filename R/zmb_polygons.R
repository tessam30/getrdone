
#' Fetch Zambia Polygons
#'
#' @param shpdata path where the VcPepfarPolygons live
#'
#' @return list of spatial data frames
#' @export
#'
#' @examples
#' \dontrun{
#' shpdata <- glamr::si_path("path_vector")
#' zmb_polygons(shpdata)
#' }
#'
zmb_polygons <- function(shpdata = shpdata){

  cntry <- "Zambia"
  spdf_pepfar <- gisr::get_vcpolygons(path = shpdata, name = "VcPepfarPolygons.shp")

  zmb_geo <- purrr::map(3:5, ~spdf_pepfar %>%
                          gisr::extract_boundaries(country = cntry,
                                             level = .x))

  names(zmb_geo) <- list("adm0", "snu1", "psnu")
  return(zmb_geo)
}


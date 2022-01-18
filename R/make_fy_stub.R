#' Return a fiscaly year stub
#'
#' @param df A dataframe containing a period column and fiscal_year variable
#'
#' @return
#' @export
#'
#' @examples
make_fy_stub <- function(df){
  df %>%
  dplyr::mutate(fy_stub = dplyr::case_when(
    period != "targets" ~ stringr::str_c("FY", stringr::str_extract(fiscal_year, ".{2}$"), "Q", stringr::str_extract(period, ".{1}$")),
    period == "targets" ~ stringr::str_c("FY", stringr::str_extract(fiscal_year, ".{2}$"), " Targets"),
    TRUE ~ NA_character_
    )
  )
}

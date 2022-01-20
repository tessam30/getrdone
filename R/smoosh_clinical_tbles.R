

#' Combines tables
#' @description Combines mechanism indicators and all USAID indicators, reshapes
#'
#' @param mech_tbl
#' @param usaid_tbl
#'
#' @return df of joined tables
#' @export
#'
#' @examples
smoosh_clinical_tbls <- function(mech_tbl, usaid_tbl){

  last_val <- length(mech_tbl)

  zmb_qtrly_tble <-
    mech_tbl %>%
    dplyr::bind_rows(usaid_tbl) %>%
    tidyr::pivot_longer(5:last_val,
                 values_to = "value",
                 names_to = "indicator") %>%
    dplyr::mutate(fy_stub = dplyr::case_when(
      period != "targets" ~ stringr::str_c("FY", stringr::str_extract(fiscal_year, ".{2}$"), "Q", stringr::str_extract(period, ".{1}$")),
      period == "targets" ~ stringr::str_c("FY", stringr::str_extract(fiscal_year, ".{2}$"), " Targets"),
      TRUE ~ NA_character_
    )
    ) %>%
    dplyr::mutate(remove_flag = dplyr::case_when(
      fiscal_year == fy - 1 & period == "targets" ~ 1,
      TRUE ~ 0
    )) %>%
    # dplyr::filter(remove_flag != 1) %>%
    dplyr::select(-fiscal_year, -period, -remove_flag) %>%
    dplyr::rename(period = fy_stub) %>%
    tidyr::pivot_wider(names_from = period,
                       values_from = value)

}

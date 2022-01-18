smoosh_clinical_tbls <- function(list_mech, zmb_usaid_tbl){

  zmb_qtrly_tble <-
    purrr:::reduce(list_mech, dplyr::left_join) %>%
    dplyr::bind_rows(zmb_usaid_tbl) %>%
    tidyr::pivot_longer(HTS_INDEX:TB_PREV_COVERAGE,
                 values_to = "value",
                 names_to = "indicator",
                 values_drop_na = TRUE) %>%
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
    dplyr::filter(remove_flag != 1) %>%
    dplyr::select(-fiscal_year, -period) %>%
    dplyr::rename(period = fy_stub) %>%
    tidyr::pivot_wider(names_from = period,
                values_from = value)

}

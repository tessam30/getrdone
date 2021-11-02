smoosh_clinical_tbls <- function(){

  zmb_qtrly_tble <-
    reduce(list(tst_all, tx_all, vl_all, prev_all), left_join) %>%
    bind_rows(zmb_usaid_tbl) %>%
    pivot_longer(HTS_INDEX:TB_PREV_COVERAGE,
                 values_to = "value",
                 names_to = "indicator",
                 values_drop_na = TRUE) %>%
    mutate(fy_stub = case_when(
      period != "targets" ~ str_c("FY", str_extract(fiscal_year, ".{2}$"), "Q", str_extract(period, ".{1}$")),
      period == "targets" ~ str_c("FY", str_extract(fiscal_year, ".{2}$"), " Targets"),
      TRUE ~ NA_character_
    )
    ) %>%
    mutate(remove_flag = case_when(
      fiscal_year == fy - 1 & period == "targets" ~ 1,
      TRUE ~ 0
    )) %>%
    filter(remove_flag != 1) %>%
    select(-fiscal_year, -period) %>%
    rename(period = fy_stub) %>%
    pivot_wider(names_from = period,
                values_from = value)

}

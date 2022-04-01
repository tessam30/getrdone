#' Return viral load suppression/coverage table
#'
#' @param df A genie extract or MSD data frame
#'
#' @return
#' @export
#'
#' @examples
return_vls_tbl <- function(df) {

  vls_peds <-
    df %>%
    dplyr::filter(indicator %in% "TX_PVLS",
           standardizeddisaggregate == "Age/Sex/Indication/HIVStatus",
           trendscoarse == "<15") %>%
    sum_indics_peds(., "TX_PVLS") %>%
    dplyr::rename(TX_PVLS_N_PEDS = TX_PVLS_PEDS) %>%
    dplyr::left_join(return_tx_tbl(df) %>% dplyr::select(1:4, TX_CURR_PEDS))

  vls_df <-
    df %>%
    dplyr::filter(indicator %in% c("TX_PVLS", "TX_CURR"),
           standardizeddisaggregate == "Age/Sex/Indication/HIVStatus") %>%
    sum_indics() %>%
    dplyr::rename(TX_PVLS_N = TX_PVLS) %>%
    dplyr::left_join(return_tx_tbl(df) %>% dplyr::select(1:4, TX_CURR))

  # Need a time variable with targets omitted so we can use lag operator
  vls_df_pd <-
    vls_df %>%
    dplyr::left_join(vls_peds) %>%
    make_fy_stub()

  vls_df_tgt <- vls_df_pd %>% dplyr::filter(period == "targets")

  vls_df_all <- vls_df_pd %>%
    dplyr::filter(period != "targets") %>%
    dplyr::arrange(mech_code, fy_stub) %>%
    dplyr::group_by(mech_code) %>%
    dplyr::mutate(TX_PVLS_COVERAGE = calc_pct(TX_PVLS_D, dplyr::lag(TX_CURR, n = 2)),
           TX_PVLS_COVERAGE_PEDS = calc_pct(TX_PVLS_D_PEDS, dplyr::lag(TX_CURR_PEDS, n = 2)),
           TX_PVLS_SUPPRESSION = calc_pct(TX_PVLS_N, TX_PVLS_D),
           TX_PVLS_SUPPRESSION_PEDS = calc_pct(TX_PVLS_N_PEDS, TX_PVLS_D_PEDS)) %>%
    dplyr::bind_rows(vls_df_tgt)%>%
    dplyr::select(-c(TX_CURR, TX_CURR_PEDS, fy_stub))

  return(vls_df_all)
}

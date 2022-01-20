

#' Return prevention indicators
#'
#' @param df A genie extract or MSD data frame
#'
#' @return
#' @export
#'
#' @examples
return_prev_tbl <- function(df){

  prev_indic <- c("PrEP_CT", "PrEP_NEW", "PrEP_CURR", "TB_PREV", "VMMC_CIRC", "TB_STAT")
  vmmc_indic <- c("VMMC_CIRC")

  vmmc <-
    df %>%
    dplyr::filter(indicator %in% vmmc_indic,
           trendssemifine %in% c("15-19", "20-24", "25-29"),
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus")) %>%
    sum_indics() %>%
    dplyr::rename(`VMMC_CIRC_15_29`= VMMC_CIRC)

  prev_df <-
    df %>%
    dplyr::filter(indicator %in% prev_indic,
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
    sum_indics() %>%
    dplyr::rename(TB_PREV_N = TB_PREV)

  prev_all_df <-
    prev_df %>%
    dplyr::left_join(., vmmc) %>%
    dplyr::mutate(`VMMC_CIRC_15-29_SHARE` = calc_pct(`VMMC_CIRC_15_29`,VMMC_CIRC),
           TB_PREV_COVERAGE = calc_pct(TB_PREV_N, TB_PREV_D)) %>%
    dplyr::select(-TB_STAT_D)

  return(prev_all_df)

}

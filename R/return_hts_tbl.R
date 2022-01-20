#' Return custom testing table
#'
#' @param df A genie extract or MSD data frame
#'
#' @return testing indicator summary spread wide
#' @export
#'
#' @examples
return_hts_tbl <- function(df) {

  # CW indicators
  tst_index_peds <- c("HTS_INDEX", "HTS_INDEX_NEWPOS")
  tst_peds <- c("HTS_TST", "HTS_TST_POS")

  tst <- c("HTS_TST", "HTS_TST_POS", "HTS_INDEX", "HTS_INDEX_NEWPOS", "HTS_SELF", "HTS_RECENT",
           "PMTCT_STAT", "PMTCT_STAT_POS",
           "PMTCT_EID", "PMTCT_HEI_POS")


  # First grab peds index indicators
  hts_index_peds <-
    df %>%
    dplyr::filter(indicator %in% tst_index_peds,
           trendssemifine %in% c("<01", "01-09", "10-14"),
           standardizeddisaggregate %in% c("Age/Sex/Result", "4:Age/Sex/Result"),
           modality %in% c("Index", "IndexMod")) %>%
    sum_indics_peds(., tst_index_peds) %>%
    dplyr::mutate(HTS_INDEX_POSITIVITY_PEDS = calc_pct(HTS_INDEX_NEWPOS_PEDS, HTS_INDEX_PEDS)) %>%
    dplyr::rename(HTS_INDEX_POS_PEDS = HTS_INDEX_NEWPOS_PEDS) %>%
    dplyr::ungroup()

  hts_peds <-
    df %>%
    dplyr::filter(indicator %in% tst_peds,
           trendssemifine %in% c("<01", "01-09", "10-14"),
           standardizeddisaggregate %in% c("Modality/Age/Sex/Result")) %>%
    sum_indics_peds(., tst_peds)  %>%
    dplyr::mutate(HTS_TST_POSITIVITY_PEDS = calc_pct(HTS_TST_POS_PEDS, HTS_TST_PEDS))

  # Now get the rest of index testing for all ages
  hts_tst <-
    df %>%
    dplyr::filter(indicator %in% tst,
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
    sum_indics() %>%
    dplyr::rename(HTS_INDEX_POS = HTS_INDEX_NEWPOS,
           PMTCT_STAT_N = PMTCT_STAT) %>%
    dplyr::mutate(PMTCT_STAT_COVERAGE_PCT = calc_pct(PMTCT_STAT_N, PMTCT_STAT_D),
           PMTCT_STAT_POSITIVITY = calc_pct(PMTCT_STAT_POS, PMTCT_STAT_N),
           HTS_INDEX_POSITIVITY = calc_pct(HTS_INDEX_POS, HTS_INDEX),
           HTS_TST_POSITIVITY = calc_pct(HTS_TST_POS, HTS_TST)
    )

  hts_df <- purrr::reduce(list(hts_tst, hts_index_peds, hts_peds), dplyr::left_join)
  hts_df <- hts_df %>%  dplyr::select(-PMTCT_EID_D)

  return(hts_df)
}

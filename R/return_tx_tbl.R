

#' Returns the formatted treatment table
#'
#' @param df A genie or MSD data frame
#'
#' @return A formatted data frame with calculations done
#' @export
#'
#' @examples
return_tx_tbl <- function(df) {

  tx_peds <- c("TX_CURR", "TX_NEW")
  tx <- c("PMTCT_ART", "TB_ART", "TX_CURR", "TX_ML", "TX_NEW", "TX_RTT", "TX_TB")

  tx_peds_df <-
    df %>%
    dplyr::filter(indicator %in% tx_peds,
           trendssemifine %in% c("<01", "01-09", "10-14"),
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus")) %>%
    sum_indics_peds(., tx_peds)

  tx_df <-
    df %>%
    dplyr::filter(indicator %in% tx,
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
    sum_indics() %>%
    dplyr::rename(TX_TB_N = TX_TB)

  tx_df_all <-
    tx_df %>%
    dplyr::left_join(tx_peds_df) %>%
    dplyr::select(-PMTCT_ART_D, -TB_ART_D)

  return(tx_df_all)
}

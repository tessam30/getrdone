
#' Return key population indicator table
#'
#' @param df A genie extract or MSD data frame
#'
#' @return a data frame of KP indicators
#' @export
#'
#' @examples
return_kp_tbl <- function(df, fy = fy){

  # pull the table for kp_prev indicators needed
  kp_prev_df <- df %>%
    dplyr::filter(indicator %in% c("KP_PREV"),
           standardizeddisaggregate %in% c("KeyPop"),
           fiscal_year == {{fy}},
           mech_name != "EQUIP (18304)") %>%
    dplyr::mutate(categoryoptioncomboname = stringr::str_remove_all(categoryoptioncomboname, "\\(|\\)| not SW")) %>%
    dplyr::mutate(categoryoptioncomboname = stringr::str_remove(categoryoptioncomboname, " and other enclosed settings"),
           standardizeddisaggregate = paste("KeyPop", categoryoptioncomboname, sep = "_")) %>%
    dplyr::group_by(standardizeddisaggregate, indicator, mech_name, categoryoptioncomboname, fiscal_year) %>%
    dplyr::summarise(dplyr::across(tidyselect::matches("qtr|targ"), sum, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(standardizeddisaggregate, mech_name) %>%
    tidyr::pivot_longer(targets:qtr4, names_to = "period") %>%
    tidyr::spread(period, value) %>%
    dplyr::arrange(mech_name)

kp_prev_df <- kp_prev_df %>%
  dplyr::bind_rows(kp_prev_df %>% dplyr::mutate(standardizeddisaggregate = "KeyPop") %>%
                     dplyr::group_by(mech_name, standardizeddisaggregate, fiscal_year) %>%
                     dplyr::mutate(dplyr::across(c(qtr1:targets), sum, na.rm = T))) %>%
  dplyr::group_by(standardizeddisaggregate, mech_name, fiscal_year, indicator) %>%
  dplyr::summarise(dplyr::across(c(qtr1:targets), mean))

return(kp_prev_df)

}

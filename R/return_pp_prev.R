#' Return prevention indicator
#'
#' @param df A genie extract or MSD data frame
#'
#' @return pp_prev indicator table
#' @export
#'
#' @examples
return_pp_prev <- function(df, fy = fy){

  pp_prev_df <- df %>%
    dplyr::filter(indicator == "PP_PREV", fiscal_year == {{fy}}, standardizeddisaggregate %in% c("Age/Sex", "PopulationPriorityType")) %>%
    sum_special() %>%
    dplyr::mutate(drop_flag = dplyr::case_when(
      stringr::str_detect(standardizeddisaggregate, "PopulationPriorityType") & stringr::str_detect(mech_name, "Stop") ~ 1,
      TRUE ~ 0
    ))

  return(pp_prev_df)
}

#' Return prevention indicator
#'
#' @param df A genie extract or MSD data frame
#'
#' @return pp_prev indicator table
#' @export
#'
#' @examples
return_pp_prev <- function(df){

  pp_prev_df <- df %>%
    filter(indicator == "PP_PREV", fiscal_year == fy, standardizeddisaggregate %in% c("Age/Sex", "PopulationPriorityType")) %>%
    sum_special() %>%
    mutate(drop_flag = case_when(
      str_detect(standardizeddisaggregate, "PopulationPriorityType") & str_detect(mech_name, "Stop") ~ 1,
      TRUE ~ 0
    ))

  return(pp_prev_df)
}

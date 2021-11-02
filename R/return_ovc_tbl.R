
#' Return a table of ovc indicators
#'
#' @param df A genie extract or MSD data frame
#'
#' @return OVC indictor table
#' @export
#'
#' @examples
return_ovc_tbl <- function(df){

ovc_df <- df %>%
  filter(indicator %in% c("GEND_GBV","OVC_SERV", "OVC_HIVSTAT"),
         standardizeddisaggregate %ni% c("Total Numerator", "Total Denominator", "ViolenceServiceType",
                                         "TransferExit", "ProgramStatus"),
         fiscal_year == fy) %>%
  sum_special() %>%
  mutate(drop_flag = case_when(
    indicator == "OVC_SERV" & str_detect(mech_name, "Empowered") & str_detect(standardizeddisaggregate, "Prevent") ~ 1,
    TRUE ~ 0
  ))
return(ovc_df)

}

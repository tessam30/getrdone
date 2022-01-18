
#' Return a table of ovc indicators
#'
#' @param df A genie extract or MSD data frame
#'
#' @return OVC indicator table
#' @export
#'
#' @examples
return_ovc_tbl <- function(df){

ovc_df <- df %>%
  dplyr::filter(indicator %in% c("GEND_GBV","OVC_SERV", "OVC_HIVSTAT"),
         standardizeddisaggregate %ni% c("Total Numerator", "Total Denominator", "ViolenceServiceType",
                                         "TransferExit", "ProgramStatus"),
         fiscal_year == fy) %>%
  sum_special() %>%
  dplyr::mutate(drop_flag = dplyr::case_when(
    indicator == "OVC_SERV" & stringr::str_detect(mech_name, "Empowered") & stringr::str_detect(standardizeddisaggregate, "Prevent") ~ 1,
    TRUE ~ 0
  )
  )
return(ovc_df)

}

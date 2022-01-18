

#' Query indicators
#'
#' @param x An indicator (string) to be queried
#' @param df A dataframe containing values from the custom template
#'
#' @return returns a printed count of given indicators
#' @export
#'
#' @examples
return_indic <- function(x, df){
  df %>%
    dlpyr::mutate(indicator = sub(x, "", indicator)) %>%
    dlpyr::count(indicator) %>%
    prinf()
}


# Generic function to calculate aggregates
#' Summary function for peds indicators
#'
#' @param df
#' @param indics an object that contains the peds indicators needed.
#'
#' @return A data frame that has been munged, collapsed, and reshaped
#' @export
#'
#' @examples
sum_indics_peds <- function(df, indics) {
  df %>%
    dplyr::mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>%
    dplyr::group_by(fiscal_year, indicator, mech_code, mech_name) %>%
    dplyr::summarise(dplyr::across(tidyselect::matches("qtr|targ"), sum, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(indicator = forcats::fct_relevel(paste0(indicator, "_PEDS"), paste0({{indics}}, "_PEDS"))) %>%
    dplyr::arrange(indicator, mech_name) %>%
    tidyr::pivot_longer(targets:qtr4, names_to = "period") %>%
    tidyr::spread(indicator, value)
}


#' Summary function for standard indicators
#'
#' @param df A genie extract or MSD
#'
#' @return A data frame that has been munged, collapsed, and reshaped
#' @export
#'
#' @examples
sum_indics <- function(df) {
  df %>%
    dplyr::mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>%
    dplyr::group_by(fiscal_year, indicator, mech_code, mech_name) %>%
    dplyr::summarise(dplyr::across(tidyselect::matches("qtr|targ"), sum, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(indicator, mech_name) %>%
    tidyr::pivot_longer(targets:qtr4, names_to = "period") %>%
    tidyr::spread(indicator, value)
}



# No Inf in calculations or NaNs
#' Calculate positive share
#'
#' @param x numerator for calculation
#' @param y denominator for calculation
#'
#' @return calculated share
#' @export
#'
#' @examples
calc_pct <- function(x, y) {
  ifelse(y > 0.000, (x / y), NA_real_)
}





#' Print indicator summary
#'
#' @param x program area to return
#'
#' @return
#' @export
#'
#' @examples
show_mech_indics <- function(x = "Prevention") {
  clin_cw %>%
    dplyr::filter(stringr::str_detect(`Program Area`, {{x}})) %>%
    dplyr::count(indicator_cw, IM) %>%
    tidyr::spread(IM, n) %>%
    prinf()
}


#' Sum prevention and ovc indicators
#'
#' @param df data frame of prevention and ovc indicators
#'
#' @return
#' @export
#'
#' @examples
sum_special <- function(df) {
  df %>%
    dplyr::group_by(fiscal_year, mech_name, indicator, standardizeddisaggregate) %>%
    dplyr::summarise(dplyr::across(tidyselect::matches("qtr|targ"), sum, na.rm = T)) %>%
    dplyr::ungroup()
}



#' Create dynamic column names based on objects
#'
#' @param stub The text at the end of the name (Results, Targets, Achievement)
#'
#' @return a string that can be used as a column name
#' @export
#'
#' @examples
name_column <- function(stub){
  glue::glue("FY{substr(pd, 3, 4)} {stub}")
}


#' Extract the mech_code into a new column
#'
#' @param df
#'
#' @return a column with just the mech_code
#' @export
#'
#' @examples
extract_mech_code <- function(df){
  df %>%
  mutate(mech_code = as.numeric(gsub("[^[:digit:]]+", "", IM)))
}

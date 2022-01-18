# Grab template information


#Fetches the 1st sheet template

#' Title
#'
#' @return data frame of the clinical and prevention template provided
#' @export
#'
#' @examples
fetch_clinical <- function(){
  df <- googlesheets4::read_sheet("11hnE4fInDmm4mZCxxREz77yhgRm5cj2KDnNFRkn_WP4", sheet = "Clinical and Prevention", skip = 2)
  return(df)
}



# Fetches the 2nd sheet template
#' Title
#'
#' @return data frame of the prevention and ovc sheet with mech codes added as a column
#' @export
#'
#' @examples
fetch_prev_ovc <- function(){
  df <- googlesheets4::read_sheet("11hnE4fInDmm4mZCxxREz77yhgRm5cj2KDnNFRkn_WP4", sheet = "Prevention and OVC", skip = 2) %>%
    dplyr::mutate(mech_code = as.numeric(gsub("[^[:digit:]]+", "", IM)))
  return(df)
}




# Cleans the clinical indicators so they can be matched with template
#' Title
#'
#' @param df A data frame containing the alternative indicator names used by the Mission
#'
#' @return df A cleaned data frame of idnicator names that can be replicated and merged
#' @export
#'
#' @examples
create_clin_cw <- function(df) {
  clin_cw <-
    df %>%
    dplyr:: mutate(indicator_cw = dplyr::case_when(
      stringr::str_detect(indicator, " Coverage \\(Under 15\\)") ~ stringr::str_replace_all(indicator, " Coverage \\(Under 15\\)", "_COVERAGE_PEDS"),
      stringr::str_detect(indicator, " Suppression \\(Under 15\\)") ~ stringr::str_replace_all(indicator, " Suppression \\(Under 15\\)", "_SUPPRESSION_PEDS"),
      stringr::str_detect(indicator, " \\(Under 15\\)") ~ stringr::str_replace_all(indicator, " \\(Under 15\\)", "_PEDS"),
      stringr::str_detect(indicator, " \\(N\\) Under 15") ~ stringr::str_replace_all(indicator, " \\(N\\) Under 15", "_N_PEDS"),
      stringr::str_detect(indicator, " \\(D\\) Under 15") ~ stringr::str_replace_all(indicator, " \\(D\\) Under 15", "_D_PEDS"),
      stringr::str_detect(indicator, " \\(N\\)") ~ stringr::str_replace_all(indicator, " \\(N\\)", "_N"),
      stringr::str_detect(indicator, " \\(D\\)") ~ stringr::str_replace_all(indicator, " \\(D\\)", "_D"),
      stringr::str_detect(indicator, " Coverage") ~ stringr::str_replace_all(indicator, " Coverage", "_COVERAGE"),
      stringr::str_detect(indicator, " Suppression") ~ stringr::str_replace_all(indicator, " Suppression", "_SUPPRESSION"),
      stringr::str_detect(indicator, "_Coverage \\(\\%\\)") ~ stringr::str_replace_all(indicator, "_Coverage \\(\\%\\)", "_COVERAGE_PCT"),
      stringr::str_detect(indicator, "_Positivity") ~ stringr::str_replace_all(indicator, "_Positivity", "_POSITIVITY"),
      stringr::str_detect(indicator, "Percent of VMMC 15-29") ~ stringr::str_replace_all(indicator, "Percent of VMMC 15-29 against total VMMC", "VMMC_CIRC_15-29_SHARE"),
      TRUE ~ indicator)
    ) %>%
    dplyr::select(IM:Trend, indicator_cw)
  return(clin_cw)
}

#' Format mechanism names
#'
#' @param df A genie or MSD data frame
#'
#' @return names formatted to Mission template
#' @export
#'
#' @examples
format_mech_names <- function(df){
  df %>%
    dplyr::mutate(
      mech_name = dplyr::case_when(
        mech_name == "Local Treatment Partner" & mech_code == "18304" ~ "EQUIP",
        TRUE ~ mech_name),
      mech_name = dplyr::case_when(
        stringr::str_detect(mech_name, "DISCOVER") ~ "DISCOVER-H (17399)",
        stringr::str_detect(mech_name, "EQUIP") ~ "EQUIP (18304)",
        stringr::str_detect(mech_name, "Eradicate") ~ "Eradicate TB (17400)",
        stringr::str_detect(mech_name, "SAFE") ~ "SAFE (17413)",
        stringr::str_detect(mech_name, "GBV") ~ "Stop GBV (18487)",
        stringr::str_detect(mech_name, "Open Doors") ~ "USAID Open Doors (17422)",
        stringr::str_detect(mech_name, "Zambia") ~ "Z-CHPP (17410)",
        TRUE ~ paste0(mech_name, " (", mech_code, ")")
      )
    ) %>%
    dplyr::filter(fundingagency == "USAID")
}


# Mech list that is currently used for clinical indicators


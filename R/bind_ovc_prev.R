# Combine ovc and prev indicators

#' Title
#'
#' @param df1 one of three ovc and prev data frames (kp_prev_df)
#' @param df2 two of three ovc and prev data frames (ovc_df)
#' @param df3 three of three ovc and prev data frames (pp_prev_df)
#'
#' @return
#' @export
#'
#' @examples
bind_ovc_prev <- function(df1 = kp_prev_df, df2 = ovc_df, df3 = pp_prev_df){

prev_ovc_tbl <-
  dplyr::bind_rows(df1, df2, df3) %>%
  dplyr::mutate(row_type = dplyr::case_when(
    indicator %in% snapshot_ind ~ "snapshot",
    TRUE ~ "cumulative"
  ),
  cumulative = dplyr::case_when(
    row_type == "cumulative" ~ rowSums(dplyr::across(tidyselect::contains("qtr"))),
    TRUE ~ qtr4)
  ) %>%
  dplyr::filter(is.na(drop_flag) | drop_flag == 0) %>%
  dplyr::select(-drop_flag) %>%
  dplyr::mutate(mech_code = as.numeric(gsub("[^[:digit:]]+", "", mech_name))) %>%
  dplyr::relocate(targets, .after = cumulative) %>%
  dplyr::relocate(row_type, .after = last_col()) %>%
  dplyr::mutate(`% Achievement` = ifelse(targets > 0 & cumulative > 0, cumulative/targets, NA_real_), .after = targets) %>%
  dplyr::rename(disag = standardizeddisaggregate)

  return(prev_ovc_tbl)

}

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
  bind_rows(df1, df2, df3) %>%
  mutate(row_type = case_when(
    indicator %in% snapshot_ind ~ "snapshot",
    TRUE ~ "cumulative"
  ),
  cumulative = case_when(
    row_type == "cumulative" ~ rowSums(across(contains("qtr"))),
    TRUE ~ qtr4)
  ) %>%
  filter(is.na(drop_flag) | drop_flag == 0) %>%
  select(-drop_flag) %>%
  mutate(mech_code = as.numeric(gsub("[^[:digit:]]+", "", mech_name))) %>%
  relocate(targets, .after = cumulative) %>%
  relocate(row_type, .after = last_col()) %>%
  mutate(`% Achievement` = if_else(targets > 0 & cumulative > 0, cumulative/targets, NA_real_), .after = targets) %>%
  rename(disag = standardizeddisaggregate)

  return(prev_ovc_tbl)

}

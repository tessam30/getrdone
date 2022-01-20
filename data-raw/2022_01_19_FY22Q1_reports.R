# Purpose: Create performance and targets from Genie For Zambia
# Author: Tim Essam | SI
# Date: 2022-01-19
# Notes: FY22 Q1 Review for Mission


# LIBRARIES -----------------------------------------------------------------
library(glitr)
library(glamr)
library(gisr)
library(gophr)
library(tidyverse)
library(scales)
library(extrafont)
library(tidytext)
library(ggtext)
library(here)
library(readxl)
library(glue)
library(googlesheets4)


# GLOBALS -----------------------------------------------------------------

  load_secrets()
  merdata <- file.path(glamr::si_path("path_msd"), "Genie")

  file_genie <- return_latest(folderpath = merdata,
                              pattern = "Genie-PSNUByIMs-MultipleOUs-Daily-2022-01-20")
                              #pattern = "Genie-PSNUByIMs-MultipleOUs-Daily-")


# Grab metadata
  pd <- source_info(file_genie, return = "period")
  fy <- source_info(file_genie, return = "fiscal_year")
  qtr <- source_info(file_genie, return = "quarter")



#  PREP TEMPLATE DATA AND LOAD -----------------------------------------------------


# Show mech list for FY22 Clinical Parnters
  mech_list <- c(mech_list_22, "18304")

# Fetch template info
  clin_cw <- fetch_clinical() %>% create_clin_cw()

# prev_ovc_df <- fetch_prev_ovc()
#
# prev_ovc_mechs <-
#   fetch_prev_ovc() %>%
#   distinct(IM) %>%
#   extract_mech_code() %>%
#   pull(mech_code)


# Munge core data
df_mechs <-
  gophr::read_msd(file_genie) %>%
  filter(fiscal_year %in% c(fy-1, fy)) %>%
  format_mech_names() %>%
  filter(mech_code %in% mech_list)

df_usaid <-
  read_msd(file_genie) %>%
  filter(fiscal_year %in% c(fy-1, fy),
         fundingagency == "USAID") %>%
  mutate(mech_name = "ALL USAID",
         mech_code = "99999",
         primeparter = "ALL USAID")

clin_cw %>% distinct(indicator, indicator_cw) %>% prinf()
clin_cw %>% distinct(IM)



# MUNGE TESTING -----------------------------------------------------------

# Getting all the testing indicators first
  show_mech_indics("Testing")

  tst_all <- return_hts_tbl(df_mechs)
  tst_usaid <- return_hts_tbl(df_usaid)



# # MUNGE TREATMENT -------------------------------------------------------
# Treatment indicators needed
  show_mech_indics("Treatment")

  tx_all <- return_tx_tbl(df_mechs)
  tx_usaid <- return_tx_tbl(df_usaid)

  vl_all    <- return_vls_tbl(df_mechs)
  vl_usaid <- return_vls_tbl(df_usaid)

# MUNGE PREVENTION --------------------------------------------------------

# PREVENTION INDICATOR
  show_mech_indics("Prevention")

  prev_all <-   return_prev_tbl(df_mechs)
  prev_usaid <- return_prev_tbl(df_usaid)

# OVC and prevention
# df_ovc_mechs <-
#   gophr::read_msd(file_genie) %>%
#   filter(fiscal_year %in% c(fy),
#          mech_code %in% prev_ovc_mechs) %>%
#   format_mech_names()
#
# # KP
# kp_prev_df <- return_kp_tbl(df_ovc_mechs)
# ovc_df <- return_ovc_tbl(df_ovc_mechs)
# pp_prev_df <- return_pp_prev(df_ovc_mechs)

# Bind it all together
# df <-
#   fetch_prev_ovc() %>%
#   select(IM, indicator = `Technical Area`, disag = `Disaggregation Type`, Q1:Q4, mech_code) %>%
#   left_join(bind_ovc_prev()) %>%
#   select(-c(fiscal_year, mech_code, mech_name))


# COMBINE CLINICAL TABLES ------------------------------------------------

# Bring Clinical tables together
  # Combine all the USAID indicators wide
  list_usaid <- list(tst_usaid, tx_usaid, vl_usaid, prev_usaid)
  list_mech <- list(tst_all, tx_all, vl_all, prev_all)
  # list_mech <-  mget(ls(pattern = "_all"))

  zmb_usaid_tbl <- purrr::reduce(list_usaid, left_join)
  zmb_mech_tbl  <- purrr::reduce(list_mech, left_join)


# Combine all indicators and then append (row bind) the USAID indicators
# Shooting for 230 (229 really rows)
zmb_qtrly_tble <- smoosh_clinical_tbls(zmb_mech_tbl, zmb_usaid_tbl)


# CHECK ALINGMENT TO CROSSWALK TABLE ------------------------------------------------------------

  # Take a look at what isn't aligning
  zmb_qtrly_tble %>%
    anti_join(., clin_cw,
              by = c("mech_name" = "IM",
                     "indicator" = "indicator_cw" )) %>%
    View()

  clin_cw %>%
    anti_join(., zmb_qtrly_tble, by = c("IM" = "mech_name",
                                        "indicator_cw" = "indicator")) %>%
    View()

  setdiff(clin_cw %>% distinct(indicator_cw) %>% rename(indicator = indicator_cw),
          zmb_qtrly_tble %>% distinct(indicator))

  setdiff(zmb_qtrly_tble %>% distinct(indicator),
          clin_cw %>%
            distinct(indicator_cw) %>%
            rename(indicator = indicator_cw))


  # Do a full join on data
  df_rv <-
    clin_cw %>%
    full_join(., zmb_qtrly_tble,
              by = c("IM" = "mech_name",
                     "indicator_cw" = "indicator" )) %>%
    filter(!is.na(mech_code))


# FORMAT TABLE ---------------------------------------------------------------

  df_rv <-
    df_rv %>%
    mutate(`FY21 Results` = case_when(
      str_detect(indicator_cw, "(POSITIVITY|COVERAGE|SUPPRESSION|SHARE)") ~ NA_real_,
      str_detect(indicator_cw, "(TX_CURR|TX_TB|TX_PVLS_N|TX_PVLS_D)") ~ FY21Q4,
      TRUE ~ FY21Q1 + FY21Q2 + FY21Q3 + FY21Q4),
      `FY21 Achievement` = calc_pct(`FY21 Results`, `FY21 Targets`)
      ) %>%
    mutate(`FY22 Results` = case_when(
      str_detect(indicator_cw, "(POSITIVITY|COVERAGE|SUPPRESSION|SHARE)") ~ NA_real_,
      str_detect(indicator_cw, "(TX_CURR|TX_TB|TX_PVLS_N|TX_PVLS_D)") ~ FY22Q1,
      TRUE ~ FY22Q1),
      `FY22 Achievement` = calc_pct(`FY22 Results`, `FY22 Targets`)
    ) %>%
    mutate(row_type = case_when(
      str_detect(indicator_cw, "(POSITIVITY|COVERAGE|SUPPRESSION|SHARE)") ~ "percent",
      str_detect(indicator_cw, "(TX_CURR|TX_TB|TX_PVLS_N|TX_PVLS_D)") ~ "snapshot",
      TRUE ~ "cumulative")
    ) %>%
    relocate(`FY21 Results`, .after = `FY21 Targets`) %>%
    relocate(`FY21 Achievement`, .after = `FY21 Results`)



  df_rv_formatted <-
    df_rv_fmt %>%
    mutate(across(c(FY21Q1:`FY21 Results`, `FY22Q1`:`FY22 Results`), ~case_when(
      str_detect(indicator_cw, "(POSITIVITY|COVERAGE|SUPPRESSION|SHARE)") ~format(round(.x, 2), nsmall = 2),
      TRUE ~ format(round(.x, 0), nsmall = 0)
      )
    ),
    mutate(across(matches("Achievement"),
           ~format(round(.x, 2), nsmall = 2))
           )
    )

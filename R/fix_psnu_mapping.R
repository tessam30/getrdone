
#' Fix PSNU Mapping to SNU1
#' @description Prior to FY22, three PSNUs were mapped to different SNU1s.
#' This function remaps these psnus to the correct snu1 and alters the uids.
#' @param df
#'
#' @return
#' @export
#'
#' @examples
fix_psnu_mapping <- function(df){
  df |>
    dplyr::mutate(snu1 = dplyr::case_when(
      psnu == "Itezhi-tezhi District" & fiscal_year < 2022 ~ "Central Province",
      psnu == "Chama District" & fiscal_year < 2022 ~ "Muchinga",
      psnu == "Chirundu District" & fiscal_year < 2022 ~ "Lusaka",
      TRUE ~ snu1
    ),
    snu1uid = dplyr::case_when(
      psnu == "Itezhi-tezhi District" & fiscal_year < 2022 ~ "glHv5VLHKi0",
      psnu == "Chama District" & fiscal_year < 2022 ~ "QPIMzDJei82",
      psnu == "Chirundu District" & fiscal_year < 2022 ~ "fKHv5qcp1nN",
      TRUE ~ snu1)
    )
}


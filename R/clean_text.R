#' Title
#' Clean mechanims names
#'
#' @param .data msd or genie data extract with mech_name column present
#'
#' @return cleaned mech names
#' @export
#'
clean_mechs <- function(.data) {

  .data %>%
    dplyr::mutate(mech_name = dplyr::case_when(
      # USAID
      mech_name == 'Sexual and  Reproductive Health for All Initiative (SARAI)' ~ 'SARAI',
      mech_name == 'USAID/District Coverage of Health Services (DISCOVER-H)' ~ 'DISCOVER-H',
      mech_name == 'USAID/Stop Gender Based Violence Project (Stop GBV)' ~ 'Stop GBV',
      mech_name == 'USAID/Zambia Community HIV Prevention Project (Z-CHPP)' ~ 'Z-CHPP',
      mech_name == '[Placeholder - 85117 Zambia USAID]' ~ '85117 Zambia USAID',
      TRUE ~ mech_name)
    )
}


#' Title
#' Clean implementing partner names
#' @param .data msd or genie data extract with mech_name column present
#'
#' @return cleaned prime partner names
#' @export
#'
#'
clean_partners <- function(.data){

  .data %>%
    dplyr::mutate(primepartner = dplyr::case_when(
      primpepartner == 'CLINTON HEALTH ACCESS INITIATIVE, INC.' ~ 'CHAI',
      primpepartner == 'CATHOLIC MEDICAL MISSION BOARD ZAMBIA' ~ 'CMMBZ',
      primpepartner == 'CENTRE FOR INFECTIOUS DISEASE RESEARCH IN ZAMBIA LIMITED' ~ 'CIDR-Z',
      primpepartner == 'Chemonics International, Inc.' ~ 'Chemonics',
      primpepartner == 'John Snow, Incorporated' ~ ' JSI',
      primpepartner == 'SOCIETY FOR FAMILY HEALTH LIM ITED' ~ 'SFHL',
      primpepartner == 'Family Health International' ~ "FHI",
      primpepartner == 'JSI Research And Training Institute, INC' ~ 'JSI-RTI',
      primpepartner == 'Development Aid From People T o People' ~ 'DAFPTP',
      primpepartner == 'Zambia Centre For Communicati on Programmes' ~ 'ZCCP',
      primpepartner == 'Pact, Inc.' ~ 'PACT',
      TRUE ~ primepartner
      )
    )
}

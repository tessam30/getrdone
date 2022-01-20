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
      mech_name == '[Placeholder - 160806 Zambia USAID]' ~ '160806 Zambia USAID',
      mech_name == 'Maintained Epidemic Control of HIV' ~ 'Action HIV',
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
      primepartner == 'CLINTON HEALTH ACCESS INITIATIVE, INC.' ~ 'CHAI',
      primepartner == 'CATHOLIC MEDICAL MISSION BOARD ZAMBIA' ~ 'CMMBZ',
      primepartner == 'CENTRE FOR INFECTIOUS DISEASE RESEARCH IN ZAMBIA LIMITED' ~ 'CIDR-Z',
      primepartner == 'Chemonics International, Inc.' ~ 'Chemonics',
      primepartner == 'John Snow, Incorporated' ~ 'JSI',
      primepartner == 'SOCIETY FOR FAMILY HEALTH LIM ITED' ~ 'SFHL',
      primepartner == 'Family Health International' ~ "FHI",
      primepartner == 'JSI Research And Training Institute, INC' ~ 'JSI-RTI',
      primepartner == 'Development Aid From People T o People' ~ 'DAFPTP',
      primepartner == 'Zambia Centre For Communicati on Programmes' ~ 'ZCCP',
      primepartner == 'Pact, Inc.' ~ 'PACT',
      TRUE ~ primepartner
      )
    )
}

#' Title
#' Swap targets between mechanisms. Useful for aligning targets across FYs
#' @param .data
#' @param mech1
#' @param mech2
#'
#' @return remapped mech codes for targets and results
#' @export
#'
swap_targets <- function(.data, mech1 = "18304", mech2 = "82075") {
  # Using EQUIP as default as this has to be done each time in FY21
  .data %>%
    mutate(mech_code = ifelse(mech_code == {{mech1}}, {{mech2}}, mech_code))
}



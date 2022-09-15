# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_batch_ag_Fert_IRR_MGMT_xml
#'
#' Construct XML data structure for \code{ag_Fert_IRR_MGMT.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ag_Fert_IRR_MGMT.xml}. The corresponding file in the
#' original data system was \code{batch_ag_Fert_IRR_MGMT.xml} (aglu XML).
module_aglu_batch_ag_Fert_IRR_MGMT_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c("L2062.AgCoef_Fert_ag_irr_mgmt",
      "L2062.AgCoef_Fert_bio_irr_mgmt")

  MODULE_OUTPUTS <-
    c(XML = "ag_Fert_IRR_MGMT.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    lapply(MODULE_INPUTS, function(d){
      # get name as the char after last /
      nm <- tail(strsplit(d, "/")[[1]], n = 1)
      # get data and assign
      assign(nm, get_data(all_data, d, strip_attributes = T),
             envir = parent.env(environment()))  })
    # ===================================================

    # Produce outputs
    create_xml("ag_Fert_IRR_MGMT.xml") %>%
      add_xml_data(L2062.AgCoef_Fert_ag_irr_mgmt, "AgCoef") %>%
      add_xml_data(L2062.AgCoef_Fert_bio_irr_mgmt, "AgCoef") %>%
      add_precursors("L2062.AgCoef_Fert_ag_irr_mgmt", "L2062.AgCoef_Fert_bio_irr_mgmt") ->
      ag_Fert_IRR_MGMT.xml

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}

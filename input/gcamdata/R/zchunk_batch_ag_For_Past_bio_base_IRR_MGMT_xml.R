# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_batch_ag_For_Past_bio_base_IRR_MGMT_xml
#'
#' Construct XML data structure for \code{ag_For_Past_bio_base_IRR_MGMT.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ag_For_Past_bio_base_IRR_MGMT.xml}. The corresponding file in the
#' original data system was \code{batch_ag_For_Past_bio_base_IRR_MGMT.xml.R} (aglu XML).
module_aglu_batch_ag_For_Past_bio_base_IRR_MGMT_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c("L2012.AgSupplySector",
      "L2012.AgSupplySubsector",
      "L2012.AgProduction_ag_irr_mgmt",
      "L2012.AgProduction_For",
      "L2012.AgProduction_Past",
      #"L2012.AgHAtoCL_irr_mgmt", # Note (XZ): not exporting HAtoCL as not used in GCAM; this should be examined later.
      "L2012.AgYield_bio_ref")

  MODULE_OUTPUTS <-
    c(XML = "ag_For_Past_bio_base_IRR_MGMT.xml")

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
    create_xml("ag_For_Past_bio_base_IRR_MGMT.xml") %>%
      add_logit_tables_xml(L2012.AgSupplySector, "AgSupplySector") %>%
      add_logit_tables_xml(L2012.AgSupplySubsector, "AgSupplySubsector") %>%
      add_xml_data(L2012.AgProduction_ag_irr_mgmt, "AgProduction") %>%
      add_xml_data(L2012.AgProduction_For, "AgProduction") %>%
      add_xml_data(L2012.AgProduction_Past, "AgProduction") %>%
      #add_xml_data(L2012.AgHAtoCL_irr_mgmt, "AgHAtoCL") %>%
      add_xml_data(L2012.AgYield_bio_ref, "AgYield") %>%
      add_precursors("L2012.AgSupplySubsector", "L2012.AgProduction_ag_irr_mgmt",
                     "L2012.AgProduction_For", "L2012.AgProduction_Past", #"L2012.AgHAtoCL_irr_mgmt",
                     "L2012.AgYield_bio_ref", "L2012.AgSupplySector") ->
      ag_For_Past_bio_base_IRR_MGMT.xml

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}

# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_batch_ag_cost_IRR_MGMT_xml
#'
#' Construct XML data structure for \code{ag_cost_IRR_MGMT.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ag_cost_IRR_MGMT.xml}. The corresponding file in the
#' original data system was \code{batch_ag_cost_IRR_MGMT_xml.R} (aglu XML).
module_aglu_batch_ag_cost_IRR_MGMT_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2062.AgCost_ag_irr_mgmt_adj",
             "L2062.AgCost_bio_irr_mgmt_adj",
             # Note that L2052.Agcost ag and bio files are replaced by L2062 ones as fertilizer costs were adjusted
             "L2052.AgCost_For"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "ag_cost_IRR_MGMT.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2062.AgCost_ag_irr_mgmt_adj <- get_data(all_data, "L2062.AgCost_ag_irr_mgmt_adj")
    L2062.AgCost_bio_irr_mgmt_adj <- get_data(all_data, "L2062.AgCost_bio_irr_mgmt_adj")
    L2052.AgCost_For <- get_data(all_data, "L2052.AgCost_For")

    # ===================================================

    # Produce outputs
    create_xml("ag_cost_IRR_MGMT.xml") %>%
      add_xml_data(L2062.AgCost_ag_irr_mgmt_adj, "AgCost") %>%
      add_xml_data(L2062.AgCost_bio_irr_mgmt_adj, "AgCost") %>%
      add_xml_data(L2052.AgCost_For, "AgCost") %>%
      add_precursors("L2062.AgCost_ag_irr_mgmt_adj", "L2062.AgCost_bio_irr_mgmt_adj", "L2052.AgCost_For") ->
      ag_cost_IRR_MGMT.xml

    return_data(ag_cost_IRR_MGMT.xml)
  } else {
    stop("Unknown command")
  }
}

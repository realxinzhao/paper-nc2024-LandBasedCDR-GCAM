# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_dac_USA_xml
#'
#' Construct XML data structure for \code{dac_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{dac_USA.xml}. The corresponding file in the
#' original data system was \code{batch_dac_USA_xml.R} (gcamusa XML).
module_gcamusa_batch_dac_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2999.DeleteSupplysector_USAdac",
             "L2999.Supplysector_dac_USA",
             "L2999.FinalEnergyKeyword_dac_USA",
             "L2999.SubsectorLogit_dac_USA",
             "L2999.SubsectorShrwtFllt_dac_USA",
             "L2999.SubsectorInterp_dac_USA",
             "L2999.StubTech_dac_USA",
             "L2999.PerCapitaBased_dac_USA",
             "L2999.PriceElasticity_dac_USA",
             "L2999.DeleteFinalDemand_USAdac",
             "L2999.StubTechProd_dac_USA",
             "L2999.StubTechCoef_dac_USA",
             "L2999.BaseService_dac_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "dac_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2999.DeleteSupplysector_USAdac <- get_data(all_data, "L2999.DeleteSupplysector_USAdac")
    L2999.FinalEnergyKeyword_dac_USA <- get_data(all_data, "L2999.FinalEnergyKeyword_dac_USA")
    L2999.SubsectorLogit_dac_USA <- get_data(all_data, "L2999.SubsectorLogit_dac_USA")
    L2999.SubsectorShrwtFllt_dac_USA <- get_data(all_data, "L2999.SubsectorShrwtFllt_dac_USA")
    L2999.SubsectorInterp_dac_USA <- get_data(all_data, "L2999.SubsectorInterp_dac_USA")
    L2999.StubTech_dac_USA <- get_data(all_data, "L2999.StubTech_dac_USA")
    L2999.PerCapitaBased_dac_USA <- get_data(all_data, "L2999.PerCapitaBased_dac_USA")
    L2999.PriceElasticity_dac_USA <- get_data(all_data, "L2999.PriceElasticity_dac_USA")
    L2999.DeleteFinalDemand_USAdac <- get_data(all_data, "L2999.DeleteFinalDemand_USAdac")
    L2999.Supplysector_dac_USA <- get_data(all_data, "L2999.Supplysector_dac_USA")
    L2999.StubTechProd_dac_USA <- get_data(all_data, "L2999.StubTechProd_dac_USA")
    L2999.StubTechCoef_dac_USA <- get_data(all_data, "L2999.StubTechCoef_dac_USA")
    L2999.BaseService_dac_USA <- get_data(all_data, "L2999.BaseService_dac_USA")


    # ===================================================

    # Produce outputs
    create_xml("dac_USA.xml") %>%
      add_xml_data(L2999.DeleteSupplysector_USAdac, "DeleteSupplysector") %>%
      add_xml_data(L2999.DeleteFinalDemand_USAdac, "DeleteFinalDemand") %>%
      add_logit_tables_xml(L2999.Supplysector_dac_USA, "Supplysector") %>%
      add_xml_data(L2999.FinalEnergyKeyword_dac_USA, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2999.SubsectorLogit_dac_USA, "SubsectorLogit") %>%
      add_xml_data(L2999.SubsectorShrwtFllt_dac_USA, "SubsectorShrwtFllt") %>%
      add_xml_data(L2999.SubsectorInterp_dac_USA, "SubsectorInterp") %>%
      add_xml_data(L2999.StubTech_dac_USA, "StubTech") %>%
      add_xml_data(L2999.PerCapitaBased_dac_USA, "PerCapitaBased") %>%
      add_xml_data(L2999.PriceElasticity_dac_USA, "PriceElasticity") %>%
      add_xml_data(L2999.StubTechProd_dac_USA, "StubTechProd") %>%
      add_xml_data(L2999.StubTechCoef_dac_USA, "StubTechCoef") %>%
      add_xml_data(L2999.BaseService_dac_USA, "BaseService") %>%
      add_precursors("L2999.DeleteSupplysector_USAdac",
                     "L2999.Supplysector_dac_USA",
                     "L2999.FinalEnergyKeyword_dac_USA",
                     "L2999.SubsectorLogit_dac_USA",
                     "L2999.SubsectorShrwtFllt_dac_USA",
                     "L2999.SubsectorInterp_dac_USA",
                     "L2999.StubTech_dac_USA",
                     "L2999.PerCapitaBased_dac_USA",
                     "L2999.PriceElasticity_dac_USA",
                     "L2999.DeleteFinalDemand_USAdac",
                     "L2999.StubTechProd_dac_USA",
                     "L2999.StubTechCoef_dac_USA",
                     "L2999.BaseService_dac_USA") ->
      dac_USA.xml

    return_data(dac_USA.xml)
  } else {
    stop("Unknown command")
  }
}

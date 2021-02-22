# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_dac_xml
#'
#' Construct XML data structure for \code{dac.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{dac.xml}. The corresponding file in the
#' original data system was \code{batch_dac_xml.R} (energy XML).
module_energy_batch_dac_xml <- function(command, ...) {


  TECH_PARAMETRIZATION_INPUTS <- paste0("ssp", 1:5)
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2999.Supplysector_dac",
             "L2999.FinalEnergyKeyword_dac",
             "L2999.SubsectorLogit_dac",
             "L2999.SubsectorShrwtFllt_dac",
             "L2999.SubsectorInterp_dac",
             "L2999.StubTech_dac",
             "L2999.GlobalTechShrwt_dac",
             #"L2999.GlobalTechCoef_dac",
             #"L2999.GlobalTechCost_dac",
             c(paste("L2999.GlobalTechShrwt_dac", tolower(TECH_PARAMETRIZATION_INPUTS), sep = "_")),
             c(paste("L2999.GlobalTechCoef_dac", tolower(TECH_PARAMETRIZATION_INPUTS), sep = "_")),
             c(paste("L2999.GlobalTechCost_dac", tolower(TECH_PARAMETRIZATION_INPUTS), sep = "_")),
             "L2999.GlobalTechCapture_dac",
             "L2999.StubTechProd_dac",
             "L2999.PerCapitaBased_dac",
             "L2999.BaseService_dac",
             "L2999.PriceElasticity_dac",
             "L2999.GlobalTechSCurve_dac",
             "L2999.GlobalTechProfitShutdown_dac"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "dac_ssp1.xml",
             XML = "dac_ssp2.xml",
             XML = "dac_ssp3.xml",
             XML = "dac_ssp4.xml",
             XML = "dac_ssp5.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    for(sce in TECH_PARAMETRIZATION_INPUTS){
    # Load required inputs
    L2999.Supplysector_dac <- get_data(all_data, "L2999.Supplysector_dac")
    L2999.FinalEnergyKeyword_dac <- get_data(all_data, "L2999.FinalEnergyKeyword_dac")
    L2999.SubsectorLogit_dac <- get_data(all_data, "L2999.SubsectorLogit_dac")
    #    L2999.SubsectorShrwt_dac <- get_data(all_data, "L2999.SubsectorShrwt_dac")
    L2999.SubsectorShrwtFllt_dac <- get_data(all_data, "L2999.SubsectorShrwtFllt_dac")
    L2999.SubsectorInterp_dac <- get_data(all_data, "L2999.SubsectorInterp_dac")
    #    L2999.SubsectorInterpTo_dac <- get_data(all_data, "L2999.SubsectorInterpTo_dac")
    L2999.StubTech_dac <- get_data(all_data, "L2999.StubTech_dac")
    L2999.GlobalTechShrwt_dac <- get_data(all_data, "L2999.GlobalTechShrwt_dac")

    coef_name <- paste0("L2999.GlobalTechCoef_dac_",tolower(sce))
    cost_name <- paste0("L2999.GlobalTechCost_dac_",tolower(sce))
    shwt_name <- paste0("L2999.GlobalTechShrwt_dac_",tolower(sce))

    L2999.GlobalTechCoef_dac <- get_data(all_data, coef_name)
    L2999.GlobalTechCost_dac <- get_data(all_data, cost_name)
    L2999.GlobalTechShrwt_dac <- get_data(all_data, shwt_name)

    #L2999.GlobalTechCost_dac <- get_data(all_data, "L2999.GlobalTechCost_dac")
    L2999.GlobalTechCapture_dac <- get_data(all_data, "L2999.GlobalTechCapture_dac")
    L2999.StubTechProd_dac <- get_data(all_data, "L2999.StubTechProd_dac")
    #L2999.StubTechCalInput_dac_heat <- get_data(all_data, "L2999.StubTechCalInput_dac_heat")
    #L2999.StubTechCoef_dac <- get_data(all_data, "L2999.StubTechCoef_dac")
    L2999.PerCapitaBased_dac <- get_data(all_data, "L2999.PerCapitaBased_dac")
    L2999.BaseService_dac <- get_data(all_data, "L2999.BaseService_dac")
    L2999.PriceElasticity_dac <- get_data(all_data, "L2999.PriceElasticity_dac")
    L2999.GlobalTechSCurve_dac <- get_data(all_data, "L2999.GlobalTechSCurve_dac")
    L2999.GlobalTechProfitShutdown_dac <- get_data(all_data, "L2999.GlobalTechProfitShutdown_dac")



    xmlfn <- paste0("dac_",tolower(sce), '.xml')

    # ===================================================
    # Produce outputs
    create_xml(xmlfn) %>%
      add_logit_tables_xml(L2999.Supplysector_dac, "Supplysector") %>%
      add_xml_data(L2999.FinalEnergyKeyword_dac, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2999.SubsectorLogit_dac, "SubsectorLogit") %>%
      #      add_xml_data(L2999.SubsectorShrwt_dac, "SubsectorShrwt") %>%
      add_xml_data(L2999.SubsectorShrwtFllt_dac, "SubsectorShrwtFllt") %>%
      add_xml_data(L2999.SubsectorInterp_dac, "SubsectorInterp") %>%
      #      add_xml_data(L2999.SubsectorInterpTo_dac, "SubsectorInterpTo") %>%
      add_xml_data(L2999.StubTech_dac, "StubTech") %>%
      add_xml_data(L2999.GlobalTechShrwt_dac, "GlobalTechShrwt") %>%
      add_xml_data(L2999.GlobalTechCoef_dac, "GlobalTechCoef") %>%
      add_xml_data(L2999.GlobalTechCost_dac, "GlobalTechCost") %>%
      add_xml_data(L2999.GlobalTechCapture_dac, "GlobalTechCapture") %>%
      add_xml_data(L2999.StubTechProd_dac, "StubTechProd") %>%
      #add_xml_data(L2999.StubTechCalInput_dac_heat, "StubTechCalInput") %>%
      #add_xml_data(L2999.StubTechCoef_dac, "StubTechCoef") %>%
      add_xml_data(L2999.PerCapitaBased_dac, "PerCapitaBased") %>%
      add_xml_data(L2999.BaseService_dac, "BaseService") %>%
      add_xml_data(L2999.PriceElasticity_dac, "PriceElasticity") %>%
      add_xml_data(L2999.GlobalTechSCurve_dac, "GlobalTechSCurve") %>%
      add_xml_data(L2999.GlobalTechProfitShutdown_dac, "GlobalTechProfitShutdown") %>%
      add_precursors("L2999.Supplysector_dac",
                     "L2999.FinalEnergyKeyword_dac",
                     "L2999.SubsectorLogit_dac",
                     # "L2999.SubsectorShrwt_dac",
                     "L2999.SubsectorShrwtFllt_dac",
                     "L2999.SubsectorInterp_dac",
                     # "L2999.SubsectorInterpTo_dac",
                     "L2999.StubTechProd_dac",
                     "L2999.StubTech_dac",
                     "L2999.GlobalTechShrwt_dac",
                     #"L2999.GlobalTechCoef_dac",
                     #"L2999.GlobalTechCost_dac",
                     paste0("L2999.GlobalTechShrwt_dac_",tolower(sce)),
                     paste0("L2999.GlobalTechCoef_dac_",tolower(sce)),
                     paste0("L2999.GlobalTechCost_dac_",tolower(sce)),
                     "L2999.GlobalTechCapture_dac",
                     #"L2999.StubTechCoef_dac",
                     "L2999.PerCapitaBased_dac",
                     "L2999.BaseService_dac",
                     "L2999.PriceElasticity_dac",
                     "L2999.GlobalTechSCurve_dac",
                     "L2999.GlobalTechProfitShutdown_dac") ->
      xmlobj
    assign(xmlfn, xmlobj)


  }
  return_data(dac_ssp1.xml,
              dac_ssp2.xml,
              dac_ssp3.xml,
              dac_ssp4.xml,
              dac_ssp5.xml)}
  else {
    stop("Unknown command")
  }
}

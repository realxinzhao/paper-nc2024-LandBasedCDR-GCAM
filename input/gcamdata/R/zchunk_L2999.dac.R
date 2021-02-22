# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L2999.dac
#'
#' Compute a variety of final energy keyword, sector, share weight, and technology information for dac-related GCAM inputs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2999.SectorLogitTables[[ curr_table ]]$data}, \code{L2999.Supplysector_dac}, \code{L2999.FinalEnergyKeyword_dac},
#' \code{L2999.SubsectorLogitTables[[ curr_table ]]$data}, \code{L2999.SubsectorLogit_dac}, \code{L2999.SubsectorShrwtFllt_dac},
#' \code{L2999.SubsectorInterp_dac}, \code{L2999.StubTech_dac}, \code{L2999.GlobalTechShrwt_dac}, \code{L2999.GlobalTechCoef_dac},
#' \code{L2999.GlobalTechCost_dac}, \code{L2999.GlobalTechCapture_dac}, \code{L2999.StubTechProd_dac}, \code{L2999.StubTechCalInput_dac_heat},
#' \code{L2999.StubTechCoef_dac}, \code{L2999.PerCapitaBased_dac}, \code{L2999.BaseService_dac}, \code{L2999.PriceElasticity_dac}, \code{object}.
#' The corresponding file in the original data system was \code{L2999.dac.R} (energy level2).
#' @details The chunk provides final energy keyword, supplysector/subsector information, supplysector/subsector interpolation information, global technology share weight, global technology efficiency, global technology coefficients, global technology cost, price elasticity, stub technology information, stub technology interpolation information, stub technology calibrated inputs, and etc for dac sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows distinct filter if_else group_by lag left_join mutate pull select
#' @importFrom tidyr complete nesting
#' @author JF October 2017
module_energy_L2999.dac <- function(command, ...) {

  TECH_PARAMETRIZATION_OUTPUTS <- paste0("ssp", 1:5)

  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/calibrated_techs_ces",
             #FILE = "emissions/A_PrimaryFuelCCoef",
             FILE = "energy/A999.sector",
             FILE = "energy/A999.subsector_interp",
             FILE = "energy/A999.subsector_logit",
             FILE = "energy/A999.subsector_shrwt",

             FILE = "energy/A999.globaltech_coef_ssp1",
             FILE = "energy/A999.globaltech_coef_ssp2",
             FILE = "energy/A999.globaltech_coef_ssp3",
             FILE = "energy/A999.globaltech_coef_ssp4",
             FILE = "energy/A999.globaltech_coef_ssp5",

             FILE = "energy/A999.globaltech_cost_ssp1",
             FILE = "energy/A999.globaltech_cost_ssp2",
             FILE = "energy/A999.globaltech_cost_ssp3",
             FILE = "energy/A999.globaltech_cost_ssp4",
             FILE = "energy/A999.globaltech_cost_ssp5",

             FILE = "energy/A999.globaltech_shrwt",

             FILE = "energy/A999.globaltech_shrwt_ssp1",
             FILE = "energy/A999.globaltech_shrwt_ssp2",
             FILE = "energy/A999.globaltech_shrwt_ssp3",
             FILE = "energy/A999.globaltech_shrwt_ssp4",
             FILE = "energy/A999.globaltech_shrwt_ssp5",


             FILE = "energy/A999.globaltech_co2capture",
             FILE = "energy/A999.demand",
             FILE = "energy/A999.globaltech_retirement",
             "L1999.out_Mt_R_dac_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2999.Supplysector_dac",
             "L2999.FinalEnergyKeyword_dac",
             "L2999.SubsectorLogit_dac",
             "L2999.SubsectorShrwtFllt_dac",
             "L2999.SubsectorInterp_dac",

             "L2999.GlobalTechCost_dac",

             "L2999.GlobalTechCost_dac_ssp1",
             "L2999.GlobalTechCost_dac_ssp2",
             "L2999.GlobalTechCost_dac_ssp3",
             "L2999.GlobalTechCost_dac_ssp4",
             "L2999.GlobalTechCost_dac_ssp5",

             "L2999.StubTech_dac",
             "L2999.GlobalTechShrwt_dac",

             "L2999.GlobalTechShrwt_dac_ssp1",
             "L2999.GlobalTechShrwt_dac_ssp2",
             "L2999.GlobalTechShrwt_dac_ssp3",
             "L2999.GlobalTechShrwt_dac_ssp4",
             "L2999.GlobalTechShrwt_dac_ssp5",


             'L2999.GlobalTechCoef_dac',

             "L2999.GlobalTechCoef_dac_ssp1",
             "L2999.GlobalTechCoef_dac_ssp2",
             "L2999.GlobalTechCoef_dac_ssp3",
             "L2999.GlobalTechCoef_dac_ssp4",
             "L2999.GlobalTechCoef_dac_ssp5",

             "L2999.GlobalTechCapture_dac",
             "L2999.PerCapitaBased_dac",
             "L2999.PriceElasticity_dac",
             "L2999.StubTechProd_dac",
             "L2999.BaseService_dac",
             "L2999.GlobalTechSCurve_dac",
             "L2999.GlobalTechProfitShutdown_dac"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs_ces")
    A999.sector <- get_data(all_data, "energy/A999.sector", strip_attributes = TRUE)
    #A_PrimaryFuelCCoef <- get_data(all_data, "emissions/A_PrimaryFuelCCoef")
    A999.subsector_interp <- get_data(all_data, "energy/A999.subsector_interp", strip_attributes = TRUE)
    A999.subsector_logit <- get_data(all_data, "energy/A999.subsector_logit", strip_attributes = TRUE)
    A999.subsector_shrwt <- get_data(all_data, "energy/A999.subsector_shrwt", strip_attributes = TRUE)
    #A999.globaltech_coef <- get_data(all_data, "energy/A999.globaltech_coef")
    #A999.globaltech_cost <- get_data(all_data, "energy/A999.globaltech_cost")
    #A999.globaltech_shrwt <- get_data(all_data, "energy/A999.globaltech_shrwt", strip_attributes = TRUE)
    A999.globaltech_co2capture <- get_data(all_data, "energy/A999.globaltech_co2capture")
    A999.demand <- get_data(all_data, "energy/A999.demand", strip_attributes = TRUE)
    A999.globaltech_retirement <- get_data(all_data, "energy/A999.globaltech_retirement", strip_attributes = TRUE)
    L1999.out_Mt_R_dac_Yh <- get_data(all_data, "L1999.out_Mt_R_dac_Yh", strip_attributes = TRUE)



    #load ssp parametrizations
    A999.globaltech_coef_ssp1 <- get_data(all_data, "energy/A999.globaltech_coef_ssp1")%>% gather_years %>% mutate(scenario=paste0("ssp1"))
    A999.globaltech_coef_ssp2 <- get_data(all_data, "energy/A999.globaltech_coef_ssp2")%>% gather_years %>% mutate(scenario=paste0("ssp2"))
    A999.globaltech_coef_ssp3 <- get_data(all_data, "energy/A999.globaltech_coef_ssp3")%>% gather_years %>% mutate(scenario=paste0("ssp3"))
    A999.globaltech_coef_ssp4 <- get_data(all_data, "energy/A999.globaltech_coef_ssp4")%>% gather_years %>% mutate(scenario=paste0("ssp4"))
    A999.globaltech_coef_ssp5 <- get_data(all_data, "energy/A999.globaltech_coef_ssp5")%>% gather_years %>% mutate(scenario=paste0("ssp5"))

    A999.globaltech_cost_ssp1 <- get_data(all_data, "energy/A999.globaltech_cost_ssp1")%>% gather_years %>% mutate(scenario=paste0("ssp1"))
    A999.globaltech_cost_ssp2 <- get_data(all_data, "energy/A999.globaltech_cost_ssp2")%>% gather_years %>% mutate(scenario=paste0("ssp2"))
    A999.globaltech_cost_ssp3 <- get_data(all_data, "energy/A999.globaltech_cost_ssp3")%>% gather_years %>% mutate(scenario=paste0("ssp3"))
    A999.globaltech_cost_ssp4 <- get_data(all_data, "energy/A999.globaltech_cost_ssp4")%>% gather_years %>% mutate(scenario=paste0("ssp4"))
    A999.globaltech_cost_ssp5 <- get_data(all_data, "energy/A999.globaltech_cost_ssp5")%>% gather_years %>% mutate(scenario=paste0("ssp5"))

    A999.globaltech_shrwt_ssp1 <- get_data(all_data, "energy/A999.globaltech_shrwt_ssp1")%>% gather_years %>% mutate(scenario=paste0("ssp1"))
    A999.globaltech_shrwt_ssp2 <- get_data(all_data, "energy/A999.globaltech_shrwt_ssp2")%>% gather_years %>% mutate(scenario=paste0("ssp2"))
    A999.globaltech_shrwt_ssp3 <- get_data(all_data, "energy/A999.globaltech_shrwt_ssp3")%>% gather_years %>% mutate(scenario=paste0("ssp3"))
    A999.globaltech_shrwt_ssp4 <- get_data(all_data, "energy/A999.globaltech_shrwt_ssp4")%>% gather_years %>% mutate(scenario=paste0("ssp4"))
    A999.globaltech_shrwt_ssp5 <- get_data(all_data, "energy/A999.globaltech_shrwt_ssp5")%>% gather_years %>% mutate(scenario=paste0("ssp5"))


    # ===================================================
    # 0. Give binding for variable names used in pipeline
    year <- value <- GCAM_region_ID <- sector <- fuel <- year.fillout <- to.value <-
      technology <- supplysector <- subsector <- minicam.energy.input <- coefficient <-
      remove.fraction <- minicam.non.energy.input <- input.cost <- PrimaryFuelCO2Coef.name <-
      PrimaryFuelCO2Coef <- calibration <- calOutputValue <- subs.share.weight <- region <-
      calibrated.value <- . <- scenario <- temp_lag <- base.service <- energy.final.demand <-
      value.x <- value.y <- parameter <- half.life <- median.shutdown.point <-
      L2999.GlobalTechCoef_dac_ssp1 <- L2999.GlobalTechCoef_dac_ssp2 <-L2999.GlobalTechCoef_dac_ssp3 <-L2999.GlobalTechCoef_dac_ssp4 <-L2999.GlobalTechCoef_dac_ssp5 <-NULL


    # ===================================================
    # 1. Perform computations
    # 1a. Supplysector information
    # L2999.Supplysector_dac: Supply sector information for dac sector
    A999.sector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L2999.Supplysector_dac

    # L2999.FinalEnergyKeyword_dac: Supply sector keywords for ces sector
    A999.sector %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]], GCAM_region_names) %>%
      na.omit ->
      L2999.FinalEnergyKeyword_dac

    # 1b. Subsector information
    # L2999.SubsectorLogit_dac: Subsector logit exponents of dac sector
    A999.subsector_logit %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L2999.SubsectorLogit_dac

    # and L2999.SubsectorShrwtFllt_dac: Subsector shareweights of dac sector
    A999.subsector_shrwt %>%
      filter(!is.na(year.fillout)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names) ->
      L2999.SubsectorShrwtFllt_dac

    # L2999.SubsectorInterp_dac: Subsector shareweight interpolation of dac sector
    A999.subsector_interp %>%
      filter(is.na(to.value)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterp"]], GCAM_region_names) ->
      L2999.SubsectorInterp_dac

    # 1c. Technology information
    # L2999.StubTech_dac: Identification of stub technologies of dac
    # Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)

    A999.globaltech_shrwt <- bind_rows(A999.globaltech_shrwt_ssp1,A999.globaltech_shrwt_ssp2,A999.globaltech_shrwt_ssp3,A999.globaltech_shrwt_ssp4,A999.globaltech_shrwt_ssp5)

    A999.globaltech_shrwt %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]], GCAM_region_names) %>%
      rename(stub.technology = technology) ->
      L2999.StubTech_dac

    # L2999.GlobalTechShrwt_dac: Shareweights of global dac technologies



    A999.globaltech_shrwt %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology,scenario), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, scenario,year) %>%
      group_by(scenario,supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, value, rule = 1)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "share.weight","scenario") ->
      L2999.GlobalTechShrwt_dac


    #concatenate tables of all ssps into one table for later filtering
    A999.globaltech_coef <- bind_rows(A999.globaltech_coef_ssp1,A999.globaltech_coef_ssp2,A999.globaltech_coef_ssp3,A999.globaltech_coef_ssp4,A999.globaltech_coef_ssp5)
    # L2999.GlobalTechCoef_dac: Energy inputs and coefficients of dac technologies
    A999.globaltech_coef %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input,scenario), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.energy.input, scenario,year) %>%
      group_by(scenario,supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(coefficient = approx_fun(year, value, rule = 1),
             coefficient = round(coefficient, energy.DIGITS_COEFFICIENT)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]],'scenario') ->
      L2999.GlobalTechCoef_dac

    # Carbon capture rates for dac.
    # L2999.GlobalTechCapture_dac: defines CO2 capture fractions for dac (by definition 1, as all inputs are defined per tonne C removed from the atmosphere),
    # as well as a separately-defined process heat dac sector.
    # This allows separate consideration of the capture fraction of any combustion emissions resulting from the process heat input
    # No need to consider historical periods here
    A999.globaltech_co2capture %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(remove.fraction = approx_fun(year, value, rule = 1),
             remove.fraction = round(remove.fraction, energy.DIGITS_REMOVE.FRACTION)) %>%
      ungroup %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "remove.fraction") %>%
      mutate(storage.market = energy.CO2.STORAGE.MARKET) ->
      L2999.GlobalTechCapture_dac



    A999.globaltech_cost <- bind_rows(A999.globaltech_cost_ssp1,A999.globaltech_cost_ssp2,A999.globaltech_cost_ssp3,A999.globaltech_cost_ssp4,A999.globaltech_cost_ssp5)




    # L2999.GlobalTechCost_dac: Non-energy costs of global dac manufacturing technologies

    A999.globaltech_cost %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology, minicam.non.energy.input,scenario), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.non.energy.input, scenario,year) %>%
      group_by(scenario,supplysector, subsector, technology, minicam.non.energy.input) %>%
      mutate(input.cost = approx_fun(year, value, rule = 1),
             input.cost = round(input.cost, energy.DIGITS_COST)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCost"]],'scenario') ->
      L2999.GlobalTechCost_dac # intermediate tibble



    L2999.GlobalTechCapture_dac %>%
      pull(remove.fraction) %>%
      mean -> dac_CO2_capture_frac

    L2999.GlobalTechCoef_dac %>%
      filter(minicam.energy.input == "airCO2") %>%
      pull(coefficient) %>%
      mean ->
      coef_mean # temporary value


    # Adjust the non-energy costs in the table for model input
#    L2999.GlobalTechCost_dac %>%
#      filter(technology %in% L2999.GlobalTechCapture_dac[["technology"]]) %>%
#      mutate(input.cost = input.cost) %>%
#      bind_rows(filter(L2999.GlobalTechCost_dac, !(technology %in% L2999.GlobalTechCapture_dac[["technology"]]))) %>%
#      mutate(input.cost = round(input.cost, energy.DIGITS_COST)) ->
#      L2999.GlobalTechCost_dac



     #Calibration and region-specific data
    # L2999.StubTechProd_dac: calibrated ces production
    calibrated_techs %>%
      filter(calibration == "output") %>% # Only take the tech IDs where the calibration is identified as output
      select(sector, supplysector, subsector, technology) %>%
      distinct ->
      calibrated_techs_export # temporary tibble

    L1999.out_Mt_R_dac_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calOutputValue = round(value, energy.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(calibrated_techs_export, by = "sector") %>%
      mutate(stub.technology = technology,
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) ->
      L2999.StubTechProd_dac


    # L2999.StubTechCoef_dac: region-specific coefficients of dac production technologies
    # Take this as a given in all years for which data is available
    calibrated_techs %>%
      select(sector, fuel, supplysector, subsector, technology, minicam.energy.input) %>%
      distinct ->
      calibrated_techs_export # temporary tibble


    # L2999.StubTechCalInput_dac_heat: calibrated dac production
    calibrated_techs %>%
      select(sector, fuel, supplysector, subsector, technology, minicam.energy.input) %>%
      distinct ->
      calibrated_techs_export # temporary tibble



    # L2999.PerCapitaBased_dac: per-capita based flag for dac exports final demand.  Note that this should be zero as the amount of DAC shouldn't be explicitly tied to population
    A999.demand %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["PerCapitaBased"]], GCAM_region_names) ->
      L2999.PerCapitaBased_dac


#     L2999.BaseService_dac: base-year service output of dac
      L2999.StubTechProd_dac %>%
      select(region, year, base.service = calOutputValue) %>%
      mutate(energy.final.demand = A999.demand[["energy.final.demand"]]) ->
      L2999.BaseService_dac

    # L2999.PriceElasticity_dac: price elasticity
    A999.demand %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["PriceElasticity"]][LEVEL2_DATA_NAMES[["PriceElasticity"]] != "year"], GCAM_region_names) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["PriceElasticity"]]) ->
      L2999.PriceElasticity_dac


    # Retirement information
    A999.globaltech_retirement %>%
      set_years %>%
      mutate(year = as.integer(year)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      A999.globaltech_retirement_with_years

    # Copy the data in the last base year period through to the end year
    A999.globaltech_retirement_with_years %>%
      filter(year == max(MODEL_BASE_YEARS)) ->
      A999.globaltech_retirement_max_baseyear

    A999.globaltech_retirement_with_years %>%
      filter(year == min(MODEL_FUTURE_YEARS)) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      bind_rows(A999.globaltech_retirement_max_baseyear) ->
      L2999.globaltech_retirement

    # Retirement may consist of any of three types of retirement function (phased, s-curve, or none)
    # All of these options have different headers, and all are allowed

    # L2999.GlobalTechSCurve_dac: Global tech lifetime and s-curve retirement function
    L2999.globaltech_retirement %>%
      filter(!is.na(half.life)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "steepness", "half.life") ->
      L2999.GlobalTechSCurve_dac

    # L2999.GlobalTechProfitShutdown_dac: Global tech profit shutdown decider.
    L2999.globaltech_retirement %>%
      filter(!is.na(median.shutdown.point)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "median.shutdown.point", "profit.shutdown.steepness") ->
      L2999.GlobalTechProfitShutdown_dac







    # ===================================================
    # Produce outputs

    # Extract SSP data and assign to separate tables

    for(sce in TECH_PARAMETRIZATION_OUTPUTS) {
      L2999.GlobalTechCost_dac %>%
        filter(scenario == sce) %>%
        select(-c(scenario))%>%
        #add_title("Energy inputs and coefficients of dac technologies") %>%
        add_title(paste("Cost coefficients of dac -", sce)) %>%
        add_units("1975$/kg for supplysector dac; 1975$/GJ for supplysector process heat dac") %>%
        add_comments(sce) %>%
        add_comments("Includes non-energy related capture costs only per kgC captured from the atmosphere. Storage costs will be computed endogenously through the carbon storage markets. Additional non-energy cost of process heat dac assumed zero.") %>%
        #add_legacy_name("L2999.GlobalTechCoef_dac") %>%
        add_legacy_name(paste0("L2999.GlobalTechCost_dac_", tolower(sce))) %>%
        add_precursors(paste0("energy/A999.globaltech_cost_", tolower(sce))) ->
        x
      assign(paste0("L2999.GlobalTechCost_dac_", tolower(sce)), x)
    }



    for(sce in TECH_PARAMETRIZATION_OUTPUTS) {
      L2999.GlobalTechCoef_dac %>%
        filter(scenario == sce) %>%
        select(-c(scenario))%>%
        #add_title("Energy inputs and coefficients of dac technologies") %>%
        add_title(paste("Tech coefficients of dac -", sce)) %>%
        add_units("airCO2 input is unitless (Mt airCO2 per Mt dac); all others are GJ per kg (EJ of energy per Mt of dac)") %>%
        add_comments(sce) %>%
        add_comments("For dac sector, the energy use coefficients from A999.globaltech_coef are interpolated into all model years") %>%
        #add_legacy_name("L2999.GlobalTechCoef_dac") %>%
        add_legacy_name(paste0("L2999.GlobalTechCoef_dac_", tolower(sce))) %>%
        add_precursors(paste0("energy/A999.globaltech_coef_", tolower(sce))) ->
        x
      assign(paste0("L2999.GlobalTechCoef_dac_", tolower(sce)), x)
    }


    for(sce in TECH_PARAMETRIZATION_OUTPUTS) {
      L2999.GlobalTechShrwt_dac %>%
        filter(scenario == sce) %>%
        select(-c(scenario))%>%
        add_title("Shareweights of global dac technologies") %>%
        add_units("Unitless") %>%
        add_comments("For dac sector, the share weights from A999.globaltech_shrwt are interpolated into all base years and future years") %>%
        add_legacy_name(paste0("L2999.GlobalTechShrwt_dac_",tolower(sce))) %>%
        add_precursors(paste0("energy/A999.globaltech_shrwt_",tolower(sce))) ->
        x
      assign(paste0("L2999.GlobalTechShrwt_dac_", tolower(sce)), x)
    }




    L2999.Supplysector_dac %>%
      add_title("Supply sector information for ces (climate engineering services) sector") %>%
      add_units("NA") %>%
      add_comments("For dac sector, the supply sector information (output.unit, input.unit, price.unit, logit.year.fillout, logit.exponent) from A999.sector is expended into all GCAM regions") %>%
      add_legacy_name("L2999.Supplysector_dac") %>%
      add_precursors("energy/A999.sector", "common/GCAM_region_names") ->
      L2999.Supplysector_dac

    L2999.FinalEnergyKeyword_dac %>%
      add_title("Supply sector keywords for dac sector") %>%
      add_units("NA") %>%
      add_comments("For dac sector, the supply sector final energy keywords from A999.sector are expended into all GCAM regions") %>%
      add_legacy_name("L2999.FinalEnergyKeyword_dac") %>%
      add_precursors("energy/A999.sector", "common/GCAM_region_names") ->
      L2999.FinalEnergyKeyword_dac

    L2999.SubsectorLogit_dac %>%
      add_title("Subsector logit exponents of dac sector") %>%
      add_units("Unitless") %>%
      add_comments("For dac sector, the subsector logit exponents from A999.subsector_logit are expanded into all GCAM regions") %>%
      add_legacy_name("L2999.SubsectorLogit_dac") %>%
      add_precursors("energy/A999.subsector_logit", "common/GCAM_region_names") ->
      L2999.SubsectorLogit_dac

    L2999.SubsectorShrwtFllt_dac %>%
      add_title("Subsector shareweights of dac sector") %>%
      add_units("unitless") %>%
      add_comments("For dac sector, the subsector shareweights from A999.subsector_shrwt are expanded into all GCAM regions") %>%
      add_legacy_name("L2999.SubsectorShrwtFllt_dac") %>%
      add_precursors("energy/A999.subsector_shrwt", "common/GCAM_region_names") ->
      L2999.SubsectorShrwtFllt_dac

    L2999.SubsectorInterp_dac %>%
      add_title("Subsector shareweight interpolation of dac sector") %>%
      add_units("NA") %>%
      add_comments("For dac sector, the subsector shareweight interpolation function infromation from A999.subsector_interp is expanded into all GCAM regions") %>%
      add_legacy_name("L2999.SubsectorInterp_dac") %>%
      add_precursors("energy/A999.subsector_interp", "common/GCAM_region_names") ->
      L2999.SubsectorInterp_dac

    L2999.StubTech_dac %>%
      add_title("Identification of stub technologies of dac") %>%
      add_units("NA") %>%
      add_comments("For dac sector, the stub technologies from A999.globaltech_shrwt are expanded into all GCAM regions") %>%
      add_legacy_name("L2999.StubTech_dac") %>%
      add_precursors("energy/A999.globaltech_shrwt", "common/GCAM_region_names") ->
      L2999.StubTech_dac

    L2999.GlobalTechShrwt_dac %>%
      add_title("Shareweights of global dac technologies") %>%
      add_units("Unitless") %>%
      add_comments("For dac sector, the share weights from A999.globaltech_shrwt are interpolated into all base years and future years") %>%
    #  add_legacy_name("L2999.GlobalTechShrwt_dac") %>%
      add_precursors("energy/A999.globaltech_shrwt_ssp1","energy/A999.globaltech_shrwt_ssp2","energy/A999.globaltech_shrwt_ssp3","energy/A999.globaltech_shrwt_ssp4","energy/A999.globaltech_shrwt_ssp5") ->
      #add_precursors("energy/A999.globaltech_shrwt") ->
      L2999.GlobalTechShrwt_dac

    L2999.GlobalTechCoef_dac %>%
      add_title("Energy inputs and coefficients of dac technologies") %>%
      add_units("airCO2 input is unitless (Mt airCO2 per Mt dac); all others are GJ per kg (EJ of energy per Mt of dac)") %>%
      add_comments("For dac sector, the energy use coefficients from A999.globaltech_coef are interpolated into all model years") %>%
      #add_legacy_name("L2999.GlobalTechCoef_dac") %>%
      add_precursors("energy/A999.globaltech_coef_ssp1","energy/A999.globaltech_coef_ssp2","energy/A999.globaltech_coef_ssp3","energy/A999.globaltech_coef_ssp4","energy/A999.globaltech_coef_ssp5") ->
      L2999.GlobalTechCoef_dac



    L2999.GlobalTechCost_dac %>%
      add_title("Non-energy costs of global dac manufacturing technologies") %>%
      add_units("1975$/kg for supplysector dac; 1975$/GJ for supplysector process heat dac") %>%
      add_comments("Includes non-energy related capture costs only per kgC captured from the atmosphere. Storage costs will be computed endogenously through the carbon storage markets. Additional non-energy cost of process heat dac assumed zero.") %>%
      #add_legacy_name("L2999.GlobalTechCost_dac") %>%
      add_precursors("energy/A999.globaltech_cost_ssp1","energy/A999.globaltech_cost_ssp2","energy/A999.globaltech_cost_ssp3","energy/A999.globaltech_cost_ssp4","energy/A999.globaltech_cost_ssp5") ->
      L2999.GlobalTechCost_dac

    L2999.GlobalTechCapture_dac %>%
      add_title("CO2 capture fractions from global dac production technologies with CCS") %>%
      add_units("Unitless") %>%
      add_comments("For dac sector, the remove fractions from A999.globaltech_co2capture are interpolated into all model years") %>%
      add_legacy_name("L2999.GlobalTechCapture_dac") %>%
      add_precursors("energy/A999.globaltech_co2capture") ->
      L2999.GlobalTechCapture_dac


    L2999.PerCapitaBased_dac %>%
      add_title("per-capita based flag for dac exports final demand") %>%
      add_units("NA") %>%
      add_comments("Per-capita based flags for dac from A999.demand are expanded into all GCAM regions") %>%
      add_legacy_name("L2999.PerCapitaBased_dac") %>%
      add_precursors("energy/A999.demand", "common/GCAM_region_names") ->
      L2999.PerCapitaBased_dac


    L2999.PriceElasticity_dac %>%
      add_title("price elasticity for dac") %>%
      add_units("Unitless") %>%
      add_comments("The elasticity values from A999.demand are expanded into all GCAM_regions") %>%
      add_legacy_name("L2999.PriceElasticity_dac") %>%
      add_precursors("energy/A999.demand", "common/GCAM_region_names") ->
      L2999.PriceElasticity_dac

    L2999.StubTechProd_dac %>%
      add_title("calibrated ces values") %>%
      add_units("Mt") %>%
      add_comments("Values are calculated using L1999.out_Mt_R_dac_Yh then added GCAM region information and supplysector, subsector, and technology information") %>%
      add_legacy_name("L2999.StubTechProd_dac") %>%
      add_precursors("energy/calibrated_techs_ces", "L1999.out_Mt_R_dac_Yh", "common/GCAM_region_names") ->
      L2999.StubTechProd_dac

    L2999.BaseService_dac %>%
      add_title("base-year service output of dac") %>%
      add_units("Mt") %>%
      add_comments("Transformed from L2999.StubTechProd_dac by adding energy.final.demand from A999.demand") %>%
      add_legacy_name("L2999.BaseService_dac") %>%
      add_precursors("energy/A999.demand", "energy/calibrated_techs_ces", "L1999.out_Mt_R_dac_Yh", "common/GCAM_region_names") ->
      L2999.BaseService_dac

    L2999.GlobalTechSCurve_dac %>%
      add_title("Global tech lifetime and s-curve retirement function") %>%
      add_units("year for lifetime and halflife; Unitless for steepness") %>%
      add_comments("The values are extracted from L2999.globaltech_retirement for entries that half life value is not NA") %>%
      add_legacy_name("L2999.GlobalTechSCurve_dac") %>%
      add_precursors("energy/A999.globaltech_retirement") ->
      L2999.GlobalTechSCurve_dac

    L2999.GlobalTechProfitShutdown_dac %>%
      add_title("Global tech profit shutdown decider") %>%
      add_units("Unitless") %>%
      add_comments("The values are extracted from L2999.globaltech_retirement for entries that median shutdown point is not NA") %>%
      add_legacy_name("L2999.GlobalTechProfitShutdown_dac") %>%
      add_precursors("energy/A999.globaltech_retirement") ->
      L2999.GlobalTechProfitShutdown_dac

    return_data(L2999.Supplysector_dac, L2999.FinalEnergyKeyword_dac, L2999.SubsectorLogit_dac,
                L2999.SubsectorShrwtFllt_dac, L2999.SubsectorInterp_dac,
                L2999.GlobalTechCost_dac,
                L2999.GlobalTechCost_dac_ssp1,L2999.GlobalTechCost_dac_ssp2,L2999.GlobalTechCost_dac_ssp3,L2999.GlobalTechCost_dac_ssp4,L2999.GlobalTechCost_dac_ssp5,
                L2999.StubTech_dac,
                L2999.GlobalTechShrwt_dac_ssp1,L2999.GlobalTechShrwt_dac_ssp2,L2999.GlobalTechShrwt_dac_ssp3,L2999.GlobalTechShrwt_dac_ssp4,L2999.GlobalTechShrwt_dac_ssp5,
                L2999.GlobalTechShrwt_dac,
                L2999.GlobalTechCoef_dac,
                L2999.GlobalTechCoef_dac_ssp1, L2999.GlobalTechCoef_dac_ssp2, L2999.GlobalTechCoef_dac_ssp3, L2999.GlobalTechCoef_dac_ssp4, L2999.GlobalTechCoef_dac_ssp5,
                L2999.GlobalTechCapture_dac,
                L2999.PerCapitaBased_dac,
                L2999.PriceElasticity_dac,L2999.StubTechProd_dac,L2999.BaseService_dac,L2999.GlobalTechSCurve_dac,
                L2999.GlobalTechProfitShutdown_dac)
  } else {
    stop("Unknown command")
  }
}

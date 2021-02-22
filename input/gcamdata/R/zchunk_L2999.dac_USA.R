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
module_gcamdata_L2999.dac_USA <- function(command, ...) {



  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/A999.calibration_state",
             FILE = "energy/calibrated_techs_ces",
             FILE = "energy/A999.demand",
             "L2999.GlobalTechCoef_dac",
             "L2999.Supplysector_dac",
             "L2999.FinalEnergyKeyword_dac",
             "L2999.SubsectorLogit_dac",
             "L2999.SubsectorShrwtFllt_dac",
             "L2999.SubsectorInterp_dac",
             "L2999.StubTech_dac",
             "L2999.PerCapitaBased_dac",
             "L2999.PriceElasticity_dac"))
#"L2999.StubTechProd_dac"

  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2999.DeleteSupplysector_USAdac",
             "L2999.DeleteFinalDemand_USAdac",
             "L2999.StubTechCoef_dac_USA",
             "L2999.SubsectorLogit_dac_USA",
             "L2999.SubsectorShrwtFllt_dac_USA",
             "L2999.SubsectorInterp_dac_USA",
             "L2999.StubTech_dac_USA",
             "L2999.PerCapitaBased_dac_USA",
             "L2999.PriceElasticity_dac_USA",
             "L2999.FinalEnergyKeyword_dac_USA",
             "L2999.BaseService_dac_USA",
             "L2999.Supplysector_dac_USA",
             "L2999.StubTechProd_dac_USA"
             ))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs_ces")
    A999.calibration_state <- get_data(all_data, "gcam-usa/A999.calibration_state")
    A999.demand <- get_data(all_data, "energy/A999.demand")
    #A999.globaltech_coef <- get_data(all_data, "energy/A999.globaltech_coef_ssp2")

    L2999.GlobalTechCoef_dac <- get_data(all_data, "L2999.GlobalTechCoef_dac", strip_attributes = TRUE)
    L2999.Supplysector_dac <- get_data(all_data, "L2999.Supplysector_dac", strip_attributes = TRUE)
    L2999.FinalEnergyKeyword_dac <- get_data(all_data, "L2999.FinalEnergyKeyword_dac", strip_attributes = TRUE)
    L2999.SubsectorLogit_dac <- get_data(all_data, "L2999.SubsectorLogit_dac", strip_attributes = TRUE)
    L2999.SubsectorShrwtFllt_dac <- get_data(all_data, "L2999.SubsectorShrwtFllt_dac", strip_attributes = TRUE)
    L2999.SubsectorInterp_dac <- get_data(all_data, "L2999.SubsectorInterp_dac", strip_attributes = TRUE)
    L2999.StubTech_dac <- get_data(all_data, "L2999.StubTech_dac", strip_attributes = TRUE)
    L2999.PerCapitaBased_dac <- get_data(all_data, "L2999.PerCapitaBased_dac", strip_attributes = TRUE)
    L2999.PriceElasticity_dac <- get_data(all_data, "L2999.PriceElasticity_dac", strip_attributes = TRUE)
    #L2999.StubTechProd_dac <- get_data(all_data, "L2999.StubTechProd_dac", strip_attributes = TRUE)







    # ===================================================
    # 0. Give binding for variable names used in pipeline
    state <- region <- supplysector <- energy.final.demand <- region <- year <-
      value <- calibration <- sector <- subsector <- technology <- calOutputValue <-
      subs.share.weight <- share.weight.year <- fuel <- minicam.energy.input <-
      coefficient <- market.name <- grid_region <- stub.technology <- calibrated.value <-
      tech.share.weight <- object <- NULL

    # ===================================================
    # 1. Perform computations


    states_subregions %>%
      select(state) ->
      dac_states

    # 1a. Supplysector information
    # L2999.Supplysector_dac: Supply sector information for dac sector

    L2999.Supplysector_dac %>%
      filter(region == gcam.USA_REGION) %>%
      select(region, supplysector) %>%
      # Mutate to remove attributes.
      mutate(region = region) ->
      L2999.DeleteSupplysector_USAdac

    L2999.PerCapitaBased_dac %>%
      filter(region == gcam.USA_REGION) %>%
      select(region, energy.final.demand) %>%
      # Mutate to remove attributes.
      mutate(region = region) ->
      L2999.DeleteFinalDemand_USAdac


    dac_USA_processing <- function(data, dac_states) {

      # Subset the input data frame for the USA region. The subsetted data will be used
      # to check to see if the data frame needs to be processed, it's assumed that if the USA
      # is not found in the region column that regions have already been processed.

      check_df <- filter(data, region == gcam.USA_REGION)

      if(nrow(check_df) == 0) {

        # This does not change the entries of the data frame but will strip the attributes
        # from the input data frame.
        new_data <- mutate(data, region = region)

      } else {

        # If the input data frame contains USA region information
        # then expand the input data to all states.

        data %>%
          filter(region == gcam.USA_REGION) %>%
          write_to_all_states(names = names(data)) %>%
          filter(region %in% dac_states[["state"]]) ->
          new_data

      }

      return(new_data)
    } # end of function

    # Use the dac_USA_processing function to check and or process the following data frames so that
    # all of the output data frames contain information for all states.
    L2999.Supplysector_dac_USA <- dac_USA_processing(L2999.Supplysector_dac, dac_states)
    L2999.FinalEnergyKeyword_dac_USA <- dac_USA_processing(L2999.FinalEnergyKeyword_dac, dac_states)
    L2999.SubsectorLogit_dac_USA <- dac_USA_processing(L2999.SubsectorLogit_dac, dac_states)
    L2999.SubsectorShrwtFllt_dac_USA <- dac_USA_processing(L2999.SubsectorShrwtFllt_dac, dac_states)
    L2999.SubsectorInterp_dac_USA <- dac_USA_processing(L2999.SubsectorInterp_dac, dac_states)
    L2999.StubTech_dac_USA <- dac_USA_processing(L2999.StubTech_dac, dac_states)
    L2999.PerCapitaBased_dac_USA <- dac_USA_processing(L2999.PerCapitaBased_dac, dac_states)
    L2999.PriceElasticity_dac_USA <- dac_USA_processing(L2999.PriceElasticity_dac, dac_states)

    L2999.GlobalTechCoef_dac %>%
      mutate(region=gcam.USA_REGION) ->
    L2999.GlobalTechCoef_dac

    L2999.GlobalTechCoef_dac_USA <- dac_USA_processing(L2999.GlobalTechCoef_dac, dac_states)




    A999.calibration_state %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calOutputValue = signif(value, energy.DIGITS_CALOUTPUT),
             region = state ) ->
      L2999.StubTechProd_dac_USA


    # Subset the calibrated intermediate sectors and fuels to supplysector / subsector / technology
    # mapping file for unique sector / calibration / supplysector/ subsector / technology combinations.
    # This tibble will be used to add dac sector information add to the state dac
    # input table.
    calibrated_techs %>%
      # We are only interested in the technology IDs where calibration = output.
      filter(calibration == "output") %>%
      select(sector, calibration, supplysector, subsector, technology) %>%
      distinct ->
      calibrated_techs_dac_sector_info

    # Combine the dac sector information found above and the stub-technology calibrated
    # dac production into a single data frame.
    L2999.StubTechProd_dac_USA %>%
      left_join_error_no_match(calibrated_techs_dac_sector_info, by = "sector") %>%
      select(state, sector, calOutputValue, year, region, supplysector, subsector, technology) ->
      L2999.StubTechProd_dac_USA

    # Add share weight information to the state dac production data frame and format.
    L2999.StubTechProd_dac_USA %>%
      mutate(stub.technology = technology,
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(region, supplysector, subsector, stub.technology, year, calOutputValue,
             share.weight.year, subs.share.weight, tech.share.weight) ->
      L2999.StubTechProd_dac_USA


    # Create the coefficients of dac production technologies input table.
    #
    # Start by creating a data frame of unique sector, fuel, supplysector, subsector,
    # technology, and minicam.energy.input combinations. This data frame will be used to
    # add sector information to input-ouput coefficients for state dac production.
    calibrated_techs %>%
      select(sector, fuel, supplysector, subsector, technology, minicam.energy.input) %>%
      distinct ->
      dac_production_technologies

    # Add dac sector information to the data frame of input-output coefficients of
    # dac production by state.
    #L1999.IO_GJkg_state_dac_F_Yh %>%
    #  left_join_error_no_match(dac_production_technologies, by = c("sector", "fuel")) %>%
    #  select(state,fuel, sector, year, value, supplysector, subsector, technology, minicam.energy.input) ->
    #  L2999.IO_GJkg_state_dac_F_Yh

    # Interpolate dac production default coefficients to future years.
    #
    # First change format of the the production default coefficients data frame from wide to long.
    #A999.globaltech_coef_long <- gather_years(A999.globaltech_coef)

    # Then linearly interpolate the default coefficients for future years. In the next step these
    # values will be added to the state input-output coefficients data frame.
    #A999.globaltech_coef_long %>%
    #  complete(nesting(supplysector, subsector, minicam.energy.input, technology), year = c(year, MODEL_FUTURE_YEARS)) %>%
    ##  arrange(supplysector, subsector, minicam.energy.input, technology, year) %>%
    #  group_by(supplysector, subsector, minicam.energy.input, technology) %>%
    #  mutate(value = approx_fun(year, value), value = signif(value, energy.DIGITS_COEFFICIENT)) %>%
    #  ungroup ->
    #  L2999.globaltech_coef

    #write.csv(L2999.globaltech_coef,"L2999.globaltech_coef.csv")



    L2999.GlobalTechCoef_dac_USA %>%
      rename(supplysector=sector.name,
             subsector=subsector.name,
             value=coefficient)->
      L2999.StubTechCoef_dac_USA


    # Add market information. process heat are state level markets where as electricity
    # comes from the grid level.
    L2999.StubTechCoef_dac_USA %>%
      mutate(market.name = region,
             market.name = if_else(grepl("elec", minicam.energy.input), gcam.USA_REGION, market.name)) %>%
      # replace market name with the grid region name if the minicam.energy.input is
      # considered a regional fuel market
      left_join(states_subregions %>%
                  select(region = state, grid_region),
                by = "region") %>%
      mutate(market.name = if_else(minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS,
                                   grid_region, market.name)) %>%
      select(-grid_region) ->
      L2999.StubTechCoef_dac_USA


    # Change market name to reflect the fact that electricity is consumed from state markets.
    L2999.StubTechCoef_dac_USA %>%
      mutate(replace = if_else(minicam.energy.input %in% gcamusa.ELECT_TD_SECTORS, 1, 0),
             market.name = if_else(replace == 1, region, market.name)) %>%
      select(-replace) ->
      L2999.StubTechCoef_dac_USA

    #change market name for water use to USA
    #L2999.StubTechCoef_dac_USA %>%
    #  mutate(market.name = if_else(grepl("water", minicam.energy.input), 'USA', market.name)) ->
    #  L2999.StubTechCoef_dac_USA

    #change market name for airCO2 to global
    L2999.StubTechCoef_dac_USA %>%
      mutate(market.name = if_else(grepl("airCO2", minicam.energy.input), 'global', market.name)) ->
      L2999.StubTechCoef_dac_USA

    L2999.StubTechCoef_dac_USA %>%
      rename(stub.technology=technology,
             coefficient=value) ->
      L2999.StubTechCoef_dac_USA

    #use ssp2 middle of road assumption for USA permutation
    L2999.StubTechCoef_dac_USA %>%
      filter(scenario=="ssp2") %>%
      select(-scenario)->
      L2999.StubTechCoef_dac_USA


    # Create the base-year service output for dac final demand input table.
    #
    # Since base service is equal to the output of the dac supplysector use
    # the coefficients from the stub technology production data frame and add
    # energy.final.demand form dac final demand perCapitaBased and price elasticity
    # assumption file.

    L2999.StubTechProd_dac_USA %>%
      mutate(energy.final.demand = A999.demand$energy.final.demand) %>%
      select(region, energy.final.demand, year, base.service = calOutputValue) ->
      L2999.BaseService_dac_USA



    # ===================================================
    # Produce outputs

    L2999.DeleteSupplysector_USAdac %>%
      add_title("Supply sector information for ces (climate engineering services) sector") %>%
      add_units("NA") %>%
      add_comments("For dac sector, the supply sector information (output.unit, input.unit, price.unit, logit.year.fillout, logit.exponent) from A999.sector is expended into all GCAM regions") %>%
      add_legacy_name("L2999.Supplysector_dac") %>%
      add_precursors("L2999.Supplysector_dac") ->
      L2999.DeleteSupplysector_USAdac


    L2999.DeleteFinalDemand_USAdac %>%
      add_title("Supply sector keywords for dac sector") %>%
      add_units("NA") %>%
      add_comments("For dac sector, the supply sector final energy keywords from A999.sector are expended into all GCAM regions") %>%
      add_legacy_name("L2999.FinalEnergyKeyword_dac") %>%
      add_precursors("L2999.FinalEnergyKeyword_dac") ->
      L2999.DeleteFinalDemand_USAdac

    L2999.SubsectorLogit_dac_USA %>%
      add_title("Supply sector keywords for dac sector in all states") %>%
      add_units("Unitless") %>%
      add_comments("Expanded subsector logit exponents to to prospective dac states in region USA") %>%
      add_legacy_name("L2999.SubsectorLogit_dac_USA") %>%
      add_precursors("L2999.SubsectorLogit_dac") ->
      L2999.SubsectorLogit_dac_USA

    L2999.SubsectorShrwtFllt_dac_USA %>%
      add_title("Subsector shareweights of dac sector in dac producing states") %>%
      add_units("Unitless") %>%
      add_comments("Expanded subsector shareweights to to all states in region USA") %>%
      add_legacy_name("L2999.SubsectorShrwtFllt_dac_USA") %>%
      add_precursors("L2999.SubsectorShrwtFllt_dac") ->
      L2999.SubsectorShrwtFllt_dac_USA

    L2999.SubsectorInterp_dac_USA %>%
      add_title("Subsector shareweight interpolation of dac sector in all states") %>%
      add_units("NA") %>%
      add_comments("Expanded subsector shareweight interpolation to all states in region USA") %>%
      add_legacy_name("L2999.SubsectorInterp_dac_USA") %>%
      add_precursors("L2999.SubsectorInterp_dac") ->
      L2999.SubsectorInterp_dac_USA

    L2999.StubTech_dac_USA %>%
      add_title("Identification of stub technologies of dac in all states") %>%
      add_units("NA") %>%
      add_comments("Expanded identification of stub technologies of dac to all states in region USA") %>%
      add_legacy_name("L2999.StubTech_dac_USA") %>%
      add_precursors("L2999.StubTech_dac") ->
      L2999.StubTech_dac_USA

    L2999.PerCapitaBased_dac_USA %>%
      add_title("Per-capita based flag for dac exports final demand in all states") %>%
      add_units("NA") %>%
      add_comments("Expanded Per-capita based flag for dac exports final demand to all states in region USA") %>%
      add_legacy_name("L2999.PerCapitaBased_dac_USA") %>%
      add_precursors("L2999.PerCapitaBased_dac") ->
      L2999.PerCapitaBased_dac_USA

    L2999.PriceElasticity_dac_USA %>%
      add_title("Price elasticity for dac in all states") %>%
      add_units("Unitless") %>%
      add_comments("Expanded price elasticity for dac to all states in region USA") %>%
      add_legacy_name("L2999.PriceElasticity_dac_USA") %>%
      add_precursors("L2999.PriceElasticity_dac") ->
      L2999.PriceElasticity_dac_USA

    L2999.FinalEnergyKeyword_dac_USA %>%
      add_title("Subsector logit exponents of dac sector in dac producing states") %>%
      add_units("Unitless") %>%
      add_comments("Expanded supply sector keywords information dac producing states in region USA") %>%
      add_legacy_name("L2999.FinalEnergyKeyword_dac_USA") %>%
      add_precursors("L2999.FinalEnergyKeyword_dac") ->
      L2999.FinalEnergyKeyword_dac_USA


    L2999.StubTechCoef_dac_USA %>%
      add_title("dac technologies by state") %>%
      add_units("coefficient = GJ/kg (gigajoules per kilogram of dac)") %>%
      add_comments("Rename markets with regional gird name if using regional regional fuel markets") %>%
      add_legacy_name("L2999.StubTechCoef_dac_USA") %>%
      add_precursors("L2999.GlobalTechCoef_dac","gcam-usa/states_subregions") ->
      L2999.StubTechCoef_dac_USA

    L2999.Supplysector_dac_USA %>%
      add_title("Supply sector information for dac sector in all states") %>%
      add_units("NA") %>%
      add_comments("Expanded supply sector information to dac producing states in region USA") %>%
      add_legacy_name("L2999.Supplysector_dac_USA") %>%
      add_precursors("L2999.Supplysector_dac") ->
      L2999.Supplysector_dac_USA

    L2999.StubTechProd_dac_USA %>%
      #add_title("Dummy calibration for all dac stub technologies for all states") %>%
      add_units("NA") %>%
      add_comments("Added dac calibration coefficients to all states") %>%
      add_legacy_name("L2999.StubTechProd_dac_USA") %>%
      add_precursors("gcam-usa/A999.calibration_state","energy/A999.demand","energy/calibrated_techs_ces") ->
      L2999.StubTechProd_dac_USA

    #write.csv(L2999.DeleteSupplysector_USAdac,"L2999.DeleteSupplysector_USAdac.csv")
    #write.csv(L2999.DeleteFinalDemand_USAdac,"L2999.DeleteFinalDemand_USAdac.csv")
    #write.csv(L2999.SubsectorLogit_dac_USA,"L2999.SubsectorLogit_dac_USA.csv")
    #write.csv(L2999.SubsectorShrwtFllt_dac_USA,"L2999.SubsectorShrwtFllt_dac_USA.csv")
    #write.csv(L2999.SubsectorInterp_dac_USA,"L2999.SubsectorInterp_dac_USA.csv")
    #write.csv(L2999.StubTech_dac_USA,"L2999.StubTech_dac_USA.csv")
    #write.csv(L2999.PerCapitaBased_dac_USA,"L2999.PerCapitaBased_dac_USA.csv")
    #write.csv(L2999.PriceElasticity_dac_USA,"L2999.PriceElasticity_dac_USA.csv")
    #write.csv(L2999.FinalEnergyKeyword_dac_USA,"L2999.FinalEnergyKeyword_dac_USA.csv")
    #write.csv(L2999.BaseService_dac_USA,"L2999.BaseService_dac_USA.csv")
    #write.csv(L2999.StubTechCoef_dac_USA,"L2999.StubTechCoef_dac_USA.csv")
    #write.csv(L2999.Supplysector_dac_USA,"L2999.Supplysector_dac_USA.csv")
    #write.csv(L2999.StubTechProd_dac_USA,"L2999.StubTechProd_dac_USA.csv")

    return_data(L2999.DeleteSupplysector_USAdac,
                L2999.DeleteFinalDemand_USAdac,
                L2999.SubsectorLogit_dac_USA,
                L2999.SubsectorShrwtFllt_dac_USA,
                L2999.SubsectorInterp_dac_USA,
                L2999.StubTech_dac_USA,
                L2999.PerCapitaBased_dac_USA,
                L2999.PriceElasticity_dac_USA,
                L2999.FinalEnergyKeyword_dac_USA,
                L2999.BaseService_dac_USA,
                L2999.StubTechCoef_dac_USA,
                L2999.Supplysector_dac_USA,
                L2999.StubTechProd_dac_USA)
  } else {
    stop("Unknown command")
  }
}

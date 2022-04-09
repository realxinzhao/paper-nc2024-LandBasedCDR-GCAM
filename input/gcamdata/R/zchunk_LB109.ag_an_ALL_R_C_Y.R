# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_LB109.ag_an_ALL_R_C_Y
#'
#' Calculate primary agricultural good and animal product mass balances, by region / commodity / year.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L109.ag_ALL_Mt_R_C_Y}, \code{L109.an_ALL_Mt_R_C_Y}. The corresponding file in the
#' original data system was \code{LB109.ag_an_ALL_R_C_Y.R} (aglu level1).
#' @details This chunk combines all flow tables of GCAM agricultural commodities, calculates mass balances by
#' GCAM region, commodity and year, and adjusts global and regional net exports to remove negative other uses.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else group_by left_join mutate pull select summarise
#' @importFrom tidyr gather spread
#' @author RC April 2017
module_aglu_LB109.ag_an_ALL_R_C_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L101.ag_Food_Mt_R_C_Y",
              "L101.ag_Prod_Mt_R_C_Y",
              "L105.an_Food_Mt_R_C_Y",
              "L105.an_Prod_Mt_R_C_Y",
              "L108.ag_Feed_Mt_R_C_Y",
              "L108.ag_NetExp_Mt_R_FodderHerb_Y",
              "L122.in_Mt_R_C_Yh",
             # temp added XZ
              "L1091.GrossTrade_Mt_R_C_Y"
             ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L109.ag_ALL_Mt_R_C_Y",
             "L109.an_ALL_Mt_R_C_Y"))
  } else if(command == driver.MAKE) {

    year <- value <- GCAM_region_ID <- GCAM_commodity <- . <- flow <- Feed_Mt <-
      Prod_Mt <- NetExp_Mt <- Supply_Mt <- Food_Mt <- Biofuels_Mt <-
      OtherUses_Mt <- NegOtherUses_Mt <- OtherUses_Mt_adj <-
      GlobalOtherUses_Mt <- NetExp_Mt_adj <- NetExpAdjFrac <-
      GlobalNetExpAdj <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs ----
    L101.ag_Food_Mt_R_C_Y <- get_data(all_data, "L101.ag_Food_Mt_R_C_Y")
    L101.ag_Prod_Mt_R_C_Y <- get_data(all_data, "L101.ag_Prod_Mt_R_C_Y")
    L105.an_Food_Mt_R_C_Y <- get_data(all_data, "L105.an_Food_Mt_R_C_Y")
    L105.an_Prod_Mt_R_C_Y <- get_data(all_data, "L105.an_Prod_Mt_R_C_Y")
    L108.ag_Feed_Mt_R_C_Y <- get_data(all_data, "L108.ag_Feed_Mt_R_C_Y")
    L108.ag_NetExp_Mt_R_FodderHerb_Y <- get_data(all_data, "L108.ag_NetExp_Mt_R_FodderHerb_Y")
    L122.in_Mt_R_C_Yh <- get_data(all_data, "L122.in_Mt_R_C_Yh")

    L1091.GrossTrade_Mt_R_C_Y <- get_data(all_data, "L1091.GrossTrade_Mt_R_C_Y")


    # This chunk will adjust bioenergy feedstock and also feed demand using others
    # There will be some assumptions needed
    # Trade of secondary products will also be traced here
    # New balance
    # c("BeginStock_Mt", "Prod_Mt", "GrossImp_Mt", "Supply_Mt",
    #   "Food_Mt", "Feed_Mt", "Biofuels_Mt","GrossExp_Mt", "NetExp_Mt", "Losses", "OtherUses_Mt", "EndStock_Mt")
    # Stock variation is not included here yet!
    # Here, when other use is negative, net trade is adjusted
    # There will be concerns on primary vs. secondary trade and trade within or across aggregated regions.

    # List of commodities in production table ----
    L101.ag_Prod_Mt_R_C_Y %>%
      pull(GCAM_commodity) %>%
      unique() -> Primary_commodities
    # List of any commodities (e.g. pasture, residue, scavenging) in feed but not in production table
    L108.ag_Feed_Mt_R_C_Y %>%
      filter(!(GCAM_commodity %in% Primary_commodities)) %>%
      pull(GCAM_commodity) %>%
      unique() -> Feed_commodities

    L105.an_Prod_Mt_R_C_Y %>%
      pull(GCAM_commodity) %>%
      unique() -> Meat_commodities

    # Part 1: Primary agricultural goods ----

    ## Combine all flow tables ----

  L1091.GrossTrade_Mt_R_C_Y %>%
    filter(GCAM_commodity %in% Primary_commodities) %>%
    mutate(NetExp_Mt = GrossExp_Mt - GrossImp_Mt) %>%
    gather(flow, value, -GCAM_region_ID, -GCAM_commodity, -year) %>%
    # Name the flows in each table, and combine all tables
      bind_rows(L108.ag_NetExp_Mt_R_FodderHerb_Y %>% mutate(flow = "NetExp_Mt")) %>%
      bind_rows(mutate(L101.ag_Prod_Mt_R_C_Y, flow = "Prod_Mt")) %>%
      bind_rows(mutate(L101.ag_Food_Mt_R_C_Y, flow = "Food_Mt")) %>%
      bind_rows(mutate(L108.ag_Feed_Mt_R_C_Y, flow = "Feed_Mt")) %>%
      bind_rows(mutate(L122.in_Mt_R_C_Yh, flow = "Biofuels_Mt")) %>%
      # Get all combinations of each GCAM_commodity and flow, by spreading to wide format
      spread(flow, value) %>%
      # adjust for feedherb
      mutate(GrossExp_Mt = if_else(is.na(GrossExp_Mt) & NetExp_Mt > 0, NetExp_Mt, GrossExp_Mt),
             GrossExp_Mt = if_else(is.na(GrossExp_Mt) & NetExp_Mt <= 0, 0, GrossExp_Mt),
             GrossImp_Mt = GrossExp_Mt - NetExp_Mt) %>%
      filter(year %in% aglu.AGLU_HISTORICAL_YEARS) %>%
      # Set missing values in the complete combinations to zero
      dplyr::mutate_if(is.numeric, list(~ replace(., is.na(.), 0))) %>%
      # For any feed commodities (e.g. pasture, residue, scavenging) that are not reported in production or trade table,
      # assume all production are domestic, and set production = feed
      mutate(Prod_Mt = if_else(GCAM_commodity %in% Feed_commodities, Feed_Mt, Prod_Mt),
             # Calculate the domestic supply
             Supply_Mt = Prod_Mt - NetExp_Mt,
             # Calculate other uses
             OtherUses_Mt = Supply_Mt - Food_Mt - Feed_Mt - Biofuels_Mt)  ->
    L109.ag_ALL_Mt_R_C_Y


  ## Adjust global and regional crop mass balances to remove net negative other uses ----
  # Assign negative other net uses to imports, and adjust global trade to maintain balances
  # Changes in global net exports are apportioned among regions with positive other uses, according to regional shares

  if(any(L109.ag_ALL_Mt_R_C_Y$OtherUses_Mt < 0)){
  # Filter commodities that may be imbalanced
  L109.ag_ALL_Mt_R_C_Y %>%
    filter(GCAM_commodity %in% Primary_commodities) %>%
    filter(GCAM_commodity != "FodderHerb") %>%
    mutate(negOther = if_else(OtherUses_Mt < 0, "Neg", "Pos") ) ->
    L109.ag_ALL_Mt_R_C_Y_1

  # Ship negative otheruse to other regions with positives
  # positive regions will be scaled down simplely
  L109.ag_ALL_Mt_R_C_Y_1 %>%
    group_by(GCAM_commodity, year, negOther) %>%
    # world processed by neg or pos
    summarise(OtherUses_Mt = sum(OtherUses_Mt, na.rm = T)) %>%
    ungroup() %>%
    spread(negOther, OtherUses_Mt) %>%
    mutate(World_pos_scaler = -Neg/Pos) %>%
    select(-Neg, -Pos)->
    Pos_OtherUse_scaler

  # NA scalers mean no negative other uses in all region for the item
  L109.ag_ALL_Mt_R_C_Y_1 %>%
    left_join(Pos_OtherUse_scaler, by = c("year", "GCAM_commodity")) %>%
    filter(is.na(World_pos_scaler)) %>%
    select(-negOther, -World_pos_scaler) %>%
    # all positive other use bind ones with negative values
    bind_rows(
      L109.ag_ALL_Mt_R_C_Y_1 %>%
        left_join(Pos_OtherUse_scaler, by = c("year", "GCAM_commodity")) %>%
        filter(!is.na(World_pos_scaler)) %>%
        # Adjust negative OtherUse first by moving to import
        # set neg OtherUse to zero
        mutate(GrossImp_Mt = if_else(negOther == "Neg", GrossImp_Mt - OtherUses_Mt, GrossImp_Mt),
               OtherUses_Mt = if_else(negOther == "Neg", 0, OtherUses_Mt),
               # Adjust positive OtherUse by scaling and moving to export
               # reduce positive scaler
               GrossExp_Mt = if_else(negOther == "Pos", GrossExp_Mt + OtherUses_Mt * World_pos_scaler, GrossExp_Mt),
               OtherUses_Mt = if_else(negOther == "Pos", OtherUses_Mt - OtherUses_Mt * World_pos_scaler, OtherUses_Mt)) %>%
        select(-negOther, -World_pos_scaler) %>%
        mutate(NetExp_Mt = GrossExp_Mt - GrossImp_Mt,
               Supply_Mt = Prod_Mt - NetExp_Mt)
    ) ->
    L109.ag_ALL_Mt_R_C_Y_2

  # Bind commodities that were balanced
  L109.ag_ALL_Mt_R_C_Y_2 %>%
  bind_rows(
    L109.ag_ALL_Mt_R_C_Y %>%
    filter(GCAM_commodity %in% c("FodderHerb", Feed_commodities))
    ) %>%
    gather(element, value, -GCAM_region_ID, -GCAM_commodity, -year) %>%
    mutate(value = round(value, aglu.DIGITS_CALOUTPUT))->
    L109.ag_ALL_Mt_R_C_Y_3

  L109.ag_ALL_Mt_R_C_Y <- L109.ag_ALL_Mt_R_C_Y_3 %>% spread(element, value)

  rm(L109.ag_ALL_Mt_R_C_Y_1,
     L109.ag_ALL_Mt_R_C_Y_2,
     L109.ag_ALL_Mt_R_C_Y_3)
  }

    # After these adjustments, crops may still have negative other uses if the bottom-up biofuel estimates exceed the
    # domestic supply minus known (and fixed) food and feed quantities. This needs to be addressed on a case-by-case
    # basis, but failure to address the issue here will result in negative calibration values being read to GCAM, and
    # model solution failure.
    if(any(L109.ag_ALL_Mt_R_C_Y$OtherUses_Mt < 0)){
      stop("Negative other uses, possibly due to biofuel crop requirements exceeding available domestic supply")
    }

    # Part 2: Animal commodities ----

    ## Combine all flow tables ----

    # Name the flows in each table
    L1091.GrossTrade_Mt_R_C_Y %>%
      filter(GCAM_commodity %in% Meat_commodities) %>%
      mutate(NetExp_Mt = GrossExp_Mt - GrossImp_Mt) %>%
      gather(flow, value, -GCAM_region_ID, -GCAM_commodity, -year) %>%
      bind_rows(mutate(L105.an_Prod_Mt_R_C_Y, flow = "Prod_Mt")) %>%
      bind_rows(mutate(L105.an_Food_Mt_R_C_Y, flow = "Food_Mt")) %>%
      spread(flow, value) %>%
      filter(year %in% aglu.AGLU_HISTORICAL_YEARS) %>%
      # Set missing values in the complete combinations to zero
      dplyr::mutate_if(is.numeric, list(~ replace(., is.na(.), 0))) %>%
      mutate(# Calculate the domestic supply
             Supply_Mt = Prod_Mt - NetExp_Mt,
             # Calculate other uses
             OtherUses_Mt = Supply_Mt - Food_Mt)  ->
      L109.an_ALL_Mt_R_C_Y




    if(any(L109.an_ALL_Mt_R_C_Y$OtherUses_Mt < 0)){
      # Filter commodities that may be imbalanced
      L109.an_ALL_Mt_R_C_Y %>%
        mutate(negOther = if_else(OtherUses_Mt < 0, "Neg", "Pos") ) ->
        L109.an_ALL_Mt_R_C_Y_1

      # Ship negative otheruse to other regions with positives
      # positive regions will be scaled down simplely
      L109.an_ALL_Mt_R_C_Y_1 %>%
        group_by(GCAM_commodity, year, negOther) %>%
        # world processed by neg or pos
        summarise(OtherUses_Mt = sum(OtherUses_Mt, na.rm = T)) %>%
        ungroup() %>%
        spread(negOther, OtherUses_Mt) %>%
        mutate(World_pos_scaler = -Neg/Pos) %>%
        select(-Neg, -Pos)->
        Pos_OtherUse_scaler

      # NA scalers mean no negative other uses in all region for the item
      L109.an_ALL_Mt_R_C_Y_1 %>%
        left_join(Pos_OtherUse_scaler, by = c("year", "GCAM_commodity")) %>%
        filter(is.na(World_pos_scaler)) %>%
        select(-negOther, -World_pos_scaler) %>%
        # all positive other use bind ones with negative values
        bind_rows(
          L109.an_ALL_Mt_R_C_Y_1 %>%
            left_join(Pos_OtherUse_scaler, by = c("year", "GCAM_commodity")) %>%
            filter(!is.na(World_pos_scaler)) %>%
            # Adjust negative OtherUse first by moving to import
            # set neg OtherUse to zero
            mutate(GrossImp_Mt = if_else(negOther == "Neg", GrossImp_Mt - OtherUses_Mt, GrossImp_Mt),
                   OtherUses_Mt = if_else(negOther == "Neg", 0, OtherUses_Mt),
                   # Adjust positive OtherUse by scaling and moving to export
                   # reduce positive scaler
                   GrossExp_Mt = if_else(negOther == "Pos", GrossExp_Mt + OtherUses_Mt * World_pos_scaler, GrossExp_Mt),
                   OtherUses_Mt = if_else(negOther == "Pos", OtherUses_Mt - OtherUses_Mt * World_pos_scaler, OtherUses_Mt)) %>%
            select(-negOther, -World_pos_scaler) %>%
            mutate(NetExp_Mt = GrossExp_Mt - GrossImp_Mt,
                   Supply_Mt = Prod_Mt - NetExp_Mt)
        ) ->
        L109.an_ALL_Mt_R_C_Y_2

      # round to aglu.DIGITS_CALOUTPUT
      L109.an_ALL_Mt_R_C_Y_2 %>%
        gather(element, value, -GCAM_region_ID, -GCAM_commodity, -year) %>%
        mutate(value = round(value, aglu.DIGITS_CALOUTPUT))->
        L109.an_ALL_Mt_R_C_Y_3

      L109.an_ALL_Mt_R_C_Y <- L109.an_ALL_Mt_R_C_Y_3 %>% spread(element, value)

      # For some reason 1973 pork was the only place that trade adjustments were not enough
      # because Pos_OtherUse_scaler > 1
      # But the remaining imbalance is very small
      # So assuming this was the only change needed, food may be adjusted here
      # also 1973 (5-year mean) is not used in the modeling
      # ignore for now
      # Pos_OtherUse_scaler %>% filter(World_pos_scaler>1)

      rm(L109.an_ALL_Mt_R_C_Y_1,
         L109.an_ALL_Mt_R_C_Y_2,
         L109.an_ALL_Mt_R_C_Y_3)
    }


    # Adjust self-trade to ensure export < production ----
    # this was an assumption in GCAM cpp
    # the assumption could be strong e.g., US does not product OilPalm but could export OilPalm product
    L109.ag_ALL_Mt_R_C_Y %>%
      # reduce import and export both by the same (GrossExp_Mt - Prod_Mt)
      mutate(GrossImp_Mt = if_else(GrossExp_Mt > Prod_Mt,
                                   GrossImp_Mt - (GrossExp_Mt - Prod_Mt),GrossImp_Mt),
             GrossExp_Mt = if_else(GrossExp_Mt > Prod_Mt,
                                   Prod_Mt, GrossExp_Mt)) ->
      L109.ag_ALL_Mt_R_C_Y

    L109.an_ALL_Mt_R_C_Y %>%
      # reduce import and export both by the same (GrossExp_Mt - Prod_Mt)
      mutate(GrossImp_Mt = if_else(GrossExp_Mt > Prod_Mt,
                                   GrossImp_Mt - (GrossExp_Mt - Prod_Mt),GrossImp_Mt),
             GrossExp_Mt = if_else(GrossExp_Mt > Prod_Mt,
                                   Prod_Mt, GrossExp_Mt)) ->
      L109.an_ALL_Mt_R_C_Y

    # Produce outputs
    L109.ag_ALL_Mt_R_C_Y %>%
      add_title("Primary agricultural good mass balances, by region / commodity / year.") %>%
      add_units("Mt") %>%
      add_comments("Calculate primary agricultural good mass balances by GCAM region, commodity and year") %>%
      add_comments("Adjusts global and regional net exports to remove net negative other uses") %>%
      add_legacy_name("L109.ag_ALL_Mt_R_C_Y") %>%
      add_precursors("L101.ag_Food_Mt_R_C_Y",
                     "L101.ag_Prod_Mt_R_C_Y",
                     "L108.ag_Feed_Mt_R_C_Y",
                     "L108.ag_NetExp_Mt_R_FodderHerb_Y",
                     "L122.in_Mt_R_C_Yh",
                     "L1091.GrossTrade_Mt_R_C_Y"
                     ) ->
      L109.ag_ALL_Mt_R_C_Y

    L109.an_ALL_Mt_R_C_Y %>%
      add_title("Animal product mass balances, by region / commodity / year.") %>%
      add_units("Mt") %>%
      add_comments("Calculate animal product mass balances by GCAM region, commodity and year") %>%
      add_comments("Adjusts global and regional net exports to remove net negative other uses") %>%
      add_legacy_name("L109.an_ALL_Mt_R_C_Y") %>%
      add_precursors("L105.an_Food_Mt_R_C_Y",
                     "L105.an_Prod_Mt_R_C_Y",
                     "L1091.GrossTrade_Mt_R_C_Y") ->
      L109.an_ALL_Mt_R_C_Y

    return_data(L109.ag_ALL_Mt_R_C_Y, L109.an_ALL_Mt_R_C_Y)
  } else {
    stop("Unknown command")
  }
}


# Appendix old code ----


# List of all flows for primary agricultural good balances
# ag_Flow_cols <- c("Prod_Mt", "NetExp_Mt", "Supply_Mt", "Food_Mt", "Feed_Mt", "Biofuels_Mt", "OtherUses_Mt")

# # First calculate the changes in global net exports
# if(any(L109.ag_ALL_Mt_R_C_Y$OtherUses_Mt < 0)) {
#   L109.ag_ALL_Mt_R_C_Y %>%
#     # Subset negative and positive "other uses" separately
#     mutate(NegOtherUses_Mt = if_else(OtherUses_Mt < 0, OtherUses_Mt, 0),
#            # Positive will be the new adjusted "other uses", and replace negative with zero
#            OtherUses_Mt_adj = if_else(OtherUses_Mt >= 0, OtherUses_Mt, 0),
#            # Assign negative "other uses" to imports, and calculate the adjusted regional net exports
#            NetExp_Mt_adj = NetExp_Mt + NegOtherUses_Mt) %>%
#     group_by(GCAM_commodity, year) %>%
#     # Calculate the changes in global net exports = sum of negative other uses, global other uses =  sum of positive other uses
#     summarise(GlobalNetExpAdj = sum(NegOtherUses_Mt),
#               GlobalOtherUses_Mt = sum(OtherUses_Mt_adj)) -> L109.ag_ALL_Mt_glbl_C_Y
#
#   # Second, distribute changes in global net exports among regions with positive other uses, according to regional shares
#   L109.ag_ALL_Mt_R_C_Y %>%
#     # Subset negative and positive "other uses" separately
#     mutate(NegOtherUses_Mt = if_else(OtherUses_Mt < 0, OtherUses_Mt, 0),
#            # Positive will be the new adjusted "other uses", and replace negative with zero
#            OtherUses_Mt_adj = if_else(OtherUses_Mt >= 0, OtherUses_Mt, 0),
#            # Assign negative "other uses" to imports, and calculate the adjusted regional net exports
#            NetExp_Mt_adj = NetExp_Mt + NegOtherUses_Mt) %>%
#     # Combine with global adjusted other uses and changes in global net exports
#     left_join(L109.ag_ALL_Mt_glbl_C_Y, by = c("GCAM_commodity", "year")) %>%
#     # Calculate the regional share of global adjusted other uses, the share is zero for regions with negative other uses
#     mutate(NetExpAdjFrac = if_else(GlobalOtherUses_Mt == 0, 0, OtherUses_Mt_adj / GlobalOtherUses_Mt),
#            # Allocate the changes in global net exports (total of negative other uses) among regions with positive other uses
#            NetExp_Mt = NetExp_Mt_adj - NetExpAdjFrac * GlobalNetExpAdj,
#            # Rebuild the mass balance table
#            Supply_Mt = Prod_Mt - NetExp_Mt,
#            OtherUses_Mt = Supply_Mt - Food_Mt - Biofuels_Mt - Feed_Mt) %>%
#     gather(flow, value, -GCAM_region_ID, -GCAM_commodity, -year) %>%
#     # Select only the flow variables for primary agricultural goods
#     filter(flow %in% ag_Flow_cols) %>%
#     # Re-order the flow variables so the columns are in the right order
#     mutate(flow = factor(flow, levels = ag_Flow_cols),
#            value = round(value, aglu.DIGITS_CALOUTPUT),
#            year = as.integer(year)) %>%
#     spread(flow, value) ->
#     L109.ag_ALL_Mt_R_C_Y
# }


# List of all flows for animal products
#an_Flow_cols <- c("Prod_Mt", "NetExp_Mt", "Supply_Mt", "Food_Mt", "OtherUses_Mt")

# if(any(L109.an_ALL_Mt_R_C_Y$OtherUses_Mt < 0)) {
#   L109.an_ALL_Mt_R_C_Y %>%
#     # Subset negative and positive "other uses" separately
#     mutate(NegOtherUses_Mt = if_else(OtherUses_Mt < 0, OtherUses_Mt, 0),
#            # Positive will be the new adjusted "other uses", and replace negative with zero
#            OtherUses_Mt_adj = if_else(OtherUses_Mt >= 0, OtherUses_Mt, 0),
#            # Assign negative "other uses" to imports, and calculate the adjusted regional net exports
#            NetExp_Mt_adj = NetExp_Mt + NegOtherUses_Mt) %>%
#     group_by(GCAM_commodity, year) %>%
#     # Calculate the changes in global net exports = sum of negative other uses, global other uses =  sum of positive other uses
#     summarise(GlobalNetExpAdj = sum(NegOtherUses_Mt),
#               GlobalOtherUses_Mt = sum(OtherUses_Mt_adj)) ->
#     L109.an_ALL_Mt_glbl_C_Y
#
#   # Second, distribute changes in global net exports among regions with positive other uses, according to regional shares
#   L109.an_ALL_Mt_R_C_Y %>%
#     # Subset negative and positive "other uses" separately
#     mutate(NegOtherUses_Mt = if_else(OtherUses_Mt < 0, OtherUses_Mt, 0),
#            # Positive will be the new adjusted "other uses", and replace negative with zero
#            OtherUses_Mt_adj = if_else(OtherUses_Mt >= 0, OtherUses_Mt, 0),
#            # Assign negative "other uses" to imports, and calculate the adjusted regional net exports
#            NetExp_Mt_adj = NetExp_Mt + NegOtherUses_Mt) %>%
#     # Combine with global adjusted other uses and changes in global net exports
#     left_join(L109.an_ALL_Mt_glbl_C_Y, by = c("GCAM_commodity", "year")) %>%
#     # Calculate the regional share of global adjusted other uses, the share is zero for regions with negative other uses
#     mutate(NetExpAdjFrac = if_else(GlobalOtherUses_Mt == 0, 0, OtherUses_Mt_adj / GlobalOtherUses_Mt),
#            # Allocate the changes in global net exports (total of negative other uses) among regions with positive other uses
#            NetExp_Mt = NetExp_Mt_adj - NetExpAdjFrac * GlobalNetExpAdj,
#            # Rebuild animal product mass balance table
#            Supply_Mt = Prod_Mt - NetExp_Mt,
#            OtherUses_Mt = Supply_Mt - Food_Mt) %>%
#     gather(flow, value, -GCAM_region_ID, -GCAM_commodity, -year) %>%
#     # Select only the flow variables for animal products
#     filter(flow %in% an_Flow_cols) %>%
#     # Re-order the flow variables so the columns are in the right order
#     mutate(flow = factor(flow, levels = an_Flow_cols),
#            value = round(value, aglu.DIGITS_CALOUTPUT)) %>%
#     spread(flow, value) ->
#     L109.an_ALL_Mt_R_C_Y
# }

# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_LA100.FAO_downscale_ctry
#'
#' Downscale FAO production and consumption agricultural data to AGLU countries.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.FAO_ag_HA_ha}, \code{L100.FAO_ag_Prod_t}, \code{L100.FAO_ag_Feed_t}, \code{L100.FAO_ag_Food_t}, \code{L100.FAO_an_Food_t}, \code{L100.FAO_an_Prod_t}, \code{L100.FAO_CL_kha}, \code{L100.FAO_fallowland_kha}, \code{L100.FAO_harv_CL_kha}, \code{L100.FAO_Fert_Cons_tN}, \code{L100.FAO_Fert_Prod_tN}, \code{L100.FAO_For_Exp_m3}, \code{L100.FAO_For_Imp_m3}, \code{L100.FAO_For_Prod_m3}. The corresponding file in the
#' original data system was \code{LA100.FAO_downscale_ctry.R} (aglu level1).
#' @details Extrapolate each FAO dataset to 2011; match with country names; extrapolate to countries that
#' split or combined at some point (e.g. Czechoslovakia needs to be split into Czech Republic and
#' Slovakia); and calculate rolling five-year averages.
#' @importFrom assertthat assert_that
#' @importFrom tibble tibble
#' @importFrom stats aggregate
#' @importFrom dplyr bind_rows distinct filter full_join left_join rename select add_row
#' @importFrom tidyr gather replace_na spread
#' @author BBL
module_aglu_LA100.FAO_downscale_ctry <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "aglu/AGLU_ctry",
             # FILE = "aglu/FAO/FAO_ag_HA_ha_PRODSTAT",
             # FILE = "aglu/FAO/FAO_ag_Prod_t_PRODSTAT",
             # FILE = "aglu/FAO/FAO_ag_Feed_t_SUA",
             # FILE = "aglu/FAO/FAO_ag_Food_t_SUA",
             # FILE = "aglu/FAO/FAO_an_Food_t_SUA",
             # FILE = "aglu/FAO/FAO_an_Prod_t_SUA",
             # FILE = "aglu/FAO/FAO_an_Stocks",
             # FILE = "aglu/FAO/FAO_an_Dairy_Stocks",
             FILE = "aglu/FAO/FAO_CL_kha_RESOURCESTAT",
             FILE = "aglu/FAO/FAO_fallowland_kha_RESOURCESTAT",
             FILE = "aglu/FAO/FAO_harv_CL_kha_RESOURCESTAT",
             FILE = "aglu/FAO/FAO_Fert_Cons_tN_RESOURCESTAT_archv",
             FILE = "aglu/FAO/FAO_Fert_Cons_tN_RESOURCESTAT",
             FILE = "aglu/FAO/FAO_Fert_Prod_tN_RESOURCESTAT_archv",
             FILE = "aglu/FAO/FAO_Fert_Prod_tN_RESOURCESTAT",
             FILE = "aglu/FAO/FAO_For_Exp_m3_FORESTAT",
             FILE = "aglu/FAO/FAO_For_Imp_m3_FORESTAT",
             FILE = "aglu/FAO/FAO_For_Prod_m3_FORESTAT",
             FILE = "common/iso_GCAM_regID",
             FILE = "aglu/FAO/FAO_ag_items_cal_SUA",
             FILE = "aglu/FAO/FAO_an_items_cal_SUA",

             # Replace raw data with processed FAO data in a restructured manner

             FILE = "aglu/FAO/L106.ag_NetExp_Mt_R_C_Y",
             FILE = "aglu/FAO/L106.an_NetExp_Mt_R_C_Y",
             FILE = "aglu/FAO/L100.FAO_ag_HA_ha",
             FILE = "aglu/FAO/L100.FAO_ag_Prod_t",
             FILE = "aglu/FAO/L100.FAO_ag_Feed_t",
             FILE = "aglu/FAO/L100.FAO_ag_Food_t",
             FILE = "aglu/FAO/L100.FAO_an_Food_t",
             FILE = "aglu/FAO/L100.FAO_an_Prod_t",
             FILE = "aglu/FAO/L100.FAO_an_Stocks",
             FILE = "aglu/FAO/L100.FAO_an_Dairy_Stocks",

             FILE = "aglu/A_recent_feed_modifications",

             FILE = "aglu/FAO/FAO_ag_items_PRODSTAT"
             ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L100.FAO_ag_HA_ha",
             "L100.FAO_ag_Prod_t",
             "L100.FAO_ag_Feed_t",
             "L100.FAO_ag_Food_t",
             "L100.FAO_an_Food_t",
             "L100.FAO_an_Prod_t",
             "L100.FAO_an_Stocks",
             "L100.FAO_an_Dairy_Stocks",
             "L100.FAO_CL_kha",
             "L100.FAO_fallowland_kha",
             "L100.FAO_harv_CL_kha",
             "L100.FAO_Fert_Cons_tN",
             "L100.FAO_Fert_Prod_tN",
             "L100.FAO_For_Exp_m3",
             "L100.FAO_For_Imp_m3",
             "L100.FAO_For_Prod_m3",

             "L101.ag_Food_Mt_R_C_Y",
             "L101.ag_Food_Pcal_R_C_Y",
             "L101.ag_kcalg_R_C_Y",

             #test adding L106 here
             "L106.ag_NetExp_Mt_R_C_Y",
             "L106.an_NetExp_Mt_R_C_Y",

             #test adding L105 here
             "L105.an_Food_Mt_R_C_Y",
             "L105.an_Food_Pcal_R_C_Y",
             "L105.an_kcalg_R_C_Y",
             "L105.an_Prod_Mt_R_C_Y",
             "L105.an_Prod_Mt_ctry_C_Y",
             # test adding L108 here
             "L101.ag_Feed_Mt_R_C_Y",

             "L100.FAO_PRODSTAT_TO_DOWNSCAL"

             ))
  } else if(command == driver.MAKE) {

    iso <- FAO_country <- `country codes` <- `element codes` <- `item codes` <-
      year <- value <- countries <- country.codes <- item <- item.codes <-
      element <- element.codes <- `2011` <- NULL # silence package chck.


    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data(all_data, "aglu/AGLU_ctry") %>%
      select(iso, FAO_country) %>%
      distinct ->
      AGLU_ctry

    # FAO_ag_HA_ha_PRODSTAT <- get_data(all_data, "aglu/FAO/FAO_ag_HA_ha_PRODSTAT")
    # FAO_ag_Prod_t_PRODSTAT <- get_data(all_data, "aglu/FAO/FAO_ag_Prod_t_PRODSTAT")
    # FAO_ag_Feed_t_SUA <- get_data(all_data, "aglu/FAO/FAO_ag_Feed_t_SUA")
    # FAO_ag_Food_t_SUA <- get_data(all_data, "aglu/FAO/FAO_ag_Food_t_SUA")
    # FAO_an_Food_t_SUA <- get_data(all_data, "aglu/FAO/FAO_an_Food_t_SUA")
    # FAO_an_Prod_t_SUA <- get_data(all_data, "aglu/FAO/FAO_an_Prod_t_SUA")
    # FAO_an_Stocks <- get_data(all_data, "aglu/FAO/FAO_an_Stocks")
    # FAO_an_Dairy_Stocks <- get_data(all_data, "aglu/FAO/FAO_an_Dairy_Stocks")

    L106.ag_NetExp_Mt_R_C_Y <- get_data(all_data, "aglu/FAO/L106.ag_NetExp_Mt_R_C_Y", strip_attributes = T)
    L106.an_NetExp_Mt_R_C_Y <- get_data(all_data, "aglu/FAO/L106.an_NetExp_Mt_R_C_Y", strip_attributes = T)
    L100.FAO_ag_HA_ha <- get_data(all_data, "aglu/FAO/L100.FAO_ag_HA_ha", strip_attributes = T)
    L100.FAO_ag_Prod_t <- get_data(all_data, "aglu/FAO/L100.FAO_ag_Prod_t", strip_attributes = T)
    L100.FAO_ag_Feed_t <- get_data(all_data, "aglu/FAO/L100.FAO_ag_Feed_t", strip_attributes = T)
    L100.FAO_ag_Food_t <- get_data(all_data, "aglu/FAO/L100.FAO_ag_Food_t", strip_attributes = T)
    L100.FAO_an_Food_t <- get_data(all_data, "aglu/FAO/L100.FAO_an_Food_t", strip_attributes = T)
    L100.FAO_an_Prod_t <- get_data(all_data, "aglu/FAO/L100.FAO_an_Prod_t", strip_attributes = T)
    L100.FAO_an_Stocks <- get_data(all_data, "aglu/FAO/L100.FAO_an_Stocks", strip_attributes = T)
    L100.FAO_an_Dairy_Stocks <- get_data(all_data, "aglu/FAO/L100.FAO_an_Dairy_Stocks", strip_attributes = T)

    A_recent_feed_modifications <- get_data(all_data, "aglu/A_recent_feed_modifications")
    FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO/FAO_ag_items_PRODSTAT")
    # assert FAO_an_Stocks has unit of head

    # The new data has consistent Production between PRODSTAT and SUA
    # For fodder crops 2012 - 2017 == 2011


    # # Some data in an_Stocks are in 1000s of heads instead of just heads; convert them
    # fhyc <- names(FAO_an_Stocks) %in% aglu.FAO_HISTORICAL_YEARS
    # thr <- FAO_an_Stocks$units == "1000 Head"
    # FAO_an_Stocks[thr, fhyc] <- FAO_an_Stocks[thr, fhyc] * 1000
    # FAO_an_Stocks$units <- FAO_an_Dairy_Stocks$units <- NULL
    # # as necessary, expand the animal stocks data to 2012
    # if(!"2012" %in% names(FAO_an_Stocks)) FAO_an_Stocks$`2012` <- FAO_an_Stocks$`2011`

    # Fodder data only goes through 2011, but won't be extrapolated to 2012 in
    # the function below because the column `2012` exists.
    # Fill in the Fodder 2012 data here.
    # FAO_ag_HA_ha_PRODSTAT %>%
    #   filter(grepl("forage", item) | grepl("Fodder", item) | grepl("silage", item)) %>%
    #   mutate(`2012` = `2011`) ->
    #   FAO_fodder_HA_ha_PRODSTAT
    #
    # FAO_ag_HA_ha_PRODSTAT %>%
    #   filter(!grepl("forage", item) & !grepl("Fodder", item) & !grepl("silage", item)) %>%
    #   bind_rows(FAO_fodder_HA_ha_PRODSTAT) ->
    #   FAO_ag_HA_ha_PRODSTAT
    #
    # FAO_ag_Prod_t_PRODSTAT %>%
    #   filter(grepl("forage", item) | grepl("Fodder", item) | grepl("silage", item)) %>%
    #   mutate(`2012` = `2011`) ->
    #   FAO_fodder_Prod_t_PRODSTAT
    #
    # FAO_ag_Prod_t_PRODSTAT %>%
    #   filter(!grepl("forage", item) & !grepl("Fodder", item) & !grepl("silage", item)) %>%
    #   bind_rows(FAO_fodder_Prod_t_PRODSTAT) ->
    #   FAO_ag_Prod_t_PRODSTAT



    FAO_CL_kha_RESOURCESTAT <- get_data(all_data, "aglu/FAO/FAO_CL_kha_RESOURCESTAT")
    FAO_fallowland_kha_RESOURCESTAT <- get_data(all_data, "aglu/FAO/FAO_fallowland_kha_RESOURCESTAT")
    FAO_harv_CL_kha_RESOURCESTAT <- get_data(all_data, "aglu/FAO/FAO_harv_CL_kha_RESOURCESTAT")
    FAO_Fert_Cons_tN_RESOURCESTAT_archv <- get_data(all_data, "aglu/FAO/FAO_Fert_Cons_tN_RESOURCESTAT_archv")
    FAO_Fert_Cons_tN_RESOURCESTAT <- get_data(all_data, "aglu/FAO/FAO_Fert_Cons_tN_RESOURCESTAT")
    FAO_Fert_Prod_tN_RESOURCESTAT_archv <- get_data(all_data, "aglu/FAO/FAO_Fert_Prod_tN_RESOURCESTAT_archv")
    FAO_Fert_Prod_tN_RESOURCESTAT<- get_data(all_data, "aglu/FAO/FAO_Fert_Prod_tN_RESOURCESTAT")
    FAO_For_Exp_m3_FORESTAT <- get_data(all_data, "aglu/FAO/FAO_For_Exp_m3_FORESTAT")
    FAO_For_Imp_m3_FORESTAT <- get_data(all_data, "aglu/FAO/FAO_For_Imp_m3_FORESTAT")
    FAO_For_Prod_m3_FORESTAT <- get_data(all_data, "aglu/FAO/FAO_For_Prod_m3_FORESTAT")

    # Add these
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    FAO_ag_items_cal_SUA <- get_data(all_data, "aglu/FAO/FAO_ag_items_cal_SUA")
    FAO_an_items_cal_SUA <- get_data(all_data, "aglu/FAO/FAO_an_items_cal_SUA")



    itel_colnames <- c("item", "item codes", "element", "element codes")
    coitel_colnames <- c("countries", "country codes", itel_colnames)
    FAO_histyear_cols <- as.character(aglu.FAO_HISTORICAL_YEARS)


    #kbn 2019/11/04 Write function to fill holes for base year-
    #This function will fill holes from the specified cut-off year to the base year for any region, comoditty combination
    #that is missing data in any year. It will not fill in data if no data exists for a region commoditty combination before the cut-off year.
    #It will not add new commoditties or regions.If Printlog is set to TRUE, the function will print out,how many data points you are missing in the
    #base year, how many points could be filled in and how many points could not be filled in.Current cut-off year is set to 2007. 1961 is the first year of FAO
    #data.

    get_baseyear_or_mostrecent_FAO<-function(df,BaseYear=MODEL_FINAL_BASE_YEAR,StartYear=1961,CutOff=2007,Printlog=FALSE){

      #Step 1: Get values for start year, last year and dataset name
      StartYear<-toString(StartYear)
      LastYear<-colnames(df)[ncol(df)]
      name<-deparse(substitute(df))

      #Step 2: Convert from tibble to dataframe for processing.Will convert back to tibble in the last step.
     df<-as.data.frame(df)

     #Step 3: Create a list of required years
    Yrs<-seq(CutOff+1,BaseYear,1)

      #Step 4: If there is no column, create it.
      for (i in Yrs){
        if (!toString(i) %in% c(colnames(df))){
          df[,toString(i)]<-NA_integer_
        }
      }

      #Step 5: Print first warning message
      if (Printlog==TRUE){

        print(paste("No data found in dataset ",name," for ",(sum(is.na((df[,toString(BaseYear)]))))," values.",sep = ""))}


      #Step 6: Fill with neighbor

      for (i in Yrs){
        df[,toString(i)]<-if_else(is.na(df[,toString(i)]),df[,toString(i-1)],df[,toString(i)])
      }


      if (Printlog==TRUE){
        #Step7: Print second warning message
        print(paste("Could not fill in data for base year ",name," for ",sum(is.na(df[,toString(BaseYear)]))," values. There was no data available after ", CutOff,sep = ""))}

    #Step 8: Convert back to tibble

    df<-as_tibble(df)

      return(df)
    }


    #kbn 2019/12/16 Adding in hole-filling for all FAO data.
    # FAO_ag_HA_ha_PRODSTAT <- get_baseyear_or_mostrecent_FAO(FAO_ag_HA_ha_PRODSTAT)
    # FAO_ag_Prod_t_PRODSTAT <- get_baseyear_or_mostrecent_FAO(FAO_ag_Prod_t_PRODSTAT)
    # FAO_ag_Feed_t_SUA <- get_baseyear_or_mostrecent_FAO(FAO_ag_Feed_t_SUA)
    # FAO_ag_Food_t_SUA <- get_baseyear_or_mostrecent_FAO(FAO_ag_Food_t_SUA)
    # FAO_an_Food_t_SUA <- get_baseyear_or_mostrecent_FAO(FAO_an_Food_t_SUA)
    # FAO_an_Prod_t_SUA <- get_baseyear_or_mostrecent_FAO(FAO_an_Prod_t_SUA)
    FAO_fallowland_kha_RESOURCESTAT <- get_baseyear_or_mostrecent_FAO(FAO_fallowland_kha_RESOURCESTAT)
    FAO_harv_CL_kha_RESOURCESTAT <- get_baseyear_or_mostrecent_FAO(FAO_harv_CL_kha_RESOURCESTAT)
    FAO_Fert_Cons_tN_RESOURCESTAT <- get_baseyear_or_mostrecent_FAO(FAO_Fert_Cons_tN_RESOURCESTAT)
    FAO_Fert_Prod_tN_RESOURCESTAT<- get_baseyear_or_mostrecent_FAO(FAO_Fert_Prod_tN_RESOURCESTAT)


    # Replace the item and element code names with what is used in the more recent datasets
    FAO_Fert_Cons_tN_RESOURCESTAT_archv[itel_colnames] <- FAO_Fert_Cons_tN_RESOURCESTAT[1, itel_colnames]
    FAO_Fert_Prod_tN_RESOURCESTAT_archv[itel_colnames] <- FAO_Fert_Prod_tN_RESOURCESTAT[1, itel_colnames]

    # Merge resourcestat fertilizer databases with 'archive' years (1961-2002) and more recent
    # years (2002-2010). FAOSTAT notes that the methods changed between the two datasets; we
    # ignore this discrepancy but use the 2002 data from the more recent dataset
    FAO_Fert_Cons_tN_RESOURCESTAT_archv$`2002` <- NULL
    FAO_Fert_Prod_tN_RESOURCESTAT_archv$`2002` <- NULL

    # Interesting: dplyr can't go as fast as the approach taken in the original data system
    # A number of dplyr operations are *considerably* slower with this big dataset, and take more lines
    # So most of this function, the slowest in the entire data system, retains the original
    # code (though cleaned up considerably) and logic
    cons <- full_join(FAO_Fert_Cons_tN_RESOURCESTAT_archv,
                      FAO_Fert_Cons_tN_RESOURCESTAT, by = c("countries", "country codes", "item", "item codes", "element", "element codes"))
    prod <- full_join(FAO_Fert_Prod_tN_RESOURCESTAT_archv,
                      FAO_Fert_Prod_tN_RESOURCESTAT, by = c("countries", "country codes", "item", "item codes", "element", "element codes"))

    # Aggregate to complete the merge of the two datasets
    FAO_Fert_Cons_tN_RESOURCESTAT <- aggregate(cons[names(cons) %in% FAO_histyear_cols],
                                               by = as.list(cons[coitel_colnames]),
                                               sum, na.rm = TRUE) %>% as_tibble()
    FAO_Fert_Prod_tN_RESOURCESTAT <- aggregate(prod[names(prod) %in% FAO_histyear_cols],
                                               by = as.list(prod[coitel_colnames]),
                                               sum, na.rm = TRUE) %>% as_tibble()



    # Not all databases go to 2012. Extrapolate each dataset to 2012, repeating
    # the data for 2009/10. Where missing 1961, substitute 1962
    list(
         # "FAO_ag_Feed_t_SUA" = FAO_ag_Feed_t_SUA,
         # "FAO_ag_Food_t_SUA" = FAO_ag_Food_t_SUA,
         # "FAO_an_Food_t_SUA" = FAO_an_Food_t_SUA,
         # "FAO_an_Prod_t_SUA" = FAO_an_Prod_t_SUA,
         # "FAO_an_Stocks" = FAO_an_Stocks,
         # "FAO_an_Dairy_Stocks" = FAO_an_Dairy_Stocks,
         # "FAO_ag_HA_ha_PRODSTAT" = FAO_ag_HA_ha_PRODSTAT,
         # "FAO_ag_Prod_t_PRODSTAT" = FAO_ag_Prod_t_PRODSTAT,
         "FAO_Fert_Cons_tN_RESOURCESTAT" = FAO_Fert_Cons_tN_RESOURCESTAT,
         "FAO_Fert_Prod_tN_RESOURCESTAT" = FAO_Fert_Prod_tN_RESOURCESTAT,
         "FAO_For_Exp_m3_FORESTAT" = FAO_For_Exp_m3_FORESTAT,
         "FAO_For_Imp_m3_FORESTAT" = FAO_For_Imp_m3_FORESTAT,
         "FAO_For_Prod_m3_FORESTAT" = FAO_For_Prod_m3_FORESTAT) %>%
      # apply the following function over all list elements
      lapply(FUN = function(df) {
        if(!"1961" %in% colnames(df)) df$`1961` <- df$`1962`
        if(!"2013" %in% colnames(df)) df$`2013` <- df$`2012`
        if(!"2014" %in% colnames(df)) df$`2014` <- df$`2013`
        if(!"2015" %in% colnames(df)) df$`2015` <- df$`2014`
        df$element <- NULL
        df
      }) %>%
      # combine everything together
      bind_rows(.id = "element") ->
      FAO_data_ALL

    # Replace all missing numeric values with 0
    FAO_data_ALL <- dplyr::mutate_if(FAO_data_ALL, is.numeric, replace_na, replace = 0)

    # Match the iso names
    FAO_data_ALL %>%
      left_join(distinct(AGLU_ctry, FAO_country, .keep_all = TRUE), by = c("countries" = "FAO_country")) ->
      FAO_data_ALL

    # Downscale countries individually NOTE: This is complicated. The FAO data need to be downscaled
    # to all FAO historical years (i.e. back to 1961 regardless of when we are starting our
    # historical time series). Otherwise the early historical years will get averaged with zeroes.
    # Czechoslovakia
    FAO_data_ALL %>%
      filter(iso %in% AGLU_ctry$iso[AGLU_ctry$FAO_country == "Czechoslovakia"]) %>%
      downscale_FAO_country("Czechoslovakia", 1993L, years = aglu.FAO_HISTORICAL_YEARS) ->
      FAO_data_ALL_cze

    # USSR
    FAO_data_ALL %>%
      filter(iso %in% AGLU_ctry$iso[AGLU_ctry$FAO_country == "USSR"]) %>%
      downscale_FAO_country("USSR", 1992L, years = aglu.FAO_HISTORICAL_YEARS) ->
      FAO_data_ALL_ussr

    # Yugoslavia
    FAO_data_ALL %>%
      filter(iso %in% AGLU_ctry$iso[AGLU_ctry$FAO_country == "Yugoslav SFR"]) %>%
      downscale_FAO_country("Yugoslav SFR", 1992L, years = aglu.FAO_HISTORICAL_YEARS) ->
      FAO_data_ALL_yug

    # Drop these countries from the full database and combine
    FAO_data_ALL %>%
      filter(!iso %in% unique(c(FAO_data_ALL_cze$iso, FAO_data_ALL_ussr$iso, FAO_data_ALL_yug$iso))) %>%
      # combine these downscaled databases
      bind_rows(FAO_data_ALL_cze, FAO_data_ALL_ussr, FAO_data_ALL_yug) ->
      FAO_data_ALL

    # Make sure histyear_cols uses only names in our data set
    FAO_histyear_cols <- intersect(FAO_histyear_cols, names(FAO_data_ALL))
    # Drop observations where all years are zero
    FAO_data_ALL <- FAO_data_ALL[rowSums(FAO_data_ALL[FAO_histyear_cols]) != 0, ]

    # Calculate rolling five-year averages from available data
    FAO_data_ALL_5yr <- FAO_data_ALL

    # In the first and last two years, use the 3 and 4 available years
    FAO_data_ALL_5yr[FAO_histyear_cols][1] <- rowMeans(FAO_data_ALL[FAO_histyear_cols][1:3])
    FAO_data_ALL_5yr[FAO_histyear_cols][2] <- rowMeans(FAO_data_ALL[FAO_histyear_cols][1:4])

    # Precalculate a few things for loop speed
    lastcol <- ncol(FAO_data_ALL_5yr[FAO_histyear_cols]) - 2
    x <- FAO_data_ALL[FAO_histyear_cols]
    lenXFAO <- length(FAO_histyear_cols)

    # Main calculation loop
    for(i in 3:lastcol) {
      FAO_data_ALL_5yr[FAO_histyear_cols][, i] <- rowMeans(x[i + -2:2])
    }
    FAO_data_ALL_5yr[FAO_histyear_cols][lenXFAO - 1] <-
      rowMeans(FAO_data_ALL[FAO_histyear_cols][(lenXFAO - 3):lenXFAO])
    FAO_data_ALL_5yr[FAO_histyear_cols][lenXFAO] <-
      rowMeans(FAO_data_ALL[FAO_histyear_cols][(lenXFAO - 2):lenXFAO])

    # From here on, only use the specified AGLU historical years
    FAO_data_ALL_5yr <- FAO_data_ALL_5yr[c(coitel_colnames, "iso", as.character(aglu.AGLU_HISTORICAL_YEARS))]

    # Rename columns to old names
    FAO_data_ALL_5yr %>%
      rename(country.codes = `country codes`,
             element.codes = `element codes`,
             item.codes = `item codes`) ->
      FAO_data_ALL_5yr

    # Change `element` columns to match old data and reshape
    #    FAO_data_ALL_5yr <- FAO_data_ALL_5yr[c(1:6,8:47,7)]
    FAO_data_ALL_5yr$element <- gsub(pattern = "_[A-Z]*$", "", FAO_data_ALL_5yr$element)
    FAO_data_ALL_5yr$element <- gsub(pattern = "^FAO_", "", FAO_data_ALL_5yr$element)
    FAO_data_ALL_5yr <- gather_years(FAO_data_ALL_5yr)

    # Re-split into separate tables for each element
    L100.FAOlist <- split(seq(1, nrow(FAO_data_ALL_5yr)), FAO_data_ALL_5yr$element)
    names(L100.FAOlist) <- lapply(names(L100.FAOlist), function(x) { paste0("L100.FAO_", x) })
    # change list names to match the legacy
    # names
    fixup <- function(irows, legacy.name) {
        FAO_data_ALL_5yr[irows,] %>%
          add_comments("Downscale countries; calculate 5-yr averages") %>%
          add_legacy_name(legacy.name)
    }
    L100.FAOlist <- Map(fixup, L100.FAOlist, names(L100.FAOlist))

    # Fallow land data sets are missing lots of years, so we don't include them in the 5yr average
    # They need to be reshaped and written out now for use downstream
    FAO_CL_kha_RESOURCESTAT %>%
      left_join(distinct(AGLU_ctry, FAO_country, .keep_all = TRUE), by = c("countries" = "FAO_country")) %>%
      rename(country.codes = `country codes`,
             element.codes = `element codes`,
             item.codes = `item codes`) %>%
      gather_years() %>%
      add_legacy_name("L100.FAO_CL_kha") %>%
      na.omit() %>%
      mutate(value = as.numeric(value)) %>%
      add_comments("Downscale countries") ->
      L100.FAO_CL_kha
    FAO_fallowland_kha_RESOURCESTAT %>%
      left_join(distinct(AGLU_ctry, FAO_country, .keep_all = TRUE), by = c("countries" = "FAO_country")) %>%
      rename(country.codes = `country codes`,
             element.codes = `element codes`,
             item.codes = `item codes`) %>%
      gather_years()  %>%
      add_legacy_name("L100.FAO_fallowland_kha") %>%
      na.omit() %>%
      mutate(value = as.numeric(value)) %>%
      add_comments("Downscale countries") ->
      L100.FAO_fallowland_kha
    FAO_harv_CL_kha_RESOURCESTAT %>%
      left_join(distinct(AGLU_ctry, FAO_country, .keep_all = TRUE), by = c("countries" = "FAO_country")) %>%
      rename(country.codes = `country codes`,
             element.codes = `element codes`,
             item.codes = `item codes`) %>%
      gather_years() %>%
      add_legacy_name("L100.FAO_harv_CL_kha") %>%
      na.omit() %>%
      mutate(value = as.numeric(value)) %>%
      add_comments("Downscale countries") ->
      L100.FAO_harv_CL_kha


    # Amendment ----
    # move to FAOSTAT_GCAM package later for the alfalfa adjustment
    L100.FAO_ag_Prod_t %>%
      mutate(value = if_else(iso == "usa" & item == "Alfalfa for forage and silage",
                             value / 4, value))  ->                                                                          # Aggregate by ISO, item, and region (some iso codes apply to multiple lines in data)
      L100.FAO_ag_Prod_t


    # Add description, units, process (done above), and precursor information
    L100.FAO_ag_HA_ha %>%
      add_title("FAO agricultural harvested area by country, item, year", overwrite = T) %>%
      add_units("Ha") %>%
      add_precursors("aglu/FAO/L100.FAO_ag_HA_ha", "aglu/AGLU_ctry") ->
      L100.FAO_ag_HA_ha
    L100.FAO_ag_Prod_t %>%
      add_title("FAO agricultural production by country, item, year", overwrite = T) %>%
      add_units("t") %>%
      add_precursors("aglu/FAO/L100.FAO_ag_Prod_t", "aglu/AGLU_ctry") ->
      L100.FAO_ag_Prod_t
    L100.FAO_ag_Feed_t %>%
      add_title("FAO agricultural feed by country, item, year", overwrite = T) %>%
      add_units("t") %>%
      add_precursors("aglu/FAO/L100.FAO_ag_Feed_t", "aglu/AGLU_ctry") ->
      L100.FAO_ag_Feed_t
    L100.FAO_ag_Food_t %>%
      add_title("FAO agricultural food consumption by country, item, year", overwrite = T) %>%
      add_units("t") %>%
      add_precursors("aglu/FAO/L100.FAO_ag_Food_t", "aglu/AGLU_ctry") ->
      L100.FAO_ag_Food_t
    L100.FAO_an_Food_t %>%
      add_title("FAO animal food consumption by country, item, year", overwrite = T) %>%
      add_units("t") %>%
      add_precursors("aglu/FAO/L100.FAO_an_Food_t", "aglu/AGLU_ctry") ->
      L100.FAO_an_Food_t
    L100.FAO_an_Prod_t %>%
      add_title("FAO animal production by country, item, year", overwrite = T) %>%
      add_units("t") %>%
      add_precursors("aglu/FAO/L100.FAO_an_Prod_t", "aglu/AGLU_ctry") ->
      L100.FAO_an_Prod_t
    L100.FAO_an_Stocks %>%
      add_title("FAO animal stocks country, item, year", overwrite = T) %>%
      add_units("number") %>%
      add_precursors("aglu/FAO/L100.FAO_an_Stocks", "aglu/AGLU_ctry") ->
      L100.FAO_an_Stocks
    L100.FAO_an_Dairy_Stocks %>%
      add_title("FAO dairy producing animal stocks country, item, year", overwrite = T) %>%
      add_units("number") %>%
      add_precursors("aglu/FAO/L100.FAO_an_Dairy_Stocks", "aglu/AGLU_ctry") ->
      L100.FAO_an_Dairy_Stocks
    L100.FAO_CL_kha %>%
      add_title("FAO cropland area by country, year") %>%
      add_units("kha") %>%
      add_precursors("aglu/FAO/FAO_CL_kha_RESOURCESTAT", "aglu/AGLU_ctry") ->
      L100.FAO_CL_kha
    L100.FAO_fallowland_kha %>%
      add_title("FAO fallow land area by country, year") %>%
      add_units("kha") %>%
      add_precursors("aglu/FAO/FAO_fallowland_kha_RESOURCESTAT", "aglu/AGLU_ctry") ->
      L100.FAO_fallowland_kha
    L100.FAO_harv_CL_kha %>%
      add_title("FAO harvested cropland (temporary crops) area by country, year") %>%
      add_units("kha") %>%
      add_precursors("aglu/FAO/FAO_harv_CL_kha_RESOURCESTAT", "aglu/AGLU_ctry") ->
      L100.FAO_harv_CL_kha
    L100.FAOlist[["L100.FAO_Fert_Cons_tN"]] %>%
      add_title("FAO fertilizer consumption by country, year") %>%
      add_units("tonnes N") %>%
      add_precursors("aglu/FAO/FAO_Fert_Cons_tN_RESOURCESTAT",
                     "aglu/FAO/FAO_Fert_Cons_tN_RESOURCESTAT_archv",
                     "aglu/AGLU_ctry") ->
      L100.FAO_Fert_Cons_tN
    L100.FAOlist[["L100.FAO_Fert_Prod_tN"]] %>%
      add_title("FAO fertilizer production by country, year") %>%
      add_units("tonnes N") %>%
      add_precursors("aglu/FAO/FAO_Fert_Prod_tN_RESOURCESTAT",
                     "aglu/FAO/FAO_Fert_Prod_tN_RESOURCESTAT_archv",
                     "aglu/AGLU_ctry") ->
      L100.FAO_Fert_Prod_tN
    L100.FAOlist[["L100.FAO_For_Exp_m3"]] %>%
      add_title("FAO forestry exports by country, year") %>%
      add_units("m3") %>%
      add_precursors("aglu/FAO/FAO_For_Exp_m3_FORESTAT", "aglu/AGLU_ctry") ->
      L100.FAO_For_Exp_m3
    L100.FAOlist[["L100.FAO_For_Imp_m3"]] %>%
      add_title("FAO forestry imports by country, year") %>%
      add_units("m3") %>%
      add_precursors("aglu/FAO/FAO_For_Imp_m3_FORESTAT", "aglu/AGLU_ctry") ->
      L100.FAO_For_Imp_m3
    L100.FAOlist[["L100.FAO_For_Prod_m3"]] %>%
      add_title("FAO forestry production by country, year") %>%
      add_units("m3") %>%
      add_precursors("aglu/FAO/FAO_For_Prod_m3_FORESTAT", "aglu/AGLU_ctry") ->
      L100.FAO_For_Prod_m3

# Moving LA101 to here for food and land pieces ----------------


# Only keep spatial downscale of prod and HA in LA101

    # Process FAO food consumption data (tons): remove unnecessary columns, convert units, aggregate to region and commodity
    L100.FAO_ag_Food_t %>%
      select(-countries, -country.codes, -item.codes, -element, -element.codes) %>%                               # Remove unnecessary columns
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%                                                   # Map in ISO codes
      left_join(FAO_ag_items_cal_SUA, by = "item") %>%                                                           # Map in GCAM commodities
      filter(!is.na(GCAM_commodity)) %>%                                                                          # Remove commodities not included in GCAM
      mutate(value = value * CONV_TON_MEGATON) %>%                                                               # Convert from tons to Mt
      group_by(GCAM_region_ID, GCAM_commodity, year) %>%                                                          # Group by region, commodity, year
      summarise(value = sum(value)) %>%                                                                           # Aggregate then map to appropriate data frame
      ungroup() %>%                                                                                               # Ungroup before complete
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),
               GCAM_commodity, year, fill = list(value = 0)) ->                                                   # Fill in missing region/commodity combinations with 0
      L101.ag_Food_Mt_R_C_Y

    # Process FAO food consumption data (Pcal): remove unnecessary columns, convert units, aggregate to region and commodity
    L100.FAO_ag_Food_t %>%
      select(-countries, -country.codes, -item.codes, -element, -element.codes) %>%                               # Remove unnecessary columns
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%                                                   # Map in ISO codes
      left_join(FAO_ag_items_cal_SUA, by = "item") %>%                                                           # Map in GCAM commodities
      filter(!is.na(GCAM_commodity)) %>%                                                                          # Remove commodities not included in GCAM
      mutate(value = value * Mcal_t * CONV_MCAL_PCAL) %>%                                                        # Convert from tons to Pcal
      group_by(GCAM_region_ID, GCAM_commodity, year) %>%                                                          # Group by region, commodity, year
      summarise(value = sum(value)) %>%                                                                           # Aggregate then map to appropriate data frame
      ungroup() %>%                                                                                               # Ungroup before complete
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),
               GCAM_commodity, year, fill = list(value = 0)) ->                                                   # Fill in missing region/commodity combinations with 0
      L101.ag_Food_Pcal_R_C_Y

    # Calculate average caloric content of consumed commodities (kcal/g)
    L101.ag_Food_Pcal_R_C_Y %>%
      left_join(L101.ag_Food_Mt_R_C_Y, by = c("GCAM_region_ID", "GCAM_commodity", "year")) %>%                  # Join food in Mt to food in Pcal
      mutate(value = if_else(value.y == 0, 1, value.x / value.y)) %>%                                          # Calculate average caloric content, set NA values to 1
      select(-value.x, -value.y) ->                                                                             # Remove extra columns
      L101.ag_kcalg_R_C_Y

    # Produce outputs
    L101.ag_Food_Mt_R_C_Y %>%
      add_title("FAO food consumption by GCAM region, commodity, and year") %>%
      add_units("Mt/yr") %>%
      add_comments("Aggregates FAO data by GCAM region, commodity, and year") %>%
      add_comments("Data is also converted from tons to Mt") %>%
      add_legacy_name("L101.ag_Food_Mt_R_C_Y") %>%
      add_precursors("L100.FAO_ag_Food_t", "aglu/FAO/FAO_ag_items_cal_SUA", "common/iso_GCAM_regID") ->
      L101.ag_Food_Mt_R_C_Y
    L101.ag_Food_Pcal_R_C_Y %>%
      add_title("FAO food consumption by GCAM region, commodity, and year") %>%
      add_units("Pcal/yr") %>%
      add_comments("Aggregates FAO data by GCAM region, commodity, and year") %>%
      add_comments("Data is also converted from tons to Pcal") %>%
      add_legacy_name("L101.ag_Food_Pcal_R_C_Y") %>%
      add_precursors("L100.FAO_ag_Food_t", "aglu/FAO/FAO_ag_items_cal_SUA", "common/iso_GCAM_regID") ->
      L101.ag_Food_Pcal_R_C_Y
    L101.ag_kcalg_R_C_Y %>%
      add_title("Weighted average commodity caloric content by GCAM region, commodity, and year") %>%
      add_units("kcal/g") %>%
      add_comments("Combines the L101.ag_Food_Mt_R_C_Y and L101.ag_Food_Pcal_R_C_Y data frames") %>%
      add_legacy_name("L101.ag_kcalg_R_C_Y") %>%
      add_precursors("L100.FAO_ag_Food_t", "aglu/FAO/FAO_ag_items_cal_SUA", "common/iso_GCAM_regID") ->
      L101.ag_kcalg_R_C_Y


    ## Generate FAO_PRODSTAT_TO_DOWNSCAL ----
    # Clean FAO production and harvested area tables
    L100.FAO_ag_HA_ha %>%
      select(-countries, -country.codes, -item.codes, -element, -element.codes) %>%                             # Remove unnecessary columns
      mutate(iso = if_else(iso %in% c("srb", "mne"), "scg", iso)) %>%                                           # Re-map Serbia (srb) and Montenegro (mne) to the iso code for the single country (scg)
      group_by(item, iso, year) %>%                                                                             # scg is the iso code in the Monfreda data because its base year is ~2000
      summarise(value = sum(value)) ->                                                                          # Aggregate by ISO, item, and region (some iso codes apply to multiple lines in data)
      FAO_ag_HA_ha

    L100.FAO_ag_Prod_t %>%
      select(-countries, -country.codes, -item.codes, -element, -element.codes) %>%                             # Remove unnecessary columns
      mutate(iso = if_else(iso %in% c("srb", "mne"), "scg", iso)) %>%                                           # Re-map Serbia (srb) and Montenegro (mne) to the iso code for the single country (scg)
      group_by(item, iso, year) %>%                                                                             # scg is the iso code in the Monfreda data because its base year is ~2000
      summarise(value = sum(value)) ->                                                                          # Aggregate by ISO, item, and region (some iso codes apply to multiple lines in data)
      FAO_ag_Prod_t

    # Set production to zero when harvested area is zero and vice versa
    FAO_ag_HA_ha %>% ungroup %>%
      inner_join(FAO_ag_Prod_t, by = c("iso", "item", "year")) %>%                                              # Join production and harvested area
      rename(harvested.area = value.x, production = value.y) %>%                                              # Rename variables
      mutate(harvested.area = if_else(production == 0, 0, harvested.area),                                  # Set harvested area to zero if production is zero
             production = if_else(harvested.area == 0, 0, production)) ->                                       # Set production to zero if harvested area is zero
      FAO_PRODSTAT_MERGED

    # FAO_PRODSTAT_TO_DOWNSCAL: FAO Prodstat data aggregated by GCAM commodity and to be downscaled to GLU.
    FAO_PRODSTAT_MERGED %>%
      left_join_error_no_match(distinct(select(FAO_ag_items_PRODSTAT, item, GCAM_commodity, GCAM_subsector)), by = "item",      # distinct() to avoid duplicating data for items with multiple rows in FAO_ag_items_PRODSTAT
                               ignore_columns = c("GCAM_commodity", "GCAM_subsector")) %>%                                           # ignore GCAM_commodity column to avoid error in ljenm (this column has NA for FAO items not modeled in GCAM)
      filter(!is.na(GCAM_commodity)) %>%                                                                        # Remove commodities not included in GCAM
      group_by(iso, GCAM_commodity, GCAM_subsector, year) %>%
      summarise(harvested.area = sum(harvested.area),
                production = sum(production)) %>%
      ungroup() ->
      L100.FAO_PRODSTAT_TO_DOWNSCAL

    L100.FAO_PRODSTAT_TO_DOWNSCAL %>%
      add_title("FAO agricultural production and harvested area by country, item, year", overwrite = T) %>%
      add_units("Ha and t") %>%
      add_precursors("aglu/FAO/L100.FAO_ag_HA_ha",
                     "aglu/FAO/L100.FAO_ag_Prod_t",
                     "aglu/FAO/FAO_ag_items_PRODSTAT",
                     "aglu/AGLU_ctry") ->
      L100.FAO_PRODSTAT_TO_DOWNSCAL

# Moving L105 to here ----
    # Process FAO animal products food consumption data: map in GCAM region and commodities, convert units, aggregate to region and commodity
    L100.FAO_an_Food_t %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%                               # Map in GCAM regions
      left_join(FAO_an_items_cal_SUA, by = "item") %>%                                       # Map in GCAM commodities, creates NAs
      filter(!is.na(GCAM_commodity)) %>%                                                      # Remove commodities not included in GCAM
      mutate(Mt = value * CONV_TON_MEGATON, Pcal = value * Mcal_t * CONV_MCAL_PCAL) %>%      # Convert from tons to Mt and Pcal
      group_by(GCAM_region_ID, GCAM_commodity, year) %>%                                      # Group by region, commodity, year
      summarize(Mt = sum(Mt), Pcal = sum(Pcal)) %>%                                           # Aggregate food consumption in Mt and Pcal
      ungroup() %>%                                                                           # Ungroup before complete
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),                        # Fill in missing region/commodity combinations with 0
               GCAM_commodity, year, fill = list(Mt = 0, Pcal = 0)) %>%
      # Calculate average caloric content of consumed commodities (Pcal/Mt = 10e12 kcal / 10e12 g = kcal/g) for each region and animal type, set NA values to 1
      mutate(value = if_else(Mt == 0, 1, Pcal / Mt))  ->
      L105.an_Food_R_C_Y

    # Build table of food consumption in Mt
    L105.an_Food_R_C_Y %>%
      select(GCAM_region_ID, GCAM_commodity, year, value = Mt) ->
      L105.an_Food_Mt_R_C_Y
    # Build table of food consumption in Pcal
    L105.an_Food_R_C_Y %>%
      select(GCAM_region_ID, GCAM_commodity, year, value = Pcal) ->
      L105.an_Food_Pcal_R_C_Y
    # Build table of caloric content in kcal/g
    L105.an_Food_R_C_Y %>%
      select(GCAM_region_ID, GCAM_commodity, year, value) ->
      L105.an_kcalg_R_C_Y

    # Process FAO animal products production data: map in GCAM region and commodities, convert units
    L100.FAO_an_Prod_t %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%                               # Map in GCAM regions
      left_join(FAO_an_items_cal_SUA, by = "item") %>%                                       # Map in GCAM commodities, creates NAs
      filter(!is.na(GCAM_commodity)) %>%                                                      # Remove commodities not included in GCAM
      mutate(value = value * CONV_TON_MEGATON)  ->                                           # Convert from tons to Mt
      L105.FAO_an_Prod_Mt
    # Build country table, aggregate to iso and GCAM commodity
    L105.FAO_an_Prod_Mt %>%
      group_by(iso, GCAM_commodity, year) %>%                                                 # Group by iso, commodity, year
      summarize(value = sum(value)) %>%                                                       # Aggregate food consumption in Mt
      ungroup() %>%                                                                           # Ungroup before complete
      complete(iso = unique(iso_GCAM_regID$iso),                                              # Fill in missing country/commodity combinations with 0
               GCAM_commodity, year, fill = list(value = 0)) ->
      L105.an_Prod_Mt_ctry_C_Y
    # Build region table, aggregate to GCAM region and commodity
    L105.FAO_an_Prod_Mt %>%
      group_by(GCAM_region_ID, GCAM_commodity, year) %>%                                      # Group by region, commodity, year
      summarize(value = sum(value)) %>%                                                       # Aggregate food consumption in Mt
      ungroup() %>%                                                                           # Ungroup before complete
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),                        # Fill in missing region/commodity combinations with 0
               GCAM_commodity, year, fill = list(value = 0)) ->
      L105.an_Prod_Mt_R_C_Y

    # Produce outputs
    L105.an_Food_Mt_R_C_Y %>%
      add_title("Animal consumption by GCAM region / commodity / year") %>%
      add_units("Mt") %>%
      add_comments("Aggregate FAO country and item data by GCAM region, commodity, and year") %>%
      add_comments("Convert data from ton to Mt") %>%
      add_legacy_name("L105.an_Food_Mt_R_C_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/FAO/FAO_an_items_cal_SUA",
                     "L100.FAO_an_Food_t") ->
      L105.an_Food_Mt_R_C_Y

    L105.an_Food_Pcal_R_C_Y %>%
      add_title("Animal consumption by GCAM region / commodity / year") %>%
      add_units("Pcal") %>%
      add_comments("Aggregate FAO country and item data by GCAM region, commodity, and year") %>%
      add_comments("Convert data from ton to Pcal") %>%
      add_legacy_name("L105.an_Food_Pcal_R_C_Y") %>%
      same_precursors_as(L105.an_Food_Mt_R_C_Y) ->
      L105.an_Food_Pcal_R_C_Y

    L105.an_kcalg_R_C_Y %>%
      add_title("Average caloric content of animal products by GCAM region / commodity / year") %>%
      add_units("kcal/g") %>%
      add_comments("Combine animal consumption in Mt (L105.an_Food_Mt_R_C_Y) and in Pcal (L105.an_Food_Pcal_R_C_Y)") %>%
      add_comments("Calculate the average caloric content as Pcal devided by Mt") %>%
      add_legacy_name("L105.an_kcalg_R_C_Y") %>%
      same_precursors_as(L105.an_Food_Mt_R_C_Y) ->
      L105.an_kcalg_R_C_Y

    L105.an_Prod_Mt_R_C_Y %>%
      add_title("Animal production by GCAM region / commodity / year") %>%
      add_units("Mt") %>%
      add_comments("Aggregate FAO country and item data by GCAM region commodity, and year") %>%
      add_comments("Convert data from ton to Mt") %>%
      add_legacy_name("L105.an_Prod_Mt_R_C_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/FAO/FAO_an_items_cal_SUA",
                     "L100.FAO_an_Prod_t") ->
      L105.an_Prod_Mt_R_C_Y

    L105.an_Prod_Mt_ctry_C_Y %>%
      add_title("Animal production by country / commodity / year") %>%
      add_units("Mt") %>%
      add_comments("Aggregate FAO country and item data by GCAM commodity, and year") %>%
      add_comments("Convert data from ton to Mt") %>%
      add_legacy_name("L105.an_Prod_Mt_ctry_C_Y") %>%
      same_precursors_as(L105.an_Prod_Mt_R_C_Y) ->
      L105.an_Prod_Mt_ctry_C_Y

# Moving L106 to here ----

    # Produce outputs
    L106.ag_NetExp_Mt_R_C_Y %>%
      add_title("Net exports of primary agricultural goods by GCAM region / commodity / year") %>%
      add_units("Mt") %>%
      add_comments("Aggregate FAO primary agricultural goods gross exports and imports and calculate net exports by GCAM region, commodity and year") %>%
      add_comments("Gross exports are adjusted so that global net exports add to zero") %>%
      add_comments("Re-calculate regional net exports using adjusted gross exports minus gross imports") %>%
      add_legacy_name("L106.ag_NetExp_Mt_R_C_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/FAO/L106.ag_NetExp_Mt_R_C_Y") ->
      L106.ag_NetExp_Mt_R_C_Y

    L106.an_NetExp_Mt_R_C_Y %>%
      add_title("Net exports of animal products by GCAM region / commodity / year") %>%
      add_units("Mt") %>%
      add_comments("Aggregate FAO animal products gross exports and imports and calculate net exports by GCAM region, commodity and year") %>%
      add_comments("Gross exports are adjusted so that global net exports add to zero") %>%
      add_comments("Re-calculate regional net exports using adjusted gross exports minus gross imports") %>%
      add_legacy_name("L106.an_NetExp_Mt_R_C_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/FAO/L106.an_NetExp_Mt_R_C_Y") ->
      L106.an_NetExp_Mt_R_C_Y

    # Moving L108 partly to here ----
    L100.FAO_ag_Feed_t %>%
      select(iso, item, year, value) %>%
      left_join(A_recent_feed_modifications, by = c("iso", "item", "year")) %>%                    # Swap in modified feed data where relevant
      mutate(value = if_else(is.na(feed), value, feed)) %>%
      select(-feed) %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%                                     # Map in GCAM region ID
      left_join(select(FAO_ag_items_cal_SUA, item, GCAM_commodity), by = "item") %>%               # Map in GCAM commodity
      filter(!is.na(GCAM_commodity)) %>%                                                           # Remove entries that are not GCAM comodities
      group_by(GCAM_region_ID, GCAM_commodity, year) %>%
      summarize(value = sum(value)) %>%                                                            # Aggregate by crop, region, year
      mutate(value = value * CONV_TON_MEGATON) %>%                                                 # Convert from tons to Mt
      ungroup() %>%
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),
               GCAM_commodity, year, fill = list(value = 0)) ->                                   # Fill in missing region/commodity combinations with 0
      L101.ag_Feed_Mt_R_C_Y


    L101.ag_Feed_Mt_R_C_Y %>%
      add_title("Feed use by GCAM region, commodity, and year aggregated from FAO") %>%
      add_units("Mt/yr") %>%
      add_legacy_name("L101.ag_Feed_Mt_R_C_Y") %>%
      add_precursors("common/iso_GCAM_regID", "aglu/FAO/FAO_ag_items_cal_SUA",
                     "aglu/A_recent_feed_modifications", "L100.FAO_ag_Feed_t") ->
      L101.ag_Feed_Mt_R_C_Y

    # Read in L132 prices and remove the chunk ----

    # Produce outputs

    # Return data ----
    return_data(L100.FAO_ag_HA_ha,
                L100.FAO_ag_Prod_t,
                L100.FAO_ag_Feed_t,
                L100.FAO_ag_Food_t,
                L100.FAO_an_Food_t,
                L100.FAO_an_Prod_t,
                L100.FAO_an_Stocks,
                L100.FAO_an_Dairy_Stocks,
                L100.FAO_CL_kha,
                L100.FAO_fallowland_kha,
                L100.FAO_harv_CL_kha,
                L100.FAO_Fert_Cons_tN,
                L100.FAO_Fert_Prod_tN,
                L100.FAO_For_Exp_m3,
                L100.FAO_For_Imp_m3,
                L100.FAO_For_Prod_m3,

                L101.ag_Food_Mt_R_C_Y, L101.ag_Food_Pcal_R_C_Y, L101.ag_kcalg_R_C_Y,
                L105.an_Food_Mt_R_C_Y, L105.an_Food_Pcal_R_C_Y, L105.an_kcalg_R_C_Y, L105.an_Prod_Mt_R_C_Y, L105.an_Prod_Mt_ctry_C_Y,
                L106.ag_NetExp_Mt_R_C_Y, L106.an_NetExp_Mt_R_C_Y,
                L101.ag_Feed_Mt_R_C_Y,
                L100.FAO_PRODSTAT_TO_DOWNSCAL
                )
  } else {
    stop("Unknown command")
  }
}

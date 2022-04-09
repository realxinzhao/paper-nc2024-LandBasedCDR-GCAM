library(dplyr)
library(tidyr)
library(tibble)
library(assertthat)
library(gcamdata)

source("R/constants.R")
source("R/utils.R")
source("R/utils-data.R")
source("R/pipeline-helpers.R")
source("R/module-helpers.R")

get_data <- function(Dir = "./outputs/", fn, strip_attributes = TRUE){

  if (grepl("/", fn)) {
    load_csv_files(fn, FALSE, quiet = TRUE)[[1]]
  } else
    if(grepl("/", fn) == F){
      read.csv(paste0(Dir,fn, ".csv"), comment.char = "#") %>% as_tibble()
    } else {
      stop("check path")
    }

}
all_data <- "./outputs/"

FAO_ag_Prod_t_HA_ha_PRODSTAT <- get_data(all_data, "./FAO_ag_Prod_t_HA_ha_PRODSTAT", strip_attributes = T)

FAO_GDP_Deflators <- get_data(all_data, "aglu/FAO/FAO_ag_an_ProducerPrice")
FAO_ag_Prod_t_HA_ha_PRODSTAT.csv
get_data(all_data, "aglu/FAO/FAO_ag_Prod_t_HA_ha_PRODSTAT")

Various_ag_resbio_data <- readr::read_csv("./inst/extdata/aglu/Various_ag_resbio_data.csv", comment = "#")

Mekonnen_Hoekstra_Rep47_A2 <- readr::read_csv("./inst/extdata/aglu/Mekonnen_Hoekstra_Rep47_A2.csv", comment = "#")


FAO_ag_Prod_t_HA_ha_PRODSTAT <- readr::read_csv("./inst/extdata/aglu/FAO/FAO_ag_Prod_t_HA_ha_PRODSTAT.csv.gz", comment = "#")
AGLU_ctry <- readr::read_csv("./inst/extdata/aglu/AGLU_ctry.csv", comment = "#")
FAO_ag_items_PRODSTAT <-  readr::read_csv("./inst/extdata/aglu/FAO/FAO_ag_items_PRODSTAT.csv", comment = "#")
iso_GCAM_regID


GCAM_AgLU_SUA_1973_2019 <- readr::read_csv("./inst/extdata/aglu/FAO/GCAM_AgLU_SUA_1973_2019.csv.gz", comment = "#")
SUA_food_MKcal_APE <- readr::read_csv("./inst/extdata/aglu/FAO/SUA_food_MKcal_APE.csv.gz", comment = "#")
FAO_an_Prod_t_1973_2019  <- readr::read_csv("./inst/extdata/aglu/FAO/FAO_an_Prod_t_1973_2019.csv.gz", comment = "#")










library(ggplot2)

# Unmanaged land value
L221.LN1_ValueLogit <- get_data(all_data, "L221.LN1_ValueLogit")

L221.LN1_ValueLogit %>%
  mutate(unManagedLandValue = unManagedLandValue/100000 * gdp_deflator(2015, 1975)) %>%
  separate(LandNode1, into = c("land", "basin")) %>% filter(land == "AgroForestLand") -> A

A%>% filter(unManagedLandValue<= 2000) %>%
  ggplot() + #facet_wrap(~region) +
  geom_density(aes(x = unManagedLandValue))+ theme_bw() +
  labs(x = "unmanaged land rent (2015$ per ha) truncated < 2000$") -> B

B %>% ggsave(filename = "unmanagedlandvaluedist1.png", device = "png")
  #geom_bar(aes(x = basin , y = unManagedLandValue), stat = "identity") + theme_bw()

A%>%
  ggplot() + #facet_wrap(~region) +
  #geom_density(aes(x = unManagedLandValue))+ theme_bw()
  geom_bar(aes(x = reorder(basin, unManagedLandValue), y = unManagedLandValue), stat = "identity") + theme_bw() +
  theme(axis.text.x = element_blank()) +
  labs(y = "unmanaged land rent (2015$ per ha)", x = "Basin") -> B
B %>% ggsave(filename = "unmanagedlandvaluedist.png", device = "png")
summary(A)



FAO_ag_HA_ha_PRODSTAT %>% gather_years() %>%
  filter(item %in% c("FodderHerb", "FodderGrass")) %>%
  group_by(item, year) %>% summarise(value = sum(value)) %>% spread(item, value) %>% filter(year >= 2000) ->A2



#--------------------------------
# Check aggregation
# That is confirming that gcamdata is
# aggregating FAO data to GCAM sectors
# and downscaling to GLU and tech x water

GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
L100.FAO_ag_Prod_t <- get_data(all_data, "L100.FAO_ag_Prod_t")
L101.ag_Prod_Mt_R_C_Y_GLU <- get_data(all_data, "L101.ag_Prod_Mt_R_C_Y_GLU")
L2012.AgProduction_ag_irr_mgmt <- get_data(all_data, "L2012.AgProduction_ag_irr_mgmt")
L181.ag_Prod_Mt_R_C_Y_GLU_irr_level <- get_data(all_data, "L181.ag_Prod_Mt_R_C_Y_GLU_irr_level")

L101.ag_Prod_Mt_R_C_Y_GLU %>% filter(year == 2015) %>%
  left_join(GCAM_region_names) %>%
  group_by(region, GCAM_commodity, year) %>%
  summarise(value = sum(value)) %>% rename(in_Mt = value) %>%
  left_join(
    L2012.AgProduction_ag_irr_mgmt %>% #filter(year == 2015) %>%
      group_by(region, AgSupplySector, year) %>%
      summarise(out_Mt = sum(calOutputValue)) %>%
      rename(GCAM_commodity = AgSupplySector)
  ) %>%
  mutate(diff_Mt = out_Mt - in_Mt) ->
  in_out_prod_ag

L101.ag_Prod_Mt_R_C_Y_GLU %>% #filter(year == 2015) %>%
  left_join(GCAM_region_names) %>%
  group_by(region, GCAM_subsector, GLU) %>%
  summarise(value = sum(value)) %>% rename(in_Mt = value) %>%
  left_join(
    L181.ag_Prod_Mt_R_C_Y_GLU_irr_level %>% #filter(year == 2015) %>%
      left_join(GCAM_region_names) %>%
      group_by(region, GCAM_subsector, GLU) %>%
      summarise(out_Mt = sum(value))
  ) %>%
  mutate(diff_Mt = out_Mt - in_Mt) %>%
  filter(diff_Mt >= 0.001)->
  in_out_prod_ag


L101.ag_Prod_Mt_R_C_Y_GLU %>% filter(year == 2015) %>%
  left_join(GCAM_region_names) %>%
  group_by(GCAM_commodity) %>%
  summarise(value = sum(value)) %>% rename(in_Mt = value) %>%
  left_join(
    FAO_PRODSTAT_DOWNSCALED %>% filter(year == 2015) %>%
      group_by(GCAM_commodity) %>%
      summarise(out_Mt = sum(production))
  ) %>%
  mutate(diff_Mt = out_Mt - in_Mt*1000000) ->
  in_out_prod_ag

L100.FAO_ag_Prod_t %>% filter(item != "Pumpkins for Fodder") %>%
  filter(year == 2015) %>%
  summarise(value = sum(value)/1000000)
FAO_PRODSTAT_DOWNSCALED %>%
  filter(year == 2015) %>%
  summarise(value = sum(production)/1000000)


L100.FAO_ag_Prod_t %>%
  filter(year == 2015, item %in% c("Maize", "Maize, green", "Popcorn")) %>%
  summarise(value = sum(value)/1000000)
#match!
#--------------------------------
# livestock prod
L105.an_Prod_Mt_R_C_Y <- get_data(all_data, "L105.an_Prod_Mt_R_C_Y")
L202.StubTechProd_an <- get_data(all_data, "L202.StubTechProd_an")

L105.an_Prod_Mt_R_C_Y %>% filter(year == 2015) %>%
  left_join(GCAM_region_names) %>% rename(in_Mt = value) %>%
  left_join(
    L202.StubTechProd_an %>% filter(year == 2015) %>%
      group_by(region, GCAM_commodity = supplysector, year) %>%
      summarise(out_Mt = sum(calOutputValue))
  ) %>%
  mutate(diff_Mt = out_Mt - in_Mt) ->
  in_out_prod_an

#--------------------------------
# Food downscale consistency
L101.ag_Food_Mt_R_C_Y <- get_data(all_data, "L101.ag_Food_Mt_R_C_Y")
L101.ag_Food_Pcal_R_C_Y <- get_data(all_data, "L101.ag_Food_Pcal_R_C_Y")

L105.an_Food_Mt_R_C_Y <- get_data(all_data, "L105.an_Food_Mt_R_C_Y")
L105.an_Food_Pcal_R_C_Y <- get_data(all_data, "L105.an_Food_Pcal_R_C_Y")


L203.StubCalorieContent <- get_data(all_data, "L203.StubCalorieContent")
L203.StubTechProd_food <- get_data(all_data, "L203.StubTechProd_food")

L203.StubTechProd_food %>% filter(year == 2015) %>% left_join(
  L203.StubCalorieContent %>% filter(year == 2015)
) %>% mutate(out_Mt = calOutputValue / efficiency) %>%
  select(region, GCAM_commodity = subsector, year, out_Pcal = calOutputValue, out_Mt) ->
  out_food

unique(A$GCAM_commodity)
L101.ag_Food_Pcal_R_C_Y %>% left_join(GCAM_region_names) %>%
  filter(year == 2015) %>% rename(in_Pcal = value) %>%
  left_join(L101.ag_Food_Mt_R_C_Y %>% rename(in_Mt = value)) %>%
  bind_rows(
    L105.an_Food_Pcal_R_C_Y %>% left_join(GCAM_region_names) %>%
      filter(year == 2015) %>% rename(in_Pcal = value) %>%
      left_join(L105.an_Food_Mt_R_C_Y %>% rename(in_Mt = value))
  ) %>%
  left_join(out_food) %>%
  mutate(diff_Mt = out_Mt - in_Mt,
         diff_Pcal = out_Pcal - in_Pcal)-> in_out_food
#-----------------------------------------------

# Area from L101.ag_HA_bm2_R_C_Y_GLU to L2252.LN5_MgdAllocation_crop
GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
L101.ag_HA_bm2_R_C_Y <- get_data(all_data, "L101.ag_HA_bm2_R_C_Y")
L101.ag_HA_bm2_R_C_Y_GLU <- get_data(all_data, "L101.ag_HA_bm2_R_C_Y_GLU")
L161.ag_irrHA_bm2_R_C_Y_GLU <- get_data(all_data, "L161.ag_irrHA_bm2_R_C_Y_GLU")
L161.ag_rfdHA_bm2_R_C_Y_GLU <- get_data(all_data, "L161.ag_rfdHA_bm2_R_C_Y_GLU")
L161.ag_irrProd_Mt_R_C_Y_GLU <- get_data(all_data, "L161.ag_irrProd_Mt_R_C_Y_GLU")
L161.ag_rfdProd_Mt_R_C_Y_GLU <- get_data(all_data, "L161.ag_rfdProd_Mt_R_C_Y_GLU")
L181.LC_bm2_R_C_Yh_GLU_irr_level <- get_data(all_data, "L181.LC_bm2_R_C_Yh_GLU_irr_level")

L2012.AgProduction_ag_irr_mgmt <- get_data(all_data, "L2012.AgProduction_ag_irr_mgmt")

L2252.LN5_MgdAllocation_crop <- get_data(all_data, "L2252.LN5_MgdAllocation_crop")
# harvest frequency
L2012.AgHAtoCL_irr_mgmt <- get_data(all_data, "L2012.AgHAtoCL_irr_mgmt")

L2252.LN5_MgdAllocation_crop %>% separate(LandNode4, into = c("crop", "basin")) ->
  L2252.LN5_MgdAllocation_crop1

L2252.LN5_MgdAllocation_crop1 %>% filter(year == 2015) %>%
  left_join(L2012.AgHAtoCL_irr_mgmt %>% filter(year == 2015) %>%
              select(region, year, LandLeaf = AgProductionTechnology, harvests.per.year)) ->
  L2252.LN5_MgdAllocation_crop2

L2252.LN5_MgdAllocation_crop2 %>% summarise(value = sum(allocation * harvests.per.year))


L101.ag_HA_bm2_R_C_Y_GLU %>% filter(year == 2015) %>%
  left_join(GCAM_region_names) %>%
  group_by(year, region, crop = GCAM_subsector
           ) %>%
  summarise(in_bm2 = sum(value)) %>%
  left_join(
    L2252.LN5_MgdAllocation_crop2 %>%
      group_by(year, region, crop
               ) %>%
      summarise(out_bm2 = sum(allocation *harvests.per.year
                              ))
  ) %>%
  mutate(diff_bm2 = (out_bm2 - in_bm2)*1000000) -> in_out_area

# match!

L2252.LN5_MgdAllocation_crop2 %>% filter(year == 2015) %>%
  group_by(year, region, crop
  )  %>%
  summarise(in_bm2 = sum(allocation)) %>%
  left_join(
    L181.LC_bm2_R_C_Yh_GLU_irr_level%>% filter(year == 2015)%>%
      left_join(GCAM_region_names) %>%
      group_by(year, region, crop = GCAM_subsector) %>%
      summarise(out_bm2 = sum(value))
  )%>%
  mutate(diff_bm2 = (out_bm2 - in_bm2)*1000000) -> in_out_area



L161.ag_irrHA_bm2_R_C_Y_GLU %>%
  bind_rows(L161.ag_rfdHA_bm2_R_C_Y_GLU) %>% filter(year == 2015) %>%
  group_by(GCAM_region_ID, GCAM_subsector, year) %>%
  summarise(value = sum(value)) -> L161HA

L101.ag_HA_bm2_R_C_Y_GLU %>% filter(year == 2015) %>%
  left_join(GCAM_region_names) %>%
  group_by(year, region, crop = GCAM_subsector
  ) %>%
  summarise(in_bm2 = sum(value)) %>%
  left_join(
    L161HA  %>%
      left_join(GCAM_region_names) %>%
      group_by(year, region, crop = GCAM_subsector) %>%
      summarise(out_bm2 = sum(value
      ))
  ) %>%
  mutate(diff_bm2 = abs(out_bm2 - in_bm2)*1000000) %>%
  filter(diff_bm2 > 0.00001)-> in_out_area


L161.ag_irrProd_Mt_R_C_Y_GLU %>%
  bind_rows(L161.ag_rfdProd_Mt_R_C_Y_GLU) %>% filter(year == 2015) %>%
  group_by(GCAM_region_ID, GCAM_subsector, year) %>%
  summarise(value = sum(value)) -> L161PROD
L101.ag_Prod_Mt_R_C_Y_GLU %>% filter(year == 2015) %>%
  left_join(GCAM_region_names) %>%
  group_by(year, region, crop = GCAM_subsector
  ) %>%
  summarise(in_bm2 = sum(value)) %>%
  left_join(
    L161PROD  %>%
      left_join(GCAM_region_names) %>%
      group_by(year, region, crop = GCAM_subsector) %>%
      summarise(out_bm2 = sum(value
      ))
  ) %>%
  mutate(diff_bm2 = abs(out_bm2 - in_bm2)*1000000) %>%
  filter(diff_bm2 > 0.00001)-> in_out_area




GCAM_APE_crops <- readRDS("C:/Users/zhao752/Dropbox/Z_PNNL_Info/Reading/FAOSTAT/FAOtoGCAM/output/RDS_Output/GCAM_APE_crops.rds")

GCAM_APE_crops %>% distinct(item) -> A
GCAM_APE_crops %>% filter(item == "Soybean_cake") %>%
  filter(element == "Production") %>%
  filter(area == "Brazil")

# Cost share ----

L132.ag_an_For_Prices %>% left_join(L133.ag_Cost_75USDkg_C) %>%
  left_join(L164.ag_Cost_75USDkg_C %>% rename(Cost_75USDkg_new = Cost_75USDkg)) %>%
  mutate(cr = Cost_75USDkg / calPrice,
         cr_new = Cost_75USDkg_new / calPrice)->A


# Downscal comparison--------

FAO_PRODSTAT_DOWNSCALED_new %>% filter(year == 2015) %>%
  group_by(GCAM_region_ID) %>%
  summarise(Prod_out_Mt = sum(production),
            Area_out_Ha = sum(harvested.area)) %>%
  ungroup() %>%
  left_join(
    FAO_PRODSTAT_DOWNSCALED %>% filter(year == 2015) %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      group_by(GCAM_region_ID) %>%
      summarise(Prod_in_Mt = sum(production),
                Area_in_Ha = sum(harvested.area)) %>%
      ungroup()
  ) %>%
  mutate(Prod_diff_Mt = Prod_in_Mt  - Prod_out_Mt,
         Area_diff_Mt = Area_in_Ha  - Area_out_Ha) %>%
  left_join(GCAM_region_names) -> A

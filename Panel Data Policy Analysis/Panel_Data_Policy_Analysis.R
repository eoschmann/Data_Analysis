####                        Supplementary R Script                          ####
#------------------------------------------------------------------------------#
###               DiD Policy Analysis Replication & Extension                ###
#------------------------------------------------------------------------------#

#------------------------------Research Question-------------------------------#
#         Replication and extension of the model, based on the paper: 
#
#                         Lin, B., & Li, J. (2026)
#                 "Does connectivity power solar energy? 
#     Evidence from the Belt and Road Initiative. Energy, 344, 139842.
#                https://doi.org/10.1016/j.energy.2025.139842


#------------------------------------------------------------------------------#
####                              1. Setup                                  ####
#------------------------------------------------------------------------------#

## Packages
#install.packages("tidyverse")
#install.packages("readxl")
#install.packages("janitor")
#install.packages("countrycode")
#install.packages("plm")
#install.packages("did")
#install.packages("car")
#install.packages("stargazer")
#install.packages("lubridate")
#install.packages("lmtest")
#install.packages("fixest")
#install.packages("ggplot2")
#install.packages("performance")
library(tidyverse)
library(readxl)
library(janitor)      
library(countrycode)
library(plm)
library(did)
library(car)
library(stargazer)
library(lubridate)
library(lmtest)
library(fixest)
library(ggplot2)
library(performance)
library(ggplot2)


#--------------------------1.1 World Bank controls-----------------------------#

## Variables for Replication 
wb <- read_csv("~/Desktop/REPLICATION_PAPER/Replication/World_Bank_Controls_Replication.csv", 
               na = c("..", "")) %>%
  clean_names() %>%
  rename(iso3c = country_code,
         year  = time,
         labor       = labor_force_total_sl_tlf_totl_in,
         gfcf        = gross_fixed_capital_formation_percent_of_gdp_ne_gdi_ftot_zs,
         gcf         = gross_capital_formation_percent_of_gdp_ne_gdi_totl_zs,
         fuel_exp    = fuel_exports_percent_of_merchandise_exports_tx_val_fuel_zs_un,
         ores_exp    = ores_and_metals_exports_percent_of_merchandise_exports_tx_val_mmtl_zs_un,
         fdi_usd     = foreign_direct_investment_net_inflows_bo_p_current_us_bx_klt_dinv_cd_wd,
         fdi_gdp     = foreign_direct_investment_net_inflows_percent_of_gdp_bx_klt_dinv_wd_gd_zs,
         exports_gdp = exports_of_goods_and_services_percent_of_gdp_ne_exp_gnfs_zs,
         gdp_pc_cur  = gdp_per_capita_current_us_ny_gdp_pcap_cd,
         gdp_pc      = gdp_per_capita_constant_2015_us_ny_gdp_pcap_kd,
         gdp_pc_ppp  = gdp_per_capita_ppp_current_international_ny_gdp_pcap_pp_cd,
         lpi         = logistics_performance_index_overall_1_low_to_5_high_lp_lpi_ovrl_xq)


#-----------------------1.2 World Bank Political Controls----------------------#

## Additional Political Controls 
wb2 <- read.csv("~/Desktop/DATA/WB_Pol_Indicators/WB_Pol_Indicators.csv",
                na = c("..", "")) %>%
  clean_names() %>%
  rename(iso3c = country_code,
         year  = time,
         # --- Worldwide Governance Indicators (0-100 percentile-rank scores) ---
         wgi_polstab      = political_stability_governance_score_0_100_gov_wgi_pv_sc,
         wgi_corruption   = control_of_corruption_governance_score_0_100_gov_wgi_cc_sc,   # higher = LESS corrupt
         wgi_goveffect    = government_effectiveness_governance_score_0_100_gov_wgi_ge_sc,
         wgi_regquality   = regulatory_quality_governance_score_0_100_gov_wgi_rq_sc,
         wgi_ruleoflaw    = rule_of_law_governance_score_0_100_gov_wgi_rl_sc,
         wgi_voice        = voice_and_accountability_governance_score_0_100_gov_wgi_va_sc,
         # --- Fiscal / macro ---
         govt_debt_gdp    = central_government_debt_total_of_gdp_gc_dod_totl_gd_zs,
         real_interest    = real_interest_rate_fr_inr_rinr,
         cpia_fiscal      = cpia_fiscal_policy_rating_1_low_to_6_high_iq_cpa_fisp_xq,
         cpia_proprights  = cpia_property_rights_and_rule_based_governance_rating_1_low_to_6_high_iq_cpa_prop_xq,
         # --- Military / security ---
         milex_govt       = military_expenditure_of_general_government_expenditure_ms_mil_xpnd_zs,
         arms_exports     = arms_exports_sipri_trend_indicator_values_ms_mil_xprt_kd,
         arms_imports     = arms_imports_sipri_trend_indicator_values_ms_mil_mprt_kd,
         # --- Energy investment (directly on-topic for solar) ---
         energy_ppi       = investment_in_energy_with_private_participation_current_us_ie_ppi_engy_cd,
         energy_ppp       = public_private_partnerships_investment_in_energy_current_us_ie_ppn_engy_cd,
         # --- Aid, tourism, land ---
         oda_received     = net_official_development_assistance_and_official_aid_received_constant_2023_us_dt_oda_alld_kd,
         tourism_departs  = international_tourism_number_of_departures_st_int_dprt,
         land_area        = land_area_sq_km_ag_lnd_totl_k2,
         forest_pct       = forest_area_of_land_area_ag_lnd_frst_zs,
         rural_land       = rural_land_area_sq_km_ag_lnd_totl_ru_k2)

summary(wb2$milex_govt)

#--------------------------1.3 IRENA solar generation--------------------------#

## IRENA: MISSING VALUES ARE REMOVED
irena <- read_csv(
  "~/Desktop/REPLICATION_PAPER/Replication/C-ELECGEN_20260715-181305.csv",
  skip = 2,
  skip_empty_rows = FALSE,
  na = "-",
  locale = locale(encoding = "latin1")) %>%
  clean_names() %>%
  filter(grid_connection == "All",
         technology == "Solar photovoltaic") %>%
  group_by(country_area, year) %>%
  summarise(
    solar_gwh = if (
      all(is.na(electricity_generation_statistics))) {
      NA_real_} else {
      sum(electricity_generation_statistics, na.rm = TRUE)},
    .groups = "drop") %>%
    mutate(
    iso3c = countrycode(
      country_area,
      "country.name",
      "iso3c",
      custom_match = c("Kosovo" = "XKX"))) %>% select(iso3c, year, solar_gwh)

## IRENA2: MISSING VALUES OF GENERATION ARE ASSUMED TO BE 0
irena2 <- read_excel(
  "~/Desktop/REPLICATION_PAPER/EXTENSION/V1_C-ELECGEN_20260803-135009.xlsx",
  sheet     = "C-ELECGEN",
  skip      = 2,
  col_names = c("country_area", "technology", "data_type",
                "grid_connection", "year", "electricity_generation_statistics")) %>%
  fill(country_area, grid_connection) %>%
  filter(grid_connection == "All") %>%
  group_by(country_area, year) %>%
  summarise(
    solar_gwh = if (
      all(is.na(electricity_generation_statistics))) {NA_real_} else {
      sum(electricity_generation_statistics, na.rm = TRUE)},
    .groups = "drop") %>%
  mutate(year  = as.integer(year),
         iso3c = countrycode( 
           country_area,
           "country.name",
           "iso3c",
        custom_match = c("Kosovo" = "XKX"))) %>% select(iso3c, year, solar_gwh)


#--------------------------1.3 BRI participation-------------------------------#

bri <- read_excel(
  "~/Desktop/REPLICATION_PAPER/EXTENSION/V3_BRI_Participation_Dates .xlsx",
  sheet = "BRI countries",
  col_types = c("text", "text", "text", "text", "date", "date")) %>%
  clean_names() %>%
  rename(iso3c     = country_code,
         join_date = likely_date_of_joining,
         exit_date = likely_date_of_exit) %>%
  mutate(join_year = year(join_date)) %>%
  select(iso3c, region, income_group, join_date, exit_date, join_year)


#-------------------1.4 Weather Controls (CCKP / ERA5)-------------------------#

wx_path <- "~/Desktop/REPLICATION_PAPER/Replication/Weather_WB.xlsx"
wx_cwd <- read_excel(wx_path, sheet = "cwd") %>%
  clean_names() %>%
  select(iso3c = code, matches("^x\\d{4}_07$")) %>%
  pivot_longer(-iso3c, names_to = "year", values_to = "wx_cwd") %>%     # consecutive wet days
  mutate(year = as.integer(str_sub(year, 2, 5)))

wx_hurs <- read_excel(wx_path, sheet = "hurs") %>%
  clean_names() %>%
  select(iso3c = code, matches("^x\\d{4}_07$")) %>%
  pivot_longer(-iso3c, names_to = "year", values_to = "wx_hurs") %>%    # relative humidity, %
  mutate(year = as.integer(str_sub(year, 2, 5)))

wx_sd <- read_excel(wx_path, sheet = "sd") %>%
  clean_names() %>%
  select(iso3c = code, matches("^x\\d{4}_07$")) %>%
  pivot_longer(-iso3c, names_to = "year", values_to = "wx_sd") %>%      # summer days (Tmax > 25C)
  mutate(year = as.integer(str_sub(year, 2, 5)))

weather <- wx_cwd %>%
  left_join(wx_hurs, by = c("iso3c", "year")) %>%
  left_join(wx_sd,   by = c("iso3c", "year"))


#-------------------1.5 Foreign Exchange Control-------------------------------#
fx <- read_csv(
  "~/Desktop/REPLICATION_PAPER/Replication/EXCHANGE_RATE_WB.csv",
  skip = 4, skip_empty_rows = FALSE,
  col_types = cols(.default = "c")   # year columns mix numbers and blanks
) %>%
  clean_names() %>%
  select(iso3c = country_code, matches("^x\\d{4}$")) %>%
  pivot_longer(-iso3c, names_to = "year", values_to = "fx_lcu_per_usd") %>%
  mutate(year           = as.integer(str_remove(year, "^x")),
         fx_lcu_per_usd = as.numeric(fx_lcu_per_usd))


#--------------------------1.6 Solar Panel Prices------------------------------#

solar_price <- read_csv("~/Desktop/REPLICATION_PAPER/Replication/Solar_PV_Prices.csv") %>%
  clean_names() %>%
  filter(entity == "World") %>%
  select(year, solar_pv_price_usd = solar_pv_module_cost)


#------------------------1.7 V-Dem Control Variables---------------------------#

vdem <- read_csv("~/Desktop/REPLICATION_PAPER/EXTENSION/V-Dem-CY-Full+Others-v16.csv",
                 col_select = all_of(c("country_text_id","year",
                                       "v2xcl_prpty","v2x_rule","v2x_corr",
                                       "v2clrspct","v2exrescon","v2svinlaut",
                                       "v2clstown","v2svdomaut","v2csreprss",
                                       "v2cscnsult","v2cacritic","v2dlcommon",
                                       "v2stfisccap_ord","v2casoe_0",
                                       "v2casoe_nr","v2x_polyarchy", 
                                       "v2svstterr"))) %>%
  filter(year >= 1995, year <= 2025) %>%
  rename(iso3c              = country_text_id,
         property_rights    = v2xcl_prpty,       # 5.9.3   higher = stronger
         rule_of_law        = v2x_rule,          # 5.9.1   higher = stronger
         corruption         = v2x_corr,          # 5.7.1   *** higher = MORE corrupt ***
         impartial_admin    = v2clrspct,         # 3.9.2.2 higher = more impartial
         exec_constitution  = v2exrescon,        # higher = executive more compliant
         intl_autonomy      = v2svinlaut,        # higher = more autonomous (foreign policy)
         state_ownership    = v2clstown,         # higher = MORE state control of economy
         domestic_autonomy  = v2svdomaut,        # higher = more autonomous (domestic policy)
         cso_repression     = v2csreprss,        # *** lower = MORE repression ***
         cso_consultation   = v2cscnsult,        # higher = more consulted
         academic_freedom   = v2cacritic,
         fiscal_capacity    = v2stfisccap_ord,
         dem_index      = v2x_polyarchy,
         authority_territory = v2svstterr)%>%
  mutate(
    # v2casoe is one-hot: v2casoe_0 == 1 means "no emergency". Complement = emergency dummy.
    # v2casoe_nr == 1 is "no data" -> must be NA, not coded as emergency.
    state_of_emergency = if_else(v2casoe_nr == 1, NA_integer_, 1L - v2casoe_0)
  ) %>%
  select(-v2casoe_0, -v2casoe_nr) 


#------------------1.8 IMF Financial Indicators--------------------------------#

imf_debt <- read_csv(
  "~/Desktop/REPLICATION_PAPER/EXTENSION/GROSS&NET_DEBT_IMF.csv"
) %>%
  clean_names() %>%
  filter(indicator_id == "GGXWDG_NGDP") %>%          
  transmute(iso3c     = country_id,
            year      = as.integer(time_period),
            debt_gdp  = as.numeric(obs_value)) %>%
  filter(!is.na(iso3c), !is.na(year), !is.na(debt_gdp))

imf_interest <- read.csv("~/Desktop/BA2/REPLICATION_PAPER/EXTENSION/Interest_rates_IMF_2000_2025.csv") %>%
  clean_names() %>%
  filter(indicator_id == "MFS162_RT_PT_A_PT") %>%          # Lending Rate, % per annum
  transmute(iso3c       = country_id,
            year        = as.integer(time_period),
            interest_rt = as.numeric(obs_value)) %>%
  filter(!is.na(iso3c), !is.na(year), !is.na(interest_rt))

# ----------------------------1.9 Merge ---------------------------------------#

## Panel with irena - 0 values omitted
panel <- wb %>%
  left_join(irena,       by = c("iso3c", "year")) %>%
  left_join(bri,         by = "iso3c") %>%
  left_join(solar_price, by = "year") %>%
  left_join(fx,          by = c("iso3c", "year")) %>%
  left_join(vdem,        by = c("iso3c", "year")) %>%
  left_join(wb2,         by = c("iso3c", "year")) %>%
  left_join(gcdf_energy,        by = c("iso3c"))  %>%
  left_join(wx_cwd,      by = c("iso3c", "year")) %>%
  left_join(wx_hurs,     by = c("iso3c", "year")) %>%
  left_join(wx_sd,       by = c("iso3c", "year")) %>%
  left_join(imf_debt, by = c("iso3c", "year")) %>%
  left_join(imf_interest, by = c("iso3c", "year"))

## Panel with irena2 - 0 values coded correctly 
panel2 <- wb %>%
  left_join(irena2,       by = c("iso3c", "year")) %>%
  left_join(bri,         by = "iso3c") %>%
  left_join(solar_price, by = "year") %>%
  left_join(fx,          by = c("iso3c", "year")) %>%
  left_join(vdem,        by = c("iso3c", "year")) %>%
  left_join(wb2,         by = c("iso3c", "year")) %>%
  left_join(gcdf_energy,        by = c("iso3c"))  %>%
  left_join(wx_cwd,      by = c("iso3c", "year")) %>%
  left_join(wx_hurs,     by = c("iso3c", "year")) %>%
  left_join(wx_sd,       by = c("iso3c", "year")) %>%
  left_join(imf_debt, by = c("iso3c", "year")) %>%
  left_join(imf_interest, by = c("iso3c", "year"))

## Optional: a time-varying "is this country in the BRI this year?" dummy.      1
panel <- panel %>%
  mutate(bri_member = as.integer(!is.na(join_year) & year >= join_year))

## Optional: a time-varying "is this country in the BRI this year?" dummy.      2 
panel2 <- panel2 %>%
  mutate(bri_member = as.integer(!is.na(join_year) & year >= join_year))

## Final Merge
write_csv(panel, "merged_panel2.csv")
dput(names(panel))

## Find in Downloads
getwd()                          
file.path(getwd(), "merged_panel2.csv") 


#------------------------------------------------------------------------------#
####                      2. Descriptive Statistics                         ####
#------------------------------------------------------------------------------#
## Sanity check for correct dataset merges
names(panel)

## Difference between initial approach (NA's omitted) and new approach (NA's as 0)
summary(irena$solar_gwh)   # mean = 2015.47
summary(irena2$solar_gwh)  # mean = 1397.63


#------------------------------------------------------------------------------#
####                           3. Transformation                            ####
#------------------------------------------------------------------------------#

#----------------------------3.1 DV-Transformation-----------------------------#

## Paper Comparable Transformation
panel$lnsolar <- log(pmax(panel$solar_gwh, 0.001))

## Alternative specification (1)
panel$lnsolar1 <- ifelse(panel$solar_gwh > 0, log(panel$solar_gwh), NA)

##Alternative log specification (2)
panel$lnsolar2 <- log(1 + panel$solar_gwh)


#(2)--------------------------3.1.1 DV-Transformation--------------------------#

## Paper Comparable Transformation
panel2$lnsolarV2 <- log(pmax(panel2$solar_gwh, 0.001))

## Alternative specification (1)
panel2$lnsolar1V2 <- ifelse(panel2$solar_gwh > 0, log(panel2$solar_gwh), NA)

##Alternative log specification (2)
panel2$lnsolar2V2 <- log(1 + panel2$solar_gwh)


#----------------------------3.2 IV-Transformation-----------------------------#

## Log(Labor Force)
panel$lnlabor <- log(panel$labor)

## Log(GDP per capita current U$D)
panel$lngdp  <- log(panel$gdp_pc_cur)

## Alternative Log(GDP_pc_ppp)
panel$lngdp_ppp <- log(panel$gdp_pc_ppp)
## Worst alternative: Data much further from actual paper. 

##Alternative Log(GDP_pc_2015)
panel$lngdp2015 <- log(panel$gdp_pc)

# Paper-like IFDI
panel$ifdi2 <- ifelse(is.na(panel$fdi_usd),NA,log(pmax(panel$fdi_usd / 10000, 0.01)))
## Other IFDI for 0 Values
panel$ifdi_ihs <- asinh(panel$fdi_usd)

## Paper-like Ores variable: Fuel, ores and metals exports/merchandise exports 
panel$resource_expo <- panel$ores_exp + panel$fuel_exp

## Tourism 
panel$lntourism <- log(panel$tourism_departs)

## Land area
panel$lnland <- log(panel$land_area)

## Financing before 
panel$energy_before_bri <- as.integer(!is.na(panel$first_energy_year) &
                                        panel$first_energy_year < panel$join_year)


#-(2)-------------------------3.2.1 IV-Transformation--------------------------#

## Log(Labor Force)
panel2$lnlaborV2 <- log(panel2$labor)

## Log(GDP per capita current U$D)
panel2$lngdpV2   <- log(panel2$gdp_pc_cur)

## Alternative Log(GDP_pc_ppp)
panel2$lngdp_pppV2 <- log(panel2$gdp_pc_ppp)
## Worst alternative: Data much further from actual paper 

##Alternative Log(GDP_pc_2015)
panel2$lngdp2015V2 <- log(panel2$gdp_pc)

# Paper-like IFDI
panel2$ifdi2V2 <- ifelse(is.na(panel2$fdi_usd),NA,log(pmax(panel2$fdi_usd / 10000, 0.01)))
## Other IFDI for 0 Values
panel2$ifdi_ihsV2 <- asinh(panel2$fdi_usd)

## Paper-like Ores variable: Fuel, ores and metals exports/merchandise exports 
panel2$resource_expoV2 <- panel2$ores_exp + panel2$fuel_exp

## Tourism 
panel2$lntourismV2 <- log(panel2$tourism_departs)

## Land area
panel2$lnlandV2 <- log(panel2$land_area)

## Financing before 
panel2$energy_before_briV2 <- as.integer(!is.na(panel2$first_energy_year) &
                                           panel2$first_energy_year < panel2$join_year)


#---------------------------3.3 PV-Price-Indicator-----------------------------#

## Solar price logged
panel$lnpv_price <- log(panel$solar_pv_price_usd)

## Solar panel price indicator
price_pv <- panel$solar_pv_price_usd * panel$fx_lcu_per_usd
## Log indicator
panel$lnprice_pvfx <- log(price_pv)


#---(2)---------------------3.3.1 PV-Price-Indicator---------------------------#

## Solar price logged
panel2$lnpv_priceV2 <- log(panel2$solar_pv_price_usd)

## Solar panel price indicator
price_pvV2 <- panel2$solar_pv_price_usd * panel2$fx_lcu_per_usd

## Log indicator
panel2$lnprice_pvfxV2 <- log(price_pvV2)


#--(2)---------------------3.3.1 PV-Price-Indicator----------------------------#

## Solar price logged
panel2$lnpv_priceV2 <- log(panel2$solar_pv_price_usd)

## Solar panel price indicator
price_pvV2 <- panel2$solar_pv_price_usd * panel2$fx_lcu_per_usd

## Log indicator
panel2$lnprice_pvfxV2 <- log(price_pvV2)

#-------------------------3.4.1 Fiscal Capacity Dummy--------------------------#

panel$fiscresource <- ifelse(!is.na(panel$fiscal_capacity) & panel$fiscal_capacity == "resource", 1, 0)
panel$fiscweak <- ifelse(!is.na(panel$fiscal_capacity) & panel$fiscal_capacity == "weak", 1, 0)
panel$fisctaxstate <- ifelse(!is.na(panel$fiscal_capacity) & panel$fiscal_capacity == "taxstate", 1, 0)


#----(2)----------------3.4.1 Fiscal Capacity Dummy----------------------------#

panel2$fiscresource <- ifelse(!is.na(panel2$fiscal_capacity) & panel2$fiscal_capacity == "resource", 1, 0)
panel2$fiscweak <- ifelse(!is.na(panel2$fiscal_capacity) & panel2$fiscal_capacity == "weak", 1, 0)
panel2$fisctaxstate <- ifelse(!is.na(panel2$fiscal_capacity) & panel2$fiscal_capacity == "taxstate", 1, 0)


#------------------------------------------------------------------------------#
####               4. Panel Structure Transformation for DiD                ####
#------------------------------------------------------------------------------#

#--------------------4.1 DiD Interaction Creation------------------------------#

## Staggered DiD Term
# join year & month off the date
panel$join_year  <- as.integer(format(panel$join_date, "%Y"))
panel$join_month <- as.integer(format(panel$join_date, "%m"))

# treatment/control group dummy: 1 if the country ever joined the BRI  (your d2)
panel$treat <- ifelse(!is.na(panel$join_year), 1, 0)

# effective start year: December signers count from the next year
panel$treat_start <- panel$join_year + ifelse(panel$join_month == 12, 1, 0)

# before/after (post) dummy: 1 from the country's own start year onward  (your p2)
panel$post <- ifelse(!is.na(panel$treat_start) & panel$year >= panel$treat_start, 1, 0)

# DID = treat x post  (your d2 * p2)
panel$did <- panel$treat * panel$post

## Sanity check
identical(panel$did, panel$post)


#--(2)-----------------4.1 DiD Interaction Creation----------------------------#

## Staggered DiD Term
# join year & month off the date
panel2$join_year  <- as.integer(format(panel2$join_date, "%Y"))
panel2$join_month <- as.integer(format(panel2$join_date, "%m"))

# treatment/control group dummy: 1 if the country ever joined the BRI  (your d2)
panel2$treat <- ifelse(!is.na(panel2$join_year), 1, 0)

# effective start year: December signers count from the next year
panel2$treat_start <- panel2$join_year + ifelse(panel2$join_month == 12, 1, 0)

# before/after (post) dummy: 1 from the country's own start year onward  (your p2)
panel2$post <- ifelse(!is.na(panel2$treat_start) & panel2$year >= panel2$treat_start, 1, 0)

# DID = treat x post  (your d2 * p2)
panel2$didV2 <- panel2$treat * panel2$post

## Sanity check
identical(panel2$didV2, panel2$post)


#--------------------4.2 Listing Countries in each Group-----------------------#

## Distinguishing Treatment and Control Group Countries
s <- panel[!duplicated(panel$iso3c), c("iso3c", "treat_start")]

## Treated
s$iso3c[!is.na(s$treat_start)]

## Never treated
s$iso3c[is.na(s$treat_start)]

## Counts
table(s$treat_start, useNA = "ifany")

s$iso3c[is.na(s$treat_start) & is.na(s$cso_repression)]

## Unique id-year (att_gt requirement)
sum(duplicated(panel[, c("iso3c", "year")]))

## Country that joined BRI in 2025
panel %>%
  filter(treat_start == 2025) %>%
  distinct(iso3c, join_date, join_year, join_month)

## Country that joined BRI in 2024
panel %>%
  filter(treat_start == 2023) %>%
  distinct(iso3c, join_date, join_year, join_month)

#------------------------------------------------------------------------------#
####                            5. Panel Data                               ####
#------------------------------------------------------------------------------#

#--------------------------5.2 Panel Data Frame -------------------------------#

## V2 stands for all changes with respect to how NA's are coded in irena
## V2 = Missing values are coded as 0 instead of NA

## Dataframe ORIGINAL
pdata <- pdata.frame(panel, index = c("iso3c", "year"))


## Dataframe ALL 0's
pdata2 <- pdata.frame(panel2, index = c("iso3c", "year"))


## Dataframe ORIGINAL NO CHN
pdata_noCHN <- pdata.frame(
  subset(panel, !is.na(iso3c) & iso3c != "CHN"),
  index = c("iso3c", "year"))

## Dataframe ALL 0's NO CHN
pdata_noCHN2 <- pdata.frame(
  subset(panel2, !is.na(iso3c) & iso3c != "CHN"),
  index = c("iso3c", "year"))


## Dataframe ALL 0's NO MICROSTATES
excl <- c("ABW","ASM","BMU","CHI","CUW","CYM","FRO","GIB","GRL","GUM","HKG","IMN",
          "MAC","MAF","MNP","NCL","PRI","PYF","SXM","TCA","VGB","VIR",          
          "AND","BHS","BLZ","KNA","LCA","LIE","MCO","MHL","NRU","PLW","SMR",
          "STP","TUV","VCT") 
pdata_sov <- pdata.frame(
  subset(panel2, !is.na(iso3c) & iso3c != "CHN" & !(iso3c %in% excl)),
  index = c("iso3c", "year"))


## Sanity check and duplicate observation numbers
sum(duplicated(panel[, c("iso3c", "year")]))
sum(duplicated(panel2[, c("iso3c", "year")]))
sum(duplicated(pdata_noCHN[, c("iso3c", "year")]))
sum(duplicated(pdata_noCHN2[, c("iso3c", "year")]))


#------------------------------------------------------------------------------#
####                    6. Replication PANEL                                ####
#------------------------------------------------------------------------------#

#---------------------6.1 Baseline Model Replication---------------------------#
## Baseline model from Lin, B., & Li, J. (2026)
## Authors did not share data or code upon request
## Data and time frame from the same sources

## Baseline formula
f <- lnsolar ~ did + lngdp + lnlabor + gcf +
  resource_expo +      
  ifdi2 + exports_gdp + wx_cwd + wx_sd + rule_of_law

## Baseline Models
# (1) no FE
mod1 <- plm(f, data = pdata, model = "pooling") 

# (2) country FE
mod2 <- plm(f, data = pdata, model = "within", effect = "individual")  

# (3) country + year FE
mod3 <- plm(f, data = pdata, model = "within", effect = "twoways")     

## Overview
summary(mod1, vcov = vcovHC)
summary(mod2, vcov = vcovHC)
summary(mod3, vcov = vcovHC)

## R2 calculation
y <- model.response(model.frame(mod3))

rss <- sum(residuals(mod3)^2)
tss <- sum((y - mean(y))^2)

r2_full <- 1 - rss / tss

r2_full
#------------------------6.2 Panel Model Extension-----------------------------#

## 2a) 
mod2a <- plm( lnsolar ~ did + wx_sd + wx_cwd + lnlabor + gcf + ifdi2
              + lngdp + rule_of_law
              , model = "within", effect = "twoways", data = pdata)
summary(mod2a, vcov = vcovHC)

## 2a) R2 calculation
y <- model.response(model.frame(mod2a))

rss <- sum(residuals(mod2a)^2)
tss <- sum((y - mean(y))^2)

r2_full <- 1 - rss / tss

r2_full

## 2b) 
mod2b <- plm(lnsolar ~ did + pdata$lngdp 
             , model = "within", effect = "twoways", data = pdata)
summary(mod2b, vcov = vcovHC)


## 2b) R2 calculation
y <- model.response(model.frame(mod2b))

rss <- sum(residuals(mod2b)^2)
tss <- sum((y - mean(y))^2)

r2_full <- 1 - rss / tss

r2_full


#------------------------------------------------------------------------------#
####                 7. Replication PANEL NO CHN                            ####
#------------------------------------------------------------------------------#
## Unclear whether authors include or remove China as BRI member
## Following approach excludes China from treatment group
## all with pdata_noCHN

#--------------------7.1 No China Baseline Rep. Model--------------------------#

er <- lnsolar ~ did + lngdp + lnlabor + gcf +
  resource_expo +      
  ifdi2 + exports_gdp + wx_cwd + wx_sd

## Baseline Models
# (1) no FE
mod1 <- plm(er, data = pdata_noCHN, model = "pooling") 

# (2) country FE
mod2 <- plm(er, data = pdata_noCHN, model = "within", effect = "individual")  

# (3) country + year FE
mod3 <- plm(er, data = pdata_noCHN, model = "within", effect = "twoways")     

## Overview
summary(mod1, vcov = vcovHC)
summary(mod2, vcov = vcovHC)
summary(mod3, vcov = vcovHC)

## R2 calculation
y <- model.response(model.frame(mod3))

rss <- sum(residuals(mod3)^2)
tss <- sum((y - mean(y))^2)

r2_full <- 1 - rss / tss

r2_full


#--------------------7.2 No China Panel Extensions-----------------------------#

## 2a) Additional political controls

mod2c <- plm(lnsolar ~ did + lngdp + wx_sd + wx_cwd + ifdi2 + gcf +
               cso_repression +
               dem_index + resource_expo * solar_pv_price_usd + 
               property_rights,
             model = "within", 
             effect = "twoways",data= pdata_noCHN)
summary(mod2c, vcov = vcovHC)

## 2b) only OLS
mod2b <- plm(lnsolar ~ did + lngdp + wx_sd + wx_cwd + 
               did * cso_repression , model = "within", 
             effect = "twoways" ,data= pdata_noCHN)
summary(mod2c, vcov = vcovHC)

mod2c <- plm(lnsolar ~ did + lngdp + wx_sd + wx_cwd 
             did * cso_repression , model = "within", 
             effect = "twoways" ,data= pdata)
summary(mod2b, vcov = vcovHC)

## 2b) R2 calculation
y <- model.response(model.frame(mod2b))

rss <- sum(residuals(mod2b)^2)
tss <- sum((y - mean(y))^2)

r2_full <- 1 - rss / tss

r2_full


## 3a)
mod3a <- plm(lnsolar ~ lngdp + 
               did * pdata_noCHN$cso_repression + did * cpia_proprights, 
             data = pdata_noCHN, model = "within", effect = "twoways")
summary(mod3a, vcov = vcovHC)

## 3b) 
mod3b <- plm(lnsolar ~ did + lngdp + 
               did * gcf + pdata_noCHN$wgi_goveffect
             + wgi_regquality + wgi_polstab + corruption
             + pdata_noCHN$wgi_voice * did
             , data = pdata_noCHN, model = "within", effect = "twoways")
summary(mod3b, vcov = vcovHC)

## 3c)
mod3c <- plm(lnsolar ~ did + lngdp + did * cpia_proprights
             + ifdi2 +  did: cso_repression + did * lag(real_interest, 3) ,
             data = pdata_noCHN,
             model = "within", effect = "twoways" )
summary(mod3c, vcov = vcovHC)
## CSO Repression stays relevant at 5% level, exec_constitution on 10% level


#------------------------------------------------------------------------------#
####                 8. Replication PANEL2 NO CHN                           ####
#------------------------------------------------------------------------------#

#--------------------8.1 No China Baseline Rep. Model--------------------------#

## Baseline formula
f <- lnsolarV2 ~ didV2 + lngdpV2 * didV2 + cso_repression + lnpv_priceV2 * wx_sd +
  lnpv_priceV2 * wx_cwd 

## Baseline Models
# (1) no FE
mod1 <- plm(f, data = pdata_noCHN2, model = "pooling") 

# (2) country FE
mod2 <- plm(f, data = pdata_noCHN2, model = "within", effect = "individual")  

# (3) country + year FE
mod3 <- plm(f, data = pdata_noCHN2, model = "within", effect = "twoways")     

## Overview
summary(mod1, vcov = vcovHC)
summary(mod2, vcov = vcovHC)
summary(mod3, vcov = vcovHC)

## R2 calculation
y <- model.response(model.frame(mod3))

rss <- sum(residuals(mod3)^2)
tss <- sum((y - mean(y))^2)

r2_full <- 1 - rss / tss

r2_full


#--------------------7.2 No China Panel2 Extensions----------------------------#

## a) Model with WGI Political indicators

moda1 <- plm(lnsolarV2 ~ didV2 + didV2 * wgi_ruleoflaw
             + didV2 * real_interest + didV2 * wgi_regquality 
             +cso_repression * didV2,
             model = "within", effect = "twoways", data = pdata_noCHN2)
summary(moda1, vcov = vcovHC)


## b) Model with WGI Political indicators

modb1 <- plm(lnsolarV2 ~ didV2 + lnlaborV2 + didV2 * cpia_proprights 
             + didV2 * wgi_ruleoflaw
             + didV2 * state_ownership + didV2 * cso_repression + didV2 * lnpv_priceV2,
             model = "within", effect = "twoways", data = pdata_noCHN2)
summary(modb1, vcov = vcovHC)


## d) Model with cpia_proprights

modd1 <- plm(lnsolarV2 ~ didV2 +
               + didV2 * cpia_proprights + didV2 * cso_repression + wx_sd ,
             model = "within", effect = "twoways", data = pdata_noCHN2)
summary(modd1, vcov = vcovHC)

## Good specification
mod.spec <- plm( lnsolarV2 ~ didV2 + lnlaborV2 + didV2 * lngdpV2 + 
                   wx_sd * lnpv_priceV2 + didV2 * cpia_proprights + didV2 * 
                   cso_repression + didV2 * domestic_autonomy, data = pdata_noCHN2, 
                 effect = "twoways", model = "within")
summary(mod.spec, vcov = vcovHC)

## R2 calculation
y <- model.response(model.frame(mod.spec))

rss <- sum(residuals(mod.spec)^2)
tss <- sum((y - mean(y))^2)

r2_full <- 1 - rss / tss

r2_full

## Model with centered variables 
pdata_noCHN2$pr_c <- pdata_noCHN2$cpia_proprights - mean(pdata_noCHN2$cpia_proprights, na.rm = TRUE)
pdata_noCHN2$pr2_c <-pdata_noCHN2$property_rights - mean(pdata_noCHN2$property_rights, na.rm = TRUE)
pdata_noCHN2$v2dl_c <-pdata_noCHN2$v2dlcommon - mean(pdata_noCHN2$v2dlcommon, na.rm = TRUE)

## Good specification (2)
mod.spec <- plm( lnsolarV2 ~ didV2 + lngdpV2 + wx_sd +
                   lnpv_priceV2 + didV2 * pr2_c +
                   cso_repression + didV2 * v2dl_c + dem_index, data = pdata_noCHN2, 
                 effect = "twoways", model = "within")
summary(mod.spec, vcov = vcovHC)

## R2 calculation
y <- model.response(model.frame(mod.spec))

rss <- sum(residuals(mod.spec)^2)
tss <- sum((y - mean(y))^2)

r2_full <- 1 - rss / tss

r2_full

## Triple Difference Approach
# center repression so the did coefficient stays interpretable
pdata_noCHN2$cso_c <- pdata_noCHN2$cso_repression -
  mean(pdata_noCHN2$cso_repression, na.rm = TRUE)

mod_ddd <- plm(lnsolarV2 ~ didV2 * cso_c + lngdpV2 + wx_sd + wx_cwd + ifdi2V2 + gcf,
               data   = pdata_noCHN2,
               model  = "within",
               effect = "twoways")
summary(mod_ddd, vcov =vcovHC)
coeftest(mod_ddd, vcov = vcovHC(mod_int, cluster = "group"))

## Triple Difference R2 calculation
y <- model.response(model.frame(mod_ddd))

rss <- sum(residuals(mod_ddd)^2)
tss <- sum((y - mean(y))^2)

r2_full <- 1 - rss / tss

r2_full


#------------------------------------------------------------------------------#
####                    8. Replication PANEL2_SOV                           ####
#------------------------------------------------------------------------------#

#----------------8.1 No China NO MICRO Replication Model-----------------------#

## Baseline formula
f <- lnsolarV2 ~ didV2 + lngdpV2 + gcf +
  ifdi2V2 + wx_cwd + wx_sd + cso_repression +
  lnpv_priceV2 * didV2 + domestic_autonomy * didV2 

## Baseline Models
# (1) no FE
mod1 <- plm(f, data = pdata_sov, model = "pooling") 

# (2) country FE
mod2 <- plm(f, data = pdata_sov, model = "within", effect = "individual")  

# (3) country + year FE
mod3 <- plm(f, data = pdata_sov, model = "within", effect = "twoways")     

## Overview
summary(mod1, vcov = vcovHC)
summary(mod2, vcov = vcovHC)
summary(mod3, vcov = vcovHC)

## R2 calculation
y <- model.response(model.frame(mod3))

rss <- sum(residuals(mod3)^2)
tss <- sum((y - mean(y))^2)

r2_full <- 1 - rss / tss

r2_full

#----------------8.2 No China NO MICRO Replication Model-----------------------#

## 1) Model with WGI Political indicators

# 1a) POLS
mod1a <- plm(lnsolar ~ did + lnlabor + fiscweak + cpia_proprights
             + pdata$wgi_goveffect + pdata$milex_govt + wgi_ruleoflaw + cso_repression, 
             model = "pooling",data = pdata)
summary(mod1a, vcov = vcovHC)
## Gov debt removes 40 countries that do not have a value for this variable 

# 1b) Two-Way Country Fixed Effects
mod1b <- plm(lnsolar ~ did + lnlabor + pdata$fiscweak + pdata$cpia_proprights
             + pdata$govt_debt_gdp + pdata$wgi_goveffect + pdata$milex_govt, 
             model = "within", effect = "twoways",data = pdata)
summary(mod1b, vcov = vcovHC)

# 1.1b) TW-CFE with interaction govt_debt and did 
mod1.1b <- plm(lnsolar ~ (did * govt_debt_gdp) + lnlabor
               + pdata$govt_debt_gdp , 
               model = "within", effect = "individual",data = pdata)
summary(mod1.1b, vcov = vcovHC)
## n = 109 is bad here with wgi controls 

# 1c) Two- Way Fixed Effects
mod1c <- plm(lnsolar ~ did + (did * govt_debt_gdp) + lnlabor 
             + pdata$wgi_goveffect + pdata$milex_govt, 
             model = "within", effect = "twoways",data = pdata)
summary(mod1c, vcov = vcovHC)

## 1c) R2 calculation
y <- model.response(model.frame(mod1c))

rss <- sum(residuals(mod1c)^2)
tss <- sum((y - mean(y))^2)

r2_full <- 1 - rss / tss

r2_full


#------------------------------------------------------------------------------#
####                 9. Alternative Model Estimation                        ####
#------------------------------------------------------------------------------#


#---------------------9.1 Fixed Effects Poison Model---------------------------#

## Motivation for FEPPLM
mean(panel$solar_gwh[panel$year == 2013] == 0, na.rm = TRUE)
## All % of all observations in 2013 have no solar generation
mean(panel$solar_gwh[panel$year == 2007] == 0, na.rm = TRUE)
## 38% of all observations in 2007 have no solar generation

## 1) Baseline TWFE Poisson 
ppml1 <- fepois(solar_gwh ~ didV2 + lngdpV2 + lnlaborV2 
                | iso3c + year,
                data = pdata_noCHN2, cluster = ~iso3c)
summary(ppml1)

## 2) With the paper's full control set
ppml2 <- fepois(solar_gwh ~ did + lngdp + lnlabor + gfcf +
                  ifdi2 + wx_sd + wx_cwd + lnprice_pvfx
                | iso3c + year,
                data = pdata_noCHN, cluster = ~iso3c)
summary(ppml2)


## 3) Additional WGI political indicators
ppml1 <- fepois(solar_gwh ~ didV2  + wgi_polstab
                + wgi_corruption + cso_repression
                | iso3c + year,
                data = pdata_noCHN2, cluster = ~iso3c)
summary(ppml1)

## 4) Additional WGI political indicators
ppml2 <- fepois(solar_gwh ~ didV2 + wx_sd + wx_cwd + cso_repression 
                + wgi_goveffect + resource_expoV2 + domestic_autonomy *didV2
                | iso3c + year,
                data = pdata_sov, cluster = ~iso3c)
summary(ppml2)

## 5) Additional WGI political indicators
ppml2 <- fepois(solar_gwh ~ did + wx_sd + wx_cwd + cso_repression
                + wgi_goveffect + fiscresource + fiscweak
                | iso3c + year,
                data = pdata_noCHN, cluster = ~iso3c)
summary(ppml2)


#------------------------------------------------------------------------------#
####                 10. Callaway Sant'Anna                                 ####
#------------------------------------------------------------------------------#

library(did)
d <- panel
d$lnsolar <- ifelse(d$solar_gwh > 0, log(d$solar_gwh), NA)   # positive-only, as the paper
d$gname   <- ifelse(is.na(d$treat_start), 0, d$treat_start)   # 0 = never treated
d$id      <- as.integer(factor(d$iso3c))

att <- att_gt(yname = "lnsolar", tname = "year", idname = "id", gname = "gname",
              xformla = ~ lngdp + lnlabor + energy_before_bri 
              + wgi_goveffect, data = d, control_group = "notyettreated", 
              panel = TRUE, allow_unbalanced_panel = TRUE, na.rm = TRUE)
aggte(att, type = "group", na.rm = TRUE)     # overall ATT
aggte(att, type = "dynamic", na.rm = TRUE)   # event study (your Fig. 1 analogue)

mean(panel$solar_gwh == 0, na.rm = TRUE)
summary(panel$solar_gwh)
summary(panel$lnsolar)

## Transform to suitable dataset
d <- panel2 %>%
  filter(!is.na(iso3c), year >= 2000) %>%
  distinct(iso3c, year, .keep_all = TRUE)
d$lnsolar <- log(pmax(d$solar_gwh, 0.001))
d$gname   <- ifelse(is.na(d$treat_start), 0, d$treat_start)
d$gname   <- ifelse(d$gname > max(d$year, na.rm = TRUE), 0, d$gname)
d$id      <- as.integer(factor(d$iso3c))
sum(duplicated(d[, c("id", "year")]))    # must be 0

cs <- att_gt(
  yname          = "lnsolarV2",
  tname          = "year",
  idname         = "id",
  gname          = "gname",
  xformla        = ~ lngdpV2 + wx_hurs + wx_sd + gcf +
    ifdi2V2 + cso_repression,
  data           = d,                      # <- was panel
  control_group  = "notyettreated",
  est_method     = "dr",
  clustervars    = "id",
  panel          = TRUE,
  allow_unbalanced_panel = TRUE,
  base_period    = "universal",
  na.rm          = TRUE)

aggte(cs, type = "dynamic", na.rm = TRUE)   # event study
aggte(cs, type = "group",   na.rm = TRUE)   # by cohort
aggte(cs, type = "simple",  na.rm = TRUE)   # single summary
ggdid(aggte(cs, type = "dynamic", na.rm = TRUE))

cs2 <- att_gt(
  yname          = "lnsolarV2",
  tname          = "year",
  idname         = "id",
  gname          = "gname",
  xformla        = ~ lngdpV2 + cso_repression + dem_index +
    cso_repression,
  data           = d,                      # <- was panel
  control_group  = "nevertreated",
  est_method     = "dr",
  clustervars    = "id",
  panel          = TRUE,
  allow_unbalanced_panel = TRUE,
  base_period    = "universal",
  na.rm          = TRUE)


ggdid(aggte(cs2, type = "dynamic", na.rm = TRUE))

## Option 2 with panel_noCHN
d1 <- panel_noCHN %>%
  filter(!is.na(iso3c), year >= 2000) %>%
  distinct(iso3c, year, .keep_all = TRUE)
d1$lnsolar <- log(pmax(d1$solar_gwh, 0.001))
d1$gname   <- ifelse(is.na(d1$treat_start), 0, d1$treat_start)
d1$gname   <- ifelse(d1$gname > max(d1$year, na.rm = TRUE), 0, d1$gname)
d1$id      <- as.integer(factor(d1$iso3c))
sum(duplicated(d[, c("id", "year")]))    # must be 0

cs1 <- att_gt(
  yname          = "lnsolarV2",
  tname          = "year",
  idname         = "id",
  gname          = "gname",
  xformla        = ~ lngdpV2 + lnlaborV2 + wx_hurs + wx_sd + gcf +
    ifdi2V2 + exports_gdp + cso_repression,
  data           = d,                      # <- was panel
  control_group  = "notyettreated",
  est_method     = "dr",
  clustervars    = "id",
  panel          = TRUE,
  allow_unbalanced_panel = TRUE,
  base_period    = "universal",
  na.rm          = TRUE)

aggte(cs1, type = "dynamic", na.rm = TRUE)   # event study
aggte(cs1, type = "group",   na.rm = TRUE)   # by cohort
aggte(cs1, type = "simple",  na.rm = TRUE)   # single summary
ggdid(aggte(cs, type = "dynamic", na.rm = TRUE))


#------------------------------------------------------------------------------#
####.                            11. Robustness                             ####
#------------------------------------------------------------------------------#

## TEST 1 — Does adding the control change did? (Gelbach-style, same sample)
d_common <- panel2 %>% filter(!is.na(cso_repression), !is.na(gcf), !is.na(exports_gdp))
p <- pdata.frame(d_common, index = c("iso3c","year"))
m_base <- plm(lnsolarV2 ~ didV2 + lngdpV2 + lnlaborV2, p, model="within", effect="twoways")
m_full <- plm(lnsolarV2 ~ didV2 + lngdpV2 + lnlaborV2 + cso_repression + gcf + exports_gdp,
              p, model="within", effect="twoways")
c(base = coef(m_base)["didV2"], full = coef(m_full)["didV2"])
## Control barely changes - the bad effect variable is negligible 

## TEST 2 - Characteristics of the cso_repression states 
s <- panel[!duplicated(panel$iso3c), c("iso3c","treat_start","cso_repression")]
tapply(s$cso_repression, is.na(s$treat_start), summary)

## TEST 3 - Parallel Trends - GDP is the same? 
s <- panel2 %>%
  filter(!is.na(iso3c)) %>%
  group_by(iso3c) %>%
  summarise(treated  = any(!is.na(treat_start)),
            gdp_base = mean(gdp_pc_cur[year <= 2012], na.rm = TRUE),
            .groups  = "drop") %>%
  mutate(lngdp_base = log(gdp_base))

tapply(s$lngdp_base, s$treated, summary)
table(s$treated)
t.test(lngdp_base ~ treated, data = s)

s2 <- s %>% filter(lngdp_base >= 7, lngdp_base <= 10)
table(s2$treated)

d_trim <- panel2 %>% filter(iso3c %in% s2$iso3c)
## NO, LARGE DIFFERENCES IN GDP PRE-TREATMENT 

d_trim <- panel %>%
  filter(iso3c %in% s2$iso3c, !is.na(iso3c), iso3c != "CHN", year >= 2009) %>%
  distinct(iso3c, year, .keep_all = TRUE) %>%
  mutate(lnsolar = log(pmax(solar_gwh, 0.001)),
         g       = ifelse(is.na(treat_start), 0L, as.integer(treat_start)),
         g       = ifelse(g > max(year, na.rm = TRUE), 0L, g),
         id      = as.integer(factor(iso3c)))

cs_trim <- att_gt(yname = "lnsolar", tname = "year", idname = "id", gname = "g",
                  xformla = ~ lngdp + lnlabor + wx_sd + wx_hurs,
                  data = d_trim, control_group = "nevertreated",
                  est_method = "dr", clustervars = "id",
                  panel = TRUE, allow_unbalanced_panel = TRUE,
                  base_period = "universal", na.rm = TRUE)

aggte(cs_trim, type = "simple",  na.rm = TRUE)
aggte(cs_trim, type = "dynamic", na.rm = TRUE)

tapply(s2$lngdp_base, s2$treated, summary)
t.test(lngdp_base ~ treated, data = s2)

aggte(cs_trim, type = "dynamic", na.rm = TRUE, min_e = -6, max_e = 5)



#---------------------11.1 PPML with Sun-Abraham event study-------------------#

## Plain data frame for PPML 
d_pp <- panel2 %>%
  filter(!is.na(iso3c), iso3c != "CHN", year >= 2005) %>%
  distinct(iso3c, year, .keep_all = TRUE) %>%
  mutate(g = ifelse(is.na(treat_start), 0L, as.integer(treat_start)),
         g = ifelse(g > max(year, na.rm = TRUE), 0L, g))

class(d_pp$year); sum(duplicated(d_pp[, c("iso3c","year")]))
d_pp %>% distinct(iso3c, g) %>% count(g) # cohort sizes; g == 0 = never treated

## (1) Baseline PPML with limited variables
ppml_base <- fepois(solar_gwh ~ didV2 + lngdpV2 | iso3c + year,
                    data = d_pp, cluster = ~iso3c)
summary(ppml_base)

## (2) Sun-Abraham event study -- MAIN SPECIFICATION
ppml_sa <- fepois(solar_gwh ~ sunab(g, year) + lngdpV2 + cso_repression + dem_index | iso3c + year,
                  data = d_pp, cluster = ~iso3c)

summary(ppml_sa, agg = "att")       # single aggregated treatment effect
iplot(ppml_sa)                      # event-study plot
summary(ppml_sa)                    # full event-time coefficients


#---------------------------11.2 Mundlak Estimator-----------------------------#

d_m <- panel %>%
  group_by(iso3c) %>%
  mutate(m_did = mean(did, na.rm = TRUE),
         m_gdp = mean(lngdp, na.rm = TRUE),
         m_lab = mean(lnlabor, na.rm = TRUE),
         m_cso = mean(cso_repression, na.rm = TRUE)) %>%
  ungroup()

mund2 <- plm(lnsolar ~ did + lngdp + lnlabor + cso_repression +
               m_did + m_gdp + m_lab + m_cso + factor(year),
             data = pdata.frame(d_m, index = c("iso3c", "year")),
             model = "pooling")

summary(mund2, vcov = vcovHC)

## Mundlak check
between <- plm(lnsolarV2 ~ didV2 + lngdpV2 + cso_repression * didV2,
               data = pdata_noCHN2, model = "between")
summary(between)

within  <- plm(lnsolarV2 ~ didV2 + lngdpV2 + + cso_repression * didV2,
               data = pdata_noCHN2, model = "within", effect = "twoways")
summary(within, vcov = vcovHC)

c(within  = coef(within)["cso_repression"],
  between = coef(between)["cso_repression"])



#-------------------------11.3 GMM Method Estimator----------------------------#

gmm <- pgmm(lnsolar ~  did * cso_repression + lngdp + lnpv_price * wx_sd + rule_of_law |
              lag(lnsolar, 4:5),
            data = pdata_noCHN, effect = "individual",
            model = "onestep", collapse = TRUE, transformation = "d")
summary(gmm, robust = TRUE)

ncol(gmm$W[[1]])            # instruments per unit
length(gmm$W)               # units used
c(instruments = ncol(gmm$W[[1]]), countries = length(gmm$W))

mtest(gmm, order = 1)     # should reject — differencing induces AR(1)
mtest(gmm, order = 2)     # should NOT reject — if it does, instruments invalid
sargan(gmm)               # should NOT reject

mod22 <- plm(lnsolarV2 ~ lag(lnsolarV2, 3) + cso_c * didV2, data = pdata_noCHN2, 
             model = "within",
             effect = "twoways")
summary(mod22, vcov =vcovHC)

vif(mod22)

#------------------------------------------------------------------------------#
####.                    12. FE Poisson Estimation                          ####
#------------------------------------------------------------------------------#

d <- panel2 %>%
  filter(!is.na(iso3c), iso3c != "CHN") %>%
  distinct(iso3c, year, .keep_all = TRUE) %>%
  mutate(cso_c = cso_repression - mean(cso_repression, na.rm = TRUE),
         dem_index_c = dem_index - mean(dem_index, na.rm = TRUE),
         milex_govt_c = milex_govt - mean(milex_govt, na.rm = TRUE))

# ============================================================
# PPML interaction: BRI × repression, country + year FE
# ============================================================
ppml_int <- fepois(
  solar_gwh ~ didV2 * cso_repression + lngdpV2 + wx_sd + wx_cwd + ifdi2V2 + gcf + milex_govt
  | iso3c + year,
  data    = d,
  cluster = ~iso3c)
summary(ppml_int)

summary(d$cso_repression)

pdata_noCHN2$low_cso <- ifelse(d$cso_repression >= 1.1850, 1, 0 )

pdata_noCHN2$high_cso <- ifelse(d$cso_repression <= 1.1850, 1, 0 )



twfe1 <- plm(lnsolarV2 ~ did * high_cso + lngdpV2 + gcf + wx_sd + ifdi2V2, data = pdata_noCHN2,
             model = "within", effect = "twoways" )


ppml_int <- fepois(
  solar_gwh ~ didV2 * high_cso + lngdpV2 + wx_sd + wx_cwd + ifdi2V2 + gcf
  | iso3c + year,
  data    = d,
  cluster = ~iso3c)
summary(ppml_int)





d %>% distinct(iso3c, treat = !is.na(treat_start), cso_repression) %>%
  group_by(treat) %>%
  summarise(mean = mean(cso_repression, na.rm=TRUE),
            p10  = quantile(cso_repression, .1, na.rm=TRUE),
            p90  = quantile(cso_repression, .9, na.rm=TRUE), n = n())

ppml_int2 <- fepois(
  solar_gwh ~ didV2 * milex_govt_c + lngdpV2 + wx_sd + wx_cwd + ifdi2V2 + gcf + cso_repression
  | iso3c + year,
  data    = d,
  cluster = ~iso3c)
summary(ppml_int2)

summary(pdata_noCHN$iso3c == "AFG", pdata_noCHN$join_year)

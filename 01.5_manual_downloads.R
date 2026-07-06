library(tidyverse)

patient_sat25 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/patient_satisfaction/GPPS_2025_Practice_data_(weighted)_(csv)_PUBLIC.csv")%>%
  dplyr::select(ad_practicecode, ad_practicename, gpcontactoverall.pcteval, overallexp.pcteval)%>%
  dplyr::mutate(year=2025)%>%
  dplyr::rename(Practice_Code = ad_practicecode,
                Practice_Name = ad_practicename,
                appointment_good = gpcontactoverall.pcteval,
                overall_good = overallexp.pcteval)%>%
  dplyr::mutate(overall_good = ifelse(overall_good==-97,NA, overall_good),
                appointment_good = ifelse(appointment_good==-97,NA, appointment_good),
                overall_good = overall_good*100,
                appointment_good = appointment_good*100)

patient_sat24 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/patient_satisfaction/GPPS_2024_Practice_data_(weighted)_(csv)_PUBLIC.csv")%>%
  dplyr::select(ad_practicecode, ad_practicename, gpcontactoverall.pcteval, overallexp.pcteval)%>%
  dplyr::mutate(year=2024)%>%
  dplyr::rename(Practice_Code = ad_practicecode,
                Practice_Name = ad_practicename,
                appointment_good = gpcontactoverall.pcteval,
                overall_good = overallexp.pcteval)%>%
  dplyr::mutate(overall_good = ifelse(overall_good==-97,NA, overall_good),
                appointment_good = ifelse(appointment_good==-97,NA, appointment_good),
                overall_good = overall_good*100,
                appointment_good = appointment_good*100)
  




patient_sat23 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/patient_satisfaction/GPPS_2023_Practice_data_(weighted)_(csv)_PUBLIC.csv")%>%
  dplyr::select(Practice_Code, Practice_Name, Q28_12pct, Q18_12pct)%>%
  dplyr::mutate(year=2023)%>%
  dplyr::rename(overall_good = Q28_12pct,
                appointment_good = Q18_12pct)%>%
  dplyr::mutate(overall_good = ifelse(overall_good==-97,NA, overall_good),
                appointment_good = ifelse(appointment_good==-97,NA, appointment_good),
                overall_good = overall_good*100,
                appointment_good = appointment_good*100)


patient_sat22 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/patient_satisfaction/GPPS_2022_Practice_data_(weighted)_(csv)_PUBLIC.csv")%>%
  dplyr::select(Practice_Code, Practice_Name, Q28_12pct, Q18_12pct)%>%
  dplyr::mutate(year=2022)%>%
  dplyr::rename(overall_good = Q28_12pct,
                appointment_good = Q18_12pct)%>%
  dplyr::mutate(overall_good = ifelse(overall_good==-97,NA, overall_good),
                appointment_good = ifelse(appointment_good==-97,NA, appointment_good),
                overall_good = overall_good*100,
                appointment_good = appointment_good*100)



patient_sat21 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/patient_satisfaction/GPPS_2021_Practice_data_(weighted)_(csv)_PUBLIC.csv")%>%
  dplyr::select(Practice_Code, Practice_Name, Q28_12pct, Q18_12pct)%>%
  dplyr::mutate(year=2021)%>%
  dplyr::rename(overall_good = Q28_12pct,
                appointment_good = Q18_12pct)%>%
  dplyr::mutate(overall_good = ifelse(overall_good==-97,NA, overall_good),
                appointment_good = ifelse(appointment_good==-97,NA, appointment_good),
                overall_good = overall_good*100,
                appointment_good = appointment_good*100)


patient_sat20 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/patient_satisfaction/GPPS_2020_Practice_data_(weighted)_(csv)_PUBLIC.csv")%>%
  dplyr::select(Practice_Code, Practice_Name, Q28_12pct, Q18_12pct)%>%
  dplyr::mutate(year=2020)%>%
  dplyr::rename(overall_good = Q28_12pct,
                appointment_good = Q18_12pct)%>%
  dplyr::mutate(overall_good = ifelse(overall_good==-97,NA, overall_good),
                appointment_good = ifelse(appointment_good==-97,NA, appointment_good),
                overall_good = overall_good*100,
                appointment_good = appointment_good*100)

patient_sat19 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/patient_satisfaction/GPPS_2019_Practice_data_(weighted)_(csv)_PUBLIC.csv")%>%
  dplyr::select(Practice_Code, Practice_Name, Q28_12pct, Q18_12pct)%>%
  dplyr::mutate(year=2019)%>%
  dplyr::rename(overall_good = Q28_12pct,
                appointment_good = Q18_12pct)%>%
  dplyr::mutate(overall_good = ifelse(overall_good==-97,NA, overall_good),
                appointment_good = ifelse(appointment_good==-97,NA, appointment_good),
                overall_good = overall_good*100,
                appointment_good = appointment_good*100)

patient_sat18 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/patient_satisfaction/GPPS 2018 Practice data (weighted) (csv) PUBLIC.csv")%>%
  dplyr::select(Practice_Code, Practice_Name, Q28_12pct, Q18_12pct)%>%
  dplyr::mutate(year=2018)%>%
  dplyr::rename(overall_good = Q28_12pct,
                appointment_good = Q18_12pct)%>%
  dplyr::mutate(overall_good = ifelse(overall_good==-97,NA, overall_good),
                appointment_good = ifelse(appointment_good==-97,NA, appointment_good),
                overall_good = overall_good*100,
                appointment_good = appointment_good*100)

patient_sat17 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/patient_satisfaction/GPPS 2017 Practice data (weighted) (csv) PUBLIC.csv")%>%
  dplyr::select(Practice_code, Practice_Name, Q28_12pct, Q18_12pct)%>%
  dplyr::mutate(year=2017)%>%
  dplyr::rename(Practice_Code =Practice_code,
                overall_good = Q28_12pct,
                appointment_good = Q18_12pct)%>%
  dplyr::mutate(overall_good = ifelse(overall_good==-97,NA, overall_good),
                appointment_good = ifelse(appointment_good==-97,NA, appointment_good),
                overall_good = overall_good*100,
                appointment_good = appointment_good*100)




patient_sat16 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/patient_satisfaction/July 2016 Practice level data weighted CSV.csv")%>%
  dplyr::select(Practice_code, Practice_Name, Q28_12pct, Q18_12pct)%>%
  dplyr::mutate(year=2016)%>%
  dplyr::rename(Practice_Code =Practice_code,
                overall_good = Q28_12pct,
                appointment_good = Q18_12pct)%>%
  dplyr::mutate(overall_good = ifelse(overall_good==-97,NA, overall_good),
                appointment_good = ifelse(appointment_good==-97,NA, appointment_good),
                overall_good = overall_good*100,
                appointment_good = appointment_good*100)


patient_sat15 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/patient_satisfaction/July 2015 Practice level data weighted.csv")%>%
  dplyr::select(Practice_code, Practice_Name, Q28_12pct, Q18_12pct)%>%
  dplyr::mutate(year=2015)%>%
  dplyr::rename(Practice_Code =Practice_code,
                overall_good = Q28_12pct,
                appointment_good = Q18_12pct)%>%
  dplyr::mutate(overall_good = ifelse(overall_good==-97,NA, overall_good),
                appointment_good = ifelse(appointment_good==-97,NA, appointment_good),
                overall_good = overall_good*100,
                appointment_good = appointment_good*100)


patient_sat14 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/patient_satisfaction/July 2014 Practice level data weighted.csv")%>%
  dplyr::select(Practice_code, Practice_Name, Q28_12pct, Q18_12pct)%>%
  dplyr::mutate(year=2014)%>%
  dplyr::rename(Practice_Code =Practice_code,
                overall_good = Q28_12pct,
                appointment_good = Q18_12pct)%>%
  dplyr::mutate(overall_good = ifelse(overall_good==-97,NA, overall_good),
                appointment_good = ifelse(appointment_good==-97,NA, appointment_good),
                overall_good = overall_good*100,
                appointment_good = appointment_good*100)



patient_sat13 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/patient_satisfaction/June 2013 Practice level data weighted.csv")%>%
  dplyr::select(Practice_Code, Practice_Name, Q28_12pct, Q18_12pct)%>%
  dplyr::mutate(year=2013)%>%
  dplyr::rename(overall_good = Q28_12pct,
                appointment_good = Q18_12pct)%>%
  dplyr::mutate(overall_good = ifelse(overall_good==-97,NA, overall_good),
                appointment_good = ifelse(appointment_good==-97,NA, appointment_good),
                overall_good = overall_good*100,
                appointment_good = appointment_good*100)



patient_sat12 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/patient_satisfaction/June 2012 Practice level data weighted.csv")%>%
  dplyr::select(Practice_Code, Practice_Name, Q28_12pct, Q18_12pct)%>%
  dplyr::mutate(year=2012)%>%
  dplyr::rename(overall_good = Q28_12pct,
                appointment_good = Q18_12pct)%>%
  dplyr::mutate(overall_good = ifelse(overall_good==-97,NA, overall_good),
                appointment_good = ifelse(appointment_good==-97,NA, appointment_good),
                overall_good = overall_good*100,
                appointment_good = appointment_good*100)


patient_sat11 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/patient_satisfaction/2011_overall_csv.csv", skip = 7)%>%
  dplyr::select(Practice.code, Practice.name, X..Satisfied..total.)%>%
  dplyr::mutate(year=2011,
                appointment_good = NA)%>%
  dplyr::rename(overall_good = X..Satisfied..total.,
                Practice_Code = Practice.code,
                Practice_Name = Practice.name)
  

patient_sat10 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/patient_satisfaction/2010_overall_csv.csv", skip = 7)%>%
  dplyr::select(Practice.code, Practice.name, X..Satisfied..total.)%>%
  dplyr::mutate(year=2010,
                appointment_good = NA)%>%
  dplyr::rename(overall_good = X..Satisfied..total.,
                Practice_Code = Practice.code,
                Practice_Name = Practice.name)




patient_sat <- rbind(
  patient_sat25, 
  patient_sat24, 
  patient_sat23, 
  patient_sat22, 
  patient_sat21, 
  patient_sat20, 
  patient_sat19, 
  patient_sat18, 
  patient_sat17, 
  patient_sat16, 
  patient_sat15, 
  patient_sat14, 
  patient_sat13, 
  patient_sat12, 
  patient_sat11, 
  patient_sat10 
  
)

patient_sat$overall_good <- parse_number(patient_sat$overall_good)

write.csv(patient_sat, "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/patient_satisfaction.csv")





####workforce####




gp1 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/worforce_manual/1. General Practice – September 2015 Practice Level.csv")%>%
  dplyr::select(PRAC_CODE, PRAC_NAME, TOTAL_PATIENTS, TOTAL_GP_FTE, TOTAL_GP_EXL_FTE, TOTAL_NURSES_FTE, TOTAL_DPC_FTE, TOTAL_ADMIN_FTE)%>%
  dplyr::mutate(year=2015)


gp2 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/worforce_manual/2. General Practice – September 2016 Practice Level.csv")%>%
  dplyr::select(PRAC_CODE, PRAC_NAME, TOTAL_PATIENTS, TOTAL_GP_FTE, TOTAL_GP_EXL_FTE, TOTAL_NURSES_FTE, TOTAL_DPC_FTE, TOTAL_ADMIN_FTE)%>%
  dplyr::mutate(year=2016)


gp3 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/worforce_manual/3. General Practice – September 2017 Practice Level.csv")%>%
  dplyr::select(PRAC_CODE, PRAC_NAME, TOTAL_PATIENTS, TOTAL_GP_FTE, TOTAL_GP_EXL_FTE, TOTAL_NURSES_FTE, TOTAL_DPC_FTE, TOTAL_ADMIN_FTE)%>%
  dplyr::mutate(year=2017)


gp4 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/worforce_manual/4. General Practice – September 2018 Practice Level.csv")%>%
  dplyr::select(PRAC_CODE, PRAC_NAME, TOTAL_PATIENTS, TOTAL_GP_FTE, TOTAL_GP_EXL_FTE, TOTAL_NURSES_FTE, TOTAL_DPC_FTE, TOTAL_ADMIN_FTE)%>%
  dplyr::mutate(year=2018)


gp5 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/worforce_manual/5. General Practice – September 2019 Practice Level.csv")%>%
  dplyr::select(PRAC_CODE, PRAC_NAME, TOTAL_PATIENTS, TOTAL_GP_FTE, TOTAL_GP_EXL_FTE, TOTAL_NURSES_FTE, TOTAL_DPC_FTE, TOTAL_ADMIN_FTE)%>%
  dplyr::mutate(year=2019)


gp6 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/worforce_manual/6. General Practice – September 2020 Practice Level.csv")%>%
  dplyr::select(PRAC_CODE, PRAC_NAME, TOTAL_PATIENTS, TOTAL_GP_FTE, TOTAL_GP_EXL_FTE, TOTAL_NURSES_FTE, TOTAL_DPC_FTE, TOTAL_ADMIN_FTE)%>%
  dplyr::mutate(year=2020)


gp7 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/worforce_manual/7. General Practice – September 2021 Practice Level.csv")%>%
  dplyr::select(PRAC_CODE, PRAC_NAME, TOTAL_PATIENTS, TOTAL_GP_FTE, TOTAL_GP_EXL_FTE, TOTAL_NURSES_FTE, TOTAL_DPC_FTE, TOTAL_ADMIN_FTE)%>%
  dplyr::mutate(year=2021)



gp8 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/worforce_manual/37. General Practice – September 2022 Practice Level.csv")%>%
  dplyr::select(PRAC_CODE, PRAC_NAME, TOTAL_PATIENTS, TOTAL_GP_FTE, TOTAL_GP_EXL_FTE, TOTAL_NURSES_FTE, TOTAL_DPC_FTE, TOTAL_ADMIN_FTE)%>%
  dplyr::mutate(year=2022)


gp9 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/worforce_manual/1 General Practice – September 2023 Practice Level - Detailed.csv")%>%
  dplyr::select(PRAC_CODE, PRAC_NAME, TOTAL_PATIENTS, TOTAL_GP_FTE, TOTAL_GP_EXL_FTE, TOTAL_NURSES_FTE, TOTAL_DPC_FTE, TOTAL_ADMIN_FTE)%>%
  dplyr::mutate(year=2023)


gp10 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/worforce_manual/1 General Practice – September 2024 Practice Level - Detailed.csv")%>%
  dplyr::select(PRAC_CODE, PRAC_NAME, TOTAL_PATIENTS, TOTAL_GP_FTE, TOTAL_GP_EXL_FTE, TOTAL_NURSES_FTE, TOTAL_DPC_FTE, TOTAL_ADMIN_FTE)%>%
  dplyr::mutate(year=2024)


gp11 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/worforce_manual/1 General Practice – September 2025 Practice Level - Detailed.csv")%>%
  dplyr::select(PRAC_CODE, PRAC_NAME, TOTAL_PATIENTS, TOTAL_GP_FTE, TOTAL_GP_EXL_FTE, TOTAL_NURSES_FTE, TOTAL_DPC_FTE, TOTAL_ADMIN_FTE)%>%
  dplyr::mutate(year=2025)


workforce <- rbind(gp1,
                   gp2,
                   gp3,
                   gp4,
                   gp5,
                   gp6,
                   gp7,
                   gp8,
                   gp9,
                   gp10,
                   gp11)


write.csv(workforce, "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/workforce_annual.csv")











rm(list = ls())

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(stringr)
  library(readr)
  library(fingertipsR)
})
library(fingertipsR)


gp_indicators <- fingertipsR::indicators() %>%
  inner_join(
    fingertipsR::indicator_areatypes(AreaTypeID = 7L),
    by = "IndicatorID"
  )

selected_indicators <- gp_indicators %>%
  filter(str_detect(IndicatorName, "% QOF points achieved")) %>%
  dplyr::distinct(IndicatorID, .keep_all = T)



qof_score <- fingertipsR::fingertips_data(
  IndicatorID = selected_indicators$IndicatorID,
  AreaTypeID  = 7L
) %>%
  as_tibble()



qof_score <- qof_score %>%
  dplyr::filter(AreaType=="GPs")



qof_score <- qof_score %>%
  transmute(
    PRAC_CODE      = as.character(AreaCode),
    PRACTICE_NAME  = as.character(AreaName),
    INDICATOR_ID   = as.integer(IndicatorID),
    INDICATOR_NAME = as.character(IndicatorName),
    VALUE          = suppressWarnings(as.numeric(Value)),
    year = TimeperiodSortable/10000+1
  )


write.csv(qof_score, "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/qof_annual.csv")




####companies house####


cqc <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/data/complete inspection and location data noncare homes_ben_feb2025v2.csv")
prov  <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/data/providers_info_batch_all.csv")


numbers <- cqc %>%
  filter(locationtypesector == "Primary Medical Services") %>%
  dplyr::select(providercompanieshousenumber)%>%
  dplyr::distinct(.keep_all = T)%>%
  dplyr::rename(CompanyNumber=providercompanieshousenumber )

numbers <- numbers %>%
  mutate(
    CompanyNumber = ifelse(
      nchar(CompanyNumber) == 7 ,
      paste0("0", CompanyNumber),
      CompanyNumber
    )
  )


write.csv(numbers, "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/gp_companies.csv")


####controls####

gp_indicators <- fingertipsR::indicators() %>%
  inner_join(
    fingertipsR::indicator_areatypes(AreaTypeID = 7L),
    by = "IndicatorID"
  )




selected_indicators <- gp_indicators %>%
  filter(IndicatorName== "Life expectancy - MSOA based"|
           IndicatorName== "Deprivation score (IMD 2025)"|
           IndicatorName=="% active smokers (GPPS)"|
           IndicatorName=="% former smokers (GPPS)"|
           str_detect(IndicatorName, "QOF prevalence"  )) %>%
  dplyr::distinct(IndicatorID, .keep_all = T)



controls <- fingertipsR::fingertips_data(
  IndicatorID = selected_indicators$IndicatorID,
  AreaTypeID  = 7L
) %>%
  as_tibble()


controls <- controls %>%
  dplyr::filter(Age == "All ages",
                AreaType=="GPs")



qof_prev <- controls %>%
  dplyr::filter( str_detect(IndicatorName, "QOF prevalence" ))

qof_index <- qof_prev %>%
  mutate(
    year = TimeperiodSortable/10000+1,
    gp_code = AreaCode,
    gp_name = AreaName
  ) %>%
  group_by(year, IndicatorID) %>%
  mutate(
    z_value = as.numeric(scale(Value))
  ) %>%
  ungroup() %>%
  group_by(gp_code, year) %>%
  summarise(
    qof_composite = mean(z_value, na.rm = TRUE),
    n_indicators  = sum(!is.na(z_value)),
    .groups = "drop"
  )%>%
  dplyr::ungroup()

write.csv(qof_index, "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/qof_annual_prev.csv")




life_ex <- controls %>%
  dplyr::filter( str_detect(IndicatorName, "Life expectancy" ))




life_ex <- life_ex %>%
  mutate(
    year = TimeperiodSortable/10000+2,
    gp_code = AreaCode  ) %>%
  group_by(gp_code, year) %>%
  summarise(
    life_expectancy = mean(Value, na.rm = TRUE)  )%>%
  dplyr::ungroup()%>%
  dplyr::filter(year>2012)


library(tidyverse)

# Full year range
all_years <- 2013:2025

life_ex <- life_ex %>%
  group_by(gp_code) %>%
  complete(year = 2013:2025) %>%
  mutate(
    life_expectancy = case_when(
      year <= 2016 ~ life_expectancy[year == 2015][1],
      year == 2017 ~ life_expectancy[year == 2017][1],
      year <= 2019 ~ life_expectancy[year == 2018][1],
      TRUE         ~ life_expectancy[year == 2021][1]
    )
  ) %>%
  ungroup()



write.csv(life_ex, "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/life_expectancy.csv")


imd <- controls %>%
  dplyr::filter( str_detect(IndicatorName, "IMD" ))%>%
  dplyr::select(AreaCode, Value)%>%
  dplyr::rename(imd25 = Value,
                gp_code = AreaCode)


write.csv(imd, "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/imd.csv")




####takeovers chain####




# ── Libraries ─────────────────────────────────────────────────────────────────
library(tidyverse)
library(lubridate)
library(rvest)
library(httr)
library(janitor)

# ── Master output directory ────────────────────────────────────────────────────
DATA_DIR <- "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data"
DATA_DIR <- path.expand(DATA_DIR)




# ── CQC source files (unchanged from original scripts) ────────────────────────
LOCATION_FILE <- "~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/data/complete inspection and location data noncare homes_ben_feb2025v2.csv"
PROVIDER_FILE  <- "~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/data/providers_info_batch_all.csv"



loc <- read_csv(LOCATION_FILE, show_col_types = FALSE) %>%
  filter(locationtypesector == "Primary Medical Services") %>%
  mutate(
    stata_epoch         = as.Date("1960-01-01"),
    location_start_dt   = stata_epoch + days(as.integer(location_start_2025)),
    location_end_dt   = stata_epoch + days(as.integer(location_end_2025)),
    registrationdate_dt = as.Date(registrationdate)
  )

prov <- read_csv(PROVIDER_FILE, show_col_types = FALSE)

loc1 <- loc %>%
  left_join(prov %>% select(providerId, ownershipType),
            by = c("providerid" = "providerId")) %>%
  mutate(
    ownership_category = case_when(
      ownershipType %in% c("Individual", "Partnership", "NHS Body") ~ "Independent",
      ownershipType == "Organisation"                                ~ "Chain",
      TRUE                                                            ~ NA_character_
    )
  ) %>%
  distinct(locationid, next_group_id, providerid, ownership_category,
           location_start_dt, registrationdate_dt, locationodscode, location_end_dt, .keep_all = TRUE)



chdata <-  read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/gp_company_data_2.csv")



loc1 <- loc1 %>%
  mutate(
    providercompanieshousenumber = ifelse(
      nchar(providercompanieshousenumber) == 7 ,
      paste0("0", providercompanieshousenumber),
      providercompanieshousenumber
    )
  )%>%
  dplyr::left_join(
    chdata,
    by = c("providercompanieshousenumber" = "company_number")
  )%>%
  mutate(
    refined_ownership = case_when(
      ownership_category == "Independent" ~ "Independent",
      providercharitynumber != "" ~ "Charity",
      is_community_interest_company == "True" ~ "CIC",
      company_type %in% c(
        "private-limited-guarant-nsc",
        "registered-society-non-jurisdictional",
        "private-limited-guarant-nsc-limited-exemption"
      ) ~ "Other non-profit",
      company_type %in% c("ltd", "llp") ~ "For-profit company",
      TRUE ~ NA_character_
    )
  )





wut <- loc1 %>% dplyr::select(organisationid, locationid, ownership_category, refined_ownership, ownershipType,
                              ownership, providerid, providercompanieshousenumber, locationhscastartdate, locationhscaenddate,
                              providername, locationodscode, next_group_id, locationname, locationpostalcode, providercharitynumber,
                              rel1_relatedlocationid, rel2_relatedlocationid, is_community_interest_company, company_name, company_type)


library(dplyr)

problem_cases <- wut %>%
  group_by(locationpostalcode, locationname) %>%
  filter(n_distinct(next_group_id, na.rm = TRUE) > 1) %>%
  ungroup()

# Practice → ownership lookup (for ALL practices)
practice_ownership <- loc1 %>%
  distinct(locationodscode, ownership_category, refined_ownership, location_start_dt, location_end_dt) %>%
  rename(PRACTICE_CODE = locationodscode) %>%
  filter(!is.na(PRACTICE_CODE), !is.na(ownership_category))

library(dplyr)
library(tidyr)
library(lubridate)

annual_ownership <- practice_ownership %>%
  mutate(
    spell_start = location_start_dt,
    spell_end   = coalesce(location_end_dt, Sys.Date()),
    start_year  = year(spell_start),
    end_year    = year(spell_end)
  ) %>%
  rowwise() %>%
  mutate(year = list(seq(start_year, end_year))) %>%
  unnest(year) %>%
  mutate(
    year_start   = as.Date(paste0(year, "-01-01")),
    year_end     = as.Date(paste0(year, "-12-31")),
    overlap_days  = pmax(
      0L,
      as.integer(pmin(spell_end, year_end) - pmax(spell_start, year_start) + 1L)
    )
  ) %>%
  ungroup() %>%
  filter(overlap_days > 0) %>%
  group_by(PRACTICE_CODE, year, ownership_category, refined_ownership) %>%
  summarise(overlap_days = sum(overlap_days), .groups = "drop") %>%
  group_by(PRACTICE_CODE, year) %>%
  arrange(desc(overlap_days), ownership_category) %>%   # tie-breaker
  slice(1) %>%
  ungroup() %>%
  group_by(PRACTICE_CODE) %>%
  complete(year = seq(min(year), max(year), by = 1)) %>%
  ungroup()



write_csv(practice_ownership,
          file.path(DATA_DIR, "practice_ownership_map.csv"))

write_csv(annual_ownership,
          file.path(DATA_DIR, "practice_ownership_annual.csv"))










# ── Takeover event identification ─────────────────────────────────────────────
group_summary <- loc1 %>%
  group_by(next_group_id) %>%
  summarise(
    n_providers   = n_distinct(providerid, na.rm = TRUE),
    ownership_set = list(unique(na.omit(ownership_category))),
    .groups = "drop"
  )

takeover_groups <- group_summary %>%
  filter(
    n_providers > 1,
    map_lgl(ownership_set, ~ all(c("Chain", "Independent") %in% .x)))


check <- loc1 %>%
  dplyr::filter(next_group_id %in% takeover_groups$next_group_id)%>%
  dplyr::select(locationid, location_start_dt, locationhscaenddate, next_group_id, ownership_category)%>%
  arrange(next_group_id)


library(dplyr)
library(lubridate)

takeover_groups <- check %>%
  mutate(
    location_start_dt = as.Date(location_start_dt),
    locationhscaenddate = dmy(locationhscaenddate)
  ) %>%
  group_by(next_group_id) %>%
  summarise(
    ind_end = if (any(ownership_category == "Independent" & !is.na(locationhscaenddate))) {
      max(locationhscaenddate[ownership_category == "Independent"], na.rm = TRUE)
    } else {
      as.Date(NA)
    },
    chain_start = if (any(ownership_category == "Chain" & !is.na(location_start_dt))) {
      min(location_start_dt[ownership_category == "Chain"], na.rm = TRUE)
    } else {
      as.Date(NA)
    },
    .groups = "drop"
  ) %>%
  filter(
    !is.na(ind_end),
    chain_start >= ind_end - days(365),
    chain_start <= ind_end + days(365)
  ) 




loc_ods_map <- loc1 %>% distinct(locationid, locationodscode)

takeover_events <- loc1 %>%
  filter(next_group_id %in% takeover_groups$next_group_id) %>%
  arrange(next_group_id, location_start_dt) %>%
  group_by(next_group_id) %>%
  summarise(
    pre_location  = last(locationid[ownership_category == "Independent" &
                                      !is.na(locationid)]),
    post_location = first(locationid[ownership_category == "Chain" &
                                       !is.na(locationid)]),
    post_provider = first(providerid[ownership_category == "Chain" &
                                       !is.na(providerid)]),
    takeover_date = coalesce(
      first(location_start_dt[ownership_category == "Chain"]),
      first(registrationdate_dt[ownership_category == "Chain"])
    ),
    n_sites = n(),
    .groups = "drop"
  ) %>%
  filter(!is.na(takeover_date)) %>%
  mutate(takeover_year = year(takeover_date)) %>%
  left_join(loc_ods_map, by = c("pre_location" = "locationid")) %>%
  rename(pre_practice_code = locationodscode) %>%
  filter(!is.na(pre_practice_code))

write_csv(takeover_events, file.path(DATA_DIR, "takeover_events_all_chain.csv"))






####takeovers for-profit#####

# ── Libraries ─────────────────────────────────────────────────────────────────
library(tidyverse)
library(lubridate)
library(rvest)
library(httr)
library(janitor)

# ── Master output directory ────────────────────────────────────────────────────
DATA_DIR <- "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data"
DATA_DIR <- path.expand(DATA_DIR)




# ── CQC source files (unchanged from original scripts) ────────────────────────
LOCATION_FILE <- "~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/data/complete inspection and location data noncare homes_ben_feb2025v2.csv"
PROVIDER_FILE  <- "~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/data/providers_info_batch_all.csv"



loc <- read_csv(LOCATION_FILE, show_col_types = FALSE) %>%
  filter(locationtypesector == "Primary Medical Services") %>%
  mutate(
    stata_epoch         = as.Date("1960-01-01"),
    location_start_dt   = stata_epoch + days(as.integer(location_start_2025)),
    location_end_dt   = stata_epoch + days(as.integer(location_end_2025)),
    registrationdate_dt = as.Date(registrationdate)
  )

prov <- read_csv(PROVIDER_FILE, show_col_types = FALSE)

loc1 <- loc %>%
  left_join(prov %>% select(providerId, ownershipType),
            by = c("providerid" = "providerId")) %>%
  mutate(
    ownership_category = case_when(
      ownershipType %in% c("Individual", "Partnership", "NHS Body") ~ "Independent",
      ownershipType == "Organisation"                                ~ "Chain",
      TRUE                                                            ~ NA_character_
    )
  ) %>%
  distinct(locationid, next_group_id, providerid, ownership_category,
           location_start_dt, registrationdate_dt, locationodscode, location_end_dt, .keep_all = TRUE)



chdata <-  read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/gp_company_data_2.csv")



loc1 <- loc1 %>%
  mutate(
    providercompanieshousenumber = ifelse(
      nchar(providercompanieshousenumber) == 7 ,
      paste0("0", providercompanieshousenumber),
      providercompanieshousenumber
    )
  )%>%
  dplyr::left_join(
    chdata,
    by = c("providercompanieshousenumber" = "company_number")
  )%>%
  mutate(
    refined_ownership = case_when(
      ownership_category == "Independent" ~ "Independent",
      providercharitynumber != "" ~ "Charity",
      is_community_interest_company == "True" ~ "CIC",
      company_type %in% c(
        "private-limited-guarant-nsc",
        "registered-society-non-jurisdictional",
        "private-limited-guarant-nsc-limited-exemption"
      ) ~ "Other non-profit",
      company_type %in% c("ltd", "llp") ~ "For-profit company",
      TRUE ~ NA_character_
    )
  )





wut <- loc1 %>% dplyr::select(organisationid, locationid, ownership_category, refined_ownership, ownershipType,
                              ownership, providerid, providercompanieshousenumber, locationhscastartdate, locationhscaenddate,
                              providername, locationodscode, next_group_id, locationname, locationpostalcode, providercharitynumber,
                              rel1_relatedlocationid, rel2_relatedlocationid, is_community_interest_company, company_name, company_type)


library(dplyr)

problem_cases <- wut %>%
  group_by(locationpostalcode, locationname) %>%
  filter(n_distinct(next_group_id, na.rm = TRUE) > 1) %>%
  ungroup()

# Practice → ownership lookup (for ALL practices)
practice_ownership <- loc1 %>%
  distinct(locationodscode, ownership_category, refined_ownership, location_start_dt, location_end_dt) %>%
  rename(PRACTICE_CODE = locationodscode) %>%
  filter(!is.na(PRACTICE_CODE), !is.na(ownership_category))

library(dplyr)
library(tidyr)
library(lubridate)

annual_ownership <- practice_ownership %>%
  mutate(
    spell_start = location_start_dt,
    spell_end   = coalesce(location_end_dt, Sys.Date()),
    start_year  = year(spell_start),
    end_year    = year(spell_end)
  ) %>%
  rowwise() %>%
  mutate(year = list(seq(start_year, end_year))) %>%
  unnest(year) %>%
  mutate(
    year_start   = as.Date(paste0(year, "-01-01")),
    year_end     = as.Date(paste0(year, "-12-31")),
    overlap_days  = pmax(
      0L,
      as.integer(pmin(spell_end, year_end) - pmax(spell_start, year_start) + 1L)
    )
  ) %>%
  ungroup() %>%
  filter(overlap_days > 0) %>%
  group_by(PRACTICE_CODE, year, ownership_category, refined_ownership) %>%
  summarise(overlap_days = sum(overlap_days), .groups = "drop") %>%
  group_by(PRACTICE_CODE, year) %>%
  arrange(desc(overlap_days), ownership_category) %>%   # tie-breaker
  slice(1) %>%
  ungroup() %>%
  group_by(PRACTICE_CODE) %>%
  complete(year = seq(min(year), max(year), by = 1)) %>%
  ungroup()



write_csv(practice_ownership,
          file.path(DATA_DIR, "practice_ownership_map.csv"))

write_csv(annual_ownership,
          file.path(DATA_DIR, "practice_ownership_annual.csv"))










# ── Takeover event identification ─────────────────────────────────────────────
group_summary <- loc1 %>%
  group_by(next_group_id) %>%
  summarise(
    n_providers   = n_distinct(providerid, na.rm = TRUE),
    ownership_set = list(unique(na.omit(refined_ownership))),
    .groups = "drop"
  )

takeover_groups <- group_summary %>%
  filter(
    n_providers > 1,
    map_lgl(ownership_set, ~ all(c("For-profit company", "Independent") %in% .x)))


check <- loc1 %>%
  dplyr::filter(next_group_id %in% takeover_groups$next_group_id)%>%
  dplyr::select(locationid, location_start_dt, locationhscaenddate, next_group_id, refined_ownership)%>%
  arrange(next_group_id)


library(dplyr)
library(lubridate)

takeover_groups <- check %>%
  mutate(
    location_start_dt = as.Date(location_start_dt),
    locationhscaenddate = dmy(locationhscaenddate)
  ) %>%
  group_by(next_group_id) %>%
  summarise(
    ind_end = if (any(refined_ownership == "Independent" & !is.na(locationhscaenddate))) {
      max(locationhscaenddate[refined_ownership == "Independent"], na.rm = TRUE)
    } else {
      as.Date(NA)
    },
    chain_start = if (any(refined_ownership == "For-profit company" & !is.na(location_start_dt))) {
      min(location_start_dt[refined_ownership == "For-profit company"], na.rm = TRUE)
    } else {
      as.Date(NA)
    },
    .groups = "drop"
  ) %>%
  filter(
    !is.na(ind_end),
    chain_start >= ind_end - days(365),
    chain_start <= ind_end + days(365)
  ) 




loc_ods_map <- loc1 %>% distinct(locationid, locationodscode)

takeover_events <- loc1 %>%
  filter(next_group_id %in% takeover_groups$next_group_id) %>%
  arrange(next_group_id, location_start_dt) %>%
  group_by(next_group_id) %>%
  summarise(
    pre_location  = last(locationid[refined_ownership == "Independent" &
                                      !is.na(locationid)]),
    post_location = first(locationid[refined_ownership == "For-profit company" &
                                       !is.na(locationid)]),
    post_provider = first(providerid[refined_ownership == "For-profit company" &
                                       !is.na(providerid)]),
    takeover_date = coalesce(
      first(location_start_dt[refined_ownership == "For-profit company"]),
      first(registrationdate_dt[refined_ownership == "For-profit company"])
    ),
    n_sites = n(),
    .groups = "drop"
  ) %>%
  filter(!is.na(takeover_date)) %>%
  mutate(takeover_year = year(takeover_date)) %>%
  left_join(loc_ods_map, by = c("pre_location" = "locationid")) %>%
  rename(pre_practice_code = locationodscode) %>%
  filter(!is.na(pre_practice_code))

write_csv(takeover_events, file.path(DATA_DIR, "takeover_events_all_for_profit.csv"))



####nhs payments####

nhspay14 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/GP_payments/nhspaymentsgp-13-14-csv.csv") %>%
  dplyr::select(Practice_Code, Contract_Type, Dispensing_Practice, Payment_Description, Value) %>%
  tidyr::pivot_wider(
    id_cols = c(Practice_Code, Contract_Type, Dispensing_Practice),
    names_from = Payment_Description,
    values_from = Value
  ) %>%
  dplyr::rename(
    Average_Payment_per_Registered_Patient = `03_Average_Payment_per_Registered_Patient`,
    Average_Payment_per_Weighted_Patient   = `04_Average_Payment_per_Weighted_Patient`,
    Total_QOF_Payments                     = `14_Total_QOF_Payments`,
    Total_NHS_Payments_to_General_Practice = `35_Total_NHS_Payments_to_General_Practice`,
    Total_NHS_Payments_to_General_Practice_Minus_Deductions = `37_Total_NHS_Payments_to_General_Practice_Minus_Deductions`
  ) %>%
  dplyr::mutate(
    Practice_Type = NA_character_,
    Practice_Rurality = NA_character_
  ) %>%
  dplyr::select(
    Practice_Code, Contract_Type, Practice_Type, Dispensing_Practice, Practice_Rurality,
    Average_Payment_per_Registered_Patient, Average_Payment_per_Weighted_Patient,
    Total_QOF_Payments, Total_NHS_Payments_to_General_Practice,
    Total_NHS_Payments_to_General_Practice_Minus_Deductions
  )%>%
  dplyr::mutate(year=2014)

nhspay15 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/GP_payments/nhspaymentsgp-14-15-ann1.csv", skip = 7) %>%
  dplyr::rename(
    Practice_Code = Practice.Code,
    Average_Payment_per_Registered_Patient = Average.Payment.per.Registered.Patient,
    Average_Payment_per_Weighted_Patient = Average.Payment.per.Weighted.Patient,
    Total_QOF_Payments = Total.QOF.Payments,
    Total_NHS_Payments_to_General_Practice = Total.NHS.Payments.to.General.Practice,
    Total_NHS_Payments_to_General_Practice_Minus_Deductions = Total.NHS.Payments.to.General.Practice.Minus.Deductions,
    Contract_Type = Contract.type
  ) %>%
  dplyr::mutate(
    Practice_Type = NA_character_,
    Practice_Rurality = NA_character_,
    Dispensing_Practice = NA_character_
  ) %>%
  dplyr::select(
    Practice_Code, Contract_Type, Practice_Type, Dispensing_Practice, Practice_Rurality,
    Average_Payment_per_Registered_Patient, Average_Payment_per_Weighted_Patient,
    Total_QOF_Payments, Total_NHS_Payments_to_General_Practice,
    Total_NHS_Payments_to_General_Practice_Minus_Deductions
  )%>%
  dplyr::mutate(year=2015)

nhspay16 <- read_csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/GP_payments/nhspaymentsgp-15-16-csv.csv", locale = locale(encoding = "Latin1")) %>%
  dplyr::rename(
    Practice_Code = PracticeCode,
    Contract_Type = ContractType,
    Dispensing_Practice = DispensingPractice,
    Average_Payment_per_Registered_Patient = `AveragePaymentPerRegisteredPatient_£`,
    Average_Payment_per_Weighted_Patient = `AveragePaymentPerWeightedPatient_£`,
    Total_NHS_Payments_to_General_Practice = `TotalNHSPaymentsToGeneralPractice_£`,
    Total_NHS_Payments_to_General_Practice_Minus_Deductions = `TotalNHSPaymentsToGeneralPracticeMinusDeductions_£`,
    Total_QOF_Payments = `TotalQOFPayments_£`
  ) %>%
  dplyr::mutate(
    Practice_Type = NA_character_,
    Practice_Rurality = NA_character_
  ) %>%
  dplyr::select(
    Practice_Code, Contract_Type, Practice_Type, Dispensing_Practice, Practice_Rurality,
    Average_Payment_per_Registered_Patient, Average_Payment_per_Weighted_Patient,
    Total_QOF_Payments, Total_NHS_Payments_to_General_Practice,
    Total_NHS_Payments_to_General_Practice_Minus_Deductions
  )%>%
  dplyr::mutate(year=2016)

nhspay17 <- read_csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/GP_payments/nhspaymentsgp-16-17-csv.csv", locale = locale(encoding = "Latin1")) %>%
  dplyr::rename(
    Practice_Code = PracticeCode,
    Contract_Type = ContractType,
    Dispensing_Practice = DispensingPractice,
    Practice_Rurality = PracticeRurality,
    Average_Payment_per_Registered_Patient = `AveragePaymentsPerRegisteredPatient_£`,
    Average_Payment_per_Weighted_Patient = `AveragePaymentsPerWeightedPatient_£`,
    Total_NHS_Payments_to_General_Practice = `TotalNHSPaymentsToGeneralPractice_£`,
    Total_NHS_Payments_to_General_Practice_Minus_Deductions = `TotalNHSPaymentsToGeneralPracticeMinusDeductions_£`,
    Total_QOF_Payments = `TotalQOFPayments_£`
  ) %>%
  dplyr::mutate(
    Practice_Type = NA_character_
  ) %>%
  dplyr::select(
    Practice_Code, Contract_Type, Practice_Type, Dispensing_Practice, Practice_Rurality,
    Average_Payment_per_Registered_Patient, Average_Payment_per_Weighted_Patient,
    Total_QOF_Payments, Total_NHS_Payments_to_General_Practice,
    Total_NHS_Payments_to_General_Practice_Minus_Deductions
  )%>%
  dplyr::mutate(year=2017)

nhspay18 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/GP_payments/nhspaymentsgp-17-18-csv.csv") %>%
  dplyr::rename(
    Practice_Code = Practice.Code,
    Contract_Type = Contract.Type,
    Practice_Type = Practice.Type,
    Dispensing_Practice = Dispensing.Practice,
    Practice_Rurality = Practice.Rurality,
    Average_Payment_per_Registered_Patient = Average.payments.per.registered.patient,
    Average_Payment_per_Weighted_Patient = Average.payments.per.weighted.patient,
    Total_QOF_Payments = Total.QOF.Payments,
    Total_NHS_Payments_to_General_Practice = Total.NHS.Payments.to.General.Practice,
    Total_NHS_Payments_to_General_Practice_Minus_Deductions = Total.NHS.Payments.to.General.Practice.Minus.Deductions
  ) %>%
  dplyr::select(
    Practice_Code, Contract_Type, Practice_Type, Dispensing_Practice, Practice_Rurality,
    Average_Payment_per_Registered_Patient, Average_Payment_per_Weighted_Patient,
    Total_QOF_Payments, Total_NHS_Payments_to_General_Practice,
    Total_NHS_Payments_to_General_Practice_Minus_Deductions
  )%>%
  dplyr::mutate(year=2018)

nhspay19 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/GP_payments/nhspaymentsgp-18-19-csv.csv") %>%
  dplyr::rename(
    Practice_Code = Practice.Code,
    Contract_Type = Contract.Type,
    Practice_Type = Practice.Type,
    Dispensing_Practice = Dispensing.Practice,
    Practice_Rurality = Practice.Rurality,
    Average_Payment_per_Registered_Patient = Average.payments.per.registered.patient,
    Average_Payment_per_Weighted_Patient = Average.payments.per.weighted.patient,
    Total_QOF_Payments = Total.QOF.Payments,
    Total_NHS_Payments_to_General_Practice = Total.NHS.Payments.to.General.Practice,
    Total_NHS_Payments_to_General_Practice_Minus_Deductions = Total.NHS.Payments.to.General.Practice.Minus.Deductions
  ) %>%
  dplyr::select(
    Practice_Code, Contract_Type, Practice_Type, Dispensing_Practice, Practice_Rurality,
    Average_Payment_per_Registered_Patient, Average_Payment_per_Weighted_Patient,
    Total_QOF_Payments, Total_NHS_Payments_to_General_Practice,
    Total_NHS_Payments_to_General_Practice_Minus_Deductions
  )%>%
  dplyr::mutate(year=2019)

nhspay20 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/GP_payments/nhspaymentsgp-19-20-prac-csv.csv") %>%
  dplyr::rename(
    Practice_Code = Practice.Code,
    Contract_Type = Contract.Type,
    Practice_Type = Practice.Type,
    Dispensing_Practice = Dispensing.Practice,
    Practice_Rurality = Practice.Rurality,
    Average_Payment_per_Registered_Patient = Average.payments.per.registered.patient1,
    Average_Payment_per_Weighted_Patient = Average.payments.per.weighted.patient1,
    Total_QOF_Payments = Total.QOF.Payments,
    Total_NHS_Payments_to_General_Practice = Total.NHS.Payments.to.General.Practice,
    Total_NHS_Payments_to_General_Practice_Minus_Deductions = Total.NHS.Payments.to.General.Practice.Minus.Deductions2
  ) %>%
  dplyr::select(
    Practice_Code, Contract_Type, Practice_Type, Dispensing_Practice, Practice_Rurality,
    Average_Payment_per_Registered_Patient, Average_Payment_per_Weighted_Patient,
    Total_QOF_Payments, Total_NHS_Payments_to_General_Practice,
    Total_NHS_Payments_to_General_Practice_Minus_Deductions
  )%>%
  dplyr::mutate(year=2020)

nhspay21 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/GP_payments/nhspaymentsgp-20-21-prac-csv.csv") %>%
  dplyr::rename(
    Practice_Code = Practice.Code,
    Contract_Type = Contract.Type,
    Practice_Type = Practice.type,
    Dispensing_Practice = Dispensing.Practice,
    Practice_Rurality = Practice.Rurality,
    Average_Payment_per_Registered_Patient = Average.payments.per.registered.patient,
    Average_Payment_per_Weighted_Patient = Average.payments.per.weighted.patient,
    Total_QOF_Payments = Total.QOF.Payments,
    Total_NHS_Payments_to_General_Practice = Total.NHS.Payments.to.General.Practice,
    Total_NHS_Payments_to_General_Practice_Minus_Deductions = Total.NHS.Payments.to.General.Practice.Minus.Deductions
  ) %>%
  dplyr::select(
    Practice_Code, Contract_Type, Practice_Type, Dispensing_Practice, Practice_Rurality,
    Average_Payment_per_Registered_Patient, Average_Payment_per_Weighted_Patient,
    Total_QOF_Payments, Total_NHS_Payments_to_General_Practice,
    Total_NHS_Payments_to_General_Practice_Minus_Deductions
  )%>%
  dplyr::mutate(year=2021)

nhspay22 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/GP_payments/nhspaymentsgp-21-22-prac-csv-v2.csv") %>%
  dplyr::rename(
    Practice_Code = Practice.Code,
    Contract_Type = Contract.Type,
    Practice_Type = Practice.type,
    Dispensing_Practice = Dispensing.Practice,
    Practice_Rurality = Practice.Rurality,
    Average_Payment_per_Registered_Patient = Average.payments.per.registered.patient,
    Average_Payment_per_Weighted_Patient = Average.payments.per.weighted.patient,
    Total_QOF_Payments = Total.QOF.Payments,
    Total_NHS_Payments_to_General_Practice = Total.NHS.Payments.to.General.Practice,
    Total_NHS_Payments_to_General_Practice_Minus_Deductions = Total.NHS.Payments.to.General.Practice.Minus.Deductions
  ) %>%
  dplyr::select(
    Practice_Code, Contract_Type, Practice_Type, Dispensing_Practice, Practice_Rurality,
    Average_Payment_per_Registered_Patient, Average_Payment_per_Weighted_Patient,
    Total_QOF_Payments, Total_NHS_Payments_to_General_Practice,
    Total_NHS_Payments_to_General_Practice_Minus_Deductions
  )%>%
  dplyr::mutate(year=2022)

nhspay23 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/GP_payments/nhspaymentsgp-22-23-prac-csv.csv") %>%
  dplyr::rename(
    Practice_Code = Practice.Code,
    Contract_Type = Contract.Type,
    Practice_Type = Practice.type,
    Dispensing_Practice = Dispensing.Practice,
    Practice_Rurality = Practice.Rurality,
    Average_Payment_per_Registered_Patient = Average.payments.per.registered.patient,
    Average_Payment_per_Weighted_Patient = Average.payments.per.weighted.patient,
    Total_QOF_Payments = Total.QOF.Payments,
    Total_NHS_Payments_to_General_Practice = Total.NHS.Payments.to.General.Practice,
    Total_NHS_Payments_to_General_Practice_Minus_Deductions = Total.NHS.Payments.to.General.Practice.Minus.Deductions
  ) %>%
  dplyr::select(
    Practice_Code, Contract_Type, Practice_Type, Dispensing_Practice, Practice_Rurality,
    Average_Payment_per_Registered_Patient, Average_Payment_per_Weighted_Patient,
    Total_QOF_Payments, Total_NHS_Payments_to_General_Practice,
    Total_NHS_Payments_to_General_Practice_Minus_Deductions
  )%>%
  dplyr::mutate(year=2023)

nhspay24 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/GP_payments/nhspaymentsgp-23-24-prac-csv.csv") %>%
  dplyr::rename(
    Practice_Code = Practice.Code,
    Contract_Type = Contract.Type,
    Practice_Type = Practice.type,
    Dispensing_Practice = Dispensing.Practice,
    Practice_Rurality = Practice.Rurality,
    Average_Payment_per_Registered_Patient = Average.payments.per.registered.patient,
    Average_Payment_per_Weighted_Patient = Average.payments.per.weighted.patient,
    Total_QOF_Payments = Total.QOF.Payments,
    Total_NHS_Payments_to_General_Practice = Total.NHS.Payments.to.General.Practice,
    Total_NHS_Payments_to_General_Practice_Minus_Deductions = Total.NHS.Payments.to.General.Practice.Minus.Deductions
  ) %>%
  dplyr::select(
    Practice_Code, Contract_Type, Practice_Type, Dispensing_Practice, Practice_Rurality,
    Average_Payment_per_Registered_Patient, Average_Payment_per_Weighted_Patient,
    Total_QOF_Payments, Total_NHS_Payments_to_General_Practice,
    Total_NHS_Payments_to_General_Practice_Minus_Deductions
  )%>%
  dplyr::mutate(year=2024)

nhspay25 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/GP_payments/nhspaymentsgp-24-25-prac-csv (1).csv") %>%
  dplyr::rename(
    Practice_Code = Practice.Code,
    Contract_Type = Contract.Type,
    Practice_Type = Practice.type,
    Dispensing_Practice = Dispensing.Practice,
    Practice_Rurality = Practice.Rurality,
    Average_Payment_per_Registered_Patient = Average.payments.per.registered.patient,
    Average_Payment_per_Weighted_Patient = Average.payments.per.weighted.patient,
    Total_QOF_Payments = Total.QOF.Payments,
    Total_NHS_Payments_to_General_Practice = Total.NHS.Payments.to.General.Practice,
    Total_NHS_Payments_to_General_Practice_Minus_Deductions = Total.NHS.Payments.to.General.Practice.Minus.Deductions
  ) %>%
  dplyr::select(
    Practice_Code, Contract_Type, Practice_Type, Dispensing_Practice, Practice_Rurality,
    Average_Payment_per_Registered_Patient, Average_Payment_per_Weighted_Patient,
    Total_QOF_Payments, Total_NHS_Payments_to_General_Practice,
    Total_NHS_Payments_to_General_Practice_Minus_Deductions
  )%>%
  dplyr::mutate(year=2025)


num_cols <- c(
  "Average_Payment_per_Registered_Patient",
  "Average_Payment_per_Weighted_Patient",
  "Total_QOF_Payments",
  "Total_NHS_Payments_to_General_Practice",
  "Total_NHS_Payments_to_General_Practice_Minus_Deductions"
)

clean_numeric <- function(df) {
  df %>%
    dplyr::mutate(
      dplyr::across(all_of(num_cols), ~ readr::parse_number(as.character(.)))
    )
}


nhspay_all <- dplyr::bind_rows(
  clean_numeric(nhspay14),
  clean_numeric(nhspay15),
  clean_numeric(nhspay16),
  clean_numeric(nhspay17),
  clean_numeric(nhspay18),
  clean_numeric(nhspay19),
  clean_numeric(nhspay20),
  clean_numeric(nhspay21),
  clean_numeric(nhspay22),
  clean_numeric(nhspay23),
  clean_numeric(nhspay24),
  clean_numeric(nhspay25)
)





write_csv(nhspay_all, "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/nhs_pay.csv")



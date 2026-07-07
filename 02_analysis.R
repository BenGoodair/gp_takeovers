rm(list = ls())

####intro and guide####

#Welcome to the coding file for a research study analysing the corporate delivery of primary care in England.
#All underlying data to this file are published at https://github.com/BenGoodair/gp_takeovers/tree/main/Data
#The code uses curl to pull the data directly from the github page, this a) signposts what data comes from where and b) should make it replicable, depending on dependencies and packages.
#Code used to clean the underlying data is available at https://github.com/BenGoodair/gp_takeovers/blob/main/01.5_manual_downloads.R
#This file is structured with some data merging of all the different underlying datasets into a master_df, followed by the analyses as per the order of the paper
#Analyses are separated by headings with four hashtages
#All supplementary material analyses are included in this file, at the end of the document.
#For now the code uses stored objects in order, for final publication, each analysis will run from raw data in separately published functions
#Some of the appendix will require re-running the main models before working.


suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(patchwork)
  library(ggdist)       # half-violin / rain-cloud plots
  library(scales)
  library(did)          # Callaway–Sant'Anna DiD
  library(broom)
  library(glue)
  library(ggrepel)
  library(curl)
  library(viridis)
})

# DATA_DIR <- path.expand(
#   "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data"
# )




# Colour palette: muted, accessible, colourblind-safe
PAL <- c(
  Corporation       = "#1A6FBF",   # blue
  Independent = "#D4622A"    # orange
)

# Annotation colours
COL_CHAIN <- PAL["Corporation"]
COL_IND   <- PAL["Independent"]



# Staff-category palette (5 categories)
PAL_STAFF <- c(
  GP_qualified = "#1A6FBF",
  GP_locum     = "#8EC4E8",
  Nursing      = "#D4622A",
  DPC          = "#EDA96A",
  Admin        = "#6DAD60"
)

# Typography & geometry
BASE_SIZE  <- 9      # Nature single-column text size
AXIS_SIZE  <- 8
STRIP_SIZE <- 8.5
POINT_ALPHA <- 0.22
JITTER_W    <- 0.12
BOX_W       <- 0.40


#### LOAD DATA####



ownership_annual <- read_csv(curl("https://raw.githubusercontent.com/BenGoodair/gp_takeovers/refs/heads/main/Data/practice_ownership_annual.csv"))

ownership_annual <- ownership_annual %>%dplyr::mutate(ownership_category=ifelse(ownership_category=="Chain", "Corporation", ownership_category))

patient_sat <- read_csv(curl("https://raw.githubusercontent.com/BenGoodair/gp_takeovers/refs/heads/main/Data/patient_satisfaction.csv"))


workforce_annual <- read_csv(curl("https://raw.githubusercontent.com/BenGoodair/gp_takeovers/refs/heads/main/Data/workforce_annual.csv")) %>%
  dplyr::mutate(GP_LOCUM_FTE = parse_number(TOTAL_GP_FTE) - parse_number(TOTAL_GP_EXL_FTE))





workforce_annual <- workforce_annual %>%
  mutate(
    across(all_of(c("TOTAL_GP_FTE","GP_LOCUM_FTE","TOTAL_NURSES_FTE","TOTAL_DPC_FTE","TOTAL_ADMIN_FTE","TOTAL_PATIENTS")),
           ~ suppressWarnings(as.numeric(as.character(.x)))),
    TOTAL_GP_FTE_PER1K = if_else(TOTAL_PATIENTS > 100, TOTAL_GP_FTE / TOTAL_PATIENTS * 1000, NA_real_),
    GP_LOCUM_PER1K    = if_else(TOTAL_PATIENTS > 100, GP_LOCUM_FTE    / TOTAL_PATIENTS * 1000, NA_real_),
    NURSE_PER1K       = if_else(TOTAL_PATIENTS > 100, TOTAL_NURSES_FTE       / TOTAL_PATIENTS * 1000, NA_real_),
    DPC_PER1K         = if_else(TOTAL_PATIENTS > 100, TOTAL_DPC_FTE         / TOTAL_PATIENTS * 1000, NA_real_),
    ADMIN_PER1K       = if_else(TOTAL_PATIENTS > 100, TOTAL_ADMIN_FTE       / TOTAL_PATIENTS * 1000, NA_real_)
  )



library(dplyr)
library(tidyr)
library(ggplot2)
library(ggdist)
library(patchwork)
library(scales)
library(readr)

STAFF_METRICS <- c(
  TOTAL_GP_FTE_PER1K = "Qualified GP FTE\nper 1,000 patients",
  NURSE_PER1K        = "Nursing FTE\nper 1,000 patients"
)

PATIENT_METRICS <- c(
  appointment_good = "Appointment experience\ngood",
  overall_good     = "Overall practice experience\ngood"
)

OWNERSHIP_LEVELS <- names(PAL)

pub_theme <- theme_bw(base_size = 10) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 11, margin = margin(b = 4)),
    axis.text = element_text(colour = "black"),
    axis.text.y = element_text(face = "bold"),
    strip.background = element_rect(fill = "grey95", colour = NA),
    strip.text = element_text(face = "bold", size = 9),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.spacing = unit(0.9, "lines"),
    plot.margin = margin(5, 5, 5, 5)
  )


workforce_long <- workforce_annual %>%
  rename(PRACTICE_CODE = PRAC_CODE) %>%
  left_join(ownership_annual, by = c("PRACTICE_CODE", "year")) %>%
  mutate(ownership_category = factor(ownership_category, levels = OWNERSHIP_LEVELS)) %>%
  group_by(PRACTICE_CODE, ownership_category, year) %>%
  summarise(across(all_of(names(STAFF_METRICS)), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(
    cols = all_of(names(STAFF_METRICS)),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(metric_label = factor(STAFF_METRICS[metric], levels = unname(STAFF_METRICS)))


workforce_time <- workforce_annual %>%
  rename(PRACTICE_CODE = PRAC_CODE) %>%
  left_join(ownership_annual, by = c("PRACTICE_CODE", "year")) %>%
  mutate(ownership_category = factor(ownership_category, levels = OWNERSHIP_LEVELS)) %>%
  group_by(year, ownership_category) %>%
  summarise(across(all_of(names(STAFF_METRICS)), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(
    cols = all_of(names(STAFF_METRICS)),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(metric_label = factor(STAFF_METRICS[metric], levels = unname(STAFF_METRICS)))

patient_clean <- patient_sat %>%
  rename(PRACTICE_CODE = Practice_Code) %>%
  left_join(ownership_annual, by = c("PRACTICE_CODE", "year")) %>%
  mutate(
    ownership_category = factor(ownership_category, levels = OWNERSHIP_LEVELS),
    appointment_good = as.numeric(appointment_good),
    overall_good = parse_number(as.character(overall_good))
  ) 

patient_cross <- patient_clean %>%
  group_by(PRACTICE_CODE, ownership_category) %>%
  summarise(across(all_of(names(PATIENT_METRICS)), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(
    cols = all_of(names(PATIENT_METRICS)),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(metric_label = factor(PATIENT_METRICS[metric], levels = unname(PATIENT_METRICS)))

patient_time <- patient_clean %>%
  group_by(year, ownership_category) %>%
  summarise(across(all_of(names(PATIENT_METRICS)), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(
    cols = all_of(names(PATIENT_METRICS)),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(metric_label = factor(PATIENT_METRICS[metric], levels = unname(PATIENT_METRICS)))






qof <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/gp_takeovers/refs/heads/main/Data/qof_annual.csv"))%>%
  dplyr::rename(PRACTICE_CODE = PRAC_CODE)%>%
  left_join(ownership_annual, by = c("PRACTICE_CODE", "year"))

las <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/gp_takeovers/refs/heads/main/Data/las.csv"))



####master data ####


master_df <- workforce_annual%>%
  dplyr::full_join(.,qof %>%
                     dplyr::select(PRACTICE_CODE, PRACTICE_NAME, VALUE, year)%>%
                     dplyr::rename(PRAC_CODE = PRACTICE_CODE,
                                   QOF_points = VALUE,
                                   qof_name = PRACTICE_NAME))


master_df <- master_df %>%
  dplyr::full_join(., patient_clean%>%
                     dplyr::select(PRACTICE_CODE, Practice_Name, appointment_good, overall_good, year)%>%
                     rename(PRAC_CODE = PRACTICE_CODE,
                            patient_name = Practice_Name))


master_df <- master_df %>%
  dplyr::left_join(., ownership_annual%>%
                     dplyr::rename(PRAC_CODE = PRACTICE_CODE))




master_df <- master_df %>%
  dplyr::left_join(., las%>%
                     dplyr::rename(PRAC_CODE = locationodscode))

lifeex <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/gp_takeovers/refs/heads/main/Data/life_expectancy.csv"))

qof_prev <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/gp_takeovers/refs/heads/main/Data/qof_annual_prev.csv"))


imd <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/gp_takeovers/refs/heads/main/Data/imd.csv"))





master_df <- master_df %>%
  dplyr::left_join(., lifeex%>%
                     dplyr::rename(PRAC_CODE = gp_code)%>%
                     dplyr::select(-X))

master_df <- master_df %>%
  dplyr::left_join(., qof_prev%>%
                     dplyr::rename(PRAC_CODE = gp_code)%>%
                     dplyr::select(-X))

master_df <- master_df %>%
  dplyr::left_join(., imd%>%
                     dplyr::rename(PRAC_CODE = gp_code)%>%
                     dplyr::select(-X))



pay <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/gp_takeovers/refs/heads/main/Data/nhs_pay.csv"))


pay <- pay %>%
  dplyr::mutate(
    Dispensing_Practice = dplyr::na_if(Dispensing_Practice, "UNKNOWN"),
    Dispensing_Practice = dplyr::na_if(Dispensing_Practice, "Unknown"),
    Practice_Rurality = dplyr::na_if(Practice_Rurality, "MISSING"),
    Practice_Rurality = dplyr::na_if(Practice_Rurality, "Missing")
  ) %>%
  dplyr::group_by(Practice_Code) %>%
  tidyr::fill(
    Dispensing_Practice,
    Practice_Rurality,
    .direction = "updown"
  ) %>%
  dplyr::ungroup()

master_df <-master_df%>% dplyr::left_join(., pay%>%
                                dplyr::rename(PRAC_CODE = Practice_Code))

master_df <- master_df %>%
  dplyr::group_by(PRAC_CODE) %>%
  tidyr::fill(
    Dispensing_Practice,
    Practice_Rurality,
    PRAC_NAME,
    .direction = "updown"
  ) %>%
  dplyr::ungroup()

#write_csv(master_df, "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/gp_takeovers/Data/master_data_GPs_May.csv")

####table 1####
library(gt)
library(gtsummary)

summary_table <- master_df %>%
  dplyr::select(PRAC_CODE, TOTAL_GP_FTE_PER1K, NURSE_PER1K, QOF_points, overall_good, appointment_good, ownership_category, Practice_Rurality, Dispensing_Practice, TOTAL_PATIENTS) %>%
  dplyr::filter(!is.na(ownership_category))%>%
  dplyr::group_by(PRAC_CODE, ownership_category, Practice_Rurality, Dispensing_Practice)  %>%
  dplyr::summarise(TOTAL_GP_FTE_PER1K = mean(TOTAL_GP_FTE_PER1K, na.rm=T),
                   NURSE_PER1K= mean(NURSE_PER1K, na.rm=T),
                   QOF_points= mean(QOF_points, na.rm=T),
                   overall_good= mean(overall_good, na.rm=T),
                   appointment_good= mean(appointment_good, na.rm=T),
                   TOTAL_PATIENTS = mean(TOTAL_PATIENTS, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::select(TOTAL_GP_FTE_PER1K, NURSE_PER1K, QOF_points, overall_good, appointment_good, ownership_category, Practice_Rurality, Dispensing_Practice, TOTAL_PATIENTS) %>%
  tbl_summary(
    by = ownership_category,
    missing = "no",
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c(
      "{median}, {mean}",
      "({min} : {max}), ({p25} : {p75})",   # <-- Q1 and Q3 displayed here
      "{N_nonmiss}"
    ),
    label = list(
      TOTAL_GP_FTE_PER1K ~ "GPs hired per 1k patients (FTE)",
      NURSE_PER1K ~ "Nurses hired per 1k patients (FTE)",
      QOF_points ~ "Quality Outcome Framework points achieved (%)",
      overall_good ~ "Patient satisfaction (overall, good, %)",
      appointment_good ~ "Patient satisfaction (appointment, good, %)",
      TOTAL_PATIENTS ~ "Total patient list (n)",
      Practice_Rurality ~ "Practice rurality",
      Dispensing_Practice = "Dispensing practice"
    )
  ) %>%
  add_n() %>%
  modify_spanning_header(
    c("stat_1", "stat_2") ~ "**Ownership**"
  ) %>%
  modify_caption("**Table. General practice quality and chartacteristics**") %>%
  modify_header(label = "**Variable**") %>%
  bold_labels()

# Convert the gtsummary table to a gt table to add sub-headers and styling
gt_table <- as_gt(summary_table) %>%
  
  
  # Additional styling for a polished look
  tab_options(
    table.font.names = "Calibri",
    table.font.size = 12,
    row_group.background.color = "#E6E6E6",
    table.border.top.style = "solid",
    table.border.top.width = px(2),
    table.border.top.color = "gray"
  )

gt_table




####Figure 1####
comp_vals <- ownership_annual %>%
  summarise(n = n_distinct(PRACTICE_CODE))

comp_vals <- ownership_annual %>%
  dplyr::filter(ownership_category=="Corporation")%>%
  summarise(n = n_distinct(PRACTICE_CODE))


chain_share_year <- ownership_annual %>%
  dplyr::filter(PRACTICE_CODE %in% patient_clean$PRACTICE_CODE) %>%
  distinct(PRACTICE_CODE, year, ownership_category) %>%
  group_by(year) %>%
  summarise(
    n_total = n(),
    n_chain = sum(ownership_category == "Corporation", na.rm = TRUE),
    pct_chain = 100 * n_chain / n_total,
    .groups = "drop"
  ) %>%
  filter(!is.na(pct_chain), year>2012)

# Optional end labels
chain_labels <- chain_share_year %>%
  dplyr::slice(c(1, n())) %>%
  mutate(lbl = sprintf("%.1f%%", pct_chain))

chdata <-  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/gp_takeovers/refs/heads/main/Data/gp_company_data_2.csv"))



cqcref <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/gp_takeovers/refs/heads/main/Data/cqcref.csv"))

orgs <- cqcref %>%
  left_join(
    chdata,
    by = c("providercompanieshousenumber" = "company_number")
  ) %>%
  filter(ownershipType == "Organisation") %>%
  mutate(
    refined_ownership = case_when(
      !is.na(providercharitynumber) & providercharitynumber != "" ~ "Charity",
      is_community_interest_company == TRUE |
        is_community_interest_company == "True" ~ "CIC",
      company_type %in% c(
        "private-limited-guarant-nsc",
        "private-limited-guarant-nsc-limited-exemption",
        "registered-society-non-jurisdictional"
      ) ~ "Other non-profit",
      company_type %in% c("ltd", "llp") ~ "For-profit company",
      TRUE ~ NA_character_
    )
  )

check <- orgs %>%
  dplyr::filter(is.na(refined_ownership))


plot_org <- orgs %>%
  dplyr::select(refined_ownership, locationodscode )%>%
  dplyr::distinct(.keep_all = T)

unique_GPs <- patient_clean %>%
  dplyr::select(PRACTICE_CODE, Practice_Name)%>%
  dplyr::distinct(.keep_all=T)


plot_org <- plot_org %>%
  dplyr::left_join(.,  unique_GPs,   by = c("locationodscode" = "PRACTICE_CODE")
  )



library(dplyr)
library(ggplot2)
library(forcats)
library(scales)

# Harmonious palette aligned with your existing figures
PAL_OWNERSHIP <- c(
  `For-profit company` = "#1A6FBF",  # blue
  Charity              = "#D4622A",  # orange
  `CIC`                = "#6DAD60",  # green
  `Other non-profit`   = "#9B4A9E",  # purple
  Missing              = "grey70"
)

ownership_df <- plot_org %>%
  mutate(
    refined_ownership = fct_explicit_na(refined_ownership, na_level = "Missing")
  ) %>%
  dplyr::count(refined_ownership, name = "n") %>%
  mutate(
    refined_ownership = fct_reorder(refined_ownership, n)
  )

fig_ownership <- ggplot(
  ownership_df,
  aes(x = refined_ownership, y = n, fill = refined_ownership)
) +
  geom_col(width = 0.68, colour = "grey25", linewidth = 0.25) +
  geom_text(
    aes(label = n),
    hjust = -0.10,
    size = 2.7,
    colour = "grey20"
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = PAL_OWNERSHIP, drop = FALSE) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.10)),
    labels = label_number()
  ) +
  labs(
    title = "Ownership type of Corporate-run GP Practices",
    x = NULL,
    y = "Count"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = BASE_SIZE + 1, face = "bold"),
    axis.title = element_text(size = AXIS_SIZE),
    axis.text = element_text(size = AXIS_SIZE),
    panel.grid.major.y = element_blank()
  )+
  pub_theme




fig1_E <- ggplot(chain_share_year, aes(x = year, y = pct_chain)) +
  geom_area(fill = COL_CHAIN, alpha = 0.18) +
  geom_line(linewidth = 1.1, colour = COL_CHAIN) +
  geom_point(size = 1.8, colour = COL_CHAIN) +
  geom_text(
    data = chain_labels,
    aes(label = lbl),
    vjust = if_else(chain_labels$year == min(chain_labels$year), -0.8, 1.2),
    fontface = "bold",
    size = 3.0,
    colour = COL_CHAIN
  ) +
  scale_x_continuous(breaks = c(2013, 2014,
                                2015, 2016,
                                2017, 2018,
                                2019, 2020,
                                2021, 2022,
                                2023, 2024,
                                2025, 2026)) +
  scale_y_continuous(
    limits = c(0, max(chain_share_year$pct_chain, na.rm = TRUE) * 1.15),
    labels = label_number(accuracy = 1, suffix = "%"),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Share of practices run by a corporation",
    x = NULL,
    y = "% of practices"
  ) +
  pub_theme +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 5, 0, 5),
    axis.title.x = element_blank()
  )




treatment_timing <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/gp_takeovers/refs/heads/main/Data/takeover_events_all_chain.csv"))%>%
  dplyr::rename(PRACTICE_CODE = pre_practice_code)%>%
  dplyr::mutate(g = factor(takeover_year))%>%
  dplyr::group_by(g)%>%
  dplyr::summarise(n_takeovers = n())%>%
  dplyr::ungroup()

fig1_f <- ggplot(treatment_timing, aes(x = g, y = n_takeovers)) +
  geom_col() +
  labs(
    title = "Number of corporate GP takeovers each year",
    x = "Year",
    y = "n of practices"
  ) +
  pub_theme +
  scale_x_discrete(breaks = c("2013", "2014",
                              "2015", "2016",
                              "2017", "2018",
                              "2019", "2020",
                              "2021", "2022",
                              "2023", "2024",
                              "2025", "2026")) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 5, 0, 5),
  )


fig1_E/fig1_f|fig_ownership





####Figure 2####

fig1_A <- ggplot(
  workforce_long%>%
    filter(!is.na(ownership_category), !is.na(value)) %>%
    group_by(metric) %>%
    mutate(p99 = quantile(value, 0.99, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(value <= p99) %>%
    select(-p99),
  aes(x = value, y = ownership_category, fill = ownership_category, colour = ownership_category)
) +
  ggdist::stat_halfeye(
    orientation = "y",
    adjust = 0.9,
    width = 0.55,
    .width = 0,
    justification = -0.22,
    point_colour = NA,
    alpha = 0.80
  )  +
  geom_point(
    position = position_jitter(height = 0.08, width = 0, seed = 1),
    size = 0.45,
    alpha = 0.35,
    stroke = 0
  ) +
  geom_boxplot(
    orientation = "y",
    width = 0.18,
    outlier.shape = NA,
    linewidth = 0.35,
    fill = NA,
    colour = "grey25"
  )+
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 18,
    size = 2.8,
    fill = "black",
    colour = "black",    show.legend = FALSE
  ) +
  facet_wrap(~ metric_label, scales = "free_x", nrow = 1) +
  scale_fill_manual(values = PAL, drop = FALSE) +
  scale_colour_manual(values = PAL, drop = FALSE) +
  scale_x_continuous(labels = label_number(accuracy = 0.01)) +
  labs(
    title = "A. Workforce supply",
    x = "FTE per 1,000 registered patients",
    y = NULL
  ) +
  pub_theme+
  theme(legend.position = "none")

fig1_B <- ggplot(
  workforce_time %>% filter(!is.na(ownership_category), !is.na(value), year > 2012),
  aes(x = year, y = value, colour = ownership_category, group = ownership_category)
) +
  geom_line(linewidth = 0.55) +
  geom_point(size = 1.0) +
  facet_wrap(~ metric_label, scales = "free_y", nrow = 1) +
  scale_colour_manual(values = PAL, drop = FALSE) +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(
    title = "",
    x = "Year",
    y = "FTE per 1,000 registered patients"
  ) +
  pub_theme+
  theme(legend.position = "none")

fig1_C <- ggplot(
  patient_cross %>% filter(!is.na(ownership_category), !is.na(value)),
  aes(x = value, y = ownership_category, fill = ownership_category, colour = ownership_category)
) +
  ggdist::stat_halfeye(
    orientation = "y",
    adjust = 1.2,
    width = 0.55,
    .width = 0,
    justification = -0.22,
    point_colour = NA,
    alpha = 0.80
  ) +
  geom_point(
    position = position_jitter(height = 0.08, width = 0, seed = 1),
    size = 0.45,
    alpha = 0.35,
    stroke = 0
  ) +
  geom_boxplot(
    orientation = "y",
    width = 0.18,
    outlier.shape = NA,
    linewidth = 0.35,
    fill = NA,
    colour = "grey25"
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 18,
    size = 2.8,
    fill = "black",
    colour = "black",
    show.legend = FALSE
  )+
  facet_wrap(~ metric_label, scales = "free_x", nrow = 1) +
  scale_fill_manual(values = PAL, drop = FALSE) +
  scale_colour_manual(values = PAL, drop = FALSE) +
  scale_x_continuous(labels = label_number(accuracy = 1, suffix = "%")) +
  labs(
    title = "C  Patient experience",
    x = "Patients reporting good experience (%)",
    y = NULL
  ) +
  pub_theme+
  theme(legend.position = "none")

fig1_D <- ggplot(
  patient_time %>% filter(!is.na(ownership_category), !is.na(value), year > 2012),
  aes(x = year, y = value, colour = ownership_category, group = ownership_category)
) +
  geom_line(linewidth = 0.55) +
  geom_point(size = 1.0) +
  facet_wrap(~ metric_label, scales = "free_y", nrow = 1) +
  scale_colour_manual(values = PAL, drop = FALSE) +
  scale_y_continuous(labels = label_number(accuracy = 1, suffix = "%")) +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(
    title = "",
    x = "Year",
    y = "Patients reporting good experience (%)"
  ) +
  pub_theme+
  theme(legend.position = "none")



qof <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/gp_takeovers/refs/heads/main/Data/qof_annual.csv"))%>%
  dplyr::rename(PRACTICE_CODE = PRAC_CODE)%>%
  left_join(ownership_annual, by = c("PRACTICE_CODE", "year"))





fig1_Cqof <- ggplot(
  qof %>% filter(!is.na(ownership_category), !is.na(VALUE))%>%
    dplyr::group_by(PRACTICE_CODE,ownership_category, VALUE)%>%
    dplyr::summarise(VALUE = mean(VALUE, na.rm=T))%>%
    dplyr::ungroup(),
  aes(x = VALUE, y = ownership_category, fill = ownership_category, colour = ownership_category)
) +
  ggdist::stat_halfeye(
    orientation = "y",
    adjust = 1.2,
    width = 0.55,
    .width = 0,
    justification = -0.22,
    point_colour = NA,
    alpha = 0.80
  ) +
  geom_point(
    position = position_jitter(height = 0.08, width = 0, seed = 1),
    size = 0.45,
    alpha = 0.35,
    stroke = 0
  ) +
  geom_boxplot(
    orientation = "y",
    width = 0.18,
    outlier.shape = NA,
    linewidth = 0.35,
    fill = NA,
    colour = "grey25"
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 18,
    size = 2.8,
    fill = "black",
    colour = "black",
    show.legend = FALSE
  )+
  scale_fill_manual(values = PAL, drop = FALSE) +
  scale_colour_manual(values = PAL, drop = FALSE) +
  scale_x_continuous(labels = label_number(accuracy = 1, suffix = "%")) +
  labs(
    title = "B. Quality Outcomes Framework (QOF)",
    x = "Percentage of points achieved (%)",
    y = NULL
  ) +
  pub_theme+
  theme(legend.position = "none")





fig1_Dqof <- ggplot(
  qof %>% filter(!is.na(ownership_category), !is.na(VALUE), year > 2012)%>%
    group_by(year, ownership_category)%>%
    dplyr::summarise(VALUE = mean(VALUE, na..rm=T)),
  aes(x = year, y = VALUE, colour = ownership_category, group = ownership_category)
) +
  geom_line(linewidth = 0.55) +
  geom_point(size = 1.0) +
  scale_colour_manual(values = PAL, drop = FALSE) +
  scale_y_continuous(labels = label_number(accuracy = 1, suffix = "%")) +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(
    title = "",
    x = "Year",
    y = "Percentage of points achieved (%)"
  ) +
  pub_theme+
  theme(  legend.text = element_text(size = 11),
          legend.direction = "vertical"
          )









figure_1_panel <- (fig1_A | fig1_B) /  (fig1_Cqof | fig1_Dqof) / (fig1_C | fig1_D)  +
  plot_layout( guides = "collect")

figure_1_panel



####Table 2: cross sectional regressions####



library(dplyr)
library(modelsummary)
library(sandwich)

df <- master_df %>%
  mutate(
    ownership_category = relevel(factor(ownership_category), ref = "Independent"),
    year = factor(year),
    TOTAL_PATIENTS = TOTAL_PATIENTS/1000,
    across(
      c(TOTAL_GP_FTE_PER1K, NURSE_PER1K, QOF_points, overall_good, appointment_good),
      ~ as.numeric(scale(.)),
      .names = "z_{.col}"
    )
  )

library(ordinal)
mods <- list(
  "GP FTE per 1,000" = lm(z_TOTAL_GP_FTE_PER1K ~ ownership_category  + qof_composite+life_expectancy + imd25 +Dispensing_Practice+ Practice_Rurality+ TOTAL_PATIENTS + year+locationlocalauthority,
                          data = df),
  "Nurses per 1,000" = lm(z_NURSE_PER1K ~ ownership_category  + qof_composite+life_expectancy + imd25 +Dispensing_Practice+ Practice_Rurality+ TOTAL_PATIENTS + year+locationlocalauthority,
                          data = df),
  "QOF points" = lm(z_QOF_points ~ ownership_category  + qof_composite+Dispensing_Practice+ Practice_Rurality+ life_expectancy + imd25 + TOTAL_PATIENTS + year+locationlocalauthority,
                    data = df),
  "Overall good" = lm(z_overall_good ~ ownership_category  + qof_composite+Dispensing_Practice+ Practice_Rurality+ life_expectancy + imd25 + TOTAL_PATIENTS + year+locationlocalauthority,
                      data = df),
  "Appointment good" = lm(z_appointment_good ~ ownership_category  + qof_composite +Dispensing_Practice+ Practice_Rurality+ life_expectancy+ imd25 + TOTAL_PATIENTS + year+locationlocalauthority,
                          data = df)
)

library(modelsummary)
library(sandwich)
library(lmtest)
library(dplyr)

# Clustered VCOV function
vcov_cluster <- function(model) {
  sandwich::vcovCL(model, cluster = ~PRAC_CODE)
}

# Custom coefficient table function (adds CI + p-values)
tidy_bmj <- function(model) {
  co <- coef(model)
  vc <- vcov_cluster(model)
  se <- sqrt(diag(vc))
  
  tibble(
    term = names(co),
    estimate = co,
    se = se,
    conf.low = co - 1.96 * se,
    conf.high = co + 1.96 * se,
    p.value = 2 * pnorm(-abs(co / se))
  )
}

# modelsummary with custom output
modelsummary(
  mods,
  vcov = vcov_cluster,
  
  coef_omit = "^year",
  
  coef_map = c(
    "ownership_categoryCorporation" = "Corporate provider (ref Independent)",
    "Practice_RuralityUrban" = "Urban location (ref Rural)",
    "Dispensing_PracticeYes" = "Dispensing practice (ref No)",
    "imd25" = "IMD score",
    "TOTAL_PATIENTS" = "Registered patients (thousands)"
  ),
  
  # BMJ-style statistics: coefficient + CI + p-value
  estimate = "{estimate}",
  statistic = "({conf.low}, {conf.high}) | p={p.value}",
  
  fmt = 3,
  
  stars = FALSE,
  
  title = "Adjusted regressions with standardized outcomes",
  
  notes = c(
    "Outcomes are standardised (mean = 0, SD = 1).",
    "Robust standard errors clustered at practice level.",
    "Year and local authority fixed effects included but omitted from table."
  ),
  
  output = "gt"
)


####Table 3:event study#####
library(dplyr)
library(tidyr)
library(did)
library(ggplot2)


# Collapse metric rows to one value per practice-year-metric
metrics_wide <- workforce_long %>%
  select(PRACTICE_CODE, year, metric, value) %>%
  group_by(PRACTICE_CODE, year, metric) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from  = metric,
    values_from = value
  )

workforce_raw <-workforce_annual %>% dplyr::select(PRAC_CODE, TOTAL_GP_FTE, TOTAL_NURSES_FTE,TOTAL_PATIENTS, year)%>%
  dplyr::rename(PRACTICE_CODE = PRAC_CODE)

metrics_wide <- merge(metrics_wide, workforce_raw, by=c("year", "PRACTICE_CODE"), all=T)


treatment_timing <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/gp_takeovers/refs/heads/main/Data/takeover_events_all_chain.csv"))%>%
  dplyr::rename(PRACTICE_CODE = pre_practice_code)%>%
  dplyr::mutate(g = takeover_year)




# One ownership value per practice-year
treat_timing <- workforce_long %>%
  select(PRACTICE_CODE, year) %>%
  distinct()  %>%
  arrange(PRACTICE_CODE, year) %>%
  group_by(PRACTICE_CODE) %>%
  summarise(
    first_year = min(year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::left_join(treatment_timing )%>%
  dplyr::select(PRACTICE_CODE, first_year, g)%>%
  dplyr::mutate(g = ifelse(is.na(g), 0 , g))


panel_did_staff <- metrics_wide %>%
  left_join(treat_timing, by = "PRACTICE_CODE") %>%
  mutate(
    practice_id   = as.integer(factor(PRACTICE_CODE)),
    gp_fte_per1k  = scale(TOTAL_GP_FTE_PER1K),
    nurse_per1k   = scale(NURSE_PER1K),
    TOTAL_NURSES_FTE   = scale(TOTAL_NURSES_FTE),
    TOTAL_GP_FTE   = scale(TOTAL_GP_FTE),
    TOTAL_PATIENTS   = scale(TOTAL_PATIENTS)
  ) %>%
  # drop practices first observed after takeover or already treated at first obs
  filter( g == 0 | g > first_year)


run_cs_did <- function(df, outcome, outcome_label, control_group = "nevertreated") {
  
  d <- df %>%
    select(practice_id, PRACTICE_CODE, year, g, all_of(outcome)) %>%
    filter(!is.na(.data[[outcome]]))
  
  att <- att_gt(
    yname   = outcome,
    tname   = "year",
    idname  = "practice_id",
    gname   = "g",
    xformla = ~ 1,
    data    = d,
    panel   = TRUE,
    allow_unbalanced_panel = TRUE,
    control_group = control_group,
    est_method = "reg"
  )
  
  dyn <- aggte(
    att,
    type  = "dynamic",
    min_e = -4,
    max_e = 4,
    na.rm = TRUE
  )
  grp  <- aggte(att, type = "group")
  simp <- aggte(att, type = "simple")
  
  list(
    att  = att,
    dyn  = dyn,
    grp  = grp,
    simp = simp,
    p_att = ggdid(att, title = paste0("Group-time ATT: ", outcome_label)),
    p_dyn = ggdid(dyn, title = paste0("Event study: ", outcome_label)),
    p_grp = ggdid(grp, title = paste0("Cohort effects: ", outcome_label))
  )
}

set.seed(123)

# If you have few never-treated practices, try control_group = "notyettreated"
res_gp <- run_cs_did(panel_did_staff, "gp_fte_per1k",  "GP FTE per 1,000 patients")
res_nu <- run_cs_did(panel_did_staff, "nurse_per1k",   "Nurse FTE per 1,000 patients")
res_nu_raw <- run_cs_did(panel_did_staff, "TOTAL_NURSES_FTE",   "Nurse FTE total")
res_gp_raw <- run_cs_did(panel_did_staff, "TOTAL_GP_FTE",   "GP FTE total")
res_patients <- run_cs_did(panel_did_staff, "TOTAL_PATIENTS",   "patient list")



treatment_timing <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/gp_takeovers/refs/heads/main/Data/takeover_events_all_chain.csv"))%>%
  dplyr::rename(PRACTICE_CODE = pre_practice_code)%>%
  dplyr::mutate(g = takeover_year)




# One ownership value per practice-year
treat_timing <- patient_clean %>%
  select(PRACTICE_CODE, year) %>%
  distinct()  %>%
  arrange(PRACTICE_CODE, year) %>%
  group_by(PRACTICE_CODE) %>%
  summarise(
    first_year = min(year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::left_join(treatment_timing )%>%
  dplyr::select(PRACTICE_CODE, first_year, g)%>%
  dplyr::mutate(g = ifelse(is.na(g), 0 , g))




patient_panel <- patient_clean %>%
  left_join(treat_timing, by = "PRACTICE_CODE") %>%
  select(PRACTICE_CODE, Practice_Name, year, ownership_category,
         appointment_good, overall_good, overlap_days, g, first_year) %>%
  mutate(
    practice_id = as.integer(factor(PRACTICE_CODE)),
    appointment_good = scale(appointment_good),
    overall_good = scale(overall_good)
  ) %>%
  filter(g == 0 | g > first_year)

stopifnot(!any(duplicated(patient_panel[c("practice_id", "year")])))


set.seed(123)

# If you have few never-treated practices, change control_group to "notyettreated"
res_app <- run_cs_did(patient_panel, "appointment_good", "Patient satisfaction with appointments")
res_ovr <- run_cs_did(patient_panel, "overall_good", "Overall practice experience")




treatment_timing <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/gp_takeovers/refs/heads/main/Data/takeover_events_all_chain.csv"))%>%
  dplyr::rename(PRACTICE_CODE = pre_practice_code)%>%
  dplyr::mutate(g = takeover_year)




# One ownership value per practice-year
treat_timing <- qof %>%
  select(PRACTICE_CODE, year) %>%
  distinct()  %>%
  arrange(PRACTICE_CODE, year) %>%
  group_by(PRACTICE_CODE) %>%
  summarise(
    first_year = min(year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::left_join(treatment_timing )%>%
  dplyr::select(PRACTICE_CODE, first_year, g)%>%
  dplyr::mutate(g = ifelse(is.na(g), 0 , g))


panel_did_qof <- qof %>%
  dplyr::select(PRACTICE_CODE, VALUE, year)%>%
  dplyr::rename(qof_points = VALUE)%>%
  left_join(treat_timing, by = "PRACTICE_CODE") %>%
  # drop practices first observed after takeover or already treated at first obs
  filter( g == 0 | g > first_year)%>%
  dplyr::mutate(    practice_id = as.integer(factor(PRACTICE_CODE)),
                    qof_points = scale(qof_points))



set.seed(123)

# If you have few never-treated practices, try control_group = "notyettreated"
res_qof <- run_cs_did(panel_did_qof, "qof_points",  "QOF points %")


library(dplyr)
library(purrr)
library(gt)

extract_att <- function(res, outcome){
  
  d <- res$att$DIDparams$data
  
  tibble(
    Outcome = outcome,
    ATT = res$simp$overall.att,
    SE = res$simp$overall.se,
    `95% CI` = sprintf("%.3f to %.3f",
                       res$simp$overall.att - 1.96*res$simp$overall.se,
                       res$simp$overall.att + 1.96*res$simp$overall.se),
    `P value` = 2*pnorm(-abs(res$simp$overall.att/res$simp$overall.se)),
    Practices = n_distinct(d$practice_id),
    `Takeover events` = n_distinct(d$practice_id[d$g!=Inf]),
    Observations = nrow(d)
  )
}

results_table <-
  bind_rows(
    extract_att(res_gp,        "GP FTE per 1,000 patients"),
    extract_att(res_nu,        "Nurse FTE per 1,000 patients"),
    extract_att(res_qof,    "QOF points %"),
    extract_att(res_app,    "Patient satisfaction with appointments"),
    extract_att(res_ovr,  "Patient satisfaction overall")
  ) %>%
  mutate(
    ATT = sprintf("%.3f", ATT),
    `P value` = ifelse(`P value` < 0.001, "<0.001",
                       sprintf("%.3f", `P value`))
  )

results_table %>%
  gt() %>%
  cols_label(
    Outcome = "Outcome",
    ATT = "ATT",
    `95% CI` = "95% CI",
    `P value` = "P",
    Practices = "Practices",
    `Takeover events` = "Takeovers",
    Observations = "Practice-years"
  )







treatment_timing <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/gp_takeovers/refs/heads/main/Data/takeover_events_all_chain.csv"))%>%
  dplyr::rename(PRACTICE_CODE = pre_practice_code)%>%
  dplyr::mutate(g = takeover_year)




# One ownership value per practice-year
treat_timing <- pay %>%
  dplyr::rename(PRACTICE_CODE = Practice_Code)%>%
  select(PRACTICE_CODE, year) %>%
  distinct()  %>%
  arrange(PRACTICE_CODE, year) %>%
  group_by(PRACTICE_CODE) %>%
  summarise(
    first_year = min(year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::left_join(treatment_timing )%>%
  dplyr::select(PRACTICE_CODE, first_year, g)%>%
  dplyr::mutate(g = ifelse(is.na(g), 0 , g))


panel_did <- master_df %>%
  dplyr::rename(PRACTICE_CODE = PRAC_CODE)%>%
  dplyr::select(PRACTICE_CODE, Total_NHS_Payments_to_General_Practice_Minus_Deductions, year)%>%
  dplyr::rename(nhs_funding = Total_NHS_Payments_to_General_Practice_Minus_Deductions)%>%
  left_join(treat_timing, by = "PRACTICE_CODE") %>%
  # drop practices first observed after takeover or already treated at first obs
  filter( g == 0 | g > first_year)%>%
  dplyr::mutate(    practice_id = as.integer(factor(PRACTICE_CODE)),
                    nhs_funding = scale(nhs_funding))


panel_did <- panel_did %>%
  distinct(.keep_all = T)


set.seed(123)

# If you have few never-treated practices, try control_group = "notyettreated"
res_spend <- run_cs_did(panel_did, "nhs_funding",  "NHS funding")





####Figure 3####
library(dplyr)
library(ggplot2)
library(patchwork)

# helper: extract ggdid dynamic objects
tidy_did_dyn <- function(dyn_obj, label){
  
  df <- data.frame(
    event_time = dyn_obj$egt,
    estimate   = dyn_obj$att.egt,
    se         = dyn_obj$se.egt
  ) %>%
    mutate(
      ci_low  = estimate - 1.96 * se,
      ci_high = estimate + 1.96 * se,
      outcome = label
    )
  
  df
}

cost_df   <- tidy_did_dyn(res_spend$dyn, "NHS funding (z-score)")
qual_df   <- tidy_did_dyn(res_ovr$dyn,   "Patient experience (z-score)")



theme_bmj <- function(){
  theme_classic(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      panel.grid.major.y = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )
}


p_cost <- ggplot(cost_df, aes(x = event_time, y = estimate)) +
  
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high),
              fill = "#d73027", alpha = 0.15) +
  
  geom_line(linewidth = 1.1, colour = "#d73027") +
  
  geom_vline(xintercept = 0, linewidth = 0.8) +
  
  labs(
    title = "A) Cost to NHS increases after takeover",
    x = "Years relative to takeover",
    y = "Effect (SD units)"
  ) +
  
  theme_bmj()


p_quality <- ggplot(qual_df, aes(x = event_time, y = estimate)) +
  
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high),
              fill = "#4575b4", alpha = 0.15) +
  
  geom_line(linewidth = 1.1, colour = "#4575b4") +
  
  geom_vline(xintercept = 0, linewidth = 0.8) +
  
  labs(
    title = "B) Patient experience declines after takeover",
    x = "Years relative to takeover",
    y = "Effect (SD units)"
  ) +
  
  theme_bmj()


final_figure <- p_cost / p_quality +
  plot_annotation(
    title = "Impact of corporate takeover on general practice",
    subtitle = "Event-study estimates (Callaway & Sant’Anna; 95% confidence intervals)",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12)
    )
  )

final_figure


####dynamic figure####
# helper: extract ggdid dynamic objects
tidy_did_dyn <- function(dyn_obj, label){
  
  df <- data.frame(
    event_time = dyn_obj$egt,
    estimate   = dyn_obj$att.egt,
    se         = dyn_obj$se.egt
  ) %>%
    mutate(
      ci_low  = estimate - 1.96 * se,
      ci_high = estimate + 1.96 * se,
      outcome = label
    )
  
  df
}

gp   <- tidy_did_dyn(res_gp$dyn, "GP FTE (z-score)")
nurse   <- tidy_did_dyn(res_nu$dyn, "Nurse FTW (z-score)")
qof   <- tidy_did_dyn(res_qof$dyn, "QOF (z-score)")
overall   <- tidy_did_dyn(res_ovr$dyn,   "Overall patient experience (z-score)")
appoint   <- tidy_did_dyn(res_app$dyn,   "Appointment patient experience (z-score)")



theme_bmj <- function(){
  theme_classic(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      panel.grid.major.y = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )
}


p_gp <- ggplot(gp, aes(x = event_time, y = estimate)) +
  
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high),
              fill = "#d73027", alpha = 0.15) +
  
  geom_line(linewidth = 1.1, colour = "#d73027") +
  
  geom_vline(xintercept = 0, linewidth = 0.8) +
  
  labs(
    title = "A) GP FTE",
    x = "Years relative to takeover",
    y = "Effect (SD units)"
  ) +
  
  theme_bmj()


p_nurse <- ggplot(nurse, aes(x = event_time, y = estimate)) +
  
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high),
              fill = "#d73027", alpha = 0.15) +
  
  geom_line(linewidth = 1.1, colour = "#d73027") +
  
  geom_vline(xintercept = 0, linewidth = 0.8) +
  
  labs(
    title = "B) Nurse FTE",
    x = "Years relative to takeover",
    y = "Effect (SD units)"
  ) +
  
  theme_bmj()

p_qof <- ggplot(qof, aes(x = event_time, y = estimate)) +
  
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high),
              fill = "#d73027", alpha = 0.15) +
  
  geom_line(linewidth = 1.1, colour = "#d73027") +
  
  geom_vline(xintercept = 0, linewidth = 0.8) +
  
  labs(
    title = "C) QOF",
    x = "Years relative to takeover",
    y = "Effect (SD units)"
  ) +
  
  theme_bmj()

p_overall <- ggplot(overall, aes(x = event_time, y = estimate)) +
  
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high),
              fill = "#d73027", alpha = 0.15) +
  
  geom_line(linewidth = 1.1, colour = "#d73027") +
  
  geom_vline(xintercept = 0, linewidth = 0.8) +
  
  labs(
    title = "D) Overall satisfaction",
    x = "Years relative to takeover",
    y = "Effect (SD units)"
  ) +
  
  theme_bmj()

p_app <- ggplot(appoint, aes(x = event_time, y = estimate)) +
  
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high),
              fill = "#d73027", alpha = 0.15) +
  
  geom_line(linewidth = 1.1, colour = "#d73027") +
  
  geom_vline(xintercept = 0, linewidth = 0.8) +
  
  labs(
    title = "E) Appointment satisfaction",
    x = "Years relative to takeover",
    y = "Effect (SD units)"
  ) +
  
  theme_bmj()


final_figure <- p_gp / p_nurse/p_qof /p_overall /p_app +
  plot_annotation(
    title = "Impact of corporate takeover on general practice",
    subtitle = "Event-study estimates (Callaway & Sant’Anna; 95% confidence intervals)",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12)
    )
  )

final_figure







####spec curve####


library(dplyr)
library(purrr)
library(tidyr)
library(sandwich)
library(lmtest)
library(ggplot2)
# install.packages("patchwork") # if not already installed
library(patchwork)

candidate_controls <- c(
  "qof_composite",
  "life_expectancy",
  "imd25",
  "Dispensing_Practice",
  "Practice_Rurality",
  "TOTAL_PATIENTS"
)

fe_blocks <- c("year", "locationlocalauthority")

toggle_vars <- c(candidate_controls, fe_blocks)

spec_grid <- expand.grid(
  setNames(rep(list(c(TRUE, FALSE)), length(toggle_vars)), toggle_vars)
) %>% as_tibble()

cat(sprintf("Specifications per outcome: %d\n", nrow(spec_grid)))
cat(sprintf("Outcomes: 5   |   Total models to fit: %d\n\n", nrow(spec_grid) * 5))

# time one "full controls" model + clustered vcov, to estimate total runtime
test_formula <- as.formula(
  paste("z_TOTAL_GP_FTE_PER1K ~ ownership_category +", paste(toggle_vars, collapse = " + "))
)
test_timing <- system.time({
  fit_test <- lm(test_formula, data = df)
  used_rows_test <- as.numeric(rownames(model.frame(fit_test)))
  sandwich::vcovCL(fit_test, cluster = df$PRAC_CODE[used_rows_test])
})
cat("Time for one full-controls model + clustered vcov (seconds):\n")
print(test_timing)
cat(sprintf("\nRough estimate for full grid: %.1f minutes (varies a lot by spec size)\n\n",
            test_timing[["elapsed"]] * nrow(spec_grid) * 5 / 60))
cat("If this looks too slow, see the commented-out furrr parallel block near the bottom.\n\n")


build_formula <- function(y, spec_row) {
  included <- toggle_vars[unlist(spec_row[toggle_vars]) == TRUE]
  rhs <- c("ownership_category", included)
  as.formula(paste(y, "~", paste(rhs, collapse = " + ")))
}

run_one_spec <- function(y, spec_row, spec_id, data) {
  
  f <- build_formula(y, spec_row)
  
  fit <- tryCatch(lm(f, data = data), error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  
  co <- coef(fit)
  own_terms <- grep("^ownership_category", names(co), value = TRUE)
  if (length(own_terms) == 0) return(NULL)
  
  # Build the cluster vector explicitly (aligned to the rows lm() actually used,
  # since na.action drops incomplete rows) rather than using vcovCL's formula
  # interface, which relies on an environment lookup that breaks when the
  # formula is built inside a helper function like build_formula().
  used_rows <- as.numeric(rownames(model.frame(fit)))
  cluster_vec <- data$PRAC_CODE[used_rows]
  
  vc <- tryCatch(sandwich::vcovCL(fit, cluster = cluster_vec), error = function(e) NULL)
  if (is.null(vc)) return(NULL)
  se_all <- sqrt(diag(vc))
  
  map_dfr(own_terms, function(term_name) {
    est <- co[[term_name]]
    se  <- se_all[[term_name]]
    tibble(
      spec_id  = spec_id,
      term     = term_name,
      estimate = est,
      se       = se,
      ci_low   = est - 1.96 * se,
      ci_high  = est + 1.96 * se,
      p_value  = 2 * pnorm(-abs(est / se)),
      n_obs    = stats::nobs(fit)
    )
  }) %>%
    bind_cols(spec_row)
}

run_spec_curve <- function(data, y, y_label) {
  message(sprintf("Running specification curve for: %s ...", y_label))
  map_dfr(seq_len(nrow(spec_grid)), function(i) {
    run_one_spec(y, spec_grid[i, ], spec_id = i, data = data)
  }) %>%
    mutate(outcome = y_label)
}


outcomes <- list(
  list(y = "z_TOTAL_GP_FTE_PER1K", label = "GP FTE per 1,000"),
  list(y = "z_NURSE_PER1K",        label = "Nurses per 1,000"),
  list(y = "z_QOF_points",         label = "QOF points"),
  list(y = "z_overall_good",       label = "Overall good"),
  list(y = "z_appointment_good",   label = "Appointment good")
)

set.seed(123)
spec_results <- map2_dfr(
  map_chr(outcomes, "y"),
  map_chr(outcomes, "label"),
  ~ run_spec_curve(df, .x, .y)
)

cat(sprintf("\nSuccessfully estimated %d / %d possible specification x outcome combinations\n",
            nrow(spec_results), nrow(spec_grid) * length(outcomes)))


spec_summary <- spec_results %>%
  group_by(outcome, term) %>%
  summarise(
    n_specs          = n(),
    median_estimate  = median(estimate, na.rm = TRUE),
    pct_positive     = round(mean(estimate > 0, na.rm = TRUE) * 100, 1),
    pct_sig_positive = round(mean(estimate > 0 & p_value < 0.05, na.rm = TRUE) * 100, 1),
    pct_sig_negative = round(mean(estimate < 0 & p_value < 0.05, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )

cat("\n=== Specification curve summary ===\n")
print(spec_summary)


control_labels <- c(
  qof_composite          = "QOF composite score",
  life_expectancy        = "Life expectancy",
  imd25                  = "IMD deprivation score",
  Dispensing_Practice    = "Dispensing practice",
  Practice_Rurality      = "Urban/rural",
  TOTAL_PATIENTS         = "List size (1,000s)",
  year                   = "Year fixed effects",
  locationlocalauthority = "Local authority fixed effects"
)

plot_spec_curve <- function(res, title_label) {
  
  res <- res %>%
    arrange(estimate) %>%
    mutate(
      spec_rank = row_number(),
      sig = p_value < 0.05
    )
  
  p_top <- ggplot(res, aes(x = spec_rank, y = estimate, ymin = ci_low, ymax = ci_high, colour = sig)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_pointrange(size = 0.15, fatten = 1) +
    scale_colour_manual(values = c(`TRUE` = "firebrick", `FALSE` = "grey40"),
                        labels = c(`TRUE` = "p < .05", `FALSE` = "n.s."),
                        name = NULL) +
    labs(y = "Effect of corporate ownership\n(vs Independent)", x = NULL, title = title_label) +
    theme_minimal(base_size = 11) +
    coord_cartesian(ylim=c(-0.8,0.8))+
    theme(legend.position = "top",
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank())
  
  control_df <- res %>%
    select(spec_rank, all_of(toggle_vars)) %>%
    pivot_longer(-spec_rank, names_to = "control", values_to = "included") %>%
    mutate(control = recode(control, !!!control_labels))
  
  p_bottom <- ggplot(control_df, aes(x = spec_rank, y = control)) +
    geom_tile(aes(alpha = included), fill = "steelblue4") +
    scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.08), guide = "none") +
    labs(x = "Specs ranked by effect size", y = NULL) +
    theme_minimal(base_size = 10) +
    theme(panel.grid = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  
  p_top / p_bottom + plot_layout(heights = c(2, 1))
}

spec_plots <- list()

for (o in outcomes) {
  res_o <- spec_results %>% filter(outcome == o$label)
  
  for (trm in unique(res_o$term)) {
    res_ot <- res_o %>% filter(term == trm)
    plt <- plot_spec_curve(res_ot, paste0(o$label))
    spec_plots[[paste(o$label, trm)]] <- plt
    print(plt)
  }
}



wrap_plots(
  spec_plots$`GP FTE per 1,000 ownership_categoryCorporation`,
  spec_plots$`Nurses per 1,000 ownership_categoryCorporation`,
  spec_plots$`QOF points ownership_categoryCorporation`,
  spec_plots$`Overall good ownership_categoryCorporation`,
  spec_plots$`Appointment good ownership_categoryCorporation`,
  ncol = 5
)


####raw outcome values cross sectional####



df <- master_df %>%
  mutate(
    ownership_category = relevel(factor(ownership_category), ref = "Independent"),
    year = factor(year),
    TOTAL_PATIENTS = TOTAL_PATIENTS/1000
  )

library(ordinal)
mods <- list(
  "GP FTE per 1,000" = lm(TOTAL_GP_FTE_PER1K ~ ownership_category  + qof_composite+life_expectancy + imd25 +Dispensing_Practice+ Practice_Rurality+ TOTAL_PATIENTS + year+locationlocalauthority,
                          data = df),
  "Nurses per 1,000" = lm(NURSE_PER1K ~ ownership_category  + qof_composite+life_expectancy + imd25 +Dispensing_Practice+ Practice_Rurality+ TOTAL_PATIENTS + year+locationlocalauthority,
                          data = df),
  "QOF points" = lm(QOF_points ~ ownership_category  + qof_composite+Dispensing_Practice+ Practice_Rurality+ life_expectancy + imd25 + TOTAL_PATIENTS + year+locationlocalauthority,
                    data = df),
  "Overall good" = lm(overall_good ~ ownership_category  + qof_composite+Dispensing_Practice+ Practice_Rurality+ life_expectancy + imd25 + TOTAL_PATIENTS + year+locationlocalauthority,
                      data = df),
  "Appointment good" = lm(appointment_good ~ ownership_category  + qof_composite +Dispensing_Practice+ Practice_Rurality+ life_expectancy+ imd25 + TOTAL_PATIENTS + year+locationlocalauthority,
                          data = df)
)

library(modelsummary)
library(sandwich)
library(lmtest)
library(dplyr)

# Clustered VCOV function
vcov_cluster <- function(model) {
  sandwich::vcovCL(model, cluster = ~PRAC_CODE)
}

# Custom coefficient table function (adds CI + p-values)
tidy_bmj <- function(model) {
  co <- coef(model)
  vc <- vcov_cluster(model)
  se <- sqrt(diag(vc))
  
  tibble(
    term = names(co),
    estimate = co,
    se = se,
    conf.low = co - 1.96 * se,
    conf.high = co + 1.96 * se,
    p.value = 2 * pnorm(-abs(co / se))
  )
}

# modelsummary with custom output
modelsummary(
  mods,
  vcov = vcov_cluster,
  
  coef_omit = "^year",
  
  coef_map = c(
    "ownership_categoryCorporation" = "Corporate provider (ref Independent)",
    "Practice_RuralityUrban" = "Urban location (ref Rural)",
    "Dispensing_PracticeYes" = "Dispensing practice (ref No)",
    "imd25" = "IMD score",
    "TOTAL_PATIENTS" = "Registered patients (thousands)"
  ),
  
  # BMJ-style statistics: coefficient + CI + p-value
  estimate = "{estimate}",
  statistic = "({conf.low}, {conf.high}) | p={p.value}",
  
  fmt = 3,
  
  stars = FALSE,
  
  title = "Adjusted regressions with raw outcomes",
  
  notes = c(
    "Outcomes are standardised (mean = 0, SD = 1).",
    "Robust standard errors clustered at practice level.",
    "Year and local authority fixed effects included but omitted from table."
  ),
  
  output = "gt"
)


####raw outcome values takeover####


library(dplyr)
library(tidyr)
library(did)
library(ggplot2)


# Collapse metric rows to one value per practice-year-metric
metrics_wide <- workforce_long %>%
  select(PRACTICE_CODE, year, metric, value) %>%
  group_by(PRACTICE_CODE, year, metric) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from  = metric,
    values_from = value
  )

workforce_raw <-workforce_annual %>% dplyr::select(PRAC_CODE, TOTAL_GP_FTE, TOTAL_NURSES_FTE,TOTAL_PATIENTS, year)%>%
  dplyr::rename(PRACTICE_CODE = PRAC_CODE)

metrics_wide <- merge(metrics_wide, workforce_raw, by=c("year", "PRACTICE_CODE"), all=T)


treatment_timing <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/gp_takeovers/refs/heads/main/Data/takeover_events_all_chain.csv"))%>%
  dplyr::rename(PRACTICE_CODE = pre_practice_code)%>%
  dplyr::mutate(g = takeover_year)




# One ownership value per practice-year
treat_timing <- workforce_long %>%
  select(PRACTICE_CODE, year) %>%
  distinct()  %>%
  arrange(PRACTICE_CODE, year) %>%
  group_by(PRACTICE_CODE) %>%
  summarise(
    first_year = min(year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::left_join(treatment_timing )%>%
  dplyr::select(PRACTICE_CODE, first_year, g)%>%
  dplyr::mutate(g = ifelse(is.na(g), 0 , g))


panel_did_staff <- metrics_wide %>%
  left_join(treat_timing, by = "PRACTICE_CODE") %>%
  mutate(
    practice_id   = as.integer(factor(PRACTICE_CODE)),
    gp_fte_per1k  = (TOTAL_GP_FTE_PER1K),
    nurse_per1k   = (NURSE_PER1K),
    TOTAL_NURSES_FTE   = (TOTAL_NURSES_FTE),
    TOTAL_GP_FTE   = (TOTAL_GP_FTE),
    TOTAL_PATIENTS   = (TOTAL_PATIENTS)
  ) %>%
  # drop practices first observed after takeover or already treated at first obs
  filter( g == 0 | g > first_year)


run_cs_did <- function(df, outcome, outcome_label, control_group = "nevertreated") {
  
  d <- df %>%
    select(practice_id, PRACTICE_CODE, year, g, all_of(outcome)) %>%
    filter(!is.na(.data[[outcome]]))
  
  att <- att_gt(
    yname   = outcome,
    tname   = "year",
    idname  = "practice_id",
    gname   = "g",
    xformla = ~ 1,
    data    = d,
    panel   = TRUE,
    allow_unbalanced_panel = TRUE,
    control_group = control_group,
    est_method = "reg"
  )
  
  dyn <- aggte(
    att,
    type  = "dynamic",
    min_e = -4,
    max_e = 4,
    na.rm = TRUE
  )
  grp  <- aggte(att, type = "group")
  simp <- aggte(att, type = "simple")
  
  list(
    att  = att,
    dyn  = dyn,
    grp  = grp,
    simp = simp,
    p_att = ggdid(att, title = paste0("Group-time ATT: ", outcome_label)),
    p_dyn = ggdid(dyn, title = paste0("Event study: ", outcome_label)),
    p_grp = ggdid(grp, title = paste0("Cohort effects: ", outcome_label))
  )
}

set.seed(123)

# If you have few never-treated practices, try control_group = "notyettreated"
res_gp <- run_cs_did(panel_did_staff, "gp_fte_per1k",  "GP FTE per 1,000 patients")
res_nu <- run_cs_did(panel_did_staff, "nurse_per1k",   "Nurse FTE per 1,000 patients")
res_nu_raw <- run_cs_did(panel_did_staff, "TOTAL_NURSES_FTE",   "Nurse FTE total")
res_gp_raw <- run_cs_did(panel_did_staff, "TOTAL_GP_FTE",   "GP FTE total")
res_patients <- run_cs_did(panel_did_staff, "TOTAL_PATIENTS",   "patient list")





treatment_timing <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/gp_takeovers/refs/heads/main/Data/takeover_events_all_chain.csv"))%>%
  dplyr::rename(PRACTICE_CODE = pre_practice_code)%>%
  dplyr::mutate(g = takeover_year)




# One ownership value per practice-year
treat_timing <- patient_clean %>%
  select(PRACTICE_CODE, year) %>%
  distinct()  %>%
  arrange(PRACTICE_CODE, year) %>%
  group_by(PRACTICE_CODE) %>%
  summarise(
    first_year = min(year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::left_join(treatment_timing )%>%
  dplyr::select(PRACTICE_CODE, first_year, g)%>%
  dplyr::mutate(g = ifelse(is.na(g), 0 , g))




patient_panel <- patient_clean %>%
  left_join(treat_timing, by = "PRACTICE_CODE") %>%
  select(PRACTICE_CODE, Practice_Name, year, ownership_category,
         appointment_good, overall_good, overlap_days, g, first_year) %>%
  mutate(
    practice_id = as.integer(factor(PRACTICE_CODE)),
    appointment_good = (appointment_good),
    overall_good = (overall_good)
  ) %>%
  filter(g == 0 | g > first_year)

stopifnot(!any(duplicated(patient_panel[c("practice_id", "year")])))


set.seed(123)

# If you have few never-treated practices, change control_group to "notyettreated"
res_app <- run_cs_did(patient_panel, "appointment_good", "Patient satisfaction with appointments")
res_ovr <- run_cs_did(patient_panel, "overall_good", "Overall practice experience")




treatment_timing <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/gp_takeovers/refs/heads/main/Data/takeover_events_all_chain.csv"))%>%
  dplyr::rename(PRACTICE_CODE = pre_practice_code)%>%
  dplyr::mutate(g = takeover_year)




# One ownership value per practice-year
treat_timing <- qof %>%
  select(PRACTICE_CODE, year) %>%
  distinct()  %>%
  arrange(PRACTICE_CODE, year) %>%
  group_by(PRACTICE_CODE) %>%
  summarise(
    first_year = min(year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::left_join(treatment_timing )%>%
  dplyr::select(PRACTICE_CODE, first_year, g)%>%
  dplyr::mutate(g = ifelse(is.na(g), 0 , g))


panel_did_qof <- qof %>%
  dplyr::select(PRACTICE_CODE, VALUE, year)%>%
  dplyr::rename(qof_points = VALUE)%>%
  left_join(treat_timing, by = "PRACTICE_CODE") %>%
  # drop practices first observed after takeover or already treated at first obs
  filter( g == 0 | g > first_year)%>%
  dplyr::mutate(    practice_id = as.integer(factor(PRACTICE_CODE)),
                    qof_points = (qof_points))



set.seed(123)

# If you have few never-treated practices, try control_group = "notyettreated"
res_qof <- run_cs_did(panel_did_qof, "qof_points",  "QOF points %")




library(dplyr)
library(purrr)
library(gt)

extract_att <- function(res, outcome){
  
  d <- res$att$DIDparams$data
  
  tibble(
    Outcome = outcome,
    ATT = res$simp$overall.att,
    SE = res$simp$overall.se,
    `95% CI` = sprintf("%.3f to %.3f",
                       res$simp$overall.att - 1.96*res$simp$overall.se,
                       res$simp$overall.att + 1.96*res$simp$overall.se),
    `P value` = 2*pnorm(-abs(res$simp$overall.att/res$simp$overall.se)),
    Practices = n_distinct(d$practice_id),
    `Takeover events` = n_distinct(d$practice_id[d$g!=Inf]),
    Observations = nrow(d)
  )
}

results_table <-
  bind_rows(
    extract_att(res_gp,        "GP FTE per 1,000 patients"),
    extract_att(res_nu,        "Nurse FTE per 1,000 patients"),
    extract_att(res_qof,    "QOF points %"),
    extract_att(res_app,    "Patient satisfaction with appointments"),
    extract_att(res_ovr,  "Patient satisfaction overall")
  ) %>%
  mutate(
    ATT = sprintf("%.3f", ATT),
    `P value` = ifelse(`P value` < 0.001, "<0.001",
                       sprintf("%.3f", `P value`))
  )

results_table %>%
  gt() %>%
  cols_label(
    Outcome = "Outcome",
    ATT = "ATT",
    `95% CI` = "95% CI",
    `P value` = "P",
    Practices = "Practices",
    `Takeover events` = "Takeovers",
    Observations = "Practice-years"
  )







treatment_timing <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/gp_takeovers/refs/heads/main/Data/takeover_events_all_chain.csv"))%>%
  dplyr::rename(PRACTICE_CODE = pre_practice_code)%>%
  dplyr::mutate(g = takeover_year)




# One ownership value per practice-year
treat_timing <- pay %>%
  dplyr::rename(PRACTICE_CODE = Practice_Code)%>%
  select(PRACTICE_CODE, year) %>%
  distinct()  %>%
  arrange(PRACTICE_CODE, year) %>%
  group_by(PRACTICE_CODE) %>%
  summarise(
    first_year = min(year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::left_join(treatment_timing )%>%
  dplyr::select(PRACTICE_CODE, first_year, g)%>%
  dplyr::mutate(g = ifelse(is.na(g), 0 , g))


panel_did <- master_df %>%
  dplyr::rename(PRACTICE_CODE = PRAC_CODE)%>%
  dplyr::select(PRACTICE_CODE, Total_NHS_Payments_to_General_Practice_Minus_Deductions, year)%>%
  dplyr::rename(nhs_funding = Total_NHS_Payments_to_General_Practice_Minus_Deductions)%>%
  left_join(treat_timing, by = "PRACTICE_CODE") %>%
  # drop practices first observed after takeover or already treated at first obs
  filter( g == 0 | g > first_year)%>%
  dplyr::mutate(    practice_id = as.integer(factor(PRACTICE_CODE)),
                    nhs_funding = scale(nhs_funding))


panel_did <- panel_did %>%
  distinct(.keep_all = T)


run_cs_did <- function(df, outcome, outcome_label, control_group = "nevertreated") {
  
  d <- df %>%
    select(practice_id, PRACTICE_CODE, year, g, all_of(outcome)) %>%
    filter(!is.na(.data[[outcome]]))
  
  att <- att_gt(
    yname   = outcome,
    tname   = "year",
    idname  = "practice_id",
    gname   = "g",
    xformla = ~ 1,
    data    = d,
    panel   = TRUE,
    allow_unbalanced_panel = TRUE,
    control_group = control_group,
    est_method = "reg"
  )
  
  dyn <- aggte(
    att,
    type  = "dynamic",
    min_e = -4,
    max_e = 4,
    na.rm = TRUE
  )
  grp  <- aggte(att, type = "group",  na.rm = TRUE)
  simp <- aggte(att, type = "simple", na.rm = TRUE)
  
  list(
    att  = att,
    dyn  = dyn,
    grp  = grp,
    simp = simp,
    p_att = ggdid(att, title = paste0("Group-time ATT: ", outcome_label)),
    p_dyn = ggdid(dyn, title = paste0("Event study: ", outcome_label)),
    p_grp = ggdid(grp, title = paste0("Cohort effects: ", outcome_label))
  )
}

set.seed(123)

# If you have few never-treated practices, try control_group = "notyettreated"
res_spend <- run_cs_did(panel_did, "nhs_funding",  "NHS funding")



####nhs spend models ####



treatment_timing <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/gp_takeovers/refs/heads/main/Data/takeover_events_all_chain.csv"))%>%
  dplyr::rename(PRACTICE_CODE = pre_practice_code)%>%
  dplyr::mutate(g = takeover_year)




# One ownership value per practice-year
treat_timing <- pay %>%
  dplyr::rename(PRACTICE_CODE = Practice_Code)%>%
  select(PRACTICE_CODE, year) %>%
  distinct()  %>%
  arrange(PRACTICE_CODE, year) %>%
  group_by(PRACTICE_CODE) %>%
  summarise(
    first_year = min(year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::left_join(treatment_timing )%>%
  dplyr::select(PRACTICE_CODE, first_year, g)%>%
  dplyr::mutate(g = ifelse(is.na(g), 0 , g))


panel_did <- master_df %>%
  dplyr::rename(PRACTICE_CODE = PRAC_CODE)%>%
  dplyr::select(PRACTICE_CODE, Total_NHS_Payments_to_General_Practice_Minus_Deductions, year)%>%
  dplyr::rename(nhs_funding = Total_NHS_Payments_to_General_Practice_Minus_Deductions)%>%
  left_join(treat_timing, by = "PRACTICE_CODE") %>%
  # drop practices first observed after takeover or already treated at first obs
  filter( g == 0 | g > first_year)%>%
  dplyr::mutate(    practice_id = as.integer(factor(PRACTICE_CODE)),
                    nhs_funding_z = scale(nhs_funding))


panel_did <- panel_did %>%
  distinct(.keep_all = T)


run_cs_did <- function(df, outcome, outcome_label, control_group = "nevertreated") {
  
  d <- df %>%
    select(practice_id, PRACTICE_CODE, year, g, all_of(outcome)) %>%
    filter(!is.na(.data[[outcome]]))
  
  att <- att_gt(
    yname   = outcome,
    tname   = "year",
    idname  = "practice_id",
    gname   = "g",
    xformla = ~ 1,
    data    = d,
    panel   = TRUE,
    allow_unbalanced_panel = TRUE,
    control_group = control_group,
    est_method = "reg"
  )
  
  dyn <- aggte(
    att,
    type  = "dynamic",
    min_e = -4,
    max_e = 4,
    na.rm = TRUE
  )
  grp  <- aggte(att, type = "group",  na.rm = TRUE)
  simp <- aggte(att, type = "simple", na.rm = TRUE)
  
  list(
    att  = att,
    dyn  = dyn,
    grp  = grp,
    simp = simp,
    p_att = ggdid(att, title = paste0("Group-time ATT: ", outcome_label)),
    p_dyn = ggdid(dyn, title = paste0("Event study: ", outcome_label)),
    p_grp = ggdid(grp, title = paste0("Cohort effects: ", outcome_label))
  )
}

set.seed(123)

# If you have few never-treated practices, try control_group = "notyettreated"
res_spend <- run_cs_did(panel_did, "nhs_funding",  "NHS funding")
res_spend_z <- run_cs_did(panel_did, "nhs_funding_z",  "NHS funding (standardised)")



extract_att <- function(res, outcome){
  
  d <- res$att$DIDparams$data
  
  tibble(
    Outcome = outcome,
    ATT = res$simp$overall.att,
    SE = res$simp$overall.se,
    `95% CI` = sprintf("%.3f to %.3f",
                       res$simp$overall.att - 1.96*res$simp$overall.se,
                       res$simp$overall.att + 1.96*res$simp$overall.se),
    `P value` = 2*pnorm(-abs(res$simp$overall.att/res$simp$overall.se)),
    Practices = n_distinct(d$practice_id),
    `Takeover events` = n_distinct(d$practice_id[d$g!=Inf]),
    Observations = nrow(d)
  )
}

results_table <-
  bind_rows(
    extract_att(res_spend,        "NHS funding received"),
    extract_att(res_spend_z,        "NHS funding received (standardised)")
  ) %>%
  mutate(
    ATT = sprintf("%.3f", ATT),
    `P value` = ifelse(`P value` < 0.001, "<0.001",
                       sprintf("%.3f", `P value`))
  )

results_table %>%
  gt() %>%
  cols_label(
    Outcome = "Outcome",
    ATT = "ATT",
    `95% CI` = "95% CI",
    `P value` = "P",
    Practices = "Practices",
    `Takeover events` = "Takeovers",
    Observations = "Practice-years"
  )


####mechanims models####




# Collapse metric rows to one value per practice-year-metric
metrics_wide <- workforce_long %>%
  select(PRACTICE_CODE, year, metric, value) %>%
  group_by(PRACTICE_CODE, year, metric) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from  = metric,
    values_from = value
  )

workforce_raw <-workforce_annual %>% dplyr::select(PRAC_CODE, TOTAL_GP_FTE, TOTAL_NURSES_FTE,TOTAL_PATIENTS, year)%>%
  dplyr::rename(PRACTICE_CODE = PRAC_CODE)

metrics_wide <- merge(metrics_wide, workforce_raw, by=c("year", "PRACTICE_CODE"), all=T)


treatment_timing <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/gp_takeovers/refs/heads/main/Data/takeover_events_all_chain.csv"))%>%
  dplyr::rename(PRACTICE_CODE = pre_practice_code)%>%
  dplyr::mutate(g = takeover_year)




# One ownership value per practice-year
treat_timing <- workforce_long %>%
  select(PRACTICE_CODE, year) %>%
  distinct()  %>%
  arrange(PRACTICE_CODE, year) %>%
  group_by(PRACTICE_CODE) %>%
  summarise(
    first_year = min(year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::left_join(treatment_timing )%>%
  dplyr::select(PRACTICE_CODE, first_year, g)%>%
  dplyr::mutate(g = ifelse(is.na(g), 0 , g))


panel_did_staff <- metrics_wide %>%
  left_join(treat_timing, by = "PRACTICE_CODE") %>%
  mutate(
    practice_id   = as.integer(factor(PRACTICE_CODE)),
    gp_fte_per1k  = scale(TOTAL_GP_FTE_PER1K),
    nurse_per1k   = scale(NURSE_PER1K),
    TOTAL_NURSES_FTE   = scale(TOTAL_NURSES_FTE),
    TOTAL_GP_FTE   = scale(TOTAL_GP_FTE),
    TOTAL_PATIENTS   = scale(TOTAL_PATIENTS)
  ) %>%
  # drop practices first observed after takeover or already treated at first obs
  filter( g == 0 | g > first_year)


run_cs_did <- function(df, outcome, outcome_label, control_group = "nevertreated") {
  
  d <- df %>%
    select(practice_id, PRACTICE_CODE, year, g, all_of(outcome)) %>%
    filter(!is.na(.data[[outcome]]))
  
  att <- att_gt(
    yname   = outcome,
    tname   = "year",
    idname  = "practice_id",
    gname   = "g",
    xformla = ~ 1,
    data    = d,
    panel   = TRUE,
    allow_unbalanced_panel = TRUE,
    control_group = control_group,
    est_method = "reg"
  )
  
  dyn <- aggte(
    att,
    type  = "dynamic",
    min_e = -4,
    max_e = 4,
    na.rm = TRUE
  )
  grp  <- aggte(att, type = "group")
  simp <- aggte(att, type = "simple")
  
  list(
    att  = att,
    dyn  = dyn,
    grp  = grp,
    simp = simp,
    p_att = ggdid(att, title = paste0("Group-time ATT: ", outcome_label)),
    p_dyn = ggdid(dyn, title = paste0("Event study: ", outcome_label)),
    p_grp = ggdid(grp, title = paste0("Cohort effects: ", outcome_label))
  )
}

set.seed(123)


res_nu_raw <- run_cs_did(panel_did_staff, "TOTAL_NURSES_FTE",   "Nurse FTE total")
res_gp_raw <- run_cs_did(panel_did_staff, "TOTAL_GP_FTE",   "GP FTE total")
res_patients <- run_cs_did(panel_did_staff, "TOTAL_PATIENTS",   "patient list")



extract_att <- function(res, outcome){
  
  d <- res$att$DIDparams$data
  
  tibble(
    Outcome = outcome,
    ATT = res$simp$overall.att,
    SE = res$simp$overall.se,
    `95% CI` = sprintf("%.3f to %.3f",
                       res$simp$overall.att - 1.96*res$simp$overall.se,
                       res$simp$overall.att + 1.96*res$simp$overall.se),
    `P value` = 2*pnorm(-abs(res$simp$overall.att/res$simp$overall.se)),
    Practices = n_distinct(d$practice_id),
    `Takeover events` = n_distinct(d$practice_id[d$g!=Inf]),
    Observations = nrow(d)
  )
}

results_table <-
  bind_rows(
    extract_att(res_gp_raw,        "Total GP FTE"),
    extract_att(res_nu_raw,        "Total Nurse FTE"),
    extract_att(res_patients,        "Patient List")
  ) %>%
  mutate(
    ATT = sprintf("%.3f", ATT),
    `P value` = ifelse(`P value` < 0.001, "<0.001",
                       sprintf("%.3f", `P value`))
  )

results_table %>%
  gt() %>%
  cols_label(
    Outcome = "Outcome",
    ATT = "ATT",
    `95% CI` = "95% CI",
    `P value` = "P",
    Practices = "Practices",
    `Takeover events` = "Takeovers",
    Observations = "Practice-years"
  )

####Descriptive before/after plots####


library(dplyr)
library(ggplot2)


make_before_after_plots <- function(df, outcome, outcome_label, window = 5) {
  
  df2 <- df %>%
    mutate(
      treated = g > 0,
      event_time = ifelse(treated, year - g, NA_integer_),
      period = case_when(
        treated & year < g  ~ "Before takeover",
        treated & year >= g ~ "Post takeover",
        TRUE ~ NA_character_
      ),
      group = ifelse(treated, "Treated practices", "Never treated")
    )
  
  # 1) Calendar-time mean trends
  cal_sum <- df2 %>%
    group_by(year, group) %>%
    summarise(
      mean_y = mean(.data[[outcome]], na.rm = TRUE),
      .groups = "drop"
    )
  
  p_calendar <- ggplot(cal_sum, aes(x = year, y = mean_y, color = group)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    labs(
      title = paste0(outcome_label, ": mean outcome over calendar time"),
      x = "Year",
      y = paste0("Mean ", outcome_label),
      color = NULL
    ) +
    theme_minimal()
  
  # 2) Event-time mean trends around takeover
  ev_sum <- df2 %>%
    filter(treated, !is.na(event_time), event_time >= -window, event_time <= window) %>%
    group_by(event_time) %>%
    summarise(
      mean_y = mean(.data[[outcome]], na.rm = TRUE),
      .groups = "drop"
    )
  
  p_event <- ggplot(ev_sum, aes(x = event_time, y = mean_y)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(
      title = paste0(outcome_label, ": mean outcome around takeover"),
      x = "Years relative to takeover",
      y = paste0("Mean ", outcome_label)
    ) +
    theme_minimal()
  
  # 3) Simple pre/post comparison for treated practices only
  pp_sum <- df2 %>%
    filter(treated, !is.na(period)) %>%
    group_by(period) %>%
    summarise(
      mean_y = mean(.data[[outcome]], na.rm = TRUE),
      se = sd(.data[[outcome]], na.rm = TRUE) / sqrt(n()),
      n = n(),
      .groups = "drop"
    )
  
  p_prepost <- ggplot(pp_sum, aes(x = period, y = mean_y)) +
    geom_col(width = 0.6) +
    geom_errorbar(aes(ymin = mean_y - 1.96 * se, ymax = mean_y + 1.96 * se),
                  width = 0.15) +
    labs(
      title = paste0(outcome_label, ": simple pre vs post average"),
      x = NULL,
      y = paste0("Mean ", outcome_label)
    ) +
    theme_minimal()
  
  list(
    calendar = p_calendar,
    event_time = p_event,
    pre_post = p_prepost,
    summary = pp_sum
  )
}


plots_app <- make_before_after_plots(
  patient_panel,
  outcome = "appointment_good",
  outcome_label = "Patient satisfaction with appointments",
  window = 5
)

plots_qof <- make_before_after_plots(
  panel_did_qof,
  outcome = "qof_points",
  outcome_label = "QOF points",
  window = 5
)

plots_gps <- make_before_after_plots(
  panel_did_staff,
  outcome = "gp_fte_per1k",
  outcome_label = "GPs FTE per patients",
  window = 5
)

plots_nurses <- make_before_after_plots(
  panel_did_staff,
  outcome = "nurse_per1k",
  outcome_label = "Nurses FTE per patients",
  window = 5
)

plots_ovr <- make_before_after_plots(
  patient_panel,
  outcome = "overall_good",
  outcome_label = "Overall practice experience",
  window = 5
)



# Print plots
prepost <-
  plots_gps$pre_post+
  plots_nurses$pre_post+
  plots_qof$pre_post+
  plots_app$pre_post+
  plots_ovr$pre_post


cal  <-
  plots_gps$calendar+
  plots_nurses$calendar+
  plots_qof$calendar+
  plots_app$calendar+
  plots_ovr$calendar



####Balance check: treated (t-1) vs never-treated, by cohort, then pooled####
library(dplyr)
library(ggplot2)

get_pretakeover_balance_bycohort <- function(df, outcome, outcome_label) {
  
  df2 <- df %>%
    mutate(
      treated = g > 0,
      event_time = ifelse(treated, year - g, NA_integer_)
    )
  
  # Treated obs at event_time == -1, keep their cohort (g) and year
  treated_pre <- df2 %>%
    filter(treated, event_time == -1) %>%
    mutate(cohort = g)
  
  # For each cohort's t-1 calendar year, get the never-treated mean in that same year
  cohort_years <- treated_pre %>% distinct(cohort, year)
  
  never_by_year <- df2 %>%
    filter(!treated) %>%
    group_by(year) %>%
    summarise(
      never_mean = mean(.data[[outcome]], na.rm = TRUE),
      never_n    = sum(!is.na(.data[[outcome]])),
      .groups = "drop"
    )
  
  treated_by_cohort <- treated_pre %>%
    group_by(cohort, year) %>%
    summarise(
      treated_mean = mean(.data[[outcome]], na.rm = TRUE),
      treated_n    = sum(!is.na(.data[[outcome]])),
      .groups = "drop"
    )
  
  cohort_compare <- treated_by_cohort %>%
    left_join(never_by_year, by = "year")
  
  # Pool across cohorts (weighted by treated cohort size) for one treated estimate,
  # and pool never-treated across the same set of years (weighted by n)
  treated_pooled <- treated_pre %>%
    summarise(
      mean_y = mean(.data[[outcome]], na.rm = TRUE),
      se     = sd(.data[[outcome]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[outcome]]))),
      n      = sum(!is.na(.data[[outcome]]))
    ) %>%
    mutate(group = "Treated (year before takeover)")
  
  never_pooled <- df2 %>%
    filter(!treated, year %in% cohort_years$year) %>%
    summarise(
      mean_y = mean(.data[[outcome]], na.rm = TRUE),
      se     = sd(.data[[outcome]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[outcome]]))),
      n      = sum(!is.na(.data[[outcome]]))
    ) %>%
    mutate(group = "Never treated (same years, cohort-matched)")
  
  bind_rows(treated_pooled, never_pooled) %>%
    mutate(
      ci_low  = mean_y - 1.96 * se,
      ci_high = mean_y + 1.96 * se,
      outcome = outcome_label
    )
}

# Run for each outcome
bal_gps    <- get_pretakeover_balance_bycohort(panel_did_staff, "gp_fte_per1k",     "GPs FTE per patients")
bal_nurses <- get_pretakeover_balance_bycohort(panel_did_staff, "nurse_per1k",      "Nurses FTE per patients")
bal_qof    <- get_pretakeover_balance_bycohort(panel_did_qof,   "qof_points",       "QOF points")
bal_app    <- get_pretakeover_balance_bycohort(patient_panel,   "appointment_good", "Patient satisfaction (appts)")
bal_ovr    <- get_pretakeover_balance_bycohort(patient_panel,   "overall_good",     "Overall practice experience")

balance_all <- bind_rows(bal_gps, bal_nurses, bal_qof, bal_app, bal_ovr) %>%
  mutate(outcome = factor(outcome, levels = c(
    "GPs FTE per patients", "Nurses FTE per patients", "QOF points",
    "Patient satisfaction (appts)", "Overall practice experience"
  )))

p_forest <- ggplot(balance_all, aes(x = mean_y, y = group, color = group)) +
  geom_pointrange(aes(xmin = ci_low, xmax = ci_high), size = 0.6, fatten = 3) +
  facet_wrap(~outcome, scales = "free_x", ncol = 1, strip.position = "top") +
  labs(
    title = "Pre-takeover levels: treated (year before takeover) vs never-treated",
    x = "Mean outcome (95% CI)",
    y = NULL,
    color = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

p_forest

####drop one takeover each ####

library(dplyr)
library(purrr)
library(did)
library(ggplot2)
library(tidyr)





loo_cs_did <- function(df, outcome, outcome_label, control_group = "nevertreated") {
  
  # practices that contribute a takeover event
  treated_practices <- df %>%
    filter(g != 0) %>%
    distinct(PRACTICE_CODE) %>%
    pull(PRACTICE_CODE)
  
  message(sprintf("[%s] %d takeover events to leave out, one at a time...",
                  outcome_label, length(treated_practices)))
  
  get_simple_att <- function(d) {
    dd <- d %>%
      select(practice_id, PRACTICE_CODE, year, g, all_of(outcome)) %>%
      filter(!is.na(.data[[outcome]]))
    
    att <- att_gt(
      yname   = outcome,
      tname   = "year",
      idname  = "practice_id",
      gname   = "g",
      xformla = ~ 1,
      data    = dd,
      panel   = TRUE,
      allow_unbalanced_panel = TRUE,
      control_group = control_group,
      est_method = "reg"
    )
    
    simp <- aggte(att, type = "simple", na.rm = TRUE)
    
    tibble(att = simp$overall.att, se = simp$overall.se)
  }
  
  # full-sample baseline, for reference line on the plot
  baseline <- get_simple_att(df) %>%
    mutate(dropped_practice = "Full sample (baseline)")
  
  # leave-one-out loop over each takeover event
  loo_results <- map_dfr(treated_practices, function(p) {
    d_loo <- df %>% filter(PRACTICE_CODE != p)
    
    out <- tryCatch(
      get_simple_att(d_loo),
      error = function(e) {
        message(sprintf("  -> dropping %s failed: %s", p, conditionMessage(e)))
        tibble(att = NA_real_, se = NA_real_)
      }
    )
    out %>% mutate(dropped_practice = as.character(p))
  })
  
  bind_rows(baseline, loo_results) %>%
    mutate(
      outcome     = outcome_label,
      ci_low      = att - 1.96 * se,
      ci_high     = att + 1.96 * se,
      p_value     = 2 * pnorm(-abs(att / se)),
      is_baseline = dropped_practice == "Full sample (baseline)"
    )
}



set.seed(123)

loo_gp  <- loo_cs_did(panel_did_staff, "gp_fte_per1k",     "GP FTE per 1,000 patients")
loo_nu  <- loo_cs_did(panel_did_staff, "nurse_per1k",      "Nurse FTE per 1,000 patients")
loo_qof <- loo_cs_did(panel_did_qof,   "qof_points",       "QOF points (%)")
loo_app <- loo_cs_did(patient_panel,   "appointment_good", "Patient satisfaction: appointments")
loo_ovr <- loo_cs_did(patient_panel,   "overall_good",     "Patient satisfaction: overall")

loo_all <- bind_rows(loo_gp, loo_nu, loo_qof, loo_app, loo_ovr) %>%
  filter(!is.na(att))   # drop any failed iterations

# flag any leave-one-out estimate that flips sign or loses/gains significance
# relative to baseline, for a quick text summary alongside the figure
loo_flags <- loo_all %>%
  group_by(outcome) %>%
  mutate(
    baseline_att = att[is_baseline],
    baseline_sig = p_value[is_baseline] < 0.05,
    sign_flip    = !is_baseline & sign(att) != sign(baseline_att),
    sig_flip     = !is_baseline & (p_value < 0.05) != baseline_sig
  ) %>%
  ungroup()

cat("\n=== Practices whose removal flips the sign of the ATT ===\n")
print(loo_flags %>% filter(sign_flip) %>% select(outcome, dropped_practice, att, p_value))

cat("\n=== Practices whose removal flips statistical significance (5% level) ===\n")
print(loo_flags %>% filter(sig_flip) %>% select(outcome, dropped_practice, att, p_value))



plot_df <- loo_all %>%
  group_by(outcome) %>%
  mutate(
    baseline_att = att[is_baseline],
    label = ifelse(is_baseline, "Full sample", dropped_practice)
  ) %>%
  arrange(outcome, att) %>%
  mutate(label = factor(label, levels = unique(label))) %>%
  ungroup()

baseline_lines <- plot_df %>% distinct(outcome, baseline_att)

p_loo <- ggplot(plot_df, aes(x = label, y = att, ymin = ci_low, ymax = ci_high)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_hline(
    data = baseline_lines,
    aes(yintercept = baseline_att),
    linetype = "dotted", colour = "steelblue4", linewidth = 0.6,
    inherit.aes = FALSE
  ) +
  geom_pointrange(aes(colour = is_baseline, size = is_baseline)) +
  scale_colour_manual(values = c(`FALSE` = "grey20", `TRUE` = "firebrick"), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 0.35, `TRUE` = 0.55), guide = "none") +
  coord_cartesian(ylim = c(-0.5, 0.5))+
  coord_flip() +
  facet_wrap(~ outcome, scales = "fixed", ncol = 5) +
  labs(
    title    = "Sensitivity of estimated takeover effects to individual events",
    x = NULL,
    y = "Average treatment effect on the treated (95% CI)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text      = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.y      = element_text(size = 7)
  )

print(p_loo)


####missing data####
missingcheck <- master_df %>%
  dplyr::select(PRAC_CODE, year, TOTAL_PATIENTS, TOTAL_GP_FTE, TOTAL_NURSES_FTE, QOF_points, appointment_good, overall_good, Total_NHS_Payments_to_General_Practice_Minus_Deductions)


library(tidyverse)
library(patchwork)
library(viridis)

vars <- c(
  "TOTAL_PATIENTS",
  "TOTAL_GP_FTE",
  "TOTAL_NURSES_FTE",
  "QOF_points",
  "appointment_good",
  "overall_good",
  "Total_NHS_Payments_to_General_Practice_Minus_Deductions"
)

var_labels <- c(
  TOTAL_PATIENTS = "Patients",
  TOTAL_GP_FTE = "GP FTE",
  TOTAL_NURSES_FTE = "Nurse FTE",
  QOF_points = "QOF",
  appointment_good = "Appointment experience",
  overall_good = "Overall experience",
  Total_NHS_Payments_to_General_Practice_Minus_Deductions = "NHS payments"
)


long_dat <- missingcheck %>%
  pivot_longer(
    all_of(vars),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(variable = recode(variable, !!!var_labels))


heat_dat <- long_dat %>%
  group_by(variable, year) %>%
  summarise(
    pct_missing = mean(is.na(value)) * 100,
    .groups = "drop"
  )

p_heat <- ggplot(
  heat_dat,
  aes(year,
      forcats::fct_rev(variable),
      fill = pct_missing)
) +
  geom_tile(colour = "white") +
  scale_fill_viridis(
    name = "% missing",
    option = "C",
    limits = c(0,100)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Missingness by year and variable"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

line_dat <- heat_dat

p_line <- ggplot(
  line_dat,
  aes(year, pct_missing, colour = variable)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  labs(
    x = NULL,
    y = "% practices missing",
    colour = NULL,
    title = "Missingness over time"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

practice_order <- missingcheck %>%
  mutate(
    row_missing =
      rowMeans(across(all_of(vars), ~is.na(.x)))
  ) %>%
  group_by(PRAC_CODE) %>%
  summarise(
    overall = mean(row_missing),
    .groups = "drop"
  ) %>%
  arrange(overall) %>%
  mutate(order = row_number())

raster_dat <- missingcheck %>%
  left_join(practice_order, by = "PRAC_CODE") %>%
  mutate(
    row_missing =
      rowMeans(across(all_of(vars), ~is.na(.x)))
  )

p_raster <- ggplot(
  raster_dat,
  aes(year, order, fill = row_missing)
) +
  geom_raster() +
  scale_fill_viridis(
    name = "Fraction\nmissing",
    option = "C",
    limits = c(0,1)
  ) +
  labs(
    x = NULL,
    y = "Practices",
    title = "Practice-year missingness"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank()
  )

hist_dat <- long_dat %>%
  group_by(PRAC_CODE, variable) %>%
  summarise(
    prop_missing = mean(is.na(value)),
    .groups = "drop"
  )

p_hist <- ggplot(
  hist_dat,
  aes(prop_missing)
) +
  geom_histogram(
    bins = 20,
    fill = "grey60",
    colour = "white"
  ) +
  facet_wrap(~variable, ncol = 2) +
  labs(
    x = "Proportion of years missing",
    y = "Practices",
    title = "Distribution of missingness across practices"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.background = element_rect(fill = "grey95")
  )

(p_heat ) /
  (p_raster ) +
  plot_annotation(
    tag_levels = "A",
    title = "Patterns of missing data across practices, years and variables"
  ) &
  theme(
    plot.title = element_text(face = "bold")
  )






####discarded####
twfe_panel <- patient_panel %>%
  mutate(
    ever_treated = ifelse(g > 0, 1, 0),
    post = ifelse(g > 0 & year >= g, 1, 0),
    did = ever_treated * post
  )

twfe_app2 <- feols(
  appointment_good ~ did | practice_id + year,
  data = twfe_panel,
  cluster = ~practice_id
)

twfe_ovr2 <- feols(
  overall_good ~ did | practice_id + year,
  data = twfe_panel,
  cluster = ~practice_id
)

summary(twfe_app2)
summary(twfe_ovr2)


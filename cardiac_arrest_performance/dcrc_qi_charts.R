###################
#
# DCRC QI Script  #
#
###################

# About:
#
# This script provides essential graphs for monitoring cardiac arrest survival
# in the District of Columbia - starting with the year 2021. The primary focus
# is on the following survival metrics:
#
# 1) Overall survival rate
# 2) Utstein 1 survival rate
# 3) Utstein 2 survival rate
#
# These metrics provide a system level indicator of the performance of the
# D.C. Resuscitation Collaborative. 
# 
# Additional metrics that help to look at System level performance include:
#
# 3) AED Usage
# 4) Shockable Rhythm
# 5) Bystander/Family Member CPR Data
# 6) ROSC Rates
# 7) End of Event Data (transport data)
# 8) ED Resuscitation Data
#
# See .txt file, 'DCRC QI Charts Notes' for additional explanation of metrics
# and variables. 


# Libraries
library(tidyverse)
library(lubridate)
library(qicharts2)
library(ggthemes)
library(janitor)

# Set Directory
setwd("P:/Analysis_R/Data/cardiac arrest/cares")


# Load in data
df <- read_csv("cares_qicharts.csv")



# DATA CLEANING ----
df_clean <- 
  df %>% 
  clean_names() %>% 
  select(ems_agency,
         date_of_arrest,
         incident_number,
         age,
         age_modifier,
         gender,
         race_ethnicity,
         destination_hospital,
         location_type,
         location_type_other,
         arrest_witness_status,
         presumed_cardiac_arrest_etiology,
         resuscitation_attempted,
         initiated_cpr,
         did_law_enforcement_initiate_cpr:transferred_hospital_section_status) %>% 
  mutate_all(toupper) %>% 
  mutate_at(.vars = vars(date_of_arrest),
            .funs = mdy) %>% 
  mutate_at(.vars = vars(age),
            .funs = as.numeric) %>% 
  mutate(
    presumed_cardiac_arrest_etiology = case_when(
      str_detect(presumed_cardiac_arrest_etiology, "[A-Z]") ~ presumed_cardiac_arrest_etiology,
               TRUE ~ "NA"),
    destination_hospital = case_when(
      str_detect(destination_hospital, "[A-Z]") ~ destination_hospital,
      TRUE ~ "NA"
    ),
    type_of_bystander_cpr_provided = case_when(
      str_detect(type_of_bystander_cpr_provided, "[A-Z]") ~ type_of_bystander_cpr_provided,
    TRUE ~ "NA"
    ),
    initiated_cpr = case_when(
      str_detect(initiated_cpr, "[A-Z]") ~ initiated_cpr,
      TRUE ~ "NA"
    ),
    aed_used_during_resuscitation = case_when(
      str_detect(aed_used_during_resuscitation, "[A-Z]") ~ aed_used_during_resuscitation,
      TRUE ~ "NA"
    ),
    was_an_aed_applied_prior_to_ems_arrival = case_when(
      str_detect(was_an_aed_applied_prior_to_ems_arrival, "[A-Z]") ~ was_an_aed_applied_prior_to_ems_arrival,
      TRUE ~ "NA"
    ),
    first_monitored_rhythm = case_when(
      str_detect(first_monitored_rhythm, "[A-Z]") ~ first_monitored_rhythm,
      TRUE ~ "NA"
    ),
    rosc = case_when(
      str_detect(rosc, "[A-Z]") ~ rosc,
      TRUE ~ "NA"
    ),
    sustained_rosc = case_when(
      str_detect(sustained_rosc, "[A-Z]") ~ sustained_rosc,
      TRUE ~ "NA"
    ),
    emergency_room_outcome = case_when(
      str_detect(emergency_room_outcome, "[A-Z]") ~ emergency_room_outcome,
      TRUE ~ "NA"
    ),
    hospital_outcome = case_when(
      str_detect(hospital_outcome, "[A-Z]") ~ hospital_outcome,
      TRUE ~ "NA"
    ),
    neurological_outcome = case_when(
      str_detect(neurological_outcome, "[A-Z]") ~ neurological_outcome,
      TRUE ~ "NA"
    ),
    out_of_hospital_disposition = case_when(
      str_detect(out_of_hospital_disposition, "[A-Z]") ~ out_of_hospital_disposition,
      TRUE ~ "NA"
    ),
    discharge_from_the_hospital = case_when(
      str_detect(discharge_from_the_hospital, "[A-Z]") ~ discharge_from_the_hospital,
      TRUE ~ "NA"
    ),
    arrest_witness_status = case_when(
      str_detect(arrest_witness_status, "[A-Z]") ~ arrest_witness_status,
      TRUE ~ "NA"
    ),
    destination_hospital = case_when(
      str_detect(destination_hospital, "[A-Z]") ~ destination_hospital,
      TRUE ~ "NA"
    )
  ) %>% 
  filter(!presumed_cardiac_arrest_etiology %in% c(
    "TRAUMA",
    "OTHER",
    "DROWNING/SUBMERSION",
    "ELECTROCUTION",
    "EXSANGUINATION/HEMORRHAGE",
    "NA"
  )) %>%
  filter(age >= 18,
         age_modifier == "YEARS")

# QI CHART DATA ----

unique(df_clean$arrest_witness_status)

df_qi<- 
  df_clean %>% 
  select(date_of_arrest,
         incident_number,
         age,
         presumed_cardiac_arrest_etiology,
         location_type,
         arrest_witness_status,
         initiated_cpr,
         was_an_aed_applied_prior_to_ems_arrival,
         first_monitored_rhythm,
         rosc,
         sustained_rosc,
         end_of_the_event,
         destination_hospital,
         hospital_outcome,
         discharge_from_the_hospital,
         neurological_outcome,
         out_of_hospital_disposition) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(yr = year(date_of_arrest),
         mnth = month(date_of_arrest),
         new_date = as.Date(paste0(yr,"-",mnth,"-01")),
         rhythm_type = case_when(
           first_monitored_rhythm %in% c("ASYSTOLE",
                                         "IDIOVENTRICULAR/PEA",
                                         "UNKNOWN UNSHOCKABLE RHYTHM") ~ "NON-SHOCK",
           TRUE ~ "SHOCK"
         ),
         
         dx_alive = case_when(hospital_outcome == "DISCHARGED ALIVE" ~ 1,
                              TRUE ~ 0),
         u1    = case_when(arrest_witness_status == "WITNESSED BY BYSTANDER" & 
                                rhythm_type == "SHOCK" ~ 1,
                              TRUE ~ 0),
         dx_u1    = case_when(arrest_witness_status == "WITNESSED BY BYSTANDER" & 
                             rhythm_type == "SHOCK" & dx_alive == "1" ~ 1,
                           TRUE ~ 0),
         u2    = case_when(arrest_witness_status == "WITNESSED BY BYSTANDER" & 
                                 rhythm_type == "SHOCK" &
                                ((initiated_cpr == "BYSTANDER" || initiated_cpr == "FAMILY MEMBER") | 
                                   str_detect(was_an_aed_applied_prior_to_ems_arrival, "YES")) ~ 1,
                               TRUE ~ 0),
         bystd_cpr = case_when(initiated_cpr == "BYSTANDER" | initiated_cpr == "FAMILY MEMBER" ~ 1,
                               TRUE ~ 0),
         rosc_data = case_when(str_detect(sustained_rosc, "YES") ~ "ROSC",
                               TRUE ~ "NO_ROSC"),
         event_end = case_when(str_detect(end_of_the_event, "IN ED") ~ "TRANSPORTED",
                               TRUE ~ "NO_TRANSPORT"),
         aed_use   = case_when(str_detect(was_an_aed_applied_prior_to_ems_arrival, "YES") ~ "YES",
                               TRUE ~ "NO"))

summary(df_qi)
## RUN CHARTS ----

# Run Charts of cardiac arrests




# 1) Overall survival ----

# Data frame for run and control charts - OVERALL SURVIVAL
df_run_sca <- 
  df_qi %>% 
  group_by(new_date) %>% 
  summarise(n = n(),
            discharged = sum(dx_alive)) %>% 
  mutate(percent = (discharged/n)*100)

run_sca <- qic(x = new_date,
               y = n,
               data = df_run_sca)

run_surv_sca <- qic(x = new_date,
                    y = discharged,
                    data = df_run_sca)

run_survp_sca <- qic(x = new_date,
                     y = percent,
                     data = df_run_sca)

run_1 <- df_qi %>% 
  group_by(new_date) %>% 
  summarise(n = n(),
            discharged = sum(dx_alive)) %>% 
  mutate(prop = discharged/n)




# CONTROL CHARTS --

# Variables for i_chart_sca
df_run_sca$notes     <- ""
df_run_sca$notes[4]  <- "COVID-19 Wave 1"
df_run_sca$notes[13] <- "COVID-19 Wave 3"

i_chart_sca <- 
  qic(x = new_date,
      y = n,
      notes = notes,
      data = df_run_sca,
      chart = "i",
      title = "I-Chart: Number of Sudden Cardiac Arrests in the District of Columbia",
      ylab     = "Number of Cardiac Arrests",
      xlab     = "Months")

i_chart_sca$data

i_chart_surv <- 
  qic(x = new_date,
      y = discharged,
      data = df_run_sca,
      part = 14,
      chart = "i",
      title = "I-Chart: Number of Sudden Cardiac Arrest Survivors in the District of Columbia",
      ylab     = "Number of Cardiac Arrests",
      xlab     = "Months",x.format = "%b, '%y",
      y.neg = FALSE)

i_chart_surv_p <- 
  qic(x = new_date,
      y = percent,
      data = df_run_sca,
      part = 14,
      chart = "i",
      title = "I-Chart: Number of Sudden Cardiac Arrest Survivors in the District of Columbia",
      ylab     = "Number of Cardiac Arrests",
      xlab     = "Months", y.neg = FALSE)


# 2) Utstein 1 Survival -----

df_run_sca_u1 <- 
  df_qi %>% 
  group_by(new_date) %>% 
  summarise(n = n(),
            u1_criteria = sum(u1),
            discharged_u1 = sum(dx_u1)) %>% 
  mutate(percent = (discharged_u1/u1_criteria)*100)

run_sca_u1 <- qic(x = new_date,
                       y = u1_criteria,
                       data = df_run_sca_u1)

run_surv_sca_u1 <- qic(x = new_date,
                    y = discharged_u1,
                    data = df_run_sca_u1)

run_survp_sca_u1 <- qic(x = new_date,
                     y = percent,
                     data = df_run_sca_u1)

# CONTROL CHARTS

i_charts_u1 <- qic(x = new_date,
                  y = u1_criteria,
                  data = df_run_sca_u1, 
                  chart = "i",
                  y.neg = FALSE)

i_charts_u1p <- qic(x = new_date,
                   y = percent,
                   data = df_run_sca_u1, 
                   chart = "i",
                   y.neg = FALSE)




# 3) Utstein 2 Survival ----

# 4) AED Usage ----

df_run_aed <- 
  df_qi %>%
  group_by(new_date,
           aed_use) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = "aed_use",
              values_from = "n") %>%
  ungroup() %>%
  mutate(total = YES + NO,
         percent = YES/total)

# RUN CHARTS

run_aed <- qic(x = new_date,
               y = YES,
               data = df_run_aed)

run_aed_p <- qic(x = new_date,
                 y = percent,
                 y.expand = c(0,1),
                 y.percent = T,
                 data = df_run_aed,
                 title = "Run Chart: Percent of Cardiac Arrest cases with AED used prior to EMS Arrival",
                 ylab  = "Percent of Cases",
                 xlab  = "Months",
                 x.format = "%b '%y")


# 5) Shockable Arrest ----

df_run_shock <- 
  df_qi %>%
  group_by(new_date, rhythm_type) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = "rhythm_type",
              values_from = "n") %>%
  ungroup() %>%
  mutate(total = `NON-SHOCK` + SHOCK,
         percent_shock = SHOCK/total)

# RUN CHARTS
run_shock <- qic(x = new_date,
                 y = SHOCK,
                 data = df_run_shock) # run control chart - 2 spikes

# CONTROL CHARTS

i_chart_shock <- qic(x = new_date,
                     y = SHOCK,
                     data = df_run_shock,
                     chart = "i")

p_chart_shock <- qic(x = new_date,
                     y = SHOCK,
                     n = total,
                     data = df_run_shock,
                     chart = "pp")


# 6) Bystander/Family Member CPR ----

df_run_byst_cpr <- 
  df_qi %>%
  group_by(new_date,
           bystd_cpr) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = "bystd_cpr",
              values_from = "n", 
              names_prefix = "bystd_cpr_") %>%
  ungroup() %>%
  mutate(total = bystd_cpr_0 + bystd_cpr_1,
         percent = bystd_cpr_1/total)

# RUN CHARTS

run_bystd_cpr <- qic(x = new_date,
                     y = bystd_cpr_1,
                     data = df_run_byst_cpr)

run_bystd_cpr_p <- qic(x = new_date,
                       y = percent,
                       data = df_run_byst_cpr,
                       y.percent = T,
                       title = "Run Chart: Percentage of cases with Bystander/Family Member Initiated CPR",
                       ylab  = "Percent of cases",
                       xlab  = "Months",
                       x.format = "%b '%y",
                       y.expand = 0)

# 7) ROSC ----

df_run_rosc <- 
  df_qi %>%
  group_by(new_date, rosc_data) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = "rosc_data",
              values_from = "n") %>%
  ungroup() %>%
  mutate(total = NO_ROSC + ROSC,
         percent = ROSC/total)


# RUN CHARTS
run_rosc <- qic(x = new_date,
                y = ROSC,
                data = df_run_rosc)


run_rosc_p <- qic(x = new_date,
                y = percent,
                data = df_run_rosc,
                y.percent = TRUE,
                y.expand = 0)


# CONTROL CHARTS

i_chart_rosc <- qic(x = new_date,
                    y = percent,
                    data = df_run_rosc,
                    chart = "i")

pp_chart_rosc <- qic(x = new_date,
                     y = ROSC,
                     n = total,
                     data = df_run_rosc,
                     chart = "pp",
                     title = "P-Chart: Percentage of Cases Achieving ROSC per Month",
                     ylab  = "Percent of Cases",
                     xlab  = "Months",
                     x.format = "%b '%y",
                     y.expand = 0)

# 8) End of the Event ----

df_run_eoe <- 
  df_qi %>%
  group_by(new_date, event_end) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = "event_end",
              values_from = "n") %>%
  ungroup() %>%
  mutate(total = NO_TRANSPORT + TRANSPORTED,
         percent = TRANSPORTED/total)

run_trans <- qic(x = new_date,
                 y = TRANSPORTED,
                 data = df_run_eoe)

run_trans_p <- qic(x = new_date,
                   y = percent,
                   y.percent = T,
                   y.expand = c(0,1),
                   data = df_run_eoe)

# 9) ED Resuscitation ----

df_run_edr <- 
  df_qi %>%
  filter(event_end == "TRANSPORTED") %>%
  group_by(new_date,
           end_of_the_event) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = "end_of_the_event",
              values_from = "n") %>%
  ungroup() %>%
  clean_names() %>%
  mutate(total = ongoing_resuscitation_in_ed + pronounced_dead_in_ed,
         percent = ongoing_resuscitation_in_ed/total)

qic(x = new_date,
    y = ongoing_resuscitation_in_ed,
    data = df_run_edr)

qic(x = new_date,
    y = percent,
    y.expand = c(0,1),
    y.percent = T,
    data = df_run_edr)

# 10) Which hospitals receive patients


# EXPLORATORY ANALYSIS
df_dest_hosp <- 
  df_qi %>%
  select(incident_number,
         date_of_arrest,
         sustained_rosc,
         rosc_data,
         destination_hospital) %>%
  filter(destination_hospital != "NA") %>%
  mutate(yr = year(date_of_arrest),
         mnth = month(date_of_arrest),
         new_date = as.Date(paste0(yr,'-',mnth,'-01')),
         stemi_facility = case_when(
           destination_hospital == "GEORGE WASHINGTON UNIVERSITY HOSPITAL" |
             destination_hospital == "WASHINGTON HOSPITAL CENTER - MEDSTAR" |
             destination_hospital == "HOWARD UNIVERSITY HOSPITAL" ~ "STEMI_FAC",
           TRUE ~ "NON_STEMI_FAC")) %>%
  group_by(new_date,
           destination_hospital) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n)) %>%
  filter(destination_hospital %in% c(
    "GEORGE WASHINGTON UNIVERSITY HOSPITAL",
    "WASHINGTON HOSPITAL CENTER - MEDSTAR",
    "HOWARD UNIVERSITY HOSPITAL"
  ))




  
# Line Graph: Number of cardiac arrest cases by month transported to STEMI Hospitals
ggplot(data = df_dest_hosp,
       aes(x = new_date,
           y = n,
           color = destination_hospital)) +
  geom_line() +
  geom_point() +
  labs(title = "Number of Cardiac Arrest Patients Transported Hospitals: 5, 8, and 13",
       y     = "Number of Cardiac Arrest Patients",
       x     = "Months",
       color = "Destination Hospital")

# Line Graph: Proportion of cardiac arrest cases by month transported to STEMI Hospitals
ggplot(data = df_dest_hosp,
       aes(x = new_date,
           y = prop,
           color = destination_hospital)) +
  geom_line() +




df_dest_hosp_rosc <- 
  df_qi %>%
  select(incident_number,
         date_of_arrest,
         age,
         sustained_rosc,
         rosc_data,
         destination_hospital) %>%
  filter(destination_hospital != "NA") %>%
  mutate(yr = year(date_of_arrest),
         mnth = month(date_of_arrest),
         new_date = as.Date(paste0(yr,'-',mnth,'-01'))) %>%
  group_by(new_date,
           rosc_data,
           destination_hospital) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n)) %>%
  filter(destination_hospital %in% c(
    "GEORGE WASHINGTON UNIVERSITY HOSPITAL",
    "WASHINGTON HOSPITAL CENTER - MEDSTAR",
    "HOWARD UNIVERSITY HOSPITAL"
  ),
  rosc_data == 'ROSC')


# Line Graph: Proportion of cardiac arrests with ROSC transported to hospitals

ggplot(data = df_dest_hosp_rosc,
       aes(x = new_date,
           y = prop,
           color = destination_hospital)) +
  geom_line() +
  expand_limits(y = 1) +
  labs(title = "Proportion of Sustained or Transient ROSC Patients Transported to One of Three STEMI Centers",
       y     = "Proportion of ROSC Patients Transported",
       x     = "Months",
       color = "Destination Hospital")




df_dest_hosp_rosc_age <- 
  df_qi %>%
  select(incident_number,
         date_of_arrest,
         age,
         sustained_rosc,
         rosc_data,
         destination_hospital) %>%
  filter(destination_hospital != "NA") %>%
  mutate(yr = year(date_of_arrest),
         mnth = month(date_of_arrest),
         new_date = as.Date(paste0(yr,'-',mnth,'-01'))) %>%
  group_by(destination_hospital) %>%
#  summarise(n = n()) %>%
 # mutate(prop = n/sum(n)) %>%
  filter(destination_hospital %in% c(
    "GEORGE WASHINGTON UNIVERSITY HOSPITAL",
    "WASHINGTON HOSPITAL CENTER - MEDSTAR",
    "HOWARD UNIVERSITY HOSPITAL"
  ),
  rosc_data == 'ROSC')



# Box Plot: Age Distributions of ROSC

ggplot(data = df_dest_hosp_rosc_age,
       aes(x = destination_hospital,
           y = age,
           fill = as.factor(yr))) +
  geom_boxplot() +
  labs(title = "Age Distribution of Patients with Sustained or Transient ROSC by Dest. Hospital",
       y     = "Age (Years)",
       x     = NULL,
       fill  = "Year")

# QI CHARTS:

df_stemi_fac_qi <- 
  df_qi %>%
  select(incident_number,
         date_of_arrest,
         sustained_rosc,
         rosc_data,
         destination_hospital) %>%
  filter(destination_hospital != "NA") %>%
  mutate(yr = year(date_of_arrest),
         mnth = month(date_of_arrest),
         new_date = as.Date(paste0(yr,'-',mnth,'-01')),
         stemi_facility = case_when(
           destination_hospital == "GEORGE WASHINGTON UNIVERSITY HOSPITAL" |
             destination_hospital == "WASHINGTON HOSPITAL CENTER - MEDSTAR" |
             destination_hospital == "HOWARD UNIVERSITY HOSPITAL" ~ "STEMI_FAC",
           TRUE ~ "NON_STEMI_FAC")) %>%
  group_by(new_date,
           stemi_facility) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = "stemi_facility",
              values_from = "n") %>%
  mutate(total = NON_STEMI_FAC + STEMI_FAC,
         prop_stemi  = STEMI_FAC/total)

# RUN CHARTS: Aggregated by STEMI Faclities
run_stemi_fac_total <- qic(x = new_date,
                           y = total,
                           data = df_stemi_fac_qi)

run_stemi_fac_prop <- qic(x = new_date,
                          y = prop_stemi,
                          data = df_stemi_fac_qi,
                          y.expand = c(0,1),
                          y.percent = T,
                          title = "Run Chart: Percent of Patients Transported to H5, H8, and H13",
                          ylab     = "Percent",
                          xlab     = "Months")


# CONTROL CHARTS: Proportion of patients transported to STEMI Facilities
pchrt_stemi_fac <- qic(x = new_date,
                       y = STEMI_FAC,
                       n = total,
                       data = df_stemi_fac_qi,
                       chart = "p",
                       y.expand = c(0,1),
                       title    = "P-Chart: Percent of Patients Transported to H5, H8, and H13",
                       ylab     = "Percent",
                       xlab     = "Months")

# 11) Non-Shockable Arrests with ROSC

df_nonshock <- 
  df_qi %>%
  select(incident_number,
         date_of_arrest,
         rhythm_type,
         rosc_data,
         sustained_rosc,
         destination_hospital,
         discharge_from_the_hospital) %>%
  filter(rhythm_type      == 'NON-SHOCK',
         sustained_rosc   == 'NO') %>%
  mutate(yr = year(date_of_arrest),
         mnth = month(date_of_arrest),
         new_date = as.Date(paste0(yr,"-",mnth,"-01"))) %>%
  group_by(new_date,
           destination_hospital) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n)) %>%
  filter(destination_hospital != "NA" & 
           destination_hospital %in% c(
             "GEORGE WASHINGTON UNIVERSITY HOSPITAL",
             "WASHINGTON HOSPITAL CENTER - MEDSTAR",
             "HOWARD UNIVERSITY HOSPITAL"
  ))

ggplot(data = df_nonshock,
       aes(x = new_date,
           y = n,
           color = destination_hospital)) +
  geom_line() +
  labs(title = "Number of Transports per Month of Likely Non-Viable Cardiac Arrests",
       y     = "Number of Cardiac Arrests",
       x     = "Months",
       color = "Destination Hospital")


ggplot(data = df_nonshock,
       aes(x = new_date,
           y = prop,
           color = destination_hospital)) +
  geom_line() +
  labs(title = "Proportion of Transports per Month of Likely Non-Viable Cardiac Arrests",
       y     = "Proportion of Cardiac Arrests",
       x     = "Months",
       color = "Destination Hospital")



df_nonshock_outcome <- 
  df_qi %>%
  select(incident_number,
         date_of_arrest,
         rhythm_type,
         rosc_data,
         sustained_rosc,
         destination_hospital,
         hospital_outcome) %>%
  filter(rhythm_type      == 'NON-SHOCK',
         sustained_rosc   == 'NO') %>%
  mutate(yr = year(date_of_arrest),
         mnth = month(date_of_arrest),
         new_date = as.Date(paste0(yr,"-",mnth,"-01"))) %>%
  group_by(new_date,
           destination_hospital,
           hospital_outcome) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n)) %>%
  filter(!hospital_outcome %in% c("NA", "NOT APPLICABLE") & 
           destination_hospital %in% c(
             "GEORGE WASHINGTON UNIVERSITY HOSPITAL",
             "WASHINGTON HOSPITAL CENTER - MEDSTAR",
             "HOWARD UNIVERSITY HOSPITAL"
           ))


# DATA CHECKS

# 1) Found AED with defib data inconsistent with first monitored rhythm data
non_matching_data <-  
  df_qi %>%
  select(incident_number, 
         date_of_arrest, 
         was_an_aed_applied_prior_to_ems_arrival,
         first_monitored_rhythm) %>%
  filter(was_an_aed_applied_prior_to_ems_arrival == "YES, WITH DEFIBRILLATION" &
           first_monitored_rhythm != "UNKNOWN SHOCKABLE RHYTHM")




print(non_matching_data)
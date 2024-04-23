## hospitalisation dataset PatientDF

## load packages
library(dplyr)
library(tidyverse)
library(data.table)
library(ggplot2)
library(readr)

## load event data
load("event.data.Rdata")

## remove duplicates
event.data <- distinct(event.data)

## filter event type with "IP"&"DT" (note, actually no event type DT)
event.data <- event.data %>%
  filter(EVENT_TYPE==c("IP","DT"))

## load data
load("diags.10.Rdata")

## tidy datasets
diags.10 <- diags.10 %>%
  select("EVENT_ID",starts_with("diag_")) %>%
  pivot_longer(names_to = "codes", 
               cols = -EVENT_ID) %>%
  select(EVENT_ID,value) %>%
  rename(ICD_10_codes = value) %>%
  filter(ICD_10_codes!="")

## remove replicates
diags.10 <- distinct(diags.10)

## join event and diags files 
event.diagnosis <- event.data %>%
  left_join(diags.10,by="EVENT_ID") %>%
  mutate(EVENT_START_DATETIME = dmy(EVENT_START_DATETIME),
         EVENT_END_DATETIME = dmy(EVENT_END_DATETIME))

## filter clinical codes (STEMI, NSTEMI, UNSTABLE ANGINA)
event.diagnosis <- event.diagnosis %>%
  filter(ICD_10_codes %in% c('I210', 'I211', 'I212', 'I213', 'I220', 'I221', 'I228', 'I229','I214', 
                             'I219', 'I252', 'I200')) %>%
  mutate(CLINICAL_CODE_GROUP=ifelse(ICD_10_codes %in% c('I214','I219', 'I252'),'NSTEMI','STEMI'))

## assign unstable angina when clinical code=I200
event.diagnosis[event.diagnosis$ICD_10_codes=='I200',]$CLINICAL_CODE_GROUP <- 'Unstable angina'

## extract first record for each ID
event.diagnosis <- event.diagnosis %>%
  arrange(EVENT_START_DATETIME) %>%
  group_by(MASTER_ENCRYPTED_HCU_ID) %>%
  slice_head(n=1)%>%
  arrange(EVENT_START_DATETIME)

# Define breaks for age groups (e.g., 0-9, 10-19, 20-29, ...)
breaks <- seq(0, max(event.diagnosis$AGE_AT_ADMISSION) + 10, by = 10) - 1

# Create age groups using cut()
event.diagnosis$age_group <- cut(event.diagnosis$AGE_AT_ADMISSION, breaks = breaks, labels = paste0(breaks[-length(breaks)] + 1, "-", breaks[-1]))
write.csv(event.diagnosis, "C:\\Users\\64210\\Desktop\\event.diagnosis.csv", row.names=FALSE)
## check ethnic group
event.diagnosis$ethnic_4 <- apply(event.diagnosis[, c("ETHNICGP", "ETHNICG1", "ETHNICG2","ETHNICG3")], 1, function(x) any(grepl("^4\\d{1}$", x)))
event.diagnosis$ethnic_3 <- apply(event.diagnosis[, c("ETHNICGP", "ETHNICG1", "ETHNICG2","ETHNICG3")], 1, function(x) any(grepl("^3\\d{1}$", x)))
event.diagnosis$ethnic_36 <- apply(event.diagnosis[, c("ETHNICGP", "ETHNICG1", "ETHNICG2","ETHNICG3")], 1, function(x) any(grepl("^36$", x)))
event.diagnosis$ethnic_43 <- apply(event.diagnosis[, c("ETHNICGP", "ETHNICG1", "ETHNICG2","ETHNICG3")], 1, function(x) any(grepl("^43$", x)))
event.diagnosis$ethnic_36_43 <- event.diagnosis$ethnic_36 & event.diagnosis$ethnic_43

event.diagnosis <- event.diagnosis %>%
  select(MASTER_ENCRYPTED_HCU_ID, GENDER, AGE_AT_ADMISSION, age_group, 
         ethnic_4, ethnic_3, ethnic_36, ethnic_43, ethnic_36_43, CLINICAL_CODE_GROUP)

write_xlsx(event.diagnosis,"C:\\Users\\64210\\Desktop\\Hospitalisation.xlsx")
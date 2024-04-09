# Author: Kristianny Ruelas-Vargas
# PI: Dr. Ajitesh Srivastava
# Source: Delphi COVID-19 Trends & Impacts Survey
# Ming Hsieh Department of Computer and Electrical Engineering
# University of Southern California
#DAILY DATA 
#Packages to Install
install.packages("covidcast")
install.packages('ghibli')
install.packages('sjPlot')
install.packages('lubridate')
install.packages('tidyverse')
install.packages("devtools")
install.packages("igraph")
install.packages("viridis")

library(viridis)
library(conflicted)
library(sjPlot)
library(lubridate)
library(ghibli)
library(dplyr)
library(tidyverse)
library(covidcast)
library(conflicted)
#solving conflicts between packages through trial and error
conflicted::conflict_prefer("map2", "purrr")  # Prioritize map2 from purrr package
conflicted::conflict_prefer("filter", "dplyr")  # Prioritize filter from dplyr package
library(gridExtra)

options(covidcast.auth = "3e5e14007dd9e")# this is the authorization code to be able to access the delphi covidcast aggregated data

library(viridisLite)
library(igraph)


## Key Main Rules
# 1) Always do min-max normalization when adjusting for a new time period
# 2) Only define states and signals being used once
# 3) Create the wide format of the dataset you'll use and use it for all analysis (implement this in the future)


#### Official Data Cleaning ####

#DATA CLEANING#
data <- covidcast_meta()
print(data) #511 signals 

#general filtering, we only want signals that have info for state and by day 
filtered.data <- subset(data, data_source == "fb-survey" & geo_type == "state" & time_type == "day")
print(filtered.data)# num of signals is 357

#filtering by signal names to only include those that start with smoothed_w
filtered.signals <- subset(filtered.data, grepl("^smoothed_w", signal)) #looks at the signal columns and finds the rows that start with smoothed_w
print(filtered.signals) # num of signals is 209

#filter out the names that should have two w's but only have one: ex smoothed_want should not be there only smoothed_wwant

#finding out those with ww 
ww.names <- subset(filtered.signals, grepl("^smoothed_ww", signal)) # ^ makes sure that smoothed_w is at the beginning of the string
# there is 18 signals that need to be excluded

# Define the patterns to exclude 
exclude.patterns <- c("smoothed_want_info_children_education", "smoothed_want_info_covid_treatment",
                      "smoothed_want_info_covid_variants", "smoothed_want_info_employment",
                      "smoothed_want_info_mental_health",    "smoothed_want_info_none",              
                      "smoothed_want_info_relationships",    "smoothed_want_info_vaccine_access",    
                      "smoothed_want_info_vaccine_types",   "smoothed_wanted_test_14d" ,
                      "smoothed_wearing_mask", "smoothed_wearing_mask_7d",             
                      "smoothed_work_outside_home_1d" , "smoothed_work_outside_home_indoors_1d",
                      "smoothed_worried_become_ill", "smoothed_worried_catch_covid",         
                      "smoothed_worried_finances",   "smoothed_worried_vaccine_side_effects")

cleaned.signals <- subset(filtered.signals, !signal %in% exclude.patterns) # has 191 signals 
summary(cleaned.signals)# makes sure that all the signals in cleaned.signals is available at the state level


# Getting the Data
# Initialize an empty list to store the covidcast_signal dataframes
fb.covidcast.data <- list()

# Iterate over each signal and fetch data using covidcast_signals()
for (signal in cleaned.signals$signal) {
  signal.data <- covidcast_signal(
    data_source = "fb-survey",
    signal = signal,
    geo_type = "state",
    time_type = "day")#we will be doing the weekly later
  
  # Append the signal data to the list
  fb.covidcast.data[[signal]] <- signal.data
}
#shows warnings, that it is just missing data from the first day, this is removed later

## HOSPITALIZATION AND MORTALITY INFO ADDED 

#hospitalizations
#source from U.S. Department of Health & Human Services (HHS)
hospitalizations <- covidcast_signal(data_source = "hhs", signal = "confirmed_admissions_covid_1d_7dav", start_day = "2020-04-06", end_day = "2022-06-25", geo_type = "state", time_type = "day")
#start and end time was decided by the following commands:
min(cleaned.signals$min_time)#2020-04-06
max(cleaned.signals$max_time)#2022-06-25
#Deaths	
#Johns Hopkins University (JHU)
deaths <- covidcast_signal(data_source = "jhu-csse", signal = "deaths_incidence_num", geo_type = "state", time_type ="day")

#adding it to the fb list
fb.covidcast.data[["confirmed_admissions_covid_1d_7dav"]] <- hospitalizations
fb.covidcast.data[["deaths_incidence_num"]] <- deaths

#using aggregate to add all the covidcast_signal objects (193) of them into one data frame in "long" format
#this means that each of the 193 signals has a row for each data value entry that it has 
full.ds <- aggregate_signals(fb.covidcast.data, format = "long")
dim(full.ds) #2704700 rows,  16 columns (working with 2.7 million answers in this df)


#### CREATING CATEGORY TYPE COLUMN ####
sig.names<- unique(full.ds$signal) #a vector

###BEHAVIOR INDICATORS###
#mask
mask.all <- c("smoothed_wwearing_mask_7d","smoothed_wwearing_mask", "smoothed_wothers_masked_public", "smoothed_wothers_masked" )
mask.match <- intersect(sig.names, mask.all) # finds common values between mask and signames
mask.match

full.ds$category_type <- ""
full.ds$category_type[full.ds$signal %in% mask.match] <- "mask_use"

#CHECKING to see that it worked
#unique(full.ds$signal[full.ds$category_type == "mask_use"]) #checking to see if it worked 
unique(full.ds$category_type[full.ds$signal == "smoothed_wothers_masked" | full.ds$signal == "smoothed_wothers_masked_public"] == "mask_use") 
#making sure the numbers are the same in category and # of times the signal names show up in full.ds
length(full.ds$category_type[full.ds$signal == "smoothed_wothers_masked" | full.ds$signal == "smoothed_wothers_masked_public"] == "mask_use")
length(full.ds$signal[full.ds$signal == "smoothed_wothers_masked_public"]) + length(full.ds$signal[full.ds$signal == "smoothed_wothers_masked"])

#social_distancing_and_travel 
social.all <- c("smoothed_wothers_distanced_public",
                "smoothed_wpublic_transit_1d",
                "smoothed_wtravel_outside_state_7d",
                "smoothed_wwork_outside_home_indoors_1d",
                "smoothed_wshop_indoors_1d",
                "smoothed_wrestaurant_indoors_1d",
                "smoothed_wspent_time_indoors_1d",
                "smoothed_wlarge_event_indoors_1d",
                "smoothed_wtravel_outside_state_5d",
                "smoothed_wwork_outside_home_1d",
                "smoothed_wshop_1d",
                "smoothed_wrestaurant_1d",
                "smoothed_wspent_time_1d",
                "smoothed_wlarge_event_1d")
social.match <- intersect(sig.names, social.all) # finds common values between social and signames
social.match #14/14signals

full.ds$category_type[full.ds$signal %in% social.match] <- "social_distancing_and_travel" 

#schooling_type
schooling.type <- c("smoothed_winperson_school_fulltime_oldest",
                    "smoothed_winperson_school_parttime_oldest",
                    "smoothed_wremote_school_fulltime_oldest",
                    "smoothed_winperson_school_fulltime",
                    "smoothed_winperson_school_parttime")
schooling.match <- intersect(sig.names, schooling.type)
schooling.match #all 5 signals

full.ds$category_type[full.ds$signal %in% schooling.match] <-"schooling_type"
unique(full.ds$category_type)
length(full.ds$signal[full.ds$category_type == "schooling_type"]) #52441

#school safety measures
school.measures.all <- c("smoothed_wschool_safety_measures_mask_students",
                         "smoothed_wschool_safety_measures_mask_teachers",
                         "smoothed_wschool_safety_measures_restricted_entry",
                         "smoothed_wschool_safety_measures_separators",
                         "smoothed_wschool_safety_measures_extracurricular",
                         "smoothed_wschool_safety_measures_symptom_screen",
                         "smoothed_wschool_safety_measures_ventilation",
                         "smoothed_wschool_safety_measures_testing_staff",
                         "smoothed_wschool_safety_measures_testing_students",
                         "smoothed_wschool_safety_measures_vaccine_staff",
                         "smoothed_wschool_safety_measures_vaccine_students",
                         "smoothed_wschool_safety_measures_cafeteria",
                         "smoothed_wschool_safety_measures_dont_know")
schooling.measures.match <- intersect(sig.names, school.measures.all)
schooling.measures.match #13/13 signals

full.ds$category_type[full.ds$signal %in% schooling.measures.match] <-"school_safety_measures"
unique(full.ds$category_type)
length(full.ds$signal[full.ds$category_type == "school_safety_measures"]) #31551


###TESTING INDICATOR ###
#(no type)
testing.indic.all <- c("smoothed_wtested_14d",
                       "smoothed_wtested_positive_14d",
                       "smoothed_wscreening_tested_positive_14d",
                       "smoothed_whad_covid_ever",
                       "smoothed_wwanted_test_14d")
testing.indic.match <- intersect(sig.names, testing.indic.all)
testing.indic.match #5/5 signals

full.ds$category_type[full.ds$signal %in% testing.indic.match] <-"testing_indicators"
unique(full.ds$category_type)
length(full.ds$signal[full.ds$category_type == "testing_indicators"]) #114484

### VACCINATION INDICATORS ###
#vaccine_uptake_and_acceptance
uptake.acceptance.all <- c("smoothed_wcovid_vaccinated_appointment_or_accept",
                           "smoothed_wcovid_vaccinated_or_accept",
                           "smoothed_wappointment_or_accept_covid_vaccine",
                           "smoothed_waccept_covid_vaccine_no_appointment",
                           "smoothed_waccept_covid_vaccine",
                           "smoothed_wcovid_vaccinated",
                           "smoothed_wappointment_not_vaccinated",
                           "smoothed_wcovid_vaccinated_friends",
                           "smoothed_wtry_vaccinate_1m",
                           "smoothed_wflu_vaccinated_2021")
uptake.acceptance.match <- intersect(sig.names, uptake.acceptance.all)
uptake.acceptance.match #10/10 signals

full.ds$category_type[full.ds$signal %in% uptake.acceptance.match] <-"vaccine_uptake_and_acceptance"
unique(full.ds$category_type)
length(full.ds$signal[full.ds$category_type == "vaccine_uptake_and_acceptance"]) #161672

#vaccine_uptake_and_acceptance_for_children
uptake.accept.children.all <- c("smoothed_wvaccinate_child_oldest",
                                "smoothed_wchild_vaccine_already",
                                "smoothed_wchild_vaccine_yes_def",
                                "smoothed_wchild_vaccine_yes_prob",
                                "smoothed_wchild_vaccine_no_prob",
                                "smoothed_wchild_vaccine_no_def",
                                "smoothed_wvaccinate_children")
uptake.accept.children.match <- intersect(sig.names, uptake.accept.children.all)
uptake.accept.children.match #7/7 signals

full.ds$category_type[full.ds$signal %in% uptake.accept.children.match] <-"vaccine_uptake_and_acceptance_for_children"
unique(full.ds$category_type)
length(full.ds$signal[full.ds$category_type == "vaccine_uptake_and_acceptance_for_children"]) #33720

#vaccine_doses
doses.all <- c("smoothed_winitial_dose_one_of_one",
               "smoothed_winitial_dose_one_of_two",
               "smoothed_winitial_dose_two_of_two",
               "smoothed_wvaccinated_one_booster",
               "smoothed_wvaccinated_two_or_more_boosters",
               "smoothed_wvaccinated_no_booster",
               "smoothed_wvaccinated_at_least_one_booster",
               "smoothed_wvaccinated_booster_accept",
               "smoothed_wvaccinated_booster_hesitant",
               "smoothed_wvaccinated_booster_defyes",
               "smoothed_wvaccinated_booster_probyes",
               "smoothed_wvaccinated_booster_probno",
               "smoothed_wvaccinated_booster_defno",
               "smoothed_wreceived_2_vaccine_doses")
doses.all.match <- intersect(sig.names, doses.all)
doses.all.match #14/14 signals

full.ds$category_type[full.ds$signal %in% doses.all.match] <-"vaccine_doses"
unique(full.ds$category_type)
length(full.ds$signal[full.ds$category_type == "vaccine_doses"]) #84679

#barriers_to_accessing_vaccination
barriers.all <- c("smoothed_wvaccine_barrier_eligible_tried",
                  "smoothed_wvaccine_barrier_no_appointments_tried",
                  "smoothed_wvaccine_barrier_appointment_time_tried",
                  "smoothed_wvaccine_barrier_technical_difficulties_tried",
                  "smoothed_wvaccine_barrier_document_tried",
                  "smoothed_wvaccine_barrier_technology_access_tried",
                  "smoothed_wvaccine_barrier_travel_tried",
                  "smoothed_wvaccine_barrier_language_tried",
                  "smoothed_wvaccine_barrier_childcare_tried",
                  "smoothed_wvaccine_barrier_time_tried",
                  "smoothed_wvaccine_barrier_type_tried",
                  "smoothed_wvaccine_barrier_none_tried",
                  "smoothed_wvaccine_barrier_other_tried",
                  "smoothed_wvaccine_barrier_appointment_location_tried",
                  "smoothed_wvaccine_barrier_eligible",
                  "smoothed_wvaccine_barrier_no_appointments",
                  "smoothed_wvaccine_barrier_appointment_time",
                  "smoothed_wvaccine_barrier_technical_difficulties",
                  "smoothed_wvaccine_barrier_document",
                  "smoothed_wvaccine_barrier_technology_access",
                  "smoothed_wvaccine_barrier_travel",
                  "smoothed_wvaccine_barrier_language",
                  "smoothed_wvaccine_barrier_childcare",
                  "smoothed_wvaccine_barrier_time",
                  "smoothed_wvaccine_barrier_type",
                  "smoothed_wvaccine_barrier_none",
                  "smoothed_wvaccine_barrier_other",
                  "smoothed_wvaccine_barrier_appointment_location",
                  "smoothed_wvaccine_barrier_eligible_has",
                  "smoothed_wvaccine_barrier_no_appointments_has",
                  "smoothed_wvaccine_barrier_appointment_time_has",
                  "smoothed_wvaccine_barrier_technical_difficulties_has",
                  "smoothed_wvaccine_barrier_document_has",
                  "smoothed_wvaccine_barrier_technology_access_has",
                  "smoothed_wvaccine_barrier_travel_has",
                  "smoothed_wvaccine_barrier_language_has",
                  "smoothed_wvaccine_barrier_childcare_has",
                  "smoothed_wvaccine_barrier_time_has",
                  "smoothed_wvaccine_barrier_type_has",
                  "smoothed_wvaccine_barrier_none_has",
                  "smoothed_wvaccine_barrier_other_has",
                  "smoothed_wvaccine_barrier_appointment_location_has")
barriers.all.match <- intersect(sig.names, barriers.all)
barriers.all.match #42/42 signals

full.ds$category_type[full.ds$signal %in% barriers.all.match] <-"barriers_to_accessing_vaccination"
unique(full.ds$category_type)
length(full.ds$signal[full.ds$category_type == "barriers_to_accessing_vaccination"]) #310354


#reasons_for_vaccine_hesitancy
hesitancy.all <- c("smoothed_wworried_vaccine_side_effects", #I noticed that I made an error earlier excluding wworried instead of worried, has been fixed
                   "smoothed_whesitancy_reason_sideeffects",
                   "smoothed_whesitancy_reason_allergic",
                   "smoothed_whesitancy_reason_ineffective",
                   "smoothed_whesitancy_reason_unnecessary",
                   "smoothed_whesitancy_reason_dislike_vaccines_generally",
                   "smoothed_whesitancy_reason_dislike_vaccines",
                   "smoothed_whesitancy_reason_not_recommended",
                   "smoothed_whesitancy_reason_wait_safety",
                   "smoothed_whesitancy_reason_low_priority",
                   "smoothed_whesitancy_reason_cost",
                   "smoothed_whesitancy_reason_distrust_vaccines",
                   "smoothed_whesitancy_reason_distrust_gov",
                   "smoothed_whesitancy_reason_health_condition",
                   "smoothed_whesitancy_reason_pregnant",
                   "smoothed_whesitancy_reason_religious",
                   "smoothed_whesitancy_reason_other")
hesitancy.all.match <- intersect(sig.names, hesitancy.all)
length(hesitancy.all)
hesitancy.all.match #17/17 signals

full.ds$category_type[full.ds$signal %in% hesitancy.all.match] <-"reasons_for_vaccine_hesitancy"
unique(full.ds$category_type)
length(full.ds$signal[full.ds$category_type == "reasons_for_vaccine_hesitancy"]) #310328


#reasons_for_believing_vaccine_is_unnecessary
unnecessary.all <- c("smoothed_wdontneed_reason_had_covid",
                     "smoothed_wdontneed_reason_dont_spend_time",
                     "smoothed_wdontneed_reason_not_high_risk",
                     "smoothed_wdontneed_reason_precautions",
                     "smoothed_wdontneed_reason_not_serious",
                     "smoothed_wdontneed_reason_not_beneficial",
                     "smoothed_wdontneed_reason_other")

unnecessary.all.match <- intersect(sig.names, unnecessary.all)
unnecessary.all.match #7/7
length(unnecessary.all)

full.ds$category_type[full.ds$signal %in% unnecessary.all.match] <-"reasons_for_believing_vaccine_is_unnecessary"
unique(full.ds$category_type)
length(full.ds$signal[full.ds$category_type == "reasons_for_believing_vaccine_is_unnecessary"]) #119049


#outreach_and_image
outreach.all <- c("smoothed_wvaccine_likely_friends",
                  "smoothed_wvaccine_likely_local_health",
                  "smoothed_wvaccine_likely_who",
                  "smoothed_wvaccine_likely_govt_health",
                  "smoothed_wvaccine_likely_politicians",
                  "smoothed_wvaccine_likely_doctors")

outreach.all.match <- intersect(sig.names, outreach.all)
outreach.all.match #6/6
length(outreach.all.match)

full.ds$category_type[full.ds$signal %in% outreach.all.match] <-"outreach_and_image"
unique(full.ds$category_type)
length(full.ds$signal[full.ds$category_type == "outreach_and_image"]) #55645


### MENTAL HEALTH INDICATORS ###
#(no type)
mental.all <- c("smoothed_wworried_finances",
                "smoothed_wanxious_7d",
                "smoothed_wdepressed_7d",
                "smoothed_wworried_catch_covid",
                "smoothed_wfelt_isolated_7d",
                "smoothed_wanxious_5d",
                "smoothed_wdepressed_5d",
                "smoothed_wfelt_isolated_5d",
                "smoothed_wworried_become_ill")
mental.all.match <- intersect(sig.names, mental.all)
mental.all.match #9/9
length(mental.all.match)

full.ds$category_type[full.ds$signal %in% mental.all.match] <-"mental_health_indicators"
unique(full.ds$category_type)
length(full.ds$signal[full.ds$category_type == "mental_health_indicators"]) #156141


### BELIEF,EXPERIENCE, AND INFORMATION INDICATORS
#beliefs_about_covid_19
belief.all <- c("smoothed_wbelief_masking_effective",
                "smoothed_wbelief_distancing_effective",
                "smoothed_wbelief_vaccinated_mask_unnecessary",
                "smoothed_wbelief_children_immune",
                "smoothed_wbelief_created_small_group",
                "smoothed_wbelief_govt_exploitation")
belief.all.match <- intersect(sig.names, belief.all)
belief.all.match #6/6
length(belief.all.match)

full.ds$category_type[full.ds$signal %in% belief.all.match] <-"beliefs_about_covid_19"
unique(full.ds$category_type)
length(full.ds$signal[full.ds$category_type == "beliefs_about_covid_19"]) #108178

#medical_care_experiences
medical.all <- c("smoothed_wdelayed_care_cost",
                 "smoothed_wrace_treated_fairly_healthcare")
medical.all.match <- intersect(sig.names, medical.all)
medical.all.match #2/2
length(medical.all.match)

full.ds$category_type[full.ds$signal %in% medical.all.match] <-"medical_care_experiences"
unique(full.ds$category_type)
length(full.ds$signal[full.ds$category_type == "medical_care_experiences"]) #40640


#sources_of_news
source.all <- c("smoothed_wreceived_news_local_health",
                "smoothed_wreceived_news_experts",
                "smoothed_wreceived_news_cdc",
                "smoothed_wreceived_news_govt_health",
                "smoothed_wreceived_news_politicians",
                "smoothed_wreceived_news_journalists",
                "smoothed_wreceived_news_friends",
                "smoothed_wreceived_news_religious",
                "smoothed_wreceived_news_none")
source.all.match <- intersect(sig.names, source.all)
source.all.match #9/9
length(source.all.match) 

full.ds$category_type[full.ds$signal %in% source.all.match] <-"sources_of_news"
unique(full.ds$category_type)
length(full.ds$signal[full.ds$category_type == "sources_of_news"]) #183015


#trusted_sources_of_information
trusted.all <- c("smoothed_wtrust_covid_info_doctors",
                 "smoothed_wtrust_covid_info_experts",
                 "smoothed_wtrust_covid_info_cdc",
                 "smoothed_wtrust_covid_info_govt_health",
                 "smoothed_wtrust_covid_info_politicians",
                 "smoothed_wtrust_covid_info_journalists",
                 "smoothed_wtrust_covid_info_friends",
                 "smoothed_wtrust_covid_info_religious")
trusted.all.match <- intersect(sig.names, trusted.all)
trusted.all.match #8/8
length(trusted.all.match) 

full.ds$category_type[full.ds$signal %in% trusted.all.match] <-"trusted_sources_of_information"
unique(full.ds$category_type)
length(full.ds$signal[full.ds$category_type == "trusted_sources_of_information"]) #162426


#desired_information
desired.all <- c("smoothed_wwant_info_covid_treatment",
                 "smoothed_wwant_info_vaccine_access",
                 "smoothed_wwant_info_vaccine_types",
                 "smoothed_wwant_info_covid_variants",
                 "smoothed_wwant_info_children_education",
                 "smoothed_wwant_info_mental_health",
                 "smoothed_wwant_info_relationships",
                 "smoothed_wwant_info_employment",
                 "smoothed_wwant_info_none")
desired.all.match <- intersect(sig.names, desired.all)
desired.all.match #9/9
length(desired.all.match) 

full.ds$category_type[full.ds$signal %in% desired.all.match] <-"desired_information"
unique(full.ds$category_type)
length(full.ds$signal[full.ds$category_type == "desired_information"]) #182925

#exploring the ones that yet to have a category type
unique(full.ds$signal[full.ds$category_type == ""]) #there are 5 signals that dont have a category

### COVID LIKE INDICATORS ###
#no type
covid.all <- c("smoothed_wcli", 
               "smoothed_whh_cmnty_cli",
               "smoothed_wili",
               "smoothed_wnohh_cmnty_cli")
covid.all.match <- intersect(sig.names, covid.all)
covid.all.match #4/4
length(covid.all.match) 

full.ds$category_type[full.ds$signal %in% covid.all.match] <-"covid_like_indicators"
unique(full.ds$category_type)
length(full.ds$signal[full.ds$category_type == "covid_like_indicators"]) #164948


### EPIDEMIOLOGICAL INDICATORS ###

#confirmed_hospitalizations
hosp <- c("confirmed_admissions_covid_1d_7dav")
full.ds$category_type[full.ds$signal %in% hosp] <-"confirmed_hospitalizations"
unique(full.ds$category_type)
length(full.ds$signal[full.ds$category_type == "confirmed_hospitalizations"]) #43289

#confirmed_deaths
death.name <- c("deaths_incidence_num")
full.ds$category_type[full.ds$signal %in% death.name] <-"confirmed_deaths"
unique(full.ds$category_type)
length(full.ds$signal[full.ds$category_type == "confirmed_deaths"]) #63863

unique(full.ds$signal[full.ds$category_type == ""]) #all of the signals have a category type 



#CREATING INDICATOR CATEGORY COLUMN (based on the category type)
#indicator_type: create a column that is named for which indicator it is "behavior", "testing", "vaccination", "mental_health", "belief_experience_and_information", "covid_like_illness", "epidemiological"
full.ds$indicator <- ""
behavior <- c("mask_use", "social_distancing_and_travel","schooling_type", "school_safety_measures")
testing <- c("testing_indicators")
vaccination <- c("vaccine_uptake_and_acceptance", "vaccine_uptake_and_acceptance_for_children",
                 "vaccine_doses", "barriers_to_accessing_vaccination", "reasons_for_vaccine_hesitancy",
                 "reasons_for_believing_vaccine_is_unnecessary", "outreach_and_image")
mental <- c("mental_health_indicators")
belief<- c("beliefs_about_covid_19", "medical_care_experiences","sources_of_news", "trusted_sources_of_information", "desired_information")
CLI <- c("covid_like_indicators")
epid <- c("confirmed_hospitalizations", "confirmed_deaths")

full.ds$indicator[full.ds$category_type %in% behavior] <-"behavior"
#testing to make sure it works 
unique(full.ds$indicator)
unique(full.ds$signal[full.ds$indicator == "behavior"]) # shows which signals are under this category
unique(full.ds$category_type[full.ds$indicator == "behavior"]) #shows the category type of those whose indicator is behavior (correct)

full.ds$indicator[full.ds$category_type %in% testing] <-"testing"
unique(full.ds$indicator)

full.ds$indicator[full.ds$category_type %in% vaccination] <-"vaccination"
unique(full.ds$indicator)

full.ds$indicator[full.ds$category_type %in% mental] <-"mental_health"
unique(full.ds$indicator)

full.ds$indicator[full.ds$category_type %in% belief] <-"belief_experience_and_information"
unique(full.ds$indicator)

full.ds$indicator[full.ds$category_type %in% CLI] <-"covid_like_illness"
unique(full.ds$indicator)

full.ds$indicator[full.ds$category_type %in% epid] <-"epidemiological"
unique(full.ds$indicator)

#testing to make sure the categories correspond to the right indicator
unique(full.ds$category_type[full.ds$indicator == "testing"])
unique(full.ds$category_type[full.ds$indicator == "vaccination"])
unique(full.ds$category_type[full.ds$indicator == "mental_health"])
unique(full.ds$category_type[full.ds$indicator == "belief_experience_and_information"])
unique(full.ds$category_type[full.ds$indicator == "covid_like_illness"])
unique(full.ds$category_type[full.ds$indicator == "epidemiological"])

#View(full.ds)

#### Removing any missing values ####
#remove the rows whose value in the column value is missing- DONT FORGET TO RUN THIS!
dim(full.ds) #2704700      18
cleaned.full.ds <- full.ds[complete.cases(full.ds$value), ]  #this removes the row number too
#we could also use na.omit(full.ds[full.ds$value,]) #this keeps the dataframe intact gives error occasionally 
dim(cleaned.full.ds) #2588589      18


#### Min-Max Normalization : FOR THE ORIGINAL FULL DATASET 191 SIGNALS####

#Min-Max normalization FOR THE ORIGINAL FULL DATASET 191 SIGNALS
# this can be used when graphing raw signals without any date constraints
# Caution: Do not use this when analyzing for multiple states
norm.full.ds <- cleaned.full.ds %>%
  group_by(signal) %>%
  mutate(norm.value = scale(value, center = min(value), scale = max(value) - min(value))) %>%
  ungroup()

summary(norm.full.ds$norm.value)
# V1        
# Min.   :0.0000  
# 1st Qu.:0.2167  
# Median :0.3612  
# Mean   :0.3769  
# 3rd Qu.:0.5236  
# Max.   :1.0000  

#summary for each of the signals
by(norm.full.ds$norm.value, norm.full.ds$signal, summary)

#time to be formatted in year/month/day 
norm.full.ds$new_time_value <- ymd(norm.full.ds$time_value)

#### TIME SERIES GRAPHS: Indicators and Categories (includes min-max normalization: norm.full.ds) ####
##Visualizations for the state of California
sub.ds <- subset(norm.full.ds, geo_value == "ca", indicator == "behavior")

##Linear Regression for each Indicator and Epidemiological outcomes for California
#scales = "free_y" so each panel has its own scale scales = "fixed" so all the panels have the same.
behavior.reg <- norm.full.ds %>%
  filter(geo_value == "ca") %>%
  filter(indicator == "behavior"| indicator == "epidemiological") %>%
  ggplot(aes(x = new_time_value, y = norm.value, color = as.factor(signal))) +
  geom_line(linewidth = 1.2, alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.2) + #CI se=TRUE size=1.5 adjusts the size of the CI 
  facet_wrap_paginate(~ signal, scales = "free_y", ncol = 5, nrow = 8) +
  labs(x = "Time", y = "Value", color = "Signal") +
  #scale_color_manual(values = viridis(nlevels(as.factor(signal)))) +
  scale_color_viridis_d() + 
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(), #removes grid lines
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(face = "bold")
  ) +
  scale_x_date(date_labels = "%b %Y")
length(unique(norm.full.ds$signal[norm.full.ds$indicator =="behavior"])) #36+2 = 38 signals total used for ncol and nrow^^

testing.reg <- norm.full.ds %>%
  filter(indicator == "testing"| indicator == "epidemiological") %>%
  ggplot(aes(x = new_time_value, y = norm.value, color = as.factor(signal))) +
  geom_line(linewidth = 1.2, alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.2) + #CI se=TRUE size=1.5 adjusts the size of the CI 
  facet_wrap_paginate(~ signal, scales = "free_y", ncol = 2, nrow = 4) +
  labs(x = "Time", y = "Value", color = "Signal") +
  #scale_color_manual(values = viridis(nlevels(as.factor(signal)))) +
  scale_color_viridis_d() + 
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(), #removes grid lines
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(face = "bold")
  ) +
  scale_x_date(date_labels = "%b %Y")
length(unique(norm.full.ds$signal[norm.full.ds$indicator =="testing"])) # 5+2 = 7 

vaccination.reg <- norm.full.ds %>%
  filter(indicator == "vaccination" | indicator == "epidemiological") %>%
  ggplot(aes(x = new_time_value, y = norm.value, color = as.factor(signal))) +
  geom_line(linewidth = 1.2, alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.2) + #CI se=TRUE size=1.5 adjusts the size of the CI 
  facet_wrap_paginate(~ signal, scales = "free_y", ncol = 15, nrow = 7) +
  labs(x = "Time", y = "Value", color = "Signal") +
  #scale_color_manual(values = viridis(nlevels(as.factor(signal)))) +
  scale_color_viridis_d() + 
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(), #removes grid lines
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(face = "bold")
  ) +
  scale_x_date(date_labels = "%b %Y")
length(unique(norm.full.ds$signal[norm.full.ds$indicator =="vaccination"])) #103 signals total used for ncol and nrow^^

mental.health.reg <- norm.full.ds %>%
  filter(indicator == "mental_health" | indicator == "epidemiological") %>%
  ggplot(aes(x = new_time_value, y = norm.value, color = as.factor(signal))) +
  geom_line(linewidth = 1.2, alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.2) + #CI se=TRUE size=1.5 adjusts the size of the CI 
  facet_wrap_paginate(~ signal, scales = "free_y", ncol = 2, nrow = 6) +
  labs(x = "Time", y = "Value", color = "Signal") +
  #scale_color_manual(values = viridis(nlevels(as.factor(signal)))) +
  scale_color_viridis_d() + 
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(), #removes grid lines
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(face = "bold")
  ) +
  scale_x_date(date_labels = "%b %Y")

length(unique(norm.full.ds$signal[norm.full.ds$indicator =="mental_health"])) #9 signals total used for ncol and nrow^^

belief.exp.reg <- norm.full.ds %>%
  filter(indicator == "belief_experience_and_information" | indicator == "epidemiological") %>%
  ggplot(aes(x = new_time_value, y = norm.value, color = as.factor(signal))) +
  geom_line(linewidth = 1.2, alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.2) + #CI se=TRUE size=1.5 adjusts the size of the CI 
  facet_wrap_paginate(~ signal, scales = "free_y", ncol = 4, nrow = 9) +
  labs(x = "Time", y = "Value", color = "Signal") +
  #scale_color_manual(values = viridis(nlevels(as.factor(signal)))) +
  scale_color_viridis_d() + 
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(), #removes grid lines
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(face = "bold")
  ) +
  scale_x_date(date_labels = "%b %Y")

length(unique(norm.full.ds$signal[norm.full.ds$indicator =="belief_experience_and_information"])) #34 signals total used for ncol and nrow^^

cli.reg <- norm.full.ds %>%
  filter(indicator == "covid_like_illness" | indicator == "epidemiological") %>%
  ggplot(aes(x = new_time_value, y = norm.value, color = as.factor(signal))) +
  geom_line(linewidth = 1.2, alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.2) + #CI se=TRUE size=1.5 adjusts the size of the CI 
  facet_wrap_paginate(~ signal, scales = "free_y", ncol = 2, nrow = 3) +
  labs(x = "Time", y = "Value", color = "Signal") +
  #scale_color_manual(values = viridis(nlevels(as.factor(signal)))) +
  scale_color_viridis_d() + 
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(), #removes grid lines
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(face = "bold")
  ) +
  scale_x_date(date_labels = "%b %Y")

length(unique(norm.full.ds$signal[norm.full.ds$indicator =="covid_like_illness"])) #4 signals total used for ncol and nrow^^

epi.reg <- norm.full.ds %>%
  filter(indicator == "epidemiological") %>%
  ggplot(aes(x = new_time_value, y = norm.value, color = as.factor(signal))) +
  geom_line(linewidth = 1.2, alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.2) + #CI se=TRUE size=1.5 adjusts the size of the CI 
  facet_wrap_paginate(~ signal, scales = "free_y", ncol = 1, nrow = 2) +
  labs(x = "Time", y = "Value", color = "Signal") +
  #scale_color_manual(values = viridis(nlevels(as.factor(signal)))) +
  scale_color_viridis_d() + 
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(), #removes grid lines
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(face = "bold")
  ) +
  scale_x_date(date_labels = "%b %Y")

length(unique(norm.full.ds$signal[norm.full.ds$indicator =="epidemiological"])) #2 signals total used for ncol and nrow^^

behavior.reg
testing.reg
vaccination.reg
mental.health.reg
belief.exp.reg
cli.reg
epi.reg


### Category Type Graphs ### 

#"vaccine_uptake_and_acceptance" 
v.uptake <- norm.full.ds %>%
  filter(category_type == "vaccine_uptake_and_acceptance" |
           category_type == "confirmed_hospitalizations" |
           category_type == "confirmed_deaths") %>%
  ggplot(aes(x = new_time_value, y = norm.value, color = as.factor(signal))) +
  geom_line(linewidth = 1.2, alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.2) + #CI se=TRUE size=1.5 adjusts the size of the CI 
  facet_wrap_paginate(~ signal, scales = "free_y", ncol = 2, nrow = 6) +
  labs(x = "Time", y = "Value", color = "Signal") +
  #scale_color_manual(values = viridis(nlevels(as.factor(signal)))) +
  scale_color_viridis_d() + 
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(), #removes grid lines
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(face = "bold")
  ) +
  scale_x_date(date_labels = "%b %Y")
length(unique(norm.full.ds$signal[norm.full.ds$category_type =="vaccine_uptake_and_acceptance"])) #12 signals total used for ncol and nrow^^



#"mental_health_indicators"  
m.heath <- norm.full.ds %>%
  filter(category_type == "mental_health_indicators" |
           category_type == "confirmed_hospitalizations" |
           category_type == "confirmed_deaths") %>%
  ggplot(aes(x = new_time_value, y = norm.value, color = as.factor(signal))) +
  geom_line(linewidth = 1.2, alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.2) + #CI se=TRUE size=1.5 adjusts the size of the CI 
  facet_wrap_paginate(~ signal, scales = "free_y", ncol = 2, nrow = 6) +
  labs(x = "Time", y = "Value", color = "Signal") +
  #scale_color_manual(values = viridis(nlevels(as.factor(signal)))) +
  scale_color_viridis_d() + 
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(), #removes grid lines
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(face = "bold")
  ) +
  scale_x_date(date_labels = "%b %Y")
length(unique(norm.full.ds$signal[norm.full.ds$category_type =="mental_health_indicators"])) #11 signals total used for ncol and nrow^^


#"beliefs_about_covid_19"
b.about <- norm.full.ds %>%
  filter(category_type == "beliefs_about_covid_19" |
           category_type == "confirmed_hospitalizations" |
           category_type == "confirmed_deaths") %>%
  ggplot(aes(x = new_time_value, y = norm.value, color = as.factor(signal))) +
  geom_line(linewidth = 1.2, alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.2) + #CI se=TRUE size=1.5 adjusts the size of the CI 
  facet_wrap_paginate(~ signal, scales = "free_y", ncol = 2, nrow = 4) +
  labs(x = "Time", y = "Value", color = "Signal") +
  #scale_color_manual(values = viridis(nlevels(as.factor(signal)))) +
  scale_color_viridis_d() + 
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(), #removes grid lines
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(face = "bold")
  ) +
  scale_x_date(date_labels = "%b %Y")
length(unique(norm.full.ds$signal[norm.full.ds$category_type =="beliefs_about_covid_19"])) #8 total signals total used for ncol and nrow^^


#"vaccine_uptake_and_acceptance_for_children" 
v.c.about <- norm.full.ds %>%
  filter(category_type == "vaccine_uptake_and_acceptance_for_children" |
           category_type == "confirmed_hospitalizations" |
           category_type == "confirmed_deaths") %>%
  ggplot(aes(x = new_time_value, y = norm.value, color = as.factor(signal))) +
  geom_line(linewidth = 1.2, alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.2) + #CI se=TRUE size=1.5 adjusts the size of the CI 
  facet_wrap_paginate(~ signal, scales = "free_y", ncol = 3, nrow = 3) +
  labs(x = "Time", y = "Value", color = "Signal") +
  #scale_color_manual(values = viridis(nlevels(as.factor(signal)))) +
  scale_color_viridis_d() + 
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(), #removes grid lines
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(face = "bold")
  ) +
  scale_x_date(date_labels = "%b %Y") #+ labs(title ="vaccine_uptake_and_acceptance_for_children")
length(unique(norm.full.ds$signal[norm.full.ds$category_type =="vaccine_uptake_and_acceptance_for_children"])) #9 total signals total used for ncol and nrow^^


#"covid_like_indicators" 
cli.ind <- norm.full.ds %>%
  filter(category_type == "covid_like_indicators" |
           category_type == "confirmed_hospitalizations" |
           category_type == "confirmed_deaths") %>%
  ggplot(aes(x = new_time_value, y = norm.value, color = as.factor(signal))) +
  geom_line(linewidth = 1.2, alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.2) + #CI se=TRUE size=1.5 adjusts the size of the CI 
  facet_wrap_paginate(~ signal, scales = "free_y", ncol = 2, nrow = 3) +
  labs(x = "Time", y = "Value", color = "Signal") +
  #scale_color_manual(values = viridis(nlevels(as.factor(signal)))) +
  scale_color_viridis_d() + 
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(), #removes grid lines
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(face = "bold")
  ) +
  scale_x_date(date_labels = "%b %Y") #+ labs(title ="vaccine_uptake_and_acceptance_for_children")
length(unique(norm.full.ds$signal[norm.full.ds$category_type =="covid_like_indicators"])) #9 total signals total used for ncol and nrow^^


#"medical_care_experiences"  
med.care <- norm.full.ds %>%
  filter(category_type == "medical_care_experiences" |
           category_type == "confirmed_hospitalizations" |
           category_type == "confirmed_deaths") %>%
  ggplot(aes(x = new_time_value, y = norm.value, color = as.factor(signal))) +
  geom_line(linewidth = 1.2, alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.2) + #CI se=TRUE size=1.5 adjusts the size of the CI 
  facet_wrap_paginate(~ signal, scales = "free_y", ncol = 2, nrow = 3) +
  labs(x = "Time", y = "Value", color = "Signal") +
  #scale_color_manual(values = viridis(nlevels(as.factor(signal)))) +
  scale_color_viridis_d() + 
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(), #removes grid lines
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(face = "bold")
  ) +
  scale_x_date(date_labels = "%b %Y") #+ labs(title ="vaccine_uptake_and_acceptance_for_children")
length(unique(norm.full.ds$signal[norm.full.ds$category_type =="medical_care_experiences"])) #4 total signals total used for ncol and nrow^^



#"reasons_for_believing_vaccine_is_unnecessary" 
reas.bel <- norm.full.ds %>%
  filter(category_type == "reasons_for_believing_vaccine_is_unnecessary" |
           category_type == "confirmed_hospitalizations" |
           category_type == "confirmed_deaths") %>%
  ggplot(aes(x = new_time_value, y = norm.value, color = as.factor(signal))) +
  geom_line(linewidth = 1.2, alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.2) + #CI se=TRUE size=1.5 adjusts the size of the CI 
  facet_wrap_paginate(~ signal, scales = "free_y", ncol = 3, nrow = 3) +
  labs(x = "Time", y = "Value", color = "Signal") +
  #scale_color_manual(values = viridis(nlevels(as.factor(signal)))) +
  scale_color_viridis_d() + 
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(), #removes grid lines
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(face = "bold")
  ) +
  scale_x_date(date_labels = "%b %Y") #+ labs(title ="vaccine_uptake_and_acceptance_for_children")
length(unique(norm.full.ds$signal[norm.full.ds$category_type =="reasons_for_believing_vaccine_is_unnecessary"])) #9 total signals total used for ncol and nrow^^


#"testing_indicators"  
test.ind <- norm.full.ds %>%
  filter(category_type == "testing_indicators" |
           category_type == "confirmed_hospitalizations" |
           category_type == "confirmed_deaths") %>%
  ggplot(aes(x = new_time_value, y = norm.value, color = as.factor(signal))) +
  geom_line(linewidth = 1.2, alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.2) + #CI se=TRUE size=1.5 adjusts the size of the CI 
  facet_wrap_paginate(~ signal, scales = "free_y", ncol = 2, nrow = 3) +
  labs(x = "Time", y = "Value", color = "Signal") +
  #scale_color_manual(values = viridis(nlevels(as.factor(signal)))) +
  scale_color_viridis_d() + 
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(), #removes grid lines
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(face = "bold")
  ) +
  scale_x_date(date_labels = "%b %Y") #+ labs(title ="vaccine_uptake_and_acceptance_for_children")
length(unique(norm.full.ds$signal[norm.full.ds$category_type =="testing_indicators"])) #9 total signals total used for ncol and nrow^^


#"reasons_for_vaccine_hesitancy"   
rea.vacc <- norm.full.ds %>%
  filter(category_type == "reasons_for_vaccine_hesitancy" |
           category_type == "confirmed_hospitalizations" |
           category_type == "confirmed_deaths") %>%
  ggplot(aes(x = new_time_value, y = norm.value, color = as.factor(signal))) +
  geom_line(linewidth = 1.2, alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.2) + #CI se=TRUE size=1.5 adjusts the size of the CI 
  facet_wrap_paginate(~ signal, scales = "free_y", ncol = 4, nrow = 5) +
  labs(x = "Time", y = "Value", color = "Signal") +
  #scale_color_manual(values = viridis(nlevels(as.factor(signal)))) +
  scale_color_viridis_d() + 
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(), #removes grid lines
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(face = "bold")
  ) +
  scale_x_date(date_labels = "%b %Y") #+ labs(title ="vaccine_uptake_and_acceptance_for_children")
length(unique(norm.full.ds$signal[norm.full.ds$category_type =="reasons_for_vaccine_hesitancy"])) #19 total signals total used for ncol and nrow^^


#"vaccine_doses"  
vacc.doses <- norm.full.ds %>%
  filter(category_type == "vaccine_doses" |
           category_type == "confirmed_hospitalizations" |
           category_type == "confirmed_deaths") %>%
  ggplot(aes(x = new_time_value, y = norm.value, color = as.factor(signal))) +
  geom_line(linewidth = 1.2, alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.2) + #CI se=TRUE size=1.5 adjusts the size of the CI 
  facet_wrap_paginate(~ signal, scales = "free_y", ncol = 4, nrow = 4) +
  labs(x = "Time", y = "Value", color = "Signal") +
  #scale_color_manual(values = viridis(nlevels(as.factor(signal)))) +
  scale_color_viridis_d() + 
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(), #removes grid lines
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(face = "bold")
  ) +
  scale_x_date(date_labels = "%b %Y") #+ labs(title ="vaccine_uptake_and_acceptance_for_children")
length(unique(norm.full.ds$signal[norm.full.ds$category_type =="vaccine_doses"])) #16 total signals total used for ncol and nrow^^


#"schooling_type"   
scho.type <- norm.full.ds %>%
  filter(category_type == "schooling_type" |
           category_type == "confirmed_hospitalizations" |
           category_type == "confirmed_deaths") %>%
  ggplot(aes(x = new_time_value, y = norm.value, color = as.factor(signal))) +
  geom_line(linewidth = 1.2, alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.2) + #CI se=TRUE size=1.5 adjusts the size of the CI 
  facet_wrap_paginate(~ signal, scales = "free_y", ncol = 2, nrow = 4) +
  labs(x = "Time", y = "Value", color = "Signal") +
  #scale_color_manual(values = viridis(nlevels(as.factor(signal)))) +
  scale_color_viridis_d() + 
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(), #removes grid lines
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(face = "bold")
  ) +
  scale_x_date(date_labels = "%b %Y") #+ labs(title ="vaccine_uptake_and_acceptance_for_children")
length(unique(norm.full.ds$signal[norm.full.ds$category_type =="schooling_type"])) #7 total signals total used for ncol and nrow^^


#"social_distancing_and_travel"  
soc.dis <- norm.full.ds %>%
  filter(category_type == "social_distancing_and_travel" |
           category_type == "confirmed_hospitalizations" |
           category_type == "confirmed_deaths") %>%
  ggplot(aes(x = new_time_value, y = norm.value, color = as.factor(signal))) +
  geom_line(linewidth = 1.2, alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.2) + #CI se=TRUE size=1.5 adjusts the size of the CI 
  facet_wrap_paginate(~ signal, scales = "free_y", ncol = 4, nrow = 4) +
  labs(x = "Time", y = "Value", color = "Signal") +
  #scale_color_manual(values = viridis(nlevels(as.factor(signal)))) +
  scale_color_viridis_d() + 
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(), #removes grid lines
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(face = "bold")
  ) +
  scale_x_date(date_labels = "%b %Y") #+ labs(title ="vaccine_uptake_and_acceptance_for_children")
length(unique(norm.full.ds$signal[norm.full.ds$category_type =="social_distancing_and_travel"])) #7 total signals total used for ncol and nrow^^


#"mask_use"  
m.use <- norm.full.ds %>%
  filter(category_type == "mask_use" |
           category_type == "confirmed_hospitalizations" |
           category_type == "confirmed_deaths") %>%
  ggplot(aes(x = new_time_value, y = norm.value, color = as.factor(signal))) +
  geom_line(linewidth = 1.2, alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.2) + #CI se=TRUE size=1.5 adjusts the size of the CI 
  facet_wrap_paginate(~ signal, scales = "free_y", ncol = 2, nrow = 3) +
  labs(x = "Time", y = "Value", color = "Signal") +
  #scale_color_manual(values = viridis(nlevels(as.factor(signal)))) +
  scale_color_viridis_d() + 
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(), #removes grid lines
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(face = "bold")
  ) +
  scale_x_date(date_labels = "%b %Y") #+ labs(title ="vaccine_uptake_and_acceptance_for_children")
length(unique(norm.full.ds$signal[norm.full.ds$category_type =="mask_use"])) #6 total signals total used for ncol and nrow^^


#"sources_of_news" 
source.news <- norm.full.ds %>%
  filter(category_type == "sources_of_news" |
           category_type == "confirmed_hospitalizations" |
           category_type == "confirmed_deaths") %>%
  ggplot(aes(x = new_time_value, y = norm.value, color = as.factor(signal))) +
  geom_line(linewidth = 1.2, alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.2) + #CI se=TRUE size=1.5 adjusts the size of the CI 
  facet_wrap_paginate(~ signal, scales = "free_y", ncol = 3, nrow = 3) +
  labs(x = "Time", y = "Value", color = "Signal") +
  #scale_color_manual(values = viridis(nlevels(as.factor(signal)))) +
  scale_color_viridis_d() + 
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(), #removes grid lines
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(face = "bold")
  ) +
  scale_x_date(date_labels = "%b %Y") #+ labs(title ="vaccine_uptake_and_acceptance_for_children")
length(unique(norm.full.ds$signal[norm.full.ds$category_type =="sources_of_news"])) #6 total signals total used for ncol and nrow^^


#"school_safety_measures"   
saf.mea <- norm.full.ds %>%
  filter(category_type == "school_safety_measures" |
           category_type == "confirmed_hospitalizations" |
           category_type == "confirmed_deaths") %>%
  ggplot(aes(x = new_time_value, y = norm.value, color = as.factor(signal))) +
  geom_line(linewidth = 1.2, alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.2) + #CI se=TRUE size=1.5 adjusts the size of the CI 
  facet_wrap_paginate(~ signal, scales = "free_y", ncol = 3, nrow = 5) +
  labs(x = "Time", y = "Value", color = "Signal") +
  #scale_color_manual(values = viridis(nlevels(as.factor(signal)))) +
  scale_color_viridis_d() + 
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(), #removes grid lines
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(face = "bold")
  ) +
  scale_x_date(date_labels = "%b %Y") #+ labs(title ="vaccine_uptake_and_acceptance_for_children")
length(unique(norm.full.ds$signal[norm.full.ds$category_type =="school_safety_measures"])) #6 total signals total used for ncol and nrow^^


#"trusted_sources_of_information"  
trus.sources <- norm.full.ds %>%
  filter(category_type == "trusted_sources_of_information" |
           category_type == "confirmed_hospitalizations" |
           category_type == "confirmed_deaths") %>%
  ggplot(aes(x = new_time_value, y = norm.value, color = as.factor(signal))) +
  geom_line(linewidth = 1.2, alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.2) + #CI se=TRUE size=1.5 adjusts the size of the CI 
  facet_wrap_paginate(~ signal, scales = "free_y", ncol = 2, nrow = 5) +
  labs(x = "Time", y = "Value", color = "Signal") +
  #scale_color_manual(values = viridis(nlevels(as.factor(signal)))) +
  scale_color_viridis_d() + 
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(), #removes grid lines
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(face = "bold")
  ) +
  scale_x_date(date_labels = "%b %Y") #+ labs(title ="vaccine_uptake_and_acceptance_for_children")
length(unique(norm.full.ds$signal[norm.full.ds$category_type =="trusted_sources_of_information"])) #6 total signals total used for ncol and nrow^^


#"barriers_to_accessing_vaccination"   
barr.acc <- norm.full.ds %>%
  filter(category_type == "barriers_to_accessing_vaccination" |
           category_type == "confirmed_hospitalizations" |
           category_type == "confirmed_deaths") %>%
  ggplot(aes(x = new_time_value, y = norm.value, color = as.factor(signal))) +
  geom_line(linewidth = 1.2, alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.2) + #CI se=TRUE size=1.5 adjusts the size of the CI 
  facet_wrap_paginate(~ signal, scales = "free_y", ncol = 4, nrow = 11) +
  labs(x = "Time", y = "Value", color = "Signal") +
  #scale_color_manual(values = viridis(nlevels(as.factor(signal)))) +
  scale_color_viridis_d() + 
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(), #removes grid lines
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(face = "bold")
  ) +
  scale_x_date(date_labels = "%b %Y") #+ labs(title ="vaccine_uptake_and_acceptance_for_children")
length(unique(norm.full.ds$signal[norm.full.ds$category_type =="barriers_to_accessing_vaccination"])) #6 total signals total used for ncol and nrow^^



#"outreach_and_image" 
out.im <- norm.full.ds %>%
  filter(category_type == "outreach_and_image" |
           category_type == "confirmed_hospitalizations" |
           category_type == "confirmed_deaths") %>%
  ggplot(aes(x = new_time_value, y = norm.value, color = as.factor(signal))) +
  geom_line(linewidth = 1.2, alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.2) + #CI se=TRUE size=1.5 adjusts the size of the CI 
  facet_wrap_paginate(~ signal, scales = "free_y", ncol = 2, nrow = 4) +
  labs(x = "Time", y = "Value", color = "Signal") +
  #scale_color_manual(values = viridis(nlevels(as.factor(signal)))) +
  scale_color_viridis_d() + 
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(), #removes grid lines
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(face = "bold")
  ) +
  scale_x_date(date_labels = "%b %Y") #+ labs(title ="vaccine_uptake_and_acceptance_for_children")
length(unique(norm.full.ds$signal[norm.full.ds$category_type =="outreach_and_image"])) #6 total signals total used for ncol and nrow^^


#"desired_information"   
des.inf <- norm.full.ds %>%
  filter(category_type == "desired_information" |
           category_type == "confirmed_hospitalizations" |
           category_type == "confirmed_deaths") %>%
  ggplot(aes(x = new_time_value, y = norm.value, color = as.factor(signal))) +
  geom_line(linewidth = 1.2, alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.2) + #CI se=TRUE size=1.5 adjusts the size of the CI 
  facet_wrap_paginate(~ signal, scales = "free_y", ncol = 3, nrow = 4) +
  labs(x = "Time", y = "Value", color = "Signal") +
  #scale_color_manual(values = viridis(nlevels(as.factor(signal)))) +
  scale_color_viridis_d() + 
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(), #removes grid lines
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(face = "bold")
  ) +
  scale_x_date(date_labels = "%b %Y") #+ labs(title ="vaccine_uptake_and_acceptance_for_children")
length(unique(norm.full.ds$signal[norm.full.ds$category_type =="desired_information"])) #6 total signals total used for ncol and nrow^^




##### DATA: POST 2021-2022 #####
#used for capstone presentation  
ca.sigs <- list()
states <- list("ca","tx")
num_states <- length(states)
indicator.names <- unique(norm.full.ds$indicator)
signals.states.list <- vector("list", length = num_states)
number.index = 1
for (state in states){
  counter <- 1
  for (individual in indicator.names){
    # Filter the dataset for specific state
    state.ds <- norm.full.ds %>% 
      #mutate(new_time_value = as.Date(time_value)) %>%
      filter(geo_value == state, indicator == individual)
    
    # rewrite  date column
    state.ds <- state.ds %>%
      mutate(new_time_value = as.Date(time_value))
    
    # Arrange the data by date within each signal
    filtered.ds <- state.ds %>%
      group_by(signal) %>%
      arrange(new_time_value) %>%
      select(new_time_value,signal, norm.value)
    
    # Pivot the data to wide format
    wide.ds <- filtered.ds %>%
      pivot_wider(names_from = signal, values_from = norm.value, values_fn = list) # this allows there to be multiple norm.value for a single date
    
    # filter by date
    dates.ds <-  wide.ds %>%
      filter(new_time_value >= as.Date("2021-05-20") & new_time_value <= as.Date("2022-06-25"))
    
    df <- as.data.frame(dates.ds)
    
    # Filter out columns with NULL values
    comp.wide.ds <- df[, apply(df, 2, function(col) !any(sapply(col,is.null)))]
    #View(comp.wide.ds)
    
    ca.sigs[[counter]] <- colnames(comp.wide.ds[,-1])
    counter = counter +1
    
  }
  signals.states.list[[number.index]] <- ca.sigs
  number.index =  number.index + 1
}

View(signals.states.list)
sigs <- signals.states.list[[1]][1:7] #86 signals
sig.list <- unlist(sigs)


##### Global Variables: Signals, States, Wide-Format (Min-Max) ####
#26 signals used
sigs.final <- c("smoothed_whesitancy_reason_cost","smoothed_whesitancy_reason_distrust_gov"         
                ,"smoothed_whesitancy_reason_ineffective"        ,"smoothed_whesitancy_reason_low_priority"         
                ,"smoothed_whesitancy_reason_religious"          ,"smoothed_whesitancy_reason_sideeffects"       
                , "smoothed_whesitancy_reason_wait_safety"       ,"smoothed_wdontneed_reason_dont_spend_time"       
                , "smoothed_wdontneed_reason_had_covid"          ,"smoothed_wdontneed_reason_precautions"           
                , "smoothed_waccept_covid_vaccine_no_appointment"            
                , "smoothed_wworried_catch_covid"                ,"smoothed_wbelief_created_small_group"            
                , "smoothed_wbelief_distancing_effective"        ,"smoothed_wbelief_govt_exploitation"              
                , "smoothed_wdelayed_care_cost"                  ,"smoothed_wrace_treated_fairly_healthcare"        
                , "smoothed_wtrust_covid_info_friends"           ,"smoothed_wtrust_covid_info_govt_health"          
                , "smoothed_wtrust_covid_info_religious"         , "smoothed_whh_cmnty_cli"                                             
                , "smoothed_wpublic_transit_1d"                     
                , "smoothed_wwearing_mask_7d"                                    
                , "smoothed_wspent_time_indoors_1d"                      
                ,"deaths_incidence_num"                            
                , "confirmed_admissions_covid_1d_7dav" )

#Removing this signal as of Jul 20:"smoothed_wtrust_covid_info_cdc", 26 official signals to work with 
#reason: extremely similair trends with trust in gov --> it's redundant to have


trial.signals<- sigs.final #renaming the sig names because trial.signals is used in the implementation of the methods. 
a <- choose(length(trial.signals), 2) # helper variable to see how long the output.list is gonna be
states <- list("ca", "tx")#Specify the extact states
num_signals <- length(trial.signals)
num_states <- length(states)
#the date constraints
date.min <- "2021-05-20" 
date.max <- "2022-06-25"

cleaned.full.ds$new_time_value <- ymd(cleaned.full.ds$time_value)

## This is independent min-max normalization DAILY !!! (takes state, signal, and geo_value into account)
post.norm.full.ds <- cleaned.full.ds %>%
        #add this line if normalizing only by state
        filter(new_time_value >= as.Date("2021-05-20") & new_time_value <= as.Date("2022-06-25")) %>%
        filter(geo_value %in% states)%>% #filter(geo_value %in% unique(cleaned.full.ds$geo_value))%>% ##CURRENTLY DOES SELECTED STATES
        arrange(geo_value) %>%
        group_by(signal, geo_value) %>%
        mutate(norm.value = scale(value, center = min(value), scale = max(value) - min(value))) %>%
        ungroup()

summary(post.norm.full.ds$norm.value)

# V1        
# Min.   :0.0000  
# 1st Qu.:0.2604  
# Median :0.4350  
# Mean   :0.4435  
# 3rd Qu.:0.6160  
# Max.   :1.0000  
# NA's   :41 
#summary for each of the signals
by(post.norm.full.ds$norm.value, post.norm.full.ds$signal, summary)


## Use this for FULL STATES long format for plot visualization (not arranged by date)
#has the signals from sigs.final
full.states.long.ds <- post.norm.full.ds%>% #weekly.norm.full.ds %>% #post.norm.full.ds %>%
  filter(geo_value %in% states)%>%
  mutate(new_time_value = as.Date(time_value)) %>%
  #DATE
  filter(new_time_value >= as.Date(date.min) & new_time_value <= as.Date(date.max)) %>% 
  arrange(geo_value) %>%
  filter(signal %in% sigs.final) %>%
  select(new_time_value,geo_value,signal, norm.value)


by(full.states.long.ds$norm.value, full.states.long.ds$signal, summary)
by(full.states.long.ds$new_time_value, full.states.long.ds$signal, summary) #o check the start and end dates for the signals

#his upcomign line is not needed for the daily analysis
#length(ifelse(full.states.long.ds$new_time_value %in% weekly.dates, TRUE, FALSE))#THEY DO have the same dates : JAN 28


##04/03/2024
#ADDING VARIABLE TYPE
full.states.long.ds$variable_type <- ""
trust_intervention <- c("smoothed_wbelief_distancing_effective",
                        "smoothed_wdontneed_reason_had_covid",
                        "smoothed_wspent_time_indoors_1d", 
                        "smoothed_wwearing_mask_7d",
                        "smoothed_wdontneed_reason_precautions",
                        "smoothed_waccept_covid_vaccine_no_appointment",
                        "smoothed_wpublic_transit_1d",
                        "smoothed_wdontneed_reason_dont_spend_time")

source_trust <- c("smoothed_wbelief_govt_exploitation",
                  "smoothed_wdelayed_care_cost",
                  "smoothed_wtrust_covid_info_govt_health", 
                  "smoothed_wbelief_created_small_group",
                  "smoothed_wrace_treated_fairly_healthcare",
                  "smoothed_wtrust_covid_info_religious",
                  "smoothed_wtrust_covid_info_friends")

risk_perception <- c("smoothed_wworried_catch_covid",
                  "smoothed_whesitancy_reason_distrust_gov",
                  "smoothed_whesitancy_reason_religious", 
                  "smoothed_whesitancy_reason_ineffective",
                  "smoothed_whesitancy_reason_sideeffects",
                  "smoothed_whesitancy_reason_cost",
                  "smoothed_whesitancy_reason_low_priority",
                  "smoothed_whesitancy_reason_wait_safety")

epi_outcomes <- c("smoothed_whh_cmnty_cli",
                  "confirmed_admissions_covid_1d_7dav",
                  "deaths_incidence_num")

#adding them to the full.states.long ds 
full.states.long.ds$variable_type[full.states.long.ds$signal %in% trust_intervention] <-"trust in intervention"
full.states.long.ds$variable_type[full.states.long.ds$signal %in% source_trust] <-"sources of trust"
full.states.long.ds$variable_type[full.states.long.ds$signal %in% risk_perception] <-"perception of risk"
full.states.long.ds$variable_type[full.states.long.ds$signal %in% epi_outcomes] <-"epidemiological outcomes"

#CLEANING UP SIGNAL NAMES
#04/03/2024
full.states.long.ds$name_signal <-""
full.states.long.ds$name_signal <- ifelse(grepl("smoothed", full.states.long.ds$signal),
                         substring(full.states.long.ds$signal, 11),
                         substring(full.states.long.ds$signal, 0))
rename.sigs <- c("accept covid vaccine no appointment", "belief: created by small group"      ,    "belief: distancing is effective"      ,  
                  "belief: govt exploiting covid"            ,"delayed medical care: cost"                ,   "dont need vaccine: dont spend time"   , 
                  "dont need vaccine: had covid"           ,"dont need vaccine: other precautions"      ,   "vaccine hesitancy: worried cost"             , 
                  "vaccine hesitancy: distrust gov"     ,  "vaccine hesitancy: ineffective"   ,     "vaccine hesitancy: low priority"   ,   
                  "vaccine hesitancy: against religion"        ,  "vaccine hesitancy: worried side effects"   ,     "vaccine hesitancy: wait to see if safe"    ,   
                  "covid-like illness in community"    ,  "used public transit"              ,     "race are treated fairly in healthcare"  ,   
                  "socializing indoors"             ,  "trust: friends & family for covid info"       ,     "trust: govt health for covid info"    ,   
                  "trust: religious leaders for covid info"        ,  "wear mask in public"                ,     "worried about catching covid"             ,   
                  "covid hospital admissions",  "covid confirmed deaths"  )

#MOST IMPORTANT: renames the signals to the shorten/audience names
full.states.long.ds$variable_names <- rename.sigs[match(full.states.long.ds$signal,unique(full.states.long.ds$signal))]
  
#testing to make sure it works 
unique(full.states.long.ds$variable_type)
unique(full.states.long.ds$signal[full.states.long.ds$variable_type == "trust in intervention"]) # shows which signals are under this 
unique(full.states.long.ds$category_type[full.states.long.ds$variable_type == "trust_intervention"]) #shows the category type


#### POSTER VISUALIZATIONS ####
#Figure 1: Variable by Category
df.distinct <- full.states.long.ds %>% distinct(variable_names,variable_type)
un.distinct <- as.data.frame(unique(df.distinct))

first.app <- c("epidemiological outcomes", "trust in intervention", "perception of risk", "sources of trust")
df.distinct$variable_type <- factor(df.distinct$variable_type, levels = first.app)

df.distinct.order <- df.distinct %>%
  dplyr::arrange(variable_type)

df.distinct.order$name_signal <- factor(df.distinct.order$variable_names, levels = unique(df.distinct.order$variable_names))
#IMPORTANT COLORS #04/03/2024
#var_type_colors <- c("#0055A4","#FF9B56", "#58b1ff","#AAAAAA")
#CHANGING THE TYPE COLORS
var_type_colors <- c("#A78FCF","#8DADD8", "#D5D5DC", "#F0D77B" )
# Plot to visualize the variable by category 
o <- ggplot(df.distinct.order, aes(x = variable_type, y = name_signal, color = variable_type)) +
  geom_point(size = 4) +
  scale_color_manual(values = var_type_colors) +  # Use the defined color palette
  labs(title = "Variable by Category",
       x = "Category", # 04/07/2024: Changing variable type name to category because its confusing (not to be confused with category_type defined by Delphi)
       y = "Variable",
       color = "Legend") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) #+
#theme(plot.title = element_text(hjust = 0.5))#, text=element_text(family="Times New Roman"))
o
o+theme(legend.position = "right", legend.box = "vertical") + guides(color = guide_legend(nrow=4))


#04/01
# Figure 2: Time Series: Covid-Like Illnes in Community & Socializing Indoors
df.sigs <- full.states.long.ds %>%
  filter(geo_value == "ca", signal == "smoothed_whh_cmnty_cli" | signal == "smoothed_wspent_time_indoors_1d") %>%
  group_by(signal) %>%
  arrange(new_time_value, .by_group = TRUE)%>% #arrange by the dates within each group
  select(new_time_value,norm.value,signal) %>%
  pivot_wider(names_from = signal, values_from = norm.value) #this pivots the table output to have the signals be the columns and the rows be the norm vals
df.sigs
#smoothed_whh_cmnty_cli
#smoothed_wspent_time_indoors_1d
#smoothed_waccept_covid_vaccine_no_appointment | smoothed_wpublic_transit_1d #this was a close second 
#plot of two signals
#"Covid-Like Ilnnes in Community"
#"Spent Time Indoors with Others"
#"Indoors with Non-Household Members"

p<- ggplot(df.sigs, aes(x= new_time_value)) +
  geom_line(aes(y=smoothed_wspent_time_indoors_1d, color = "Socializing Indoors"), linewidth = 1.3)  +
  geom_line(aes(y=smoothed_whh_cmnty_cli, color = "Covid-Like Illness in Community"), linewidth = 1.3) +
  scale_color_manual(values = c("purple4", "steelblue"))+
  labs(color = "Variables:") +
  theme_bw()+
  ggtitle("Time Series: Covid-Like Illness in Community & Socializing Indoors")+
  xlab("Time (Days)")+
  ylab("Value of the Respondents (Min-Max Normalization)")+
  #xlim(c(min(df.sigs$new_time_value),max(df.sigs$new_time_value)))+
  theme(plot.title = element_text(hjust = 0.5))#, text=element_text(family="Times New Roman"))
#
p+theme(legend.position = "bottom", legend.box = "vertical") + guides(color = guide_legend(nrow=1))

# Figure 4: Dynamic Time Warping(A,B)
# Perform the dtw alignment and store the result
dtw_result <- dtw(df.sigs$smoothed_whh_cmnty_cli, df.sigs$smoothed_wspent_time_indoors_1d, keep = TRUE, window.type = "sakoechiba", window.size = c(21,21))
dtw_result_v2v1 <- dtw(df.sigs$smoothed_wspent_time_indoors_1d, df.sigs$smoothed_whh_cmnty_cli, keep = TRUE, window.type = "sakoechiba", window.size = c(21,21))
# Plot the dtw_result with both colors and a bolder line width (lwd)
plot(dtw_result, type = "twoway", col = c("purple4", "steelblue"), lwd = 2, xlab="Time (Days)",  main = "DTW: Variable A & Variable B", ylab= "Value of the Respondents")
legend("topleft", legend = c("A: Covid-Like Illness in Community (query)", "B: Socializing Indoors (reference)"), col = c("purple4", "steelblue"), lty = 1, cex = 1.5)
#04/03 tried adding dates for dtw plot but it doesnt work 
#dtw_result_v2v1$dates <- as.Date(df.sigs$new_time_value, "%Y/%m/%d")

#Figure 5: Dynamic Time Warping(B,A)
plot(dtw_result_v2v1, type = "twoway", col = c("steelblue", "purple4"), lwd = 2, xlab="Time (Days)", main = "DTW: Variable B & Variable A", ylab= "Value of the Respondents")
legend("topleft", legend = c("B: Socializing Indoors (query)","A: Covid-Like Illness in Community (reference)"), col = c("steelblue", "purple4"), lty = 1, cex = 1.5)
#axis(1,dtw_result_v2v1$dates, format(dtw_result_v2v1$dates, "%m -%d - %Y"), cex.axis= 0.7)
min(df.sigs$new_time_value)

#Figure 3: DTW Alignment Path & heat map
dtwPlotThreeWay(dtw_result, xts = df.sigs$smoothed_whh_cmnty_cli, yts = df.sigs$smoothed_wspent_time_indoors_1d,
                type.align = "l", margin =4, inner.margin = 0.2, title.margin = 0, 
                main = "DTW: Alignment Path",ylab= "Socializing Indoors (days)", 
                xlab= "Covid-Like Illness in Community (days)")

#margin = 4, inner.margin = 0.2, title.margin = 1.5
#Best display of alignments
#install.packages("fields")
library("fields")
al <- dtw_result$localCostMatrix
plot.heat<- image(x=seq_len(nrow(al)), y=seq_len(ncol(al)), al)
plot.heat
#palette_used <- attr(plot.heat, "palette")

#image(x=seq_len(nrow(al)), y=seq_len(ncol(al)),al, col=palette_used)
#legend("topright", legend = seq_len(ncol(al)), fill = heat.colors(ncol(al)), title = "Distance")
custom_palette <- colorRampPalette(c("yellow", "orange", "red", "darkred"))(ncol(al))
image.plot(x=seq_len(nrow(al)), y=seq_len(ncol(al)),al,col = custom_palette, legend.lab ="distance", legend.cex = 1.5)
#legend("right", legend = "Legend", fill = custom_palette, title = "Distance Scale", horiz = TRUE)

#vertical.image.legend(al)
text(row(al),col(al),label = al, cex= 0.02)
lines(dtw_result$index1,dtw_result$index2, lwd=2.5)

## extra help when learning how to visualize
# dtwPlotThreeWay(dtw_result, xts = df.sigs$smoothed_wspent_time_indoors_1d, yts = df.sigs$smoothed_whh_cmnty_cli, type.align = "l", main = "Alignment Path: Local Cost", sub= "Figure 2", xlab= "Covid-Like Illness in Community", ylab="Spent Time Indoors with Others")
# #Best display of alignments
# al <- alignment_figuring$localCostMatrix
# image(x=seq_len(nrow(al)), y=seq_len(ncol(al)),al)
# text(row(al),col(al),label = al, cex= 0.02)
# lines(alignment_figuring$index1,alignment_figuring$index2)

#dtw- regular exploration

# plot(dtw(var1[[1]],var2[[1]], keep=TRUE, window.type="sakoechiba", window.size= 21),type="twoway")
# plot(dtw(var1[[1]],-1*var2[[1]]+1, keep=TRUE, window.type="sakoechiba", window.size=21),type="twoway")

# note: 03/10: helpful in understanding what's happening with the constraints
#plot(dtw(var1[[1]],var2[[1]], keep=TRUE, window.type="sakoechiba", window.size=402),type="twoway")
#dtw(var1[[1]],var2[[1]], keep=TRUE, window.type="sakoechiba", window.size=0)$distance


#plot of 2 sigs: cmnt_cli & spent time indoors for ca
# alignment_figuring <- dtw(var1,var2, keep=TRUE, window.type="sakoechiba", window.size=c(21,21)) 
# plot(alignment_figuring, type= "twoway")
# #blue.pal <- colorRampPalette(c("lightblue","blue"))
# dtwPlot(alignment_figuring, type = "density", main = "Alignment Path: \n Window Constraint of 21 days", sub= "Figure 1", xlab= "Covid-Like Illness in Community", ylab="Indoors with Non-Household Members")
# lines(0:alignment_figuring$N, 0:alignment_figuring$N, col = "black", lty = 1)
# 
# dtwPlotAlignment(alignment_figuring, main = "Alignment Path: \n Window Constraint of 21 days", sub= "Figure 1", xlab= "Covid-Like Illness in Community", ylab="Indoors with Non-Household Members")
# lines(0:alignment_figuring$N +21, col = "red", lty = 2) 
# lines(0:alignment_figuring$N -21, col = "red", lty = 2)
# 
# dtwPlotDensity(alignment_figuring)
#dtwPlotThreeWay(dtw_result, xts = df.sigs$smoothed_whh_cmnty_cli, yts = df.sigs$smoothed_wspent_time_indoors_1d, type.align = "l", main = "Alignment Path: Local Cost", sub= "Figure 2",  ylab= "Indoors with Non-Household Members" , xlab= "Covid-Like Illness in Community")
#hi <- (1:length(dtw_result$index1))[(dtw_result$index1 %in% round(0:21))]

# 
# dtwPlotThreeWay(dtw_result, xts = df.sigs$smoothed_whh_cmnty_cli, yts = df.sigs$smoothed_wspent_time_indoors_1d,
#                 type.align = "p", type.ts = "l", margin =4, inner.margin = 0.2, title.margin = 0, 
#                 main = "Alignment Path: Local Cost",ylab= "Indoors with Non-Household Members", 
#                 xlab= "Covid-Like Illness in Community")

#04/04/24 
#HELPFUL MATCH INDICES EXAMPLE FROM CRAN
# idx<-seq(0,6.28,len=100);
# query<-sin(idx)+runif(100)/10;
# reference<-cos(idx)
# dtw(query,reference,keep=TRUE)->alignment;
# ## Beware of the reference's y axis, may be confusing
# ## Equivalent to plot(alignment,type="three");
# dtwPlotThreeWay(alignment);
# ## Highlight matches of chosen QUERY indices. We will do some index
# ## arithmetics to recover the corresponding indices along the warping
# ## curve
# hq <- (0:8)/8
# hq <- round(hq*100) # indices in query for pi/4 .. 7/4 pi
# hw <- (alignment$index1 %in% hq) # where are they on the w. curve?
# hi <- (1:length(alignment$index1))[hw]; # get the indices of TRUE elems
# dtwPlotThreeWay(alignment,match.indices=hi);
# alignment_figuring <- dtw(state_signals[[21]],state_signals[[24]], keep=TRUE, window.type="sakoechiba", window.size=c(21,21)) 
# plot(alignment_figuring, type= "twoway")
# 
# dtwPlot(alignment_figuring, type = "density", main = "Alignment Path")
# lines(0:alignment_figuring$N, 0:alignment_figuring$N, col = "black", lty = 1)
# 
# dtwPlotAlignment(alignment_figuring)
# lines(0:alignment_figuring$N, 0:alignment_figuring$N, col = "red", lty = 1) #this line is added to see if the alignment path happens on both sides or just one 
# dtwPlotDensity(alignment_figuring)
# dtwPlotThreeWay(alignment_figuring, xts = state_signals[[21]], yts = state_signals[[24]], type.align = "l")
# #Best display of alignments
# al <- alignment_figuring$localCostMatrix
# image(x=seq_len(nrow(al)), y=seq_len(ncol(al)),al)
# text(row(al),col(al),label = al, cex= 0.02)
# lines(alignment_figuring$index1,alignment_figuring$index2)

#more exploration that helped 
plot(dtw(state_signals[[21]],state_signals[[24]], keep=TRUE, window.type="sakoechiba", window.size=c(0,21)),type="twoway")
plot(dtw(state_signals[[21]],-1*state_signals[[24]]+1, keep=TRUE, window.type="sakoechiba", window.size=c(0,21)),type="twoway")

pos.dtw.distance <- 1/(dtw(state_signals[[21]], state_signals[[24]], window.type = "sakoechiba", window.size = c(0,21))$distance + 1) #dist.method = "Euclidean" by default
neg.dtw.distance <- 1/(dtw(state_signals[[21]], -1*state_signals[[24]], window.type = "sakoechiba", window.size = c(0,21))$distance + 1)
pos.dtw.distance
neg.dtw.distance

#### METHODS: DTW & Network Graphs ####
#install.packages("dtw")
library(dtw)

state.val.list <- vector("list", length = num_states)
output.list <- vector("list", length = num_states)
output.matrix_list <- vector("list", length = num_states)
#removes the smoothed part of the vocab for network graphs 
trial.signals <- c("accept_covid_vaccine_no_appointment", "belief_created_small_group"      ,    "belief_distancing_effective"      ,  
                  "belief_govt_exploitation"            ,"delayed_care_cost"                ,   "dontneed_reason_dont_spend_time"   , 
                  "dontneed_reason_had_covid"           ,"dontneed_reason_precautions"      ,   "hesitancy_reason_cost"             , 
                    "hesitancy_reason_distrust_gov"     ,  "hesitancy_reason_ineffective"   ,     "hesitancy_reason_low_priority"   ,   
                    "hesitancy_reason_religious"        ,  "hesitancy_reason_sideeffects"   ,     "hesitancy_reason_wait_safety"    ,   
                    "hh_cmnty_cli"                      ,  "public_transit_1d"              ,     "race_treated_fairly_healthcare"  ,   
                    "spent_time_indoors_1d"             ,  "trust_covid_info_friends"       ,     "trust_covid_info_govt_health"    ,   
                    "trust_covid_info_religious"        ,  "wearing_mask_7d"                ,     "worried_catch_covid"             ,   
                    "confirmed_admissions_covid_1d_7dav",  "deaths_incidence_num"  )
for (i in 1:num_states) {
  state <- states[[i]]
  state_signals <- list()
  state_output <- list()  # Initialize sublist for output
  
  for (j in 1:num_signals) {
    one.sig <- trial.signals[j]
    state_data <- full.states.long.ds %>% 
      filter(geo_value == state, name_signal == one.sig) %>%
      select(norm.value) ## ONLY NORM VALUE
    
    state_signals[[j]] <- state_data  # 26 signal values
  }
  
  index <- 1
  output.matrix <- matrix(NA, nrow = num_signals, ncol = num_signals, dimnames = list(paste0(states[i], "_", trial.signals),
                                                                                      paste0(states[i], "_", trial.signals)))
  for (k in 1:(num_signals - 1)) {
    for (l in (k + 1):num_signals) {
      var1 <- state_signals[[k]]
      var2 <- state_signals[[l]]
      
      
      # DTW DISTANCE
      
      #WE ARE ADDING ONE SO THAT IF THE SIGNAL IS THE SAME THE DISTANCE WILL BE ONE INSTEAD OF ZERO
      dtw.distance <- 1/(dtw(var1, var2, window.type = "sakoechiba", window.size = c(21,21))$distance + 1) #dist.method = "Euclidean" by default
      inverse.dtw.distance <- 1/(dtw(var1, (-1*var2)+1, window.type = "sakoechiba", window.size = c(21,21))$distance + 1) #changing window size from 21 days to 3 weeks
      
      v2.dtw.distance <- 1/(dtw(var2, var1, window.type = "sakoechiba", window.size = c(21,21))$distance + 1) #dist.method = "Euclidean" by default
      v2.inverse.dtw.distance <- 1/(dtw(var2, (-1*var1)+1, window.type = "sakoechiba", window.size = c(21,21))$distance + 1)
      
       #upper triangle 
      if(is.na(output.matrix[k,l])){
        #checks to see if non-inverse dtw > inverse dtw for var1,var2
        dist.val<- ifelse(dtw.distance > inverse.dtw.distance, dtw.distance, -1*inverse.dtw.distance)
        output.matrix[k,l] <- dist.val #puts var1,var2 in  [k,l]
        
        #bottom part of triangle 
        if(is.na(output.matrix[l,k])){
          #checks to see if non-inverse dtw > inverse dtw for var2,var1
          dist.val.2 <- ifelse(v2.dtw.distance > v2.inverse.dtw.distance, v2.dtw.distance, -1*v2.inverse.dtw.distance)
          output.matrix[l,k] <-dist.val.2 #puts var2,val1 in [l,k]
        }
      }
      index <- index + 1
    }
    
  }
  
  #Diagonal is 1 for output matrix 26 x 26
  #jul 17: keeping this as one because earlier we made it so that if there is zero distance it becomes 1/(0+1) = 1
  diag(output.matrix) <- 1
  state.val.list[[i]] <- state_signals
  
  # output.list[[i]] <- as.matrix(state_output)[,1:index]  # Assign sublist to output.list & Convert to matrix
  output.list[[i]] <- state_output
  output.matrix_list[[i]] <- output.matrix
  
}

# print
output.matrix_list

#greater outputs indicate higher similarity, while smaller output indicate greater dissimilarity bc diagonal is 1

## FILTERING BY THETA
#theta <- 0.008 # Threshold value CA 33 edges #38 for TX
#theta <- 0.022   
theta <- 0.023# Threshold value TX 32 edges 0.023
filtered_output_matrix <- lapply(output.matrix_list, function(m) {
  m[!(m < theta*-1 | m > theta)] <- 0
  m
})

(sum(rowSums(filtered_output_matrix[[2]] != 0))-26)/2


#02/03

#print(filtered_output_matrix)
#last.df <- as.data.frame(filtered_output_matrix)
#View(last.df)

#write.csv(filtered_output_matrix, "og_twelve_sig_filtered_output.csv")  # Export to CSV file

matrix_data <- as.matrix(filtered_output_matrix[[2]]) # You change this index number for the state that you want

# Round the matrix values to the thousandth decimal place
rounded_matrix_data <- round(matrix_data, digits = 3)

#02/04:SAVES RESULTS MATRIX TO FOLDER 
round_mat <- round(output.matrix_list[[1]], digits = 3)
state <- substring(colnames(rounded_matrix_data)[1], 1, 2)
file_name <- paste0("/Users/kristianny/Desktop/USC_data_2024/matrices/dtw/", state, "_DAILY_26_correct_hopefully_dtw_matrix_output_ROUND.csv")
#write.csv(round_mat, file = file_name, row.names = TRUE)

# NETWORK GRAPHS CREATED USING IGRAPH 
graph <- graph_from_adjacency_matrix(abs(rounded_matrix_data), mode = "undirected", weighted = TRUE) 
sign.graph <- graph_from_adjacency_matrix(rounded_matrix_data, mode = "undirected", weighted = TRUE) 

# potentially not needed  
# Set the vertex labels as the signal names (removing the first 14 characters)
# V(graph)$label <- ifelse(grepl("smoothed", colnames(rounded_matrix_data)),
#                          substring(colnames(rounded_matrix_data), 14),
#                          substring(colnames(rounded_matrix_data), 3))

#to remove state
V(graph)$label <- ifelse(grepl("smoothed", colnames(rounded_matrix_data)),
                         substring(colnames(rounded_matrix_data), 4),
                         substring(colnames(rounded_matrix_data), 4))

# Extract the state from the original matrix_data
state <- substring(colnames(rounded_matrix_data)[1], 1, 2)

# Set the graph title as the state
graph_title <- paste( toupper(state), "DTW (threshold:", theta, ")") #toupper makes capitalized

graph <- igraph::simplify(graph, remove.loops = TRUE)
sign.graph <- igraph::simplify(sign.graph, remove.loops = TRUE)

# Compute the layout with absolute values for graph (not sign.graph)
layout <- layout_nicely(graph, weights = E(graph)$weight)

# Format the edge labels with sign
format_edge_labels <- function(weights) {
  sign_labels <- ifelse(weights > 0, "+", "-")
  paste(sign_labels, abs(weights))
}

# Get the formatted edge labels
edge_labels <- format_edge_labels(E(sign.graph)$weight)

#saving dtw as a graphml file
# Set the vertex labels
V(graph)$label <- vertex_labels
gephi_name <- paste0("/Users/kristianny/Desktop/USC_data_2024/graphs/", state, "_dtw2_practicing_gephi.graphml")
write_graph(graph, file = gephi_name, format = "graphml")#, vertex.attr = list(label = V(graph)$label))
# 
# #download edges reference
edge_ends <- ends(graph, E(graph))
# # Get the shortened labels for the source and target vertices
# source_labels <- V(graph)$label[edge_ends[, 1]]
# target_labels <- V(graph)$label[edge_ends[, 2]]

# # # Convert the result to a data frame
edge_labels <- data.frame(
  Source = as.character(edge_ends[, 1]),
  Target = as.character(edge_ends[, 2]),
  Label = list(format_edge_labels(E(sign.graph)$weight))
)

edge_name <-  paste0("/Users/kristianny/Desktop/USC_data_2024/matrices/dtw/", state, "_DAILY_26_dtw_edge_name.csv")
write.table(edge_labels, file = edge_name, sep = ",", quote = FALSE, row.names = FALSE)

plot(graph,
     edge.label = edge_labels,
     layout = layout,
     vertex.color = "white",
     edge.color = "gray",
     label.font = 2,
     #label.dist = 1, #did not work
     vertex.shape = "circle",
     vertex.size = 1.5,
     vertex.label.cex = 0.5,
     edge.width = 1,
     edge.arrow.size = 0.2,
     edge.label.color = "black",
     edge.label.cex = 0.5,
     vertex.label.color = "darkblue",
     main = graph_title)

#03/12: trying to get a better visualization
# Plot the undirected graph with edge labels
plot(graph,
     edge.label = edge_labels,
     layout = layout,
     #label_context(labels = V(graph)$label,multi_line = TRUE, sep = "_" ),
     vertex.color = "white",
     edge.color = "gray10",
     label.font = 2,
     #label.dist = 1, #did not work
     vertex.shape = "circle",
     vertex.size = 20,
     vertex.label.cex = 0.5,
     edge.width = 0.45,
     edge.arrow.size = 0.1,
     edge.label.color = "gray28",
     edge.label.cex = 0.5,
     vertex.label.color = "darkblue",
     main = graph_title)


#### NOTE: The code below is extra stuff that is useful for visualization and exploration ####
### FOR SAVING PURPOSES

## Use this for saving INDIVIUDAL STATES WIDE as csv files
# rows = days
# columns = signals
per.state.wide.ds <- post.norm.full.ds %>%
  filter(geo_value == "tx")%>%
  mutate(new_time_value = as.Date(time_value)) %>%
  filter(new_time_value >= as.Date(date.min) & new_time_value <= as.Date(date.max)) %>% 
  arrange(geo_value) %>%
  filter(signal %in% sigs.final) %>% 
  group_by(signal) %>%
  arrange(new_time_value) %>%
  select(new_time_value,signal, norm.value)%>%
  pivot_wider(names_from = signal, values_from = norm.value, values_fn = list)

#View(per.state.wide.ds)

#View(per.state.wide.ds)
dim(per.state.wide.ds) # 402  27

# Checking to see if there are any NULLS
ind.comp.wide.ds <- per.state.wide.ds[apply(per.state.wide.ds[, -1], 1, function(row) !any(sapply(row, is.null))), ]
dim(ind.comp.wide.ds) #402  27


# Checking to see if time is continous
date.diff <- diff(per.state.wide.ds$new_time_value)
all(date.diff == 1) #should be TRUE

#SAVING AS CSV FILE
# Convert list columns to character vectors bc they were a list and can't save as csv file
ind.comp.wide.ds <- ind.comp.wide.ds %>%
  mutate(across(where(is.list), ~ purrr::map_chr(., function(x) if (length(x) == 0) NA_character_ else as.character(x))))

file.path <- "/Users/kristianny/Desktop/DTW_S/state_26_data/weekly_ca_26_signals_official.csv" #have to change the name for each file
#write.csv(ind.comp.wide.ds, file = file.path, row.names = FALSE)


#jul 30
# Use this for saving INDIVIDUAL STATES WIDE as CSV files
#geo_values <- unique(weekly.norm.full.ds$geo_value)
geo_values <- unique(cleaned.full.ds$geo_value)

for (geo_val in geo_values) {
  per_state_wide_ds <- post.norm.full.ds %>% 
    filter(geo_value == geo_val) %>%
    mutate(new_time_value = as.Date(time_value)) %>%
    filter(new_time_value >= as.Date(date.min) & new_time_value <= as.Date(date.max)) %>%
    arrange(geo_value) %>%
    filter(signal %in% sigs.final) %>%
    group_by(signal) %>%
    arrange(new_time_value) %>%
    select(new_time_value, signal, norm.value) %>%
    pivot_wider(names_from = signal, values_from = norm.value, values_fn = list) %>%
    mutate(across(where(is.list), ~ purrr::map_chr(., function(x) if (length(x) == 0) NA_character_ else as.character(x))))
  
  
  # Save the data for this geo_value as a CSV file
  file_name <- paste0("/Users/kristianny/Desktop/USC_data_2024/26_daily_all_states/", geo_val, "_DAILY_26_signals_official_norm.csv")
  write.csv(per_state_wide_ds, file = file_name, row.names = FALSE)
}


#EXPLORATION PURPOSES

#Just an exploration
# not needed for the daily exploration just shows which signals by unique indicators and categories
# #CATEGORY AND INDICATOR VISUAL
# time.post <- post.norm.full.ds %>%
#   filter(geo_value %in% states)%>%
#   filter(signal %in% sigs.final) %>%
#   mutate(new_time_value = as.Date(time_value)) %>%
#   #DATE
#   filter(new_time_value >= as.Date(date.min) & new_time_value <= as.Date(date.max))
# 
# sig.cat.tab <- time.post %>% distinct(signal,category_type, indicator)
# sig.indic.ds <- as.data.frame(unique(sig.cat.tab)) #shows signals and the corresponding category type 
# sig.indic.ds
# 
# 
# category_colors <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a",
#                      "#fb9a99", "#fdbf6f", "#cab2d6", "#b2df8a", "#a6cee3",
#                      "#000080", "#984ea3")
# 
# # Create the ggplot with scatter plot and adjust x-axis labels size
# ggplot(sig.cat.tab, aes(x = indicator, y = signal, color = category_type)) +
#   geom_point(size = 4) +
#   scale_color_manual(values = category_colors) +  # Use the defined color palette
#   theme_minimal() +
#   labs(title = "Signals by Unique Indicators and Categories",
#        x = "Indicator",
#        y = "Signal",
#        color = "Category Type") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

#Helful info about categories overall:
unique(time.post$indicator[unique(time.post$signal %in% sigs.final)])
unique(time.post$category_type[unique(time.post$signal %in% sigs.final)])

# FOR VISUALIZATION PURPOSES
#### Graphs for states ####

## POST:

one.states.long.ds <- post.norm.full.ds %>%
  filter(geo_value == "ca")%>%
  mutate(new_time_value = as.Date(time_value)) %>%
  #DATE
  filter(new_time_value >= as.Date(date.min) & new_time_value <= as.Date(date.max)) %>% 
  arrange(geo_value) %>%
  filter(signal %in% sigs.final) %>%
  select(new_time_value,geo_value,signal, norm.value)

by(one.states.long.ds$norm.value, one.states.long.ds$signal, summary)

#library(ggplot2)

new.stuff <- full.states.long.ds %>%
  filter(geo_value == "ca") %>%
  filter(new_time_value >= as.Date("2021-05-20") & new_time_value <= as.Date("2022-06-25")) %>%
  ggplot(aes(x = new_time_value, y = norm.value, color = as.factor(signal))) +
  geom_line(linewidth = 1.2, alpha = 0.7) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.2) + #CI se=TRUE size=1.5 adjusts the size of the CI 
  facet_wrap_paginate(~ signal, scales = "free_y", ncol = 4, nrow = 7) +
  labs(x = "Time", y = "Value", color = "Signal") +
  #scale_color_manual(values = viridis(nlevels(as.factor(signal)))) +
  scale_color_viridis_d() + 
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(), #removes grid lines
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(face = "bold")
  ) +
  scale_x_date(date_labels = "%b %Y") + labs(title ="California")

new.stuff

#Summer visualization
state_signals <- list()
state_data <- full.states.long.ds %>%
  filter(geo_value == "ca", signal == "smoothed_whh_cmnty_cli") %>% #original one from poster
  #filter(geo_value == "ca", signal == "smoothed_whesitancy_reason_low_priority") %>% #figuring out the dtw 
  select(norm.value) ## ONLY NORM VALUE
state_signals[[1]] <- state_data 
state_data <- full.states.long.ds %>%
  filter(geo_value == "ca", signal == "smoothed_wspent_time_indoors_1d") %>%  #original one from poster
  #filter(geo_value == "ca", signal == "smoothed_waccept_covid_vaccine_no_appointment") %>%
  select(norm.value) ## ONLY NORM VALUE

state_signals[[2]] <- state_data 
var1 <- state_signals[[1]]
var2 <- state_signals[[2]]

# time series data
plot(var1, type = "l", col = "#862b2b", xlab = "Days", ylab = "Min-Max Value", main = "Time Series of Covid-Like Illness in Community and Spent Time Indoors with Others")
lines( var2, col = "#FFB519")
legend("topleft", legend = c("cmnt_cli", "spent_time_indoors"), col = c("#862b2b", "#FFB519"), lty = 1,cex = 0.8)

#lagged corr
lag.days <- 21

#var1, var2 K,L
v1v2.lag.corr <- ccf(var1, var2, plot = FALSE, lag.max = lag.days)
v1v2.neg.lag.corr <- v1v2.lag.corr[-1*lag.days:0] #-21:0 lags
v1v2.acf.values <- v1v2.neg.lag.corr$acf

plot(v1v2.acf.values,ylab = "ACF Values", xlab="Negative Lag (Days)", main = paste("Lagged Correlation:CLI Community and Spent Time Indoors with Others"))

#03/09
#Note: you can add a window size constraint, can't be negative 
#plot(dtw(var1[[1]],var2[[1]], keep=TRUE, window.type="sakoechiba", window.size=c(21,0)),type="twoway")
#plot(dtw(var1[[1]],var2[[1]], keep=TRUE, window.type="sakoechiba", window.size=c(0,21)),type="twoway")
# Define darker colors (you can adjust the RGB values as needed)
darker_yellow <- "#FFB519"
darker_red <- "#862b2b"

# Perform the dtw alignment and store the result
dtw_result <- dtw(var1[[1]], var2[[1]], keep = TRUE, window.type = "sakoechiba", window.size = c(21,21))
dtw_result_v2v1 <- dtw(var2[[1]], var1[[1]], keep = TRUE, window.type = "sakoechiba", window.size = c(21,21))

dtw_inverse <-dtw(var1[[1]],-1*var2[[1]]+1, keep=TRUE, window.type="sakoechiba", window.size=c(21,21))
dtw_inverse_v2v1 <-dtw(var2[[1]],-1*var1[[1]]+1, keep=TRUE, window.type="sakoechiba", window.size=c(21,21))
# Set the graphical parameters to combine both plots in the same panel
par(mfrow = c(2, 1))  # Combine plots in 1 row and 2 columns

# Plot the dtw_result with both colors and a bolder line width (lwd)
plot(dtw_result, type = "twoway", col = c(darker_red, darker_yellow), lwd = 2, xlab="Days",  main = "Dynamic Time Warping")
legend("topleft", legend = c("cmnt_cli(leading)", "spent_time_indoors(lagging)"), col = c(darker_red, darker_yellow), lty = 1, cex = 0.8)

plot(dtw_result_v2v1, type = "twoway", col = c(darker_yellow, darker_red), lwd = 2, xlab="Days",  main = "Dynamic Time Warping")


plot(dtw_inverse, type = "twoway", col =c(darker_red, darker_yellow), lwd = 2, xlab="Days", main = "Dynamic Time Warping: Inverse Signal")
legend("topleft", legend = c("cmnt_cli(leading)", "Inverse:spent_time_indoors(lagging)"), col = c(darker_red, darker_yellow), lty = 1)


# Plot the dtw_result_v2v1 with both colors and a bolder line width (lwd)
plot(dtw_result_v2v1, type = "twoway", col = c(darker_yellow, darker_red), lwd = 2)

# Reset the graphical parameters
par(mfrow = c(1, 1))

# Add legend for var1 and var2 on the top-left
legend("topleft", legend = c("cmnt_cli(lagging)", "spent_time_indoors(leading)"), col = c(darker_red, darker_yellow), lty = 1, cex = 0.8)

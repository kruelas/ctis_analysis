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

# Loading other packages
library(sjPlot)
library(lubridate)
library(ghibli)
library(dplyr)
library(tidyverse)
library(covidcast)
library(gridExtra)

options(covidcast.auth = "3e5e14007dd9e")#this is the authorization code to be able to access the delphi covidcast aggregated data

library(viridisLite)
#library(ggforce)
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



#cleaning 

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

# CREATING CATEGORY TYPE COLUMN
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

#remove the rows whose value in the column value is missing- DONT FORGET TO RUN THIS!
dim(full.ds) #2704700      18
cleaned.full.ds <- full.ds[complete.cases(full.ds$value), ]  #this removes the row number too
#we could also use na.omit(full.ds[full.ds$value,]) #this keeps the dataframe intact gives error occasionally 
dim(cleaned.full.ds) #2588589      18


#### READING IN THE CLEANED.FULL.DS####
#you no longer have to run the code above
###cleaned.full.ds <- read.csv(file="/Users/kristianny/Desktop/USC_data_2024/ctis_cleaned_full_ds.csv", header = TRUE)
#did not work

#### Min-Max Normalization : FOR THE ORIGINAL FULL DATASET 191 SIGNALS####

#Min-Max normalization FOR THE ORIGINAL FULL DATASET 191 SIGNALS
#this can be used when graphing raw signals without any date constraints
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



#install.packages("viridis")
library(viridis)

#### TIME SERIES GRAPHS: Indicators and Categories (includes min-max normalization: norm.full.ds) ####
##Visulizations
#library(viridis)

sub.ds <- subset(norm.full.ds, geo_value == "ca", indicator == "behavior")

#Linear Regression for each Indicator and Epidemiological outcomes
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





##### Filtering to find the signals for 4 states ####

##### PRE 2020-2021 #####

a.ca.sigs <- list()
states <- list("ca","ny","tx","fl")
num_states <- length(states)
indicator.names <- unique(norm.full.ds$indicator)
a.signals.states.list <- vector("list", length = num_states)
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
    
    
    #trying to filter by date
    dates.ds <-  wide.ds %>%
      #filter(new_time_value >= as.Date("2020-11-24") &new_time_value <= as.Date("2021-02-08"))
      filter(new_time_value >= as.Date("2020-09-08") & new_time_value <= as.Date("2021-03-15"))
    
    ###  filter(new_time_value >= as.Date("2021-05-20") & new_time_value <= as.Date("2022-06-25"))
    # 
    
    # 
    df <- as.data.frame(dates.ds)
    # 
    # 
    # # Filter out columns with NULL values
    a.comp.wide.ds <- df[, apply(df, 2, function(col) !any(sapply(col,is.null)))]
    #View(comp.wide.ds)
    # 
    a.ca.sigs[[counter]] <- colnames(a.comp.wide.ds[,-1])
    counter = counter +1
    
  }
  a.signals.states.list[[number.index]] <- a.ca.sigs
  number.index =  number.index + 1
  
}

View(a.signals.states.list)
a.sigs <- a.signals.states.list[[1]][1:7] #86 signals
a.sig.list <- unlist(a.sigs)


##### POST 2021-2022 #####
#used for presentation poster 
ca.sigs <- list()
states <- list("ca","ny","tx","fl")
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
    
    
    #trying to filter by date
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
#reason: extremely similair trends with trust in gov --> redundant 

trial.signals<- sigs.final
a <- choose(length(trial.signals), 2) # helper variable to see how long the output.list is gonna be
states <- list("ca", "tx")#Specify the number of states
#states <- list("ca","ny","tx","fl")
#states <- list("ny")
num_signals <- length(trial.signals)
num_states <- length(states)
date.min <- "2021-05-20"
date.max <- "2022-06-25"

cleaned.full.ds$new_time_value <- ymd(cleaned.full.ds$time_value)

# #MAKE IT WEEKLY 
#install.packages("zoo")
library(zoo)

#JUL 29
#
smooth_row_wise <- function(x) {
  rollapply(x, width = 14, FUN = mean, fill = NA, align = "center", by.column = FALSE, partial = TRUE)
}
# 
# 
# # Filter the data and perform data processing
# weekly.full.ds <- cleaned.full.ds %>%
#   filter(new_time_value >= as.Date(date.min) & new_time_value <= as.Date(date.max)) %>%
#   filter(geo_value %in% states) %>%
#   arrange(geo_value) %>%
#   group_by(signal, geo_value) %>%
#   mutate(
#     value_smoothed = smooth_row_wise(value),
#     weekly_index = row_number() %% 7 == 0
#   ) %>% # binary to flag for weekly data points
#   filter(weekly_index) %>% #rows corresponding to weekly data points
#   select(-weekly_index) %>% # Remove rest of days
#   ungroup()
# 
# 
# # Print unique dates for each state and signal
# by(weekly.full.ds$new_time_value, weekly.full.ds$signal, summary) # this is for the 182 signals0
# 
# #MIN MAX NORM
# # weekly.norm.full.ds <- weekly.full.ds %>%
#   group_by(signal, geo_value) %>% #taking each unique signal and state into account
#   mutate(
#     norm.value = scale(value_smoothed, center = min(value_smoothed, na.rm = TRUE),
#                        scale = max(value_smoothed, na.rm = TRUE) - min(value_smoothed, na.rm = TRUE) + 1e-6) #adding this
#     #because we dont want 1/0
#   ) %>%
#   ungroup()
# .
# summary(weekly.norm.full.ds$norm.value)
# by(weekly.norm.full.ds$norm.value, weekly.norm.full.ds$signal, summary)
# 
# norm_summary <- weekly.norm.full.ds %>%
#   group_by(signal, geo_value) %>%
#   summarize(min_norm_value = min(norm.value, na.rm = TRUE),
#             max_norm_value = max(norm.value, na.rm = TRUE))
# 
# #to know how many weeks are there
# length(unique(weekly.norm.full.ds$new_time_value))

## This is min-max normalization DAILY !!!
# 
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


## Use this for saving INDIVIUDAL STATES WIDE as csv files
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

file.path <- "/Users/kristianny/Desktop/DTW_S/state_26_data/weekly_ny_26_signals_official.csv" #have to change the name for each file
write.csv(ind.comp.wide.ds, file = file.path, row.names = FALSE)






#jul 30

# Use this for saving INDIVIDUAL STATES WIDE as CSV files
#geo_values <- unique(weekly.norm.full.ds$geo_value)
 geo_values <- unique(cleaned.full.ds$geo_value)

for (geo_val in geo_values) {
  per_state_wide_ds <- post.norm.full.ds %>%  #weekly.norm.full.ds %>%
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

#checking to see if the signals do have the same weekly dates amongst different states
checking <-full.states.long.ds[full.states.long.ds$geo_value=="tx",]
checking$new_time_value[checking$signal=="smoothed_waccept_covid_vaccine_no_appointment"]

weekly.dates <- c( "2021-05-26", "2021-06-02","2021-06-09","2021-06-16","2021-06-23", "2021-06-30" ,"2021-07-07" ,"2021-07-14", "2021-07-21", "2021-07-28",
                   "2021-08-04", "2021-08-11","2021-08-18","2021-08-25","2021-09-01", "2021-09-08" ,"2021-09-15" ,"2021-09-22", "2021-09-29", "2021-10-06",
                   "2021-10-13", "2021-10-20","2021-10-27","2021-11-03","2021-11-10", "2021-11-17" ,"2021-11-24" ,"2021-12-01", "2021-12-08", "2021-12-15",
                   "2021-12-22", "2021-12-29","2022-01-05","2022-01-12","2022-01-19", "2022-01-26" ,"2022-02-02" ,"2022-02-09", "2022-02-16", "2022-02-23",
                   "2022-03-02", "2022-03-09","2022-03-16","2022-03-23","2022-03-30", "2022-04-06" ,"2022-04-13" ,"2022-04-20", "2022-04-27", "2022-05-04",
                   "2022-05-11", "2022-05-18","2022-05-25","2022-06-01","2022-06-08", "2022-06-15" ,"2022-06-22")

length(ifelse(full.states.long.ds$new_time_value %in% weekly.dates, TRUE, FALSE))#THEY DO have the same dates : JAN 28

time.post <- weekly.norm.full.ds %>%
  filter(geo_value %in% states)%>%
  filter(signal %in% sigs.final) %>%
  mutate(new_time_value = as.Date(time_value)) %>%
  #DATE
  filter(new_time_value >= as.Date(date.min) & new_time_value <= as.Date(date.max))


sig.cat.tab <- time.post %>% distinct(signal, category_type, indicator)
sig.indic.ds <- as.data.frame(unique(sig.cat.tab)) #shows signals and the corresponding category type 
sig.indic.ds


category_colors <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a",
                     "#fb9a99", "#fdbf6f", "#cab2d6", "#b2df8a", "#a6cee3",
                     "#000080", "#984ea3")

# Create the ggplot with scatter plot and adjust x-axis labels size
ggplot(sig.cat.tab, aes(x = indicator, y = signal, color = category_type)) +
  geom_point(size = 4) +
  scale_color_manual(values = category_colors) +  # Use the defined color palette
  theme_minimal() +
  labs(title = "Signals by Unique Indicators and Categories",
       x = "Indicator",
       y = "Signal",
       color = "Category Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))



#Helful info about categories overall:
unique(time.post$indicator[unique(time.post$signal %in% sigs.final)])
unique(time.post$category_type[unique(time.post$signal %in% sigs.final)])


# #THIS WORKS FINALLY!!!!!!!! JUL 10
# comp.wide.ds <- df[apply(df[, -1], 1, function(row) !any(sapply(row, is.null))), ]
# #complete.wide.ds <- df[apply(df[, -1], 1, function(row) !any(sapply(row, function(x) is.null(x) || is.na(x)))), ]
# 


#### Graphs for 4 states ####
## PRE:

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

#### LAG CORRELATION MATRIX ####
#(aka cross correlation) measures the linear relationship between two time series variables at different time lags
#Facts: Cross-Correlation Function (ccf) does not explore the relationship among three different variables at the same time
#what we can do is:
#calculate pairwise CCFs ( A&B A&C B&C)


#### THIS PUT THE NEGATIVE OR POSITIVE MAX CORRELATION IN THE MATRIX ####
## Does the negative lag correlation on 21 days (3 weeks)

#states <- "ca"
#trial.signals <- c("smoothed_whh_cmnty_cli", "smoothed_wspent_time_indoors_1d")
state.val.list <- vector("list", length = num_states)
output.list <- vector("list", length = num_states)
output.matrix_list <- vector("list", length = num_states)

for (i in 1:num_states) {
  state <- states[i]
  state_signals <- list()
  state_output <- list()  # Initialize sublist for output
  
  #this is redundant and should be optimized later
  for (j in 1:num_signals) {
    one.sig <- trial.signals[j]
    state_data <- full.states.long.ds %>% #01/20: NEEED TO CHANGE THIS TO WEEKLY!!!!!!!!!! it actually is based on the weekly one, inspect closer 0-0
      filter(geo_value == state, signal == one.sig) %>%
      select(norm.value) ## ONLY NORM VALUE
    
    state_signals[[j]] <- state_data  # 4 signal values
  }
  
  
  index <- 1
  output.matrix <- matrix(NA, nrow = num_signals, ncol = num_signals, dimnames = list(paste0(states[i], "_", trial.signals),
                                                                                      paste0(states[i], "_", trial.signals)))
  #before it was dimnames = list(trial.signals, trial.signals) <- only had the signal names
  
  
  for (k in 1:(num_signals - 1)) {
    for (l in (k + 1):num_signals) {
      var1 <- state_signals[[k]]
      var2 <- state_signals[[l]]
      
      #Updated: Jul 12
      # Calculating the lag correlation: negative lag of 21 days (-21)
      lag.days <- 21
      
      ##CHANGE TO 3 B/C OF WEEKLY!!1
      #lag.weeks <- 3
      
      #var1, var2 K,L
      v1v2.lag.corr <- ccf(var1, var2, plot = FALSE, lag.max = lag.days)
      v1v2.neg.lag.corr <- v1v2.lag.corr[-1*lag.days:0] #-21:0 lags CHANGE THE LAD DAYS --> WEEK
      
      #var2, var1 L,K
      var2v1.lag.corr <- ccf(var2, var1, plot = FALSE, lag.max = lag.days)
      var2v1.neg.lag.corr <- var2v1.lag.corr[-1*lag.days:0] #-21:0 lags
      
      # 
      # # Print the cross-correlation results
      # cat("Cross-correlation between", trial.signals[[k]], "and", trial.signals[[l]], ":\n")
      v1v2.acf.values <- v1v2.neg.lag.corr$acf
      # print(v1v2.acf.values)
      # cat("\n")
      # 
      # 
      # cat("Cross-correlation between", trial.signals[[l]], "and", trial.signals[[k]], ":\n")
      var2v1.acf.values <- var2v1.neg.lag.corr$acf
      # print(var2v1.acf.values)
      # cat("\n")
      
      # Plotting the lag correlation
      #plot(v1v2.acf.values, main = paste("Cross-correlation:", trial.signals[l], trial.signals[k]))
      
      #PART A
      v1v2.max.abs.value <- max(abs(v1v2.acf.values))
      v1v2.max.value <- max(v1v2.acf.values)
      v1v2.min.value <- min(v1v2.acf.values)
      
      #PART B
      var2v1.max.abs.value <- max(abs(var2v1.acf.values))
      var2v1.max.value <- max(var2v1.acf.values)
      var2v1.min.value <- min(var2v1.acf.values)
      
      #state_output[[index]] <- max.abs.value #NOT SURE TO KEEP OR NOT 
      
      # Assign values to the appropriate positions in the matrix based on signal order
      ifelse(v1v2.max.abs.value == v1v2.max.value,  output.matrix[k, l] <- v1v2.max.value, output.matrix[k, l] <- v1v2.min.value )
      ifelse(var2v1.max.abs.value == var2v1.max.value, output.matrix[l, k] <- var2v1.max.value,  output.matrix[l, k] <- var2v1.min.value)
      
      index <- index + 1
      
    }
  }
  #assigns 1 diagnally because signals are the same
  diag(output.matrix) <- 1
  state.val.list[[i]] <- state_signals
  
  # output.list[[i]] <- as.matrix(state_output)[,1:index]  # Assign sublist to output.list & Convert to matrix
  #output.list[[i]] <- state_output
  output.matrix_list[[i]] <- output.matrix
}


## FILTERING BY THETA

#theta <- 0.76  # Threshold value
theta <- 0.82

filtered_output_matrix <- lapply(output.matrix_list, function(m) {
  m[!(m < theta*-1 | m > theta)] <- 0
  m
})

sum(rowSums(filtered_output_matrix[[2]] != 0))-26 #CHANGED to 26 jan 28 because we are working with 26 signals and not 27 


# 31 #0.76
# 29 #0.72
# 35 #0.80
# 33 # 0.78

# 39 #0.70
# 36 #0.70
# 58 #0.75
# 46 #0.75
# 
# #THIS IS TO SAVE ALL THE OUTPUTS FROM LAGGED CORR


# theta_states <- c(31,29,35,33)
# iii <- 1
# for (theta in theta_states){
#   filtered_output_matrix <- lapply(output.matrix_list, function(m) {
#                                 m[!(m < theta*-1 | m > theta)] <- 0
#                                 m
#                               })
#   filtered_output_matrix
#   #sum(rowSums(filtered_output_matrix[[iii]] != 0))-26 
#   matrix_data <- as.matrix(filtered_output_matrix[[iii]])
#   
#   # Round the matrix values to the thousandth decimal place
#   rounded_matrix_data <- round(matrix_data, digits = 3)
#   state_name <- substring(colnames(rounded_matrix_data)[1], 1, 2)
#   
#   file_name <- paste0("/Users/kristianny/Desktop/USC_data_2024/matrices/lagged_correlation/", state_name, "_DAILY_26_lagcorr_matrix_output.csv")
#   write.csv(rounded_matrix_data, file = file_name, row.names = FALSE)
#   iii <- iii + 1
# }
# print(filtered_output_matrix)
# last.df <- as.data.frame(filtered_output_matrix)
# View(last.df)

#write.csv(filtered_output_matrix, "og_twelve_sig_filtered_output.csv")  # Export to CSV file


#output.matrix_list[[1]]

matrix_data <- as.matrix(filtered_output_matrix[[2]]) # You change this index number for the state that you want

# Round the matrix values to the thousandth decimal place
rounded_matrix_data <- round(matrix_data, digits = 3)


# Extract the state from the original matrix_data
#SAVES MATRIX TO FOLDER DOES WORK 02/03
out_p <- as.matrix(output.matrix_list[[1]])
round_out_p <- round(out_p, digits = 3)

state <- substring(colnames(rounded_matrix_data)[1], 1, 2) #DONT COMMENT THIS OUT

state_name <- substring(colnames(round_out_p)[1], 1, 2)
file_name <- paste0("/Users/kristianny/Desktop/USC_data_2024/matrices/lagged_correlation/", state_name, "_DAILY_26_lagcorr_matrix_output.csv")
##write.csv(round_out_p, file = file_name, row.names = TRUE)

##Commenting this out 02/18 to practice
# graph <- graph_from_adjacency_matrix(abs(rounded_matrix_data), mode = "directed", weighted = TRUE) 
# sign.graph <- graph_from_adjacency_matrix(rounded_matrix_data, mode = "directed", weighted = TRUE) 
# # # Set the vertex labels as the signal names (removing the first 14 characters)
# V(graph)$label <- ifelse(grepl("smoothed", colnames(rounded_matrix_data)),
#                          substring(colnames(rounded_matrix_data), 14),
#                          substring(colnames(rounded_matrix_data), 3))

# Modified 02/18: purpose: to be able to have the gephi file node names in correct format without the ca_... and only have signal name
# creating the vertex labels first
vertex_labels <- ifelse(grepl("smoothed", colnames(rounded_matrix_data)),
                        substring(colnames(rounded_matrix_data), 14),
                        substring(colnames(rounded_matrix_data), 3))

# creating the graphs first
graph <- graph_from_adjacency_matrix(abs(rounded_matrix_data), mode = "directed", weighted = TRUE)
sign.graph <- graph_from_adjacency_matrix(rounded_matrix_data, mode = "directed", weighted = TRUE)

# setting the graph labels 
V(graph)$label <- vertex_labels
V(sign.graph)$label <- vertex_labels


# Set the graph title as the state
graph_title <- paste(toupper(state), "Daily Lagged Correlation (threshold:", theta, ")") #toupper makes capitalized

graph <- igraph::simplify(graph, remove.loops = TRUE)
sign.graph <- igraph::simplify(sign.graph, remove.loops = TRUE)

#number of degree fro each node/vertices
deg<- degree(graph, mode= "all")
sign_deg <-  degree(sign.graph, mode= "all")
#hierarchical algorithm 

# Get the edge weights of sign.graph
#edge_weights <- edge_attr(graph, "weight")

edge_weights <- E(sign.graph)$weight

# Define a function to format the edge labels with sign
format_edge_labels <- function(weights) {
  sign_labels <- ifelse(weights > 0, "+", "-")
  paste(sign_labels, abs(weights))
}

# Get the edge weights from the graph
#edge_weights <- edge_attr(graph, "weight")


# Compute the layout with absolute values
#layout <- layout_with_fr(graph, weights = abs(E(graph)$weight),  niter =10)# , niter = 1000 #layout_with_sugiyama, #layout_with_kk 
#set.seed(1)
layout <- layout_with_lgl(graph)#, maxiter = 20)
#layout <- layout_nicely(graph,  weights = abs(E(graph)$weight))
#layout <- layout_with_sugiyama(graph, weights = E(graph)$weight)
#layout <- layout_with_kk(graph, weights = E(graph)$weight)

#02/08: Saving graph to use in gephi 

# DOES NOT WORK 02/18
#graphml_obj <- graph_from_adjacency_matrix(graph, edge.attr = list(weight = format_edge_labels(edge_weights)))

# Save the graphml object to a file

#COMMENT THESE OUT IF YOU DON'T WANT THE OUTPUT TO HAVE POSITIVE AND NEGATIVE EDGES
# Assuming your graph has edge weights as an attribute named 'weight'
edge_weights <- E(sign.graph)$weight

# Set the edge weights to the graph
E(graph)$weight <- edge_weights

# Set the vertex labels
V(graph)$label <- vertex_labels
gephi_name <- paste0("/Users/kristianny/Desktop/USC_data_2024/graphs/", state_name, "_FINAL_practicing_gephi.graphml")
write_graph(graph, file = gephi_name, format = "graphml")#, vertex.attr = list(label = V(graph)$label))



edge_ends <- ends(graph, E(graph))
# Get the shortened labels for the source and target vertices
source_labels <- V(graph)$label[edge_ends[, 1]]
target_labels <- V(graph)$label[edge_ends[, 2]]

# # Convert the result to a data frame
edge_labels <- data.frame(
  Source = as.character(edge_ends[, 1]),
  Target = as.character(edge_ends[, 2]),
  Label = list(format_edge_labels(edge_weights))
)


edge_name <-  paste0("/Users/kristianny/Desktop/USC_data_2024/matrices/lagged_correlation/", state_name, "_DAILY_26_lagcorr_edge_name.csv")
write.table(edge_labels, file = edge_name, sep = ",", quote = FALSE, row.names = FALSE)


plot(graph,
     edge.label = format_edge_labels(edge_weights),
     layout = layout,
     vertex.color = "white",
     edge.color = "gray",
     font = 2,
     vertex.shape = "circle",
     #vertex.size = deg * 2,
     vertex.size = 1.5, #0.2
     edge.arrow.size = 0.1,
     edge.width = 1.2,
     vertex.label.cex = 0.6, #0.55
     edge.label.color = "black",
     edge.label.cex = 0.6,
     #edge.curved = 0.2,  # Adjust the curvature level
     main = graph_title)



#install.packages("forecast")
library(forecast)


# 
# # Create a Tk plot
# tkplot(graph, edge.label = format_edge_labels(edge_weights),
#        layout = layout_with_lgl(graph),
#        vertex.label = ifelse(grepl("smoothed", colnames(rounded_matrix_data)),
#                              substring(colnames(rounded_matrix_data), 14),
#                              substring(colnames(rounded_matrix_data), 3)),
#        vertex.frame.color = "orange",
#        edge.color = "gray", edge.arrow.size = 0.4,
#        edge.width = 1.2, vertex.size = 0.2)
# 


### for poster ###
state_signals <- list()
#state_data <- full.states.long.ds %>%
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

#dtw

plot(dtw(var1[[1]],var2[[1]], keep=TRUE, window.type="sakoechiba", window.size= 21),type="twoway")
plot(dtw(var1[[1]],-1*var2[[1]]+1, keep=TRUE, window.type="sakoechiba", window.size=21),type="twoway")

# note: 03/10: helpful in understanding what's happening with the constraints
#plot(dtw(var1[[1]],var2[[1]], keep=TRUE, window.type="sakoechiba", window.size=402),type="twoway")
#dtw(var1[[1]],var2[[1]], keep=TRUE, window.type="sakoechiba", window.size=0)$distance

#03/09
#Note: you can add a window size constraint, can't be negative 
#plot(dtw(var1[[1]],var2[[1]], keep=TRUE, window.type="sakoechiba", window.size=c(21,0)),type="twoway")
#plot(dtw(var1[[1]],var2[[1]], keep=TRUE, window.type="sakoechiba", window.size=c(0,21)),type="twoway")
# Define darker colors (you can adjust the RGB values as needed)
darker_yellow <- "#FFB519"
darker_red <- "#862b2b"

# Perform the dtw alignment and store the result
dtw_result <- dtw(var1[[1]], var2[[1]], keep = TRUE, window.type = "sakoechiba", window.size = c(0,21))
dtw_result_v2v1 <- dtw(var2[[1]], var1[[1]], keep = TRUE, window.type = "sakoechiba", window.size = c(0,21))

dtw_inverse <-dtw(var1[[1]],-1*var2[[1]]+1, keep=TRUE, window.type="sakoechiba", window.size=c(0,21))
dtw_inverse_v2v1 <-dtw(var2[[1]],-1*var1[[1]]+1, keep=TRUE, window.type="sakoechiba", window.size=c(0,21))
# Set the graphical parameters to combine both plots in the same panel
par(mfrow = c(2, 1))  # Combine plots in 1 row and 2 columns

# Plot the dtw_result with both colors and a bolder line width (lwd)
plot(dtw_result, type = "twoway", col = c(darker_red, darker_yellow), lwd = 2, xlab="Days",  main = "Dynamic Time Warping")
legend("topleft", legend = c("cmnt_cli(leading)", "spent_time_indoors(lagging)"), col = c(darker_red, darker_yellow), lty = 1, cex = 0.8)

plot(dtw_inverse, type = "twoway", col =c(darker_red, darker_yellow), lwd = 2, xlab="Days", main = "Dynamic Time Warping: Inverse Signal")
legend("topleft", legend = c("cmnt_cli(leading)", "Inverse:spent_time_indoors(lagging)"), col = c(darker_red, darker_yellow), lty = 1)


# Plot the dtw_result_v2v1 with both colors and a bolder line width (lwd)
plot(dtw_result_v2v1, type = "twoway", col = c(darker_yellow, darker_red), lwd = 2)

# Reset the graphical parameters
par(mfrow = c(1, 1))

# Add legend for var1 and var2 on the top-left
legend("topleft", legend = c("cmnt_cli(lagging)", "spent_time_indoors(leading)"), col = c(darker_red, darker_yellow), lty = 1, cex = 0.8)

#### DTW ####
#install.packages("dtw")
library(dtw)

#trial.signals <- c("smoothed_wothers_masked_public", "smoothed_wwearing_mask_7d" )

#unique(norm.full.ds$signal[norm.full.ds$category_type == "mask_use"])

state.val.list <- vector("list", length = num_states)
output.list <- vector("list", length = num_states)
output.matrix_list <- vector("list", length = num_states)



for (i in 1:num_states) {
  state <- states[i]
  state_signals <- list()
  state_output <- list()  # Initialize sublist for output
  
  
  for (j in 1:num_signals) {
    one.sig <- trial.signals[j]
    state_data <- full.states.long.ds %>% #CHANGEEEEE!!
      filter(geo_value == state, signal == one.sig) %>%
      select(norm.value) ## ONLY NORM VALUE
    
    state_signals[[j]] <- state_data  # 4 signal values
  }
  
  
  index <- 1
  output.matrix <- matrix(NA, nrow = num_signals, ncol = num_signals, dimnames = list(paste0(states[i], "_", trial.signals),
                                                                                      paste0(states[i], "_", trial.signals)))
  #before it was dimnames = list(trial.signals, trial.signals) <- only had the signal names
  
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
      
      
      #03/11 code is incorrect #ACTUALLY IT IS CORRECT
      #upper triangle 
        if(is.na(output.matrix[k,l])){
        dist.val<- ifelse(dtw.distance > inverse.dtw.distance, dtw.distance, -1*inverse.dtw.distance)
        output.matrix[k,l] <- dist.val
        
        #bottom part of triangle 
        if(is.na(output.matrix[l,k])){
         dist.val.2 <- ifelse(v2.dtw.distance > v2.inverse.dtw.distance, v2.dtw.distance, -1*v2.inverse.dtw.distance)
         output.matrix[l,k] <-dist.val.2
        }
      }
      index <- index + 1
    }
    
  }
  #assigns 0 diagnally because signals are the same
  #diag(output.matrix) <- 0 # I CHANGE THE DIAGNOL TO BE ZERO: because they are the same signal so there's zero distance
  #jul 17: keeping this as one because earlier we made it so that if there is zero distance it becomes 1/(0+1) = 1
  diag(output.matrix) <- 1
  state.val.list[[i]] <- state_signals
  
  # output.list[[i]] <- as.matrix(state_output)[,1:index]  # Assign sublist to output.list & Convert to matrix
  output.list[[i]] <- state_output
  output.matrix_list[[i]] <- output.matrix
  
}

# print
output.matrix_list

# 
# for (i in 1:num_states) {
#   state <- states[i]
#   state_signals <- list()
#   state_output <- list()  # Initialize sublist for output
#   
#   
#   for (j in 1:num_signals) {
#     one.sig <- trial.signals[j]
#     state_data <- full.states.long.ds %>% #CHANGEEEEE!!
#       filter(geo_value == state, signal == one.sig) %>%
#       select(norm.value) ## ONLY NORM VALUE
#     
#     state_signals[[j]] <- state_data  # 4 signal values
#   }
#   
#   
#   index <- 1
#   output.matrix <- matrix(NA, nrow = num_signals, ncol = num_signals, dimnames = list(paste0(states[i], "_", trial.signals),
#                                                                                       paste0(states[i], "_", trial.signals)))
#   #before it was dimnames = list(trial.signals, trial.signals) <- only had the signal names
#   
#   
#   
#   for (k in 1:(num_signals - 1)) {
#     for (l in (k + 1):num_signals) {
#       var1 <- state_signals[[k]]
#       var2 <- state_signals[[l]]
#       
#       
#       # DTW DISTANCE
#       
#       #WE ARE ADDING ONE SO THAT IF THE SIGNAL IS THE SAME THE DISTANCE WILL BE ONE INSTEAD OF ZERO
#       
#       #PART A
#       dtw.distance <- 1/(dtw(var1, var2, window.type = "sakoechiba", window.size = c(0,21))$distance + 1) #dist.method = "Euclidean" by default
#       inverse.dtw.distance <- 1/(dtw(var1, (-1*var2)+1, window.type = "sakoechiba", window.size = c(0,21))$distance + 1) #changing window size from 21 days to 3 weeks
#       
#       v1v2.max.ab <- max(abs(dtw.distance))#redundant because it only produces one number and it will be positive never negative
#       v1v2.inv.max.ab <- max(abs(inverse.dtw.distance))
#       
#       #PART B 
#       v2.dtw.distance <- 1/(dtw(var2, var1, window.type = "sakoechiba", window.size = c(0,21))$distance + 1) #dist.method = "Euclidean" by default
#       v2.inverse.dtw.distance <- 1/(dtw(var2, (-1*var1)+1, window.type = "sakoechiba", window.size = c(0,21))$distance + 1)
#       
#       v2v1.max.ab <- max(abs(v2.dtw.distance))
#       v2v1.inv.max.ab <- max(abs(v2.inverse.dtw.distance))
#       
#       #03/11: CORRECT CODE #UPDATE: INCORRECT
#       # ifelse(v1v2.max.ab > v2v1.max.ab | v1v2.max.ab == v2v1.max.ab,output.matrix[k, l] <- v1v2.max.ab, output.matrix[k, l] <- v2v1.max.ab)
#       # ifelse(v1v2.inv.max.ab > v2v1.inv.max.ab | v1v2.inv.max.ab == v2v1.inv.max.ab,output.matrix[l,k] <- -1*v1v2.inv.max.ab, output.matrix[l,k] <- -1*v2v1.inv.max.ab)
#       # 
#       
#       #03/11 code is incorrect #ACTUALLY IT IS CORRECT
#       # Assign values to the appropriate positions in the matrix based on signal order
#       ifelse(dtw.distance > inverse.dtw.distance,  output.matrix[k, l] <- dtw.distance, output.matrix[k, l] <- -1*inverse.dtw.distance )
#       #ifelse(v2.dtw.distance > v2.inverse.dtw.distance, output.matrix[l, k] <- v2.dtw.distance,  output.matrix[l, k] <- -1*v2.inverse.dtw.distance)
# 
# 
#       
#       #HELPFUL
#       #dtw(x, y, window.type = "sakoechiba", window.size = 21)
#       # 
#       #           trying <-  dtw(var1[[1]],var2[[1]], keep=TRUE, window.type="sakoechiba", window.size=21)
#       #           trying$distance
#       # plot(dtw(var1[[1]],var2[[1]], keep=TRUE, window.type="sakoechiba", window.size=21),type="twoway")
#       # plot(dtw(var1[[1]],-1*var2[[1]]+1, keep=TRUE, window.type="sakoechiba", window.size=21),type="twoway")
#       
#       #           
#       #           plot(1/(dtw(var1, var2, window.type = "sakoechiba", window.size = 21) + 1),type="twoway")
#       #          
#       #           
#       #           
#       #           
#       #           #inverse HELPFUL
#       #           inv.trying <-  dtw(var1[[1]],-1*var2[[1]], keep=TRUE, window.type="sakoechiba", window.size=21)
#       #           inv.trying$distance
#       #           1/(inv.trying$distance +1)
#       #plot(dtw(var1[[1]],-1*var2[[1]], keep=TRUE, window.type="sakoechiba", window.size=21),type="twoway")
#       #           
#       #plot(1/(dtw(var1, var2, window.type = "sakoechiba", window.size = 21) + 1),type="twoway")
#       
#       # 
#       # p.max <- max(dtw.distance)
#       # n.max <- max(inverse.dtw.distance)
#       # #not needed because there is only 1 output 
#       # p.max <- max(dtw.distance)
#       # n.max <- max(inverse.dtw.distance)
#       # 
#       # if (p.max > n.max){
#       #   val.outcome <- p.max
#       # }
#       # else{
#       #   val.outcome <- -1 * n.max
#       # }
#       #   
#       # 
#       # state_output[[index]] <- val.outcome
#       # output.matrix[k, l] <- val.outcome
#       # output.matrix[l, k] <- val.outcome
#       
#       
#       index <- index + 1
#       
#       # Plotting the lag correlation
#       # plot(lag.corr, main = paste("Cross-correlation:", trial.signals[k], trial.signals[l]))
#     }
#   }
#   #assigns 0 diagnally because signals are the same
#   #diag(output.matrix) <- 0 # I CHANGE THE DIAGNOL TO BE ZERO: because they are the same signal so there's zero distance
#   #jul 17: keeping this as one because earlier we made it so that if there is zero distance it becomes 1/(0+1) = 1
#   diag(output.matrix) <- 1
#   state.val.list[[i]] <- state_signals
#   
#   # output.list[[i]] <- as.matrix(state_output)[,1:index]  # Assign sublist to output.list & Convert to matrix
#   output.list[[i]] <- state_output
#   output.matrix_list[[i]] <- output.matrix
#   
# }
# 
# # print
# output.matrix_list


#plot of 2 sigs: cmnt_cli & spent time indoors for ca

#   
# }
# 
# # print
# output.matrix_list


#plot of 2 sigs: cmnt_cli & spent time indoors for ca
alignment_figuring <- dtw(var1,var2, keep=TRUE, window.type="sakoechiba", window.size=c(21,21)) 
plot(alignment_figuring, type= "twoway")
#blue.pal <- colorRampPalette(c("lightblue","blue"))
dtwPlot(alignment_figuring, type = "density", main = "Alignment Path: \n Window Constraint of 21 days", sub= "Figure 1", xlab= "Covid-Like Illness in Community", ylab="Spent Time Indoors with Others")
lines(0:alignment_figuring$N, 0:alignment_figuring$N, col = "black", lty = 1)

dtwPlotAlignment(alignment_figuring, main = "Alignment Path: \n Window Constraint of 21 days", sub= "Figure 1", xlab= "Covid-Like Illness in Community", ylab="Spent Time Indoors with Others")
lines(0:alignment_figuring$N +21, col = "red", lty = 2) 
lines(0:alignment_figuring$N -21, col = "red", lty = 2)

dtwPlotDensity(alignment_figuring)
dtwPlotThreeWay(alignment_figuring, xts = var1, yts = var2, type.align = "l", main = "Alignment Path: Local Cost", sub= "Figure 2", xlab= "Covid-Like Illness in Community", ylab="Spent Time Indoors with Others")
#Best display of alignments
al <- alignment_figuring$localCostMatrix
image(x=seq_len(nrow(al)), y=seq_len(ncol(al)),al)
text(row(al),col(al),label = al, cex= 0.02)
lines(alignment_figuring$index1,alignment_figuring$index2)
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


plot(dtw(state_signals[[21]],state_signals[[24]], keep=TRUE, window.type="sakoechiba", window.size=c(0,21)),type="twoway")
plot(dtw(state_signals[[21]],-1*state_signals[[24]]+1, keep=TRUE, window.type="sakoechiba", window.size=c(0,21)),type="twoway")



pos.dtw.distance <- 1/(dtw(state_signals[[21]], state_signals[[24]], window.type = "sakoechiba", window.size = c(0,21))$distance + 1) #dist.method = "Euclidean" by default
neg.dtw.distance <- 1/(dtw(state_signals[[21]], -1*state_signals[[24]], window.type = "sakoechiba", window.size = c(0,21))$distance + 1)
pos.dtw.distance
neg.dtw.distance


#although the inverse is very similair to the the first signal, the pos dtw will always havea smaller #this is not correct log 03.09
#distance than the inverse



#Smaller distances indicate higher similarity, while larger distances indicate greater dissimilarity

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

#02/04:SAVES MATRIX TO FOLDER 
round_mat <- round(output.matrix_list[[1]], digits = 3)
state <- substring(colnames(rounded_matrix_data)[1], 1, 2)
file_name <- paste0("/Users/kristianny/Desktop/USC_data_2024/matrices/dtw/", state, "_DAILY_26_correct_hopefully_dtw_matrix_output_ROUND.csv")
write.csv(round_mat, file = file_name, row.names = TRUE)


# Make the adjacency matrix symmetric
#rounded_matrix_data[upper.tri(rounded_matrix_data)] <- rounded_matrix_data[lower.tri(rounded_matrix_data)]

graph <- graph_from_adjacency_matrix(abs(rounded_matrix_data), mode = "undirected", weighted = TRUE) 
sign.graph <- graph_from_adjacency_matrix(rounded_matrix_data, mode = "undirected", weighted = TRUE) 


# # Set the vertex labels as the signal names (removing the first 14 characters)
V(graph)$label <- ifelse(grepl("smoothed", colnames(rounded_matrix_data)),
                         substring(colnames(rounded_matrix_data), 14),
                         substring(colnames(rounded_matrix_data), 3))


# Extract the state from the original matrix_data
state <- substring(colnames(rounded_matrix_data)[1], 1, 2)

# Set the graph title as the state
graph_title <- paste( toupper(state), "DTW (threshold:", theta, ")") #toupper makes capitalized



graph <- igraph::simplify(graph, remove.loops = TRUE)
sign.graph <- igraph::simplify(sign.graph, remove.loops = TRUE)
#sign.graph<-  igraph::simplify(sign.graph, remove.loops = TRUE)


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
gephi_name <- paste0("/Users/kristianny/Desktop/USC_data_2024/graphs/", state, "_FINAL_practicing_gephi.graphml")
write_graph(graph, file = gephi_name, format = "graphml")#, vertex.attr = list(label = V(graph)$label))

#download edges reference
edge_ends <- ends(graph, E(graph))
# Get the shortened labels for the source and target vertices
source_labels <- V(graph)$label[edge_ends[, 1]]
target_labels <- V(graph)$label[edge_ends[, 2]]

# # Convert the result to a data frame
edge_labels <- data.frame(
  Source = as.character(edge_ends[, 1]),
  Target = as.character(edge_ends[, 2]),
  Label = list(format_edge_labels(edge_weights))
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
# Plot the directed graph with edge labels
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



#number of degree fro each node/vertice
#deg<- degree(graph, mode= "all")
#sign_deg <-  degree(sign.graph, mode= "all")
#hierarchical algorithm 

# 
# # Compute the layout with absolute values
# #layout <- layout_with_fr(graph, weights = E(graph)$weight,  niter = 20000)# , niter = 1000 #layout_with_sugiyama, #layout_with_kk 
# #layout <- layout_with_lgl(graph)
# layout <- layout_nicely(graph,  weights = E(graph)$weight)
# #layout <- layout_with_lgl(graph, maxiter = 5)
# #layout <- layout_with_sugiyama(graph, weights = E(graph)$weight)
# 
# #layout <- layout_with_kk(graph, weights = E(graph)$weight)
# # Get the edge weights of sign.graph
# #edge_weights <- get.edge.attribute(graph, "weight")
# 
# edge_weights <- as.numeric(E(sign.graph)$weight)
# 
# # format the edge labels with sign
# format_edge_labels <- function(weights) {
#   sign_labels <- ifelse(weights > 0, "+", "-")
#   paste(sign_labels, abs(weights))
# }
# 
# edge_labels <- format_edge_labels(edge_weights)
# 
# plot(graph,
#      edge.label = edge_labels,
#      layout = layout,
#      vertex.color = "white",
#      edge.color = "gray",
#      font = 2,
#      vertex.shape = "circle",
#      #vertex.size = deg * 2,
#      vertex.size = 1.5,
#      #edge.arrow.size = 0.4,
#      edge.width = 1.2,
#      vertex.label.cex = 0.6,
#      edge.label.color = "black",
#      edge.label.cex = 0.6,
#      #edge.curved = 0.2,  # Adjust the curvature level
#      main = graph_title)
# 





#### MATLAB ####

#install.packages("R.matlab")
library(R.matlab)
matlab.start()

# Read MATLAB script files

script1 <- readLines("/Users/kristianny/Desktop/DTW_S/shape_dtw_dist.m")
script2 <- readLines("/Users/kristianny/Desktop/DTW_S/shape_ts_transform.m")
script3 <- readLines("/Users/kristianny/Desktop/DTW_S/shapelet_transform.m")
script4 <- readLines("/Users/kristianny/Desktop/DTW_S/dtw_cons_md.m")

ind.states.ds <- norm.full.ds %>%
  filter(geo_value == "ca")%>%
  mutate(new_time_value = as.Date(time_value)) %>%
  arrange(geo_value) %>%
  filter(signal %in% sigs.final) %>% 
  group_by(signal) %>%
  arrange(new_time_value) %>%
  select(new_time_value,signal, norm.value)%>%
  pivot_wider(names_from = signal, values_from = norm.value, values_fn = list)%>%
  filter(new_time_value >= as.Date("2021-05-20") & new_time_value <= as.Date("2022-06-25"))

View(ind.states.ds)
dim(ind.states.ds) # 402  28


# Checking to see if there are any NULLS
ind.comp.wide.ds <- ind.states.ds[apply(ind.states.ds[, -1], 1, function(row) !any(sapply(row, is.null))), ]
dim(ind.comp.wide.ds) #402  28


ind_comp_wide_mat <- as.matrix(ind.comp.wide.ds[,-1])
ind_comp_wide_mat <- t(ind_comp_wide_mat)

# Convert the matrix to a character string in MATLAB syntax
data.mat <- sprintf("matrixMATLAB = [%s];", paste(ind_comp_wide_mat, collapse = ", "))

# Pass the matrix to MATLAB using matlab() function
matlab(data.mat)


# Execute the shape_dtw_dist function in MATLAB
for (line in script1) {
  result <- matlab(line)
}

# Call the shape_dtw_dist function in MATLAB 
matlab("shape_dtw_dist(data_mat, 21)")





# Access the output from MATLAB
result <- matlab("dist_mat")$get("dist_mat")
print(result)

matlab.quit()
























#### DATAFRAME WITH SPECIFIC SIGNALS ####
california <- norm.full.ds %>%
  filter(geo_value =="ca")

#HELPFUL INFO
unique(california$indicator[unique(california$signal %in% small.set.sig)])
unique(california$category_type[unique(california$signal %in% small.set.sig)])

small.ds  <- california$norm.value[california$signal %in% small.set.sig]


#california$norm.value[california$signal == "confirmed_admissions_covid_1d_7dav"]




#"MAKING GRAPHS TO VISUALLY SEE"   
small.img <- california %>%
  filter(signal %in% sample_sig) %>%
  filter(new_time_value >= as.Date("2021-05-20") & as.Date(new_time_value) <= "2022-06-25") %>%
  ggplot(aes(x = new_time_value, y = norm.value, color = as.factor(signal))) +
  geom_line(linewidth = 1.2, alpha = 0.5) +
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

new.img <- california %>%
  filter(signal %in% small.set.sig) %>%
  filter(new_time_value >= as.Date("2021-03-02") & as.Date(new_time_value) <= "2022-06-25") %>%
  ggplot(aes(x = new_time_value, y = norm.value, color = as.factor(signal))) +
  geom_line(linewidth = 1.2, alpha = 0.5) +
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


##SMALLER SUBSET WITH SMALLER DATE WINDOW
kk <- california %>%
  filter(signal %in% small.set.sig) %>%
  filter(new_time_value >= as.Date("2021-03-02") & as.Date(new_time_value) <= "2022-06-25") 

dddd <- kk %>% select(signal, category_type)
unique(dddd) #shows signals and the corresponding category type 

q <- list("smoothed_whh_cmnty_cli",
          "smoothed_wtested_positive_14d")

w <- list("smoothed_wpublic_transit_1d",
          "smoothed_wspent_time_indoors_1d",
          "smoothed_wwearing_mask_7d")
e<- list("smoothed_waccept_covid_vaccine_no_appointment",
         "smoothed_wbelief_govt_exploitation",
         "smoothed_wdontneed_reason_had_covid",
         "smoothed_wdontneed_reason_precautions",
         "smoothed_whesitancy_reason_distrust_gov",
         "smoothed_whesitancy_reason_religious",
         "smoothed_whesitancy_reason_sideeffects")

r <- list("smoothed_wreceived_news_friends",
          "smoothed_wreceived_news_govt_health",
          "smoothed_wtrust_covid_info_cdc",              
          "smoothed_wtrust_covid_info_govt_health",
          "smoothed_wtrust_covid_info_religious" ,
          "smoothed_wwant_info_relationships")
t <- list("deaths_incidence_num",
          "confirmed_admissions_covid_1d_7dav")
set.seed(1234) #twelve sig

#set.seed(2345) #tensig_b

# Randomly sample from each list
sampled_list <- c(sample(q, 2, replace = FALSE),
                  sample(w, 2, replace = FALSE),
                  sample(e, 3, replace = FALSE),
                  sample(r, 3, replace = FALSE),
                  sample(t, 2, replace = FALSE))

# The 'sampled_list' now contains the randomly sampled elements from the lists
print(sampled_list)


# What this does: each unique signal has its own column, each norm.value is arranged based on dates for each signal

# Filter the dataset for specific state
california.ds <- norm.full.ds %>%
  filter(geo_value == "ca", signal %in% sampled_list)

# rewrite  date column
california.ds <- california.ds %>%
  mutate(new_time_value = as.Date(time_value))

# Arrange the data by date within each signal
filtered.data <- california.ds %>%
  group_by(signal) %>%
  arrange(new_time_value) %>%
  select(new_time_value,signal, norm.value)

# Pivot the data to wide format
wide.data <- filtered.data %>%
  pivot_wider(names_from = signal, values_from = norm.value, values_fn = list) # this allows there to be multiple norm.value for a single date


df <- as.data.frame(wide.data)

#THIS WORKS FINALLY!!!!!!!! JUL 10
complete.wide.ds <- df[apply(df[, -1], 1, function(row) !any(sapply(row, is.null))), ] 

View(complete.wide.ds)


# date.min <- min(complete.wide.ds$new_time_value)
# date.max <- max(complete.wide.ds$new_time_value)



#Jul 11: trying to visualized the grouped data from similarity based in cleaning above^^
kk <- california %>%
  filter(signal %in% unique(grouped_data[[2]][["signal"]]))  #has 39 signals

dddd <- kk %>% select(signal, indicator)
sig.indic.ds <- as.data.frame(unique(dddd)) #shows signals and the corresponding category type 

similair.ca <- california %>%
  filter(signal %in% unique(grouped_data[[2]][["signal"]])) %>% #has 39 signals
  #filter(new_time_value >= as.Date(date.min) & new_time_value <= as.Date(date.max)) %>%
  ggplot(aes(x = new_time_value, y = norm.value, color = as.factor(signal))) +
  geom_line(linewidth = 1.2, alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.2) + #CI se=TRUE size=1.5 adjusts the size of the CI 
  facet_wrap_paginate(~ signal, scales = "free_y", ncol = 4, nrow = 10) +
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


unique(grouped_data[[26]][["signal"]])

#TRYING EUCLADIAN Minimum Spanning Tree
#install.packages("emstreeR")
# library(emstreeR)
# 
# out <- ComputeMST(complete.wide.ds[,c(2,3)], verbose = FALSE)
# plot(out)
# 
# 
# #install.packages("igraph")
# library(igraph)
# matrix_data <- as.matrix(complete.wide.ds[, -1])
# mst_graph <- graph_from_adjacency_matrix(matrix_data, mode = "undirected", weighted = TRUE)
# vertex_labels <- complete.wide.ds[, 1]
# V(mst_graph)$label <- vertex_labels
# plot(mst_graph, edge.label = E(mst_graph)$weight, layout = layout.fruchterman.reingold)






##saving the full 163 signals for california only 
# Filter the dataset for specific state
full.california.ds <- norm.full.ds %>%
  filter(geo_value =="ca")

# rewrite  date column
full.california.ds <- full.california.ds %>%
  mutate(new_time_value = as.Date(time_value))

# Arrange the data by date within each signal
filtered.data <- full.california.ds %>%
  group_by(signal) %>%
  arrange(new_time_value) %>%
  select(new_time_value,signal, norm.value)

# Pivot the data to wide format
full.california.wide <- filtered.data %>%
  pivot_wider(names_from = signal, values_from = norm.value, values_fn = list) # this allows there to be multiple norm.value for a single date

df <- as.data.frame(full.california.wide)

#not working 
ca.complete.wide.ds <- df[apply(df[, -1], 1, function(row) !any(sapply(row, is.null))), ] 
#complete.wide.ds <- df[apply(df[, -1], 1, function(row) !any(sapply(row, function(x) is.null(x) || is.na(x)))), ]



#SAVING AS CSV FILE
# Convert list columns to character vectors bc they were a list and can't save as csv file
full.california.wide <- full.california.wide %>%
  mutate(across(where(is.list), ~ purrr::map_chr(., function(x) if (length(x) == 0) NA_character_ else as.character(x))))

file.path <- "/Users/kristianny/Desktop/full.california.193.csv" #have to change the name for each file
write.csv(full.california.wide, file = file.path, row.names = FALSE)



























#We want: evolving perception of risk, trust in intervention, and the community's influence 
#We wish: model and verify joint probabilities
#Accomplished: visualize all the signals in a scale where we can see the variations
#Current Goal: Identify the relationship and visually find which signals are related and how 
##potential exploration that hasnt been done: schooling indicators, vaccination indicators, and beliefs & exp










#### DID NOT WORK ####

# Attempting to get the index correct for the double for loop for asssigning the lag values to the correct index
#isFirst <- TRUE  # Track the first time: j - i == 1

# a <- 1:(length(trial.signals)-1)
# b <- max(a)+1:length(trial.signals)

# if (j - i == 1 && isFirst) {
#   index <- 1
#   isFirst <- FALSE
# } else if (j - i %in% 1:length(output.list)) {
#   index <- (j + i) - 1
# } else {
#   index <- j - i
# }
# output.list[[index]] <- lag.corr$acf
#filtering using subset function the values in the list that do not fit the threshold
#trying:??? 
#pair.corr <- acf(ts.union(norm.full.ds$value[norm.full.ds$signal == "smoothed_winperson_school_fulltime_oldest", norm.full.ds$value[norm.full.ds$signal == "smoothed_winperson_school_parttime_oldest"]))

# ### trying again
# # Filter the dataset
# filtered_data <- norm.full.ds %>%
#   filter(geo_value == "ca", signal %in% small.set.sig)
# 
# # Convert filtered data to wide format
# wide_data <- filtered_data %>%
#   pivot_wider(names_from = signal, values_from = norm.value)
# 
# # Print the resulting wide_data
# print(wide_data)
# 
# 
# 
# 
# # Filter the dataset
# filtered_data <- norm.full.ds %>%
#   filter(geo_value == "ca", signal %in% small.set.sig)
# 
# # Merge the norm.values by matching dates
# merged_data <- filtered_data %>%
#   select(new_time_value, signal, norm.value) %>%
#   left_join(filtered_data %>%
#               select(new_time_value, signal, norm.value),
#             by = c("new_time_value", "signal"))
# 
# # Print the merged_data
# print(merged_data)
# 
# 
# # Filter the data for California and the desired signals
# california_filtered <- norm.full.ds %>%
#   filter(geo_value == "ca", signal %in% small.set.sig)
# 
# # Convert new_time_value to Date data type
# california_filtered <- california_filtered %>%
#   mutate(new_time_value = as.Date(new_time_value))
# 
# # Pivot the dataframe into a wider format
# wide_data <- california_filtered %>%
#   pivot_wider(names_from = signal, values_from = norm.value, values_fill = NA)
# 
# # Gather the signal columns into a new_time_value and norm.value column
# # Gather the signal columns into a new_time_value and norm.value column
# 
# 
# # Define the column names to exclude
# columns_to_exclude <- c("data_source", "time_value", "geo_type",
#                         "time_type", "issue", "lag", "missing_value",
#                         "missing_stderr", "missing_sample_size",
#                         "stderr", "sample_size", "dt",
#                         "indicator",
#                         "value", "source", "category_type")
# 
# # Filter the wide_data dataframe to exclude the specified columns
# filtered_data <- wide_data %>%
#   select(-one_of(columns_to_exclude))
# #View(wide_data)
# View(filtered_data)
# 
# 
# 
# 
# 
# ### last attempt for today ####
# # Filter the data for California and the desired signals
# california_filtered <- norm.full.ds %>%
#   filter(geo_value == "ca", signal %in% small.set.sig)
# 
# california_filtered <- california_filtered %>%
#   mutate(new_time_value = as.Date(new_time_value))
# 
# # Create a list of data frames for each signal
# signal_data <- map(small.set.sig, ~ filter(california_filtered, signal == .x))
# 
# # Merge the data frames by date
# merged_data <- reduce(signal_data, full_join, by = "new_time_value")
# 
# # Print the resulting merged data
# print(merged_data)
# 
# 
# # Define the column names to exclude
# columns_to_exclude <- c("data_source", "time_value", "geo_type",
#                         "time_type", "issue", "lag", "missing_value",
#                         "missing_stderr", "missing_sample_size",
#                         "stderr", "sample_size", "dt",
#                         "indicator",
#                         "value", "source", "category_type")
# 
# # Filter the wide_data dataframe to exclude the specified columns
# filtered_data <- merged_data %>%
#   select(-one_of(columns_to_exclude))
# #View(wide_data)
# View(filtered_data)
# 
# 
# ###LAST LAST ONE!! 
# # Filter the data for California and the desired signals
# california_filtered <- norm.full.ds %>%
#   filter(geo_value == "ca", signal %in% small.set.sig)
# 
# # Split the data into separate datasets by signal
# signal_datasets <- split(california_filtered, california_filtered$signal)
# # Perform a full join on the datasets by time_value
# merged_data <- reduce(signal_datasets, full_join, by = "time_value")
# View(merged_data)
# 
# wide_data <- pivot_wider(merged_data, names_from = signal.x, values_from = norm.value)
# View(wide_data)
# 
# ###LASTSTTTTTT
# # Filter the data for California and the desired signals
# california_filtered <- norm.full.ds %>%
#   filter(geo_value == "ca", signal %in% small.set.sig)
# 
# # Create a list of data frames for each signal
# signal_datasets <- split(california_filtered, california_filtered$signal)
# 
# # Perform a full join on the datasets by time_value
# merged_data <- reduce(signal_datasets, left_join, by = "new_time_value")
# 
# # Select the desired columns, including the norm.value column
# #merged_data <- merged_data %>%
# #  select(time_value, signal, norm.value)
# 
# # Pivot the merged_data wider
# wide_data <- merged_data %>%
#   pivot_wider(names_from = signal, values_from = norm.value)
# 
# # Print the resulting wide_data
# print(wide_data)
# 
# # Filter the data for California and the desired signals
# california_filtered <- norm.full.ds %>%
#   filter(geo_value == "ca", signal %in% small.set.sig)
# 
# # Create an empty dataframe to store the merged data
# merged_data <- data.frame(time_value = unique(california_filtered$time_value))
# 
# # Loop through each unique signal
# for (signal in unique(california_filtered$signal)) {
#   # Subset the data for the current signal
#   signal_data <- california_filtered[california_filtered$signal == signal, ]
#   
#   # Perform a left join on the datasets by time_value
#   merged_data <- merged_data %>%
#     left_join(signal_data, by = "time_value")
# }
# 
# # Print the resulting merged dataset
# print(merged_data)
# 
# View(merged_data)




#### figuring things out####
# Behavior Indicators #
# Mask Use #
mask.a <- covidcast_signal(data_source = "fb-survey", signal = "smoothed_wwearing_mask_7d",
                           geo_type = "state", time_type ="day")
mask.b <- covidcast_signal(data_source = "fb-survey", signal = "smoothed_wwearing_mask",
                           geo_type = "state", time_type ="day")
mask.c <- covidcast_signal(data_source = "fb-survey", signal = "smoothed_wothers_masked_public",
                           geo_type = "state", time_type ="day")
mask.d <- covidcast_signal(data_source = "fb-survey", signal = "smoothed_wothers_masked",
                           geo_type = "state", time_type ="day")



#new start 
data <- covidcast_meta()
print(data)

filtered_data <- subset(data, data_source == "fb-survey" & geo_type == "state" & time_type == "day")
print(filtered_data)



filtered_data$start_date <- as.Date(filtered_data$min_time)
filtered_data$end_date <- as.Date(filtered_data$max_time)


calculate_similarity <- function(start1, end1, start2, end2) {
  start_diff <- abs(start1 - start2)
  end_diff <- abs(end1 - end2)
  similarity <- start_diff + end_diff
  return(similarity)
}

grouped_data <- list()
grouped_data[[1]] <- filtered_data[1, ]  # Initialize the first group with the first signal

# Iterate through the remaining signals
for (i in 2:nrow(filtered_data)) {
  is_grouped <- FALSE  # Flag to indicate if the signal has been grouped with any existing group
  
  # Compare the signal with each existing group
  for (j in 1:length(grouped_data)) {
    start_diff <- abs(filtered_data$start_date[i] - grouped_data[[j]]$start_date)
    end_diff <- abs(filtered_data$end_date[i] - grouped_data[[j]]$end_date)
    
    # Define a threshold for similarity, adjust it as per your requirements
    if (max(start_diff, end_diff) <= 7) {  # If the signal is similar to the group
      grouped_data[[j]] <- rbind(grouped_data[[j]], filtered_data[i, ])
      is_grouped <- TRUE
      break  # Exit the loop once the signal is grouped
    }
  }
  
  # If the signal is not grouped with any existing group, create a new group
  if (!is_grouped) {
    grouped_data[[length(grouped_data) + 1]] <- filtered_data[i, ]
  }
}

print(grouped_data)
View(grouped_data)
grouped_data[[4]]

grouped_data[[4]]$signal

# signal.name <- c( "smoothed_waccept_covid_vaccine_no_appointment" , "smoothed_wappointment_not_vaccinated",  
#    "smoothed_wappointment_or_accept_covid_vaccine"   , "smoothed_wbelief_created_small_group"          ,  
#    "smoothed_wbelief_distancing_effective"           , "smoothed_wbelief_govt_exploitation"            ,  
#   "smoothed_wcovid_vaccinated_appointment_or_accept" , "smoothed_wcovid_vaccinated_friends"            ,  
#    "smoothed_wdelayed_care_cost"                     , "smoothed_whad_covid_ever"                      ,  
#    "smoothed_worried_catch_covid"                    , "smoothed_wothers_masked_public"                ,  
#    "smoothed_wrace_treated_fairly_healthcare"        , "smoothed_wreceived_news_cdc"                   ,  
#    "smoothed_wreceived_news_experts"                 , "smoothed_wreceived_news_friends"               ,  
#    "smoothed_wreceived_news_govt_health"             , "smoothed_wreceived_news_journalists"           ,  
#    "smoothed_wreceived_news_local_health"            , "smoothed_wreceived_news_none"                  ,  
#    "smoothed_wreceived_news_politicians"             , "smoothed_wreceived_news_religious"             ,  
#    "smoothed_wtrust_covid_info_cdc"                  , "smoothed_wtrust_covid_info_doctors"            ,  
#    "smoothed_wtrust_covid_info_experts"              , "smoothed_wtrust_covid_info_friends"            ,  
#    "smoothed_wtrust_covid_info_govt_health"          , "smoothed_wtrust_covid_info_journalists"        ,  
#    "smoothed_wtrust_covid_info_politicians"          , "smoothed_wtrust_covid_info_religious"          ,  
#    "smoothed_wwant_info_children_education"          , "smoothed_wwant_info_covid_treatment"           ,  
#    "smoothed_wwant_info_covid_variants"              , "smoothed_wwant_info_employment"                ,  
#    "smoothed_wwant_info_mental_health"               , "smoothed_wwant_info_none"                      ,  
#    "smoothed_wwant_info_relationships"               , "smoothed_wwant_info_vaccine_access"            ,  
#    "smoothed_wwant_info_vaccine_types"               , "smoothed_wworried_catch_covid" )


#signal_names_full <- lapply(grouped_data, function(group) group$signal)

signal_names <- lapply(grouped_data, function(group) {
  filtered_signals <- subset(group, grepl("^smoothed_w", signal)) # ^ makes sure that smoothed_w is at the beginning of the string
  filtered_signals$signal
})

#finding out those with ww 
w_names <- lapply(grouped_data, function(group) {
  filtered_signals <- subset(group, grepl("^smoothed_ww", signal)) # ^ makes sure that smoothed_w is at the beginning of the string
  filtered_signals$signal
})


# Define the patterns to exclude
exclude_patterns <- c("smoothed_want_info_children_education", "smoothed_want_info_covid_treatment",
                      "smoothed_want_info_covid_variants", "smoothed_want_info_employment",
                      "smoothed_want_info_mental_health",    "smoothed_want_info_none",              
                      "smoothed_want_info_relationships",    "smoothed_want_info_vaccine_access",    
                      "smoothed_want_info_vaccine_types",    "smoothed_worried_catch_covid")

signal_names <- lapply(grouped_data, function(group) {
  filtered_signals <- subset(group, grepl("^smoothed_w", signal) & !signal %in% exclude_patterns)
  filtered_signals$signal
})

#want a for loop that aggregates the signals for each list

aggregate.list <- list()

for (i in 1:length(signal_names)) {
  group <- covidcast_signals(data_source = "fb-survey", signal = signal_names[[i]],
                             geo_type = "state", time_type="day")
  aggregate.list[[paste("Group", i, sep = "_")]] <- group
}


new.ds <- covidcast_signals(data_source = "fb-survey", signal = signal.name,
                            geo_type = "state")``

agg.ds <- aggregate_signals(new.ds, dt = NULL, format = c("long"))

#Min-Max normalization

agg.ds$new_time_value <- ymd(agg.ds$time_value)
#checking for NA values
sum(is.na(agg.ds$value)) #6767
length(agg.ds$value) #806725
# excluding ___ of the data when calculating min-max normalization 

final.ds <- agg.ds %>%
  group_by(signal) %>%
  #na.rm = TRUE is so that any NA values in the value column are ignored
  mutate(new.value = scale(value, center = min(value, na.rm = TRUE), scale = max(value, na.rm = TRUE) - min(value, na.rm = TRUE))) %>%
  ungroup()

summary(final.ds$new.value)

#summary for each of the signals
by(final.ds$new.value, final.ds$signal, summary)

ds <- final.ds


# ca <- ds %>%
#   filter(geo_value == "ca") %>%
#   ggplot(aes(x = new_time_value, y = new.value, color = as.factor(signal))) +
#   geom_line(linewidth = 1.2, alpha = 0.7) +
#   geom_smooth(method = "lm", formula = y ~ x, se = TRUE, size = 0.2) +
#   facet_wrap(~ signal, scales = "free_y", ncol = 1, nrow = 10) +
#   #facet_wrap_paginate(n = 10)+
#   labs(x = "Time", y = "Value", color = "Signal") +
#   scale_color_manual(values = viridis(nlevels(as.factor(ds$signal)))) +
#   theme_minimal() +
#   theme(
#     legend.position = "top",
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line = element_line(linewidth = 0.8),
#     axis.text = element_text(size = 8),
#     axis.title = element_text(size = 12, face = "bold"),
#     legend.text = element_text(face = "bold"),
#     plot.title = element_text(hjust = 0.5)
#   ) +
#   scale_x_date(date_labels = "%b %Y") +
#   labs(title = "California")
# 
# ca

#all states

all.states<-  ds %>%
  ggplot(aes(x = new_time_value, y = new.value, color = as.factor(signal))) +
  geom_line(linewidth = 1.2, alpha = 0.7) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, size = 0.2) +
  facet_wrap_paginate(~ signal, scales = "free_y", ncol = 5, nrow = 8) +
  labs(x = "Time", y = "Value", color = "Signal") +
  scale_color_manual(values = viridis(nlevels(as.factor(ds$signal)))) +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_x_date(date_labels = "%b %Y")
all.states

#state by state
ca <- ds %>%
  filter(geo_value == "ca") %>%
  ggplot(aes(x = new_time_value, y = new.value, color = as.factor(signal))) +
  geom_line(linewidth = 1.2, alpha = 0.7) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, size = 0.2) +
  facet_wrap_paginate(~ signal, scales = "free_y", ncol = 5, nrow = 8) +
  labs(x = "Time", y = "Value", color = "Signal") +
  scale_color_manual(values = viridis(nlevels(as.factor(ds$signal)))) +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_x_date(date_labels = "%b %Y") +
  labs(title = "California")

ca 



#Social Distancing & Travel

#Schooling Indicators
#Schooling Type

# School Safety Measures

# Testing Indicators 



# start_dates <- as.Date(filtered_data$min_time)
# start_date_differences <- outer(start_dates, start_dates, "-")  # Calculate pairwise differences in start dates
# threshold <- 7  # Set the threshold for similarity (e.g., 7 days)
# similar_signals <- which(start_date_differences <= threshold, arr.ind = TRUE)
# similar_data <- filtered_data[similar_signals[, 1], ]
# print(similar_data)


# finguring stuff out 
# a <- covidcast_signals("usa-facts", signal = c("confirmed_incidence_num",
#                                                "deaths_incidence_num"),
#                        start_day = "2020-08-15", end_day = "2020-10-01")
# a
# new.a <- aggregate_signals(a, dt = NULL, format = c("long"))
# new.a





#### DID WORK BUT NO LONGER NEEDED ####
# What this does: each unique signal has its own column, each norm.value is arranged based on dates for each signal

# Filter the dataset for specific state
california.ds <- norm.full.ds %>%
  filter(geo_value == "ca", signal %in% small.set.sig)

# rewrite  date column
california.ds <- california.ds %>%
  mutate(new_time_value = as.Date(time_value))

# Arrange the data by date within each signal
filtered.data <- california.ds %>%
  group_by(signal) %>%
  arrange(new_time_value) %>%
  select(new_time_value,signal, norm.value)

# Pivot the data to wide format
wide.data <- filtered.data %>%
  pivot_wider(names_from = signal, values_from = norm.value, values_fn = list) # this allows there to be multiple norm.value for a single date

#View(wide.data)


complete.wide.ds <-  as.data.frame(wide.data) %>%
  filter(!if_any(across(-1), is.null) | !if_any(across(-1), is.na))
#Notes: selects(wide.data, excludes first column b/c its date) checks to see if any of the values are na for each row
# sums the number of NA values for each row and compares it to != 0
# If there are NA's in that row, the statement becomes !(true) => FALSE. the row is not included
# !(false) => TRUE for any rows that do not have any NA values, excluding the first column
View(complete.wide.ds)



#SAVING AS CSV FILE
# Convert list columns to character vectors bc they were a list and can't save as csv file
wide.data <- wide.data %>%
  mutate(across(where(is.list), ~ purrr::map_chr(., function(x) if (length(x) == 0) NA_character_ else as.character(x))))

file.path <- "/Users/kristianny/Desktop/twenty_sig.csv" #have to change the name for each file
write.csv(wide.data, file = file.path, row.names = FALSE)


# Specify the number of states

threshold <- 0.4
states <- list ()

# Specify the COVIDcast signals to use  
signal.name <- list(
  smoothed_winperson_school_fulltime_oldest = norm.full.ds$value[norm.full.ds$signal == "smoothed_winperson_school_fulltime_oldest"],
  smoothed_winperson_school_parttime_oldest = norm.full.ds$value[norm.full.ds$signal == "smoothed_winperson_school_parttime_oldest"]
)

#gonna do pairwise for multiple signals
# Calculate and plot the lag correlation for each pair of signal.name
for (i in 1:(length(signal.name)-1)) {
  for (j in (i+1):length(signal.name)) {
    var1 <- signal.name[[i]]
    var2 <- signal.name[[j]]
    
    # Calculating the lag correlation
    lag.corr <- ccf(var1, var2, plot = FALSE)
    
    # Print 
    cat("Cross-correlation between", names(signal.name)[i], "and", names(signal.name)[j], ":\n")
    
    #This is an array with the same dimensions as lag containing the estimated acf (auto-correlation function estimation.
    print(lag.corr$acf) #save this to a variable type list
    cat("\n")  
    
    # Plotting the lag correlation
    plot(lag.corr, main = paste("Cross-correlation:", names(signal.name)[i], names(signal.name)[j]))
  }
}


# trying for just the signals, all values
#gonna do pairwise for multiple signals

threshold <- 0.4 # initializing threshold
output.list <- list()



# Specify the COVIDcast signals to use  
trial.signals <- unique(norm.full.ds$signal[norm.full.ds$category_type == "mask_use"])
a <- choose(length(trial.signals), 2) # helper variable to see how long the output.list is gonna be
index<- 1


## TO DO: FILTER THE SIGNALS TO MAKE SURE THE DATE ARE NO MORE THAN 2 MOTNHS APART USE FROM EARLIER^^^
#extracts all the values in the signal
value.list <- list()
for (i in 1:length(trial.signals)){
  #a <- str(trial.signals[i])
  #value.list[[i]] <-  norm.full.ds$norm.value[norm.full.ds$signal == trial.signals[i]]
  value.list[[i]] <- norm.full.ds %>%
    filter(geo_value == "ca", signal == trial.signals[i]) %>%
    select(signal,norm.value, geo_value)
}
# Calculate and plot the lag correlation for each pair of signal.name

for (i in 1:(length(trial.signals)-1)) {
  for (j in (i+1):length(trial.signals)) {
    var1 <- value.list[[i]]
    var2 <- value.list[[j]]
    
    # Calculating the lag correlation
    lag.corr <- ccf(var1, var2, plot = FALSE)
    
    # Print 
    cat("Cross-correlation between", trial.signals[i], "and", trial.signals[j], ":\n")
    
    #This is an array with the same dimensions as lag containing the estimated acf (auto-correlation function estimation.
    print(lag.corr$acf) #save this to a variable type list
    cat("\n")  
    
    if (index <= a){
      output.list[[index]] <- lag.corr$acf
    }
    
    output.list[[index]] <- lag.corr$acf
    
    index = index + 1
    
    # Plotting the lag correlation
    plot(lag.corr, main = paste("Cross-correlation:",  trial.signals[i], trial.signals[j]))
  }
}


# trying by state for signals, all values#


states <- list("ca", "ny") # Specify the number of states


# Specify the COVIDcast signals to use  
trial.signals <- unique(norm.full.ds$signal[norm.full.ds$category_type == "mask_use"])


num_signals <- length(trial.signals)
num_states <- length(states)
state.val.list <- vector("list", length = num_signals * num_states)
state.index <- 1


for (state in states) {
  signal.index <- 1
  for (i in 1:length(trial.signals)) {
    state.val.list[[state.index]] <- norm.full.ds %>%
      filter(geo_value == state, signal == trial.signals[i]) %>%
      select(signal,norm.value, geo_value)
    state.index <- state.index + 1
    signal.index <- signal.index + 1
  }
}

View(state.val.list) #a list of 8



trial.signals <- unique(norm.full.ds$signal[norm.full.ds$category_type == "mask_use"])
num_signals <- length(trial.signals)
num_states <- length(states)
state.val.list <- vector("list", length = num_states)
# THIS DOES WORK
for (i in 1:num_states) {
  state <- states[i]
  state_signals <- list()
  
  for (j in 1:num_signals) {
    one.sig <- trial.signals[j]
    state_data <- norm.full.ds %>%
      filter(geo_value == state, signal == one.sig) %>%
      select(signal, norm.value, geo_value) ##this was to check that the correct states and signals were being added
    
    state_signals[[j]] <- state_data
  }
  
  state.val.list[[i]] <- state_signals
}

View(state.val.list)#a list of 2, for each list there are 4 signals


# ### working on
#  a <- choose(length(trial.signals), 2) # helper variable to see how long the output.list is gonna be
#  index<- 1
# 
#  for (i in 1:num_states) {
#    state <- states[i]
#    state_signals <- list()
# 
#    #4 signals values will be saved in state_signals
#    for (j in 1:num_signals) {
#      one.sig <- trial.signals[j]
#      state_data <- norm.full.ds %>%
#        filter(geo_value == state, signal == one.sig) %>%
#        select(norm.value) ##ONLY NORM VALUE
# 
#      state_signals[[j]] <- state_data
#    }
#    
#    
#    #i want it to do 6 combos 
#    for (k in 1:(length(trial.signals)-1)) {
#      for (l in (i+1):length(trial.signals)) {
#        var1 <- state_signals[[k]]
#        var2 <- state_signals[[l]]
# 
#        # Calculating the lag correlation
#        lag.corr <- ccf(var1, var2, plot = FALSE)
# 
#        # Print
#        cat("Cross-correlation between", trial.signals[k], "and", trial.signals[l], ":\n")
# 
#        #This is an array with the same dimensions as lag containing the estimated acf (auto-correlation function estimation.
#        print(lag.corr$acf) #save this to a variable type list
#        cat("\n")
# 
#        if (index <= a){
#          output.list[[index]] <- lag.corr$acf
#        }
# 
#        #output.list[[index]] <- lag.corr$acf
# 
#        index = index + 1
#        
#        if (index > a){
#          index = 1
#        }
# 
#        # Plotting the lag correlation
#        #plot(lag.corr, main = paste("Cross-correlation:",  trial.signals[k], trial.signals[l]))
#      }
#    }
# 
#    state.val.list[[i]] <- output.list
#  }
# 
# View(output.list)
# View(state.val.list) #list of list: [[california signals],[new york signals] ]


## FILTER THE SIGNALS TO MAKE SURE THE DATE ARE NO MORE THAN 2 MOTNHS APART USE FROM EARLIER^^^
#extracts all the values in the signal
value.list <- list()
for (i in 1:length(trial.signals)){
  #a <- str(trial.signals[i])
  value.list[[i]] <-  norm.full.ds$norm.value[norm.full.ds$signal == trial.signals[i]]
}
# Calculate and plot the lag correlation for each pair of signal.name

for (i in 1:(length(trial.signals)-1)) {
  for (j in (i+1):length(trial.signals)) {
    var1 <- value.list[[i]]
    var2 <- value.list[[j]]
    
    # Calculating the lag correlation
    lag.corr <- ccf(var1, var2, plot = FALSE)
    
    # Print 
    cat("Cross-correlation between", trial.signals[i], "and", trial.signals[j], ":\n")
    
    #This is an array with the same dimensions as lag containing the estimated acf (auto-correlation function estimation.
    print(lag.corr$acf) #save this to a variable type list
    cat("\n")  
    
    if (index <= a){
      output.list[[index]] <- lag.corr$acf
    }
    
    output.list[[index]] <- lag.corr$acf
    
    index = index + 1
    
    # Plotting the lag correlation
    plot(lag.corr, main = paste("Cross-correlation:",  trial.signals[i], trial.signals[j]))
  }
}




#Figuring out for loops for outcome







#Jul 11:
#### LOOPS for the absolute value only####



###wWORKSS JUL 10

#TRYING TO MAKE IT TO A MATRIX
trial.signals <- unique(norm.full.ds$signal[norm.full.ds$category_type == "mask_use"]) # 4signals
a <- choose(length(trial.signals), 2) # helper variable to see how long the output.list is gonna be
states <- list("ca", "ny") # Specify the number of states
num_signals <- length(trial.signals)
num_states <- length(states)
state.val.list <- vector("list", length = num_states)


output.list <- vector("list", length = num_states)

for (i in 1:num_states) {
  state <- states[i]
  state_signals <- list()
  state_output <- list()  # Initialize sublist for output
  
  for (j in 1:num_signals) {
    one.sig <- trial.signals[j]
    state_data <- norm.full.ds %>%
      filter(geo_value == state, signal == one.sig) %>%
      select(norm.value) ## ONLY NORM VALUE
    
    state_signals[[j]] <- state_data  #4signal values
  }
  
  index <- 1
  #does pairwise combination in this case 6
  for (k in 1:(num_signals - 1)) {
    for (l in (k + 1):num_signals) {
      var1 <- state_signals[[k]]
      var2 <- state_signals[[l]]
      
      
      # Calculating the lag correlation: max lag of 35 days aka 5 weeks
      lag.corr <- ccf(var1, var2, plot = FALSE, lag.max = 35)
      
      # Print 
      cat("Cross-correlation between", trial.signals[k], "and", trial.signals[l], ":\n")
      
      # acf (auto-correlation function estimation).
      acf.values <- lag.corr$acf
      print(acf.values) 
      cat("\n")
      
      max.abs.value <- max(abs(acf.values))
      state_output[[index]] <- max.abs.value
      #state_output[[index]] <- as.matrix(lag.corr$acf)[,1:a]  # Convert to matrix k:did not work
      
      index <- index + 1
      
      # Plotting the lag correlation
      #plot(lag.corr, main = paste("Cross-correlation:", trial.signals[k], trial.signals[l]))
    }
  }
  
  state.val.list[[i]] <- state_signals
  
  #output.list[[i]] <- as.matrix(state_output)[,1:index]  # Assign sublist to output.list & Convert to matrix
  output.list[[i]] <- state_output
}

View(state.val.list) # list of list: [[california signals],[new york signals] ]
View(output.list) # list of list: [[california output],[new york output] ]
View(output.list[[1]])

##next step is to convert the output.list into a matrix
print(output.list)

#worked:
output.matrix <- lapply(output.list, function(x) {
  max_len <- max(sapply(x, length))
  matrix(unlist(lapply(x, function(m) {
    if (length(m) < max_len) {
      m <- c(m, rep(NA, max_len - length(m)))
    }
    m
  })), nrow = max_len)
})


# 1 x 6 matrix (just contains the absolute lag corr for each combo ie 6 )




#IGRAPH

#creating n x n matrix where n= #signals
#trial.signals <- unique(norm.full.ds$signal[norm.full.ds$category_type == "mask_use"]) # 4 signals
trial.signals<- sampled_list
a <- choose(length(trial.signals), 2) # helper variable to see how long the output.list is gonna be
states <- list("ca", "ny") # Specify the number of states
num_signals <- length(trial.signals)
num_states <- length(states)
state.val.list <- vector("list", length = num_states)

output.list <- vector("list", length = num_states)
output.matrix_list <- vector("list", length = num_states)

for (i in 1:num_states) {
  state <- states[i]
  state_signals <- list()
  state_output <- list()  # Initialize sublist for output
  
  for (j in 1:num_signals) {
    one.sig <- trial.signals[j]
    state_data <- norm.full.ds %>%
      filter(geo_value == state, signal == one.sig) %>%
      select(norm.value) ## ONLY NORM VALUE
    
    state_signals[[j]] <- state_data  # 4 signal values
  }
  
  index <- 1
  output.matrix <- matrix(NA, nrow = num_signals, ncol = num_signals, dimnames = list(paste0(states[i], "_", trial.signals),
                                                                                      paste0(states[i], "_", trial.signals)))
  #before it was dimnames = list(trial.signals, trial.signals) <- only had the signal names
  
  
  # Does pairwise combination in this case 6
  for (k in 1:(num_signals - 1)) {
    for (l in (k + 1):num_signals) {
      var1 <- state_signals[[k]]
      var2 <- state_signals[[l]]
      
      # Calculating the lag correlation: max lag of 35 days aka 5 weeks
      lag.corr <- ccf(var1, var2, plot = FALSE, lag.max = 35)
      
      # Print 
      cat("Cross-correlation between", trial.signals[[k]], "and", trial.signals[[l]], ":\n")
      
      # ACF (auto-correlation function estimation).
      acf.values <- lag.corr$acf
      print(acf.values) 
      cat("\n")
      
      max.abs.value <- max(abs(acf.values))
      state_output[[index]] <- max.abs.value
      output.matrix[k, l] <- max.abs.value
      output.matrix[l, k] <- max.abs.value
      
      index <- index + 1
      
      # Plotting the lag correlation
      # plot(lag.corr, main = paste("Cross-correlation:", trial.signals[k], trial.signals[l]))
    }
  }
  
  #assigns 1 diagnally because signals are the same
  diag(output.matrix) <- 1
  state.val.list[[i]] <- state_signals
  
  # output.list[[i]] <- as.matrix(state_output)[,1:index]  # Assign sublist to output.list & Convert to matrix
  output.list[[i]] <- state_output
  output.matrix_list[[i]] <- output.matrix
}

#View(output.matrix_list)
print(output.matrix_list)
View(state.val.list) # list of list: [[california signals],[new york signals] ]
View(output.list) # list of list: [[california output],[new york output] ]
View(output.list[[1]])

# California
print(output.matrix_list[[1]])

# New York
print(output.matrix_list[[2]])




# FILTERING THE FINAL OUTPUT MATRIX BASED ON THRESHOLD
theta <- 0.5  # Threshold 

filtered_output_matrix <- lapply(output.matrix_list, function(m) {
  m[m < theta] <- 0
  m
})

print(filtered_output_matrix)
#last.df <- as.data.frame(filtered_output_matrix)
#View(last.df)

#write.csv(filtered_output_matrix, "twelve_sig_filtered_output.csv")  # Export to CSV file



matrix_data <- as.matrix(filtered_output_matrix[[1]])
mst_graph <- graph_from_adjacency_matrix(matrix_data, mode = "directed", weighted = TRUE)
vertex_labels <- colnames(matrix_data)
V(mst_graph)$label <- vertex_labels
plot(mst_graph, edge.label = E(mst_graph)$weight, layout = ) #layout.fruchterman.reingold





# Convert the filtered_output_matrix to a matrix
matrix_data <- as.matrix(filtered_output_matrix[[2]])

# Create a directed graph from the matrix
graph <- graph_from_adjacency_matrix(matrix_data, mode = "directed", weighted = TRUE) 

# Set the vertex labels as the signal names
V(graph)$label <- colnames(matrix_data)

# Plot the graph
#plot(graph, edge.label = E(graph)$weight) #ignores the edge weights if there are non-positive weights, which includes negative weights. 
plot(graph,
     edge.label = E(graph)$weight,
     layout = layout_with_kk,
     #layout = layout.fruchterman.reingold,
     repulserad = 20,
     vertex.color = "orange", 
     edge.color = "gray", 
     vertex.shape = "circle", 
     vertex.size = 15,
     edge.arrow.size = 0.5,
     edge.width = 2,
     vertex.label.cex = 1,
     edge.label.color = "black",
     edge.label.cex = 0.8)











plot(graph,
     edge.label = E(graph)$weight,
     layout = layout_with_kk(graph, scale = 0.5),
     vertex.color = "orange",
     edge.color = "gray",
     font = 2,
     vertex.shape = "circle",
     vertex.size = 8,
     edge.arrow.size = 0.5,
     edge.width = 2,
     vertex.label.cex = 1,
     edge.label.color = "black",
     edge.label.cex = 0.8)






#JUL 13: old draft of loops, new draft properly does lag(a,b) lag(b,a)
#### LOOPS ####   
# 
# output.list <- vector("list", length = num_states)
# output.matrix_list <- vector("list", length = num_states)
# 
# for (i in 1:num_states) {
#   state <- states[i]
#   state_signals <- list()
#   state_output <- list()  # Initialize sublist for output
#   
#   for (j in 1:num_signals) {
#     one.sig <- trial.signals[j]
#     state_data <- norm.full.ds %>%
#       filter(geo_value == state, signal == one.sig) %>%
#       select(norm.value) ## ONLY NORM VALUE
#     
#     state_signals[[j]] <- state_data  # 4 signal values
#   }
#   
#   index <- 1
#   output.matrix <- matrix(NA, nrow = num_signals, ncol = num_signals, dimnames = list(paste0(states[i], "_", trial.signals),
#                                                                                       paste0(states[i], "_", trial.signals)))
#   #before it was dimnames = list(trial.signals, trial.signals) <- only had the signal names
#   
#   
#   
#   for (k in 1:(num_signals - 1)) {
#     for (l in (k + 1):num_signals) {
#       var1 <- state_signals[[k]]
#       var2 <- state_signals[[l]]
#       
#       # Calculating the lag correlation: max lag of 21 days aka 3 weeks
#       lag.corr <- ccf(var1, var2, plot = FALSE, lag.max = -21)
#       
#       # Print 
#       cat("Cross-correlation between", trial.signals[[k]], "and", trial.signals[[l]], ":\n")
#       
#       # ACF (auto-correlation function estimation).
#       acf.values <- lag.corr$acf
#       print(acf.values) 
#       cat("\n")
#       
#       max.abs.value <- max(abs(acf.values))
#       max.value <- max(acf.values)
#       min.value <- min(acf.values)
#       
#       state_output[[index]] <- max.abs.value
#       output.matrix[k, l] <- ifelse(max.abs.value == max.value, max.value, min.value)
#       output.matrix[l, k] <- ifelse(max.abs.value == max.value, max.value, min.value)
#       
#       index <- index + 1
#       
#       # Plotting the lag correlation
#       # plot(lag.corr, main = paste("Cross-correlation:", trial.signals[k], trial.signals[l]))
#     }
#   }
#   #assigns 1 diagnally because signals are the same
#   diag(output.matrix) <- 1
#   state.val.list[[i]] <- state_signals
#   
#   # output.list[[i]] <- as.matrix(state_output)[,1:index]  # Assign sublist to output.list & Convert to matrix
#   output.list[[i]] <- state_output
#   output.matrix_list[[i]] <- output.matrix
# }
# 
# # print
# output.matrix_list











#Jul 17:
#### Graphs that did/didnt work for lag corr ####


# plot(graph,
#      edge.label = ifelse(E(sign.graph)$weight > 0,E(graph)$weight, -1* E(graph)$weight),
#      layout = layout.fruchterman.reingold,#layout_with_sugiyama, #layout_with_kk #layout = "fr"
#      vertex.color = "orange", #node color
#      edge.color = "gray", #the lines
#      font = 2, #makes it bold
#      vertex.shape = "circle",
#      vertex.size = deg*2, #size of the node aka vertices
#      #vertex.size = c(10, 40),
#      edge.arrow.size = 0.8,
#      edge.width = 2, 
#      vertex.label.cex = 0.8,#increases the font size
#      edge.label.color = "black",
#      edge.label.cex = 0.95,
#       main = graph_title)#,
#      # rescale = FALSE,
#      # ylim= c(1,4),
#      # xlim=c(-17,24),
#      # asp=0
#      
# 


# Plot the graph
#ignores the edge weights if there are non-positive weights, which includes negative weights. 
# plot(graph,
#      edge.label = E(graph)$weight,
#      #layout = layout_with_kk,
#      vertex.color = "orange", 
#      edge.color = "gray", 
#      font = 2,
#      vertex.shape = "circle",
#      vertex.size = 8,
#      edge.arrow.size = 0.5,
#      edge.width = 2,
#      vertex.label.cex = 1,
#      edge.label.color = "black",
#      edge.label.cex = 0.8)



#install.packages("visNetwork")
library(visNetwork)

matrix_data <- as.matrix(filtered_output_matrix[[2]])
graph <- graph_from_adjacency_matrix(matrix_data, mode = "directed", weighted = TRUE)

adj_matrix <- as.data.frame(as.table(get.adjacency(graph, sparse = FALSE)))
names(adj_matrix) <- c("from", "to", "weight")
visGraph <- visNetwork(nodes = data.frame(id = colnames(matrix_data)),
                       edges = adj_matrix,
                       directed = TRUE)%>%
  visEdges(arrows = 'to') 
visGraph$x$edges$arrows <- list(to = list(enabled = TRUE, scaleFactor = 0.0001))
visGraph <- visGraph %>%
  visIgraphLayout() %>%
  visNodes(color = "orange") %>%
  visEdges(color = "gray", width = 2) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
visGraph


#install.packages("qgraph")
library(qgraph)

matrix_data <- as.matrix(filtered_output_matrix[[2]])
vertex_labels <- colnames(matrix_data)
graph <- qgraph(matrix_data, layout = "spring", edge.labels = matrix_data, labels = vertex_labels)    # Create the graph using the adjacency matrix and vertex labels
plot(graph,
     layout = "spring",
     edge.color = "gray",
     edge.width = 1,
     edge.label.cex = 0.8,
     edge.arrow.size = 0.5,
     edge.arrow.width = 0.5,
     edge.arrow.length = 0.1,
     edge.lty = "solid",
     edge.label.color = "black",
     vertex.color = "orange",
     vertex.size = 10,
     vertex.label.font = 2,  
     vertex.label.cex = 1)   




#install.packages("ggraph")
library(ggraph)

matrix_data <- as.data.frame(filtered_output_matrix[[2]])
graph <- graph_from_data_frame(matrix_data, directed = TRUE)
layout <- create_layout(graph, layout = "fr") #Fruchterman-Reingold is a force-directed graph layout algorithm that positions the nodes of a graph based on attractive and repulsive forces between them.
ggraph(layout) +
  geom_edge_link(aes(width = abs(weight), color = weight > 0), arrow = arrow(length = unit(0.15, "inches"))) +
  geom_node_point(color = "orange", size = 10) +
  geom_node_text(aes(label = name), vjust = 1, size = 6) +
  theme_graph()


library(igraph)

# Create the graph from the adjacency matrix using igraph
matrix_data <- as.matrix(filtered_output_matrix[[2]])

graph <- graph_from_adjacency_matrix(matrix_data, mode = "directed", weighted = TRUE)

# Set the vertex labels as the signal names
V(graph)$label <- colnames(matrix_data)

# Choose the Sugiyama layout algorithm
layout <- layout_with_sugiyama(graph)

# Plot the graph with the chosen layout
plot(graph, layout = layout, edge.label = E(graph)$weight)









#trial.signals <- unique(norm.full.ds$signal[norm.full.ds$category_type == "mask_use"]) # 4 signals
trial.signals<- sampled_list
a <- choose(length(trial.signals), 2) # helper variable to see how long the output.list is gonna be
#states <- list("ca", "ny", "tx")#Specify the number of states
states <- list("ca","ny","tx","fl","nv","pa","ma") #"dc", "hi"
num_signals <- length(trial.signals)
num_states <- length(states)
state.val.list <- vector("list", length = num_states)

output.list <- vector("list", length = num_states)
output.matrix_list <- vector("list", length = num_states)

for (i in 1:num_states) {
  state <- states[i]
  state_signals <- list()
  state_output <- list()  # Initialize sublist for output
  
  for (j in 1:num_signals) {
    one.sig <- trial.signals[j]
    state_data <- norm.full.ds %>%
      filter(geo_value == state, signal == one.sig) %>%
      select(norm.value) ## ONLY NORM VALUE
    
    state_signals[[j]] <- state_data  # 4 signal values
  }
  
  index <- 1
  output.matrix <- matrix(NA, nrow = num_signals, ncol = num_signals, dimnames = list(paste0(states[i], "_", trial.signals),
                                                                                      paste0(states[i], "_", trial.signals)))
  #before it was dimnames = list(trial.signals, trial.signals) <- only had the signal names
  
  
  #Jul 18
  #### Signals that aren't going to use ####
  
  # 
  # sigs.final <- c("smoothed_wcovid_vaccinated"     ,"smoothed_wworried_vaccine_side_effects"          
  #                 ,"smoothed_whesitancy_reason_cost"               ,"smoothed_whesitancy_reason_distrust_gov"         
  #                 ,"smoothed_whesitancy_reason_ineffective"        ,"smoothed_whesitancy_reason_low_priority"         
  #                 ,"smoothed_whesitancy_reason_other"              ,"smoothed_whesitancy_reason_religious"            
  #                 ,"smoothed_whesitancy_reason_sideeffects"        ,"smoothed_whesitancy_reason_unnecessary"          
  #                 , "smoothed_whesitancy_reason_wait_safety"       ,"smoothed_wdontneed_reason_dont_spend_time"       
  #                 , "smoothed_wdontneed_reason_had_covid"          ,"smoothed_wdontneed_reason_not_beneficial"        
  #                 , "smoothed_wdontneed_reason_not_high_risk"      ,"smoothed_wdontneed_reason_not_serious"           
  #                 , "smoothed_wdontneed_reason_other"              ,"smoothed_wdontneed_reason_precautions"           
  #                 , "smoothed_waccept_covid_vaccine_no_appointment","smoothed_wappointment_not_vaccinated"            
  #                 , "smoothed_wcovid_vaccinated_friends"                                 
  #                 , "smoothed_wworried_catch_covid"                ,"smoothed_wbelief_created_small_group"            
  #                 , "smoothed_wbelief_distancing_effective"        ,"smoothed_wbelief_govt_exploitation"              
  #                 , "smoothed_wdelayed_care_cost"                  ,"smoothed_wrace_treated_fairly_healthcare"        
  #                 ,"smoothed_wtrust_covid_info_cdc"                  
  #                 , "smoothed_wtrust_covid_info_doctors"           ,"smoothed_wtrust_covid_info_experts"              
  #                 , "smoothed_wtrust_covid_info_friends"           ,"smoothed_wtrust_covid_info_govt_health"          
  #                 , "smoothed_wtrust_covid_info_journalists"       ,"smoothed_wtrust_covid_info_politicians"          
  #                 , "smoothed_wtrust_covid_info_religious"         , "smoothed_whh_cmnty_cli"                                             
  #                 ,"smoothed_wtested_positive_14d"                 ,"smoothed_wpublic_transit_1d"                     
  #                 , "smoothed_wwearing_mask_7d"                    ,"smoothed_wlarge_event_indoors_1d"                
  #                 , "smoothed_wspent_time_indoors_1d"              ,"smoothed_wwork_outside_home_indoors_1d"          
  #                 , "smoothed_wothers_masked_public"               ,"deaths_incidence_num"                            
  #                 , "confirmed_admissions_covid_1d_7dav" )
  
  
  
  # # ,"smoothed_wwant_info_children_education"          
  # , "smoothed_wwant_info_covid_treatment"          ,"smoothed_wwant_info_covid_variants"              
  # , "smoothed_wwant_info_employment"               ,"smoothed_wwant_info_mental_health"               
  # , "smoothed_wwant_info_none"                     ,"smoothed_wwant_info_relationships"               
  # , "smoothed_wwant_info_vaccine_access"           ,"smoothed_wwant_info_vaccine_types"
  # 
  
  #,"smoothed_wworried_finances"                      
  #, "smoothed_wanxious_7d"                         ,"smoothed_wdepressed_7d"    
  
  
  
  
  
  
  
  
  # # Get the indices of vertices with zero degree
  # vertices_to_remove <- which(deg == 0)
  # sign_vertices_to_remove <- which(sign_deg == 0)
  # 
  # # Remove vertices with zero degree from the graph
  # graph <- delete.vertices(graph, vertices_to_remove)
  # sign.graph <- delete.vertices(sign.graph, sign_vertices_to_remove) 
  
  
  #complete.wide.ds <- df[apply(df[, -1], 1, function(row) !any(sapply(row, function(x) is.null(x) || is.na(x)))), ]
  
  
  
  
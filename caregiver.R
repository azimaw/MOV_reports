# June 19, 2024
# Caregiver Questionnaire 
# Analytics

library(readxl)
library(tidyverse)
library(lubridate)
library(rlang)
library(knitr)
library(forcats)


raw_data <- read_xlsx("data/MOV_Caretaker_Dummy.xlsx")

# Function for Select All Questions

select_all_variables <- c("reason_child_visit", "transport_facility", "source_vax_messages", "vax_card_purpose",
"side_effects_info_provided", "why_satisfied_service", "why_not_satisfied_service", "purpose_vax", "vax_suggestions")

select_all <- function(raw_data, select_all_variables) {
  
  results <- lapply(select_all_variables, function(variable) {
    
    # Ensure the variable exists in the data
    if (!variable %in% names(raw_data)) {
      stop(paste("Variable", variable, "not found in the data"))
    }
    
    # Step 1: Separate the multiple answer options into individual rows
    split_data <- raw_data %>%
      separate_rows(!!sym(variable), sep = " ")
    
    # Step 2: Count the occurrences of each option
    option_counts <- split_data %>%
      count(!!sym(variable))
    
    # Step 3: Calculate the proportion of each option over the total number of unique IDs
    total_ids <- n_distinct(split_data$participant_id)
    option_proportions <- option_counts %>%
      mutate(Proportion = n / total_ids)
    
    # Return the results as a list
    list(
      Variable = variable,
      Counts = option_counts,
      Proportions = option_proportions
    )
  })
  
  assign("select_all_count_data", results, envir = .GlobalEnv)
}

# Single Answer Questions
# Function that creates a list of all single answer questions with counts, and proportions

calculate_counts_proportions <- function() {
  
  file_path <- "data/MOV_Caretaker_Dummy.xlsx"
  # removed doc_not_official because this is on the new updated survey
  variables_of_interest <- c("dob_known", "age_units", "child_gender", "caregiver_gender", "caregiver_relationship", "caregiver_literacy", "caregiver_education", "caregiver_employment", "time_to_facility_units", "does_it_cost_transport_hf", "amount_money_transport_hf_toomuch", "seen_messages_vax", "know_what_vaccines", "know_when_vaccines", "child_ever_vaxed", "why_child_not_vaxed", "refused_vax", "why_refused_vax", "vax_decision_maker", "child_has_vax_card", "why_child_vax_card_nothere", "why_no_vax_card", "lost_vax_card", "difficulty_replace_card", "today_staff_ask_vax_card", "today_staff_ask_vax_status", "today_child_vaxed", "blockA_healthworkers", "blocA_healthworkers_sick", "blockB_caregivers", "blockC_hf", "vax_referal", "vax_wait_time_units", "vax_wait_shade", "tell_which_vax_given", "tell_date_next_vax", "write_date_next_vax", "side_effects_info", "info_what_to_do_side_effects", "satisfied_service", "ever_pay_vax", "type_hf_pay_vax", "ever_pay_health_card", "type_hf_pay_health_card", "could_get_disease", "bcg_given", "oral_polio_d0_given", "oral_polio_d1_given", "oral_polio_d2_given", "oral_polio_d3_given", "ipv_given", "penta_d1_given", "penta_d2_given", "penta_d3_given", "rota_d1_given", "rota_d2_given", "rota_d3_given", "pcv_d1_given", "pcv_d2_given", "pcv_d3_given", "mcv_d1_given", "mcv_d2_given", "yf_d1_given", "source_vax_dates", "recording_area_available", "rec_area_child_background", "rec_area_vax_history", "rec_area_vitA", "rec_area_growth_mon", "rec_area_vision", "rec_area_delivery", "thank_end")
  
  # Read the Excel file
  survey_data <- read_excel(file_path)
  
  # Calculate counts and proportions for each variable of interest
  count_data <- lapply(variables_of_interest, function(variable) {
    # Convert variable name to symbol
    variable_sym <- sym(variable)
    
    # Calculate count and proportion
    count_var <- survey_data %>%
      count(!!variable_sym) %>%
      mutate(Proportion = n / sum(n))
    
    # Return as a list
    return(list(
      Variable = variable,
      Count = count_var,
      Proportion = count_var %>% select(!!variable_sym, Proportion)
    ))
  })
  
  assign("global_count_data", count_data, envir = .GlobalEnv)
}

global_count_data[[1]]$Count

# Chosen Questions to Report

# Where / How did you hear or see the message? (how info is being received)
# select all source_vax_messages

source_vax_msgs_renamed_data <- select_all_count_data[[3]]$Counts %>%
  mutate(source_vax_messages = recode(source_vax_messages,
                                      "radio" = "Radio",
                                      "tv" = "Television",
                                      "newspaper" = "Newspaper",
                                      "hf" = "Health facility",
                                      "telephone" = "Telephone message",
                                      "fb" = "Facebook or internet",
                                      "school" = "Child's school",
                                      "religious_place" = "Place of worship",
                                      "home_visit" = "During home visit",
                                      "meetings" = "Community meetings",
                                      "other" = "Other")) %>% 
  filter(complete.cases(.)) %>% 
  mutate(`Proportion (%)` =  n/sum(n) * 100) %>% 
  mutate(`Proportion (%)` = round(`Proportion (%)`, 1)) %>% 
  rename(`Where / How did you hear or see the message?` = source_vax_messages)


plot_source_vax_msgs <- ggplot(data = source_vax_msgs_renamed_data) +
  geom_col(aes(x = `Where / How did you hear or see the message?`, y = n)) +
  labs(x = "Source of Vaccination Messages", y = "Count")

kable(source_vax_msgs_renamed_data)

# Create the pie chart
pie_source_vax_msgs <- ggplot(source_vax_msgs_renamed_data, aes(x = "", y = n, fill = `Where / How did you hear or see the message?`)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = paste0(n, " (", scales::percent(n/sum(n)), ")")),
            position = position_stack(vjust = 0.5)) +  # Adds labels in the middle of each segment
  labs(fill = "", title = "Where / How did you hear or see the message?")


# Do you feel that you know the vaccines your child needs? (knowledge)
# select one know_what_vaccines

know_what_vaccines_renamed <- global_count_data[[13]]$Count %>% 
  mutate(know_what_vaccines = recode(know_what_vaccines,
                                     "no" = "No",
                                     "notsure" = "Not sure",
                                     "yes" = "Yes")) %>% 
  filter(complete.cases(.)) %>% 
  select(-Proportion) %>% 
  mutate(`Proportion (%)` = n/sum(n) * 100) %>%
  mutate(`Proportion (%)` = round(`Proportion (%)`, 1)) %>% 
  rename(`Do you feel that you know the vaccines your child needs?` = know_what_vaccines)

# Reorder x variables
know_what_vaccines_renamed$`Do you feel that you know the vaccines your child needs?` <- factor(know_what_vaccines_renamed$`Do you feel that you know the vaccines your child needs?`, 
                                                        levels = c("Yes", "No", "Not sure"))


plot_know_what_vaccines <- ggplot(data = know_what_vaccines_renamed) +
  geom_col(aes(x = `Do you feel that you know the vaccines your child needs?`, y = n)) +
  labs(x = "", y = "Count")

pie_know_what_vaccines <- ggplot(know_what_vaccines_renamed, aes(x = "", y = n, fill = `Do you feel that you know the vaccines your child needs?`)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = paste0(n, " (", scales::percent(n/sum(n)), ")")),
            position = position_stack(vjust = 0.5)) +  # Adds labels in the middle of each segment
  labs(fill = "", title = "Do you feel that you know what vaccines your child needs?")

kable(know_what_vaccines_renamed)

# Has this child ever been vaccinated? (proportion zero-dose)
# select one child_ever_vaxed

child_ever_vaxed <- global_count_data[[15]]$Count %>%
  mutate(child_ever_vaxed  = recode(child_ever_vaxed ,
                                     "no" = "No",
                                     "notsure" = "Not sure",
                                     "yes" = "Yes")) %>% 
  filter(complete.cases(.)) %>% 
  select(-Proportion) %>% 
  mutate(`Proportion (%)` = n/sum(n) * 100) %>% 
  mutate(`Proportion (%)` = round(`Proportion (%)`, 1)) %>% 
  rename(`Has this child ever been vaccinated?` = child_ever_vaxed)

# Reorder x variables
child_ever_vaxed$`Has this child ever been vaccinated?` <- factor(child_ever_vaxed$`Has this child ever been vaccinated?`, 
                                                        levels = c("Yes", "No", "Not sure"))

plot_child_ever_vaxed <- ggplot(data = child_ever_vaxed) +
  geom_col(aes(x = `Has this child ever been vaccinated?`, y = n)) +
  labs(x = "", y = "Count")

pie_child_ever_vaxed <- ggplot(child_ever_vaxed, aes(x = "", y = n, fill = `Has this child ever been vaccinated?`)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = paste0(n, " (", scales::percent(n/sum(n)), ")")),
            position = position_stack(vjust = 0.5)) +  # Adds labels in the middle of each segment
  labs(fill = "", title = "Has this child ever been vaccinated?")


kable(child_ever_vaxed)

# Why didn’t they vaccinate the child? (supply-side reasons for non-vax)
# select all why_refused_vax

why_refused_vax <- global_count_data[[18]]$Count %>%
  mutate(why_refused_vax  = recode(why_refused_vax,
                                    "sick" = "Child was sick",
                                    "stockout" = "Vaccine stockout",
                                    "not_vax_day" = "Not a vaccination day",
                                   "vax_area_closed" = "Vaccination area closed",
                                   "person_not_there" = "Person in charge of vaccination not there",
                                   "no_card"= "No under-five booklet",
                                   "hours_limited" = "Vaccination hours limited",
                                   "too_old" = "Child was too old",
                                   "other" = "Other")) %>% 
  filter(complete.cases(.)) %>% 
  select(-Proportion) %>% 
  mutate(`Proportion (%)` = n/sum(n) * 100) %>%
  mutate(`Proportion (%)` = round(`Proportion (%)`, 1)) %>% 
  rename(`Why didn't they vaccinate the child?` = why_refused_vax)

plot_why_refused_vax <- ggplot(data = why_refused_vax) +
  geom_col(aes(x = `Why didn't they vaccinate the child?`, y = n)) +
  labs(x = "", y = "Count")

pie_why_refused_vax <- ggplot(why_refused_vax, aes(x = "", y = n, fill = `Why didn't they vaccinate the child?`)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = paste0(n, " (", scales::percent(n/sum(n)), ")")),
            position = position_stack(vjust = 0.5)) +  # Adds labels in the middle of each segment
  labs(fill = "", title = "Why didn’t they vaccinate the child?")


kable(why_refused_vax)


save(plot_source_vax_msgs, source_vax_msgs_renamed_data, pie_source_vax_msgs,
     plot_know_what_vaccines, know_what_vaccines_renamed, pie_know_what_vaccines,
     plot_child_ever_vaxed, pie_child_ever_vaxed, child_ever_vaxed,
     plot_why_refused_vax, pie_why_refused_vax, why_refused_vax,
     file = "output/caregiver_questions_report.Rdata")



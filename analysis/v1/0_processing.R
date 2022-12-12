library(tidyverse)
library(ggplot2)
library(here)
library(jsonlite)
library(janitor)
source("helper_functions.R")

#paths
exp_path <- here("experiment_data")
survey_path <- here("survey_data")
processed_path <- here("processed_data")

#read in experimental data
exp_data <- read_and_combine_data(exp_path,column_types = cols(.default = "c"))
#read in survey data
survey_data <- read_and_combine_data(survey_path,column_types = cols(.default = "c"))


stim_info <- read.csv(here(processed_path,"CoAct_stimuli_items_with_info.csv")) %>%
  mutate(aoa_adj=case_when(
    is.na(aoa) ~ max(aoa,na.rm=TRUE),
    TRUE ~ aoa
  ))
stim_info_renamed <- stim_info
colnames(stim_info_renamed) <- paste0(colnames(stim_info),"_grid")

#### EXPERIMENT DATA ####

exp_data <- exp_data %>%
  filter(trial_type %in% c("coact-test","coact-grid","coact-grid-choice","coact-grid-choice-audio")) %>%
  mutate(trial_index=as.numeric(as.character(trial_index))) %>%
  filter(!(subject_id=="p002"&trial_index>198)) %>%
  mutate(correct_helper = ifelse(cur_target_image == chosen_image,1,0)) %>%
  relocate(correct_helper,.after=correct) %>%
  mutate(correct = ifelse(!is.na(correct_helper),correct_helper,correct)) %>%
  mutate(correct=as.numeric(correct)) %>%
  mutate(
    target_word = case_when(
      is.na(target) ~ cur_target,
      TRUE ~ target
    )
  ) %>%
  relocate(target_word,.after=target) %>%
  left_join(stim_info,by=c("target_word"="word")) %>%
  mutate(
    chosen_word = str_remove(
      str_remove(
        str_remove(
          str_remove(chosen_audio,"stims/audio/"),"thats_"),"its_"),".wav")
  ) %>%
  left_join(stim_info_renamed,by=c("chosen_word"="word_grid"))

write_csv(exp_data,here(processed_path,"coact_pilot_v1_processed.csv"))

#### SURVEY DATA ####
survey_data <- survey_data %>%
  mutate(participant_id = case_when(
    time_elapsed == "222023" ~ "p001",
    time_elapsed == "222171" ~ "p002",
    TRUE ~ subject
  )) %>%
  select(participant_id, trial_type,rt,question_order,response,words) %>%
  filter(trial_type == "survey-likert") %>%
  mutate(response = map(response, ~ fromJSON(.) %>% as.data.frame())) %>%
  mutate(words = map(words, ~ fromJSON(.) %>% 
                       as.data.frame(row.names=paste0("Q",seq(0,39))) %>% 
                       rownames_to_column(var = "question")))

temp <- survey_data %>%
  select(participant_id,response) %>%
  unnest(response) %>%
  pivot_longer(cols=Q0:Q39,names_to = "question",values_to="rating")

survey_data <- survey_data %>%
  unnest(words) %>%
  clean_names() %>%
  rename(word = x) %>%
  left_join(temp) %>%
  left_join(stim_info) %>%
  #set aoa to max(aoa) for items we don't have
  mutate(
    aoa_nas_filled = case_when(
      is.na(aoa) ~ max(aoa,na.rm=TRUE),
      TRUE ~ aoa
    )
  )

write_csv(survey_data,here(processed_path,"coact_pilot_v1_survey_processed.csv"))







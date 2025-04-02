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
init_data <- exp_data %>%
  filter(trial_type=="survey-text") %>%
  select(subject_id,seed,condition_index,condition_order,condition_vector,condition_name,fam_stims,round_stims,fam_test_index)
#read in survey data
survey_data <- read_and_combine_data(survey_path,column_types = cols(.default = "c"))
#read in participant spreadsheet
participant_info <- read.csv(here(processed_path,"CoAct Participants Spreadsheet - participants.csv")) %>%
  rename(
    condition_part=condition,
    notes_part=notes,
    seed_part=seed
  )
#read in demographic info
demographics <- read.csv(here(processed_path,"General Demographics_October 28, 2023_12.19_processed.csv")) %>%
  mutate(subject_id=tolower(subject_id))


stim_info <- read.csv(here(processed_path,"CoAct_stimuli_items_with_info.csv")) %>%
  mutate(aoa_adj=case_when(
    #non-trivial decision - match missing aoas to max aoa
    is.na(aoa) ~ max(aoa,na.rm=TRUE),
    TRUE ~ aoa
  ))
stim_info_renamed <- stim_info
colnames(stim_info_renamed) <- paste0(colnames(stim_info),"_grid")

#### EXPERIMENT DATA ####

## preprocessing data
exp_data <- exp_data %>%
  filter(trial_type %in% c("coact-test","coact-grid","coact-grid-choice","coact-grid-choice-audio")) %>%
  mutate(trial_index=as.numeric(as.character(trial_index))) %>%
  left_join(stim_info,by=c("target"="word")) %>%
  mutate(
    chosen_word = str_remove(
      str_remove(
        str_remove(
          str_remove(chosen_audio,"stims/audio/"),"thats_"),"its_"),".wav")
  ) %>%
  left_join(stim_info_renamed,by=c("chosen_word"="word_grid")) %>%
  select(-c(seed,condition_index,condition_order,condition_vector,condition_name,fam_stims,round_stims,fam_test_index))

exp_data <- exp_data %>%
  left_join(init_data)

write_csv(exp_data,here(processed_path,"coact_v1_processed.csv"))

#### SURVEY DATA ####

## preprocessing data
survey_data <- survey_data %>%
  rename(participant_id=subject) %>%
  select(participant_id, trial_type,rt,question_order,response,words) %>%
  filter(trial_type == "survey-likert") %>%
  mutate(response = map(response, ~ fromJSON(.) %>% as.data.frame())) %>%
  mutate(words = map(words, ~ fromJSON(.) %>% 
                       as.data.frame(row.names=paste0("Q",seq(0,39))) %>% 
                       rownames_to_column(var = "question")))

survey_exclude_list <- c("test","Shashi",NA,"Test","Test2","Test-er","Ellatest","0","555","toyanc","Trestring","Trestr","Trest","Testr")

survey_data <- survey_data %>%
  filter(!(participant_id %in% survey_exclude_list))

#rename participant ids with errors
survey_data <- survey_data %>%
  mutate(
    participant_id = case_when(
      participant_id=="p068" & rt == 271735 ~ "p070",
      participant_id=="p053" & rt == 174169 ~ "p052",
      participant_id=="p051" & rt == 193985 ~ "p053",
      TRUE ~ participant_id
    )
  )

temp <- survey_data %>%
  select(participant_id,rt,response) %>%
  unnest_wider(response) %>%
  mutate(across(Q0:Q39,~sapply(.x,toString))) %>%
  mutate(across(Q0:Q39,~sapply(.x,as.numeric))) %>%
  pivot_longer(cols=Q0:Q39,names_to = "question",values_to="rating") 

survey_data_final <- survey_data %>%
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

survey_data_p101 <- survey_data_final %>%
  filter(participant_id=="p101") %>%
  group_by(participant_id,word) %>%
  summarize(
    N=n(),
    mean_rating=mean(rating,na.rm=TRUE),
    min_rating=min(rating,na.rm=TRUE),
    disagreement=ifelse(mean_rating!=min_rating,"yes","no")
  )

#removing the second, shorter survey for p101 for now for simplicity
survey_data_final <- survey_data_final %>%
  filter(!(participant_id=="p101"&rt==89891))

write_csv(survey_data_final,here(processed_path,"coact_v1_survey_processed.csv"))

#### demographic data ####

demographics_processed <- demographics %>%
  filter(!(subject_id %in% "test")) %>%
  filter(gender != "") %>%
  filter(!(subject_id == "p067" & study=="coAct")) %>% # remove response that was later revised
  mutate(
    subject_id = case_when(
      subject_id == "p0189" ~ "p019",
      subject_id == "p021" & age == "3" ~ "p020",
      TRUE ~ subject_id
    )
  ) %>%
  mutate(
    gender = case_when(
      gender %in% c("Male","M","Male ","Boy") ~ "m",
      gender %in% c("Female","Girl","Female ","F") ~ "f"
    )
  )

write_csv(demographics_processed,here(processed_path,"coact_v1_demographics_processed.csv"))



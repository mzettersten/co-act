library(tidyverse)
library(ggplot2)
library(here)
library(cowplot)
library(jsonlite)

#paths
data_path <- here("analysis","v1-pilot","processed_data")
figure_path <- here("analysis","v1-pilot","figures")
#read in data
exp_data <- read.csv(here(data_path,"coact_pilot_v1_processed.csv"))
survey_data <- read.csv(here(data_path, "coact_pilot_v1_survey_processed.csv"))
stim_info <- read.csv(here(data_path,"CoAct_stimuli_items_with_info.csv")) %>%
  mutate(aoa_adj=case_when(
    is.na(aoa) ~ max(aoa,na.rm=TRUE),
    TRUE ~ aoa
  ))

#### SURVEY DATA ####

#plot relationship between AOA and parent rating
ggplot(survey_data,aes(aoa,rating,color=participant_id))+
  geom_jitter(width=0.05)+
  geom_smooth()
#with AOA set to max value
ggplot(survey_data,aes(aoa_nas_filled,rating,color=participant_id))+
  geom_jitter(width=0.05)+
  geom_smooth()
ggsave(here(figure_path,"coact_v1_survey_aoa.pdf"))
#look at each participant more specifically - are we creating a gradient?
#looks pretty good, oldest kid has the "weakest" gradient (so maybe need to make it harder?)
ggplot(survey_data,aes(aoa,rating))+
  geom_jitter(width=0.05)+
  geom_smooth()+
  facet_wrap(~participant_id)
ggplot(survey_data,aes(aoa_nas_filled,rating))+
  geom_jitter(width=0.05)+
  geom_smooth()+
  facet_wrap(~participant_id)
#with the "test-based" AOA
ggplot(survey_data,aes(AoAtestbased,rating,color=participant_id))+
  geom_jitter(width=0.05)+
  geom_smooth()

#break into categories
#(our intuitive categories) - looks pretty nice!
ggplot(survey_data,aes(familiar_classification,rating))+
  geom_boxplot()+
  geom_jitter(width=0.05,alpha=0.3)
ggsave(here(figure_path,"coact_v1_survey_fam_class.pdf"))

#by animal and classification
survey_data %>%
  mutate(ordered_words = fct_reorder(word, aoa)) %>%
  ggplot(aes(ordered_words,rating))+
  geom_boxplot()+
  facet_wrap(~familiar_classification,scale="free_x")+
  theme(axis.text.x=element_text(angle = 90))
ggsave(here(figure_path,"coact_v1_survey_item_rating.pdf"))

#### EXPERIMENT DATA ####

#summarize test data

##overall
test_exp <- exp_data %>%
  filter(trial_type=="coact-test") %>%
  group_by(subject_id,round_type) %>%
  summarize(
    accuracy=mean(correct,na.rm=TRUE)
  ) %>%
  ungroup()
test_exp$round_type <- fct_relevel(test_exp$round_type,"pretest","posttest")


overall_exp <- test_exp %>%
  group_by(round_type) %>%
  summarize(
    N=n(),
    mean_accuracy=mean(accuracy,na.rm=T),
    sd=sd(accuracy,na.rm=T),
    ci=qt(0.975, N-1)*sd(accuracy,na.rm=T)/sqrt(N),
  ) %>%
  mutate(round_type=as.factor(round_type))
overall_exp$round_type <- fct_relevel(overall_exp$round_type,"pretest","posttest")

overall_exp %>%
  filter(!(round_type %in% c("practice_pretest","practice_posttest"))) %>%
  ggplot(aes(round_type,mean_accuracy))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mean_accuracy-ci,ymax=mean_accuracy+ci),width=.05)+
  geom_hline(yintercept=1/8,linetype="dashed")
ggsave(here(figure_path,"coact_v1_pilot_change_pre_post.pdf"))

## plot aoa
exp_data %>%
  filter(round_type %in% c("pretest","posttest")) %>%
  ggplot(aes(aoa,correct, color=round_type))+
  geom_jitter(width=0.05,height=0.05)+
  geom_smooth()+
  geom_hline(yintercept=1/8,linetype="dashed")+
  theme_cowplot()
ggsave(here(figure_path,"coact_v1_pilot_change_pre_post_aoa.pdf"))

## plot aoa by subject
test_exp %>%
  filter(round_type %in% c("pretest","posttest")) %>%
  ggplot(aes(round_type,accuracy,group=subject_id,color=subject_id))+
  geom_point()+
  geom_line()+
  theme_cowplot()
ggsave(here(figure_path,"coact_v1_pilot_change_pre_post_subject.pdf"))

## plot pre-post-test by item
exp_data %>%
  filter(round_type %in% c("pretest","posttest")) %>%
  ggplot(aes(target_word,correct))+
  geom_boxplot()+
  theme_cowplot()+
  facet_wrap(~round_type)+
  theme(axis.text.x=element_text(angle = 90))

test_word <- exp_data %>%
  filter(round_type %in% c("pretest","posttest")) %>%
  group_by(target_word,round_type) %>%
  summarize(
    N=n(),
    accuracy=mean(correct)
  ) 


#summarize sample data

##overall
sample_exp <- exp_data %>%
  filter(trial_type=="coact-grid-choice") %>%
  #filter(condition!="passive") %>%
  select(subject_id, set,chosen_items_in_order,chosen_word,aoa_grid,aoa_adj_grid,familiar_classification_grid) %>%
  rowwise() %>%
  mutate(
    left_choice = fromJSON(chosen_items_in_order)[1],
    right_choice = fromJSON(chosen_items_in_order)[2]
  ) %>%
  left_join(
    select(stim_info,word,aoa_adj), by = c("left_choice"="word")
  ) %>%
  rename(aoa_left=aoa_adj) %>%
  left_join(
    select(stim_info,word,aoa_adj), by = c("right_choice"="word")
  ) %>%
  rename(aoa_right=aoa_adj) %>%
  left_join(
    select(stim_info,word,familiar_classification), by = c("right_choice"="word")
  ) %>%
  rename(familiar_classification_right=familiar_classification) %>%
  left_join(
    select(stim_info,word,familiar_classification), by = c("left_choice"="word")
  ) %>%
  rename(familiar_classification_left=familiar_classification) %>%
  mutate(aoa_left_difference = aoa_left-aoa_right) %>%
  mutate(
    left_item_chosen = ifelse(chosen_word==left_choice,1,0)
  ) %>%
  filter(set!="familiar") %>%
  mutate(caregiver_combos=pmap_chr(list(familiar_classification_left,familiar_classification_right), ~paste(sort(c(...)), collapse = " + ")))

ggplot(sample_exp,aes(aoa_left_difference,left_item_chosen)) +
  geom_smooth()+
  geom_smooth(aes(color=subject_id),method="lm",se=FALSE)

m <- glm(left_item_chosen~aoa_left_difference,data=sample_exp)
summary(m)

ggplot(sample_exp,aes(aoa_adj_grid)) +
  geom_histogram()

ggplot(sample_exp,aes(familiar_classification_grid)) +
  geom_bar()

caregiver_combo_order <- c(
  "familiar + familiar",
  "familiar + somewhat_familiar",
  "familiar + somewhat_unfamiliar",
  "familiar + unfamiliar",
  "somewhat_familiar + somewhat_familiar",
  "somewhat_familiar + somewhat_unfamiliar",
  "somewhat_familiar + unfamiliar",
  "somewhat_unfamiliar + somewhat_unfamiliar",
  "somewhat_unfamiliar + unfamiliar",
  "unfamiliar + unfamiliar"
)

ggplot(sample_exp,aes(caregiver_combos))+
  geom_bar()+
  scale_x_discrete(limits=caregiver_combo_order)+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5))

#summarize scaffolding data
scaffolding_exp <- exp_data %>%
  filter(trial_type=="coact-grid") %>%
  select(subject_id, condition,set,chosen_items_in_order) %>%
  rowwise() %>%
  mutate(
    left_choice = fromJSON(chosen_items_in_order)[1],
    right_choice = fromJSON(chosen_items_in_order)[2]
  )%>%
  mutate(
    left_choice = fromJSON(chosen_items_in_order)[1],
    right_choice = fromJSON(chosen_items_in_order)[2]
  ) %>%
  left_join(
    select(stim_info,word,aoa_adj), by = c("left_choice"="word")
  ) %>%
  rename(aoa_left=aoa_adj) %>%
  left_join(
    select(stim_info,word,aoa_adj), by = c("right_choice"="word")
  ) %>%
  rename(aoa_right=aoa_adj) %>%
  left_join(
    select(stim_info,word,familiar_classification), by = c("right_choice"="word")
  ) %>%
  rename(familiar_classification_right=familiar_classification) %>%
  left_join(
    select(stim_info,word,familiar_classification), by = c("left_choice"="word")
  ) %>%
  rename(familiar_classification_left=familiar_classification) %>%
  mutate(aoa_left_difference = aoa_left-aoa_right) %>%
  filter(set!="familiar") %>%
  pivot_longer(cols=c(aoa_right,aoa_left),names_to="item",names_prefix="aoa_",values_to="aoa") %>%
  #fix condition labeling
  mutate(condition = case_when(
    subject_id == "p003" & set == 1 ~ "active",
    subject_id == "p003" & set ==3 ~ "passive",
    subject_id == "p002" & set %in% c(1,4) ~ "active",
    subject_id == "p002" & set %in% c(2,3) ~ "passive",
    TRUE ~ condition
  ))

ggplot(scaffolding_exp,aes(aoa,color=condition)) +
  geom_density()

ggplot(scaffolding_exp,aes(aoa)) +
  geom_histogram()+
  facet_wrap(~condition)

average_scaffolding <- scaffolding_exp %>%
  group_by(subject_id,condition) %>%
  summarize(
    mean_aoa=mean(aoa)
  )

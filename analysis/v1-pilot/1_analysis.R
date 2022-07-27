library(tidyverse)
library(ggplot2)
library(here)
library(cowplot)

#paths
data_path <- here("analysis","v1-pilot","processed_data")
figure_path <- here("analysis","v1-pilot","figures")
#read in data
exp_data <- read.csv(here(data_path,"coact_pilot_v1_processed.csv"))
survey_data <- read.csv(here(data_path, "coact_pilot_v1_survey_processed.csv"))

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


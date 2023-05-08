library(tidyverse)
library(ggplot2)
library(here)
library(cowplot)
library(jsonlite)
library(lme4)
library(gghalves)
library(lmerTest)

#paths
data_path <- here("processed_data")
figure_path <- here("figures")
#read in data
exp_data <- read.csv(here(data_path,"coact_v1_processed.csv"))
survey_data <- read.csv(here(data_path, "coact_v1_survey_processed.csv"))
stim_info <- read.csv(here(data_path,"CoAct_stimuli_items_with_info.csv")) %>%
  mutate(aoa_adj=case_when(
    is.na(aoa) ~ max(aoa,na.rm=TRUE),
    TRUE ~ aoa
  ))
participant_info <- read.csv(here(data_path,"CoAct Participants Spreadsheet - participants.csv")) %>%
  rename(
    condition_part=condition,
    notes_part=notes,
    seed_part=seed
  )
demographics <- read.csv(here(data_path,"coact_v1_demographics_processed.csv"))

# combine
exp_data <- exp_data %>%
  left_join(participant_info) %>%
  left_join(demographics)

#extract condition
exp_data <- exp_data %>%
  rowwise() %>%
  mutate(condition_round_1 = fromJSON(condition_name)[1],
         condition_round_2 = fromJSON(condition_name)[2],
         condition_round_3 = fromJSON(condition_name)[3],
         condition_round_4 = fromJSON(condition_name)[4]) %>%
  mutate(
    current_condition_name=case_when(
      round_index==0 ~ condition_round_1,
      round_index==1 ~ condition_round_2,
      round_index==2 ~ condition_round_3,
      round_index==3 ~ condition_round_4
    )
  ) %>% 
  ungroup()

# handle exclusions
exp_data_pre_exclusion <- exp_data
exp_data_final <- exp_data %>%
  filter(participant_level_exclusion=="n") %>%
  mutate(
    exclude_round = case_when(
      round_name == 1 ~ exclude_round_1,
      round_name == 2 ~ exclude_round_2,
      round_name == 3 ~ exclude_round_3,
      round_name == 4 ~ exclude_round_4
    )
  ) %>%
  filter(
    exclude_round=="n"
  )
  

#### SURVEY DATA ####

#plot relationship between AOA and parent rating
ggplot(survey_data,aes(aoa,rating,color=participant_id))+
  #geom_jitter(width=0.05)+
  geom_smooth(se=F)+
  theme(legend.position="none")
#with AOA set to max value
ggplot(survey_data,aes(aoa_nas_filled,rating,color=participant_id))+
  #geom_jitter(width=0.05)+
  geom_smooth(se=F)+
  theme(legend.position="none")
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

test_data <- exp_data_final %>%
  filter(trial_type=="coact-test") %>%
  mutate(
    child_condition=case_when(
      current_condition_name %in% c("active_active","passive_active") ~ "active",
      TRUE ~ "passive"
    ),
    caregiver_condition=case_when(
      current_condition_name %in% c("active_active","active_passive") ~ "active",
      TRUE ~ "passive"
    )
  )

##overall
test_exp <- exp_data_final %>%
  filter(trial_type=="coact-test") %>%
  group_by(subject_id,age_group, age_mos,round_type) %>%
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

overall_exp_by_age <- test_exp %>%
  group_by(age_group,round_type) %>%
  summarize(
    N=n(),
    mean_accuracy=mean(accuracy,na.rm=T),
    sd=sd(accuracy,na.rm=T),
    ci=qt(0.975, N-1)*sd(accuracy,na.rm=T)/sqrt(N),
  ) %>%
  mutate(round_type=as.factor(round_type))
overall_exp_by_age$round_type <- fct_relevel(overall_exp_by_age$round_type,"pretest","posttest")


overall_exp %>%
  filter(!(round_type %in% c("practice_pretest","practice_posttest"))) %>%
  ggplot(aes(round_type,mean_accuracy))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mean_accuracy-ci,ymax=mean_accuracy+ci),width=.05)+
  geom_hline(yintercept=1/8,linetype="dashed")
ggsave(here(figure_path,"coact_v1_pilot_change_pre_post.pdf"))

## plot change pre-post across items
exp_data_final %>%
  filter(round_type %in% c("pretest","posttest")) %>%
  ggplot(aes(aoa,correct, color=round_type))+
  geom_jitter(width=0.05,height=0.05)+
  geom_smooth(method="lm")+
  geom_smooth(method="loess",se=FALSE)+
  geom_hline(yintercept=1/8,linetype="dashed")+
  theme_cowplot()
ggsave(here(figure_path,"coact_v1_pilot_change_pre_post_aoa.pdf"))

exp_data_final %>%
  filter(round_type %in% c("pretest","posttest")) %>%
  ggplot(aes(aoa,correct, color=round_type))+
  geom_jitter(width=0.05,height=0.05)+
  geom_smooth(method="lm")+
  geom_smooth(method="loess",se=FALSE)+
  geom_hline(yintercept=1/8,linetype="dashed")+
  theme_cowplot()+
  facet_wrap(~age_group)

## plot pre-post by subject
test_exp %>%
  filter(round_type %in% c("pretest","posttest")) %>%
  ggplot(aes(round_type,accuracy,group=subject_id,color=subject_id))+
  geom_point()+
  geom_line()+
  theme_cowplot()+
  theme(legend.position="none")+
  geom_errorbar(data=filter(overall_exp,!(round_type %in% c("practice_pretest","practice_posttest"))),aes(y=mean_accuracy,ymin=mean_accuracy-ci,ymax=mean_accuracy+ci,group=NA),color="black",width=0,size=1.5)+
  geom_line(data=filter(overall_exp,!(round_type %in% c("practice_pretest","practice_posttest"))),aes(y=mean_accuracy,group=NA),color="black",size=2)+
  geom_point(data=filter(overall_exp,!(round_type %in% c("practice_pretest","practice_posttest"))),aes(y=mean_accuracy,group=NA),color="black",size=5)
ggsave(here(figure_path,"coact_v1_pilot_change_pre_post_subject.pdf"))

## plot pre-post by subject
test_exp %>%
  filter(round_type %in% c("pretest","posttest")) %>%
  ggplot(aes(round_type,accuracy,group=subject_id,color=subject_id))+
  geom_point()+
  geom_line()+
  theme_cowplot()+
  theme(legend.position="none")+
  geom_errorbar(data=filter(overall_exp_by_age,!(round_type %in% c("practice_pretest","practice_posttest"))),aes(y=mean_accuracy,ymin=mean_accuracy-ci,ymax=mean_accuracy+ci,group=NA),color="black",width=0,size=1.5)+
  geom_line(data=filter(overall_exp_by_age,!(round_type %in% c("practice_pretest","practice_posttest"))),aes(y=mean_accuracy,group=NA),color="black",size=2)+
  geom_point(data=filter(overall_exp_by_age,!(round_type %in% c("practice_pretest","practice_posttest"))),aes(y=mean_accuracy,group=NA),color="black",size=5)+
  facet_wrap(~age_group)

## by condition

##overall
test_exp_condition <- exp_data_final %>%
  filter(trial_type=="coact-test") %>%
  group_by(subject_id,age_group,age_mos,round_type,current_condition_name) %>%
  summarize(
    accuracy=mean(correct,na.rm=TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    child_condition=case_when(
      current_condition_name %in% c("active_active","passive_active") ~ "active",
      TRUE ~ "passive"
    ),
    caregiver_condition=case_when(
      current_condition_name %in% c("active_active","active_passive") ~ "active",
      TRUE ~ "passive"
    )
  )
test_exp_condition$round_type <- fct_relevel(test_exp_condition$round_type,"pretest","posttest")


test_exp_diff_condition <- test_exp_condition %>%
  group_by(subject_id,age_group,age_mos,current_condition_name,child_condition,caregiver_condition) %>%
  summarize(
    acc_increase = accuracy[round_type=="posttest"]-accuracy[round_type=="pretest"]
  ) 

overall_exp <- test_exp_condition %>%
  group_by(round_type,current_condition_name) %>%
  summarize(
    N=n(),
    mean_accuracy=mean(accuracy,na.rm=T),
    sd=sd(accuracy,na.rm=T),
    ci=qt(0.975, N-1)*sd(accuracy,na.rm=T)/sqrt(N),
  ) %>%
  mutate(round_type=as.factor(round_type))
overall_exp$round_type <- fct_relevel(overall_exp$round_type,"pretest","posttest")

overall_diff_exp <- test_exp_diff_condition %>%
  group_by(current_condition_name,child_condition,caregiver_condition) %>%
  summarize(
    N=n(),
    mean_accuracy_increase=mean(acc_increase,na.rm=T),
    sd=sd(acc_increase,na.rm=T),
    ci=qt(0.975, N-1)*sd(acc_increase,na.rm=T)/sqrt(N),
  )

overall_exp_by_age <- test_exp_condition %>%
  group_by(age_group,round_type,current_condition_name) %>%
  summarize(
    N=n(),
    mean_accuracy=mean(accuracy,na.rm=T),
    sd=sd(accuracy,na.rm=T),
    ci=qt(0.975, N-1)*sd(accuracy,na.rm=T)/sqrt(N),
  ) %>%
  mutate(round_type=as.factor(round_type))
overall_exp_by_age$round_type <- fct_relevel(overall_exp_by_age$round_type,"pretest","posttest")


overall_exp %>%
  filter(!(round_type %in% c("practice_pretest","practice_posttest"))) %>%
  ggplot(aes(round_type,mean_accuracy))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mean_accuracy-ci,ymax=mean_accuracy+ci),width=.05)+
  geom_hline(yintercept=1/8,linetype="dashed")+
  facet_wrap(~current_condition_name)
ggsave(here(figure_path,"coact_v1_pilot_change_pre_post.pdf"))

test_exp_condition %>%
  filter(!(round_type %in% c("practice_pretest","practice_posttest"))) %>%
  ggplot(aes(round_type,accuracy))+
  geom_violin()+
  facet_wrap(~current_condition_name)

p1 <- test_exp_diff_condition %>%
  filter(caregiver_condition=="active") %>%
  ggplot(aes(child_condition,acc_increase,fill=child_condition,color=child_condition))+
  theme_cowplot()+
  #geom_violin(adjust=2)+
  #geom_boxplot()+
  # geom_half_violin(data = filter(test_exp_diff_condition, caregiver_condition=="active"&child_condition == "active"), side = "l",adjust=1.5) + 
  # geom_half_boxplot(data = filter(test_exp_diff_condition, caregiver_condition=="active"&child_condition == "active"),side = "l", width = 0.2, notch = TRUE)+
  # geom_half_violin(data = filter(test_exp_diff_condition, caregiver_condition=="active"&child_condition == "passive"),side = "r",adjust=1.5) + 
  # geom_half_boxplot(data = filter(test_exp_diff_condition, caregiver_condition=="active"&child_condition == "passive"),side = "r", width = 0.2, notch = TRUE)+
  geom_half_violin(data= filter(test_exp_diff_condition, caregiver_condition=="active"&child_condition == "active"),position = position_nudge(x = -.1, y = 0), width=1,adjust=1.5,trim = FALSE, alpha = .8,color=NA,side="l")+
  geom_half_violin(data=filter(test_exp_diff_condition, caregiver_condition=="active"&child_condition == "passive"),position = position_nudge(x = .1, y = 0), width=1,adjust=1.5,trim = FALSE, alpha = .8,color=NA,side="r")+
  geom_line(aes(group=subject_id),position = position_jitter(width = 0.08,height=0.05, seed = 123),alpha=0.2,color="black") +
  geom_point(position = position_jitter(width = 0.08,height=0.05, seed = 123))+
  geom_errorbar(data=filter(overall_diff_exp,caregiver_condition=="active"),aes(y=mean_accuracy_increase,ymin=mean_accuracy_increase-ci,ymax=mean_accuracy_increase+ci),width=0,size=1.5,color="black")+
  geom_point(data=filter(overall_diff_exp,caregiver_condition=="active"),aes(y=mean_accuracy_increase),size=6,color="black")+
  geom_line(data=filter(overall_diff_exp,caregiver_condition=="active"),aes(y=mean_accuracy_increase,group=1),size=2,color="black")+
  geom_hline(yintercept=0, linetype="dashed",color = "black", linewidth=.5)+
  scale_y_continuous(limits=c(-0.5,0.5),breaks=c(-0.5, -0.25,0,0.25,-0.5))+
  ylim(-0.5,0.5)+
  scale_color_brewer(palette="Set1")+
  scale_fill_brewer(palette="Set1")+
  facet_wrap(~caregiver_condition)+
  theme(axis.title.x = element_text(face="bold", size=20),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16),
        strip.text.x = element_text(size = 16,face="bold"))+
  theme(legend.position="none")+
  xlab("Child Sampling Condition")+
  ylab("Accuracy Increase\n(Posttest-Pretest)")
p1
p2 <- test_exp_diff_condition %>%
  filter(caregiver_condition=="passive") %>%
  ggplot(aes(child_condition,acc_increase,fill=child_condition,color=child_condition))+
  theme_cowplot()+
  geom_half_violin(data= filter(test_exp_diff_condition, caregiver_condition=="passive"&child_condition == "active"),position = position_nudge(x = -.1, y = 0), width=1,adjust=1.5,trim = FALSE, alpha = .8,color=NA,side="l")+
  geom_half_violin(data=filter(test_exp_diff_condition, caregiver_condition=="passive"&child_condition == "passive"),position = position_nudge(x = .1, y = 0), width=1,adjust=1.5,trim = FALSE, alpha = .8,color=NA,side="r")+
  geom_line(aes(group=subject_id),position = position_jitter(width = 0.08,height=0.05, seed = 123),alpha=0.2,color="black") +
  geom_point(position = position_jitter(width = 0.08,height=0.05, seed = 123))+
  geom_errorbar(data=filter(overall_diff_exp,caregiver_condition=="passive"),aes(y=mean_accuracy_increase,ymin=mean_accuracy_increase-ci,ymax=mean_accuracy_increase+ci),width=0,size=1.5,color="black")+
  geom_point(data=filter(overall_diff_exp,caregiver_condition=="passive"),aes(y=mean_accuracy_increase),size=6,color="black")+
  geom_line(data=filter(overall_diff_exp,caregiver_condition=="passive"),aes(y=mean_accuracy_increase,group=1),size=2,color="black")+
  geom_hline(yintercept=0, linetype="dashed",color = "black", linewidth=.5)+
  scale_y_continuous(limits=c(-0.5,0.5),breaks=c(-0.5, -0.25,0,0.25,-0.5))+
  ylim(-0.5,0.5)+
  scale_color_brewer(palette="Set1")+
  scale_fill_brewer(palette="Set1")+
  facet_wrap(~caregiver_condition)+
  theme(axis.title.x = element_text(face="bold", size=20),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16),
        strip.text.x = element_text(size = 16,face="bold"))+
  theme(legend.position="none")+
  xlab("Child Sampling Condition")+
  ylab("Accuracy Increase\n(Posttest-Pretest)")
p2
pos <- position_jitter(width = 0.08,height=0.05, seed = 123)
test_exp_diff_condition <- test_exp_diff_condition %>%
  mutate(caregiver_condition_ordered=factor(caregiver_condition,levels=c("active","passive"))) %>%
  arrange(caregiver_condition_ordered,subject_id,child_condition)
p <- ggplot(data=test_exp_diff_condition,aes(child_condition,acc_increase,fill=child_condition,color=child_condition))+
  geom_half_violin(data= filter(test_exp_diff_condition, child_condition == "active"),position = position_nudge(x = -.1, y = 0), width=1,adjust=1.5,trim = TRUE, alpha = .8,color=NA,side="l")+
  geom_half_violin(data=filter(test_exp_diff_condition, child_condition == "passive"),position = position_nudge(x = .1, y = 0), width=1,adjust=1.5,trim = TRUE, alpha = .8,color=NA,side="r")+
  geom_point(data=test_exp_diff_condition,aes(group=subject_id,color=child_condition),position = position_jitter(width = 0.08,height=0.05, seed = 123))+
  geom_line(data=test_exp_diff_condition,aes(group=subject_id),position = position_jitter(width = 0.08,height=0.05, seed = 123),alpha=0.2,color="black") +
  #lemon::geom_pointpath(data=test_exp_diff_condition,aes(group=subject_id,color=child_condition),position=position_jitter(width = 0.08,height=0.05, seed = 123),linecolor="black",alpha=0.2)+
  geom_errorbar(data=overall_diff_exp,aes(y=mean_accuracy_increase,ymin=mean_accuracy_increase-ci,ymax=mean_accuracy_increase+ci),width=0,size=1.5,color="black")+
  geom_point(data=overall_diff_exp,aes(y=mean_accuracy_increase),size=6,color="black")+
  geom_line(data=overall_diff_exp,aes(y=mean_accuracy_increase,group=1),size=2,color="black")+
  geom_hline(yintercept=0, linetype="dashed",color = "black", linewidth=.5)+
  scale_y_continuous(breaks=c(-0.5, -0.25,0,0.25,0.5))+
  #ylim(-0.5,0.5)+
  scale_color_brewer(palette="Set1")+
  scale_fill_brewer(palette="Set1")+
  facet_wrap(.~caregiver_condition)+
  theme_cowplot()+
  theme(axis.title.x = element_text(face="bold", size=28),
        axis.text.x  = element_text(size=24),
        axis.title.y = element_text(face="bold", size=28),
        axis.text.y  = element_text(size=24),
        strip.text.x = element_text(size = 24,face="bold"))+
  theme(legend.position="none")+
  xlab("Child Sampling Condition")+
  ylab("Accuracy Increase\n(Posttest-Pretest)")
p
ggsave(here(figure_path,"coact_v1_test.pdf"),width=12,height=8)


m <- lmer(acc_increase~current_condition_name+ (1|subject_id), data=test_exp_diff_condition)
summary(m)
Anova(m, type="III")

m <- lmer(acc_increase~current_condition_name*age_mos+ (1|subject_id), data=test_exp_diff_condition)
summary(m)
Anova(m, type="III")

test_exp_diff_condition <- test_exp_diff_condition %>%
  mutate(
    child_condition_c=ifelse(child_condition=="active",0.5,-0.5),
    caregiver_condition_c=ifelse(caregiver_condition=="active",0.5,-0.5),
  )

m <- lmer(acc_increase~child_condition_c*caregiver_condition_c+ (1|subject_id), data=test_exp_diff_condition)
summary(m)

m <- lmer(acc_increase~child_condition+ (1|subject_id), data=test_exp_diff_condition)
summary(m)

test_exp_diff_condition %>%
  ggplot(aes(current_condition_name,acc_increase))+
  geom_violin()+
  geom_boxplot()+
  geom_jitter(width=0.1)+
  facet_wrap(~age_group)



overall_exp_by_age %>%
  filter(round_type=="posttest") %>%
  ggplot(aes(current_condition_name,mean_accuracy))+
  geom_bar(stat="identity")+
  facet_wrap(~age_group)
  

## plot pre-post-test by item
exp_data_final %>%
  filter(round_type %in% c("pretest","posttest")) %>%
  ggplot(aes(target,correct))+
  geom_boxplot()+
  theme_cowplot()+
  facet_wrap(~round_type)+
  theme(axis.text.x=element_text(angle = 90))

test_word <- exp_data_final %>%
  filter(round_type %in% c("pretest","posttest")) %>%
  group_by(target,round_type) %>%
  summarize(
    N=n(),
    accuracy=mean(correct)
  ) 


#summarize sample data

##overall
sample_exp <- exp_data_final %>%
  filter(trial_type=="coact-grid-choice") %>%
  select(subject_id, age_mos,age_group,gender,condition,current_condition_name,set,chosen_items_in_order,chosen_word,aoa_grid,aoa_adj_grid,familiar_classification_grid) %>%
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
  mutate(aoa_left_difference = aoa_left-aoa_right,
         avg_aoa_choices = (aoa_left+aoa_right)/2,
         diff_choice=aoa_adj_grid-avg_aoa_choices) %>%
  mutate(
    left_item_chosen = ifelse(chosen_word==left_choice,1,0)
  ) %>%
  filter(set!="familiar") %>%
  mutate(caregiver_combos=pmap_chr(list(familiar_classification_left,familiar_classification_right), ~paste(sort(c(...)), collapse = " + "))) %>%
  mutate(chosen_classification=case_when(
    left_item_chosen==1 ~ familiar_classification_left,
    left_item_chosen==0 ~ familiar_classification_right
  ))

ggplot(sample_exp,aes(aoa_left_difference,left_item_chosen)) +
  geom_smooth(method="lm")+
  geom_smooth(method="loess")+
  geom_smooth(aes(color=subject_id),method="lm",se=FALSE)+
  theme(legend.position="none")+
  facet_wrap(~condition)

ggplot(sample_exp,aes(aoa_left_difference,left_item_chosen)) +
  geom_smooth(method="lm")+
  geom_smooth(method="loess")+
  #geom_smooth(aes(color=subject_id),method="lm",se=FALSE)+
  theme(legend.position="none")+
  facet_wrap(~condition)+
  facet_wrap(~age_group)

m <- glmer(left_item_chosen~aoa_left_difference*condition+(1|subject_id),data=sample_exp,family=binomial)
summary(m)

m <- glmer(left_item_chosen~aoa_left_difference+(1|subject_id),data=filter(sample_exp, condition=="active"),family=binomial)
summary(m)

m <- glmer(left_item_chosen~aoa_left_difference*age_mos+(1|subject_id),data=filter(sample_exp, condition=="active"),family=binomial)
summary(m)

average_sampling <- sample_exp %>%
  group_by(subject_id,age_group,age_mos,condition) %>%
  summarize(
    mean_aoa=mean(aoa_adj_grid),
    mean_diff=mean(diff_choice)
  )

ggplot(average_sampling,aes(condition,mean_aoa))+
  geom_violin()

ggplot(average_sampling,aes(condition,mean_aoa))+
  geom_violin()

ggplot(average_sampling,aes(condition,mean_aoa))+
  geom_violin()+
  facet_wrap(~age_group)

ggplot(sample_exp,aes(condition,diff_choice))+
  geom_violin()+
  geom_boxplot()+
  geom_jitter(width=0.01)

ggplot(sample_exp,aes(condition,diff_choice))+
  geom_violin()+
  geom_boxplot()+
  geom_jitter(width=0.01)+
  facet_wrap(~age_group)

ggplot(average_sampling,aes(age_mos,mean_diff))+
  geom_smooth(method="lm")+
  geom_point()+
  facet_wrap(~condition)

ggplot(filter(average_sampling,condition=="active"),aes(age_mos,mean_diff))+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_smooth(method="lm",color="black")+
  geom_point(color="#d7191c",size=3)+
  #facet_wrap(~condition)+
  xlab("Child Age (in months)")+
  ylab("Sampling Preference for higher AoA\n(Sampled AoA-Average Choice AoA)")+
  scale_color_brewer(palette="Set1")+
  theme_cowplot()+
  theme(axis.title.x = element_text(face="bold", size=28),
        axis.text.x  = element_text(size=24),
        axis.title.y = element_text(face="bold", size=28),
        axis.text.y  = element_text(size=24),
        strip.text.x = element_text(size = 24,face="bold"),
        legend.position="none")
ggsave(here(figure_path,"coact_v1_diff_aoa_across_age.pdf"),width=10,height=8)

m <- lmer(diff_choice ~ age_mos + (1|subject_id),data=filter(sample_exp,condition=="active"))
summary(m)

m <- lmer(diff_choice ~ age_mos + (1|subject_id)+(1|chosen_word),data=filter(sample_exp,condition=="active"))
summary(m)

m <- lmer(diff_choice ~ condition + (1|subject_id),data=sample_exp)
summary(m)

m <- lmer(diff_choice ~ condition*age_mos + (1|subject_id),data=sample_exp)
summary(m)

m <- lmer(mean_aoa ~ condition*age_mos+(1|subject_id),data=average_sampling)
summary(m)
#complex condition name
average_sampling_cond_name <- sample_exp %>%
  group_by(subject_id,current_condition_name) %>%
  summarize(
    mean_aoa=mean(aoa_adj_grid)
  ) %>%
  mutate(
    child_condition=case_when(
      current_condition_name %in% c("active_active","passive_active") ~ "active",
      TRUE ~ "passive"
    ),
    caregiver_condition=case_when(
      current_condition_name %in% c("active_active","active_passive") ~ "active",
      TRUE ~ "passive"
    )
  ) %>%
  mutate(
    child_condition_c = ifelse(child_condition=="active",0.5,-0.5),
    caregiver_condition_c = ifelse(caregiver_condition=="active",0.5,-0.5),
  ) 

overall_sampling_cond_name <- average_sampling_cond_name %>%
  group_by(current_condition_name,child_condition,caregiver_condition) %>%
  summarize(
    N=n(),
    aoa=mean(mean_aoa,na.rm=T),
    sd=sd(mean_aoa,na.rm=T),
    ci=qt(0.975, N-1)*sd(mean_aoa,na.rm=T)/sqrt(N),
  ) 

ggplot(average_sampling_cond_name,aes(child_condition,mean_aoa,color=child_condition))+
  geom_violin()+
  #geom_boxplot()+
  geom_jitter(aes(),width=0.1,size=3,alpha=0.3,stroke=NA)+
  facet_wrap(~caregiver_condition)+
  geom_errorbar(data=overall_sampling_cond_name,aes(y=aoa,ymin=aoa-ci,ymax=aoa+ci),size=2,width=0)+
  geom_point(data=overall_sampling_cond_name,aes(y=aoa),size=8)+
  ylab("Average Age of Acquisition\nof Sampled Item")+
  xlab("Child Sampling Condition")+
  scale_color_brewer(palette="Set1")+
  theme_cowplot()+
  theme(axis.title.x = element_text(face="bold", size=28),
        axis.text.x  = element_text(size=24),
        axis.title.y = element_text(face="bold", size=28),
        axis.text.y  = element_text(size=24),
        strip.text.x = element_text(size = 24,face="bold"),
        legend.position="none")

ggsave(here(figure_path,"coact_v1_sampling_conditions.pdf"),width=10,height=8)


m <- lmer(mean_aoa~condition+(1|subject_id),data=average_sampling)
summary(m)

m <- lmer(mean_aoa~current_condition_name+(1|subject_id),data=average_sampling_cond_name)
summary(m)
Anova(m,type="III")

m <- lmer(mean_aoa~child_condition_c*caregiver_condition_c+(1|subject_id),data=average_sampling_cond_name)
summary(m)

ggplot(sample_exp,aes(aoa_adj_grid)) +
  geom_histogram()+
  facet_wrap(~condition)

ggplot(sample_exp,aes(familiar_classification_grid)) +
  geom_bar()+
  facet_wrap(~condition)

#summarize chosen words
sampling_choices <- sample_exp %>%
  group_by(subject_id,age_group,age_mos,current_condition_name,chosen_word) %>%
  summarize(
    sampling_choice_count=n()
  ) %>%
  mutate(target=chosen_word)

ggplot(sampling_choices,aes(chosen_word))+
  geom_bar()+
  facet_wrap(~current_condition_name)+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5))

test_sampling_data <- test_data %>%
  filter(!(round_type %in% c("practice_pretest","practice_posttest"))) %>%
  select(-chosen_word) %>% 
  left_join(sampling_choices) %>%
  select(subject_id,age_group,age_mos,current_condition_name,child_condition,caregiver_condition,round_name,round_type,target,sampling_choice_count,correct,aoa,aoa_adj,familiar_classification) %>%
  mutate(
    sampling_choice_count=case_when(
      is.na(sampling_choice_count) ~ 0,
      TRUE ~ sampling_choice_count))

test_sampling_data_diff <- test_sampling_data %>%
  group_by(subject_id,age_group,age_mos,current_condition_name,child_condition,caregiver_condition,round_name,target,sampling_choice_count,aoa,aoa_adj,familiar_classification) %>%
  summarize(
    accuracy_increase=correct[round_type=="posttest"]-correct[round_type=="pretest"]
  )

m <- lmer(accuracy_increase~sampling_choice_count+(1|subject_id)+(1|target),data=test_sampling_data_diff)
summary(m)

ggplot(test_sampling_data_diff,aes(sampling_choice_count,accuracy_increase,color=current_condition_name))+
  #geom_jitter()+
  geom_smooth(method="lm")+
  scale_color_brewer(palette="Set1")+
  theme_cowplot()+
  facet_wrap(~child_condition)

test_sampling_data_diff <- test_sampling_data_diff %>%
  mutate(
    child_condition_c = ifelse(child_condition=="active",0.5,-0.5),
    caregiver_condition_c = ifelse(caregiver_condition=="active",0.5,-0.5),
  ) %>%
  group_by(subject_id) %>%
  mutate(
    sampling_choice_count_c = (sampling_choice_count-mean(sampling_choice_count))/sd(sampling_choice_count)
  )

m <- lmer(accuracy_increase~sampling_choice_count_c*child_condition_c*caregiver_condition_c+(1+sampling_choice_count_c*child_condition_c|subject_id)+(1+sampling_choice_count_c|target),data=test_sampling_data_diff,control=lmerControl(optimizer="bobyqa"))
summary(m)

m <- lmer(accuracy_increase~sampling_choice_count_c+(1+sampling_choice_count_c|subject_id)+(1+sampling_choice_count_c|target),data=filter(test_sampling_data_diff,current_condition_name=="active_active"),control=lmerControl(optimizer="bobyqa"))
summary(m)

coefs_active_active <- data.frame(
  current_condition_name="active_active",
  child_condition="active",
  caregiver_condition="active",
  N=summary(m)$ngrps[1],
  predictor=row.names(summary(m)$coefficients),
  beta=summary(m)$coefficients[,1],
  se=summary(m)$coefficients[,2],
  t=summary(m)$coefficients[,4],
  p=summary(m)$coefficients[,5]
)

m <- lmer(accuracy_increase~sampling_choice_count_c+(1+sampling_choice_count_c|subject_id)+(1+sampling_choice_count_c|target),data=filter(test_sampling_data_diff,current_condition_name=="active_passive"),control=lmerControl(optimizer="bobyqa"))
summary(m)

coefs_active_passive <- data.frame(
  current_condition_name="active_passive",
  child_condition="passive",
  caregiver_condition="active",
  N=summary(m)$ngrps[1],
  predictor=row.names(summary(m)$coefficients),
  beta=summary(m)$coefficients[,1],
  se=summary(m)$coefficients[,2],
  t=summary(m)$coefficients[,4],
  p=summary(m)$coefficients[,5]
)

m <- lmer(accuracy_increase~sampling_choice_count_c+(1+sampling_choice_count_c|subject_id)+(1+sampling_choice_count_c|target),data=filter(test_sampling_data_diff,current_condition_name=="passive_active"),control=lmerControl(optimizer="bobyqa"))
summary(m)

coefs_passive_active <- data.frame(
  current_condition_name="passive_active",
  child_condition="active",
  caregiver_condition="passive",
  N=summary(m)$ngrps[1],
  predictor=row.names(summary(m)$coefficients),
  beta=summary(m)$coefficients[,1],
  se=summary(m)$coefficients[,2],
  t=summary(m)$coefficients[,4],
  p=summary(m)$coefficients[,5]
)

m <- lmer(accuracy_increase~sampling_choice_count_c+(1+sampling_choice_count_c|subject_id)+(1+sampling_choice_count_c|target),data=filter(test_sampling_data_diff,current_condition_name=="passive_passive"),control=lmerControl(optimizer="bobyqa"))
summary(m)

coefs_passive_passive <- data.frame(
  current_condition_name="passive_passive",
  child_condition="passive",
  caregiver_condition="passive",
  N=summary(m)$ngrps[1],
  predictor=row.names(summary(m)$coefficients),
  beta=summary(m)$coefficients[,1],
  se=summary(m)$coefficients[,2],
  t=summary(m)$coefficients[,4],
  p=summary(m)$coefficients[,5]
)

#combine
coefs <- bind_rows(coefs_active_active,coefs_active_passive,coefs_passive_active,coefs_passive_passive) %>%
  mutate(ci = qt(0.975, N-1)*se,
         lower_ci = beta-ci,
         upper_ci=beta+ci)
ggplot(filter(coefs, predictor %in% c("sampling_choice_count_c")),aes(child_condition,beta, color=child_condition))+
  geom_point(size=8)+
  geom_errorbar(aes(ymin=lower_ci,ymax=upper_ci),width=0)+
  geom_hline(yintercept=0, linetype="dashed")+
  facet_wrap(~caregiver_condition)+
  #coord_flip()+
  # scale_x_discrete(
  #   breaks=c("acti"),
  #   labels=c("Frequency\nExposure Phase","Frequency\nSampling Phase")
  # )+
  ylab("Effect of Sampling Frequency\n(Model Beta Coefficient w/ 95% CIs)")+
  xlab("Child Sampling Condition")+
  scale_color_brewer(palette="Set1")+
  theme_cowplot()+
  theme(axis.title.x = element_text(face="bold", size=28),
        axis.text.x  = element_text(size=24),
        axis.title.y = element_text(face="bold", size=28),
        axis.text.y  = element_text(size=24),
        strip.text.x = element_text(size = 24,face="bold"),
        legend.position="none")#+
  #theme(axis.text.x  = element_text(angle=90, vjust=0.5))
ggsave(here(figure_path,"coact_v1_test_sampling.pdf"),width=10,height=8)

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

ggplot(sample_exp,aes(chosen_classification))+
  geom_bar()+
  #scale_x_discrete(limits=caregiver_combo_order)+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5))+
  facet_wrap(~current_condition_name)

#alluvial flow plot
choice_flow_summarized <- sample_exp %>%
  #filter(current_condition_name=="active_active") %>%
  group_by(current_condition_name,caregiver_combos,chosen_classification) %>%
  summarize(
    N=n()
  ) %>%
  ungroup() %>%
  group_by(current_condition_name) %>%
  mutate(
    total_n=sum(N)
  ) %>%
  mutate(
    percent=N/total_n
  ) %>%
  mutate(caregiver_combos_pretty=
           case_when(
             caregiver_combos=="familiar + familiar" ~ "fam + fam",
             caregiver_combos=="familiar + somewhat_familiar" ~ "fam + slightly fam",
             caregiver_combos=="familiar + somewhat_unfamiliar" ~ "fam + slightly unfam",
             caregiver_combos=="familiar + unfamiliar" ~ "fam + unfam",
             caregiver_combos=="somewhat_familiar + somewhat_familiar" ~ "slightly fam + slightly fam",
             caregiver_combos=="somewhat_familiar + somewhat_unfamiliar" ~ "slightly fam + slightly unfam",
             caregiver_combos=="somewhat_familiar + unfamiliar" ~ "slightly fam + unfam",
             caregiver_combos=="somewhat_unfamiliar + somewhat_unfamiliar" ~ "slightly unfam + slightly unfam",
             caregiver_combos=="somewhat_unfamiliar + unfamiliar" ~ "slightly unfam + unfam",
             caregiver_combos=="unfamiliar + unfamiliar" ~ "unfam + unfam"
           )) %>%
  mutate(
    chosen_classification_pretty=
      case_when(
        chosen_classification=="familiar" ~ "fam",
        chosen_classification=="somewhat_familiar" ~ "slightly fam",
        chosen_classification=="somewhat_unfamiliar" ~ "slightly unfam",
        chosen_classification=="unfamiliar" ~ "unfam",
      )
  )

# starting_point <- expand.grid(
#   current_condition_name="active_active",
#   init=c("familiar","somewhat_familiar","somewhat_unfamiliar","unfamiliar"),
#   caregiver_combos=unique(choice_flow_summarized$caregiver_combos),
#   chosen_classification=unique(choice_flow_summarized$chosen_classification)
# )


# choice_flow_summarized <- choice_flow_summarized %>%
#   ungroup() %>%
#   separate(caregiver_combos,into=c("caregiver_1","caregiver_2"),sep=" [+] ",remove=FALSE) %>%
#   pivot_longer(cols=c(caregiver_1,caregiver_2),names_to="caregiver_choice_num",values_to="init")

# temp <- choice_flow_summarized %>% 
#   left_join(starting_point) %>%
#   mutate(percent_updated=case_when(
#     init=="familiar"&caregiver_combos=="familiar + familiar" ~ percent,
#     (init %in% c("familiar","somewhat_familiar")) & caregiver_combos=="familiar + somewhat_familiar" ~ percent,
#     (init %in% c("familiar","somewhat_unfamiliar")) & caregiver_combos=="familiar + somewhat_unfamiliar" ~ percent,
#     (init %in% c("familiar","unfamiliar")) & caregiver_combos=="familiar + unfamiliar" ~ percent,
#     (init %in% c("somewhat_familiar")) & caregiver_combos=="somewhat_familiar + somewhat_familiar" ~ percent,
#     (init %in% c("somewhat_familiar","somewhat_unfamiliar")) & caregiver_combos=="somewhat_familiar + somewhat_unfamiliar" ~ percent,
#     (init %in% c("somewhat_familiar","unfamiliar")) & caregiver_combos=="somewhat_familiar + unfamiliar" ~ percent,
#     (init %in% c("somewhat_unfamiliar")) & caregiver_combos=="somewhat_unfamiliar + somewhat_unfamiliar" ~ percent,
#     (init %in% c("somewhat_unfamiliar","unfamiliar")) & caregiver_combos=="somewhat_unfamiliar + unfamiliar" ~ percent,
#     init=="unfamiliar"&caregiver_combos=="unfamiliar + unfamiliar" ~ percent,
#     TRUE ~ NA
#   ))

library(ggalluvial)
ggplot(filter(choice_flow_summarized,current_condition_name=="active_active"),
       aes(axis1=caregiver_combos_pretty,
         axis2=chosen_classification_pretty,
           y=percent))+
  #geom_alluvium(aes(fill=chosen_classification))+
  geom_flow(aes(fill=chosen_classification_pretty))+
  geom_stratum(aes(fill=caregiver_combos_pretty))+
  geom_stratum(aes(fill=chosen_classification_pretty))+
  geom_label(stat="stratum",
            aes(label=after_stat(stratum)),size=6)+
  scale_x_discrete(limits = c("Caregiver","Child"),
                   expand = c(0.15, 0.05))+
  scale_fill_viridis_d(direction=-1,alpha=1,option="inferno")+
  scale_color_viridis_d(direction=-1,alpha=1,option="inferno")+
  theme_void()+
  theme(legend.position="none")#+
  #facet_wrap(~current_condition_name)

ggsave(here(figure_path,"coact_v1_sampling_flow.pdf"),width=12,height=8)


# ggplot(choice_flow_summarized,
#        aes(axis1=init,
#            axis2=caregiver_combos,
#            axis3=chosen_classification,
#            y=percent))+
#   #geom_alluvium(aes(fill=chosen_classification))+
#   geom_flow(aes(fill=chosen_classification))+
#   geom_stratum(aes(fill=init))+
#   geom_stratum(aes(fill=caregiver_combos))+
#   geom_stratum(aes(fill=chosen_classification))+
#   geom_text(stat="stratum",
#              aes(label=after_stat(stratum)))+
#   scale_x_discrete(limits = c("Caregiver","Child"),
#                    expand = c(0.15, 0.05))+
#   scale_fill_viridis_d(direction=-1,alpha=1,option="plasma")+
#   scale_color_viridis_d(direction=-1,alpha=1,option="plasma")+
#   theme_void()+
#   theme(legend.position="none")+
#  facet_wrap(~current_condition_name)

#summarize scaffolding data
scaffolding_exp <- exp_data_final %>%
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
  pivot_longer(cols=c(aoa_right,aoa_left),names_to="item",names_prefix="aoa_",values_to="aoa") 

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

ggplot(average_scaffolding,aes(condition,mean_aoa))+
  geom_violin()

#sample characteristics
sample_age <- test_exp_diff_condition %>% 
  ungroup() %>%
  distinct(subject_id,age_mos)
mean(sample_age$age_mos)
min(sample_age$age_mos)
max(sample_age$age_mos)


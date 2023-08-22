
# Secondary Models:

# Rationale:
# Main findings show that there are two main types of individuals being followed, 
# Hypotheses as to what drives these following behaviours (leader versus help/allocare)

# 1: filter for only adults followed (response variable)
# 2: only juveniles/subadults followed (response variable), 
# 3: see who is following  (fixed factors would be the columns with the ages and sexes of the individual that is doing the following).

 
require(tidyverse) 
require(glmmTMB) 
require(ggpubr)
require(MASS)
require(ggplot2)
require(lme4)
require(bbmle)
require(DHARMa)
library(installr)
require(sjPlot)
library(GLMMadaptive)
#updateR()

rm(list=ls())

#set working directory
setwd("C:/Users/chars/OneDrive/Documents/CGS Project/Data")

#import csv file
df.secondary <- read.csv("secondary.models.csv")

#merge the behavioural columns to make analysis simpler (?)
df.secondary <- df.secondary %>%
  unite(behaviour.name, b1.name, b2.name, b3.name, sep = "; ")

#same for behaviour codes
df.secondary <- df.secondary %>%
  unite(behaviour.code, b1.code, b2.code, b3.code, sep = "; ")

# set fixed factors as factors:
df_summary$Sex.interactor<-as.factor(df_summary$Sex.interactor)
df_summary$Age.interactor<-as.integer(df_summary$Age.interactor)




######## LEADER 



#filter for leaders

leader_df <- subset(df_summary, total_followed >= 1)

#leader_df <- subset(df.secondary, grepl("followed", behaviour.name)) 
#187 individuals who are followed

M_leader <- glmmTMB(total_followed~Age.interactor+Sex.interactor+ offset(total_movement), 
                    data=leader_df,
                    family=poisson)

summary(M_leader)

# nothing sig











###############            JUVENILE AND SUBADULT LEADER               ##########################



nonadult_leader_df <- subset(leader_df, age != 1)

#how many subadults are followed?


M_nonadult_leader <- glmmTMB(total_followed~Age.interactor+Sex.interactor+ offset(total_followed), 
                    data=nonadult_leader_df,
                    family=poisson())

summary(M_nonadult_leader)

#nothing significant

Res <- simulateResiduals(M_adult_leader, n = 1000, plot = TRUE)# look for good qqplots
check_overdispersion(M_nonadult_leader)


? family_glmmTMB






######################            ADULT LEADERS                    #################################




adult_leader_df <- subset(leader_df, grepl("1", age)) #114 - over half of leader occurrences are adults??
#110 adults who are followed
# 110/287 leaders are adults




M_adult_leader <- glmmTMB(total_followed~ Age.interactor +Sex.interactor+offset(total_movement),
                      data=adult_leader_df,
                      family=poisson)

summary(M_adult_leader)

#nothing sig










# M2: FOLLOWER


# filter for followers
follower_df <- subset(df.secondary, grepl("follow ", behaviour.name)) 
#107 followers

M_follower <- glmmTMB(total_follower~Sex.interactor+Age.interactor+offset(total_movement)+
                            (1|Colony)+(1|Tag), 
                          data=follower_df,
                          family=poisson)
summary(M_follower)











##########                  NON ADULT FOLLOWER              ############################



nonadult_follower_df <- subset(follower_df, Age != 1)


M_follower_notadult <- glmmTMB(total_follower~Age.interactor+Sex.interactor+offset(total_movement)+
                        (1|Colony)+(1|Tag), 
                      data=nonadult_follower_df,
                      family=poisson)
summary(M_follower_notadult)

#nothing significant







                                                                                                                                

###############      ADULT FOLLOWER                #############################  


adult_follower<-subset(follower_df,grepl("1",Age))



M_follower_adult <- glmmTMB(total_follower~Age.interactor+Sex.interactor+ offset(total_movement)+
                                 (1|Colony)+(1|Tag), 
                               data=adult_follower,
                               family=poisson)
summary(M_follower_adult)
#nothing sig




##########       CREATING TABLE             #############

df <- read.csv("summary.group.composition.csv")


result <- df %>%
  group_by(sex, age) %>%
  summarize(
    count_followed = sum(total_followed > 0),
    count_follower = sum(total_follower > 0)
  )


# Reshape the data for plotting
result_long <- result %>%
  pivot_longer(cols = c("count_followed", "count_follower"),
               names_to = "count_type",
               values_to = "count")

# Mapping for age categories
age_mapping <- c("Adult", "Subadult", "Juvenile")

# Plotting
Plot1<-ggplot(result_long, aes(x = factor(age), y = count, fill = factor(sex))) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~count_type, scales = "free", ncol = 1,labeller = labeller(count_type = c("count_followed" = "Leader",
                                                                                       "count_follower" = "Follower"))) +
  labs(x = "Age", y = "Counts", fill = "Sex") +
  scale_fill_manual(
    name = "Sex",
    labels = c("Male", "Female"),
    values = c("blue", "pink")
  ) +
  Miya_theme +
  theme(legend.position = "bottom", strip.text = element_text(size = 14)) +
  scale_x_discrete(labels = age_mapping)

ggsave("Plot1.png",width=7, height=7)


library(ggplot2)
library(lme4)
library(hydroGOF)
library(tidyr)
library(dplyr)

setwd("~/git/negative-strengthening/experiments/1-subjectivity-negation/results")


df = read.csv("results.csv",header=T)

unique(df$language)

length(unique(df$participant_id)) # n=66

## remove non-English speakers
d = df[df$language!=""&
        df$language!="blue"&
        df$language!="tomato"
        ,]
unique(d$language)
length(unique(d$participant_id)) # n=60

## load helper file for bootstrapped CIs
source("../results/helpers.r")

agr = bootsSummary(data=d, measurevar="response", groupvars=c("predicate","valence","negated"))

mean(agr$N) ## 16.67

agr_mean = bootsSummary(data=d, measurevar="response", groupvars=c("valence","negated"))

## write to CSV file
#write.csv(agr,"adjective-subjectivity.csv")

summary(lmer(response~valence*negated+(1|participant_id),data=d))


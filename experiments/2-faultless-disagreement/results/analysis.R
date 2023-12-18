library(ggplot2)
library(lme4)
library(hydroGOF)
library(tidyr)
library(dplyr)

setwd("~/git/negative-strengthening/experiments/2-faultless-disagreement/results")


df = read.csv("results.csv",header=T)

unique(df$language)

length(unique(df$participant_id)) # n=40

## remove non-English speakers
d = df[df$language!=""&
        df$language!="GREG"
        ,]
unique(d$language)
length(unique(d$participant_id)) # n=38

## load helper file for bootstrapped CIs
source("../results/helpers.r")

agr = bootsSummary(data=d, measurevar="response", groupvars=c("adjective","polarity"))

mean(agr$N) ## 12.78

## write to CSV file
#write.csv(agr,"adjective-faultless.csv")

summary(lmer(response~polarity+(1|participant_id),data=d))


#### compare with subjectivity

sf = read.csv("~/git/negative-strengthening/experiments/1-subjectivity-negation/results/adjective-subjectivity.csv",header=T)

s = sf[sf$negated=="false",]

s$faultless = agr$response

gof(s$response,s$faultless)
#r         0.56
#R2        0.32



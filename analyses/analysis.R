


setwd("~/Google Drive/Fellowships/2021 ZAS/Nicole/")

source("helpers.R")

d = read.csv("en_subjectivity.csv",header=T)

d_s = bootsSummary(data=d, measurevar="response", groupvars=c("predicate"))

#write.csv(d_s,"adjective-subjectivity-average.csv")
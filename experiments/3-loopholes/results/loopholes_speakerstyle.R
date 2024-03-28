
###Script loophole speaker interaction style######################
####Nicole Gotzner 27.02.2024#############################


###loading data
data <- read.csv(file = "./results_loopholes_comp.csv", header = 0, sep = ",", comment.char = "#", encoding = "UTF-8-BOM", quote = "", 
                 row.names = NULL, 
                 stringsAsFactors = FALSE,
                     col.names = c("time","participant","controller", "order", "number", "label", "group", "elementtype", "elementname", "parameter", "value","eventtime","comments",
                                 "itemgroup","condition","RTfirst","RTlast","comments2"))

###preprocessing #####################################

str(data)
head(data)

table(data$itemgroup)

crit<-subset(data, data$label=="main_trials")
crit<-subset(crit, crit$elementtype=="Scale")

library(dplyr)

crit<- crit %>% 
  filter(!grepl('f', itemgroup))

table(crit$itemgroup)
 

###exclude participants who respond incorrectly in more than 50% of filler trials
crit<-subset(crit, crit$participant!="4caaf04896097633a8e07aee33be1be6")
crit<-subset(crit, crit$participant!="4d57204d9ea8a9a6a7b7cb9723245f36")
crit<-subset(crit, crit$participant!="cd26112915efa4be31acc629a438ac35")
crit<-subset(crit, crit$participant!="53749878dc475c7b43c9bfa7f1bc1f4b")


library(reshape)
split<-colsplit(crit$condition,split='_',names=c("speaker","polarity","complexity")) 
crit$polarity<-split$polarity
crit$speaker<-split$speaker
crit$complexity<-split$complexity

summary(crit)
str(crit)

#factors
crit$polarity<-factor(crit$polarity)
crit$speaker<-factor(crit$speaker)
crit$complexity<-factor(crit$complexity)


###Plots#######################################################

###means
crit$value<- as.numeric(crit$value)
round(tapply(crit$value,list(crit$polarity, crit$complexity),mean,na.rm=T),digits=2)
round(tapply(crit$value,list(crit$polarity, crit$speaker),mean,na.rm=T),digits=2)
round(tapply(crit$RTfirst,list(crit$polarity, crit$speaker),mean,na.rm=T),digits=2)
round(tapply(crit$RTlast,list(crit$polarity, crit$speaker),mean,na.rm=T),digits=2)

###plot

library(ggplot2)
library(tidyverse)

###histogram with CIs overall

data.to.plot <- crit %>%
  group_by(polarity, speaker, value) %>% #group
  summarise(num=n()) %>% 
  mutate(prop = num / sum(num), 
         upperE=1.96*sqrt((prop*(1-prop))/sum(num)),lowerE=-1.96*sqrt((prop*(1-prop))/sum(num)))

data.to.plot$speaker<-ifelse(data.to.plot$speaker=="standard", "committed", "loophole")

ggplot(data = data.to.plot , aes(x= value, y=prop, fill=polarity,  na.rm = TRUE)) + 
  geom_bar(stat="identity", na.rm = TRUE,position=position_dodge(0.9),alpha=.5,color='black') +
  geom_errorbar(aes(ymin = prop+lowerE, ymax = prop+upperE,width=0.15), position=position_dodge(0.9))  +
  #theme(text = element_text(size=14))+
  #theme(legend.position="bottom") +#, legend.direction="vertical")  +
  # guides(fill=guide_legend(nrow=4, byrow=TRUE)) +
  facet_wrap(~speaker)+
  theme_bw()+
  scale_fill_manual(values=c('blue','red'))+
  labs(title="", x="\nnegative strengthening", y = "proportion of responses\n")+
  scale_x_continuous(breaks=seq(1:7))+
  scale_y_continuous(breaks=c(0,0.25,0.5))
  #ylim(0,0.55)

#ggsave("loophole-results.pdf",width=6,height=2.5)

#######Cumulative link models###########################
library(ordinal)


###Factor coding and contrasts
crit$valuef<-factor(crit$value)
crit$participant<-factor(crit$participant)

crit$polarity <- relevel(crit$polarity,ref="pos")
crit$speaker <- relevel(crit$speaker,ref="nonstandard")
contrasts(crit$polarity) <- "contr.sum"
contrasts(crit$speaker) <- "contr.sum"

#######clmm analysis###############################
###nested contrasts
m1<-clmm(valuef~speaker/polarity+ + (1+speaker/polarity|itemgroup)+ (1+speaker/polarity|participant), data =crit)
summary(m1)
m2<-clmm(valuef~speaker/polarity+ + (1+polarity+speaker|itemgroup)+ (1+polarity+speaker|participant), data =crit)
summary(m2)
m3<-clmm(valuef~speaker/polarity+ + (1+polarity+speaker|itemgroup)+ (1+polarity|participant), data =crit)
summary(m3)

###sum contrasts
m1<-clmm(valuef~speaker*polarity+ (1+speaker*polarity|itemgroup) + (1+speaker*polarity|participant), data =crit)
summary(m1)

### simplified random effects structure due to degeneracy for item random slopes correlation
m1<-clmm(valuef~speaker*polarity+ (1+speaker+polarity|itemgroup) + (1+speaker+polarity|participant), data =crit)
summary(m1)
m2<-clmm(valuef~speaker*polarity+ + (1+polarity|itemgroup)+ (1+speaker*polarity|participant), data =crit)
summary(m2)
m3<-clmm(valuef~speaker*polarity+ + (1+polarity+speaker|itemgroup)+ (1+polarity+speaker|participant), data =crit)
summary(m3)



#######reaction times###############################
library(lme4)
library(lmerTest)

###nested effects
m3<-lmer(log(RTfirst)~speaker/polarity+ + (1+polarity|itemgroup)+ (1+polarity|participant), data =crit)
summary(m3)
m3<-lmer(log(RTlast)~speaker/polarity+ + (1+polarity|itemgroup)+ (1+polarity|participant), data =crit)
summary(m3)

m3<-lmer(log(RTfirst)~polarity/speaker+ + (1+polarity|itemgroup)+ (1+polarity|participant), data =crit)
summary(m3)
m3<-lmer(log(RTlast)~polarity/speaker+ + (1+polarity|itemgroup)+ (1+polarity|participant), data =crit)
summary(m3)

###sum contrasts
m1<-lmer(log(RTfirst)~speaker*polarity+ (1+speaker*polarity|itemgroup) + (1+speaker*polarity|participant), data =crit)
summary(m1)
m1<-lmer(log(RTlast)~speaker*polarity+ (1+speaker*polarity|itemgroup) + (1+speaker*polarity|participant), data =crit)
summary(m1)


##simplified 
m1<-lmer(log(RTfirst)~speaker*polarity+ (1+speaker*polarity|itemgroup) + (1+polarity|participant), data =crit)
summary(m1)
m1<-lmer(log(RTfirst)~speaker*polarity+ (1|itemgroup) + (1+polarity|participant), data =crit)
summary(m1)


m1<-lmer(log(RTlast)~speaker*polarity+ (1+speaker*polarity|itemgroup) + (1+polarity|participant), data =crit)
summary(m1)
m1<-lmer(log(RTlast)~speaker*polarity+ (1|itemgroup) + (1+polarity|participant), data =crit)
summary(m1)


#######same sample as Gotzner and Mazzarella?###############################

library(fBasics)
loophole<-subset(crit, crit$speaker=="nonstandard")
committed<-subset(crit,crit$speaker=="standard")

gm2<-read.delim("facethreat_standard-nonstandard_anonymous.txt",header=T, dec=",")
str(gm2)

ks2Test(gm2$value,loophole$value)
ks2Test(gm2$value,committed$value)

data.to.plot <- gm2 %>%
  group_by(polarity, value) %>% #group
  summarise(num=n()) %>% 
  mutate(prop = num / sum(num), 
         upperE=1.96*sqrt((prop*(1-prop))/sum(num)),lowerE=-1.96*sqrt((prop*(1-prop))/sum(num)))

ggplot(data = data.to.plot , aes(x= value, y=prop, fill=polarity,  na.rm = TRUE)) + 
  geom_bar(stat="identity", na.rm = TRUE, width = 1) +
  geom_errorbar(aes(ymin = prop, ymax = prop+upperE), width=0.05, linewidth=0.6, size=I(0.5))  +
  labs(y='', x="Rating") +
  # scale_fill_discrete(breaks=c("negated negative strong (not filthy)", "negative strong (filthy)", "negated negative weak (not dirty)", "negative weak (dirty)", "negated positive strong (not pristine)", "positive strong (pristine)", "negated positive weak (not clean)", "positive weak (clean)")) +
  theme(text = element_text(size=14))+
  theme(legend.position="bottom") +#, legend.direction="vertical")  +
  # guides(fill=guide_legend(nrow=4, byrow=TRUE)) +
  facet_wrap(~polarity)+
  theme(
    strip.background = element_blank(),
    strip.text.y = element_blank())+ 
  theme_classic() +
  theme(text = element_text(size=20))+
  scale_fill_manual(values=c('#999999','#E69F00'))+
  labs(title="", x="Negative strengthening", y = "Proportion of response")+
  scale_x_continuous(breaks=seq(1:7))




###gender analysis#######################################################

#####appending gender vector

gender<-subset(data, data$parameter=="Gender")
gender$gender<-gender$value
gender$value<-NULL
gender$participant<-as.character(gender$participant)
crit$participant<-as.character(crit$participant)
gendershort<-cbind(gender$participant,gender$gender)
colnames(gendershort)<-c("participant","gender")
gendershort<-as.data.frame(gendershort)
gendershort$participant

critg<-merge(crit, gendershort, by.x='participant', by.y='participant', all.x=TRUE)
head(critg)


###clmm
critg$valuef<-factor(critg$value)
critg$polarity <- relevel(critg$polarity,ref="pos")
critg$speaker <- relevel(critg$speaker,ref="nonstandard")
critg$gender<-factor(critg$gender)
critg$gender <- relevel(critg$gender,ref="Female")

contrasts(crit$polarity) <- "contr.sum"
contrasts(crit$speaker) <- "contr.sum"

m3<-clmm(valuef~gender/speaker/polarity+ + (1+polarity+speaker|itemgroup)+ (1+polarity|participant), data =critg)
summary(m3)
m3<-clmm(valuef~gender/speaker/polarity+ + (1|itemgroup)+ (1|participant), data =critg)
summary(m3)
m3<-clmm(valuef~gender*speaker*polarity+ + (1|itemgroup)+ (1|participant), data =critg)
summary(m3)



###plot
critg$value<- as.numeric(critg$value)
data.to.plot <- critg %>%
  group_by(polarity, speaker, gender,value) %>% #group
  summarise(num=n()) %>% 
  mutate(prop = num / sum(num), 
         upperE=1.96*sqrt((prop*(1-prop))/sum(num)),lowerE=-1.96*sqrt((prop*(1-prop))/sum(num)))

ggplot(data = data.to.plot , aes(x= value, y=prop, fill=polarity,  na.rm = TRUE)) + 
  geom_bar(stat="identity", na.rm = TRUE, width = 1) +
  geom_errorbar(aes(ymin = prop, ymax = prop+upperE), width=0.05, linewidth=0.6, size=I(0.5))  +
  labs(y='', x="Rating") +
  # scale_fill_discrete(breaks=c("negated negative strong (not filthy)", "negative strong (filthy)", "negated negative weak (not dirty)", "negative weak (dirty)", "negated positive strong (not pristine)", "positive strong (pristine)", "negated positive weak (not clean)", "positive weak (clean)")) +
  theme(text = element_text(size=14))+
  theme(legend.position="bottom") +#, legend.direction="vertical")  +
  # guides(fill=guide_legend(nrow=4, byrow=TRUE)) +
  facet_wrap(~gender+speaker+polarity)+
  theme(
    strip.background = element_blank(),
    strip.text.y = element_blank())+ 
  theme_classic() +
  theme(text = element_text(size=20))+
  scale_fill_manual(values=c('#999999','#E69F00'))+
  labs(title="", x="Negative strengthening", y = "Proportion of response")+
  scale_x_continuous(breaks=seq(1:7))


###anonymising#######################################################

#### Anonymising dataset #####

data$Response[data$Sentence == "prolific_id"] <- "ANONYMISED"

write.csv(data, file = "./ScalarDiv_results_n100_OSF.csv")


######item variation
###histogram with CIs - item variation

data.to.plot <- crit %>%
  group_by(polarity, speaker, itemgroup, value) %>% #group
  summarise(num=n()) %>% 
  mutate(prop = num / sum(num), 
         upperE=1.96*sqrt((prop*(1-prop))/sum(num)),lowerE=-1.96*sqrt((prop*(1-prop))/sum(num)))

ggplot(data = data.to.plot , aes(x= value, y=prop, fill=polarity, na.rm = TRUE)) + 
  geom_bar(stat="identity", na.rm = TRUE, width = 1) +
  geom_errorbar(aes(ymin = prop+lowerE, ymax = prop+upperE), width=0.05, size=I(0.5))  +
  labs(y='', x="Rating") +
  # scale_fill_discrete(breaks=c("negated negative strong (not filthy)", "negative strong (filthy)", "negated negative weak (not dirty)", "negative weak (dirty)", "negated positive strong (not pristine)", "positive strong (pristine)", "negated positive weak (not clean)", "positive weak (clean)")) +
  theme(text = element_text(size=14))+
  theme(legend.position="bottom") +#, legend.direction="vertical")  +
  # guides(fill=guide_legend(nrow=4, byrow=TRUE)) +
  facet_wrap(~itemgroup+speaker, nrow=11)+
  theme(
    strip.background = element_blank(),
    strip.text.y = element_blank())+ 
  theme_classic() +
  theme(text = element_text(size=10))+
  scale_fill_manual(values=c('#999999','#E69F00'))+
  labs(title="", x="Negative strengthening", y = "Proportion of response")+
  scale_x_continuous(breaks=seq(1:7))


###filler analysis#######################################################

#### excluding participants whose response is above 4 in 4 filler trials #####

crit<-subset(data, data$label=="main_trials")
crit<-subset(crit, crit$elementtype=="Scale")

filler<- crit %>% 
  filter(grepl('f', itemgroup))

table(filler$itemgroup)

table(filler$participant,filler$value)

4caaf04896097633a8e07aee33be1be6
4d57204d9ea8a9a6a7b7cb9723245f36
cd26112915efa4be31acc629a438ac35
53749878dc475c7b43c9bfa7f1bc1f4b

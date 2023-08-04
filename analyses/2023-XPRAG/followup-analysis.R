#setwd("~/git/negative-strengthening/analyses/2023-XPRAG")
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/dropbox copy/Emmy Noether project/Experiments/Subjectivity Greg/subjectivity ratings mg2021")

####################################
#### load new subjectivity data ####
####################################

df = read.delim("results_adj.txt",header=T)

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
source("helpers.r")

agr = bootsSummary(data=d, measurevar="response", groupvars=c("predicate","valence","negated"))

mean(agr$N) ## 16.67

agr_mean = bootsSummary(data=d, measurevar="response", groupvars=c("valence","negated"))
agr_mean


###means per adjective
adj_condition_mean = bootsSummary(data=d, measurevar="response", groupvars=c("adjective","valence","negated"))
adj_condition_mean

bare_subj<-subset(adj_condition_mean, adj_condition_mean$negated=="false")
neg_subj<-subset(adj_condition_mean, adj_condition_mean$negated=="true")


###extract difference scores
pos_bare_subj<-subset(bare_subj, bare_subj$valence=="positive")
neg_bare_subj<-subset(bare_subj, bare_subj$valence=="negative")
pos_neg_subj<-subset(neg_subj, neg_subj$valence=="positive")
neg_neg_subj<-subset(neg_subj, neg_subj$valence=="negative")


#################################################
#### data from mazzarella and gotzner (2021) ####
#################################################

crit<-read.delim("exp1_power_osf.txt",header=T, dec=",")
#crit2<-read.delim("facethreat_standard-nonstandard_anonymous.txt",header=T, dec=",")
#crit2$context<-NULL

#crit<-rbind(crit, crit2)
#crit$X<-NULL
str(crit)
head(crit)

crit$complexity<-factor(crit$complexity)
crit$polarity<-factor(crit$polarity)
crit$Gender<-factor(crit$Gender)
crit$politeness<-factor(crit$politeness)

e<-crit

head(e)
summary(e)
str(e)

e$adjective = NA

head(e[e$polarity=="Pos",])

e$item1 <- as.character(e$item1)
e$item2 <- as.character(e$item2)
e[e$polarity=="Pos",]$adjective <- e[e$polarity=="Pos",]$item1
e[e$polarity=="Neg",]$adjective <- e[e$polarity=="Neg",]$item2
e$notAdj_subj = agr$response[match(paste("not",tolower(e$adjective)),agr$predicate)]
e$highScale_subj = agr$response[match(tolower(e$item2),agr$predicate)]
e$diff<-e$notAdj_subj - e$highScale_subj

head(e)

library(ordinal)
e$valuef<-factor(e$value)
e$polarity<-factor(e$polarity)
e$complexity<-factor(e$complexity)
e$Gender<-factor(e$Gender)
contrasts(e$polarity) <- "contr.sum"
contrasts(e$complexity) <- "contr.sum"
e$Gender<-relevel(e$Gender,ref="Female")


###################
###### model ######
###################

head(e)
length(unique(e$item))

e$cnotadj<-e$notAdj_subj-mean(e$notAdj_subj)
m2<-clmm(valuef~polarity*cnotadj+ (1|item) + (polarity|Worker_ID) , data =e)
summary(m2)

## without "accurate"

e_new = e[e$item!="Accurate_Inaccurate",]

length(unique(e_new$item))

e_new$cnotadj<-e_new$notAdj_subj-mean(e_new$notAdj_subj)
m2_new<-clmm(valuef~polarity*cnotadj+ (1|item) + (polarity|Worker_ID) , data =e_new)
summary(m2_new)



## base model from paper plus subjectivity
m1<-clmm(valuef~polarity*politeness + (1|item) + (1|Worker_ID) , data =e)
summary(m1)



###################
###### plots ######
###################

library(ggplot2)
library(Rmisc)
ag <- summarySE(e, measurevar="value", groupvars=c("polarity","adjective","notAdj_subj"),na.rm=T)

ag$polarity<-factor(ag$polarity,levels=c("Pos","Neg"))
ggplot(ag, aes(y=value,x=notAdj_subj, colour=polarity, label=adjective)) +
  labs(y='negative strengthening\n', x="\nnegated adjective subjectivity",colour="adjective\npolarity") +
  scale_color_manual(labels = c("positive", "negative"),values=c("blue","red"))+
  #geom_text()+
  geom_smooth(method="lm")+
  geom_point(size=2)+ 
  theme_bw()
#ggsave("neg-strengthening-vs-neg-adj-subj-new-data.pdf",width=4.25,height=2.25)


ggplot(ag, aes(y=value,x=notAdj_subj, colour=polarity, label=adjective)) +
  labs(y='negative strengthening\n', x="\nnegated adjective subjectivity",colour="adjective\npolarity") +
  scale_color_manual(labels = c("positive", "negative"),values=c("blue","red"))+
  geom_text(position=position_jitter())+
  geom_smooth(method="lm",alpha=0.15)+
  xlim(.35,0.8)+
  #geom_point(size=2,alpha=0.25)+ 
  theme_bw()
#ggsave("neg-strengthening-vs-neg-adj-subj-new-data-labeled.pdf",width=6.25,height=3.25)

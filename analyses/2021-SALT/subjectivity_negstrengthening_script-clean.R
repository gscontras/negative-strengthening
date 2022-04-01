#setwd("~/git/negative-strengthening/analyses/2021-SALT")
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

crit<-read.delim("facethreat_anonymous.txt",header=T, dec=",")
crit2<-read.delim("facethreat_standard-nonstandard_anonymous.txt",header=T, dec=",")
crit2$context<-NULL

crit<-rbind(crit, crit2)
crit$X<-NULL
str(crit)
head(crit)

crit$complexity<-factor(crit$complexity)
crit$polarity<-factor(crit$polarity)
crit$Gender<-factor(crit$Gender)

e<-crit

head(e)
summary(e)
str(e)

e$notAdj_subj = agr$response[match(paste("not",e$adjective),agr$predicate)]
e$highScale_subj = agr$response[match(tolower(e$item2),agr$predicate)]
e$diff<-e$notAdj_subj - e$highScale_subj

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

e$cnotadj<-e$notAdj_subj-mean(e$notAdj_subj)
m2<-clmm(valuef~polarity*cnotadj+ (1|item) + (polarity|Worker_ID) , data =e)
summary(m2)


###################
###### plots ######
###################

library(ggplot2)
library(Rmisc)
ag <- summarySE(e, measurevar="value", groupvars=c("polarity","adjective","notAdj_subj"),na.rm=T)

ag$polarity<-factor(ag$polarity,levels=c("Pos","Neg"))
ggplot(ag, aes(y=value,x=notAdj_subj, colour=polarity)) +
  labs(y='negative strengthening\n', x="\nnegated adjective subjectivity",colour="adjective\npolarity") +
  scale_color_manual(labels = c("positive", "negative"),values=c("blue","red"))+
  geom_smooth(method="lm")+
  geom_point(size=2,alpha=0.75)+ 
  theme_bw()
#ggsave("neg-strengthening-vs-neg-adj-subj.pdf",width=4.25,height=2.25)


#######################################
###### bootstrapping correlation ######
#######################################

library(hydroGOF)
library(boot)

p = aggregate(value~polarity*adjective*notAdj_subj,data=e[e$polarity=="Pos",],FUN=mean)

gof(p$value,p$notAdj_subj)
# r = 0.37, r2 = 0.14
results <- boot(data=p, statistic=rsq, R=10000, formula=value~notAdj_subj)
boot.ci(results, type="bca") 
# 95%   ( 0.0011,  0.6020 )  

n = aggregate(value~polarity*adjective*notAdj_subj,data=e[e$polarity=="Neg",],FUN=mean)

gof(n$value,n$notAdj_subj)
# r = -0.23, r2 = 0.05
results <- boot(data=n, statistic=rsq, R=10000, formula=value~notAdj_subj)
boot.ci(results, type="bca") 
# 95%   ( 0.0000,  0.3189 ) 
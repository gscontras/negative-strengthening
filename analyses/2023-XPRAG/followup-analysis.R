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
#### data from four experiments              ####
#################################################

crit<-read.delim("exp1_power_osf.txt",header=T, dec=",")
crit2<-read.delim("exp2_socialdistance_osf.txt",header=T, dec=",")
crit3<-read.delim("facethreat_anonymous.txt",header=T, dec=",")
crit4<-read.delim("facethreat_standard-nonstandard_anonymous.txt",header=T, dec=",")
crit2$person<-NULL
crit$context<-NULL
crit2$context<-NULL
crit4$context<-NULL
crit3$adjective<-NULL
crit4$adjective<-NULL
crit3$politeness<-"NA"
crit4$politeness<-"NA"
crit$exp = "e1"
crit2$exp = "e2"
crit3$exp = "e3"
crit4$exp = "e4"

crit<-rbind(crit, crit2,crit3,crit4)
#crit$X<-NULL
str(crit)
head(crit)

crit$complexity<-factor(crit$complexity)
crit$polarity<-factor(crit$polarity)
crit$Gender<-factor(crit$Gender)
crit$politeness<-factor(crit$politeness)

#contrasts(crit$politeness) <- "contr.sum"
#contrasts(crit$polarity) <- "contr.sum"
#contrasts(crit$complexity) <- "contr.sum"
#contrasts(crit$Gender) <- "contr.sum"

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
e$polarity<-relevel(e$polarity,ref="Pos")
contrasts(e$polarity) <- "contr.sum"
contrasts(e$complexity) <- "contr.sum"
e$Gender<-relevel(e$Gender,ref="Female")
contrasts(e$Gender) <- "contr.sum"
e$politeness<-factor(e$politeness)
e$politeness<-relevel(e$politeness,ref="Low")
contrasts(e$politeness) <- "contr.sum"

###################
###### model ######
###################

head(e)
length(unique(e$item))

e$cnotadj<-e$notAdj_subj-mean(e$notAdj_subj)
m2<-clmm(valuef~polarity*cnotadj+ (1|item) + (1|Worker_ID) , data =e)
summary(m2)

#Number of groups:  Worker_ID 240,  item 20 
#
#Coefficients:
#                   Estimate Std. Error z value Pr(>|z|)    
# polarity1          0.55079    0.04532  12.152  < 2e-16 ***
# cnotadj           -0.88546    0.38803  -2.282   0.0225 *  
# polarity1:cnotadj  1.81195    0.28142   6.439 1.21e-10 ***

## without "accurate"

e_new = e[e$item!="Accurate_Inaccurate",]

length(unique(e_new$item))

e_new$cnotadj<-e_new$notAdj_subj-mean(e_new$notAdj_subj)
m2_new<-clmm(valuef~polarity*cnotadj+ (1|item) + (1|Worker_ID) , data =e_new)
summary(m2_new)

#Number of groups:  Worker_ID 240,  item 19 
#
#Coefficients:
#                   Estimate Std. Error z value Pr(>|z|)    
# polarity1          0.57378    0.02282  25.149  < 2e-16 ***
# cnotadj           -0.98457    0.36481  -2.699  0.00696 ** 
# polarity1:cnotadj  2.00041    0.31943   6.262 3.79e-10 ***


## exp1 base model from paper plus subjectivity
me1<-clmm(valuef~polarity*politeness*Gender+cnotadj + (1|item) + (1|Worker_ID) , 
          data =e[e$exp=="e1",])
summary(me1)
me1_subj<-clmm(valuef~polarity*politeness*Gender+polarity:cnotadj + (1|item) + (1|Worker_ID) , 
                 data =e[e$exp=="e1",])
summary(me1_subj)

anova(me1,me1_subj)

#no.par    AIC  logLik LR.stat df Pr(>Chisq)
#me1          16 5733.1 -2850.6                      
#me1_subj     17 5733.5 -2849.7    1.66  1     0.1976


## exp2 base model from paper plus subjectivity
me2<-clmm(valuef~polarity*politeness*Gender+cnotadj + (1|item) + (1|Worker_ID) , 
         data =e[e$exp=="e2",])
summary(me2)
me2_subj<-clmm(valuef~polarity*politeness*Gender+polarity:cnotadj + (1|item) + (1|Worker_ID) , 
          data =e[e$exp=="e2",])
summary(me1_subj)

anova(me2,me2_subj)

#no.par    AIC  logLik LR.stat df Pr(>Chisq)   
#me2          16 5253.8 -2610.9                         
#me2_subj     17 5245.9 -2605.9  9.8628  1   0.001687 **


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
#ggsave("neg-strengthening-vs-neg-adj-subj-all-data.pdf",width=4.25,height=2.25)


ggplot(ag, aes(y=value,x=notAdj_subj, colour=polarity, label=adjective)) +
  labs(y='negative strengthening\n', x="\nnegated adjective subjectivity",colour="adjective\npolarity") +
  scale_color_manual(labels = c("positive", "negative"),values=c("blue","red"))+
  geom_text(position=position_jitter())+
  geom_smooth(method="lm",alpha=0.15)+
  xlim(.35,0.8)+
  #geom_point(size=2,alpha=0.25)+ 
  theme_bw()
#ggsave("neg-strengthening-vs-neg-adj-subj-all-data-labeled.pdf",width=6.25,height=3.25)


ag_p <- summarySE(e, measurevar="value", groupvars=c("polarity","adjective","notAdj_subj","politeness"),na.rm=T)

ggplot(ag_p, aes(y=value,x=notAdj_subj, colour=polarity, label=adjective)) +
  labs(y='negative strengthening\n', x="\nnegated adjective subjectivity",colour="adjective\npolarity") +
  scale_color_manual(labels = c("positive", "negative"),values=c("blue","red"))+
  #geom_text()+
  geom_smooth(method="lm")+
  geom_point(size=2)+ 
  facet_grid(~politeness)+
  theme_bw()



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
# 95%   ( 0.0020,  0.5423 )  

n = aggregate(value~polarity*adjective*notAdj_subj,data=e[e$polarity=="Neg",],FUN=mean)

gof(n$value,n$notAdj_subj)
# r = -0.20, r2 = 0.04
results <- boot(data=n, statistic=rsq, R=10000, formula=value~notAdj_subj)
boot.ci(results, type="bca") 
# 95%   ( 0.0001,  0.3334 )  

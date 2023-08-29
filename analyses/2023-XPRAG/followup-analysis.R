setwd("~/git/negative-strengthening/analyses/2023-XPRAG")
#setwd("~/Library/Mobile Documents/com~apple~CloudDocs/dropbox copy/Emmy Noether project/Experiments/Subjectivity Greg/subjectivity ratings mg2021")

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
crit$exp = "e3"
crit2$exp = "e4"
crit3$exp = "e1"
crit4$exp = "e2"

crit<-rbind(crit, crit2,crit3,crit4)
crit<-crit[crit$Gender!="",]
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
e$lowScale_subj = agr$response[match(tolower(e$item1),agr$predicate)]
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
#contrasts(e$Gender) <- "contr.sum"
e$politeness<-factor(e$politeness)
e$politeness<-relevel(e$politeness,ref="Low")
contrasts(e$politeness) <- "contr.sum"

head(e)
length(unique(e$item))

e$cnotadj<-e$notAdj_subj-mean(e$notAdj_subj)
e$clowscale<-e$lowScale_subj-mean(e$lowScale_subj)
e$chighscale<-e$highScale_subj-mean(e$highScale_subj)
e$cdiff<-e$diff-mean(e$diff)

##############################################################################
########################## FIRST TWO EXPERIMENTS #############################
##############################################################################

###################
###### model ######
###################

e12 = e[e$exp=="e1"|e$exp=="e2",]

e12$cnotadj<-e12$notAdj_subj-mean(e12$notAdj_subj)
e12$clowscale<-e12$lowScale_subj-mean(e12$lowScale_subj)
e12$chighscale<-e12$highScale_subj-mean(e12$highScale_subj)
e12$cdiff<-e12$diff-mean(e12$diff)
m12<-clmm(valuef~polarity*cnotadj+ (1|item) + (1|Worker_ID) , data =e12)
# m12<-clmm(valuef~polarity*clowscale+ (1|item) + (1|Worker_ID) , data =e12)
# m12<-clmm(valuef~polarity*chighscale+ (1|item) + (1|Worker_ID) , data =e12)
# m12<-clmm(valuef~polarity*cdiff+ (1|item) + (1|Worker_ID) , data =e12)
summary(m12)

#Number of groups:  Worker_ID 139,  item 20 
#
# Coefficients:
#                   Estimate Std. Error z value Pr(>|z|)    
# polarity1          0.42096    0.03054  13.782  < 2e-16 ***
# cnotadj           -0.91873    0.50030  -1.836   0.0663 .  
# polarity1:cnotadj  2.18369    0.39453   5.535 3.11e-08 ***

## without "accurate"

e12_new = e12[e12$item!="Accurate_Inaccurate",]

length(unique(e12_new$item))

e12_new$cnotadj<-e12_new$notAdj_subj-mean(e12_new$notAdj_subj)
m12_new<-clmm(valuef~polarity*cnotadj+ (1|item) + (1|Worker_ID) , data =e12_new)
summary(m12_new)

#Number of groups:  Worker_ID 139,  item 19 
#
# Coefficients:
#                   Estimate Std. Error z value Pr(>|z|)    
# polarity1          0.46311    0.03163  14.643  < 2e-16 ***
# cnotadj           -1.11048    0.49386  -2.249   0.0245 *  
# polarity1:cnotadj  1.85434    0.44874   4.132 3.59e-05 ***


###################
###### plots ######
###################

library(ggplot2)
library(Rmisc)
ag12 <- summarySE(e12, measurevar="value", groupvars=c("polarity","adjective","notAdj_subj"),na.rm=T)

ag12$polarity<-factor(ag12$polarity,levels=c("Pos","Neg"))
ggplot(ag12, aes(y=value,x=notAdj_subj, colour=polarity, label=adjective)) +
  labs(y='negative strengthening\n', x="\nsubjectivity",colour="adjective\npolarity") +
  scale_color_manual(labels = c("positive", "negative"),values=c("blue","red"))+
  #geom_text()+
  geom_smooth(method="lm")+
  geom_point(size=2)+ 
  xlim(0.35,0.8)+
  theme_bw()
#ggsave("neg-strengthening-vs-neg-adj-subj-e12-data.pdf",width=6.25,height=4.25)

ggplot(ag12, aes(y=value,x=notAdj_subj, colour=polarity, label=adjective)) +
  labs(y='negative strengthening\n', x="\nsubjectivity",colour="adjective\npolarity") +
  scale_color_manual(labels = c("positive", "negative"),values=c("blue","red"))+
  geom_text()+
  geom_smooth(method="lm")+
  geom_point(size=2,alpha=0.15)+ 
  xlim(0.35,0.8)+
  theme_bw()
#ggsave("neg-strengthening-vs-neg-adj-subj-e12-data-labeled.pdf",width=6.25,height=4.25)


ggplot(ag12, aes(y=value,x=notAdj_subj, colour=polarity, label=adjective)) +
  labs(y='negative strengthening\n', x="\nsubjectivity",colour="adjective\npolarity") +
  scale_color_manual(labels = c("positive", "negative"),values=c("blue","red"))+
  geom_text(alpha=0.25)+
  geom_smooth(method="lm")+
  geom_point(size=2,alpha=0.15)+ 
  xlim(0.35,0.8)+
  theme_bw()
#ggsave("neg-strengthening-vs-neg-adj-subj-e12-data-labeled-alpha.pdf",width=6.25,height=4.25)



#######################################
###### bootstrapping correlation ######
#######################################

library(hydroGOF)
library(boot)

p12 = aggregate(value~polarity*adjective*notAdj_subj,data=e12[e12$polarity=="Pos",],FUN=mean)

gof(p12$value,p12$notAdj_subj)
# r = 0.37, r2 = 0.14
results <- boot(data=p12, statistic=rsq, R=10000, formula=value~notAdj_subj)
boot.ci(results, type="bca") 
# 95%   ( 0.0010,  0.5742 )  

n12 = aggregate(value~polarity*adjective*notAdj_subj,data=e12[e12$polarity=="Neg",],FUN=mean)

gof(n12$value,n12$notAdj_subj)
# r = -0.24, r2 = 0.06
results <- boot(data=n12, statistic=rsq, R=10000, formula=value~notAdj_subj)
boot.ci(results, type="bca") 
# 95%  ( 0.0001,  0.3309 )   


##############################################################################
########################## ALL FOUR EXPERIMENTS ##############################
##############################################################################



###################
###### model ######
###################


m1234<-clmm(valuef~polarity*cnotadj+ (1|item) + (1|Worker_ID) , data =e)
#m1234<-clmm(valuef~polarity*clowscale+ (1|item) + (1|Worker_ID) , data =e)
#m1234<-clmm(valuef~polarity*chighscale+ (1|item) + (1|Worker_ID) , data =e)
#m1234<-clmm(valuef~polarity*cdiff+ (1|item) + (1|Worker_ID) , data =e)
summary(m1234)

#Number of groups:  Worker_ID 239,  item 20 
#
#Coefficients:
#                   Estimate Std. Error z value Pr(>|z|)    
# polarity1          0.54701    0.02211  24.744  < 2e-16 ***
# cnotadj           -0.88541    0.35936  -2.464   0.0137 *  
# polarity1:cnotadj  1.82685    0.27932   6.540 6.14e-11 ***

## without "accurate"

e_new = e[e$item!="Accurate_Inaccurate",]

length(unique(e_new$item))

e_new$cnotadj<-e_new$notAdj_subj-mean(e_new$notAdj_subj)
m1234_new<-clmm(valuef~polarity*cnotadj+ (1|item) + (1|Worker_ID) , data =e_new)
summary(m1234_new)

#Number of groups:  Worker_ID 239,  item 19 
#
# Coefficients:
#                   Estimate Std. Error z value Pr(>|z|)    
# polarity1          0.57025    0.02285  24.953  < 2e-16 ***
# cnotadj           -0.98398    0.36523  -2.694  0.00706 ** 
# polarity1:cnotadj  2.00793    0.32031   6.269 3.64e-10 ***


## combined gender analaysis

m1234_gender<-clmm(valuef~polarity*Gender*cnotadj+ (1|item) + (1|Worker_ID) , 
               data =e)
summary(m1234_gender)

# Number of groups:  Worker_ID 239,  item 20 
#
#  Coefficients:
#                               Estimate Std. Error z value Pr(>|z|)    
#  polarity1                     0.72753    0.03303  22.028  < 2e-16 ***
#  GenderMale                   -0.37677    0.24307  -1.550    0.121    
#  cnotadj                      -0.83478    0.44211  -1.888    0.059 .  
#  polarity1:GenderMale         -0.31498    0.04240  -7.428 1.10e-13 ***
#  polarity1:cnotadj             1.99805    0.37971   5.262 1.42e-07 ***
#  GenderMale:cnotadj           -0.08554    0.45287  -0.189    0.850    
#  polarity1:GenderMale:cnotadj -0.26665    0.45313  -0.588    0.556



## exp1 (our exp3) base model from paper plus subjectivity
me3<-clmm(valuef~polarity*politeness*Gender+cnotadj + (1|item) + (1|Worker_ID) , 
          data =e[e$exp=="e3",])
summary(me3)
me3_subj<-clmm(valuef~polarity*politeness*Gender+polarity:cnotadj + (1|item) + (1|Worker_ID) , 
                 data =e[e$exp=="e3",])
summary(me3_subj)

anova(me3,me3_subj)

#no.par    AIC  logLik LR.stat df Pr(>Chisq)
#me1          16 5733.1 -2850.6                      
#me1_subj     17 5733.5 -2849.7    1.66  1     0.1976


## exp2 (our exp4) base model from paper plus subjectivity
me4<-clmm(valuef~polarity*politeness*Gender+cnotadj + (1|item) + (1|Worker_ID) , 
         data =e[e$exp=="e4",])
summary(me4)
me4_subj<-clmm(valuef~polarity*politeness*Gender+polarity:cnotadj + (1|item) + (1|Worker_ID) , 
          data =e[e$exp=="e4",])
summary(me4_subj)

anova(me4,me4_subj)

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
  labs(y='negative strengthening\n', x="\nsubjectivity",colour="adjective\npolarity") +
  scale_color_manual(labels = c("positive", "negative"),values=c("blue","red"))+
  #geom_text()+
  geom_smooth(method="lm")+
  geom_point(size=2)+ 
  xlim(0.35,0.8)+
  theme_bw()
#ggsave("neg-strengthening-vs-neg-adj-subj-e1234-data.pdf",width=6.25,height=4.25)


ggplot(ag, aes(y=value,x=notAdj_subj, colour=polarity, label=adjective)) +
  labs(y='negative strengthening\n', x="\nsubjectivity",colour="adjective\npolarity") +
  scale_color_manual(labels = c("positive", "negative"),values=c("blue","red"))+
  geom_text(alpha=0.25)+
  geom_smooth(method="lm")+
  geom_point(size=2,alpha=0.15)+ 
  xlim(0.35,0.8)+
  theme_bw()
#ggsave("neg-strengthening-vs-neg-adj-subj-e1234-data-labeled.pdf",width=6.25,height=4.25)

ag_p <- summarySE(e, measurevar="value", groupvars=c("polarity","adjective","notAdj_subj","Gender"),na.rm=T)

ggplot(ag_p, aes(y=value,x=notAdj_subj, colour=polarity, label=adjective)) +
  labs(y='negative strengthening\n', x="\nnegated adjective subjectivity",colour="adjective\npolarity") +
  scale_color_manual(labels = c("positive", "negative"),values=c("blue","red"))+
  #geom_text()+
  geom_smooth(method="lm")+
  geom_point(size=2)+ 
  facet_grid(~Gender)+
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

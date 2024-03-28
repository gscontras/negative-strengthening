
########################################################################
#### Analysis of the role of subjectivity in negative strengthening ####
################## Replication with faultless disagreement measure #####
###################### 29.01.2024 ######################################
########################################################################

#setwd("~/git/negative-strengthening/analyses/paper-submission/Script/")

####################################
#### load new subjectivity data ####
####################################

df = read.csv("../data/results_faultless.csv",header=T)

unique(df$language)

length(unique(df$participant_id)) # n=66

## remove non-English speakers
d = df[df$language!=""&
         df$language!="blue"&
         df$language!="GREG"
       ,]
unique(d$language)
length(unique(d$participant_id)) # n=38

## load helper file for bootstrapped CIs
source("helpers.r")


colnames(d)[4] <- "full_predicate"
colnames(d)[5] <- "predicate"
colnames(d)[6] <- "valence"

d$negated="false"

agr = bootsSummary(data=d, measurevar="response", groupvars=c("predicate","valence","negated"))

mean(agr$N) ## 16.67

agr_mean = bootsSummary(data=d, measurevar="response", groupvars=c("valence","negated"))
agr_mean


#################################################
#### load negative strengthening data  ##########
#################################################

crit<-read.delim("../data/facethreat_anonymous.txt",header=T, dec=",")
crit2<-read.delim("../data/facethreat_standard-nonstandard_anonymous.txt",header=T, dec=",")
crit$person<-NULL
crit$context<-NULL
crit$adjective<-NULL
crit2$person<-NULL
crit2$context<-NULL
crit2$adjective<-NULL
crit$exp = "e1"
crit2$exp = "e2"

crit<-rbind(crit, crit2)
crit<-crit[crit$Gender!="",]
str(crit)
head(crit)

e<-crit
head(e)
summary(e)
str(e)

e$adjective = NA

e$item1 <- as.character(e$item1)
e$item2 <- as.character(e$item2)
e[e$polarity=="Pos",]$adjective <- e[e$polarity=="Pos",]$item1
e[e$polarity=="Neg",]$adjective <- e[e$polarity=="Neg",]$item2
e$notAdj_subj = agr$response[match(paste("not",tolower(e$adjective)),agr$predicate)]
e$adj_subj = agr$response[match(tolower(e$adjective),agr$predicate)]

e$ant = NA
e[e$polarity=="Pos",]$ant <- e[e$polarity=="Pos",]$item2
e[e$polarity=="Neg",]$ant <- e[e$polarity=="Neg",]$item1
e$ant_subj = agr$response[match(tolower(e$ant),agr$predicate)]

e$diff=NA
e$diff = e$notAdj_subj - e$ant_subj

head(e)

###contrast coding
library(ordinal)
e$valuef<-factor(e$value)
e$polarity<-factor(e$polarity)
e$polarity<-relevel(e$polarity,ref="Pos")
e$polarity<-relevel(e$polarity,ref="Neg")
contrasts(e$polarity) <- "contr.sum"

head(e)
length(unique(e$item))

##centering
e$cdiff<-e$diff-mean(e$diff)
e$cadj_subj<-e$adj_subj-mean(e$adj_subj)


#########################
###### mixed model ######
#########################

###### analyzing subjectivity directly

### maximal model subjectivity
mfulladj<-clmm(valuef~polarity*cadj_subj+ (1+polarity|item) + (1+polarity|Worker_ID) , data =e)
summary(mfulladj)

## without "accurate"

e_new = e[e$item!="Accurate_Inaccurate",]

length(unique(e_new$item))

newfull<-clmm(valuef~polarity*cadj_subj+ (1+polarity|item) + (1+polarity|Worker_ID) , data =e_new)
summary(newfull)


##################
###### plot ######
##################

library(ggplot2)
library(Rmisc)
library(ggrepel)


ag <- summarySE(e, measurevar="value", groupvars=c("polarity","adjective","adj_subj","item"),na.rm=T)

ag$polarity<-factor(ag$polarity,levels=c("Pos","Neg"))
ggplot(ag, aes(y=value,x=adj_subj, colour=polarity, label=adjective)) +
  labs(y='negative strengthening\n', x="\nsubjectivity",colour="adjective\npolarity") +
  scale_color_manual(labels = c("positive", "negative"),values=c("blue","red"))+
  geom_text_repel()+
  #geom_text()+
  geom_smooth(method="lm")+
  geom_point(size=2)+ 
  #xlim(0.25,0.85)+
  theme_bw() #+
  #theme(text = element_text(size=14))
#ggsave("adjective-subjectivity.pdf",width=8 ,height=6)



#######################################
###### bootstrapping correlation ######
#######################################

library(hydroGOF)
library(boot)


###simple adj
###pos
p = aggregate(value~polarity*adjective*adj_subj*item,data=e[e$polarity=="Pos",],FUN=mean)
gof(p$value,p$adj_subj)
results <- boot(data=p, statistic=rsq, R=10000, formula=value~adj_subj)
boot.ci(results, type="bca") 

###neg
n = aggregate(value~polarity*adjective*adj_subj*item,data=e[e$polarity=="Neg",],FUN=mean)
gof(n$value,n$adj_subj)
results <- boot(data=n, statistic=rsq, R=10000, formula=value~adj_subj)
boot.ci(results, type="bca") 


###simple adj excluding accurate
###pos
p = aggregate(value~polarity*adjective*adj_subj*item,data=e_new[e_new$polarity=="Pos",],FUN=mean)
gof(p$value,p$adj_subj)
results <- boot(data=p, statistic=rsq, R=10000, formula=value~adj_subj)
boot.ci(results, type="bca") 


###neg
n = aggregate(value~polarity*adjective*adj_subj*item,data=e_new[e_new$polarity=="Neg",],FUN=mean)
gof(n$value,n$adj_subj)
results <- boot(data=n, statistic=rsq, R=10000, formula=value~adj_subj)
boot.ci(results, type="bca") 




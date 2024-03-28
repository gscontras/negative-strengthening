
########################################################################
#### Analysis of the role of subjectivity in negative strengthening ####
###################### 18.12.2023 ######################################
########################################################################

####################################
#### load new faultless data ####
####################################

df = read.csv("../data/results_faultless.csv",header=T)

unique(df$language)

length(unique(df$participant_id)) # n=40

## remove non-English speakers
d = df[df$language!=""&
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

head(d)

agr = bootsSummary(data=d, measurevar="response", groupvars=c("predicate","valence","negated"))

mean(agr$N) ## 21.11

agr_mean = bootsSummary(data=d, measurevar="response", groupvars=c("valence","negated"))
agr_mean


####################################
#### load subjectivity data ####
####################################

df2 = read.delim("../data/results_adj_anonymous.txt",header=T)

unique(df2$language)

length(unique(df2$participant_id))

## remove non-English speakers
d2 = df2[df2$language!=""&
         df2$language!="blue"&
         df2$language!="tomato"
       ,]
unique(d2$language)
length(unique(d2$participant_id)) # n=60

## load helper file for bootstrapped CIs
source("helpers.r")

agr2 = bootsSummary(data=d2, measurevar="response", groupvars=c("predicate","valence","negated"))

mean(agr2$N) ## 16.67

agr2_mean = bootsSummary(data=d2, measurevar="response", groupvars=c("valence","negated"))
agr2_mean

agr2sub<-subset(agr2, negated=="false")


#################################################
##compare subjectivity and faultless disagreement 
#################################################

agr$subj<-agr2sub$response

agr$valence<-factor(agr$valence,levels=c("Pos","Neg"))
ggplot(agr, aes(y=subj,x=response, colour=valence, label=predicate)) +
  labs(y='subjectivity', x="\nfaultless disagreement",colour="adjective\npolarity") +
  scale_color_manual(labels = c("positive", "negative"),values=c("blue","red"))+
  geom_text_repel()+
  #geom_text()+
  geom_smooth(method="lm")+
  geom_point(size=2)

###without color
ggplot(agr, aes(y=subj,x=response, label=predicate)) +
  labs(y='subjectivity', x="\nfaultless disagreement",colour="adjective\npolarity") +
#  scale_color_manual(labels = c("positive", "negative"),values=c("blue","red"))+
  geom_text_repel()+
  #geom_text()+
  geom_smooth(method="lm")+
  geom_point(size=2)

cor.test(agr$subj,agr$response)

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
#e$notAdj_subj = agr$response[match(paste("not",tolower(e$adjective)),agr$predicate)]
e$adj_subj = agr$response[match(tolower(e$adjective),agr$predicate)]

e$ant = NA
e[e$polarity=="Pos",]$ant <- e[e$polarity=="Pos",]$item2
e[e$polarity=="Neg",]$ant <- e[e$polarity=="Neg",]$item1
e$ant_subj = agr$response[match(tolower(e$ant),agr$predicate)]

#e$diff=NA
#e$diff = e$notAdj_subj - e$ant_subj

head(e)

###contrast coding
library(ordinal)
e$valuef<-factor(e$value)
e$polarity<-factor(e$polarity)
e$polarity<-relevel(e$polarity,ref="Pos")
contrasts(e$polarity) <- "contr.sum"

head(e)
length(unique(e$item))

##centering
#e$cdiff<-e$diff-mean(e$diff)
e$cadj_subj<-e$adj_subj-mean(e$adj_subj)


#########################
###### mixed model ######
#########################

###### analyzing subjectivity directly

### maximal model subjectivity
mfulladj<-clmm(valuef~polarity*cadj_subj+ (1+polarity|item) + (1+polarity|Worker_ID) , data =e)
summary(mfulladj)

### simplifiy because model does not converge
m1adj<-clmm(valuef~polarity*cadj_subj+ (1|item) + (1+polarity|Worker_ID) , data =e)
summary(m1adj)

## without "accurate"

e_new = e[e$item!="Accurate_Inaccurate",]

length(unique(e_new$item))

newfull<-clmm(valuef~polarity*cadj_subj+ (1+polarity|item) + (1+polarity|Worker_ID) , data =e_new)
summary(newfull)

newm1<-clmm(valuef~polarity*cadj_subj+ (1|item) + (1+polarity|Worker_ID) , data =e_new)
summary(newm1)


##################
###### plot ######
##################

library(ggplot2)
library(Rmisc)
library(ggrepel)

ag <- summarySE(e, measurevar="value", groupvars=c("polarity","adjective","adj_subj","item"),na.rm=T)

ag$polarity<-factor(ag$polarity,levels=c("Pos","Neg"))
ggplot(ag, aes(y=value,x=adj_subj, colour=polarity, label=adjective)) +
  labs(y='negative strengthening\n', x="\nfaultless disagreement",colour="adjective\npolarity") +
  scale_color_manual(labels = c("positive", "negative"),values=c("blue","red"))+
  geom_text_repel()+
  #geom_text()+
  geom_smooth(method="lm")+
  geom_point(size=2)+ 
  #xlim(0.25,0.85)+
  theme_bw() #+
  #theme(text = element_text(size=14))
#ggsave("../Plots/adjective-faultless.pdf",width=8 ,height=6)


###main effect of subjectivity

ags <- summarySE(e, measurevar="value", groupvars=c("adjective","adj_subj","item"),na.rm=T)

ggplot(ags, aes(y=value,x=adj_subj, label=adjective)) +
  labs(y='negative strengthening\n', x="\nfaultless disagreement",colour="adjective\npolarity") +
  #geom_text()+
  geom_smooth(method="lm")+
  geom_text_repel()+
  geom_point(size=2)+ 
  #xlim(0.25,0.85)+
  theme_bw() #+



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
#r           0.62
#R2          0.39
#95%   ( 0.0588,  0.7486 )  

###neg
n = aggregate(value~polarity*adjective*adj_subj*item,data=e[e$polarity=="Neg",],FUN=mean)
gof(n$value,n$adj_subj)
results <- boot(data=n, statistic=rsq, R=10000, formula=value~adj_subj)
boot.ci(results, type="bca") 
#r          -0.20
#R2          0.04
#95%   ( 0.0000,  0.3201 )

###simple adj excluding accurate
###pos
p = aggregate(value~polarity*adjective*adj_subj*item,data=e_new[e_new$polarity=="Pos",],FUN=mean)
gof(p$value,p$adj_subj)
results <- boot(data=p, statistic=rsq, R=10000, formula=value~adj_subj)
boot.ci(results, type="bca") 
#r           0.42
#R2          0.17
#95%   ( 0.004,  0.459 ) 

###neg
n = aggregate(value~polarity*adjective*adj_subj*item,data=e_new[e_new$polarity=="Neg",],FUN=mean)
gof(n$value,n$adj_subj)
results <- boot(data=n, statistic=rsq, R=10000, formula=value~adj_subj)
boot.ci(results, type="bca") 
#r          -0.25
#R2          0.06
#95%   ( 0.0000,  0.4065 )



##################################################################
####### correlations between different subjectivity scores #######
##################################################################

### correlation between positive adjective and its negative antonym
all_subj = aggregate(adj_subj~item*polarity,FUN=mean,data=e)
pos_subj = all_subj[all_subj$polarity=="Pos",]
neg_subj = all_subj[all_subj$polarity=="Neg",]
cor.test(pos_subj$adj_subj,neg_subj$adj_subj)
## r=0.6735



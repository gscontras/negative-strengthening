
########################################################################
#### Analysis of the role of subjectivity in negative strengthening ####
###################### 13.10.2023 ######################################
########################################################################

setwd("~/git/negative-strengthening/analyses/2023-XPRAG")

####################################
#### load new subjectivity data ####
####################################

df = read.delim("results_adj_anonymous.txt",header=T)

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


#################################################
#### load negative strengthening data  ##########
#################################################

crit<-read.delim("facethreat_anonymous.txt",header=T, dec=",")
crit2<-read.delim("facethreat_standard-nonstandard_anonymous.txt",header=T, dec=",")
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
#r           0.54
#R2          0.30
#95%   ( 0.0035,  0.6166 )  
cor.test(p$value,p$adj_subj)
#t = 2.747, df = 18, p-value = 0.01326

###neg
n = aggregate(value~polarity*adjective*adj_subj*item,data=e[e$polarity=="Neg",],FUN=mean)
gof(n$value,n$adj_subj)
results <- boot(data=n, statistic=rsq, R=10000, formula=value~adj_subj)
boot.ci(results, type="bca") 
#r          -0.11
#R2          0.01
#95%   ( 0.0000,  0.1274 )
cor.test(n$value,n$adj_subj)
#t = -0.46956, df = 18, p-value = 0.6443

###simple adj excluding accurate
###pos
p = aggregate(value~polarity*adjective*adj_subj*item,data=e_new[e_new$polarity=="Pos",],FUN=mean)
gof(p$value,p$adj_subj)
results <- boot(data=p, statistic=rsq, R=10000, formula=value~adj_subj)
boot.ci(results, type="bca") 
#r           0.39
#R2          0.15
#95%   ( 0.0004,  0.5393 ) 
cor.test(p$value,p$adj_subj)
#t = 1.7605, df = 17, p-value = 0.0963

###neg
n = aggregate(value~polarity*adjective*adj_subj*item,data=e_new[e_new$polarity=="Neg",],FUN=mean)
gof(n$value,n$adj_subj)
results <- boot(data=n, statistic=rsq, R=10000, formula=value~adj_subj)
boot.ci(results, type="bca") 
#r          -0.33
#R2          0.11
#95%   ( 0.0002,  0.4052 )
cor.test(n$value,n$adj_subj)
#t = -1.4378, df = 17, p-value = 0.1687




##################################################################
####### difference score analysis (not presented in paper) #######
##################################################################

#########################
###### mixed model ######
#########################

###### analyzing difference score

### maximal model subjectivity
mfulladj_diff<-clmm(valuef~polarity*cdiff+ (1+polarity|item) + (1+polarity|Worker_ID) , data =e)
summary(mfulladj_diff)

## without "accurate"

newfull_diff<-clmm(valuef~polarity*cdiff+ (1+polarity|item) + (1+polarity|Worker_ID) , data =e_new)
summary(newfull_diff)

##################
###### plot ######
##################

diff <- summarySE(e, measurevar="value", groupvars=c("polarity","adjective","diff","item"),na.rm=T)

diff$polarity<-factor(diff$polarity,levels=c("Pos","Neg"))
ggplot(diff, aes(y=value,x=diff, colour=polarity, label=adjective)) +
  labs(y='negative strengthening\n', x="\nsubjectivity difference",colour="adjective\npolarity") +
  scale_color_manual(labels = c("positive", "negative"),values=c("blue","red"))+
  geom_text_repel()+
  geom_smooth(method="lm")+
  geom_point(size=2)+ 
  #xlim(0.25,0.85)+
  theme_bw()+
  theme(text = element_text(size=14))



####overall correlations between dependent variables

### correlation between positive adjective and its negative antonym
all_subj = aggregate(adj_subj~item*polarity,FUN=mean,data=e)
pos_subj = all_subj[all_subj$polarity=="Pos",]
neg_subj = all_subj[all_subj$polarity=="Neg",]
cor.test(pos_subj$adj_subj,neg_subj$adj_subj)
## r=0.448

### correlation between adjective and its negated version
adj_subj = aggregate(adj_subj~adjective*item,FUN=mean,data=e)
not_subj = aggregate(notAdj_subj~adjective*item,FUN=mean,data=e)
cor.test(adj_subj$adj_subj,not_subj$notAdj_subj)
## r=0.426

adj_subj = aggregate(adj_subj~adjective*item*polarity,FUN=mean,data=e)
pos_adj_subj = adj_subj[adj_subj$polarity=="Pos",]
neg_adj_subj = adj_subj[adj_subj$polarity=="Neg",]
not_subj = aggregate(notAdj_subj~adjective*item*polarity,FUN=mean,data=e)
pos_not_adj_subj = not_subj[not_subj$polarity=="Pos",]
neg_not_adj_subj = not_subj[not_subj$polarity=="Neg",]

### correlation between positive adjective and the negated positive
cor.test(pos_adj_subj$adj_subj,pos_not_adj_subj$notAdj_subj)
## r=0.477

### correlation between negative adjective and the negated negative
cor.test(neg_adj_subj$adj_subj,neg_not_adj_subj$notAdj_subj)
## r=0.412

### correlation between negative adjective and the negated positive
cor.test(neg_adj_subj$adj_subj,pos_not_adj_subj$notAdj_subj)
## r=0.618

### correlation between positive adjective and the negated negative
cor.test(pos_adj_subj$adj_subj,neg_not_adj_subj$notAdj_subj)
## r=0.710
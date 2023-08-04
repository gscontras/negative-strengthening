
###Script politeness and negative strengthening#############################
####Nicole Gotzner 8.6.2021#################################################

crit<-read.delim("exp2_socialdistance_osf.txt",header=T)
str(crit)

###Plots#######################################################

library(ggplot2)
library(plyr)
library(Rmisc)

crit$politeness<-factor(crit$politeness)
crit$polarity<-factor(crit$polarity) 
crit$complexity<-factor(crit$complexity) 
crit$person<-factor(crit$person)
crit$Gender<-factor(crit$Gender)

critind <- summarySEwithin(crit, measurevar="value", withinvars=c("polarity","politeness"),na.rm=T)
critindc <- summarySEwithin(crit, measurevar="value", withinvars=c("polarity","complexity"),na.rm=T)
critindg <- summarySEwithin(crit, measurevar="value",withinvars=c("polarity","politeness"),betweenvars = ("Gender"),na.rm=T)
critindgp <- summarySEwithin(crit, measurevar="value",withinvars=c("polarity","politeness","person"),betweenvars = ("Gender"),na.rm=T)

ggplot(data=critind, aes(x=polarity, y=value, fill=politeness)) + geom_bar(stat="identity", position=position_dodge())+
  labs(y = "Negative Strengthening", x = "Polarity") +
  theme(text = element_text(size=20))+
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                width=.2,position=position_dodge(.9))+
  coord_cartesian(ylim = c(4.5, 6.5))+ 
  ggsave("distance.pdf",width=6.5,height=4.5)

ggplot(data=critindg, aes(x=polarity, y=value, fill=politeness)) + geom_bar(stat="identity", position=position_dodge())+
  labs(y = "Negative Strengthening", x = "Polarity") +
  theme(text = element_text(size=20))+
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                width=.2,position=position_dodge(.9))+
  coord_cartesian(ylim = c(4.5, 6.5))+ 
  facet_wrap(~ Gender)+
  ggsave("distance_gender.pdf",width=6.5,height=4.5)

ggplot(data=critindgp, aes(x=polarity, y=value, fill=politeness)) + geom_bar(stat="identity", position=position_dodge())+
  labs(y = "Negative Strengthening", x = "Polarity") +
  theme(text = element_text(size=20))+
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                width=.2,position=position_dodge(.9))+
  coord_cartesian(ylim = c(4.5, 6.5))+ 
  facet_wrap(~ Gender+person)+
  ggsave("distance_gender_person.pdf",width=6.5,height=4.5)

ggplot(data=critindc, aes(x=polarity, y=value, fill=polarity)) + geom_bar(stat="identity", position=position_dodge())+
  labs(y = "Negative Strengthening", x = "Polarity") +
  theme(text = element_text(size=20))+
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                width=.2,position=position_dodge(.9))+
  coord_cartesian(ylim = c(4.5, 6.5))+ 
  facet_wrap(~ complexity)+
  ggsave("polarity_complexity.pdf",width=6.5,height=4.5)

#####CLMM##################################################

library(ordinal)
crit$valuef<-factor(crit$value)
contrasts(crit$politeness) <- "contr.sum"
contrasts(crit$polarity) <- "contr.sum"
contrasts(crit$complexity) <- "contr.sum"
crit$person<-relevel(crit$person,ref="F")
crit$Gender<-relevel(crit$Gender,ref="Female")

## base model
m1<-clmm(valuef~polarity*politeness + (1|item) + (1|Worker_ID) , data =crit)
summary(m1)

## model with gender
m2<-clmm(valuef~polarity*politeness*Gender + (1|item) + (1|Worker_ID) , data =crit)
summary(m2)

## disregard other models
m3<-clmm(valuef~polarity*politeness*Gender*person + (1|item) + (1|Worker_ID) , data =crit)
summary(m3)
m4<-clmm(valuef~polarity*politeness*person + (1|item) + (1|Worker_ID) , data =crit)
summary(m4)
m5<-clmm(valuef~polarity*complexity + (1|item) + (1|Worker_ID) , data =crit)
summary(m5)
  





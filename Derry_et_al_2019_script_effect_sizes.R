install.packages("metafor")
library(metafor)
#Open file that includes the effect sizes.
dat=read.csv(file.choose(),header=T)
dat$gen2<-as.numeric(dat$gen2)
dat$CONSERVATION_STRATEGY<-as.factor(dat$CONSERVATION_STRATEGY)
dat$SPECIES<-as.factor(dat$SPECIES)
dat$FITNESS<-as.factor(dat$FITNESS)
res=rma(yi,vi,data=dat,mods=cbind(CONSERVATION_STRATEGY,SPECIES,FITNESS,gen2))
res
#Data exploration and summary statistics
tapply(dat$yi,dat$FITNESS,mean)
table(dat$CONSERVATION_STRATEGY,dat$FITNESS)
table(dat$SPECIES,dat$FITNESS)
plot(dat$yi~dat$gen2,xlab="Maximum number of generations",ylab="Effect size (difference in fitness)")
abline(lm(dat$yi~dat$gen2))
tapply(dat$vi,dat$CONSERVATION_STRATEGY,mean)
tapply(dat$yA,dat$CONSERVATION_STRATEGY,mean)
tapply(dat$yB,dat$CONSERVATION_STRATEGY,mean)
#sensitivity analysis
res2=rma(yi,vi,data=dat,subset=(CONSERVATION_STRATEGY=="genetic_rescue"))
res3=rma(yi,vi,data=dat,subset=(CONSERVATION_STRATEGY=="evolutionary_rescue"))
res4=rma(yi,vi,data=dat,subset=(CONSERVATION_STRATEGY=="demographic_rescue"))
res5=rma(yi,vi,data=dat,subset=(CONSERVATION_STRATEGY=="transgenerational_plasticity"))
res6=rma(yi,vi,data=dat,subset=(CONSERVATION_STRATEGY=="hybridization"))
leave1out(res2)
leave1out(res3)
leave1out(res4)
leave1out(res5)
leave1out(res6)
#Contingency analysis using full dataset
dat2=read.csv(file.choose(),header=T)
chisq.test(dat2$cons_strat,dat2$Sign)
table(dat2$cons_strat,dat2$Sign)
#Contingency analysis using one entry per study
dat3=read.csv(file.choose(),header=T)
chisq.test(dat3$cons_strat,dat3$SIGN)
table(dat3$cons_strat,dat3$SIGN)


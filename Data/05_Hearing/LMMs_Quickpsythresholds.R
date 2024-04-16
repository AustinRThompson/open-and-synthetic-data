
# 23.09.2019 Quickpys thresholds 

# LMM of SoundLoc data for averaged thresholds
# Below can be seen the results of LMMs with two different R packages. 
# We reported the result from nlme package, which did not provide different reuslts from lmerTest package
# The only difference is nlme package can take into account of difference variances

#import threhsold data
setwd("")

# thresholds exceeding 3SD from the mean of that condition 
# where condition: one group x one arm x one position
# they are 34 out of 408, 8.3 percent

plane.thre.out <- read.table("All_Thresholds_outliersmarked-500B.txt", sep=",", header=T)

names(plane.thre.out)[4]<-c('head')
plane.thre.out$arm <- factor(plane.thre.out$arm)
plane.thre.out$subj <- factor(plane.thre.out$subj)
plane.thre.out$head <- factor(plane.thre.out$head)
plane.thre.out$planes <- factor(plane.thre.out$planes)
plane.thre.out$group <- ifelse(plane.thre.out$group=='EB', c('CB'), c('SC'))
plane.thre.out$group <-factor(plane.thre.out$group)

#omit all +-3SD outliers 
plane.thre.out<- subset(plane.thre.out, outlier==0)
plane.thre.out[which(plane.thre.out$thre >12),]
# still 7 points. One is 216589 !

#divide back
horizontal.thre.out <- subset(plane.thre.out,planes=='hor')
vertical.thre.out <- subset(plane.thre.out,planes=='ver')

rownames(horizontal.thre.out) <-c(1:nrow(horizontal.thre.out)) #correct the row numbers
horizontal.thre.out[which(horizontal.thre.out$thre >10),] # we know from glmm there's one outlier
horizontal.thre.out <-horizontal.thre.out[-c(188),] 


rownames(vertical.thre.out) <-c(1:nrow(vertical.thre.out))
vertical.thre.out[which(vertical.thre.out$thre >12),] # we know from glmm there's one outlier
vertical.thre.out <-vertical.thre.out[-c(144),] 


# in the horizontal back condition left arm is right area // right arm is left area of the subject
hor.thre.front <- subset(horizontal.thre.out, head ==1)
hor.thre.back <- subset(horizontal.thre.out, head ==2)

hor.thre.back$arm <-as.character(hor.thre.back$arm)
hor.thre.back <- within(hor.thre.back, arm[arm=="left"] <-("Alien"))#just to assign into a temp
hor.thre.back <- within(hor.thre.back, arm[arm=='right'] <- 'left')
hor.thre.back <- within(hor.thre.back, arm[arm=='Alien'] <- 'right')

horizontal.thre.out <-rbind(hor.thre.front,hor.thre.back)


# combine them back
plane.thre.out <- rbind(horizontal.thre.out,vertical.thre.out)
plane.thre.out$arm <- factor(plane.thre.out$arm)
plane.thre.out$subj <- factor(plane.thre.out$subj)
plane.thre.out$head <- factor(plane.thre.out$head)
plane.thre.out$planes <- factor(plane.thre.out$planes)
plane.thre.out$group <-factor(plane.thre.out$group)

#divide back
horizontal.thre.out <- subset(plane.thre.out,planes=='hor')
vertical.thre.out <- subset(plane.thre.out,planes=='ver')


#############################################

##### GLMMs ###########################

######################################################
# package for GLMM
library(lmerTest)
library(nlme)
library(lattice)
library(emmeans)


#############################################
# GLMM on the horizontal thresholds
#############################################

#####
# this is the model itself - subject is the random fx

horizontal.out.glmm <- lmer(thre ~ arm * group * head + (1|subj), data = horizontal.thre.out)
horizontal.out.nlme <-lme(thre ~ 1 + arm * group * head,random = ~1|subj, data = horizontal.thre.out)


# As suggested, before looking at the results we look at the whether the assumptions are met
# i.e. check the residuals

# homoscedastacity
plot(horizontal.out.glmm)
plot(horizontal.out.nlme) 


# normality of residuals
qqnorm(residuals(horizontal.out.glmm))
qqnorm(residuals(horizontal.out.nlme)) #same
qqline(residuals(horizontal.out.nlme))


# normality of random effect
#qqnorm(ranef(my.horizontal.glmm)$sub[,1])
qqline(ranef(my.horizontal.glmm)$sub[,1])

qqnorm(ranef(horizontal.out.glmm)$sub[,1])
qqline(ranef(horizontal.out.glmm)$sub[,1]) 


# Results
# to see the p-values you can look at the summary of the model
# or at the "anova" to see the global effect of a variable

summary(horizontal.out.glmm)
my.hor.anova<- anova(horizontal.out.glmm, ddf="Kenward-Roger")
my.hor.anova

#
my.hor.anova<- anova(horizontal.out.nlme)
#output
my.hor.anova



# post-hoc comparisons
# 1. ARM

#get DoF with satterthwaite approximation 
emm.hor0<- emmeans(horizontal.out.nlme, specs = pairwise ~ arm, mode = "satterthwaite",adjust = "none")
pair.hor.table0 <-as.data.frame(emm.hor0$contrasts)
DoF_Satt <-pair.hor.table0$df

emm.hor.t1 <- emmeans(horizontal.out.nlme, specs = pairwise ~ arm,  adjust = "none")

# gives warning: NOTE: Results may be misleading due to involvement in interactions
pair.hor.table <-as.data.frame(emm.hor.t1$contrasts)
pair.hor.table

#extract p values to adjust for the contrasts of interests
pair.hor.table$p.value <-round(pair.hor.table$p.value,digits = 4)
pvalues <-pair.hor.table$p.value

p.bonfer<- p.adjust(pvalues,method="bonferroni")
p.FDR<- p.adjust(pvalues,method="fdr")

p.bonfer<-as.data.frame(p.bonfer)
p.FDR<-as.data.frame(p.FDR)
names(p.bonfer) <-c('p.bonferroni')

pair.hor.table <- cbind(pair.hor.table,p.bonfer,p.FDR,DoF_Satt)

#save
#write.table(pair.hor.table, "Thres_Hor_interaction_ARM_new.txt", sep=",", row.names=F, col.names=T)


# 2. ARM x GROUP
emm.hor0<- emmeans(horizontal.out.nlme, specs = pairwise ~ arm * group, mode = "satterthwaite",adjust = "none")
pair.hor.table0 <-as.data.frame(emm.hor0$contrasts)
DoF_Satt <-pair.hor.table0$df

emm.hor <- emmeans(horizontal.out.nlme, specs = pairwise ~  arm * group,  adjust = "none")

pair.hor.table <-as.data.frame(emm.hor$contrasts)
pair.hor.table$p.value <- round(pair.hor.table$p.value,digits = 4)

pair.hor.table

#let's look at the warning
# gives warning: NOTE: Results may be misleading due to involvement in interactions
# let's plot and see if results indeed would be misleading
emmip(horizontal.out.nlme, group ~ arm | head)
emmip(horizontal.out.nlme, group ~ arm)

#choose contrasts of interest
COI <- c(3, 8, 12) #compare the groups

miss_val<-length(1:nrow(pair.hor.table)) - length(COI)
pair.hor.table$con_of_Int <- ifelse(seq_len(nrow(pair.hor.table)) %in% COI, 1, 0)

#extract p values to adjust for the contrasts of interests
pvalues_interest <- pair.hor.table$p.value[COI]
p.bonfer<-c(p.adjust(pvalues_interest,method="bonferroni"),rep.int(99,miss_val))
p.FDR<- c(p.adjust(pvalues_interest,method="fdr"),rep.int(99,miss_val))

p.bonfer<-as.data.frame(p.bonfer)
p.FDR<-as.data.frame(p.FDR)
names(p.bonfer) <-c('p.bonferroni')
temp <-cbind(p.bonfer,p.FDR)


#order back to 
arr <-1:nrow(temp)
arr2<- arr[(!(arr %in% COI))]
temp$idx <- c(COI,arr2)
temp<-temp[order(temp$idx),]
#temp$con<-pair.ver.table$con_of_Int #this is a check point
temp$idx <-NULL
temp

pair.hor.table <- cbind(pair.hor.table,temp,DoF_Satt)

#save
#write.table(pair.hor.table, "Thres_Hor_interaction_ARMxGroup_new.txt", sep=",", row.names=F, col.names=T)


#############################################
# GLMM on the vertical thresholds
#############################################

######
# this is the model itself - subject is the random fx
vertical.out.glmm <- lmer(thre ~ arm * group * head + (1|subj), data = vertical.thre.out) # singular fit
vertical.out.nlme <- lme(thre ~ 1 + arm * group * head,random = ~1|subj, data = vertical.thre.out)

# As suggested, before looking at the results we look at the whether the assumptions are met
# i.e. we check the residuals

# homoscedastacity
plot(vertical.out.glmm) #
plot(vertical.out.nlme) # same


# normality of random effect
qqnorm(ranef(vertical.out.glmm)$sub[,1])
qqline(ranef(vertical.out.glmm)$sub[,1])

#
qqnorm(residuals(vertical.out.nlme))
qqline(residuals(vertical.out.nlme))

# random effects coefficients in case you were interested
dotplot(ranef(vertical.out.glmm,condVar=TRUE))

# Results
# to see the p-values you can look at the summary of the model
# or at the "anova" to see the global effect of a variable

summary(vertical.out.nlme)
summary(vertical.out.glmm)

# anova results of lmerTest package 
my.ver.anova<- anova(vertical.out.glmm)
my.ver.anova<- anova(vertical.out.glmm, ddf="Kenward-Roger")
my.ver.anova

#anova results of nlme package - new thresholds
my.ver.anova<- anova(vertical.out.nlme)
my.ver.anova

#####
# post-hoc comparisons
#1. arm
emm.ver0<- emmeans(vertical.out.nlme, specs = pairwise ~ arm, mode = "satterthwaite",adjust = "none")
pair.ver.table0 <-as.data.frame(emm.ver0$contrasts)
DoF_Satt <-pair.ver.table0$df

emm.ver.t1 <- emmeans(vertical.out.nlme, specs = pairwise ~ arm,  adjust = "none")
pair.ver.table <-as.data.frame(emm.ver.t1$contrasts)
pair.ver.table

#extract p values to adjust for the contrasts of interests
pair.ver.table$p.value <- round(pair.ver.table$p.value,digits = 4)

pvalues <- round(pair.ver.table$p.value,digits = 4)
p.bonfer<- p.adjust(pvalues,method="bonferroni")
p.FDR<- p.adjust(pvalues,method="fdr")

p.bonfer<-as.data.frame(p.bonfer)
p.FDR<-as.data.frame(p.FDR)
names(p.bonfer) <-c('p.bonferroni')

pair.ver.table <- cbind(pair.ver.table,p.bonfer,p.FDR,DoF_Satt)

#save
#write.table(pair.ver.table, "Thres_Ver_interaction_ARM.txt", sep=",", row.names=F, col.names=T)


#2. arm x head
emm.ver0<- emmeans(vertical.out.nlme, specs = pairwise ~ arm * head, mode = "satterthwaite",adjust = "none")
pair.ver.table0 <-as.data.frame(emm.ver0$contrasts)
DoF_Satt <-pair.ver.table0$df

emm.ver <- emmeans(vertical.out.nlme, specs = pairwise ~  arm * head,  adjust = "none")
pair.ver.table <-as.data.frame(emm.ver$contrasts)
pair.ver.table$p.value <- round(pair.ver.table$p.value,digits = 4)

pair.ver.table

#choose contrasts of interest
COI <- c(3, 8, 12) #compare groups

miss_val<-length(1:nrow(pair.ver.table)) - length(COI)
pair.ver.table$con_of_Int <- ifelse(seq_len(nrow(pair.ver.table)) %in% COI, 1, 0)

#extract p values to adjust for the contrasts of interests
pvalues_interest <- pair.ver.table$p.value[COI]
p.bonfer<-c(p.adjust(pvalues_interest,method="bonferroni"),rep.int(99,miss_val))
p.FDR<- c(p.adjust(pvalues_interest,method="fdr"),rep.int(99,miss_val))

p.bonfer<-as.data.frame(p.bonfer)
p.FDR<-as.data.frame(p.FDR)
names(p.bonfer) <-c('p.bonferroni')
temp <-cbind(p.bonfer,p.FDR)


#order back to 
arr <-1:nrow(temp)
arr2<- arr[(!(arr %in% COI))]
temp$idx <- c(COI,arr2)
temp<-temp[order(temp$idx),]
#temp$con<-pair.ver.table$con_of_Int #this is a check point
temp$idx <-NULL
temp

pair.ver.table <- cbind(pair.ver.table,temp,DoF_Satt)
#save
#write.table(pair.ver.table, "Thres_Ver_interaction_ARMxHead_new.txt", sep=",", row.names=F, col.names=T)



########################################
# LMM on planes
########################################

# GLMM on the plane thresholds
# this is the model itself - subject is the random fx
my.plane.glmm <- lmer(thre ~ planes * group * head + (1|subj), data = plane.thre.out)

# take into account of high variability in the vertical planes
plane.out.nlme <- lme(thre ~ 1 + planes * group * head, random = ~ 1|subj, weights = varIdent(form = ~ 1 | planes), data = plane.thre.out)
summary(plane.out.nlme)

# As suggested by Alain, before looking at the results we look at the whether the assumptions are met
# i.e. we check the residuals - only lmerTest package output is compatible with the below analyses

# homoscedastacity
plot(plane.out.nlme) 


# Results
# to see the p-values you can look at the summary of the model
# or at the "anova" to see the global effect of a variable
summary(my.plane.glmm)

# anova on lmerTest package lmer function
my.plane.anova <- anova(my.plane.glmm)
my.plane.anova <- anova(my.plane.glmm, ddf="Kenward-Roger")
my.plane.anova

#nlme package anova on lme function
my.plane.anova <- anova(plane.out.nlme)
my.plane.anova


#####
#post-hoc tests
# interaction 1
emm.plane0<- emmeans(plane.out.nlme, specs = pairwise ~ planes * group, mode = "satterthwaite",adjust = "none")
pair.plane.table0 <-as.data.frame(emm.plane0$contrasts)
DoF_Satt <-pair.plane.table0$df

# to extract uncorrected p values
emm.plane <- emmeans(plane.out.nlme, specs = pairwise ~ planes * group,  adjust = "none")
pair.plane.table <-as.data.frame(emm.plane$contrasts)
pair.plane.table

pair.plane.table$p.value <- round(pair.plane.table$p.value,digits = 4)
#choose only contrast of interests which are 
COI <- c(2, 5) #group comparison 
miss_val<-length(1:nrow(pair.plane.table)) - length(COI)
pair.plane.table$con_of_Int <- ifelse(seq_len(nrow(pair.plane.table)) %in% COI, 1, 0)

#extract p values to adjust for the contrasts of interests
pvalues_interest <- round(pair.plane.table$p.value[COI],digits = 4)
p.bonfer<-c(p.adjust(pvalues_interest,method="bonferroni"),rep.int(99,miss_val))
p.FDR<- c(p.adjust(pvalues_interest,method="fdr"),rep.int(99,miss_val))


p.bonfer<-as.data.frame(p.bonfer)
p.FDR<-as.data.frame(p.FDR)
names(p.bonfer) <-c('p.bonferroni')
temp <-cbind(p.bonfer,p.FDR)

#order back to 
arr <-1:nrow(temp)
arr2<- arr[(!(arr %in% COI))]
temp$idx <- c(COI,arr2)

temp<-temp[order(temp$idx),]
#temp$con<-pair.ver.table$con_of_Int #this is a check point
temp$idx <-NULL
temp

# bind them to make 1 chart
pair.plane.table <- cbind(pair.plane.table,temp,DoF_Satt)

##save
#write.table(pair.plane.table, "Thres_Plane_interaction_PlanexGroup.txt", sep=",", row.names=F, col.names=T)

# interaction 2
emm.plane0<- emmeans(plane.out.nlme, specs = pairwise ~ planes * head, mode = "satterthwaite",adjust = "none")
pair.plane.table0 <-as.data.frame(emm.plane0$contrasts)
DoF_Satt <-pair.plane.table0$df

# to extract uncorrected p values
emm.plane <- emmeans(plane.out.nlme, specs = pairwise ~ planes * head,  adjust = "none")
pair.plane.table <-as.data.frame(emm.plane$contrasts)
pair.plane.table
pair.plane.table$p.value<-round(pair.plane.table$p.value,digits = 4)

#choose only contrast of interests which are 
COI <- c(2, 5) # only compare hor vs. ver in F/B
miss_val<-length(1:nrow(pair.plane.table)) - length(COI)
pair.plane.table$con_of_Int <- ifelse(seq_len(nrow(pair.plane.table)) %in% COI, 1, 0)

#extract p values to adjust for the contrasts of interests
pvalues_interest <- round(pair.plane.table$p.value[COI],digits = 4)
p.bonfer<-c(p.adjust(pvalues_interest,method="bonferroni"),rep.int(99,miss_val))
p.FDR<- c(p.adjust(pvalues_interest,method="fdr"),rep.int(99,miss_val))


p.bonfer<-as.data.frame(p.bonfer)
p.FDR<-as.data.frame(p.FDR)
names(p.bonfer) <-c('p.bonferroni')
temp <-cbind(p.bonfer,p.FDR)

#order back to 
arr <-1:nrow(temp)
arr2<- arr[(!(arr %in% COI))]
temp$idx <- c(COI,arr2)

temp<-temp[order(temp$idx),]
#temp$con<-pair.ver.table$con_of_Int #this is a check point
temp$idx <-NULL
temp

pair.plane.table <- cbind(pair.plane.table,temp,DoF_Satt)
#save
write.table(pair.plane.table, "Thres_Plane_interaction_PlanexHead.txt", sep=",", row.names=F, col.names=T)
emmip(plane.out.nlme, planes ~ head)


#############################################
#inferences 
#############################################
library("piecewiseSEM")
g.o.f.hor <- rsquared(horizontal.out.nlme, method = NULL)
g.o.f.hor


g.o.f.ver <- rsquared(vertical.out.nlme, method = NULL)
g.o.f.ver


g.o.f.planes <- rsquared(plane.out.nlme, method = NULL)
g.o.f.planes


#############################################
#save the GLMM statistical inference output
#############################################
#write.table(my.hor.anova, file="Horizontal_LMM_threshold.txt", sep=",", row.names=T)
#write.table(my.ver.anova, file="Vertical_LMM_threshold.txt", sep=",", row.names=T)
#write.table(my.plane.anova, file="Plane_LMM_threshold.txt", sep=",", row.names=T)

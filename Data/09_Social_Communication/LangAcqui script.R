#-------Packages and functions----------
library(plyr) #load this before dplyr
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(scales)
library(ggsignif)
library(popbio)
#remotes::install_github("coolbutuseless/ggpattern")
library(ggpattern)
library(ggpubr)
library(effsize) #cohen.d
library(patchwork)


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



ts_test <- function(dataL,x,y,method="t.test",idCol=NULL,paired=F,label = "p.signif",p.adjust.method="none",alternative = c("two.sided", "less", "greater"),...) {
  options(scipen = 999)
  
  annoList <- list()
  
  setDT(dataL)
  
  if(paired) {
    allSubs <- dataL[,.SD,.SDcols=idCol] %>% na.omit %>% unique
    dataL   <- dataL[,merge(.SD,allSubs,by=idCol,all=T),by=x]  #idCol!!!
  }
  
  if(method =="t.test") {
    dataA <- eval(parse(text=paste0(
      "dataL[,.(",as.name(y),"=mean(get(y),na.rm=T),sd=sd(get(y),na.rm=T)),by=x] %>% setDF"
    )))
    res<-pairwise.t.test(x=dataL[[y]], g=dataL[[x]], p.adjust.method = p.adjust.method,
                         pool.sd = !paired, paired = paired,
                         alternative = alternative, ...)
  }
  
  if(method =="wilcox.test") {
    dataA <- eval(parse(text=paste0(
      "dataL[,.(",as.name(y),"=median(get(y),na.rm=T),sd=IQR(get(y),na.rm=T,type=6)),by=x] %>% setDF"
    )))
    res<-pairwise.wilcox.test(x=dataL[[y]], g=dataL[[x]], p.adjust.method = p.adjust.method,
                              paired = paired, ...)
  }
  
  #Output the groups
  res$p.value %>% dimnames %>%  {paste(.[[2]],.[[1]],sep="_")} %>% cat("Groups ",.)
  
  #Make annotations ready
  annoList[["label"]] <- res$p.value %>% diag %>% round(5)
  
  if(!is.null(label)) {
    if(label == "p.signif"){
      annoList[["label"]] %<>% cut(.,breaks = c(-0.1, 0.0001, 0.001, 0.01, 0.05, 1),
                                   labels = c("****", "***", "**", "*", "ns")) %>% as.character
    }
  }
  
  annoList[["x"]] <- dataA[[x]] %>% {diff(.)/2 + .[-length(.)]}
  annoList[["y"]] <- {dataA[[y]] + dataA[["sd"]]} %>% {pmax(lag(.), .)} %>% na.omit
  
  #Make plot
  coli="#0099ff";sizei=1.3
  
  p <-ggplot(dataA, aes(x=get(x), y=get(y))) + 
    geom_errorbar(aes(ymin=len-sd, ymax=len+sd),width=.1,color=coli,size=sizei) +
    geom_line(color=coli,size=sizei) + geom_point(color=coli,size=sizei) + 
    scale_color_brewer(palette="Paired") + theme_minimal() +
    xlab(x) + ylab(y) + ggtitle("title","subtitle")
  
  
  #Annotate significances
  p <-p + annotate("text", x = annoList[["x"]], y = annoList[["y"]], label = annoList[["label"]])
  
  
  return(p)
}


#------------START Study 1--------
Comp <- read.csv("./Study1Comp.csv", fileEncoding = 'UTF-8-BOM')
Prod <- read.csv("./Study1Prod.csv", fileEncoding = 'UTF-8-BOM')

Prod$Person <- Prod$Target
Prod$Person = as.character(Prod$Person)
Prod$Person[Prod$Person == "E"] <- "2"
Prod$Person[Prod$Person == "C"] <- "1"
Prod$Person[Prod$Person == "B"] <- "3"
Prod$Person[Prod$Person == "G"] <- "3"
Prod$Person[Prod$Person == "M"] <- "3"
Prod$AnsCat = as.character(Prod$AnsCat)
Prod$AnsCat[Prod$AnsCat == "Occupational titles"] <- "Occu"
Prod$AnsCat[Prod$AnsCat == "Occu + name"] <- "Occu"
Prod$AnsCat[Prod$AnsCat == "Occupation"] <- "Occu"
Prod$AnsCat[Prod$AnsCat == "W"] <- "W/D"
Prod$AnsCat[Prod$AnsCat == "Kin + Name"] <- "Kin"
Prod$AnsCat[Prod$AnsCat == "Kin terms"] <- "Kin"
Prod$AnsCat[Prod$AnsCat == "Personal names"] <- "Name"
Prod$AnsCat[Prod$AnsCat == "Personal pronouns"] <- "PsPron"
Prod$AnsCat[Prod$AnsCat == "Reflexive pronouns"] <- "RfxPron"
Prod$AnsCat[Prod$AnsCat == "Nouns"] <- "Noun"



#----------Study 1: Participant Info-----------
# Summarize to get counts and percentages
Comp.pct = Comp %>% group_by(ParGroup, Accuracy) %>%
  summarise(count=n()) %>%
  mutate(pct=count/sum(count))

subjdata <- ddply(Comp, c("ParGroup","Subj","Gender"), summarise,
                  N    = length(Accuracy),
                  acc  = sum(Accuracy),
                  pct = mean(Accuracy),
                  grade = mean(Grade),
                  age = mean(Months),
                  NVIQ = mean(SSIQ)
                  )

#Participants with ASD over 10/11/12 years old
table(subjdata$age>=120 & subjdata$ParGroup== "ASD") #15
table(subjdata$age>=132 & subjdata$ParGroup== "ASD") #12
table(subjdata$age>=144 & subjdata$ParGroup== "ASD") #4

#Participants with TD over 10/11/12 years old
table(subjdata$age>=120 & subjdata$ParGroup== "TD") #20
table(subjdata$age>=132 & subjdata$ParGroup== "TD") #12
table(subjdata$age>=144 & subjdata$ParGroup== "TD") #1

pardata <- ddply(subjdata, c("ParGroup"), summarise,
               grade.mean = mean(grade),
               grade.sd = sd(grade),
               grade.min = min(grade),
               grade.max = max(grade),
               age.mean = mean(age/12),
               age.sd   = sd(age/12),
               age.min = min(age/12),
               age.max = max(age/12),
               NVIQ.mean = mean(NVIQ),
               NVIQ.sd = sd(NVIQ),
               NVIQ.min = min(NVIQ),
               NVIQ.max = max(NVIQ)
               )

table(subjdata$ParGroup,subjdata$Gender)

summary(aov(age ~ ParGroup, subjdata))
summary(aov(grade ~ ParGroup, subjdata))
summary(aov(NVIQ ~ ParGroup, subjdata))

fisher.test(subjdata$ParGroup, subjdata$Gender)

cohen.d(subjdata$age[subjdata$ParGroup == "ASD"], subjdata$age[subjdata$ParGroup == "TD"])
cohen.d(subjdata$grade[subjdata$ParGroup == "ASD"], subjdata$grade[subjdata$ParGroup == "TD"])
cohen.d(subjdata$NVIQ[subjdata$ParGroup == "ASD"], subjdata$NVIQ[subjdata$ParGroup == "TD"])


#Remove the children who scored less than 50% or withdraw from answering in the production task.
Prod<-Prod[!(Prod$Subj==5),]
Comp<-Comp[!(Comp$Subj==5),]



#--------Study 1: Statistics-------------
ProdASD = filter(Prod, ParGroup=="ASD")
ProdTD = filter(Prod, ParGroup=="TD")
CompASD = filter(Comp, ParGroup=="ASD")
CompTD = filter(Comp, ParGroup=="TD")

#Production
summary(aov(Accuracy ~ ParGroup, Prod))
# Df Sum Sq Mean Sq F value Pr(>F)  
# ParGroup      1   0.32  0.3232   4.299 0.0384 *
#   Residuals   948  71.27  0.0752
cohen.d(ProdASD$Accuracy, ProdTD$Accuracy)
# d estimate: 0.1475543 (negligible)
# 95 percent confidence interval:
#   lower       upper 
# 0.007744046 0.287364620 


#Comprehension
summary(aov(Accuracy ~ ParGroup, Comp))
# Df Sum Sq Mean Sq F value Pr(>F)    
# ParGroup       1   28.6  28.624   165.3 <2e-16 ***
#   Residuals   2848  493.3   0.173 
cohen.d(CompASD$Accuracy, CompTD$Accuracy)
# d estimate: -0.5281648 (medium)
# 95 percent confidence interval:
#   lower      upper 
# -0.6098839 -0.4464457

# (a)
summary(aov(Accuracy ~ ParGroup,  filter(Comp, Target=="phi:")))
# Df Sum Sq Mean Sq F value   Pr(>F)    
# ParGroup      1   1.65  1.6544   12.29 0.000511 ***
#   Residuals   378  50.90  0.1347                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
cohen.d(CompASD$Accuracy[CompASD$Target=="phi:"], CompTD$Accuracy[CompTD$Target=="phi:"])
# d estimate: -0.394385 (small)
# 95 percent confidence interval:
#      lower      upper 
# -0.6174023 -0.1713677 

# (b)
summary(aov(Accuracy ~ ParGroup,  filter(Comp, Target=="nO:N")))
# Df Sum Sq Mean Sq F value Pr(>F)
# ParGroup      1  0.049 0.04888   0.315  0.575
# Residuals   188 29.130 0.15495 
cohen.d(CompASD$Accuracy[CompASD$Target=="nO:N"], CompTD$Accuracy[CompTD$Target=="nO:N"])
# d estimate: -0.08937567 (negligible)
# 95 percent confidence interval:
#   lower      upper 
# -0.4034001  0.2246488 

# (c)
summary(aov(Accuracy ~ ParGroup,  filter(Comp, Target=="nu:")))
# Df Sum Sq Mean Sq F value Pr(>F)
# ParGroup     1 0.1885 0.18846   2.502  0.123
# Residuals   34 2.5615 0.07534 
cohen.d(CompASD$Accuracy[CompASD$Target=="nu:"], CompTD$Accuracy[CompTD$Target=="nu:"])
# d estimate: -0.5885251 (medium)
# 95 percent confidence interval:
#   lower     upper 
# -1.357756  0.180706 

# (d)
summary(aov(Accuracy ~ ParGroup,  filter(Comp, Target=="phom")))
# Df Sum Sq Mean Sq F value Pr(>F)  
# ParGroup      1  0.364  0.3644   2.995 0.0855 .
# Residuals   152 18.493  0.1217  
cohen.d(CompASD$Accuracy[CompASD$Target=="phom"], CompTD$Accuracy[CompTD$Target=="phom"])
# d estimate: -0.3047006 (small)
# 95 percent confidence interval:
#   lower       upper 
# -0.65423559  0.04483435 

# (e)
summary(aov(Accuracy ~ ParGroup,  filter(Comp, Target=="khun")))
# Df Sum Sq Mean Sq F value Pr(>F)  
# ParGroup      1   0.63  0.6250   3.541 0.0614 .
# Residuals   188  33.19  0.1765  
cohen.d(CompASD$Accuracy[CompASD$Target=="khun"], CompTD$Accuracy[CompTD$Target=="khun"])
# d estimate: 0.2994221 (small)
# 95 percent confidence interval:
#   lower       upper 
# -0.01593106  0.61477533 

# (f)
summary(aov(Accuracy ~ ParGroup,  filter(Comp, Target=="khaw")))
# Df Sum Sq Mean Sq F value   Pr(>F)    
# ParGroup      1   6.30   6.305   27.88 2.18e-07 ***
#   Residuals   378  85.47   0.226  
cohen.d(CompASD$Accuracy[CompASD$Target=="khaw"], CompTD$Accuracy[CompTD$Target=="khaw"])
# d estimate: -0.5941247 (medium)
# 95 percent confidence interval:
#   lower      upper 
# -0.8193827 -0.3688668 

# (g)
summary(aov(Accuracy ~ ParGroup,  filter(Comp, Target=="th7:")))
# Df Sum Sq Mean Sq F value Pr(>F)    
# ParGroup      1  14.70   14.70   91.86 <2e-16 ***
#   Residuals   568  90.91    0.16  
cohen.d(CompASD$Accuracy[CompASD$Target=="th7:"], CompTD$Accuracy[CompTD$Target=="th7:"])
# d estimate: -0.8804866 (large)
# 95 percent confidence interval:
#   lower     upper 
# -1.068060 -0.692913 

# (h)
summary(aov(Accuracy ~ ParGroup,  filter(Comp, Target=="man")))
# Df Sum Sq Mean Sq F value Pr(>F)    
# ParGroup      1  14.59  14.594   88.06 <2e-16 ***
#   Residuals   948 157.12   0.166  
cohen.d(CompASD$Accuracy[CompASD$Target=="man"], CompTD$Accuracy[CompTD$Target=="man"])
# d estimate: -0.6677763 (medium)
# 95 percent confidence interval:
#   lower      upper 
# -0.8106283 -0.5249244 


#------Study 1: Overall accuracy-------------

cdata <- ddply(subjdata, c("ParGroup"), summarise,
               pct2 = weighted.mean(pct),
               sd   = sd(pct),
               se   = sd / sqrt(length(pct)))


psubjdata <- ddply(Prod, c("ParGroup","Subj","Gender"), summarise,
                   N    = length(Accuracy),
                   acc  = sum(Accuracy),
                   pct = mean(Accuracy))

pdata <- ddply(psubjdata, c("ParGroup"), summarise,
               pct2 = weighted.mean(pct),
               sd   = sd(pct),
               se   = sd / sqrt(length(pct)))

cdata$test <- "Comprehension"
pdata$test <- "Production"
pct <- merge(cdata,pdata,all=TRUE) #or rbind
pct$test = factor(pct$test, levels=c('Production','Comprehension'))


ggplot(pct, aes(x=ParGroup, y=pct2,fill=ParGroup)) +
  theme_set(theme_gray(base_size = 16)) +
  geom_bar(stat="identity") +
  facet_grid(. ~ test) +
  xlab("") +
  ylab("Grand Means of % Correct") +
  geom_signif(data = filter(pct, test=="Production"),
              y_position = c(1), xmin = c(1), xmax = c(2),
              annotation = c("*"), tip_length = 0) +
  geom_signif(data = filter(pct, test=="Comprehension"),
              y_position = c(0.9), xmin = c(1), xmax = c(2),
              annotation = c("***"), tip_length = 0) +
  scale_fill_manual(values=c("#cccccc", "#575555")) +
  theme(legend.position="none",
        plot.margin = unit(x = c(0, 0, 0, 0), units = "cm")) +
  scale_y_continuous(limits = c(0,1.03), labels=percent) +
  geom_errorbar(aes(ymin=pct2-se, ymax=pct2+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9), color = "black") 



#------Study 1: Production - Overall + by Participant-------------
library(patchwork)

# Summarize to get counts and percentages
Prod.pct.term = filter(Prod, !NewAcc==0) %>% group_by(ParGroup, Person, AnsCat) %>%
  summarise(count=n()) %>%
  mutate(pct=count/sum(count))
#Only the accurate trials

#1st person - comparing counts of personal names and personal pronouns between ASD and TD
namepron1 = matrix(c(31,14,19,91), nrow = 2)
fisher.test(namepron1)
# data:  namepron1
# p-value = 1.646e-09
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
#   4.437412 25.645353
# sample estimates:
#   odds ratio 
# 10.3896 

#2nd person - comparing counts of kin terms and occupational titles between ASD and TD
kinoccu2 = matrix(c(22,27,64,51), nrow = 2)
fisher.test(kinoccu2)
# data:  kinoccu2
# p-value = 0.2341
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
#   0.312937 1.342186
# sample estimates:
#   odds ratio 
# 0.6510466 

#3rd person - comparing counts of nouns and all the other choices between ASD and TD
noun3 = matrix(c(151,13,363,7), nrow = 2)
fisher.test(noun3)
# data:  noun3
# p-value = 0.001884
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
#   0.07437377 0.62000443
# sample estimates:
#   odds ratio 
# 0.2247079 

ggplot(filter(Prod.pct.term, Person==1, AnsCat=="Name" | AnsCat=="PsPron"), aes(x=ParGroup, y=pct,fill=ParGroup)) +
  theme_set(theme_gray(base_size = 16)) +
  geom_bar(stat="identity") +
  facet_grid(. ~ AnsCat) +
  xlab("1st Person (C) - Production Task") +
  ylab("% Correct") +
  scale_fill_manual(values=c("#cccccc", "#575555")) +
  theme(legend.position="none",
        plot.margin = unit(x = c(0, 0, 0, 0), units = "cm")) +
  scale_y_continuous(limits = c(0,1), labels=percent)  
# geom_text(data=filter(Prod.pct.term, Person==1, AnsCat=="Name" | AnsCat=="Ps Pron"), aes(label=paste0(round(pct*100,1),"%"),
#                                                      y=pct+0.03), size=4)


s1prod1 <- ggplot(filter(Prod.pct.term, Person==1), aes(x=ParGroup, y=pct,fill=ParGroup)) +
  geom_bar(stat="identity") +
  facet_grid(. ~ reorder(AnsCat, -pct)) +
  xlab("(a) 1st Person (C) - Production Task") +
  ylab("% Correct") +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0,1), labels=percent) + 
  scale_fill_manual(values=c("#cccccc", "#575555")) +
  geom_text(data=filter(Prod.pct.term, Person==1), aes(label=paste0(round(pct*100,1),"%"),
                                                       y=pct+0.04), size=3)

s1prod2 <- ggplot(filter(Prod.pct.term, Person==2), aes(x=ParGroup, y=pct, fill=ParGroup)) +
  geom_bar(stat="identity") +
  facet_grid(. ~ reorder(AnsCat, -pct)) +
  xlab("(b) 2nd Person (E) - Production Task") +
  ylab("% Correct") +
  theme(legend.position = "none") +
  scale_fill_manual(values=c("#cccccc", "#575555")) +
  scale_y_continuous(limits = c(0,1), labels=percent) + 
  geom_text(data=filter(Prod.pct.term, Person==2), aes(label=paste0(round(pct*100,1),"%"),
                                                       y=pct+0.04), size=3)

s1prod3 <- ggplot(filter(Prod.pct.term, Person==3), aes(x=ParGroup, y=pct, fill=ParGroup)) +
  geom_bar(stat="identity") +
  facet_grid(. ~ reorder(AnsCat, -pct)) +
  xlab("(c) 3rd Person (B/G/M) - Production Task") +
  ylab("% Correct") +
  theme(legend.position = "none") +
  scale_fill_manual(values=c("#cccccc", "#575555")) +
  scale_y_continuous(limits = c(0,1.03), labels=percent) + 
  geom_text(data=filter(Prod.pct.term, Person==3), aes(label=paste0(round(pct*100,1),"%"),
                                                       y=pct+0.04), size=3)

s1prod <- s1prod1 + s1prod2 + s1prod3 + plot_layout(nrow = 3)
s1prod

#by participant
# Summarize to get counts and percentages -- Individual participant results
Prod.pct.term.indiv = filter(Prod, !NewAcc==0) %>% group_by(Person, ParGroup, Subj, AnsCat) %>%
  summarise(count=n()) %>%
  mutate(pct=count/sum(count))
#Only the accurate trials

#ASD
p1 <- ggplot(filter(Prod.pct.term.indiv, Person==1 & ParGroup=="ASD"), aes(x=AnsCat, y=pct, fill=AnsCat)) +
  geom_bar(stat="identity", width = 0.7, position=position_dodge(0.1)) +
  facet_grid(Subj ~ .) +
  xlab("1st Person (C)") +
  ylab("") +
  scale_fill_manual(values=c("#6a0d83", "#ce4993", "#ee5d6c", "#fb9062", "#eeaf61")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,1.5)) +
  theme(text = element_text(size = 12),
        panel.grid = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
        panel.border = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  guides(fill=guide_legend(title="1st AnsCat")) +
  geom_text(data=filter(Prod.pct.term.indiv, Person==1 & ParGroup=="ASD"), aes(label=paste0(round(pct*100,1),"%"),
                                                                               y=pct+0.3), size=2)


p2 <- ggplot(filter(Prod.pct.term.indiv, Person==2 & ParGroup=="ASD"), aes(x=AnsCat, y=pct, fill=AnsCat)) +
  geom_bar(stat="identity", width = 0.4, position=position_dodge(0.1)) +
  facet_grid(Subj ~ .) +
  xlab("2nd Person (E)") +
  ylab("") +
  scale_fill_manual(values=c("#6a0d83", "#ff8080", "#eeaf61")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,1.5)) +
  theme(text = element_text(size = 12),
        panel.grid = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
        panel.border = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  guides(fill=guide_legend(title="2nd AnsCat")) +
  geom_text(data=filter(Prod.pct.term.indiv, Person==2 & ParGroup=="ASD"), aes(label=paste0(round(pct*100,2),"%"),
                                                                               y=pct+0.3), size=2)

p3 <- ggplot(filter(Prod.pct.term.indiv, Person==3 & ParGroup=="ASD"), aes(x=AnsCat, y=pct, fill=AnsCat)) +
  geom_bar(stat="identity", width = 0.5, position=position_dodge(0.1)) +
  facet_grid(Subj ~ .) +
  xlab("3rd Person (B/G/M)") +
  ylab("") +
  scale_fill_manual(values=c("#6a0d83", "#950495", "#ff8080", "#fcd509")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,1.5)) +
  theme(text = element_text(size = 12),
        panel.grid = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
        panel.border = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  guides(fill=guide_legend(title="3rd AnsCat")) +
  geom_text(data=filter(Prod.pct.term.indiv, Person==3 & ParGroup=="ASD"), aes(label=paste0(round(pct*100,2),"%"),
                                                                               y=pct+0.3), size=2)

ASDindiv <- p1 + p2 + p3 + plot_layout(ncol = 3, guides = "collect")
ASDindiv

# png(file="./ASDindiv.png",
#     width=1000, height=2000)
# ASDindiv
# dev.off()


#TD
p4 <- ggplot(filter(Prod.pct.term.indiv, Person==1 & ParGroup=="TD" & Subj < 69), aes(x=AnsCat, y=pct, fill=AnsCat)) +
  geom_bar(stat="identity", width = 0.7, position=position_dodge(0.1)) +
  facet_grid(Subj ~ .) +
  xlab("1st Person (C)") +
  ylab("") +
  scale_fill_manual(values=c("#ce4993", "#950495", "#ee5d6c", "#fb9062")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,1.5)) +
  theme(text = element_text(size = 12),
        panel.grid = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
        panel.border = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  guides(fill=guide_legend(title="1st AnsCat")) +
  geom_text(data=filter(Prod.pct.term.indiv, Person==1 & ParGroup=="TD" & Subj < 69), aes(label=paste0(round(pct*100,1),"%"),
                                                                                          y=pct+0.3), size=2)

p5 <- ggplot(filter(Prod.pct.term.indiv, Person==2 & ParGroup=="TD" & Subj < 69), aes(x=AnsCat, y=pct, fill=AnsCat)) +
  geom_bar(stat="identity", width = 0.5, position=position_dodge(0.1)) +
  facet_grid(Subj ~ .) +
  xlab("2nd Person (E)") +
  ylab("") +
  scale_fill_manual(values=c("#6a0d83", "#ff8080", "#ee5d6c")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,1.5)) +
  theme(text = element_text(size = 12),
        panel.grid = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
        panel.border = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  guides(fill=guide_legend(title="2nd AnsCat")) +
  geom_text(data=filter(Prod.pct.term.indiv, Person==2 & ParGroup=="TD" & Subj < 69), aes(label=paste0(round(pct*100,2),"%"),
                                                                                          y=pct+0.3), size=2)

p6 <- ggplot(filter(Prod.pct.term.indiv, Person==3 & ParGroup=="TD" & Subj < 69), aes(x=AnsCat, y=pct, fill=AnsCat)) +
  geom_bar(stat="identity", width = 0.17, position=position_dodge(0.1)) +
  facet_grid(Subj ~ .) +
  xlab("3rd Person (B/G/M)") +
  ylab("") +
  scale_fill_manual(values=c("#950495")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,1.5)) +
  theme(text = element_text(size = 12),
        panel.grid = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
        panel.border = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  guides(fill=guide_legend(title="3rd AnsCat")) +
  geom_text(data=filter(Prod.pct.term.indiv, Person==3 & ParGroup=="TD" & Subj < 69), aes(label=paste0(round(pct*100,2),"%"),
                                                                                          y=pct+0.3), size=2)

TDindiv1 <- p4 + p5 + p6 + plot_layout(ncol = 3, guides = "collect")
TDindiv1

p7 <- ggplot(filter(Prod.pct.term.indiv, Person==1 & ParGroup=="TD" & Subj > 69), aes(x=AnsCat, y=pct, fill=AnsCat)) +
  geom_bar(stat="identity", width = 0.7, position=position_dodge(0.1)) +
  facet_grid(Subj ~ .) +
  xlab("1st Person (C)") +
  ylab("") +
  scale_fill_manual(values=c("#6a0d83", "#ce4993", "#950495", "#ee5d6c", "#fb9062")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,1.5)) +
  theme(text = element_text(size = 12),
        panel.grid = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
        panel.border = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  guides(fill=guide_legend(title="1st AnsCat")) +
  geom_text(data=filter(Prod.pct.term.indiv, Person==1 & ParGroup=="TD" & Subj > 69), aes(label=paste0(round(pct*100,1),"%"),
                                                                                          y=pct+0.3), size=2)

p8 <- ggplot(filter(Prod.pct.term.indiv, Person==2 & ParGroup=="TD" & Subj > 69), aes(x=AnsCat, y=pct, fill=AnsCat)) +
  geom_bar(stat="identity", width = 0.3, position=position_dodge(0.1)) +
  facet_grid(Subj ~ .) +
  xlab("2nd Person (E)") +
  ylab("") +
  scale_fill_manual(values=c("#6a0d83", "#ff8080")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,1.5)) +
  theme(text = element_text(size = 12),
        panel.grid = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
        panel.border = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  guides(fill=guide_legend(title="2nd AnsCat")) +
  geom_text(data=filter(Prod.pct.term.indiv, Person==2 & ParGroup=="TD" & Subj > 69), aes(label=paste0(round(pct*100,2),"%"),
                                                                                          y=pct+0.3), size=2)

p9 <- ggplot(filter(Prod.pct.term.indiv, Person==3 & ParGroup=="TD" & Subj > 69), aes(x=AnsCat, y=pct, fill=AnsCat)) +
  geom_bar(stat="identity", width = 0.4, position=position_dodge(0.1)) +
  facet_grid(Subj ~ .) +
  xlab("3rd Person (B/G/M)") +
  ylab("") +
  scale_fill_manual(values=c("#6a0d83", "#950495", "#ee5d6c")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,1.5)) +
  theme(text = element_text(size = 12),
        panel.grid = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
        panel.border = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  guides(fill=guide_legend(title="3rd AnsCat")) +
  geom_text(data=filter(Prod.pct.term.indiv, Person==3 & ParGroup=="TD" & Subj > 69), aes(label=paste0(round(pct*100,2),"%"),
                                                                                          y=pct+0.3), size=2)

TDindiv2 <- p7 + p8 + p9 + plot_layout(ncol = 3, guides = "collect")
TDindiv2


#--------------Study 1: Comp Accuracy by Item----------------

# Comprehension
# Summarize to get counts and percentages
#by person
perssubj <- ddply(Comp, c("ParGroup","Person","Subj"), summarise,
                  N    = length(Accuracy),
                  acc  = sum(Accuracy),
                  pct = weighted.mean(Accuracy))

persgroup <- ddply(perssubj, c("ParGroup","Person"), summarise,
                   pct2 = weighted.mean(pct),
                   sd   = sd(pct),
                   se   = sd / sqrt(length(pct)))

#by target
targsubj <- ddply(Comp, c("ParGroup","Target.1","Subj"), summarise,
                  N    = length(Accuracy),
                  acc  = sum(Accuracy),
                  pct = weighted.mean(Accuracy))

targgroup <- ddply(targsubj, c("ParGroup","Target.1"), summarise,
                   pct2 = weighted.mean(pct),
                   sd   = sd(pct),
                   se   = sd / sqrt(length(pct)))

#by item
itemsubj <- ddply(Comp, c("ParGroup","Target","Subj"), summarise,
                  N    = length(Accuracy),
                  acc  = sum(Accuracy),
                  pct = weighted.mean(Accuracy))

itemgroup <- ddply(itemsubj, c("ParGroup","Target"), summarise,
                   pct2 = weighted.mean(pct),
                   sd   = sd(pct),
                   se   = sd / sqrt(length(pct)))

#Graph by person
ggplot(persgroup, aes(x=ParGroup, y=pct2,color=ParGroup,fill=ParGroup)) +
  theme_set(theme_gray(base_size = 10)) +
  geom_bar(stat="identity") +
  facet_grid(. ~ Person) +
  xlab("Comprehension Task by Person") +
  ylab("Grand Means of % Correct") +
  scale_y_continuous(limits = c(0,1.05), labels=percent) + 
  geom_errorbar(aes(ymin=pct2-se, ymax=pct2+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9), color = "black") +
  geom_text(data=persgroup, aes(label=paste0(round(pct2*100,1),"%"),
                                y=(pct2+se)+0.04), size=3.4)

#Graph by target
targgroup$Target.1 = as.character(targgroup$Target.1)
targgroup$Target.1[targgroup$Target.1 == "E"] <- "1 E"
targgroup$Target.1[targgroup$Target.1 == "C"] <- "2 C"
targgroup$Target.1[targgroup$Target.1 == "B"] <- "3 B"
targgroup$Target.1[targgroup$Target.1 == "G"] <- "3 G"

ggplot(targgroup, aes(x=ParGroup, y=pct2,color=ParGroup,fill=ParGroup)) +
  theme_set(theme_gray(base_size = 10)) +
  geom_bar(stat="identity") +
  facet_grid(. ~ Target.1) +
  xlab("Comprehension Task by Target.1") +
  ylab("Grand Means of % Correct") +
  scale_y_continuous(limits = c(0,1.05), labels=percent) + 
  geom_errorbar(aes(ymin=pct2-se, ymax=pct2+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9), color = "black") +
  geom_text(data=targgroup, aes(label=paste0(round(pct2*100,1),"%"),
                                y=(pct2+se)+0.04), size=3.4)

#Graph by item
itemgroup$Target = as.character(itemgroup$Target)
itemgroup$Target[itemgroup$Target == "phi:"] <- "1E phi:"
itemgroup$Target[itemgroup$Target == "nO:N"] <- "2C nO:N"
itemgroup$Target[itemgroup$Target == "nu:"] <- "2C-F nu:"
itemgroup$Target[itemgroup$Target == "phom"] <- "2C-M phom"
itemgroup$Target[itemgroup$Target == "khun"] <- "2C khun"
itemgroup$Target[itemgroup$Target == "khaw"] <- "3B khaw"
itemgroup$Target[itemgroup$Target == "th7:"] <- "3G th7:"
itemgroup$Target[itemgroup$Target == "man"] <- "3M man"

itemgroup$Target = factor(itemgroup$Target, levels=c("1E phi:", "2C nO:N", "2C-F nu:", "2C-M phom", "2C khun", "3B khaw", "3G th7:", "3M man"))


compitem <- ggplot(itemgroup, aes(x=ParGroup, y=pct2,fill=ParGroup)) +
  theme_set(theme_gray(base_size = 12)) +
  geom_bar(stat="identity") +
  facet_grid(. ~ Target) +
  xlab("Comprehension Task by Item") +
  ylab("Grand Means of % Correct") +
  scale_fill_manual(values=c("#cccccc", "#575555")) +
  theme(legend.position="none") +
  scale_y_continuous(limits = c(0,1.05), labels=percent) + 
  geom_errorbar(aes(ymin=pct2-se, ymax=pct2+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9), color = "black") 
compitem


#--------------Study 1: Comp Error Analysis-------------
head(Comp)
# Comp.pct.error = filter(Comp, Accuracy==0) %>% group_by(Target, Answer, ParGroup, .drop = FALSE) %>%
#   summarise(count=n()) #%>%
# mutate(pct=count/sum(count(Comp))) %>%
# mutate(pos = cumsum(count) - (0.5 * count))

Comp.pct.error <- filter(Comp, Accuracy==0) %>% count(ParGroup, Target, Answer) %>% 
  complete(ParGroup, Target, Answer)
colnames(Comp.pct.error)[4] ="count"

Comp.count = count(filter(Comp) %>% group_by(ParGroup, Target))

Comp.pct.error = merge(Comp.pct.error, Comp.count, all = TRUE)
Comp.pct.error = Comp.pct.error %>% group_by(ParGroup, Target, Answer) %>%
  mutate(pct=count/n)

Comp.pct.error$pct[Comp.pct.error$Target == "nu:" & Comp.pct.error$ParGroup == "TD" & Comp.pct.error$Answer == "E"] <- 0
Comp.pct.error$pct[Comp.pct.error$Target == "phom" & Comp.pct.error$ParGroup == "TD" & Comp.pct.error$Answer == "E"] <- 0
Comp.pct.error$pct[Comp.pct.error$Target == "khun" & Comp.pct.error$ParGroup == "TD" & Comp.pct.error$Answer == "E"] <- 0
Comp.pct.error$pct[Comp.pct.error$Target == "nO:N" & Comp.pct.error$ParGroup == "TD" & Comp.pct.error$Answer == "E"] <- 0
Comp.pct.error$pct[Comp.pct.error$Target == "phi:" & Comp.pct.error$ParGroup == "ASD" & Comp.pct.error$Answer == "M"] <- 0
Comp.pct.error$pct[Comp.pct.error$Target == "phom" & Comp.pct.error$ParGroup == "ASD" & Comp.pct.error$Answer == "M"] <- 0

Comp.pct.error$Target = as.character(Comp.pct.error$Target)
Comp.pct.error$Target[Comp.pct.error$Target == "phi:"] <- "a) 1E phi:"
Comp.pct.error$Target[Comp.pct.error$Target == "nO:N"] <- "b) 2C nO:N"
Comp.pct.error$Target[Comp.pct.error$Target == "nu:"] <- "c) 2C-Fem nu:"
Comp.pct.error$Target[Comp.pct.error$Target == "phom"] <- "d) 2C-Male phom"
Comp.pct.error$Target[Comp.pct.error$Target == "khun"] <- "e) 2C khun"
Comp.pct.error$Target[Comp.pct.error$Target == "khaw"] <- "f) 3B khaw"
Comp.pct.error$Target[Comp.pct.error$Target == "th7:"] <- "g) 3G th7:"
Comp.pct.error$Target[Comp.pct.error$Target == "man"] <- "h) 3M man"

Comp.pct.error$Answer = as.character(Comp.pct.error$Answer)
Comp.pct.error$Answer = factor(Comp.pct.error$Answer, levels=c('E','C','B','G','M'))


r <- .5
t <- seq(0, 180, by = 1) * pi / 180
x <- r * cos(t)
y <- r*4 * sin(t)
y[20:162] <- y[20] # Flattens the arc

arc.df <- data.frame(ParGroup = x, pct = y)


ggplot(filter(Comp.pct.error,!is.na(pct)), aes(x=Answer, y=pct, pattern = ParGroup, fill = Answer)) +
  geom_bar(aes(fill = Answer), position = position_dodge(preserve = "single"), stat="identity") +
  theme_set(theme_gray(base_size = 15)) +
  geom_bar_pattern(aes(fill = Answer), 
                   position = position_dodge(preserve = "single"),
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6,
                   stat = "identity") +
  scale_pattern_manual(values=c('none','stripe')) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none"))) +
  facet_wrap(~Target, ncol = 2)  + #Put ", scales = "free"" inside the parenthesis if you want it to be different scale for each word but that might not be what we want.
  xlab("") +
  ylab("% Errors / Trials") +
  scale_y_continuous(labels=percent) +
  scale_fill_manual(values=c("#b37fa8", "#26c2b6", "#53a8c2", "#fd7975", "#f3d032")) +
  theme(strip.text = element_text(hjust = 0),
        strip.background = element_blank(),
        legend.position="bottom",
        plot.margin = unit(x = c(0, 0, 0, 0), units = "cm"), #reduce the bottom plot margin
        legend.margin=margin(t=-0.7, r=0, b=0, l=0, unit="cm")) + #reduce the legend box margin
  geom_text(data=Comp.pct.error, aes(x=Answer, label = paste0(round(pct*100,1),"%"), y = pct+0.03, group=ParGroup), 
            position=position_dodge(width=0.91), size=3)

#----------START Study 2----------

Study2Comp <- read.csv("./Study2Comp.csv", fileEncoding = 'UTF-8-BOM')
Study2Prod <- read.csv("./Study2Prod.csv", fileEncoding = 'UTF-8-BOM')
Study2Prod$Person <- Study2Prod$Target
Study2Prod$Person = as.character(Study2Prod$Person)
Study2Prod$Person[Study2Prod$Person == "E"] <- "2"
Study2Prod$Person[Study2Prod$Person == "C"] <- "1"
Study2Prod$Person[Study2Prod$Person == "B"] <- "3"
Study2Prod$Person[Study2Prod$Person == "G"] <- "3"
Study2Prod$Person[Study2Prod$Person == "M"] <- "3"
Study2Prod$AnsCat = as.character(Study2Prod$AnsCat)
Study2Prod$AnsCat[Study2Prod$AnsCat == "Occupational Title"] <- "Occu"
Study2Prod$AnsCat[Study2Prod$AnsCat == "Occu + Name"] <- "Occu"
Study2Prod$AnsCat[Study2Prod$AnsCat == "Kin + Name"] <- "Kin"
Study2Prod$AnsCat[Study2Prod$AnsCat == "Kin + Noun"] <- "Kin"
Study2Prod$AnsCat[Study2Prod$AnsCat == "Kin Term"] <- "Kin"
Study2Prod$AnsCat[Study2Prod$AnsCat == "Personal Name"] <- "Name"
Study2Prod$AnsCat[Study2Prod$AnsCat == "Personal Pronoun"] <- "PsPron"
Study2Prod$AnsCat[Study2Prod$AnsCat == "Reflexive Pronoun"] <- "RfxPron"



#-----------Study 2: Participant Info-------------------

Study2subjdata <- ddply(Study2Comp, c("ParGroup","Subj","Gender"), summarise,
                        N    = length(Accuracy),
                        acc  = sum(Accuracy),
                        pct = mean(Accuracy),
                        grade = mean(Grade),
                        age = mean(Age),
                        NVIQ = mean(SSIQ),
                        corsi = mean(CorsiMemorySpan)
)


#Participants with ASD over 10/11/12 years old
table(Study2subjdata$age>=10 & Study2subjdata$ParGroup== "ASD") #16
table(Study2subjdata$age>=11 & Study2subjdata$ParGroup== "ASD") #8
table(Study2subjdata$age>=12 & Study2subjdata$ParGroup== "ASD") #6

#Participants with TD over 10/11/12 years old
table(Study2subjdata$age>=10 & Study2subjdata$ParGroup== "TD") #13
table(Study2subjdata$age>=11 & Study2subjdata$ParGroup== "TD") #5
table(Study2subjdata$age>=12 & Study2subjdata$ParGroup== "TD") #0


Study2pardata <- ddply(Study2subjdata, c("ParGroup"), summarise,
                       grade.mean = mean(grade),
                       grade.sd = sd(grade),
                       grade.min = min(grade),
                       grade.max = max(grade),
                       age.mean = mean(age),
                       age.sd   = sd(age),
                       age.min = min(age),
                       age.max = max(age),
                       NVIQ.mean = mean(NVIQ),
                       NVIQ.sd = sd(NVIQ),
                       NVIQ.min = min(NVIQ),
                       NVIQ.max = max(NVIQ),
                       corsi.mean = mean(corsi),
                       corsi.sd = sd(corsi),
                       corsi.min = min(corsi),
                       corsi.max = max(corsi)
)

table(Study2subjdata$ParGroup,Study2subjdata$Gender)

summary(aov(age ~ ParGroup, Study2subjdata))
summary(aov(grade ~ ParGroup, Study2subjdata))
summary(aov(NVIQ ~ ParGroup, Study2subjdata))
summary(aov(corsi ~ ParGroup, Study2subjdata))

fisher.test(Study2subjdata$ParGroup, Study2subjdata$Gender)

cohen.d(Study2subjdata$age[Study2subjdata$ParGroup == "ASD"], Study2subjdata$age[Study2subjdata$ParGroup == "TD"])
cohen.d(Study2subjdata$grade[Study2subjdata$ParGroup == "ASD"], Study2subjdata$grade[Study2subjdata$ParGroup == "TD"])
cohen.d(Study2subjdata$NVIQ[Study2subjdata$ParGroup == "ASD"], Study2subjdata$NVIQ[Study2subjdata$ParGroup == "TD"])
cohen.d(Study2subjdata$corsi[Study2subjdata$ParGroup == "ASD"], Study2subjdata$corsi[Study2subjdata$ParGroup == "TD"])

#Remove the children who scored less than 50% or withdraw from answering in the production task.
Study2Prod<-Study2Prod[!(Study2Prod$Subj==7),]
Study2Prod<-Study2Prod[!(Study2Prod$Subj==8),]
Study2Prod<-Study2Prod[!(Study2Prod$Subj==9),]
Study2Prod<-Study2Prod[!(Study2Prod$Subj==22),]
Study2Comp<-Study2Comp[!(Study2Comp$Subj==7),]
Study2Comp<-Study2Comp[!(Study2Comp$Subj==8),]
Study2Comp<-Study2Comp[!(Study2Comp$Subj==9),]
Study2Comp<-Study2Comp[!(Study2Comp$Subj==22),]


#--------Study 2: Statistics-------------
ProdASD2 = filter(Study2Prod, ParGroup=="ASD")
ProdTD2 = filter(Study2Prod, ParGroup=="TD")
CompASD2 = filter(Study2Comp, ParGroup=="ASD")
CompTD2 = filter(Study2Comp, ParGroup=="TD")

summary(aov(Accuracy ~ ParGroup, Study2Prod))
# Df Sum Sq Mean Sq F value Pr(>F)  
# ParGroup      1  0.064 0.06441   5.126 0.0239 *
# Residuals   702  8.821 0.01256 
cohen.d(ProdASD2$Accuracy, ProdTD2$Accuracy)
# d estimate: -0.1832023 (negligible)
# 95 percent confidence interval:
#   lower       upper 
# -0.34236023 -0.02404434 

summary(aov(Accuracy ~ ParGroup, Study2Comp))
# Df Sum Sq Mean Sq F value Pr(>F)    
# ParGroup       1    2.8  2.7788   12.04 0.00053 ***
#   Residuals   2110  486.8  0.2307
cohen.d(CompASD2$Accuracy, CompTD2$Accuracy)
# d estimate: -0.1621406 (negligible)
# 95 percent confidence interval:
#   lower      upper 
# -0.2538886 -0.0703926 

# (a)
summary(aov(Accuracy ~ ParGroup,  filter(Study2Comp, Word=="kha:")))
# Df Sum Sq Mean Sq F value Pr(>F)
# ParGroup      1   0.35  0.3516   1.563  0.212
# Residuals   350  78.74  0.2250 
cohen.d(CompASD2$Accuracy[CompASD2$Word=="kha:"], CompTD2$Accuracy[CompTD2$Word=="kha:"])
# d estimate: -0.1430652 (negligible)
# 95 percent confidence interval:
#   lower       upper 
# -0.36838048  0.08225018 

# (b)
summary(aov(Accuracy ~ ParGroup,  filter(Study2Comp, Word=="phi:")))
# Df Sum Sq Mean Sq F value   Pr(>F)    
# ParGroup      1   1.07  1.0715   9.457 0.00227 **
#   Residuals   350  39.65  0.1133  
cohen.d(CompASD2$Accuracy[CompASD2$Word=="phi:"], CompTD2$Accuracy[CompTD2$Word=="phi:"])
# d estimate: -0.3519167 (small)
# 95 percent confidence interval:
#   lower      upper 
# -0.5784890 -0.1253444 

# (c)
summary(aov(Accuracy ~ ParGroup,  filter(Study2Comp, Word=="nu:")))
# Df Sum Sq Mean Sq F value Pr(>F)
# ParGroup     1  0.122 0.12175   1.484  0.229
# Residuals   54  4.432 0.08207  
cohen.d(CompASD2$Accuracy[CompASD2$Word=="nu:"], CompTD2$Accuracy[CompTD2$Word=="nu:"])
# d estimate: 0.3966644 (small)
# 95 percent confidence interval:
#   lower      upper 
# -0.2605736  1.0539024 

# (d)
summary(aov(Accuracy ~ ParGroup,  filter(Study2Comp, Word=="phom")))
# Df Sum Sq Mean Sq F value Pr(>F)  
# ParGroup      1  0.353  0.3535   3.323 0.0693 .
# Residuals   294 31.268  0.1064     
cohen.d(CompASD2$Accuracy[CompASD2$Word=="phom"], CompTD2$Accuracy[CompTD2$Word=="phom"])
# d estimate: -0.2240317 (small)
# 95 percent confidence interval:
#   lower       upper 
# -0.46656590  0.01850255 

# (e)
summary(aov(Accuracy ~ ParGroup,  filter(Study2Comp, Word=="khun")))
# Df Sum Sq Mean Sq F value Pr(>F)  
# ParGroup      1   0.89  0.8864   4.017 0.0458 *
#   Residuals   350  77.22  0.2206 
cohen.d(CompASD2$Accuracy[CompASD2$Word=="khun"], CompTD2$Accuracy[CompTD2$Word=="khun"])
# d estimate: 0.2293644 (small)
# 95 percent confidence interval:
#   lower       upper 
# 0.003657535 0.455071286 

# (f)
summary(aov(Accuracy ~ ParGroup,  filter(Study2Comp, Word=="khaw")))
# Df Sum Sq Mean Sq F value Pr(>F)
# ParGroup      1   0.56  0.5610    2.26  0.134
# Residuals   350  86.88  0.2482    
cohen.d(CompASD2$Accuracy[CompASD2$Word=="khaw"], CompTD2$Accuracy[CompTD2$Word=="khaw"])
# d estimate: -0.172037 (negligible)
# 95 percent confidence interval:
#   lower       upper 
# -0.39746365  0.05338957 

# (g)
summary(aov(Accuracy ~ ParGroup,  filter(Study2Comp, Word=="th7:")))
# Df Sum Sq Mean Sq F value   Pr(>F)    
# ParGroup      1   4.82   4.818   21.01 6.37e-06 ***
#   Residuals   350  80.27   0.229    
cohen.d(CompASD2$Accuracy[CompASD2$Word=="th7:"], CompTD2$Accuracy[CompTD2$Word=="th7:"])
# d estimate: -0.5245096 (medium)
# 95 percent confidence interval:
#   lower      upper 
# -0.7529087 -0.2961106 


#-----------Study 2: Overall Accuracy-------------------

Study2cdata <- ddply(Study2subjdata, c("ParGroup"), summarise,
                     pct2 = weighted.mean(pct),
                     sd   = sd(pct),
                     se   = sd / sqrt(length(pct)))

Study2psubjdata <- ddply(Study2Prod, c("ParGroup","Subj","Gender"), summarise,
                         N    = length(Accuracy),
                         acc  = sum(Accuracy),
                         pct = mean(Accuracy))

Study2pdata <- ddply(Study2psubjdata, c("ParGroup"), summarise,
                     pct2 = weighted.mean(pct),
                     sd   = sd(pct),
                     se   = sd / sqrt(length(pct)))

Study2cdata$test <- "Comprehension"
Study2pdata$test <- "Production"
Study2pct <- merge(Study2pdata,Study2cdata,all=TRUE) #or rbind
Study2pct$test = factor(Study2pct$test, levels=c('Production','Comprehension')) 


ggplot(Study2pct, aes(x=ParGroup, y=pct2,fill=ParGroup)) +
  theme_set(theme_gray(base_size = 16)) +
  geom_bar(stat="identity") +
  facet_grid(. ~ test) +
  xlab("") +
  ylab("Grand Means of % Correct") +
  geom_signif(data = filter(pct, test=="Production"),
              y_position = c(1.02), xmin = c(1), xmax = c(2),
              annotation = c("*"), tip_length = 0) +
  geom_signif(data = filter(pct, test=="Comprehension"),
              y_position = c(0.75), xmin = c(1), xmax = c(2),
              annotation = c("***"), tip_length = 0) +
  scale_fill_manual(values=c("#cccccc", "#575555")) +
  theme(legend.position="none",
        plot.margin = unit(x = c(0, 0, 0, 0), units = "cm")) +
  scale_y_continuous(limits = c(0,1.03), labels=percent) +
  geom_errorbar(aes(ymin=pct2-se, ymax=pct2+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9), color = "black") 


#Accuracy rates without kha:
Study2subjdatanokhaa <- ddply(Study2Comp[!(Study2Comp$Word=="kha:"),], c("ParGroup","Subj"), summarise,
                        N    = length(Accuracy),
                        acc  = sum(Accuracy),
                        pct = mean(Accuracy)
)

Study2cdatanokhaa <- ddply(Study2subjdatanokhaa, c("ParGroup"), summarise,
                     pct2 = weighted.mean(pct),
                     sd   = sd(pct),
                     se   = sd / sqrt(length(pct)))

#------Study 2: Production - Overall + by Participant -------------
# Summarize to get counts and percentages
Prod.pct.term2 = filter(Study2Prod, !Accuracy==0) %>% group_by(ParGroup, Person, AnsCat) %>%
  summarise(count=n()) %>%
  mutate(pct=count/sum(count))
#Only the accurate trials

#1st person - comparing counts of personal names and personal pronouns  between ASD and TD
Study2namepron1 = matrix(c(30,25,21,92), nrow = 2)
fisher.test(Study2namepron1)
# data:  Study2namepron1
# p-value = 4.679e-06
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
#   2.431024 11.391421
# sample estimates:
#   odds ratio 
# 5.196 

#2nd person - comparing counts of kin terms and occupational titles  between ASD and TD
Study2kinoccu2 = matrix(c(31,19,63,53), nrow = 2)
fisher.test(Study2kinoccu2)
# data:  Study2kinoccu2
# p-value = 0.3966
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
#   0.6629573 2.8816279
# sample estimates:
#   odds ratio 
# 1.369989 

#3rd person - comparing counts of nouns and all the other choices between ASD and TD
Study2noun3 = matrix(c(97,11,225,13), nrow = 2)
fisher.test(Study2noun3)
# data:  Study2noun3
# p-value = 0.1154
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
#   0.2029479 1.3068583
# sample estimates:
#   odds ratio 
# 0.5105648 

ggplot(filter(Prod.pct.term2, Person==1, AnsCat=="Name" | AnsCat=="PsPron"), aes(x=ParGroup, y=pct,fill=ParGroup)) +
  theme_set(theme_gray(base_size = 16)) +
  geom_bar(stat="identity") +
  facet_grid(. ~ AnsCat) +
  xlab("1st Person (C) - Production Task") +
  ylab("% Correct") +
  scale_fill_manual(values=c("#cccccc", "#575555")) +
  theme(legend.position="none",
        plot.margin = unit(x = c(0, 0, 0, 0), units = "cm")) +
  scale_y_continuous(limits = c(0,1), labels=percent)  


s2prod1 <- ggplot(filter(Prod.pct.term2, Person==1), aes(x=ParGroup, y=pct,fill=ParGroup)) +
  geom_bar(stat="identity") +
  facet_grid(. ~ reorder(AnsCat, -pct)) +
  xlab("(a) 1st Person (C) - Production Task") +
  ylab("% Correct") +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0,1), labels=percent) + 
  scale_fill_manual(values=c("#cccccc", "#575555")) +
  geom_text(data=filter(Prod.pct.term2, Person==1), aes(label=paste0(round(pct*100,1),"%"),
                                                        y=pct+0.04), size=3)

s2prod2 <- ggplot(filter(Prod.pct.term2, Person==2), aes(x=ParGroup, y=pct, fill=ParGroup)) +
  geom_bar(stat="identity") +
  facet_grid(. ~ reorder(AnsCat, -pct)) +
  xlab("(b) 2nd Person (E) - Production Task") +
  ylab("% Correct") +
  theme(legend.position = "none") +
  scale_fill_manual(values=c("#cccccc", "#575555")) +
  scale_y_continuous(limits = c(0,1), labels=percent) + 
  geom_text(data=filter(Prod.pct.term2, Person==2), aes(label=paste0(round(pct*100,1),"%"),
                                                        y=pct+0.04), size=3)

s2prod3 <- ggplot(filter(Prod.pct.term2, Person==3), aes(x=ParGroup, y=pct, fill=ParGroup)) +
  geom_bar(stat="identity") +
  facet_grid(. ~ reorder(AnsCat, -pct)) +
  xlab("(c) 3rd Person (B/G) - Production Task") +
  ylab("% Correct") +
  theme(legend.position = "none") +
  scale_fill_manual(values=c("#cccccc", "#575555")) +
  scale_y_continuous(limits = c(0,1.03), labels=percent) + 
  geom_text(data=filter(Prod.pct.term2, Person==3), aes(label=paste0(round(pct*100,1),"%"),
                                                        y=pct+0.04), size=3)

s2prod <- s2prod1 + s2prod2 + s2prod3 + plot_layout(nrow = 3)
s2prod

#by participant
# Summarize to get counts and percentages -- Individual participant results
Prod.pct.term.indiv2 = filter(Study2Prod, !Accuracy==0) %>% group_by(Person, ParGroup, Subj, AnsCat) %>%
  summarise(count=n()) %>%
  mutate(pct=count/sum(count))
#Only the accurate trials

#ASD
s2p1 <- ggplot(filter(Prod.pct.term.indiv2, Person==1 & ParGroup=="ASD"), aes(x=AnsCat, y=pct, fill=AnsCat)) +
  geom_bar(stat="identity", width = 0.5, position=position_dodge(0.1)) +
  facet_grid(Subj ~ .) +
  xlab("1st Person (C)") +
  ylab("") +
  scale_fill_manual(values=c("#ce4993", "#ee5d6c", "#fb9062")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,1.5)) +
  theme(text = element_text(size = 12),
        panel.grid = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
        panel.border = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  guides(fill=guide_legend(title="1st AnsCat")) +
  geom_text(data=filter(Prod.pct.term.indiv2, Person==1 & ParGroup=="ASD"), aes(label=paste0(round(pct*100,1),"%"),
                                                                                y=pct+0.3), size=2)


s2p2 <- ggplot(filter(Prod.pct.term.indiv2, Person==2 & ParGroup=="ASD"), aes(x=AnsCat, y=pct, fill=AnsCat)) +
  geom_bar(stat="identity", width = 0.5, position=position_dodge(0.1)) +
  facet_grid(Subj ~ .) +
  xlab("2nd Person (E)") +
  ylab("") +
  scale_fill_manual(values=c("#6a0d83",  "#950495", "#ff8080", "#fcd509")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,1.5)) +
  theme(text = element_text(size = 12),
        panel.grid = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
        panel.border = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  guides(fill=guide_legend(title="2nd AnsCat")) +
  geom_text(data=filter(Prod.pct.term.indiv2, Person==2 & ParGroup=="ASD"), aes(label=paste0(round(pct*100,2),"%"),
                                                                                y=pct+0.3), size=2)

s2p3 <- ggplot(filter(Prod.pct.term.indiv2, Person==3 & ParGroup=="ASD"), aes(x=AnsCat, y=pct, fill=AnsCat)) +
  geom_bar(stat="identity", width = 0.5, position=position_dodge(0.1)) +
  facet_grid(Subj ~ .) +
  xlab("3rd Person (B/G)") +
  ylab("") +
  scale_fill_manual(values=c("#6a0d83", "#950495", "#ee5d6c")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,1.5)) +
  theme(text = element_text(size = 12),
        panel.grid = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
        panel.border = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  guides(fill=guide_legend(title="3rd AnsCat")) +
  geom_text(data=filter(Prod.pct.term.indiv2, Person==3 & ParGroup=="ASD"), aes(label=paste0(round(pct*100,2),"%"),
                                                                                y=pct+0.3), size=2)

ASDindiv2 <- s2p1 + s2p2 + s2p3 + plot_layout(ncol = 3, guides = "collect")
ASDindiv2



#TD
s2p4 <- ggplot(filter(Prod.pct.term.indiv2, Person==1 & ParGroup=="TD" & Subj < 68), aes(x=AnsCat, y=pct, fill=AnsCat)) +
  geom_bar(stat="identity", width = 0.4, position=position_dodge(0.1)) +
  facet_grid(Subj ~ .) +
  xlab("1st Person (C)") +
  ylab("") +
  scale_fill_manual(values=c("#ce4993", "#ee5d6c")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,1.5)) +
  theme(text = element_text(size = 12),
        panel.grid = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
        panel.border = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  guides(fill=guide_legend(title="1st AnsCat")) +
  geom_text(data=filter(Prod.pct.term.indiv2, Person==1 & ParGroup=="TD" & Subj < 68), aes(label=paste0(round(pct*100,1),"%"),
                                                                                           y=pct+0.3), size=2)

s2p5 <- ggplot(filter(Prod.pct.term.indiv2, Person==2 & ParGroup=="TD" & Subj < 68), aes(x=AnsCat, y=pct, fill=AnsCat)) +
  geom_bar(stat="identity", width = 0.4, position=position_dodge(0.1)) +
  facet_grid(Subj ~ .) +
  xlab("2nd Person (E)") +
  ylab("") +
  scale_fill_manual(values=c("#6a0d83", "#ff8080")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,1.5)) +
  theme(text = element_text(size = 12),
        panel.grid = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
        panel.border = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  guides(fill=guide_legend(title="2nd AnsCat")) +
  geom_text(data=filter(Prod.pct.term.indiv2, Person==2 & ParGroup=="TD" & Subj < 68), aes(label=paste0(round(pct*100,2),"%"),
                                                                                           y=pct+0.3), size=2)

s2p6 <- ggplot(filter(Prod.pct.term.indiv2, Person==3 & ParGroup=="TD" & Subj < 68), aes(x=AnsCat, y=pct, fill=AnsCat)) +
  geom_bar(stat="identity", width = 0.4, position=position_dodge(0.1)) +
  facet_grid(Subj ~ .) +
  xlab("3rd Person (B/G)") +
  ylab("") +
  scale_fill_manual(values=c("#950495", "#ee5d6c")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,1.5)) +
  theme(text = element_text(size = 12),
        panel.grid = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
        panel.border = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  guides(fill=guide_legend(title="3rd AnsCat")) +
  geom_text(data=filter(Prod.pct.term.indiv2, Person==3 & ParGroup=="TD" & Subj < 68), aes(label=paste0(round(pct*100,2),"%"),
                                                                                           y=pct+0.3), size=2)

s2TDindiv1 <- s2p4 + s2p5 + s2p6 + plot_layout(ncol = 3, guides = "collect")
s2TDindiv1

s2p7 <- ggplot(filter(Prod.pct.term.indiv2, Person==1 & ParGroup=="TD" & Subj > 68), aes(x=AnsCat, y=pct, fill=AnsCat)) +
  geom_bar(stat="identity", width = 0.7, position=position_dodge(0.1)) +
  facet_grid(Subj ~ .) +
  xlab("1st Person (C)") +
  ylab("") +
  scale_fill_manual(values=c("#ce4993", "#950495", "#ee5d6c", "#fb9062")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,1.5)) +
  theme(text = element_text(size = 12),
        panel.grid = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
        panel.border = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  guides(fill=guide_legend(title="1st AnsCat")) +
  geom_text(data=filter(Prod.pct.term.indiv2, Person==1 & ParGroup=="TD" & Subj > 68), aes(label=paste0(round(pct*100,1),"%"),
                                                                                           y=pct+0.3), size=2)

s2p8 <- ggplot(filter(Prod.pct.term.indiv2, Person==2 & ParGroup=="TD" & Subj > 68), aes(x=AnsCat, y=pct, fill=AnsCat)) +
  geom_bar(stat="identity", width = 0.7, position=position_dodge(0.1)) +
  facet_grid(Subj ~ .) +
  xlab("2nd Person (E)") +
  ylab("") +
  scale_fill_manual(values=c("#6a0d83",  "#950495", "#ff8080", "#fcd509")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,1.5)) +
  theme(text = element_text(size = 12),
        panel.grid = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
        panel.border = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  guides(fill=guide_legend(title="2nd AnsCat")) +
  geom_text(data=filter(Prod.pct.term.indiv2, Person==2 & ParGroup=="TD" & Subj > 68), aes(label=paste0(round(pct*100,2),"%"),
                                                                                           y=pct+0.3), size=2)

s2p9 <- ggplot(filter(Prod.pct.term.indiv2, Person==3 & ParGroup=="TD" & Subj > 68), aes(x=AnsCat, y=pct, fill=AnsCat)) +
  geom_bar(stat="identity", width = 0.35, position=position_dodge(0.1)) +
  facet_grid(Subj ~ .) +
  xlab("3rd Person (B/G)") +
  ylab("") +
  scale_fill_manual(values=c("#6a0d83", "#950495", "#ee5d6c")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,1.5)) +
  theme(text = element_text(size = 12),
        panel.grid = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
        panel.border = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  guides(fill=guide_legend(title="3rd AnsCat")) +
  geom_text(data=filter(Prod.pct.term.indiv2, Person==3 & ParGroup=="TD" & Subj > 68), aes(label=paste0(round(pct*100,2),"%"),
                                                                                           y=pct+0.3), size=2)

s2TDindiv2 <- s2p7 + s2p8 + s2p9 + plot_layout(ncol = 3, guides = "collect")
s2TDindiv2

#--------------Study 2: Comp Accuracy by Item----------------

# Comprehension
# Summarize to get counts and percentages
#by person
perssubj2 <- ddply(Study2Comp, c("ParGroup","Person","Subj"), summarise,
                   N    = length(Accuracy),
                   acc  = sum(Accuracy),
                   pct = weighted.mean(Accuracy))

persgroup2 <- ddply(perssubj2, c("ParGroup","Person"), summarise,
                    pct2 = weighted.mean(pct),
                    sd   = sd(pct),
                    se   = sd / sqrt(length(pct)))

#by target
targsubj2 <- ddply(Study2Comp, c("ParGroup","Target","Subj"), summarise,
                   N    = length(Accuracy),
                   acc  = sum(Accuracy),
                   pct = weighted.mean(Accuracy))

targgroup2 <- ddply(targsubj2, c("ParGroup","Target"), summarise,
                    pct2 = weighted.mean(pct),
                    sd   = sd(pct),
                    se   = sd / sqrt(length(pct)))

#by item
itemsubj2 <- ddply(Study2Comp, c("ParGroup","Word","Subj"), summarise,
                   N    = length(Accuracy),
                   acc  = sum(Accuracy),
                   pct = weighted.mean(Accuracy))

itemgroup2 <- ddply(itemsubj2, c("ParGroup","Word"), summarise,
                    pct2 = weighted.mean(pct),
                    sd   = sd(pct),
                    se   = sd / sqrt(length(pct)))

#Graph by person
ggplot(persgroup2, aes(x=ParGroup, y=pct2,color=ParGroup,fill=ParGroup)) +
  theme_set(theme_gray(base_size = 10)) +
  geom_bar(stat="identity") +
  facet_grid(. ~ Person) +
  xlab("Comprehension Task by Person") +
  ylab("Grand Means of % Correct") +
  scale_y_continuous(limits = c(0,1.05), labels=percent) + 
  geom_errorbar(aes(ymin=pct2-se, ymax=pct2+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9), color = "black") +
  geom_text(data=persgroup2, aes(label=paste0(round(pct2*100,1),"%"),
                                 y=(pct2+se)+0.04), size=3.4)

#Graph by Word
targgroup2$Target = as.character(targgroup2$Target)
targgroup2$Target[targgroup2$Target == "E"] <- "1 E"
targgroup2$Target[targgroup2$Target == "C"] <- "2 C"
targgroup2$Target[targgroup2$Target == "B"] <- "3 B"
targgroup2$Target[targgroup2$Target == "G"] <- "3 G"

ggplot(targgroup2, aes(x=ParGroup, y=pct2,color=ParGroup,fill=ParGroup)) +
  theme_set(theme_gray(base_size = 10)) +
  geom_bar(stat="identity") +
  facet_grid(. ~ Target) +
  xlab("Comprehension Task by Target") +
  ylab("Grand Means of % Correct") +
  scale_y_continuous(limits = c(0,1.05), labels=percent) + 
  geom_errorbar(aes(ymin=pct2-se, ymax=pct2+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9), color = "black") +
  geom_text(data=targgroup2, aes(label=paste0(round(pct2*100,1),"%"),
                                 y=(pct2+se)+0.04), size=3.4)

#Graph by item
itemgroup2$Word = as.character(itemgroup2$Word)
itemgroup2$Word[itemgroup2$Word == "phi:"] <- "1E phi:"
itemgroup2$Word[itemgroup2$Word == "kha:"] <- "1E kha:"
itemgroup2$Word[itemgroup2$Word == "nu:"] <- "2C-F nu:"
itemgroup2$Word[itemgroup2$Word == "phom"] <- "2C-M phom"
itemgroup2$Word[itemgroup2$Word == "khun"] <- "2C khun"
itemgroup2$Word[itemgroup2$Word == "khaw"] <- "3B khaw"
itemgroup2$Word[itemgroup2$Word == "th7:"] <- "3G th7:"


itemgroup2$Word = factor(itemgroup2$Word, levels=c("1E kha:", "1E phi:", "2C-F nu:", "2C-M phom", "2C khun", "3B khaw", "3G th7:"))


compitem2 <- ggplot(itemgroup2, aes(x=ParGroup, y=pct2,fill=ParGroup)) +
  theme_set(theme_gray(base_size = 12)) +
  geom_bar(stat="identity") +
  facet_grid(. ~ Word) +
  xlab("Comprehension Task by Item") +
  ylab("Grand Means of % Correct") +
  scale_fill_manual(values=c("#cccccc", "#575555")) +
  theme(legend.position="none") +
  scale_y_continuous(limits = c(0,1.05), labels=percent) + 
  geom_errorbar(aes(ymin=pct2-se, ymax=pct2+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9), color = "black") 
compitem2


#--------------Study 2: Comp Error Analysis-------------
head(Comp)

Study2Comp.pct.error <- filter(Study2Comp, Accuracy==0) %>% count(ParGroup, Word, Answer) %>% 
  complete(ParGroup, Word, Answer)
colnames(Study2Comp.pct.error)[4] ="count"

Comp.count2 = count(filter(Study2Comp) %>% group_by(ParGroup, Word))

Study2Comp.pct.error = merge(Study2Comp.pct.error, Comp.count2, all = TRUE)
Study2Comp.pct.error = Study2Comp.pct.error %>% group_by(ParGroup, Word, Answer) %>%
  mutate(pct=count/n)

Study2Comp.pct.error$pct[Study2Comp.pct.error$Word == "nu:" & Study2Comp.pct.error$ParGroup == "ASD" & Study2Comp.pct.error$Answer == "B"] <- 0
Study2Comp.pct.error$pct[Study2Comp.pct.error$Word == "nu:" & Study2Comp.pct.error$ParGroup == "ASD" & Study2Comp.pct.error$Answer == "G"] <- 0

Study2Comp.pct.error$Word = as.character(Study2Comp.pct.error$Word)
Study2Comp.pct.error$Word[Study2Comp.pct.error$Word == "kha:"] <- "a) 1E kha:"
Study2Comp.pct.error$Word[Study2Comp.pct.error$Word == "phi:"] <- "b) 1E phi:"
Study2Comp.pct.error$Word[Study2Comp.pct.error$Word == "nu:"] <- "c) 2C-Fem nu:"
Study2Comp.pct.error$Word[Study2Comp.pct.error$Word == "phom"] <- "d) 2C-Male phom"
Study2Comp.pct.error$Word[Study2Comp.pct.error$Word == "khun"] <- "e) 2C khun"
Study2Comp.pct.error$Word[Study2Comp.pct.error$Word == "khaw"] <- "f) 3B khaw"
Study2Comp.pct.error$Word[Study2Comp.pct.error$Word == "th7:"] <- "g) 3G th7:"

Study2Comp.pct.error$Answer = as.character(Study2Comp.pct.error$Answer)
Study2Comp.pct.error$Answer = factor(Study2Comp.pct.error$Answer, levels=c('E','C','B','G','M'))

r <- .5
t <- seq(0, 180, by = 1) * pi / 180
x <- r * cos(t)
y <- r*4 * sin(t)
y[20:162] <- y[20] # Flattens the arc

arc.df <- data.frame(ParGroup = x, pct = y)


ggplot(filter(Study2Comp.pct.error,!is.na(pct)), aes(x=Answer, y=pct, pattern = ParGroup, fill = Answer)) +
  geom_bar(aes(fill = Answer), position = position_dodge(preserve = "single"), stat="identity") +
  theme_set(theme_gray(base_size = 15)) +
  geom_bar_pattern(aes(fill = Answer), 
                   position = position_dodge(preserve = "single"),
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6,
                   stat = "identity") +
  scale_pattern_manual(values=c('none','stripe')) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none"))) +
  facet_wrap(~Word, ncol = 2)  + #Put ", scales = "free"" inside the parenthesis if you want it to be different scale for each word but that might not be what we want.
  xlab("") +
  ylab("% Errors / Trials") +
  scale_y_continuous(labels=percent) +
  scale_fill_manual(values=c("#b37fa8", "#26c2b6", "#53a8c2", "#fd7975", "#f3d032")) +
  theme(strip.text = element_text(hjust = 0),
        strip.background = element_blank(),
        legend.position="bottom",
        plot.margin = unit(x = c(0, 0, 0, 0), units = "cm"), #reduce the bottom plot margin
        legend.margin=margin(t=-0.7, r=0, b=0, l=0, unit="cm")) + #reduce the legend box margin
  geom_text(data=Study2Comp.pct.error, aes(x=Answer, label = paste0(round(pct*100,1),"%"), y = pct+0.03, group=ParGroup), 
            position=position_dodge(width=0.91), size=3)

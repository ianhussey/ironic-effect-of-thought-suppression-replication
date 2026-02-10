#R codes for RRR: Wegner & Erber (1992) & Wegner et al., (1987)
#Authors: Adam Wang, Martin Hagger, Kai Qin Chan

#R codes by Kai Qin Chan
#Test data by Kai Qin Chan
#James Cook University

#18 Oct 2022.

#The below codes comprises two major segments:
#1. R codes for analysis of single-lab data
#2. R codes for meta analysis of multi-lab data


###****DATA ANALYSIS FOR SINGLE LABS***###

#Set working directory (file directory specific to KQC's PC)
setwd("C:/Users/jc506932/Dropbox/RRR (Adam Wang)") #laptop
setwd("C:/Users/chank/Dropbox/RRR (Adam Wang)") #desktop

###For Wegner & Erber (1992)

#Import datafile.
library(readxl)
RRR_WE92 <- read_excel("RRR_Hyperaccesibility.xlsx", sheet = "testdata_W&E'92_singlelab") #note: test data has hidden columns of RTs. RTs were not of interested in W&E'92 but may still be captured.
View(RRR_WE92)

#Define categorical variables
RRR_WE92$Gender.participant <- factor(RRR_WE92$Gender.participant, levels = c("Male", "Female", "Other", "Prefer not to disclose"))
RRR_WE92$Gender.experimenter <- factor(RRR_WE92$Gender.experimenter, levels = c("Male", "Female", "Other", "Prefer not to disclose"))

#To test whether the manipulation (concentration vs. suppression) produced different count outcomes in the manipulation check variable.
chisq.test(RRR_WE92$Cond, RRR_WE92$Manipulationcheck, correct=FALSE)

#Selecting data that passed manipulation checks. ##NOT RUN## (as specified by Editor)
RRR_WE92 <- subset(RRR_WE92, Manipulationcheck=="1") #1 = passed manipulation checks.

#Intra-class correlation coefficients (correct responses only)
library(psych)
ICCdata1 <- data.frame(cbind(RRR_WE92$correctresponse.rater1.target.low1, RRR_WE92$correctresponse.rater2.target.low1))
ICC(ICCdata1)

ICCdata2 <- data.frame(cbind(RRR_WE92$correctresponse.rater1.target.high1, RRR_WE92$correctresponse.rater2.target.high1))
ICC(ICCdata2)

ICCdata3 <- data.frame(cbind(RRR_WE92$correctresponse.rater1.nontarget.low1, RRR_WE92$correctresponse.rater2.nontarget.low1))
ICC(ICCdata3)

ICCdata4 <- data.frame(cbind(RRR_WE92$correctresponse.rater1.nontarget.high1, RRR_WE92$correctresponse.rater2.nontarget.high1))
ICC(ICCdata4)

ICCdata5 <- data.frame(cbind(RRR_WE92$correctresponse.rater1.target.low2, RRR_WE92$correctresponse.rater2.target.low2))
ICC(ICCdata5)

ICCdata6 <- data.frame(cbind(RRR_WE92$correctresponse.rater1.target.high2, RRR_WE92$correctresponse.rater2.target.high2))
ICC(ICCdata6)

ICCdata7 <- data.frame(cbind(RRR_WE92$correctresponse.rater1.nontarget.low2, RRR_WE92$correctresponse.rater2.nontarget.low2))
ICC(ICCdata7)

ICCdata8 <- data.frame(cbind(RRR_WE92$correctresponse.rater1.nontarget.high2, RRR_WE92$correctresponse.rater2.nontarget.high2))
ICC(ICCdata8)

#Convert wide format to long format.
#This is needed for the mixed ANOVA analyses.

library(reshape2)
library(magrittr)
library(dplyr)

data_long<-melt(RRR_WE92, id.vars=c("SubjID", "Cond", "Cond_num", "Replication", "targetword"), 
                measure.vars=c("mean.correctresponse.r1r2.target.low", "mean.correctresponse.r1r2.target.high", "mean.correctresponse.r1r2.nontarget.low", "mean.correctresponse.r1r2.nontarget.high"),
                variable.name="condition",
                value.name="meancorrectresponse") %>% arrange(SubjID)

#Create columns that define (split) the within-subject factors, i.e., time pressure and word type.

data_long$timepressure<-gl(2, 1, labels = c("Low", "High")) #define the factor and levels of the factor
data_long$wordtype<-gl(2, 2, labels = c("Target", "Nontarget")) #define the factor and levels of the factor

data_long$Cond<-factor(data_long$Cond,labels = c("Supression", "Concentration")) 
data_long$Cond_num<-factor(data_long$Cond_num,labels = c("Concentration", "Supression")) #note the swap
data_long$Replication<-factor(data_long$Replication,labels = c("1", "2", "3", "4")) 
data_long$timepressure<-factor(data_long$timepressure,labels = c("Low", "High")) 
data_long$wordtype<-factor(data_long$wordtype,labels = c("target", "nontarget")) 
data_long$SubjID<-factor(data_long$SubjID) #It doesn't seem to be necessary. Not sure.

#Packages needed for ANOVAs.
library(afex); library(emmeans)

#Run 2 x 2 x 2 ANOVA (as reported by E&W 1992)
mainresult<-aov_ez(id = "SubjID", dv = "meancorrectresponse", data=data_long, 
               between = "Cond", within = c("timepressure", "wordtype"))

summary(mainresult)
emmeans(mainresult, pairwise ~ Cond|timepressure, adjust = "none") 
emmeans(mainresult, pairwise ~ timepressure|Cond, adjust = "none") 

#The results are expected to be significant only for the target words. 
#So, we need to restrict subsequent analyses to target words only.

#Remove rows with nontarget words, if needed
long2 <- data_long[!(data_long$wordtype=="nontarget"),]

#Rerun ANOVA, but now as a 2 x 2 ANOVA.
result2<-aov_ez(id = "SubjID", dv = "meancorrectresponse", data=long2, 
                   between = "Cond", within = c("timepressure"))

summary(result2)
emmeans(result2, pairwise ~ Cond|timepressure, adjust = "none") 
emmeans(result2, pairwise ~ timepressure|Cond, adjust = "none") 

#Plot 2 x 2
emmip(result2, Cond~timepressure, xlab = "Condition", ylab = "Mean correct response", CIs = T) 

#UPDATE: Jun 2022 (as requested by Editor)
#This update specifies the four target words as random factor.
#Incidentally, the variable name for the four target words is called 'replication'. This word 'replication' is also used in the orignial Wegner & Erber article.
#Resource: For specifying the random effect, see ourcodingclub.github.io/tutorials/mixed-models/


m1 <- lmer(meancorrectresponse ~ Cond*timepressure*Replication + (1|SubjID) + (1|Replication), long2) 
#Corrected in Oct 22: Participant is a random intercept; changed from glmer to lmer.
#Note also that the dataframe is long2 (nontarget words removed), not data_long.
summary(m1)

#Update: Oct 2022 (as requested by Editor)
  #The following codes is meant to retrieve the beta and standard error of the three-way interaction (Cond x Time pressure x Word Type), so that a subsequent multi-lab meta-analysis of this three-way interaction term (which requires the point estimate and the variance) can be computed.
  #Because the design has two within-subject factors (word type: target vs. nontarget words; time pressure: high vs. low), within a regression, this must be modeled as a MLM. 
  #Condition (suppression vs. expression) is a between-subjects factor, hence this is not an issue.

m2 <- lmer(meancorrectresponse ~ Cond*timepressure*wordtype + (1|SubjID), data_long) #The factor, Replications, is omitted. It was not required by the Editor.
summary(m2)

###For Wegner et al. (1987)
library(readxl)
RRR_Wegner87 <- read_excel("RRR_Hyperaccesibility.xlsx", sheet = "testdata_Wegner'87_singlelab")
View(RRR_Wegner87)

#Define categorical variables
RRR_Wegner87$Gender.participant <- factor(RRR_Wegner87$Gender.participant, levels = c("Male", "Female", "Other", "Prefer not to disclose"))
RRR_Wegner87$Gender.experimenter <- factor(RRR_Wegner87$Gender.experimenter, levels = c("Male", "Female", "Other", "Prefer not to disclose"))

#To test whether the manipulation (concentration vs. suppression) produced different count outcomes in the manipulation check variable.
#Beware: No cell count should be < 5.
chisq.test(RRR_Wegner87$Cond, RRR_Wegner87$Manipulationcheck, correct=FALSE)

#Selecting data that passed manipulation checks. ##NOT RUN## (as specified by Editor)
RRR_Wegner87 <- subset(RRR_Wegner87, Manipulationcheck=="1") #1 = passed manipulation checks.

#Intra-class correlation coefficients (for mentions only)
library(psych)
ICCdata1 <- data.frame(cbind(RRR_Wegner87$totalmention.rater1.period1, RRR_Wegner87$totalmention.rater2.period1))
ICC(ICCdata1)

ICCdata2 <- data.frame(cbind(RRR_Wegner87$count.bellonly.rater1.period1, RRR_Wegner87$count.bellonly.rater2.period1))
ICC(ICCdata2)

ICCdata3 <- data.frame(cbind(RRR_Wegner87$count.mentiononly.rater1.period1, RRR_Wegner87$count.mentiononly.rater2.period1))
ICC(ICCdata3)

ICCdata4 <- data.frame(cbind(RRR_Wegner87$count.simulresponse.rater1.period1, RRR_Wegner87$count.simulresponse.rater2.period1))
ICC(ICCdata4)

#Packages needed for ANOVAs.
library(afex); library(emmeans)

#Main analyses
mainresult<-aov_ez(id = "SubjID", dv = "finalDV.period2", data=RRR_Wegner87, 
                   between = c("Cond", "targetword")) #note: As written in the RRR, "For the experiments testing the rebound effect, the dependent variable was the number of ‘food-related’ or ‘elephant-related’ thought occurrences during the second phase of the study where all participants were asked to express their thoughts freely."  That's why Period is not a within-subjects factor.

summary(mainresult)

#RUN IF interaction is significant and/or main effect of target word is found.
emmeans(mainresult, pairwise ~ Cond|targetword, adjust = "none") 
emmeans(mainresult, pairwise ~ targetword|Cond, adjust = "none")

#Plot 2 x 2. #RUN IF interaction is significant and/or main effect of target word is found.
emmip(mainresult, Cond~targetword, xlab = "Target Word", ylab = "Mean no. of thought intrusions", CIs = T) 

#RUN IF interaction is nonsignificant. Collapse across target word.
mainresult2<-aov_ez(id = "SubjID", dv = "finalDV.period2", data=RRR_Wegner87, 
                   between = c("Cond"))
summary(mainresult2)

#Convert wide format to long format (RRR_Wegner87)
#This is needed for the MLM analyses. Note that this uses two DVs (period 1 and period 2). 
#In the RRR, we only need the period 2 DV.

library(reshape2)
library(magrittr)
library(dplyr)

data_long<-melt(RRR_Wegner87, id.vars=c("SubjID", "Cond", "Cond_num", "Replication", "targetword"), 
                measure.vars=c("finalDV.period1", "finalDV.period2"),
                variable.name="Period",
                value.name="intrusions") %>% arrange(SubjID)

#Define factors in the long format data.
data_long$Cond<-factor(data_long$Cond,labels = c("Supression-first", "Concentration-first")) 
data_long$Cond_num<-factor(data_long$Cond_num,labels = c("Concentration-first", "Supression-first")) #note the swap
data_long$Period<-factor(data_long$Period,labels = c("1", "2"))
data_long$Replication<-factor(data_long$Replication,labels = c("1", "2")) #Unlike RRR_WE92, there are only two replications for RRR_Wegner87. Note: replication is the same as targetword, except represented as numeric variable, instead of text.
data_long$SubjID<-factor(data_long$SubjID) #It doesn't seem to be necessary. Not sure.

#Update: Oct 2022 (as requested by Editor)
  #The following codes is meant to retrieve the beta and standard error of the three-way interaction (Cond x Time pressure x Word Type), so that a subsequent multi-lab meta-analysis of this three-way interaction term (which requires the point estimate and the variance) can be computed.
  #Because the design has two within-subject factors (word type: target vs. nontarget words; time pressure: high vs. low), within a regression, this must be modeled as a MLM. 
  #Condition (supression vs. expression) is a between-subjects factor, hence this is not an issue.

  #See: https://stats.stackexchange.com/questions/396262/regression-within-and-between-subject-variable-interaction

#Assuming we need DV from Periods 1 and 2:
m1 <- lmer(intrusions ~ Cond*Period*Replication + (1|SubjID), data_long) #Random intercept of participants
summary(m1) 

#Assuming we need DV from Period 2 only:
data_long2<-data_long[!(data_long$Period=="1"),]

m1.mod <- lmer(intrusions ~ Cond*Replication + (1|Replication), data_long2) #Random intercept of target words
summary(m1.mod) 

#From Wegner et al. (1987): "The mean of the summed thought measures during expression was 19.22, and this was significantly greater than the suppression mean of 6.78, F(l, 32) = 41.0 l, p < .0001"
#If F = 41, then t = 6.4. Note: df = 32. (n = 17 per group)
#Using formula for independent samples t-test, pooled SD = 5.667.
#Using formula for pooled SD, and assuming homogeneity of variance, standard deviation per group is SQRT(5.667) = 2.3806


#Alternatively, if one assumes that Replication (targetwords) is not a variable of interest, then RUNIF:
m2 <- lmer(intrusions ~ Cond*Period + (1|SubjID), data_long) #Random intercept only. Note:  It is unlikely that a model with random intercept and random slope will converge, but we can try it: (1 + timepressure | ID) + (1 + wordtype | ID)
summary(m2) 

#RUNIF Replication is treated as a random effect (i.e., participants are nested within target word):
m3 <- lmer(intrusions ~ Cond*Period*Replication + (1|Replication:SubjID), data_long) #Random intercept only. Note:  It is unlikely that a model with random intercept and random slope will converge, but we can try it: (1 + timepressure | ID) + (1 + wordtype | ID)
summary(m3) 

###****META ANALYSIS***###
#The meta-analysis codes are for both Wegner et al. (1987) and Wegner & Erber (1992).

library(metafor)
library(readxl)

RRR_metaWE92 <- read_excel("C:/Users/jc506932/Dropbox/RRR (Adam Wang)/RRR_Hyperaccesibility.xlsx", sheet = "testdata_W&E'92_multilab")
View(RRR_metaWE92)

#d1: Enhancement effect, contrast between concentration and suppression under low load
RRR_metaWE92.effectsize.d1 <- escalc(n2i = N.conc.low, n1i = N.supp.low, 
                  m2i = M.conc.low, m1i = M.supp.low, 
                  sd2i = SD.conc.low, sd1i = SD.supp.low, data = RRR_metaWE92, measure = "SMD", 
                  append = TRUE) #Let op!  The effect size calculation is M1-M2, not M2-M1
overalld1 <- rma(yi, vi, data = RRR_metaWE92.effectsize.d1)
summary(overalld1) #These summary data will need to be saved manually in an Excel sheet, which the below forest plot codes will use.

#d2: Enhancement effect, contrast between concentration and suppression under high load
RRR_metaWE92.effectsize.d2 <- escalc(n2i = N.conc.high, n1i = N.supp.high, 
                                  m2i = M.conc.high, m1i = M.supp.high, 
                                  sd2i = SD.conc.high, sd1i = SD.supp.high, data = RRR_metaWE92, measure = "SMD", 
                                  append = TRUE) #Let op!  The effect size calculation is M1-M2, not M2-M1
overalld2 <- rma(yi, vi, data = RRR_metaWE92.effectsize.d2)
summary(overalld2) 

#d3: Rebound effect
library(readxl)

RRR_metaWegner87 <- read_excel("C:/Users/jc506932/Dropbox/RRR (Adam Wang)/RRR_Hyperaccesibility.xlsx", sheet = "testdata_Wegner'87_multilab")
View(RRR_metaWegner87)

RRR_metaWegner87.effectsize.d3 <- escalc(n2i = N.conc, n1i = N.supp, 
                                     m2i = M.conc, m1i = M.supp, 
                                     sd2i = SD.conc, sd1i = SD.supp, data = RRR_metaWegner87, measure = "SMD", 
                                     append = TRUE) #Let op!  The effect size calculation is M1-M2, not M2-M1
overalld3 <- rma(yi, vi, data = RRR_metaWegner87.effectsize.d3)
summary(overalld3) 

###****FOREST PLOT***###
#Resources consulted
#https://www.r-bloggers.com/2016/07/forest-plot-with-horizontal-bands/
#https://stackoverflow.com/questions/64595210/can-we-color-the-different-rows-covariates-studies-in-different-colors-in-r-fore

library(readxl)
library(forestplot)

data <- read_excel("RRR_Hyperaccesibility.xlsx", sheet = "forestplot")
View(data)

subgps <- c(6,7,8,11,12,13,16,17,18,21,22,23,26,27,28,31,32,33, 36, 37, 38, 41, 42, 43) #OPTIONAL: RUN IF you want to indent d1, d2, d3 after the author's name.
data$Lab[subgps] <- paste("  ",data$Lab[subgps]) 

## The rest of the columns in the table. #OPTIONAL: RUN IF you want to indent d1, d2, d3 after the author's name.
tabletext <- cbind(c("Lab (Country)","\n",data$Lab), 
                   c("N","\n",data$N))

#OPTIONAL: RUN IF you want to indent d1, d2, d3 after the author's name.
styles <- fpShapesGp(
  lines = list(
    gpar(col = "black"), #Null
    gpar(col = "black"), #Null
    gpar(col = "green"), #Overall d1
    gpar(col = "green"),
    gpar(col = "green"),
    gpar(col = "black"), #Null
    gpar(col = "black"), #Null
    gpar(col = "black"),  #first d1
    gpar(col = "blue"),
    gpar(col = "red"),
    gpar(col = "white"), #Null
    gpar(col = "white"), #Null
    gpar(col = "black"), #second d1
    gpar(col = "blue"),
    gpar(col = "red"),
    gpar(col = "white"), #Null
    gpar(col = "white"), #Null
    gpar(col = "black"),
    gpar(col = "blue"),
    gpar(col = "red"),
    gpar(col = "white"), #Null
    gpar(col = "white"), #Null
    gpar(col = "black"),
    gpar(col = "blue"),
    gpar(col = "red"),
    gpar(col = "white"), #Null
    gpar(col = "white"), #Null
    gpar(col = "black"),
    gpar(col = "blue"),
    gpar(col = "red"),
    gpar(col = "white"), #Null
    gpar(col = "white"), #Null
    gpar(col = "black"),
    gpar(col = "blue"),
    gpar(col = "red"),
    gpar(col = "white"), #Null
    gpar(col = "white"), #Null
    gpar(col = "black"),
    gpar(col = "blue"),
    gpar(col = "red"),
    gpar(col = "white"), #Null
    gpar(col = "white"), #Null
    gpar(col = "black"),
    gpar(col = "blue"),
    gpar(col = "red")
  ),
  box = list(
    gpar(fill = "black"), #Null
    gpar(fill = "black"), #Null
    gpar(fill = "green"), #Overall d1
    gpar(fill = "green"),
    gpar(fill = "green"),
    gpar(fill = "black"), #Null
    gpar(fill = "black"), #Null
    gpar(fill = "black"),  #first d1
    gpar(fill = "blue"),
    gpar(fill = "red"),
    gpar(fill = "white"), #Null
    gpar(fill = "white"), #Null
    gpar(fill = "black"), #second d1
    gpar(fill = "blue"),
    gpar(fill = "red"),
    gpar(fill = "white"), #Null
    gpar(fill = "white"), #Null
    gpar(fill = "black"),
    gpar(fill = "blue"),
    gpar(fill = "red"),
    gpar(fill = "white"), #Null
    gpar(fill = "white"), #Null
    gpar(fill = "black"),
    gpar(fill = "blue"),
    gpar(fill = "red"),
    gpar(fill = "white"), #Null
    gpar(fill = "white"), #Null
    gpar(fill = "black"),
    gpar(fill = "blue"),
    gpar(fill = "red"),
    gpar(fill = "white"), #Null
    gpar(fill = "white"), #Null
    gpar(fill = "black"),
    gpar(fill = "blue"),
    gpar(fill = "red"),
    gpar(fill = "white"), #Null
    gpar(fill = "white"), #Null
    gpar(fill = "black"),
    gpar(fill = "blue"),
    gpar(fill = "red"),
    gpar(fill = "white"), #Null
    gpar(fill = "white"), #Null
    gpar(fill = "black"),
    gpar(fill = "blue"),
    gpar(fill = "red")
  ) 
)

forestplot(labeltext=tabletext, graph.pos=3, 
           mean=c(NA,NA,data$Point), 
           lower=c(NA,NA,data$Low), upper=c(NA,NA,data$High),
           txt_gp=fpTxtGp(label=gpar(cex=1.25),
                          ticks=gpar(cex=1.1),
                          xlab=gpar(cex = 1.2),
                          title=gpar(cex = 1.2)),
           shapes_gp = styles,
           zero=0, cex=0.9, lineheight = unit(5, "mm"), boxsize=0.5, colgap=unit(6,"mm"),
           lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.4)

#Note: The indentations made to the subgps command will not run well if you amend any of the subgps after running it once.
#To see the changes, you will have to reopen the data and start from beginning.


####NEW: JUNE 2022###
###INTENTION: SPLIT d1 and d2 from d3, as requested by Editor###

#Below is for d1 and d2 (immediate enhancement effect under high and low time pressure, respectively)
library(readxl)
library(forestplot)

data <- read_excel("RRR_Hyperaccesibility.xlsx", sheet = "forestplotd1d2") #note new sheet name
View(data)

subgps <- c(6,7,11,12,16,17,21,22,26,27,31,32) #this is optional; this only needs to be tweaked if you want the d1 and d2 to be indented. If not, there is no need to run this code.
data$Lab[subgps] <- paste("  ",data$Lab[subgps]) #this is optional; if d1 and d2 doesn't need to be indented, then no need to run this.

## The rest of the columns in the table. 
tabletext <- cbind(c("Lab (Country)","\n",data$Lab), 
                   c("N","\n",data$N)) #\n means new line


styles <- fpShapesGp(
  lines = list(
    gpar(col = "black"), #Null
    gpar(col = "black"), #Null
    gpar(col = "green"), #Overall d1
    gpar(col = "green"), #Overall d2
    gpar(col = "black"), #Null
    gpar(col = "black"), 
    gpar(col = "black"), #first d1 
    gpar(col = "blue"),  #first d2
    gpar(col = "white"), #Null
    gpar(col = "white"), #Null
    gpar(col = "black"), #second d1
    gpar(col = "blue"),  #second d2
    gpar(col = "white"), #Null
    gpar(col = "white"), #Null
    gpar(col = "black"), #third d1
    gpar(col = "blue"),  #third d2
    gpar(col = "white"), #Null
    gpar(col = "white"), #Null
    gpar(col = "black"),
    gpar(col = "blue"),
    gpar(col = "white"), #Null
    gpar(col = "white"), #Null
    gpar(col = "black"),
    gpar(col = "blue"),
    gpar(col = "white"), #Null
    gpar(col = "white"), #Null
    gpar(col = "black"),
    gpar(col = "blue"),
    gpar(col = "white"), #Null
    gpar(col = "white"), #Null
    gpar(col = "black"),
    gpar(col = "blue"),
    gpar(col = "white"), #Null
    gpar(col = "white"), #Null
    gpar(col = "black"),
    gpar(col = "blue")
  ),
  box = list(
    gpar(fill = "black"), #Null
    gpar(fill = "black"), #Null
    gpar(fill = "green"), #Overall d1
    gpar(fill = "green"),
    gpar(fill = "black"), #Null
    gpar(fill = "black"), #Null
    gpar(fill = "black"),  #first d1
    gpar(fill = "blue"),
    gpar(fill = "white"), #Null
    gpar(fill = "white"), #Null
    gpar(fill = "black"), #second d1
    gpar(fill = "blue"),
    gpar(fill = "white"), #Null
    gpar(fill = "white"), #Null
    gpar(fill = "black"),
    gpar(fill = "blue"),
    gpar(fill = "white"), #Null
    gpar(fill = "white"), #Null
    gpar(fill = "black"),
    gpar(fill = "blue"),
    gpar(fill = "white"), #Null
    gpar(fill = "white"), #Null
    gpar(fill = "black"),
    gpar(fill = "blue"),
    gpar(fill = "white"), #Null
    gpar(fill = "white"), #Null
    gpar(fill = "black"),
    gpar(fill = "blue"),
    gpar(fill = "white"), #Null
    gpar(fill = "white"), #Null
    gpar(fill = "black"),
    gpar(fill = "blue"),
    gpar(fill = "white"), #Null
    gpar(fill = "white"), #Null
    gpar(fill = "black"),
    gpar(fill = "blue")
  ) 
)

forestplot(labeltext=tabletext, graph.pos=3, 
           mean=c(NA,NA,data$Point), 
           lower=c(NA,NA,data$Low), upper=c(NA,NA,data$High),
           txt_gp=fpTxtGp(label=gpar(cex=1.25),
                          ticks=gpar(cex=1.1),
                          xlab=gpar(cex = 1.2),
                          title=gpar(cex = 1.2)),
           shapes_gp = styles,
           zero=0, cex=0.9, lineheight = unit(5, "mm"), boxsize=0.5, colgap=unit(6,"mm"),
           lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.4)

#note: If plot is too big for screen, save it as pdf then screenshot to Word.

#The forest plot below is only for d3 (rebound effect), as requested by Editor.
library(readxl)
library(forestplot)

data <- read_excel("RRR_Hyperaccesibility.xlsx", sheet = "forestplotd3") #note new sheet name
View(data)

## The rest of the columns in the table. 
tabletext <- cbind(c("Lab (Country)","\n",data$Lab), 
                   c("N","\n",data$N)) #\n means new line

forestplot(labeltext=tabletext, graph.pos=3, 
           mean=c(NA,NA,data$Point), 
           lower=c(NA,NA,data$Low), upper=c(NA,NA,data$High),
           zero=0, cex=0.9, lineheight = unit(5, "mm"), boxsize=0.5, colgap=unit(6,"mm"),
           lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.4)

#Updated: Oct 2022 (as requested by Editor)
  #Editor requested a meta-analysis of the three-way interaction between Condition, Time Pressure, and Word Type.

#3-way interaction (Condition x Time Pressure x Word Type)
library(meta) 
summary(intWE92) <- metagen(TE= threeway.beta.int_term, seTE = threeway.SE.int_term, data = RRR_metaWE92)
summary(intWegner87) <- metagen(TE= threeway.beta.int_term, seTE = threeway.SE.int_term, data = RRR_metaWegner87)
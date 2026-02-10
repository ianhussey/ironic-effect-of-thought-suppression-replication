#R codes for RRR: Wegner & Erber (1992)
#Lead authors: Adam Wang, Martin Hagger, Kai Qin Chan

#R codes by Kai Qin Chan
#James Cook University, Singapore

#Based on R codes previously approved by AAMPS editor in Oct 2022

#Current version: Dec 2023.

#The below codes comprises two major segments:
#1. R codes for analysis of single-lab data
#2. R codes for meta analysis of multi-lab data

#Resources consulted
# https://r-workshop.mindsci.net/anova --- for effect size

####****DATA ANALYSIS FOR SINGLE LABS***####
####Study 1: Replicating Wegner & Erber (1992)

#Set working directory (file directory specific to KQC's PC)
setwd("C:/Users/jc506932/Dropbox/RRR (Adam Wang)/Data") #laptop
setwd("C:/Users/chank/Dropbox/RRR (Adam Wang)/Data") #desktop

####Load libraries####
library(readxl)
library(psych)
library(reshape2)
library(magrittr)
library(dplyr)
library(afex)
library(emmeans)

#Import only one datafile at any one time; for analysis of any single lab
RRR_WE92 <- read_excel("Quek_NUS_Study1_cleaned.xlsx", sheet = "cleaned") #National University of Singapore
RRR_WE92 <- read_excel("Wang_HHU_Study1_cleaned.xlsx", sheet = "cleaned") #Hohai University
#[INSERT MORE FILENAMES AS DATA COMES IN]

#Create list of Excel datafiles and temp dataframes within working directory
file_list = list.files(pattern="*.xlsx") #get all data with .xlsx
data_list <- vector("list", "length" = length(file_list)) #Create list outside loop
df1 <- data.frame(LabID=NA, author=NA, 
                  N.all = NA, N.male = NA, N.female = NA, 
                  N.conc = NA, N.supp = NA,
                  ICC = NA, mean.age = NA, sd.age = NA,
                  mean.conc.3s = NA,
                  mean.supp.3s = NA,
                  mean.conc.10s = NA,
                  mean.supp.10s = NA,
                  sd.conc.3s = NA,
                  sd.supp.3s = NA,
                  sd.conc.10s = NA,
                  sd.supp.10s = NA,
                  ES.cond_TP_wordtype = NA,
                  ES.cond_wordtype = NA,
                  ES.cond_TP = NA,
                  ES.cond = NA, 
                  ES.TP = NA,
                  ES.cond_TP.nontarget = NA
)

####LOOP: START####
for (i in seq_along(file_list)) {
  filename = file_list[[i]]
  
  RRR_WE92 <- read_excel(filename, sheet = "cleaned") 
  
#Rename variable names
names(RRR_WE92)[names(RRR_WE92) == "Participant ID"] <- "SubjID"
names(RRR_WE92)[names(RRR_WE92) == "Time pressure order"] <- "TimePressureOrder"
names(RRR_WE92)[names(RRR_WE92) == "Target word"] <- "Replication"
names(RRR_WE92)[names(RRR_WE92) == "Condition"] <- "Cond"

#Define categorical variables and its levels
RRR_WE92$gender <- factor(RRR_WE92$gender)
levels(RRR_WE92$gender) <- c("Female", "Male", "Other", "Prefer not to say")

RRR_WE92$MC <- factor(RRR_WE92$MC)
levels(RRR_WE92$MC) <- c("Think", "Not think")

RRR_WE92$Cond <- factor(RRR_WE92$Cond)
levels(RRR_WE92$Cond) <- c("Suppression", "Concentration")

RRR_WE92$Replication <- factor(RRR_WE92$Replication)
levels(RRR_WE92$Replication) <- c("mountain", "car", "child", "house")

RRR_WE92$TimePressureOrder <- factor(RRR_WE92$TimePressureOrder)
levels(RRR_WE92$TimePressureOrder) <- c("10s then 3s", "2s then 10s")

#Define who failed manipulation check
#RRR_WE92$MC.passfail <- NA
#RRR_WE92$MC.passfail[RRR_WE92$MC == 1 & RRR_WE92$Cond == 2 ] <- "pass"
#RRR_WE92$MC.passfail[RRR_WE92$MC == 2 & RRR_WE92$Cond == 1 ] <- "pass"
#RRR_WE92$MC.passfail[RRR_WE92$MC == 1 & RRR_WE92$Cond == 1 ] <- "fail"
#RRR_WE92$MC.passfail[RRR_WE92$MC == 2 & RRR_WE92$Cond == 2 ] <- "fail"

#Manipulation check (MC): To test whether the manipulation (concentration vs. suppression) produced different count outcomes in the manipulation check variable.
#table(RRR_WE92$MC.passfail) #retrieve number who passed or failed
#chisq.test(RRR_WE92$Cond, RRR_WE92$MC.passfail, correct=FALSE) #will lead to error if no one failed

#Selecting data that passed manipulation checks. ##NOT RUN## (as specified by Editor)
#RRR_WE92 <- subset(RRR_WE92, MC.passfail=="pass") #1 = passed manipulation checks.

#Intra-class correlation coefficients (correct responses only)
ICC.target.low1 <- data.frame(cbind(RRR_WE92$target.low1.coder1, RRR_WE92$target.low1.coder2))
ICC(ICC.target.low1)

ICC.target.low2 <- data.frame(cbind(RRR_WE92$target.low2.coder1, RRR_WE92$target.low2.coder2))
ICC(ICC.target.low2)

ICC.target.high1 <- data.frame(cbind(RRR_WE92$target.high1.coder1, RRR_WE92$target.high1.coder2))
ICC(ICC.target.high1)

ICC.target.high2 <- data.frame(cbind(RRR_WE92$target.high2.coder1, RRR_WE92$target.high2.coder2))
ICC(ICC.target.high2)

ICC.nontarget.low1 <- data.frame(cbind(RRR_WE92$nontarget.low1.coder1, RRR_WE92$nontarget.low1.coder2))
ICC(ICC.nontarget.low1)

ICC.nontarget.low2 <- data.frame(cbind(RRR_WE92$nontarget.low2.coder1, RRR_WE92$nontarget.low2.coder2))
ICC(ICC.nontarget.low2)

ICC.nontarget.high1 <- data.frame(cbind(RRR_WE92$nontarget.high1.coder1, RRR_WE92$nontarget.high1.coder2))
ICC(ICC.nontarget.high1)

ICC.nontarget.high2 <- data.frame(cbind(RRR_WE92$nontarget.high2.coder1, RRR_WE92$nontarget.high2.coder2))
ICC(ICC.nontarget.high2)

#Compute mean ICC for the DVs
meanICC <- mean(c(ICC(ICC.target.low1)$results["Average_raters_absolute", "ICC"],  #Retrieve only the ICC for average rater absolute
       ICC(ICC.target.low2)$results["Average_raters_absolute", "ICC"],
       ICC(ICC.target.high1)$results["Average_raters_absolute", "ICC"],
       ICC(ICC.target.high2)$results["Average_raters_absolute", "ICC"],
       ICC(ICC.nontarget.low1)$results["Average_raters_absolute", "ICC"],
       ICC(ICC.nontarget.low2)$results["Average_raters_absolute", "ICC"],
       ICC(ICC.nontarget.high1)$results["Average_raters_absolute", "ICC"], 
       ICC(ICC.nontarget.high2)$results["Average_raters_absolute", "ICC"]))

#Convert wide format to long format for the mixed ANOVA analyses.
data_long<-melt(RRR_WE92, id.vars=c("SubjID", "Cond", "Replication", "TimePressureOrder"), 
                measure.vars=c("mean.target.low", "mean.target.high", "mean.nontarget.low", "mean.nontarget.high"),
                variable.name="condition",
                value.name="meanhit") %>% arrange("SubjID")

#Create columns that define (split) the within-subject factors, i.e., time pressure and word type.
data_long$timepressure <- NA #create new variable to hold the values of time pressure
data_long$timepressure[data_long$condition == "mean.target.low" | data_long$condition == "mean.nontarget.low" ] <- 1
data_long$timepressure[data_long$condition == "mean.target.high" | data_long$condition == "mean.nontarget.high" ] <- 2

data_long$wordtype <- NA #create new variable to hold the values of target
data_long$wordtype[data_long$condition == "mean.target.low" | data_long$condition == "mean.target.high" ] <- 1
data_long$wordtype[data_long$condition == "mean.nontarget.low" | data_long$condition == "mean.nontarget.high" ] <- 2

#Define factors and levels of the newly-made variables
data_long$timepressure<-factor(data_long$timepressure,labels = c("Low", "High")) 
data_long$wordtype<-factor(data_long$wordtype,labels = c("Target", "Nontarget")) 

#Run 2 x 2 x 2 ANOVA (as reported by E&W 1992)
mainresult<-aov_ez(id = "SubjID", dv = "meanhit", data=data_long, 
               between = "Cond", within = c("timepressure", "wordtype"), anova_table = list(correction = "none", es = "pes"))

summary(mainresult)
emmeans(mainresult, pairwise ~ Cond|timepressure, adjust = "none") 
emmeans(mainresult, pairwise ~ timepressure|Cond, adjust = "none") 
emmip(mainresult, Cond~wordtype, xlab = "Word Type", ylab = "Mean prompt-relevant associations", CIs = T) 
mainresult_ES <- anova(object=mainresult, es = "pes") #get partial eta-sq

#The results are expected to be significant only for the target words. 
#So, we need to restrict subsequent analyses to target words only.

#Remove rows with nontarget words, if needed
long2 <- data_long[!(data_long$wordtype=="nontarget"),]

#Rerun ANOVA, but now as a 2 x 2 ANOVA.
result2<-aov_ez(id = "SubjID", dv = "meanhit", data=long2, 
                   between = "Cond", within = c("timepressure"), anova_table = list(correction = "none", es = "pes"))

summary(result2)
emmeans(result2, pairwise ~ Cond|timepressure, adjust = "none") 
emmeans(result2, pairwise ~ timepressure|Cond, adjust = "none") 
results2_ES <- anova(object=result2, es = "pes") #get partial eta-sq

#Plot 2 x 2
emmip(result2, Cond~timepressure, xlab = "Time Pressure", ylab = "Mean prompt-relevant associations", CIs = T) 
emmip(result2, timepressure~Cond, xlab = "Condition", ylab = "Mean prompt-relevant associations", CIs = T) 

#Basic info
df1$LabID=NA
df1$author=NA
df1$mean.age = mean(RRR_WE92$age)
df1$sd.age = sd(RRR_WE92$age)
df1$N.all = nrow(RRR_WE92)
df1$N.male = as.numeric(table(RRR_WE92$gender))[2]
df1$N.female = as.numeric(table(RRR_WE92$gender))[1]
df1$N.conc = length(subset(RRR_WE92, Cond == "Concentration")) 
df1$N.supp = length(subset(RRR_WE92, Cond == "Suppression"))
df1$ICC = meanICC

#Extract relevant means and SDs for meta-analysis; Nontarget words removed (To FIX: Why are the 2nd and 3rd means/SDs identical?)
df1$mean.conc.3s = mean(long2[long2$Cond == 'Concentration' & long2$timepressure == 'High' & long2$wordtype == 'Target', 'meanhit'])
df1$mean.supp.3s = mean(long2[long2$Cond == 'Suppression' & long2$timepressure == 'High' & long2$wordtype == 'Target', 'meanhit'])
df1$mean.conc.10s = mean(long2[long2$Cond == 'Concentration' & long2$timepressure == 'Low' & long2$wordtype == 'Target', 'meanhit'])
df1$mean.supp.10s = mean(long2[long2$Cond == 'Suppression' & long2$timepressure == 'Low' & long2$wordtype == 'Target', 'meanhit'])
df1$sd.conc.3s = sd(long2[long2$Cond == 'Concentration' & long2$timepressure == 'High' & long2$wordtype == 'Target', 'meanhit'])
df1$sd.supp.3s = sd(long2[long2$Cond == 'Suppression' & long2$timepressure == 'High' & long2$wordtype == 'Target', 'meanhit'])
df1$sd.conc.10s = sd(long2[long2$Cond == 'Concentration' & long2$timepressure == 'Low' & long2$wordtype == 'Target', 'meanhit'])
df1$sd.supp.10s = sd(long2[long2$Cond == 'Suppression' & long2$timepressure == 'Low' & long2$wordtype == 'Target', 'meanhit'])

#Effect size (partial eta sq)
  #All words
df1$ES.cond_TP_wordtype = mainresult_ES["Cond:timepressure:wordtype", "pes"]
df1$ES.cond_wordtype = mainresult_ES["Cond:wordtype", "pes"]
df1$ES.cond_TP = mainresult_ES["Cond:timepressure", "pes"]

  #Effect size (partial eta sq) after montarget words removed
df1$ES.cond = results2_ES["Cond", "pes"] #nontarget words removed
df1$ES.TP = results2_ES["timepressure", "pes"] #nontarget words removed
df1$ES.cond_TP.nontarget = results2_ES["Cond:timepressure", "pes"] #nontarget words removed

#Add info to dataframe
data_list[[i]] <- df1
}

####LOOP: END####

####Stitch data together####
df1 <- do.call(rbind, data_list)
write.csv(df1, "C:/Users/jc506932/Dropbox/RRR (Adam Wang)/RRR_WE92_meta.csv")

####Other optional codes####
describeBy(long2$meanhit, group = long2$Cond:long2$timepressure)

#UPDATE: Jun 2022 (as requested by Editor)
#This update specifies the four target words as random factor.
#Incidentally, the variable name for the four target words is called 'replication'. This word 'replication' is also used in the orignial Wegner & Erber article.
#Resource: For specifying the random effect, see ourcodingclub.github.io/tutorials/mixed-models/

####Mixed models####
m1 <- lmer(meanhit ~ Cond*timepressure*Replication + (1|SubjID) + (1|Replication), long2) 
#Corrected in Oct 22: Participant is a random intercept; changed from glmer to lmer.
#Note also that the dataframe is long2 (nontarget words removed), not data_long.
summary(m1)

#Update: Oct 2022 (as requested by Editor)
  #The following codes are meant to retrieve the beta and standard error of the three-way interaction (Cond x Time pressure x Word Type), so that a subsequent multi-lab meta-analysis of this three-way interaction term (which requires the point estimate and the variance) can be computed.
  #Because the design has two within-subject factors (word type: target vs. nontarget words; time pressure: high vs. low), within a regression, this must be modeled as a MLM. 
  #Condition (suppression vs. expression) is a between-subjects factor, hence this is not an issue.

m2 <- lmer(meanhit ~ Cond*timepressure*wordtype + (1|SubjID), data_long) #The factor, Replications, is omitted. It was not required by the Editor.
summary(m2)

####ANALYST'S NOTES####
#Wang (HHH) had two cell means and SDs that are completely identical. Further checks determined that this is entirely plausible by chance because the number of hits are very low,  the number of possible answers is small (three for any trial), and the distribution of the correct responses are random. Conclusion: Repetition is due to chance not coding error nor fraud.

####****META ANALYSIS***####
#The meta-analysis codes is for Wegner & Erber (1992).

#Load libraries
library(metafor)
library(readr)

#Load data
RRR_WE92_meta <- read_csv("C:/Users/jc506932/Dropbox/RRR (Adam Wang)/RRR_WE92_meta.csv")

#Calculate d1: Enhancement effect, contrast between concentration and suppression under low load
RRR_metaWE92.effectsize.d1 <- escalc(n2i = N.conc, 
                                     n1i = N.supp,
                                     m2i = mean.conc.10s, 
                                     m1i = mean.supp.10s, 
                                     sd2i = sd.conc.10s, 
                                     sd1i = sd.supp.10s, 
                                     data = RRR_WE92_meta, measure = "SMD", append = TRUE) #Let op!  The effect size calculation is M1-M2, not M2-M1
overalld1 <- rma(yi, vi, data = RRR_metaWE92.effectsize.d1)
summary(overalld1) #These summary data will need to be saved manually in an Excel sheet, which the below forest plot codes will use.

#Calculate d2: Enhancement effect, contrast between concentration and suppression under high load
RRR_metaWE92.effectsize.d2 <- escalc(n2i = N.conc, 
                                     n1i = N.supp, 
                                     m2i = mean.conc.3s, 
                                     m1i = mean.supp.3s, 
                                     sd2i = sd.conc.3s, 
                                     sd1i = sd.supp.3s, 
                                     data = RRR_WE92_meta, measure = "SMD", append = TRUE) #Let op!  The effect size calculation is M1-M2, not M2-M1
overalld2 <- rma(yi, vi, data = RRR_metaWE92.effectsize.d2)
summary(overalld2) 

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
#R codes for RRR: Wegner et al. (1987)
#Lead authors: Deming (Adam) Wang, Kai Qin Chan, Wakefield Morys-Carter, and Martin Hagger

#R codes by Kai Qin Chan
#James Cook University, Singapore

#Based on R codes previously approved by AAMPS editor in Oct 2022

#Current version: May 2025.

#Resources consulted
# https://r-workshop.mindsci.net/anova --- for effect size

####****DATA ANALYSIS FOR SINGLE LABS***####
####Study 2: Replicating Wegner et al. (1987)

#### Set Working Directory ####
# Update as needed for different systems
if (file.exists("C:/Users/jc506932/Dropbox/RRR (Adam Wang)/Data/Study 2")) {
  setwd("C:/Users/jc506932/Dropbox/RRR (Adam Wang)/Data/Study 2")
} else if (file.exists("C:/Users/chank/Dropbox/RRR (Adam Wang)/Data/Study 2")) {
  setwd("C:/Users/chank/Dropbox/RRR (Adam Wang)/Data/Study 2")
}

####Load libraries####
library(readxl); library(psych); library(reshape2); library(afex); library(emmeans); library(irr); library(dplyr); library(metafor); library(stringr); library(forestplot)

####Set global options
options(scipen=999) #Disables expressing small numbers in scientific notation, e. Otherwise it creates problematic Excel files.
options(digits = 4)

####For analysis of any single lab, import only one datafile at any one time.####
RRR_Wegner87 <- read_excel("Arbula_ISSA_Study2_cleaned.xlsx", sheet = "testdata", na = "NA") #Only N = 13; missing pct thinking
RRR_Wegner87 <- read_excel("Astle_TAM_Study2_cleaned.xlsx", sheet = "testdata", na = "NA") 
RRR_Wegner87 <- read_excel("Baldwin_GSU_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Barbu_AICU_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Brady_WFU_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Calvillo_CSU_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Chan_NTU_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Chen_PKU_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Dewitte_KUL_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Fiedler_Bamberg_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Hartanto_SMU_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Hawk_Digipen_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Heiman_IU_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Hussey_Bern_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Kim_WU_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Ku_SYSU_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Kumalasari_UPI_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Labarta_UCSD_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Légal_UPN_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Liem_Maret _Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Lin_USTC_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Linden_CSU_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Lopez_WPI_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Ma-Kellams_SJSU_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Majeed_UCMerced_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Mammarella_UP_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Moskowitz_LU_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Ngyuen_UCSF_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Prada_Lisbon_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Quek_NUS_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Saraiva_ISPA_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Torka_TUD_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Wang_HHU_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Wilkowski_Wyoming_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Wong_UNott_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Xu_SISU_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")
RRR_Wegner87 <- read_excel("Yusainy_Brawijaya_Study2_cleaned.xlsx", sheet = "testdata", na = "NA")

#Create list of Excel datafiles and temp dataframes within working directory
file_list = list.files(pattern="*.xlsx") #get all data with .xlsx
data_list <- vector("list", length = length(file_list)) #Create list outside loop
loop_times <- numeric(100) #capture loop time
df1 <- data.frame(Lab = NA_character_, Researcher = NA_character_, Country = NA_character_,
                  N.all = NA_real_, N.male = NA_real_, N.female = NA_real_, N.other = NA_real_,
                  included = NA_real_, pct.N.incl = NA_real_,
                  
                  N.conc = NA_real_, N.supp = NA_real_,
                  N.gorilla = 0, N.tiger = 0, N.food = 0, N.diseases = 0,
                 
                  ICC.A = NA_real_, ICC.B = NA_real_, ICC.C = NA_real_, ICC.ABC.mean = NA_real_,
                  IRR.probe = NA_real_,
                  
                  min.age = NA_real_, max.age = NA_real_, mean.age = NA_real_, sd.age = NA_real_,
                  
                  count.Manipchk.fail = NA_real_, count.Manipchk.pass = NA_real_, pct.Manipchk.pass = NA_real_, 
                  count.Compliance.fail = NA_real_, count.Compliance.pass = NA_real_, pct.Compliance.pass = NA_real_,
                  count.Probe.fail = NA_real_, count.Probe.pass = NA_real_, pct.Probe.pass = NA_real_,
                  
                  Pctthinking.phase.1 = NA_real_,
                  Pctthinking.phase.2 = NA_real_, #"what percentage of time do you think you spent thinking about a gorilla/a tiger/food/diseases in phrase x?"
                  
                  mean.conc = NA_real_, #raw means, all words
                  mean.supp = NA_real_,
                  sd.conc = NA_real_,
                  sd.supp = NA_real_,
                  
                  t.mean.conc = NA_real_, #transformed means, all words
                  t.mean.supp = NA_real_,
                  t.sd.conc = NA_real_,
                  t.sd.supp = NA_real_,
                  
                  mean.conc.tiger = NA_real_, #raw means, tiger words
                  mean.supp.tiger = NA_real_,
                  sd.conc.tiger = NA_real_,
                  sd.supp.tiger = NA_real_,
                  n.conc.tiger = NA_real_,
                  n.supp.tiger = NA_real_,
                  
                  mean.conc.gorilla = NA_real_, #raw means, gorilla words
                  mean.supp.gorilla = NA_real_,
                  sd.conc.gorilla = NA_real_,
                  sd.supp.gorilla = NA_real_,
                  n.conc.gorilla = NA_real_,
                  n.supp.gorilla = NA_real_,
                  
                  mean.conc.food = NA_real_, #raw means, food words
                  mean.supp.food = NA_real_,
                  sd.conc.food = NA_real_,
                  sd.supp.food = NA_real_,
                  n.conc.food = NA_real_,
                  n.supp.food = NA_real_,
                  
                  mean.conc.diseases = NA_real_, #raw means, diseases words
                  mean.supp.diseases = NA_real_,
                  sd.conc.diseases = NA_real_,
                  sd.supp.diseases = NA_real_,
                  n.conc.diseases = NA_real_,
                  n.supp.diseases = NA_real_,
                  
                  ges.Cond.Targetword.raw = NA_real_, #raw DV
                  ges.Cond.raw = NA_real_,
                  p.Cond.Targetword.raw = NA_real_,
                  p.Cond.raw = NA_real_,
                  
                  ges.Cond.Targetword.transf = NA_real_, #transformed DV
                  ges.Cond.transf = NA_real_,
                  p.Cond.Targetword.transf = NA_real_,
                  p.Cond.transf = NA_real_,
                  
                  d3.yi = NA_real_, #all words, raw
                  d3.vi = NA_real_,
                  d3.yi.transf = NA_real_, #all words, transformed
                  d3.vi.transf = NA_real_,
                  
                  d3.yi.tiger = NA_real_, #tiger
                  d3.vi.tiger = NA_real_,
                  d3.yi.gorilla = NA_real_, #gorilla
                  d3.vi.gorilla = NA_real_,
                  d3.yi.food = NA_real_, #food
                  d3.vi.food = NA_real_,
                  d3.yi.diseases = NA_real_, #diseases
                  d3.vi.diseases = NA_real_,
                  
                  FE.GLMM.Cond.Int.raw = NA_real_,
                  FE.GLMM.Cond.Int.transf = NA_real_,
                  FE.GLMM.Cond.NoInt.raw = NA_real_,
                  FE.GLMM.Cond.Noint.transf = NA_real_,
                  
                  p.GLMM.Cond.Int.raw = NA_real_,
                  p.GLMM.Cond.Int.transf = NA_real_,
                  p.GLMM.Cond.NoInt.raw = NA_real_,
                  p.GLMM.Cond.Noint.transf = NA_real_
                  )

####LOOP: START####
for (i in 1:37) { #Although it is more elegant to use seq_along(), using 1:n is better for debugging. Max n = 37.
  if (i == 1) next #exclude the first lab because it has too few participants (n = 13)
  start_time <- Sys.time()
  filename = file_list[[i]]
  message("Processing file ", i, " of ", length(file_list), ": ", file_list[i]) #Progress tracker
  
  RRR_Wegner87 <- read_excel(filename, sheet = "testdata") 
  RRR_Wegner87 <- RRR_Wegner87[,!grepl("period.1",names(RRR_Wegner87))] #Remove period 1 data.
 
# Rename columns for consistency
colnames(RRR_Wegner87) <- recode(colnames(RRR_Wegner87),
                             "ParticipantID" = "SubjID",
                             "Condition" = "Cond",
                             "sum.ABC_period2.final" = "intrusions")
                             
# Define categorical variables
RRR_Wegner87 <- RRR_Wegner87 %>%
  mutate(
    gender = factor(gender, levels = 1:4, labels = c("Female", "Male", "Other", "Prefer not to say")),
    MC = factor(MC, levels = 1:2, labels = c("Think", "Not think")),
    Cond = factor(Cond, levels = 1:2, labels = c("Suppression", "Concentration")), #initial suppression = 1, initial concentration = 2
    Targetword = factor(Targetword, levels = 1:4, labels = c("gorilla", "tiger", "food", "diseases")),
    probe.final = factor(probe.final, levels = 0:1, labels = c("pass", "fail")), #Probe: did participant fail probe? 1 = yes, 0 = no
    Compliance = factor(Compliance, levels = 1:2, labels = c("pass", "fail")) #Compliance: complied with experimental instructions? Yes = 1, No = 2
  )

# Coercion to numeric numeric variables
RRR_Wegner87 <- RRR_Wegner87 %>%
  mutate(
   age = as.numeric(age),
   Percentage.round.1 = as.numeric(Percentage.round.1),
   Percentage.round.2 = as.numeric(Percentage.round.2)
  )

####MANIPULATION CHECKS, COMPLIANCE, & SUSPICION PROBES####
  # Manipulation check: Define pass/fail conditions
RRR_Wegner87 <- RRR_Wegner87 %>%
  mutate(Manipchk.pass = case_when(
    MC == "Think" & Cond == "Concentration" ~ "pass",
    MC == "Not think" & Cond == "Suppression" ~ "pass",
    MC == "Think" & Cond == "Suppression" ~ "fail",
    MC == "Not think" & Cond == "Concentration" ~ "fail"
  ))

RRR_Wegner87$Manipchk.pass <- as.factor(RRR_Wegner87$Manipchk.pass) #1 = fail; 2 = pass

  #Fill in the manipulation checks, compliance, and probe numbers first, before any subsetting later
df1$N.all = nrow(RRR_Wegner87)
df1$count.Manipchk.fail <- sum(RRR_Wegner87$Manipchk.pass == "fail", na.rm = TRUE)
df1$count.Manipchk.pass <- sum(RRR_Wegner87$Manipchk.pass == "pass", na.rm = TRUE)
df1$pct.Manipchk.pass <- sum(RRR_Wegner87$Manipchk.pass == "pass", na.rm = TRUE)/(sum(RRR_Wegner87$Manipchk.pass == "pass", na.rm = TRUE)+sum(RRR_Wegner87$Manipchk.pass == "fail", na.rm = TRUE))*100

df1$count.Compliance.fail <- sum(RRR_Wegner87$Compliance == "fail", na.rm = TRUE)
df1$count.Compliance.pass <- sum(RRR_Wegner87$Compliance == "pass", na.rm = TRUE)
df1$pct.Compliance.pass <- sum(RRR_Wegner87$Compliance == "pass", na.rm = TRUE)/(sum(RRR_Wegner87$Compliance== "pass", na.rm = TRUE)+sum(RRR_Wegner87$Compliance== "fail", na.rm = TRUE))*100

df1$count.Probe.fail <- sum(RRR_Wegner87$probe.final == "fail", na.rm = TRUE)
df1$count.Probe.pass <- sum(RRR_Wegner87$probe.final == "pass", na.rm = TRUE)
df1$pct.Probe.pass <-sum(RRR_Wegner87$probe.final == "pass", na.rm = TRUE)/(sum(RRR_Wegner87$probe.final== "pass", na.rm = TRUE)+sum(RRR_Wegner87$probe.final== "fail", na.rm = TRUE))*100

  #Record counts by conditions
df1$N.conc = nrow(filter(RRR_Wegner87, Cond == "Concentration")) 
df1$N.supp = nrow(filter(RRR_Wegner87, Cond == "Suppression"))
df1$N.gorilla = nrow(filter(RRR_Wegner87, Targetword == "gorilla"))
df1$N.tiger = nrow(filter(RRR_Wegner87, Targetword == "tiger"))
df1$N.food = nrow(filter(RRR_Wegner87, Targetword == "food"))
df1$N.diseases = nrow(filter(RRR_Wegner87, Targetword == "diseases"))

#Record target word means and standard deviation
df2 <- RRR_Wegner87 %>%
  filter(complete.cases(Cond, Targetword, intrusions)) %>% #Remove rows have NAs for any of the three variables. 
  group_by(Cond, Targetword) %>%
  summarise(
    Mean_Targetword = mean(intrusions, na.rm = TRUE),
    SD_Targetword = sd(intrusions, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

 #Print sample sizes by conditions (before exclusions)
print(table(RRR_Wegner87[c("Cond", "Targetword")])) #sample size should be approximately equal across conditions

#Transform dependent variable (used in WEgner, 1987) - not preregistered
RRR_Wegner87$intrusions <- as.numeric(as.character(RRR_Wegner87$intrusions)) #Change all to character, then to numeric. Otherwise, somehow R will coerce this into character within a loop, but not when processed independently.
RRR_Wegner87$t.intrusions <- sqrt(RRR_Wegner87$intrusions) #indeed the sqrt-transformed intrusion results in a more normal distribution

#Diagnostics: Inspect QQ plots for normality violation (don't run within loop)
#par(mfrow=c(1,2)) 
#qqnorm(RRR_Wegner87$intrusions, main = "Original DV")
#qqnorm(RRR_Wegner87$t.intrusions, main = "Transformed DV")

#Note: A = number of target thought mentions only, B = number of bell rings only, C = number of thought mentions and bell rings at the same time

#Set these variables to numeric. Somehow looping through them coerces them to character. When running individual labs, this problem does not occur.
RRR_Wegner87 <- RRR_Wegner87 %>%
  mutate(across(c(A_period.2.coder1, B_period.2.coder1, C_period.2.coder1,
                  A_period.2.coder2, B_period.2.coder2, C_period.2.coder2,
                  A_period.2.final, B_period.2.final, C_period.2.final), as.numeric))

####Intra-class correlation coefficients (only period 2)
test1 <- icc(RRR_Wegner87[, c("A_period.2.coder1", "A_period.2.coder2")], model = "twoway", type = "consistency", unit = "single") #A = number of target thought mentions only
test2 <- icc(RRR_Wegner87[, c("B_period.2.coder1", "B_period.2.coder2")], model = "twoway", type = "consistency", unit = "single") #B = number of bell rings only
test3 <- icc(RRR_Wegner87[, c("C_period.2.coder1", "C_period.2.coder2")], model = "twoway", type = "consistency", unit = "single") #C = number of thought mentions and bell rings at the same time

#Compute mean Cohen's kappa for interrater reliability of probe
agree.pct <- agree(RRR_Wegner87[, c("probe.coder1", "probe.coder2")]) #cannot use kappa2 because some labs with 100% agreement will produce kappa = NaN (not a number).

####Select only participants who passed compliance checks, and suspicion probes####
#RRR_Wegner87 <-subset(RRR_Wegner87, Manipchk.pass == "pass" & probe.final == "pass" & Compliance == "pass") #NOT RUN, cos Adam and Editor felt that MC failuress should not be excluded.
RRR_Wegner87 <-subset(RRR_Wegner87, probe.final == "pass" & Compliance == "pass") #Run this preregistered analysis instead.

#####Main analyses (only for Period 2)
  #Using try(), only when Targetword is a factor, because some labs did not run all Targetword conditions.
  #try() allows the loop to continue when an error is encountered.

mainresult.dv.raw<-try(aov_ez(id = "SubjID", dv = "intrusions", data=RRR_Wegner87, 
                              between = c("Cond", "Targetword")))
try(summary(mainresult.dv.raw))

mainresult.dv.transf<-try(aov_ez(id = "SubjID", dv = "t.intrusions", data=RRR_Wegner87, 
                                 between = c("Cond", "Targetword")))
try(summary(mainresult.dv.transf))

  #Error tracking: Is ANOVA for Cond:Targetword done?
  print("ANOVA Cond:Targetword done")

#RUN IF interaction is significant and/or main effect of target word is found.
#emmeans(mainresult.dv.raw, pairwise ~ Cond|Targetword, adjust = "none") 
#emmeans(mainresult.dv.raw, pairwise ~ Targetword|Cond, adjust = "none")
#emmeans(mainresult.dv.transf, pairwise ~ Cond|Targetword, adjust = "none") 
#emmeans(mainresult.dv.transf, pairwise ~ Targetword|Cond, adjust = "none")

#Plot 2 x 2. #RUN IF interaction is significant and/or main effect of target word is found.
#emmip(mainresult, Cond~Targetword, xlab = "Target Word", ylab = "Mean no. of thought intrusions", CIs = T) 

#Collapse across target word.
mainresult2.dv.raw<-aov_ez(id = "SubjID", dv = "intrusions", data=RRR_Wegner87, 
                    between = c("Cond"))
summary(mainresult2.dv.raw)

mainresult2.dv.transf<-aov_ez(id = "SubjID", dv = "t.intrusions", data=RRR_Wegner87, 
                           between = c("Cond"))
summary(mainresult2.dv.transf)

  #Error tracking: Is ANOVA for Cond done?
  print("ANOVA Cond done")

####Basic info
df1$Lab = RRR_Wegner87$Lab[[1]]
df1$Researcher = RRR_Wegner87$Researcher[[1]]
df1$Country = RRR_Wegner87$Country[[1]]
df1$min.age=min(RRR_Wegner87$age, na.rm = TRUE)
df1$max.age=max(RRR_Wegner87$age, na.rm = TRUE)
df1$mean.age = mean(RRR_Wegner87$age, na.rm = T)
df1$sd.age = sd(RRR_Wegner87$age, na.rm = T)

df1$N.male = as.numeric(table(RRR_Wegner87$gender))[2]
df1$N.female = as.numeric(table(RRR_Wegner87$gender))[1]
df1$N.other = as.numeric(table(RRR_Wegner87$gender))[3] + as.numeric(table(RRR_Wegner87$gender))[4]

df1$included = nrow(RRR_Wegner87)
df1$pct.N.incl = (df1$included)/df1$N.all*100

df1$ICC.A = test1$value
df1$ICC.B = test2$value
df1$ICC.C = test3$value
df1$ICC.ABC.mean = mean(c(df1$ICC.A, df1$ICC.B, df1$ICC.C), na.rm = T)
df1$IRR.probe <- agree.pct$value

df1$Pctthinking.phase.1 = mean(RRR_Wegner87$Percentage.round.1, na.rm = T)
df1$Pctthinking.phase.2 = mean(RRR_Wegner87$Percentage.round.2, na.rm = T)
  
df1$ges.Cond.Targetword.raw <- try(mainresult.dv.raw$anova_table$ges[3]) #using try() whenever mainresult object is called because it may be null.
df1$ges.Cond.raw <- mainresult2.dv.raw$anova_table$ges
df1$p.Cond.Targetword.raw <- try(mainresult.dv.raw$anova_table$`Pr(>F)`[3])
df1$p.Cond.raw <- mainresult2.dv.raw$anova_table$`Pr(>F)`

df1$ges.Cond.Targetword.transf <- try(mainresult.dv.transf$anova_table$ges[3])
df1$ges.Cond.transf <- mainresult2.dv.transf$anova_table$ges
df1$p.Cond.Targetword.transf <- try(mainresult.dv.transf$anova_table$`Pr(>F)`[3])
df1$p.Cond.transf <- mainresult2.dv.transf$anova_table$`Pr(>F)`

####Extract relevant means and SDs for meta-analysis
  #Raw means (untransformed)
df1$mean.supp <- mean(RRR_Wegner87$intrusions[RRR_Wegner87$Cond == 'Suppression'], na.rm = TRUE)
df1$mean.conc<- mean(RRR_Wegner87$intrusions[RRR_Wegner87$Cond == 'Concentration'], na.rm = TRUE) #TO DO: Check against Excel
df1$sd.supp <- sd(RRR_Wegner87$intrusions[RRR_Wegner87$Cond == 'Suppression'], na.rm = TRUE)
df1$sd.conc<- sd(RRR_Wegner87$intrusions[RRR_Wegner87$Cond == 'Concentration'], na.rm = TRUE)

  #Square-root transformed means
df1$t.mean.supp <- mean(RRR_Wegner87$t.intrusions[RRR_Wegner87$Cond == 'Suppression'], na.rm = TRUE)
df1$t.mean.conc<- mean(RRR_Wegner87$t.intrusions[RRR_Wegner87$Cond == 'Concentration'], na.rm = TRUE) 
df1$t.sd.supp <- sd(RRR_Wegner87$t.intrusions[RRR_Wegner87$Cond == 'Suppression'], na.rm = TRUE)
df1$t.sd.conc<- sd(RRR_Wegner87$t.intrusions[RRR_Wegner87$Cond == 'Concentration'], na.rm = TRUE)

  #Conditional means (raw means)
df1$mean.conc.tiger = df2[df2$Cond == "Concentration" & df2$Targetword == "tiger", "Mean_Targetword"][[1]]
df1$mean.supp.tiger = df2[df2$Cond == "Suppression" & df2$Targetword == "tiger", "Mean_Targetword"][[1]]
df1$sd.conc.tiger = df2[df2$Cond == "Concentration" & df2$Targetword == "tiger", "SD_Targetword"][[1]]
df1$sd.supp.tiger = df2[df2$Cond == "Suppression" & df2$Targetword == "tiger", "SD_Targetword"][[1]]
df1$n.conc.tiger = df2[df2$Cond == "Concentration" & df2$Targetword == "tiger", "n"][[1]]
df1$n.supp.tiger = df2[df2$Cond == "Suppression" & df2$Targetword == "tiger", "n"][[1]]

df1$mean.conc.gorilla = df2[df2$Cond == "Concentration" & df2$Targetword == "gorilla", "Mean_Targetword"][[1]]
df1$mean.supp.gorilla = df2[df2$Cond == "Suppression" & df2$Targetword == "gorilla", "Mean_Targetword"][[1]]
df1$sd.conc.gorilla = df2[df2$Cond == "Concentration" & df2$Targetword == "gorilla", "SD_Targetword"][[1]]
df1$sd.supp.gorilla = df2[df2$Cond == "Suppression" & df2$Targetword == "gorilla", "SD_Targetword"][[1]]
df1$n.conc.gorilla = df2[df2$Cond == "Concentration" & df2$Targetword == "gorilla", "n"][[1]]
df1$n.supp.gorilla = df2[df2$Cond == "Suppression" & df2$Targetword == "gorilla", "n"][[1]]

df1$mean.conc.food = df2[df2$Cond == "Concentration" & df2$Targetword == "food", "Mean_Targetword"][[1]]
df1$mean.supp.food = df2[df2$Cond == "Suppression" & df2$Targetword == "food", "Mean_Targetword"][[1]]
df1$sd.conc.food = df2[df2$Cond == "Concentration" & df2$Targetword == "food", "SD_Targetword"][[1]]
df1$sd.supp.food = df2[df2$Cond == "Suppression" & df2$Targetword == "food", "SD_Targetword"][[1]]
df1$n.conc.food = df2[df2$Cond == "Concentration" & df2$Targetword == "food", "n"][[1]]
df1$n.supp.food = df2[df2$Cond == "Suppression" & df2$Targetword == "food", "n"][[1]]

df1$mean.conc.diseases = df2[df2$Cond == "Concentration" & df2$Targetword == "diseases", "Mean_Targetword"][[1]]
df1$mean.supp.diseases = df2[df2$Cond == "Suppression" & df2$Targetword == "diseases", "Mean_Targetword"][[1]]
df1$sd.conc.diseases = df2[df2$Cond == "Concentration" & df2$Targetword == "diseases", "SD_Targetword"][[1]]
df1$sd.supp.diseases = df2[df2$Cond == "Suppression" & df2$Targetword == "diseases", "SD_Targetword"][[1]]
df1$n.conc.diseases = df2[df2$Cond == "Concentration" & df2$Targetword == "diseases", "n"][[1]]
df1$n.supp.diseases = df2[df2$Cond == "Suppression" & df2$Targetword == "diseases", "n"][[1]]

  #Error tracking: Basic input done?
  print("Basic input done")

####Effect size calculation for meta-analysis
  #Raw means (all words)
d3 <- escalc(n2i = N.conc, n1i = N.supp, 
             m2i = mean.conc, m1i = mean.supp, 
             sd2i = sd.conc, sd1i = sd.supp, data = df1, measure = "SMD", 
             append = TRUE) #Let op!  The effect size calculation is M1-M2, i.e., mean.supp − mean.conc, not M2-M1.
df1$d3.yi <- d3$yi #paste Cohen's d to df1
df1$d3.vi <- d3$vi #variance of Cohen's d

  #Transformed means (all words)
d3.transf <- escalc(n2i = N.conc, n1i = N.supp, 
             m2i = t.mean.conc, m1i = t.mean.supp, 
             sd2i = t.sd.conc, sd1i = t.sd.supp, data = df1, measure = "SMD", 
             append = TRUE)
df1$d3.yi.transf <- d3.transf$yi
df1$d3.vi.transf <- d3.transf$vi

  #Raw means (by target words)
d3.tiger <- escalc(n2i = n.conc.tiger, n1i = n.supp.tiger, 
             m2i = mean.conc.tiger, m1i = mean.supp.tiger, 
             sd2i = sd.conc.tiger, sd1i = sd.supp.tiger, data = df1, measure = "SMD", 
             append = TRUE)
df1$d3.yi.tiger <- d3.tiger$yi
df1$d3.vi.tiger <- d3.tiger$vi

d3.gorilla <- escalc(n2i = n.conc.gorilla, n1i = n.supp.gorilla, 
                   m2i = mean.conc.gorilla, m1i = mean.supp.gorilla, 
                   sd2i = sd.conc.gorilla, sd1i = sd.supp.gorilla, data = df1, measure = "SMD", 
                   append = TRUE)
df1$d3.yi.gorilla <- d3.gorilla$yi
df1$d3.vi.gorilla <- d3.gorilla$vi

d3.food <- escalc(n2i = n.conc.food, n1i = n.supp.food, 
                   m2i = mean.conc.food, m1i = mean.supp.food, 
                   sd2i = sd.conc.food, sd1i = sd.supp.food, data = df1, measure = "SMD", 
                   append = TRUE)
df1$d3.yi.food <- d3.food$yi
df1$d3.vi.food <- d3.food$vi

d3.diseases <- escalc(n2i = n.conc.diseases, n1i = n.supp.diseases, 
                   m2i = mean.conc.diseases, m1i = mean.supp.diseases, 
                   sd2i = sd.conc.diseases, sd1i = sd.supp.diseases, data = df1, measure = "SMD", 
                   append = TRUE)
df1$d3.yi.diseases <- d3.diseases$yi
df1$d3.vi.diseases <- d3.diseases$vi

  #Error tracking: Meta-analytic effect size done?
  print("Effect size done")

####Mixed models (Note: Neither SubjID or Targetword::SubjID as random intercept is not a viable model. See manuscript.)
m1.raw <- lmer(intrusions ~ Cond*Targetword + (1|Targetword), data = RRR_Wegner87) #Raw dv
summary(m1.raw) #Cond*Targetword
coef.m1.raw <- coef(summary(m1.raw))

m1.transf <- lmer(t.intrusions ~ Cond*Targetword + (1|Targetword), data = RRR_Wegner87) #Transformed dv
summary(m1.transf) #Cond*Targetword
coef.m1.transf <- coef(summary(m1.transf))

m2.raw <- lmer(intrusions ~ Cond + (1|Targetword), data = RRR_Wegner87) #Raw dv
summary(m2.raw) #Cond only
coef.m2.raw <- coef(summary(m2.raw))

m2.transf <- lmer(t.intrusions ~ Cond + (1|Targetword), data = RRR_Wegner87) #Transformed dv
summary(m2.transf) #Cond only
coef.m2.transf <- coef(summary(m2.transf))

df1$FE.GLMM.Cond.Int.raw <- coef.m1.raw["CondConcentration", "Estimate"]          #GLMM = Cond*Targetword
df1$FE.GLMM.Cond.Int.transf <- coef.m1.transf["CondConcentration", "Estimate"]    #GLMM = Cond*Targetword
df1$FE.GLMM.Cond.NoInt.raw <- coef.m2.raw["CondConcentration", "Estimate"]        #GLMM = Cond
df1$FE.GLMM.Cond.Noint.transf <- coef.m2.transf["CondConcentration", "Estimate"]  #GLMM = Cond

df1$p.GLMM.Cond.Int.raw = coef.m1.raw["CondConcentration", "Pr(>|t|)"]            #GLMM = Cond*Targetword
df1$p.GLMM.Cond.Int.transf = coef.m1.transf["CondConcentration", "Pr(>|t|)"]      #GLMM = Cond*Targetword
df1$p.GLMM.Cond.NoInt.raw = coef.m2.raw["CondConcentration", "Pr(>|t|)"]          #GLMM = Cond
df1$p.GLMM.Cond.Noint.transf = coef.m2.transf["CondConcentration", "Pr(>|t|)"  ]  #GLMM = Cond

  #Error tracking: GLMM done?
  print("GLMM done")

####Compute Variance Partition Components for mixed models####
#Resource: https://benwhalley.github.io/just-enough-r/icc-and-vpc.html

VPC.m1.raw <- VarCorr(m1.raw) %>%
  as.data.frame() %>%
  mutate(icc=vcov/sum(vcov)) %>%
  select(grp, icc)

VPC.m1.transf <- VarCorr(m1.transf) %>%
  as.data.frame() %>%
  mutate(icc=vcov/sum(vcov)) %>%
  select(grp, icc)

VPC.m2.raw <- VarCorr(m2.raw) %>%
  as.data.frame() %>%
  mutate(icc=vcov/sum(vcov)) %>%
  select(grp, icc)

VPC.m2.transf <- VarCorr(m2.transf) %>%
  as.data.frame() %>%
  mutate(icc=vcov/sum(vcov)) %>%
  select(grp, icc)

df1$VPC.m1.raw <- VPC.m1.raw$icc[1]
df1$VPC.m1.transf <- VPC.m1.transf$icc[1]
df1$VPC.m2.raw <- VPC.m2.raw$icc[1]
df1$VPC.m2.transf <- VPC.m2.transf$icc[1]

#Add info to dataframe
data_list[[i]] <- df1

  #Error checking: Number of variables for each lab
  print(ncol(df1)) #should get same number for each lab

#Loop time calculation
Sys.sleep(0.05)
  
end_time <- Sys.time()
loop_times[i] <- end_time - start_time

}

####LOOP: END####

####Stitch data together####
df1 <- do.call(rbind, data_list)

####Replace ANOVAs errors with NA####
df1 <- df1 %>%
  mutate(across(
    .cols = c(ges.Cond.Targetword.raw, p.Cond.Targetword.raw, ges.Cond.Targetword.transf, p.Cond.Targetword.transf), #Recall: Some labs did not have complete cells for each target word, leading to errors in ANOVA.
    .fns = ~ as.numeric(ifelse(grepl("^error", .), NA, .)) 
  )) %>%  arrange(str_extract(df1$Researcher, '[^ ]+$')) #arrange by last name of researcher.

####Write to Excel####
write.csv(df1, "C:/Users/jc506932/Dropbox/RRR (Adam Wang)/RRR_Wegner87_meta.csv")

####Clean up####
rm(list = setdiff(ls(), c("df1"))) #Keep only df1

####Create table in manuscript####
Study2.ms.df1 <- data.frame (Researcher = str_extract(df1$Researcher, '[^ ]+$'), #Extract last name
                             Country = df1$Country,
                             Pct.dataincluded = round(df1$pct.N.incl, digits = 1),
                             Age = paste0(df1$min.age, "-", df1$max.age, ", ", round(df1$mean.age, digits = 2), " ", "(", round(df1$sd.age, digits = 2), ")"),
                             Gender = paste0(df1$N.male, ":", df1$N.female, ":", df1$N.other),
                             mean.TTO.conc = paste0(round(df1$mean.conc, digits = 2), " ", "(", round(df1$sd.conc, digits = 2), ")"), #target thought occurrences
                             mean.TTO.supp = paste0(round(df1$mean.supp, digits = 2), " ", "(", round(df1$sd.supp, digits = 2), ")")
                             ) %>%  arrange(Researcher)

write.csv(Study2.ms.df1, "C:/Users/jc506932/Dropbox/RRR (Adam Wang)/RRR_Wegner87_table2.csv")

#From Wegner et al. (1987): "The mean of the summed thought measures during expression was 19.22, and this was significantly greater than the suppression mean of 6.78, F(l, 32) = 41.0 l, p < .0001"
#If F = 41, then t = 6.4. Note: df = 32. (n = 17 per group)
#Using formula for independent samples t-test, pooled SD = 5.667.
#Using formula for pooled SD, and assuming homogeneity of variance, standard deviation per group is SQRT(5.667) = 2.3806

####Analyses as reported in the Results section of the manuscript####
#Use DescTools because it is the most straightforward.

sum(df1$N.all, na.rm = T) #Total N
mean(df1$mean.age, na.rm = T)
sd(df1$mean.age, na.rm = T)

sum(df1$count.Manipchk.fail, na.rm = T) #not required to report
sum(df1$count.Compliance.fail, na.rm = T)
sum(df1$count.Probe.fail, na.rm = T)

mean(df1$ICC.ABC.mean)
mean(df1$IRR.probe)

mean(df1$VPC.m1.raw)
mean(df1$VPC.m2.raw)

mean(df1$VPC.m1.transf)
mean(df1$VPC.m2.transf)

####****META ANALYSIS***####
#The meta-analysis codes is for Wegner et al. (1987).

#d3: Rebound effect (raw means)
overalld3 <- rma(yi = d3.yi, vi = d3.vi, data = df1)
summary(overalld3)

#d3: Rebound effect (transformed means) - Not preregistered
overalld3.transf <- rma(yi = d3.yi.transf, vi = d3.vi.transf, data = df1)
summary(overalld3.transf)

#d3: Rebound effect (by target words)
overalld3.tiger <- rma(yi = d3.yi.tiger, vi = d3.vi.tiger, data = df1)
summary(overalld3.tiger)

overalld3.gorilla <- rma(yi = d3.yi.gorilla, vi = d3.vi.gorilla, data = df1)
summary(overalld3.gorilla)

overalld3.food <- rma(yi = d3.yi.food, vi = d3.vi.food, data = df1)
summary(overalld3.food)

overalld3.diseases <- rma(yi = d3.yi.diseases, vi = d3.vi.diseases, data = df1)
summary(overalld3.diseases)

# Prepare the data for the forest plot
base_data <- tibble::tibble(mean  = df1$d3.yi,
                            lower = df1$d3.yi-1.96*sqrt(df1$d3.vi),
                            upper = df1$d3.yi+1.96*sqrt(df1$d3.vi),
                            Lab = str_extract(df1$Researcher, '[^ ]+$'),
                            ES = c("0.75", "-0.14", "0.21", "0.25", "0.39", "0.47", "0.55", "0.03", "-1.16",
                                   "0.01", "0.79", "0.58", "1.53", "-0.05", "0.36", "0.14", "-0.05", "0.30", "0.07", 
                                   "-0.14", "0.46", "0.38", "0.95", "-0.06", "0.22", "-0.17", "0.03", "-0.11", "0.07", 
                                   "0.10", "0.12", "0.38", "-0.46", "-0.45", "-0.03"))

base_data |>
  forestplot(labeltext = c(Lab, ES),
             clip = c(-3, 3),
             txt_gp = fpTxtGp(
             label = gpar(cex = 0.65),  # Regular text
             ticks = gpar(cex = 0.7),  # Axis labels
             xlab = gpar(cex = 0.8),   # X-axis title
             title = gpar(cex = 1.0)   # Main title (if any)
             ), lineheight = unit(0.7, "cm")
             )|>
  fp_add_lines(h_3 = gpar(lty = 2),
               h_39 = gpar(lwd = 1, columns = 1:2, col = "#000044")) |>
  fp_set_style(box = "royalblue",
               line = "darkblue",
               summary = "red") |>
  fp_add_header(Lab = c("", "Researcher"),
                ES = c("", "Cohen's d")) |>
  fp_append_row(mean  = overalld3$beta[1],
                lower = overalld3$ci.lb,
                upper = overalld3$ci.ub,
                Lab = "Summary",
                ES = "0.16",
                is.summary = TRUE) |>
  fp_set_zebra_style("#EFEFEF")
dev.print(pdf, file="C:/Users/jc506932/Dropbox/RRR (Adam Wang)/Forestplot2.pdf" ,onefile=T,paper='A4', width = 21/2.54, height = 29.7/2.54) 

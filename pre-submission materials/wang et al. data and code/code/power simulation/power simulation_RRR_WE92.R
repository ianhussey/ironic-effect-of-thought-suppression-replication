#Power calculation for thought suppression and rebound effect RRR (Wang et al.)
#2 Nov 2022
#Codes written by Kai Qin Chan
#James Cook University, Singapore

#EXPLANATION: The purpose of this simulation is to find out how many participants 
#are necessary to replicate the interaction effect in Study 1 of Erber & Wegner (1992).

#NOTE: Scripts with a #NOT RUN comment exist for archival purposes to document the thought processes.

#Resources:
#https://debruine.github.io/faux/articles/sim_design.html

#Packages needed
library(faux)
library(emmeans) 
library(afex)
library(psych)
library(reshape2) 
library(magrittr)
library(dplyr)
library(simr)
library(lsr)
library(effectsize)

#For quick view of the means in the 2 (Condition: Supp vs. Conc) x 2 (Time pressure: High vs. Low)
WE92means <- matrix(c(.23, .09, 4.09, .05, 2.022, .05, .29, 9.29, .005, 3.048), ncol = 5, byrow = TRUE)
colnames(WE92means) <- c("Supp","Conc", "F(1,54)", "p", "t")
rownames(WE92means) <- c("High","Low")
WE92means <- as.table(WE92means)
WE92means

#Define parameters to simulate dataframe
between <- list(Condition = c(supp = "Suppression", conc = "Concentration"))
within <- list(Timepressure = c("Low", "High"))
r <- .5 #Assume same correlation for within-subject factor
vardesc <- c(Condition = "Condition", Timepressure = "Time pressure") #Add factor labels
n <- 28  #as reported in Wegner & Erber (1992)
mu <- data.frame( #as reported in Wegner & Erber (1992)
  supp    = c(.05, .23),
  conc    = c(.29, .09),
  row.names = within$Timepressure
)
sd <- .35 #by trial-and-error

#Crate single lab dataframe, for testing purposes
df.single <- sim_design(within, between,
                        n = n, mu = mu, sd = sd, r = r, #Note: When homogeneity of variance was assumed, the p-values of the simple effects are different from Wegner & Erber (1992). So we must assume heterogeneity of variances.
                        empirical = TRUE, vardesc = vardesc, plot = F, long = F) 
df.single$Replication4 <- factor(rep(c("child","house", "mountain", "car"),each=1)) #each = 1 works only for long format data. If creating wide format data, use times.

#Examine the single-lab dataset as an ANOVA model,  for verification purposes
df.single.long <- melt(df.single, id.vars=c("id", "Condition", "Replication4"), measure.vars=c("Low", "High"), variable.name="Timepressure", value.name="meanpromptedresponse") %>% arrange(id)
model1<-aov_ez("id", "meanpromptedresponse", df.single.long, between = c("Condition"), within = c("Timepressure"))
summary(model1) #interaction should be approx F(1,54) = 16.94, p < .0001
cohens_f(model1)

#Conduct checks for single lab data
cor(df.single$Low, df.single$High) #check if correlation is reasonably close to .5
WE92means #check against this table
describeBy(df.single.long$meanpromptedresponse, df.single.long$Condition:df.single.long$Timepressure) 
emmip(model1, Condition~Timepressure, xlab = "Time Pressure", ylab = "Mean number of prompt-relevant associations per trial", CIs = T)  #Not necessary, but nice to see.

#Define parameters for loop
k <- 53 ##number of datasets to simulate, with each dataset having n*2 individuals; total 2*n*k = 2968
n <- 28 # Sample size per between-subjects condition in Wegner and Erber (1992)
datalist = vector("list", length = n)

#Create multilab dataframe
for ( i in 1:k ){ 
  df <- sim_design(within, between,
                   n = n, mu = mu, sd = sd, r = r, #Note: When homogeneity of variance was assumed, the p-values of the simple effects are different from Wegner & Erber (1992). So we must assume heterogeneity of variances.
                   empirical = TRUE, vardesc = vardesc, plot = F, long = F) #do not create long format directly because it creates problems with iteration number.
  df$Replication4 <- factor(rep(c("child","house", "mountain", "car"), each=1)) #Naming the variable "Replication" so as to be consistent with Erber & Wegner's (1992) terminology.
  df$i <- i  # keep track of which iteration produced the data
  datalist[[i]] <- df # add iteration number to list. This code must come at the end after all variables are created.
}

multilab.df = do.call(rbind, datalist)
multilab.df$SubjID <-factor(seq_along(multilab.df[,1]))

#Restructure multilab data from wide to long
multilab.df.long <- melt(multilab.df, id.vars=c("SubjID", "Condition", "i", "Replication4"), measure.vars=c("Low", "High"), variable.name="Timepressure", value.name="meanpromptedresponse") %>% arrange(SubjID)

#Conduct checks for multi-lab data
describeBy(multilab.df.long$meanpromptedresponse, multilab.df.long$Condition:multilab.df.long$Timepressure) 
model2<-aov_ez("SubjID", "meanpromptedresponse", multilab.df.long, between = c("Condition"), within = c("Timepressure"))
summary(model2)
cohens_f(model2) #interaction term should approximate f = .55 (same as single-lab's ANOVA)

#Define mixed model
mod1 <- lmer(meanpromptedresponse ~ Condition*Timepressure + (1|SubjID), data=multilab.df.long)
summary(mod1) #subjects as random intercept; 2 fixed factors as above. #NOT RUN

mod2 <- lmer(meanpromptedresponse ~ Condition*Timepressure*Replication4 + (1|SubjID) + (1|Replication4), data=multilab.df.long)
summary(mod2) #subject and target words as random intercepts; 3 fixed factors as above. #NOT RUN

mod3 <- lmer(meanpromptedresponse ~ Condition*Timepressure + (1|SubjID) + (1|Replication4), data=multilab.df.long)
summary(mod3) #subject and target words as random intercepts; 2 fixed factors as above. 

#Power analysis
fixef(mod1)["Conditionconc:TimepressureHigh"] #Current effect size is -0.38. #NOT RUN
fixef(mod1)["Conditionconc:TimepressureHigh"] <- (fixef(mod1)["Conditionconc:TimepressureHigh"])/5.5 #5 times smaller
fixef(mod1)["Conditionconc:TimepressureHigh"] 
power1 <- powerSim(mod1, test = fixed("Condition:Timepressure", method = "anova"), nsim = 5000) #If method = z, then group levels but be specified. Alternatively, use method = "anova" but specify group factprs (not group levels). See https://github.com/pitakakariki/simr/issues/93
power1  #random intercept of subject.  Result: 100% (no difference even if nsim = 50)

fixef(mod2)["Conditionconc:TimepressureHigh"] #Current effect size is -0.37. #NOT RUN
fixef(mod2)["Conditionconc:TimepressureHigh"] <- (fixef(mod2)["Conditionconc:TimepressureHigh"])/5.5 #5 times smaller
fixef(mod2)["Conditionconc:TimepressureHigh"] 
power2 <- powerSim(mod2, test = fixed("Condition:Timepressure", method = "anova"), nsim = 5000) 
power2  #random intercept of subject and target words.  Result: 100% 

fixef(mod3)["Conditionconc:TimepressureHigh"] #Current effect size is -0.38
fixef(mod3)["Conditionconc:TimepressureHigh"] <- (fixef(mod3)["Conditionconc:TimepressureHigh"])/5.5 #5 times smaller
fixef(mod3)["Conditionconc:TimepressureHigh"] 
power3 <- powerSim(mod3, test = fixed("Condition:Timepressure", method = "anova"), nsim = 1000) 
power3  #random intercept of subject and target words.  Result: 100% 

#NOT USED: If different SDs need to be defined, this is the code that will replicate the simple effects.
sd <- list(supp = c(Low = .29, High = .2),
           conc = c(Low = .29, High = .3))
#Power calculation for thought suppression and rebound effect RRR (Wang et al.)
#1 Nov 2022
#Codes written by Kai Qin Chan
#James Cook University, Singapore

#EXPLANATION: The purpose of this simulation is to find out how many participants 
#are necessary to replicate the condition effect in Study 1 of Wegner et al. (1987).

#Simulate dataframe based on reported statistics.
n <- 17 # sample size per group. This is the sample size per group in Wegner et al. (1987)
SD <- 7.5 #derived based on reported F values and means in Wegner et al. (1987). This SD would bring the t-value close to 2.24, as reported in the original paper (i.e., SQRT of F = 5.05)
k <- 87 #can try 25 (if target word is not a fixed factor but a random factor), or 87 or 88 (if target word is a fixed factor and random factor). #number of datasets to simulate, with each dataset having n individuals
N <- k*n*2 #total number of participants across all labs
datalist = vector("list", length = n)

set.seed(123)

save_ps <- length(N) #Not necessary actually
save_ts <- length(N) #To see if the average t-value comes close to 2.24

#Note: The simulation preserves the means and standard deviations of Wegner et al. (1987).

for ( i in 1:k ){ 
  group_A <- rnorm(n, mean = 16.38, sd = SD) #Initial expression, Express period: 6.96 + 4.86 + 4.56
  group_B <- rnorm(n, mean = 22.06, sd = SD) #Initial suppression, Expression period: 8 + 7.71 + 6.35
  
  dfa <- data.frame(group_A)
  names(dfa)<-c("dv")
  dfb <- data.frame(group_B)
  names(dfb)<-c("dv")
  df <- rbind(dfa, dfb)
  
  df$SubjID <-factor(seq_along(df[,1]))
  df$Cond <- factor(rep(c(1,2),each=n))
  df$Replication2 <- factor(rep(c("food","elephant"),times=n)) #Naming the variable "Replication" so as to be consistent with Erber & Wegner's (1992) terminology.
  df$Replication4 <- factor(rep(c("White Bear","Elephant", "Giraffe", "Lion"), 4, length.out = 2*n)) #target words are placeholders only.
  
  t_test <- t.test(group_A, group_B, paired=FALSE, var.equal=TRUE)
  save_ps[i] <- t_test$p.value
  save_ts[i] <- t_test$statistic
  
  df$pvalue <- t_test$p.value
  df$tvalue <- t_test$statistic
  
  df$i <- i  # keep track of which iteration produced the data
  datalist[[i]] <- df # add iteration number to list. This code must come at the end after all variables are created.
}

#Reminder: df = single lab data. Strictly speaking, the above code would give the kth dataset.
#But the kth dataset would be no different whether k = 1 or 100.
#However, keeping the loop makes the simulation more flexible. One can choose to use the extend() function on the single-lab data or perform an overall simulation on the multi-lab data.

#Verify that the four replicates are  unevenly distributed (You cannot evenly distribute 4 replicates across 34 participants): = 34)
table(df$Replication4)

#Combine to form multi-lab data.
multilab.df = do.call(rbind, datalist)
multilab.df$Subjid <-factor(seq_along(multilab.df[,1])) #To simulate unique identifier but this must take place outside the loop. See also: https://stats.stackexchange.com/questions/242821/how-will-random-effects-with-only-1-observation-affect-a-generalized-linear-mixe

#Get descriptive stats (not necessary actually)
describeBy(multilab.df$dv, multilab.df$Cond) #SD for each group should be approx 7.6.
describeBy(df$dv, df$Cond)

#Get Cohen's d; serves to check whether the simulation mirrors Wegner et al. (1987)'s effect size
cohensD(multilab.df$dv ~ multilab.df$Cond) #Cohen's d should be approx .77, because it was 0.77 in Wegner et al. (1987).
cohensD(df$dv ~ df$Cond) #Ideally, Cohen's d should be approx .77, but this is expected to be unreliable because of the small sample size for a single lab.

#Specify the mixed effects models (single lab)
df2a <- lmer(dv ~ Cond*Replication2 + (1|SubjID) + (1|Replication2), data=df) #With four target words. ) #NOT RUN.
  # Error! Model cannot be specified this way because SubjID is unique (non-repeated because there are no within-subject factors).

df2b <- lmer(dv ~ Cond*Replication2 + (1|Replication2), data=df) #NOT RUN.
df2b #random intercept of target words

df2b.multilab <- lmer(dv ~ Cond*Replication2 + (1|Replication2), data=multilab.df) #NOT RUN.
df2b.multilab #random intercept of target words. Beta of Cond = 5.9815. 

df4a <- lmer(dv ~ Cond*Replication4 + (1|SubjID) + (1|Replication4), data=df) #With four target words.
  # Error! Model cannot be specified this way because SubjID is unique (non-repeated because there are no within-subject factors). NOT RUN.

df4b <- lmer(dv ~ Cond*Replication4 + (1|Replication4), data=df) #With four target words
df4b  #random intercept of target words. #NOT RUN.

df4b.multilab <- lmer(dv ~ Cond*Replication4 + (1|Replication4), data=multilab.df)
df4b.multilab #random intercept of target words; Cond & Replication as fixed effects. 

df5b.multilab <- lmer(dv ~ Cond + (1|Replication4), data=multilab.df)
df5b.multilab #random intercept of target words, Cond as fixed effect.

#Define fixed effects for power analysis
m <- 3.85 #This is the conversion factor, which is .77/.20.

fixef(df4b)["Cond2"] #Current effect size. #NOT RUN.
fixef(df4b)["Cond2"] <- (fixef(df4b)["Cond2"])/m #3.85 times smaller than current effect size
fixef(df4b)["Cond2"] 

fixef(df4b.multilab)["Cond2"] #Current effect size
fixef(df4b.multilab)["Cond2"] <- (fixef(df4b.multilab)["Cond2"])/m #3.85 times smaller than current effect size
fixef(df4b.multilab)["Cond2"] 

fixef(df5b.multilab)["Cond2"] #Current effect size
fixef(df5b.multilab)["Cond2"] <- (fixef(df5b.multilab)["Cond2"])/m #3.85 times smaller than current effect size
fixef(df5b.multilab)["Cond2"] 

#Power  analysis
j <- 5000 #number of simulations. Can change depending on needs.
power.df4b <- powerSim(df4b, test = fixed("Cond2", method = "t"), nsim = j)
power.df4b  #random intercept of target words.

power.df4b.multilab <- powerSim(df4b.multilab, test = fixed("Cond2", method = "t"), nsim = j)
power.df4b.multilab  #random intercept of target words.

power.df5b.multilab <- powerSim(df5b.multilab, test = fixed("Cond2", method = "t"), nsim = j)
power.df5b.multilab  #random intercept of target words.

#View warnings
lastResult()$warnings 
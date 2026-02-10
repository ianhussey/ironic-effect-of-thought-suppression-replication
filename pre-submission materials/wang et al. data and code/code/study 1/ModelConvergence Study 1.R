library(lme4)

#### Set up empty lists. ####
results <- list()
file_list = list.files(pattern="*.xlsx") #get all data with .xlsx
convergence_status <- character(length(file_list))

#### #Model m1 ####
for (i in 1:37) {
  filename = file_list[[i]]
  
  RRR_WE92 <- read_excel(filename, sheet = "testdata", na = "NA")
  
  # Rename columns for consistency
  colnames(RRR_WE92) <- recode(colnames(RRR_WE92),
                               "Participant ID" = "SubjID",
                               "Time pressure order" = "TimePressureOrder",
                               "Target word" = "Replication",
                               "Condition" = "Cond"
  )
  
  # Define categorical variables
  RRR_WE92 <- RRR_WE92 %>%
    mutate(
      gender = factor(gender),
      MC = factor(MC),
      Cond = factor(Cond),
      Replication = factor(Replication),
      TimePressureOrder = factor(TimePressureOrder)
    )
  
  #Define levels of the categorical variables. I could define it in the above step, but it doesn't work.
  levels(RRR_WE92$gender) <- c("Female", "Male", "Other", "Prefer not to say")
  levels(RRR_WE92$MC) <- c("Think", "Not think")
  levels(RRR_WE92$Cond) <- c("Suppression", "Concentration")
  levels(RRR_WE92$Replication) <- c("mountain", "car", "child", "house")
  levels(RRR_WE92$TimePressureOrder) <- c("10s then 3s", "2s then 10s")
  
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
  
  #Remove rows with nontarget words, if needed
  long2 <- data_long[!(data_long$wordtype=="nontarget"),]
  
  fit <- tryCatch({
    m1 <- lmer(meanhit ~ Cond*timepressure*Replication + (1|SubjID) + (1|Replication), data = long2, control = lmerControl(check.conv.singular = "ignore")) 
    # Check convergence messages
    conv_messages <- m1@optinfo$conv$lme4$messages
    
    results[[i]] <- m1
    
    if (is.null(conv_messages)) {
      convergence_status[i] <- "Converged"
    } else {
      convergence_status[i] <- "Did not converge"
    }
    
  }, warning = function(w) {
    convergence_status[i] <- paste("Warning during fit:", conditionMessage(w))
    results[[i]] <- NULL
  }, error = function(e) {
    convergence_status[i] <- paste("Error during fit:", conditionMessage(e))
    results[[i]] <- NULL
  })
}

# Summary of convergence status
for (i in seq_along(convergence_status)) {
  cat(sprintf("Model %d: %s\n", i, convergence_status[i]))
} #These retrieves the Model number that had converged. The Model number corresponds to the file list number.


#### #Model m2 #### MUST RESET BEFORE RUNNING.
#### Set up empty lists. ####
results <- list()
file_list = list.files(pattern="*.xlsx") #get all data with .xlsx
convergence_status <- character(length(file_list))

for (i in 1:37) {
  filename = file_list[[i]]
  
  RRR_WE92 <- read_excel(filename, sheet = "testdata", na = "NA")
  
  # Rename columns for consistency
  colnames(RRR_WE92) <- recode(colnames(RRR_WE92),
                               "Participant ID" = "SubjID",
                               "Time pressure order" = "TimePressureOrder",
                               "Target word" = "Replication",
                               "Condition" = "Cond"
  )
  
  # Define categorical variables
  RRR_WE92 <- RRR_WE92 %>%
    mutate(
      gender = factor(gender),
      MC = factor(MC),
      Cond = factor(Cond),
      Replication = factor(Replication),
      TimePressureOrder = factor(TimePressureOrder)
    )
  
  #Define levels of the categorical variables. I could define it in the above step, but it doesn't work.
  levels(RRR_WE92$gender) <- c("Female", "Male", "Other", "Prefer not to say")
  levels(RRR_WE92$MC) <- c("Think", "Not think")
  levels(RRR_WE92$Cond) <- c("Suppression", "Concentration")
  levels(RRR_WE92$Replication) <- c("mountain", "car", "child", "house")
  levels(RRR_WE92$TimePressureOrder) <- c("10s then 3s", "2s then 10s")
  
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
  
  #Remove rows with nontarget words, if needed
  long2 <- data_long[!(data_long$wordtype=="nontarget"),]
  
  fit <- tryCatch({
    m2 <- lmer(meanhit ~ Cond*timepressure*wordtype + (1|SubjID), data = data_long, control = lmerControl(check.conv.singular = "ignore")) 
    # Check convergence messages
    conv_messages <- m1@optinfo$conv$lme4$messages
    
    results[[i]] <- m2
    
    if (is.null(conv_messages)) {
      convergence_status[i] <- "Converged"
    } else {
      convergence_status[i] <- "Did not converge"
    }
    
  }, warning = function(w) {
    convergence_status[i] <- paste("Warning during fit:", conditionMessage(w))
    results[[i]] <- NULL
  }, error = function(e) {
    convergence_status[i] <- paste("Error during fit:", conditionMessage(e))
    results[[i]] <- NULL
  })
}

# Summary of convergence status
for (i in seq_along(convergence_status)) {
  cat(sprintf("Model %d: %s\n", i, convergence_status[i]))
} #These retrieves the Model number that had converged. The Model number corresponds to the file list number.

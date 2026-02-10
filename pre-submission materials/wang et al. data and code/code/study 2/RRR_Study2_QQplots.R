#Diagnostics: Inspect QQ plots for normality violation (don't run within loop)


file_list <- list.files(pattern="*.xlsx") 
data_list <- vector("list", length = length(file_list)) 

png("C:/Users/jc506932/Dropbox/RRR (Adam Wang)/Wegner87_QQplots.png",
    width = 21, height = 29.7, units = "cm", res = 600)

par(mfrow = c(7, 6), mar = c(2,2,2,1))

for (i in seq_along(file_list)) {
  if (i == 1) next # skip first lab
  
  filename <- file_list[i]
  message("Processing file ", i, " of ", length(file_list), ": ", filename)
  
  RRR_Wegner87 <- read_excel(filename, sheet = "testdata")
  
  colnames(RRR_Wegner87) <- recode(colnames(RRR_Wegner87),
                                   "ParticipantID" = "SubjID",
                                   "Condition" = "Cond",
                                   "sum.of.ABC_period.1.final" = "intrusions1",
                                   "sum.ABC_period2.final" = "intrusions2")
  RRR_Wegner87$intrusions2 <- as.numeric(RRR_Wegner87$intrusions2)
  RRR_Wegner87$t.intrusions2 <- sqrt(RRR_Wegner87$intrusions2)
  
  # Shapiro-Wilk and KS tests
  shapiro_res.raw <- shapiro.test(RRR_Wegner87$intrusions2)
  ks_res.raw <- ks.test(RRR_Wegner87$intrusions2, "pnorm", mean=mean(RRR_Wegner87$intrusions2), sd=sd(RRR_Wegner87$intrusions2))
  
  shapiro_res.transf <- shapiro.test(RRR_Wegner87$t.intrusions2)
  ks_res.transf <- ks.test(RRR_Wegner87$t.intrusions2, "pnorm", mean=mean(RRR_Wegner87$intrusions2), sd=sd(RRR_Wegner87$intrusions2))
  
  # Save results in a data.frame
  df_res <- data.frame(
    Lab = RRR_Wegner87$Lab[[1]],
    shapiro.raw.p = shapiro_res.raw$p.value,
    KS.raw.p = ks_res.raw$p.value,
    shapiro.transf.p = shapiro_res.transf$p.value,
    KS.transf.p = ks_res.transf$p.value
  )
  
  data_list[[i]] <- df_res  # store in list
  # Create Q-Q plot
  qqnorm(RRR_Wegner87$intrusions2, main = paste0("Lab ", i),
         cex.axis = 0.6, cex.main = 0.7)
  qqline(RRR_Wegner87$intrusions2, col = "red", lwd = 1)
}
dev.off()

# Combine all results
df.normality <- do.call(rbind, data_list)

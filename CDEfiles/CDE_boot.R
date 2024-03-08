n_boot <- 1000

ids <- unique(c(df_a0$id,df_a1$id))
n_ids <- length(ids)

boot_cde <- lapply(1:n_boot, function(x) list())

CDEa0_Mat <- (1/(pi_df$pi[pi_df$ia.base == 0] * cdea0$WMat)) * cdea0$RiskMat
CDEa1_Mat <- (1/(pi_df$pi[pi_df$ia.base == 1] * cdea1$WMat)) * cdea1$RiskMat


for(i in 1:n_boot){
  
  print(i)

  boot_ids_a0 <- sample(1:length(df_a0$id), size = length(df_a0$id), replace = T)
  boot_ids_a1 <- sample(1:length(df_a1$id), size = length(df_a1$id), replace = T)
  
  boot_CDEa0 <- apply(CDEa0_Mat[boot_ids_a0,], 2, sum) / n_ids
  boot_CDEa1 <- apply(CDEa1_Mat[boot_ids_a1,], 2, sum) / n_ids
  boot_CDE <- boot_CDEa1 - boot_CDEa0
  
  boot_cde[[i]]$boot_CDEa0 <- boot_CDEa0
  boot_cde[[i]]$boot_CDEa1 <- boot_CDEa1
  boot_cde[[i]]$boot_CDE <- boot_CDE
  
}



mat <- t(sapply(1:1000, function(x) boot_cde[[x]][[3]]))

sd_CDE <- numeric(365)

for(i in 1:365){
  sd_CDE[i] <- sd(mat[,i])
}

quant_CDE <- matrix(NA, nrow = 365, ncol = 2)

for(i in 1:365){
  quant_CDE[i,] <-  quantile(mat[,i], probs = c(0.025, 0.975))
}


#save(boot_cde, file = "N:/durable/scrpts/OCBE/Working_files/Separable_effects/Sep_eff_final/For_Git/scripts/scriptsNew/NewBootPlot/BootCDE.Rdata")



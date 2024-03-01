n_boot <- 1000

ids <- unique(c(df_a0$id,df_a1$id))
n_ids <- length(ids)

boot_cde <- lapply(1:n_boot, function(x) list())

CDEa0_Mat <- apply((1/(pi$pi[pi$ia.base == a] * cdea0$WMat)) * cdea0$RiskMat, 2, sum) / n_ids
CDEa1_Mat <- apply((1/(pi$pi[pi$ia.base == a] * cdea1$WMat)) * cdea1$RiskMat, 2, sum) / n_ids


for(i in 1:n_boot){
  
  print(i)
  boot_ids <- sample(1:n_ids, size = n_ids, replace = T)
  
  boot_ids_a0 <- boot_ids[boot_ids %in% sbs2_a0$id]
  boot_ids_a1 <- boot_ids[boot_ids %in% sbs2_a1$id]
  
  which_rep_a0 <- match(boot_ids_a0, sbs2_a0$id)
  which_rep_a1 <- match(boot_ids_a1, sbs2_a1$id)
  
  boot_cdea0 <- apply(CDEa0_Mat[which_rep_a0,], 2, sum) / n_ids
  boot_cdea1 <- apply(CDEa1_Mat[which_rep_a1,], 2, sum) / n_ids
  boot_cde <- boot_cdea1 - boot_cdea0
  
  boot_cde[[i]]$boot_cdea0 <- boot_cdea0
  boot_cde[[i]]$boot_cdea1 <- boot_cdea1
  boot_cde[[i]]$boot_cde <- boot_cde
  
}



mat <- t(sapply(1:1000, function(x) boot_cde[[x]][[3]]))

sd_CDE <- numeric(365)

for(i in 1:365){
  sd_CDE[i] <- sd(mat[,i])
}




n_boot <- 1000

ids <- unique(c(df_a0$id,df_a1$id))
n_ids <- length(ids)

boot_SE <- lapply(1:n_boot, function(x) list())


for(i in 1:n_boot){
  
  print(i)
  boot_ids <- sample(1:n_ids, size = n_ids, replace = T)
  
  boot_ids_a0 <- boot_ids[boot_ids %in% df_a0$id]
  boot_ids_a1 <- boot_ids[boot_ids %in% df_a1$id]
  
  which_rep_a0 <- match(boot_ids_a0, df_a0$id)
  which_rep_a1 <- match(boot_ids_a1, df_a1$id)
  
  boot_CIFG_aW1aG0 <- apply(NGaW1aG0[which_rep_a0,], 2, sum) / n_ids
  boot_CIFGa0 <- apply(NGa0Mat[which_rep_a0,], 2, sum) / n_ids
  boot_CIFWa0 <- apply(NWa0Mat[which_rep_a0,], 2, sum) / n_ids
  
  boot_CIFW_aW1aG0 <- apply(NWaW1aG0[which_rep_a1,], 2, sum) / n_ids
  boot_CIFWa1 <- apply(NWa1Mat[which_rep_a1,], 2, sum) / n_ids
  boot_CIFGa1 <- apply(NGa1Mat[which_rep_a1,], 2, sum) / n_ids

  
  boot_TE_G <- boot_CIFGa1 - boot_CIFGa0
  boot_SE1_G <- boot_CIFG_aW1aG0 - boot_CIFGa0
  boot_SE2_G <- boot_CIFGa1 - boot_CIFG_aW1aG0
  
  boot_TE_W <- boot_CIFWa1 - boot_CIFWa0
  boot_SE1_W <- boot_CIFW_aW1aG0 - boot_CIFWa0
  boot_SE2_W <- boot_CIFWa1 - boot_CIFW_aW1aG0
  
  boot_SE[[i]]$boot_TE_G <- boot_TE_G
  boot_SE[[i]]$boot_SE1_G <- boot_SE1_G
  boot_SE[[i]]$boot_SE2_G <- boot_SE2_G
  boot_SE[[i]]$boot_TE_W <- boot_TE_W
  boot_SE[[i]]$boot_SE1_W <- boot_SE1_W
  boot_SE[[i]]$boot_SE2_W <- boot_SE2_W
  
  
}


mat_TE_G <- t(sapply(1:1000, function(x) boot_SE[[x]][[1]]))
mat_SE1_G <- t(sapply(1:1000, function(x) boot_SE[[x]][[2]]))
mat_SE2_G <- t(sapply(1:1000, function(x) boot_SE[[x]][[3]]))
mat_TE_W <- t(sapply(1:1000, function(x) boot_SE[[x]][[4]]))
mat_SE1_W <- t(sapply(1:1000, function(x) boot_SE[[x]][[5]]))
mat_SE2_W <- t(sapply(1:1000, function(x) boot_SE[[x]][[6]]))



sd_teG <- sd_se1G <- sd_se2G <- sd_teW <- sd_se1W <- sd_se2W <- numeric(365)

for(i in 1:365){
  sd_teG[i] <- sd(mat_TE_G[,i])
  sd_se1G[i] <- sd(mat_SE1_G[,i])
  sd_se2G[i] <- sd(mat_SE2_G[,i])
  sd_teW[i] <- sd(mat_TE_W[,i])
  sd_se1W[i] <- sd(mat_SE1_W[,i])
  sd_se2W[i] <- sd(mat_SE2_W[,i])  
}




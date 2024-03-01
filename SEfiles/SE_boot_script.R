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
  
  boot_S1 <- apply(S1Mat[which_rep_a0,], 2, sum) / n_ids
  boot_Sa0 <- apply(Sa0Mat[which_rep_a0,], 2, sum) / n_ids
  
  boot_S2 <- apply(S2Mat[which_rep_a1,], 2, sum) / n_ids
  boot_Sa1 <- apply(Sa1Mat[which_rep_a1,], 2, sum) / n_ids
  
  boot_TE <- boot_Sa1 - boot_Sa0
  boot_SE1 <- boot_S1 - boot_Sa0
  boot_SE2 <- boot_Sa1 - boot_S2
  
  boot_SE[[i]]$boot_TE <- boot_TE
  boot_SE[[i]]$boot_SE1 <- boot_SE1
  boot_SE[[i]]$boot_SE2 <- boot_SE2
  
  
}



mat_TE <- t(sapply(1:1000, function(x) boot_SE[[x]][[1]]))
mat_SE1 <- t(sapply(1:1000, function(x) boot_SE[[x]][[2]]))
mat_SE2 <- t(sapply(1:1000, function(x) boot_SE[[x]][[3]]))


sd_te <- sd_se1 <- sd_se2 <- numeric(365)

for(i in 1:365){
  sd_te[i] <- sd(mat_TE[,i])
  sd_se1[i] <- sd(mat_SE1[,i])
  sd_se2[i] <- sd(mat_SE2[,i])
}




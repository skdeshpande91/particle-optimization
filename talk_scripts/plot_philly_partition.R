### Show one particular partition of the neighborhoods ###
# In particular show the same partition as 


source("scripts/plot_functions.R")

# Make MLE plots
Y <- read.csv("data/Y_tracts.csv", header = F)
X <- read.csv("data/X_tracts.csv", header = F)
Y <- as.matrix(Y)
X <- as.matrix(X)
X <- X - rowMeans(X)
betas.mle <- numeric(n_tr)
for(i in 1:n_tr)
  betas.mle[i] <- cov(Y[i,],X[i,])/var(X[i,])
alphas.mle <- rowMeans(Y) - log(2)

phillypoly <- tracts
polyfortified <- fortify(phillypoly)

strings <- c("results/EPPrior5_0_tracts2_L10KMInitializeIS_opts02newhyper2_K6lambda100islandFIXED_kmrep.rdata",
             "results/UnifPrior_tracts2_L10KMInitializeIS_opts02newhyper2_K6lambda100islandFIXED_kmrep.rdata",
             "results/UnifEPPrior5_0_tracts2_L10KMInitializeIS_opts02newhyper2_K6lambda100islandFIXED_kmrep.rdata",
             "results/EPUnifPrior5_0_tracts2_L10KMInitializeIS_opts02newhyper2_K6lambda100islandFIXED_kmrep.rdata")

tracts_fort <- fortify(tracts)

# We only want to plot the ep-unif partition
input_string <- strings[3]
load(input_string)

i <- 3
j <- which(tmp$w==max(tmp$w))
partitionA <- tmp$particle_set_A[[j]]
partitionB <- tmp$particle_set_B[[j]]

alpha_particle <- tmp$alpha_particle[,j] - log(2)
beta_particle <- tmp$beta_particle[,j]


z1 <- numeric(n_tr)
for(k in 1:length(partitionA)){
  z1[partitionA[[k]]] <- k
}
z2 <- numeric(n_tr)
for(k in 1:length(partitionB)){
  z2[partitionB[[k]]] <- k
}

limitsA <- range(c(alpha_particle, alphas.mle))
limitsB <- range(c(beta_particle, betas.mle))

alpha_mle_partition <- plot_borders_ggplot(clusters_id = z1, phillypoly = tracts,
                                       w.sym = w.sym, var = alphas.mle, limits = limitsA,
                                       polyfortified = tracts_fort, palette_num = 3)

ggsave("alpha_mle_partition.png", plot = alpha_mle_partition, device = "png", path = "../talk_figures/",
       width = 2, height = 2, units = "cm", scale = 10)

beta_mle_partition <- plot_borders_ggplot(clusters_id = z2, phillypoly = tracts,
                                       w.sym = w.sym, var = betas.mle, limits = limitsB,
                                       polyfortified = tracts_fort, palette_num = 5)
ggsave("beta_mle_partition.png", plot = beta_mle_partition, device = "png", path = "../talk_figures/",
       width = 2, height = 2, units = "cm", scale = 10)



alpha_particle_partition <- plot_borders_ggplot(clusters_id = z1, phillypoly = tracts,
                                           w.sym = w.sym, var = alpha_particle, limits = limitsA,
                                           polyfortified = tracts_fort, palette_num = 3)

ggsave("alpha_particle_partition.png", plot = alpha_particle_partition, device = "png", path = "../talk_figures/",
       width = 2, height = 2, units = "cm", scale = 10)

beta_particle_partition <- plot_borders_ggplot(clusters_id = z2, phillypoly = tracts,
                                          w.sym = w.sym, var = beta_particle, limits = limitsB,
                                          polyfortified = tracts_fort, palette_num = 5)
ggsave("beta_particle_partition.png", plot = beta_particle_partition, device = "png", path = "../talk_figures/",
       width = 2, height = 2, units = "cm", scale = 10)



for(prior in 1:4){ 
  i=prior
  if(prior == 1){
    output_filename <-  "tract_ep_newhyp2K6_bestpart.png"
  } else if(prior == 2){
    output_filename <-  "tract_unif_newhyp2K6_bestpart.png" 
  } else if(prior == 3){
    output_filename <-  "tract_epunif_newhyp2K6_bestpart.png"
  } else if(prior == 4){
    output_filename <-  "tract_unifep_newhyp2K6_bestpart.png"
  }
  ps[[1]] <- plot_borders_ggplot(clusters_id = z1s[,i], phillypoly = tracts, 
                                 w.sym = w.sym, var = alphas[,i], limits = limitsA, polyfortified = tracts_fort, 
                                 title = paste("Mean level"), palette_num = 3,
                                 legend = TRUE)
  ps[[2]] <- plot_borders_ggplot(clusters_id = z2s[,i], phillypoly = tracts, 
                                 w.sym = w.sym, var = betas[,i], limits = limitsB, polyfortified = tracts_fort,
                                 title = paste("Time trend"), palette_num = 5,
                                 legend = TRUE)
  
  ggsave(filename = output_filename, plot = arrangeGrob(grobs = ps, nrow = 1), device = "png", path = "figures/",
         width = 5, height = 2.5, units = "cm", scale = 6)
}



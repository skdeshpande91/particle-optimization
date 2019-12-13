## Make another plot of the MLEs ##
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
# note we need to subtract log(2) because the inverse  hyperbolic since transformation (ihst) function use
# did not have the shift factor "-log(2)". Instead of re-running the algorithm, we can shift the alpha found.

phillypoly <- tracts
polyfortified <- fortify(phillypoly)
alpha_mle_base <- plot_borders_ggplot(rep(0, n_tr), phillypoly, w.sym, var = alphas.mle, polyfortified = polyfortified, legend = FALSE,  palette_num = 3)
ggsave(filename = "alpha_mle_base.png", plot = alpha_mle_base, device = "png", path = "../talk_figures/",
       width = 2, height = 2, units = "cm", scale = 10)

beta_mle_base <- plot_borders_ggplot(rep(0, n_tr), phillypoly, w.sym, var = betas.mle, polyfortified = polyfortified, legend = FALSE, palette_num = 5)
ggsave(filename = "beta_mle_base.png", plot = beta_mle_base, device = "png", path = "../talk_figures/",
       width = 2, height = 2, units = "cm", scale = 10)


p2 <- plot_borders_ggplot(rep(0, n_tr), phillypoly, w.sym, var = betas.mle, polyfortified = polyfortified, legend = TRUE, 
                          legend_name = "Time\ntrend", palette_num = 5, map = googlemap)
ggsave(filename = "alpha_mle.png", plot = p1, device = "png", path = "figures/",
       width = 2, height = 2, units = "cm", scale = 10)
ggsave(filename = "beta_mle.png", plot = p2, device = "png", path = "figures/",
       width = 2, height = 2, units = "cm", scale = 10)
### Some new plots for SKD's job talk

## spaghetti plot showing crime in most of the neighborhoods

Y <- read.csv("data/Y_tracts.csv", header = F)
X <- read.csv("data/X_tracts.csv", header = F)
Y <- as.matrix(Y)
X <- as.matrix(X)
X <- X - rowMeans(X)

n_tr <- nrow(X)

betas.mle <- numeric(n_tr)
for(i in 1:n_tr)
  betas.mle[i] <- cov(Y[i,],X[i,])/var(X[i,])
alphas.mle <- rowMeans(Y) - log(2)


which.max(betas.mle)
which.min(betas.mle)

# Definitely show tract 327: moderate increase
# Definitely show tract 290: moderate decrease
# Maybe show 133?

png("../talk_figures/raw_crime_plot.png", width = 4, height = 4, units = "in", res = 300)
par(mar = c(3,3,2,1), mgp = c(1.5, 0.8, 0), cex.axis = 0.9, cex.main = 1, cex.lab = 0.9)
plot(1, type = "n", xlab = "Year", ylab = expression(y[it]), main = "Log crime (2006 - 2017)", xlim = c(0,11), ylim = c(3, 7), xaxt = "n")
axis(side = 1, at = seq(0, 11, by = 2), labels = as.character(seq(2006, 2017, by = 2)))
for(i in 1:150){
  if(all(Y[i,] > 3)){
    points(0:11, Y[i,], pch = 16, cex = 0.4, col = 'gray')
    lines(0:11, Y[i,], col = 'gray', lwd = 0.5)
  }

}
my_index <- c(327, 290, 275,22, 34, 291)
col_list <- c("red", "blue", "orange", "purple", "cyan", "green")
for(i in 1:length(my_index)){
  points(0:11, Y[my_index[i],], pch = 16, cex = 0.8, col = col_list[i])
  lines(0:11, Y[my_index[i],], col = col_list[i], lwd = 0.75)
}

dev.off()


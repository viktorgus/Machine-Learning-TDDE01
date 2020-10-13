library("ggplot2")
library("fastICA")

set.seed(12345)
data <- data.frame(read.csv2("NIRSpectra.csv"))
data.features <- subset(data, select = -c(Viscosity))
pca.fit <-prcomp(x = data.features, center = TRUE, scale. = TRUE)
print(pca.fit)
plot(pca.fit, type="l")
# Proportion of variance for PC1 & PC2 = 0.99614
qplot(x=pca.fit$x[,1], y=pca.fit$x[,2], data=pca.fit$scores)
# 2.2

plot(pca.fit$rotation[,1], main = "Traceplot, PC1")
plot(pca.fit$rotation[,2], main = "Traceplot, PC2")

# 2.3
set.seed(12345)
a <- fastICA(X = data, 2, alg.typ = "parallel", fun = "logcosh", alpha = 1, method = "R", row.norm = FALSE, maxit = 200, tol = 0.0001, verbose = TRUE)
W.prim = a$K %*% a$W
plot(W.prim[,1], main = "Traceplot, W.prim 1")
plot(W.prim[,2], main = "Traceplot, W.prim 2")
qplot(x = a$S[,1], y = a$S[,2], data = data.features, xlab = "Latent feature 1", ylab = "Latent feature 2")

library(gld)
library(boot)
library(moments)
library(robustbase)
library(MASS)

png('Skew_Demo.png', 1080, 1080)
par(mfrow = c(2,2))
par(cex.axis = 1.6)
par(cex.main = 1.4)
par(cex.lab = 1.52)
par(font.main = 1)

gld.moments(c(0.0, 1.7, 0.02, 0.03))
gld.moments(c(0.0, 1.0, 7.2, 0.6))
gld.moments(c(4.0, 6.0, 12.2, 0.36))
gld.moments(c(4.0, 0.6, 32.2, 0.036))

set.seed(1240)
r1 <- rgl(300, 0.0, 1.7, 0.02, 0.03)
c(Skew = skewness(r1), Medcouple = mc(r1))
set.seed(1240)
r2 <- rgl(300, 0.0, 1.0, 7.2, 0.6)
c(Skew = skewness(r2), Medcouple = mc(r2))
set.seed(1240)
r3 <- rgl(300, 4.0, 6.0, 12.2, 0.36)
c(Skew = skewness(r3), Medcouple = mc(r3))
set.seed(1240)
r4 <- rgl(300, 4.0, 0.6, 32.2, 0.036)
c(Skew = skewness(r4), Medcouple = mc(r4))

hist(r1, main = 'Symmetric (Skew = 0.15, MC = -0.05)', xlab = 'Sample Values',
        breaks = 'FD', freq = FALSE, col = 'sandybrown')
lines(density(r1), col = 'blue', lwd = 2.1)
hist(r2, main = 'Moderate Skew (Skew = 0.57, MC = 0.15)', xlab = 'Sample Values',
        breaks = 'FD', freq = FALSE, col = 'sandybrown')
lines(density(r2), col = 'blue', lwd = 2.1)
hist(r3, main = 'More Skewed (Skew = 0.96, MC = 0.21)', xlab = 'Sample Values',
        breaks = 'FD', freq = FALSE, col = 'sandybrown')
lines(density(r3), col = 'blue', lwd = 2.1)
hist(r4, main = 'Heavy Skew (Skew = 2.48, MC = 0.30)', xlab = 'Sample Values',
    breaks = 'FD', freq = FALSE, col = 'sandybrown')
lines(density(r4), col = 'blue', lwd = 2.1)
dev.off()

png('Skew_Densities.png', 1080, 1080)
par(mfrow = c(2,2))
par(cex.axis = 1.6)
par(cex.main = 1.4)
par(cex.lab = 1.52)
par(font.main = 1)
curve(dgl(x, 0.0, 1.7, 0.02, 0.03), from = -4, to = 4,
        ylab = 'Density', main = 'Symmetric Density')
curve(dgl(x, 0.0, 1.0, 7.20, 0.60), from = -0.1, to = 1.52,
        ylab = 'Density', main = 'Positive Skew')
curve(dgl(x, 4.0, 12.0, 23.2, 3.36), from = 4, to = 4.01,
        ylab = 'Density', main = 'Negative Skew')
curve(dgl(x, 0.0, 1.7, 0.02, 0.03), from = 3, to = 15,
        ylab = 'Density', main = 'Heavy Positive Skew')
dev.off()

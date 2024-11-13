# make data for example analysis 1
set.seed(231011)
n_blocks <- 10

intercepts <- rnorm(n_blocks, 20, 8)
slopes <- rnorm(n_blocks, 2, 2)
error <- 2
bdat <- data.frame(
  plot = 1:3,
  block = rep(1:n_blocks, each = 3),
  diversity = runif(n_blocks * 3, 1, 10),
  biomass = NA
)
bdat$biomass <- intercepts[bdat$block] + slopes[bdat$block] * bdat$diversity + rnorm(nrow(bdat), 0, error)
bdat <- bdat[bdat$block != 3 | bdat$plot == bdat$block, ]

write.csv(bdat, "dataset_1.csv", row.names = FALSE)

# plot
par(mar = c(4, 4, 2, 2))
plot(biomass ~ diversity, col = block, data = bdat)
for (i in 1:10) {
  if (i == 3) {
    abline(intercepts[i], slopes[i], col = i, lty = 2)
  } else {
    abline(intercepts[i], slopes[i], col = i)
  }
}


# analyse
require(nlme)
mod <- lme(biomass ~ diversity, random = ~ 1 + diversity | block, data = bdat)
summary(mod)

fixef(mod)
ranef(mod)

VarCorr(mod)


mod0 <- lme(biomass ~ diversity,
           random = ~ 1 + diversity | block, data = bdat)
mod1 <- lme(biomass ~ diversity,
           random = ~ 1 | block, data = bdat)
mod2 <- gls(biomass ~ diversity,
            data = bdat)

anova(mod0, mod1)
anova(mod1, mod2)


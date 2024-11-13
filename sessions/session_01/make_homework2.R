# make data for example analysis 1
set.seed(231107)
n_blocks = 10

intercepts = rnorm(n_blocks, 14, 7)
slopes = 5
error = 5
bdat = data.frame(plot = 1:3,
                  block = rep(1:n_blocks, each = 3),
                  diversity = runif(n_blocks*3, 1, 10),
                  biomass = NA)
bdat$biomass=intercepts[bdat$block] + slopes*bdat$diversity + rnorm(nrow(bdat), 0, error)
bdat=bdat[bdat$block!=3 | bdat$plot==bdat$block,]

write.csv(bdat, "dataset_2.csv", row.names = FALSE)

# plot
par(mar=c(4,4,2,2))
plot(biomass~diversity, col = block, data = bdat)
for(i in 1:6) {
  if(i==3) {
    abline(intercepts[i], slopes, col = i, lty=2)
  } else {
    abline(intercepts[i], slopes, col = i)
  }
}


# analyse
require(nlme)
mod = lme(biomass~diversity, random = ~1|block, data = bdat)
summary(mod)

fixef(mod)
ranef(mod)

VarCorr(mod)

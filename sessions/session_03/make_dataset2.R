# make data for example analysis 1
set.seed(231011)
n_plots = 10
n_subplots = 6
n_reps = 5

intercepts_plots = rnorm(n_plots, 100, 8)
slopes_plots = rnorm(n_plots, 2, 3)
intercepts = matrix(nrow = n_plots, ncol = n_subplots)
for(i in 1:n_plots) {
  intercepts[i,] = rnorm(n_subplots, intercepts_plots[i], 5)
}

slopes = matrix(nrow = n_plots, ncol = n_subplots)
for(i in 1:n_plots) {
  slopes[i,] = rnorm(n_subplots, slopes_plots[i], 2)
}

error = 2

# "TRUE" result is:
# plot intercept variability: 8
# plot slope variability: 3
# plot/subplot intercept variability: 5
# plot/subplot slope variability: 2
# resid error: 2


bdat = data.frame(plot = rep(1:n_plots, each = n_subplots*n_reps),
                  subplot = rep(1:n_subplots, each = n_reps),
                  diversity = runif(n_plots*n_subplots*n_reps, 1, 10),
                  biomass = NA)

bdat$biomass=intercepts[cbind(bdat$plot, bdat$subplot)] + slopes[cbind(bdat$plot, bdat$subplot)]*bdat$diversity + rnorm(nrow(bdat), 0, error)

write.csv(bdat, "dataset_2.csv", row.names = FALSE)

# plot
par(mar=c(4,4,2,2))
plot(biomass~diversity, col = rainbow(n_plots)[plot], pch = subplot, data = bdat)


# analyse
require(nlme)
mod0 = glm(biomass~1, data = bdat) # note - if we want to fit a model without random effects, we need to use "glm"
mod1 = lme(biomass~1, random = ~1|plot, data = bdat, method = "ML")
AIC(mod0, mod1) # more complex model (mod1) is an improvement

mod2 = lme(biomass~diversity, random = ~1|plot, data = bdat, method = "ML")
anova(mod1, mod2) # significant improvement with more complex model

mod3 = lme(biomass~diversity, random = ~1+diversity|plot, data = bdat, method = "ML")
anova(mod2, mod3) # significant improvement with more complex model

# now we need to get a bit creative
mod4 = lme(biomass~diversity, random = ~1|plot/subplot, data = bdat, method = "ML")
anova(mod2, mod4) # significant improvement with more complex model

AIC(mod3, mod4) # mod3 performs better

mod5 = lme(biomass~diversity, random = ~1+diversity|plot/subplot, data = bdat, method = "ML")
anova(mod4, mod5) # significant improvement with more complex model

# mod5 "wins" - re-fit with REML and interpret

mod5_reml = update(mod5, method = "REML")

summary(mod5_reml) # what do p-values tell us?
 
VarCorr(mod5_reml) # how do these compare to our expectations?

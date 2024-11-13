setwd("~/Dropbox/Teaching/Graz/BayesStats/BayesModellingUG/meetings/K5")
d = read.csv("e120_data.csv")

head(d)

############ Example 1: lme4
#install.packages("lme4")
require(lme4)

# fitting a simple model with plot as random effect
#require(nlme)
#mnlme = lme(AbvBio~NumSp, data = d, random = ~ 1|Plot)

m4_1 = lmer(AbvBio~NumSp+(1|Plot), data = d)
summary(m4_1)
#summary(mnlme)

# no p-values for fixed effects! need to test these via ANOVA
m4_2 = lmer(AbvBio~1+(1|Plot), data = d)
anova(m4_1, m4_2) # automatically re-fits with ML
# significant improvement from NumSp

# check residuals
plot(m4_1)

# no weighting options in lme4 - need to transform instead
m4_3 = lmer(log(AbvBio)~NumSp+(1|Plot), data = d)
plot(m4_3) # better
# repeat model selection
m4_4 = lmer(log(AbvBio)~1+(1|Plot), data = d)
anova(m4_3, m4_4) # significant improvement

# nested random effects
m4_5 = lmer(log(AbvBio)~NumSp+(1|Plot/Year), data = d)
# doesn't converge: too few observations

# crossed random effects
m4_5 = lmer(log(AbvBio)~NumSp+(1|Plot) + (1|Year), data = d)
# simpler form, but assumes that year effects don't vary by plot (and/or that plot effects don't vary by year)
summary(m4_5)
plot(m4_5)
anova(m4_3, m4_5) # significant improvement

ranef(m4_5)
coef(m4_5) # somewhat easier format for random effects - don't need to add to fixed effects


############ Example 2: brms
#install.packages("brms")
require(brms)

options(mc.cores = floor(parallel::detectCores()/3)) # set number of cores for analyses

mb_3 = brm(log(AbvBio)~NumSp+(1|Plot), data = d)
# takes longer, but uses same syntax as lme4

summary(mb_3)
# more useful summary

# can extract random effect estimates, with 95% CI
ranef(mb_3)
mb_3_post = as_draws_df(mb_3) # directly access posterior

# directly calculate p-value comparing, e.g., two plots
mean(mb_3_post$`r_Plot[2,Intercept]`<mb_3_post$`r_Plot[3,Intercept]`)

# plot MCMC chains
plot(mb_3)

# fit more complex nested model (will take longer)
mb_4 = brm(log(AbvBio)~NumSp+(1|Plot/Year), data = d)
# note - failures from both R-hat (convergence) and transition (optimiser steps)
# long and complicated solutions, discussed here: https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup

# worth a shot, but usually you need to update your model
mb_4 = brm(log(AbvBio)~NumSp+(1|Plot/Year), data = d, 
           control = list(adapt_delta = 0.999, stepsize = 0.01, max_treedepth = 15))
# better, but not perfect


# try different families
mb_5 = brm(AbvBio~NumSp+(1|Plot), data = d, family = "gamma")
summary(mb_5)
plot(mb_5)

# deal with difficult data (e.g. zeros, or percent cover)
# lots of families to try out - see ?brmsfamily
tmp = d[1:50,]
tmp$AbvBio = 0
d_tmp = rbind(d, tmp)

mb_5b = brm(bf(AbvBio~NumSp+(1|Plot), hu ~ 1),
            data = d_tmp, family = hurdle_gamma)
summary(mb_5b)
plot(mb_5b)
# notice hurdle term
fixef(mb_5b)

# add explicit priors
p6 = prior(normal(0,1), class = b, coef = NumSp)
mb_6 = brm(AbvBio~NumSp+(1|Plot), data = d[d$Plot %in% c(2,3,5,6,9),], family = "gamma", prior = p6)
summary(mb_6)
plot(mb_6)


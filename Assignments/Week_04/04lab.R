
######--- MATCHING WORKFLOW ---######

###--- TODAY
  ###--- Review
    # 1) Research question 
    # 2) Define an estimand
    # 3) Draw the DAG
    # 4) Check balance before matching
  ###--- New
    # 5) Estimate propensity scores
    # 6) Check model predictions

###--- NEXT WEEK
    # 7) PS Matching and Weighting
    # 8) Check balance after matching
    # 9) Compute estimates



######--- RESEARCH QUESTION ---######

# What is the effect of maternal smoking on infant health?

# Same data as last week: a subsample (N = 4642) of singleton births 
# in PA between 1989-1991. 


######--- DEFINE THE ESTIMAND ---######

# Theoretical estimand: For a mother who smokes, 
# how much more or less would their baby 
# weight if they did not smoke? 

# Y(1) - Y(0) for T = 1

# Empirical estimand: the average effect of smoking 
# on their babies birth weight 
# for mothers who smoke

# ATT = E[Yi(1) − Yi(0) ∣ T = 1]

# This requires:
# E[Y(1) | T = 1] 
# E[Y(0) | T = 1] 
# Which one is observable? 

# We do not observe all the potential outcomes 
# so we have to make assumptions:

# Because it was not an experiment, we know the independence
# assumption likely does not hold


# Conditional Independence assumption (a.k.a. no unmeasured confounders)

# Assumption 1 (CIA): conditional independence Y(1), Y(0) indep T | S
# Assumption 1a (CIA): E(Y1 | T = 1, S) = E(Y1 | T = 0, S) 
# Assumption 1b (CIA): E(Y0 | T = 1, S) = E(Y0 | T = 0, S)

# Questions 1: Which of these elements are observed?
# Question 2: Which of the two sub-assumptions we do not need for the ATT? 


# Assumption 2: Overlap or Common Support Assumption
# 0 < P(T = 1 | S) < 1

# Assumption 3: SUTVA


######--- DRAW THE DAG ---######

# Where the S variable represents: marriage, alcohol, race, first baby,
# age, education, and number of prenatal visits

dag <- dagitty::dagitty("dag {
  S -> smoke
  S -> weight
  smoke -> weight
}")

dagitty::coordinates(dag) <- list(
  x = c(S = 0.5, smoke = 0, weight = 1),
  y = c(S = -1,   smoke = 0, weight = 0)
)
# Plot the DAG
plot(dag)


# If all of this holds we then get that: 
# ATT = E(Y1 | T = 1, S) - E(Y0 | T = 0, S)


######--- CHECK BALANCE BEFORE MATCHING ---######

###--- Libraries 
library(tidyverse)
library(cobalt)
library(MatchIt)
theme_set(hrbrthemes::theme_ipsum())

###--- Load the data
tbl <- readRDS("data/cattaneo.rds")


# Because means do not perfectly describe a distribution, 
# there are multiple ways to check for balance: 
# https://kosukeimai.github.io/MatchIt/articles/assessing-balance.html

###--- Create the MatchIt object
ematch_out <- 
  matchit(mbsmoke ~ mmarried + alcohol + mrace + fbaby + mage + medu + nprenatal,
          data = tbl,
          method = NULL)

###--- Summary of balance before matching
summary(ematch_out, standardize = FALSE)
summary(ematch_out, standardize = TRUE)

###--- Visalize balance (with Cobalt)

###--- Love plot
love.plot(ematch_out, 
          binary = "std",
          drop.distance = TRUE,
          var.order = "unadjusted",
          colors = c("steelblue"), 
          thresholds = .1) +
  labs(title = "Covariate balance before matching")

###--- QQ plot
plot(ematch_out, 
     type = "qq", 
     which.xs = ~ mage + nprenatal + mmarried)


###--- Bar plot
bal.plot(ematch_out, 
         var.name = "mrace",
         which = "unadjusted")

###--- Histogram
bal.plot(ematch_out, 
         var.name = "mage",
         which = "unadjusted",
         type = "histogram", 
         mirror = TRUE)


###--- ECDF
bal.plot(ematch_out, 
         var.name = "mage",
         which = "unadjusted",
         type = "ecdf")


###--- IN CLASS: Compute SMDs manually and plot them
# SMD formula used by MatchIt: https://imai.fas.harvard.edu/research/files/matchit.pdf
s_vars <- c("mmarried", "alcohol", "mrace", "fbaby", "mage", "medu", "nprenatal")
treatment <- "smoker"


tbl_bal <- 
  tbl |> 
  select(all_of(c(s_vars, treatment)))


## my work

tbl_t <- tbl_bal |> 
  filter(smoker == "Smoker")

tbl_c <- tbl_bal |> 
  filter(smoker == "Nonsmoker")

smd_mmarried <- (mean(tbl_t$mmarried) - mean(tbl_c$mmarried))/sd(tbl_t$mmarried)
smd_alcohol <- (mean(tbl_t$alcohol) - mean(tbl_c$alcohol))/sd(tbl_t$alcohol)
smd_mrace <- (mean(tbl_t$mrace) - mean(tbl_c$mrace))/sd(tbl_t$mrace)
smd_fbaby <- (mean(tbl_t$fbaby) - mean(tbl_c$fbaby))/sd(tbl_t$fbaby)
smd_mage <- (mean(tbl_t$mage) - mean(tbl_c$mage))/sd(tbl_t$mage)
smd_medu <- (mean(tbl_t$medu) - mean(tbl_c$medu))/sd(tbl_t$medu)
smd_nprenatal <- (mean(tbl_t$nprenatal) - mean(tbl_c$nprenatal))/sd(tbl_t$nprenatal)

##

## pablo's way

tbl_final <- 
  tbl_bal |> 
  pivot_longer(cols = 1:7, names_to = "s_vars", values_to = "value") |> 
  group_by(smoker, s_vars) |> 
  summarise(mean = mean(value),
            sd = sd(value)) |> 
  arrange(s_vars) |> 
  pivot_wider(names_from = smoker, values_from = c(mean, sd)) |> 
  mutate(smd = (mean_Smoker - mean_Nonsmoker)/sd_Smoker)

## 



######--- PROPENSITY SCORE MODEL ---######

ps_model <- glm(mbsmoke ~ mmarried + alcohol + mrace + fbaby + 
                mage + medu + nprenatal, 
             data = tbl,
             family = binomial)

summary(ps_model)


###--- IN CLASS: Using the function predict() 
# generate predictions for the observed data in the log odds and probability scales
# Make a plot showing the relationship between the two scales


## my work
predict(ps_model, type)

tbl_predict <- 
  tbl_bal |> 
  mutate(log_odds = predict(ps_model),
         prob = predict(ps_model, type = "response"))

ggplot(tbl_predict, aes(log_odds, prob)) +
  geom_point() +
  theme_minimal()
##


###--- Propensity score by treatment status


######--- MARGINAL EFFECTS (package) ---######
# the marginal effects package does three main things:
# 1) Predictions
# 2) Comparisons
# 3) Slopes


library(marginaleffects)


###---- Predictions

###--- For each individual
predictions(ps_model)

###--- For the "mean" individual
# every predictor is set to its sample mean (for numeric variables) 
# and to the reference category (or first level) for factors.
predictions(ps_model, newdata = "mean")
plot_predictions(ps_model, by = "mmarried")


###---
ind_new <- 
  tbl_bal |> 
  slice_head(n = 1) |> 
  uncount(weights = 2) |> 
  mutate(id = row_number()) |> 
  mutate(nprenatal = ifelse(id == 2, nprenatal/2, nprenatal))

predictions(ps_model, newdata = ind_new)

###--- IN CLASS: Make the same predictions,
# but using only the coefficients in the model








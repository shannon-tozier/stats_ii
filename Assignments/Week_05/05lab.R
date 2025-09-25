
######--- MATCHING WORKFLOW ---######

  ###--- Review
    # 1) Research question 
    # 2) Define an estimand
    # 3) Draw the DAG
    # 4) Check balance before matching
    # 5) Estimate propensity scores
    # 6) Check model predictions
    # 7) PS Matching and Weighting
    # 8) Check balance after matching (go back if no balance)
    # 9) Compute estimates



######--- RESEARCH QUESTION ---######

# What is the effect of maternal smoking on infant health?

# Same data as last week: a subsample (N = 4642) of singleton births 
# in PA between 1989-1991. 


######--- DEFINE THE ESTIMAND ---######

# Theoretical estimand: For a mother who smokes, how much more or less would their baby weight if they did not smoke? 

# Y(1) - Y(0) for T = 1

# Empirical estimand: the average effect of smoking on their babies birth weight for mothers who smoke

# ATT = E[Yi(1) − Yi(0) ∣ T = 1]

# This requires:
# E[Y(1) | T = 1] 
# E[Y(0) | T = 1] 
# Which one is observable? 

# We do not observe all the potential outcomes 
# so we have to make assumptions:

# Because it was not an experiment, we know the independence assumption likely does not hold


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

# Where the S variable represents: marriage, alcohol, race, first baby, age, education, and number of prenatal visits

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
tbl <- readRDS("04week/lab/cattaneo.rds")
tbl <- d

# Because means do not perfectly describe a distribution,  there are multiple ways to check for balance: https://kosukeimai.github.io/MatchIt/articles/assessing-balance.html

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


tbl_bal |> 
  pivot_longer(cols = 1:7, names_to = "s_vars", values_to = "value") |> 
  group_by(smoker, s_vars) |> 
  summarise(mean = mean(value),
            sd = sd(value)) |> 
  arrange(s_vars) |> 
  pivot_wider(names_from = smoker, values_from = c(mean, sd)) |> 
  mutate(smd = (mean_Smoker - mean_Nonsmoker)/sd_Smoker)
  

######--- PROPENSITY SCORE MODEL ---######

ps_model <- glm(mbsmoke ~ mmarried + alcohol + mrace + fbaby + 
                mage + medu + nprenatal, 
             data = tbl,
             family = binomial)

summary(ps_model)


###--- IN CLASS: Using the function predict() 
# 1) Generate predictions for the observed data in the log odds and probability scales
# 2) Make a plot showing the relationship between the two scales

ps <- predict(ps_model, type = "response")
log_odds <- predict(ps_model, type = "link")

tibble("ps" = ps, "log_odds" = log_odds) |> 
  ggplot(aes(ps, log_odds)) +
  geom_point() +
  xlim(c(0,1))


###--- Incorporate propensities scores in the dataset
tbl <- 
  tbl |> 
  mutate(ps = predict(ps_model, type = "response")) 
  


###--- Plot support
tbl |> 
  ggplot(aes(ps, fill = smoker)) +
  geom_histogram(alpha = .5, color = "white")



######--- PS MATCHING PT. 1 ---######
# Nearest neighbor, ps matching, with replacement

tbl_matched <- 
  matchit(mbsmoke ~ mmarried + alcohol + mrace + fbaby + 
          mage + medu + nprenatal, 
        data = tbl,
        method = "nearest", 
        distance = "glm",
        estimand = "ATT", 
        replace = TRUE)

unique(tbl_matched$weights)
sum(tbl_matched$weights)


###--- Check balance after matching
love.plot(tbl_matched, 
          binary = "std",
          drop.distance = TRUE,
          var.order = "unadjusted",
          colors = c("steelblue"), 
          thresholds = .1, 
          abs = TRUE)



# TO DO IN CLASS: 

# We are going to write an algorithm that does 1:1 nearest neighbor propensity score matching with replacement

# Steps: 
# 1) Separate treated and control units (with their respective ps scores) into two datasets 
# 2) For each treated unit then:
  # a) Pick one treated unit at random
  # b) Calculate the absolute ps distance between that unit and controls
  # c) Pick the control unit with the minimum distance
  # d) Store the id of the treated and its matched control


tbl <- 
  tbl |> 
  mutate(id = row_number())

###--- Algorithm





###--- Bring back in covariates into matched data
matched_data <- 
  matched_data |> 
  pivot_longer(cols = everything(), 
               names_to = "treated", 
               values_to = "id") |> 
  left_join(tbl)



###---- This function uses the code we wrote last week to check balance
our_balance_check <- function(data) {
  
  data |> 
  select(all_of(c(s_vars, treatment))) |> 
    arrange(smoker) |> 
    pivot_longer(cols = 1:7, names_to = "s_vars", values_to = "value") |> 
    group_by(smoker, s_vars) |> 
    summarise(mean = mean(value),
              sd = sd(value)) |> 
    arrange(s_vars) |> 
    pivot_wider(names_from = smoker, values_from = c(mean, sd)) |> 
    mutate(smd = (mean_Smoker - mean_Nonsmoker)/sd_Smoker,
           abs_smd = abs(smd)) |> 
    arrange(desc(abs_smd)) |> 
    select(s_vars, mean_Nonsmoker, mean_Smoker, abs_smd)
  
}
  

###--- Check for balance
our_balance_check(matched_data)



######--- PS MATCHING PT. 2 ---######

# We saw that some of the covariates were still unbalanced. Let's add a caliper to see if we can improve balance

###--- With the MatchIt function
tbl_matched <- 
  matchit(mbsmoke ~ mmarried + alcohol + mrace + fbaby + 
            mage + medu + nprenatal, 
          data = tbl,
          method = "nearest", 
          distance = "glm",
          estimand = "ATT", 
          replace = TRUE,
          caliper = .00001, # New argument
          std.caliper = FALSE) # New argument


###--- Check balance after matching
love.plot(tbl_matched, 
          binary = "std",
          drop.distance = TRUE,
          var.order = "unadjusted",
          colors = c("steelblue"), 
          thresholds = .1, 
          abs = TRUE)


###--- QUESTION: 

# By using a caliper, what are we gaining and sacrificing in terms of the bias and variance of out estimator? 
# What about the choice between matching with and without replacement?
# And between exact and ps matching?



###--- TO DO IN CLASS:

# Building on our algorithm for nearest neighbor matching implement the following:
# 1) Make it into a function
# 2) Include an argument for using a caliper 
# 3) Include an argument for matching without replacement 



###--- Check balance after matching



###--- Estimate treatment effect and SEs


###--- Bootstrap my data

boot_match <- function(data) {
  
  ###--- Resample my dataset
  data_boot <- data[sample(1:nrow(data), replace = TRUE), ]
  
  ###--- Run the matching function
  match <- 
    matchit(mbsmoke ~ mmarried + alcohol + mrace + fbaby + 
              mage + medu + nprenatal, 
            data = data_boot,
            method = "nearest", 
            distance = "glm",
            estimand = "ATT", 
            replace = TRUE,
            caliper = .00001, # New argument
            std.caliper = FALSE) # New argument
  
  ###--- Extract dataset with weights
  data_matched <- match.data(match)
  
  ###--- Calculate ATT
  fit <- lm(data = data_matched,  bweight ~ mbsmoke,  weights = weights)
  coef(fit)["mbsmoke"]
  
}

###--- Run bootstrap
iters <- 500

boot_estimates <- 
  replicate(iters, {
  boot_match(tbl)
})

hist(boot_estimates)
mean(boot_estimates)
sd(boot_estimates)




######--- IPTW ---######

###---- TO DO IN CLASS: 
# 1) calculate weights
# 2) check balance
# 3) treatment effect


###--- IPSW with WeighIt package now

###--- Cobal plots

  

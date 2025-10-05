
###--- Multiple Regression

###--- Today
# (1) Coefficients
# (2) Standard errors
# (4) Polynomial terms

###--- Next Week
# (5) Variable transformations
# (6) Interactions



###--- Libraries
library(tidyverse)
library(hrbrthemes)
theme_set(theme_ipsum())
set.seed(061295)

###--- Load the GSS data
data <- gssr::gss_get_yr(year = 2022)

tbl <-
  data |> 
  transmute(
    gender = if_else(sex == 2, 1, 0),
    income = realinc,
    age,
    educ
  ) |> 
  drop_na() # Don't do this on your data!


###--- Take a small sample
tbl_small <- 
  tbl |> 
  slice_sample(n = 20)

###--- Coefficients
m1 <- lm(income ~ age + educ, data = tbl_small)
summary(m1)


###--- What does the coefficient of age on income mean? 

###--- Raw relationship
tbl_small |> 
  ggplot(aes(age, income)) +
  geom_point() +
  geom_smooth(method = "lm")


###--- 
tbl_small |> 
  ggplot(aes(educ, age)) +
  geom_point()


###--- Linear model
tbl_small |> 
  ggplot(aes(educ, age)) +
  geom_point() +
  geom_smooth(method = "lm", 
              se = FALSE)

###--- Calculate Residuals
tbl_small <- 
  tbl_small |>
  mutate(
    fit = predict(lm(age ~ educ, data = tbl_small)),
    resid = age - fit
  )


###--- Plot residuals
tbl_small |> 
  ggplot(aes(x = educ, y = age)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  # draw vertical residual lines
  geom_segment(aes(x = educ, xend = educ,
                   y = fit,  yend = age),
               color = "red", alpha = 0.6)


###--- Residualized relationship
tbl_small |>
  ggplot(aes(resid, income)) +
  geom_point()


###--- Relationship in multiple regression
tbl_small |>
  ggplot(aes(resid, income)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Residual of age")



###--- To Do in Class: compute the coefficients using from the FWL formula
# Use only 1 regression per coefficient

m2 <- lm(income ~ age + educ + gender, data = tbl)
summary(m2)


# Beta: Covariance of ResidualX and Y divided by the variance of ResidualX

########## MY WAY ##########

# age
ageres <- lm(age ~ educ + gender, data = tbl)

tbl$ageres <- residuals(ageres)

agebeta <- cov(tbl$ageres, tbl$income)/var(tbl$ageres)

#educ
educres <- lm(educ ~ age + gender, data = tbl)

tbl$educres <- residuals(educres)

educbeta <- cov(tbl$educres, tbl$income)/var(tbl$educres)

#gender
genres <- lm(gender ~ educ + age, data = tbl)

tbl$genres <- residuals(genres)

genbeta <- cov(tbl$genres, tbl$income)/var(tbl$genres)

###############

##### PABLO'S WAY #####

coef <- coef(m2)[-1]
name_coef <- names(coef)
coef_manual <- c()

for(i in 1:length(coef)) {
  
  y <- name_coef[i]
  x <- name_coef[name_coef != y]
  
  x <- paste(x, collapse = "+")
  form <- paste(y, "~", x)
  form <- as.formula(form)
  
  xr <- lm(form, data = tbl)$residual
  
  coef_manual[i] <- cov(xr, tbl$income)/var(xr)
  
}

names(coef_manual) <- name_coef

#######################


###--- To Do in Class: Compute the standard errors

# SE for Beta: sqrt of (sigma/sum of squares residuals)
# sigma: sum of ALL squared residuals/(n - nparameters)


coef <- coef(m2)[-1]
name_coef <- names(coef)
ressum <- sum((m2$residuals)^2)
sigma <- ressum/(nobs(m2) - (length(coef(m2)) -1))
se_manual <- c()

for(i in 1:length(coef)) {
  
  y <- name_coef[i]
  x <- name_coef[name_coef != y]
  
  x <- paste(x, collapse = "+")
  form <- paste(y, "~", x)
  form <- as.formula(form)
  
  xr <- lm(form, data = tbl)$residual
  
  se_manual[i] <- sqrt(sigma/sum(xr^2))
  
}

names(se_manual) <- name_coef




###--- Multiple collinearity example
# Perfect predictor (can't be estimated)
# Quasi perfect predictor (inflated SE)

tbl <-
  tbl |> 
  mutate(age2 = age + rnorm(nrow(tbl), mean = 0, sd = 0.01))


m3 <- lm(income ~ age + age2 + educ + gender, data = tbl)
summary(m3)

# coefficient predictions are crazy bananas (and so are standard errors)



###--- Polynomials
# Interpreting polynomial terms
m4 <- lm(income ~ poly(age, 2, raw = TRUE) + educ + gender, data = tbl)
summary(m4)

# now the effect of a one unit increase of x depends on the value of x
# derivatives :(
# derivative only gives you a one-unit increase

# How many polynomials? Plotting x against residuals

###--- Variable transformations
###--- Interactions




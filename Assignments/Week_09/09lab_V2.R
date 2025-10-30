###--- Libraries
library(gapminder)
library(tidyverse)
library(lme4)
library(performance)
library(ggridges)

###--- Load the data
data("gapminder")

tbl <- 
  gapminder |> 
  janitor::clean_names() |> 
  mutate(gdp = gdp_percap/1e3,
         year2 = year - min(year)) |> 
  filter(continent %in% c("Asia", "Europe", "Africa"))


###--- We are studying life expectancy. Is there a hierarchical structure in the data? 
tbl |> 
  ggplot(aes(life_exp, fill = continent)) +
  geom_histogram(position = "identity", 
                 color = "white",
                 alpha = .5) +
  labs(x = "Life Expectancy")


###--- What is the ICC for continents? 
# Why do continent means do not shrink? 
m0 <- lmer(life_exp ~ 1 + (1 | continent), data = tbl, REML = FALSE) #partial pooling
summary(m0)
icc(m0)

m00 <- lm(life_exp ~ 1, data = tbl) # complete pooling
m000 <- lm(life_exp ~ continent, data = tbl) # no pooling

###--- Observed means
new_tbl <- 
  tbl |> 
  group_by(continent) |> 
  summarise(mean = mean(life_exp))

###--- Predicted means
new_tbl |> 
  mutate(pred_mean = predict(m0, newdata = new_tbl))

#### means not shrinking because of large sample size and similar inter vs intra class correlation


###--- Now do the same, but for countries. 

# (1) Plot life expectancy by country (think about what is the best way to plot this)

tbl |> 
  ggplot(aes(life_exp, fill = factor(country))) +
  geom_density(position = "identity",
               color = "white",
                 alpha = .5) +
  labs(x = "Life Expectancy") +
  guides(fill = "none")

# (2) Estimate the partially pooled means

m0_country <- lmer(life_exp ~ 1 + (1 | country), data = tbl, REML = FALSE)
check_model(m0_country)

# (3) Calculate the ratio of between country variance to total variance (it should approximate the ICC)

###--- Observed means
new_tbl_2 <- 
  tbl |> 
  group_by(country) |> 
  summarise(mean = mean(life_exp))

var(new_tbl_2$mean)/var(tbl$life_exp)

icc(m0_country)

# (4) Compare them with the no pooling means (plot it)

###--- Predicted means
new_tbl_2 <- new_tbl_2 |> 
  mutate(pred_mean = predict(m0_country, newdata = new_tbl_2))

ggplot(new_tbl_2,
       aes(mean, pred_mean)) +
  geom_point(alpha = 0.5) +
  geom_abline()

# Why is there so little shrinking?

### because most variation is within countries, not between them? 





###--- Model building (step by step)

###--- Model 1: Random intercepts with fixed slope for time.
# Each country has its own (random) baseline, but same time trend.

m1 <- lmer(life_exp ~ year2 + (1 | country), data = tbl, REML = FALSE) # year is completely pooled
summary(m1)

# 49.3 is mean and 11.4 is sd for the distribution of the intercepts. 

tbl |> 
  mutate(pred = predict(m1)) |> 
  ggplot(aes(year, life_exp, group = country)) +
  geom_point(alpha = .4) +
  geom_line() +
  facet_wrap(~ continent)


###--- Before looking at the plot, what kind of predicted lines do you expect based on the model? 

plot_pred(data = tbl, model = m1)


###--- Model 2: Random intercepts AND random slopes for time
# Each country has its own baseline AND its own time trend
m2 <- lmer(life_exp ~ year2 + (year2 | country), 
           data = tbl, 
           REML = FALSE,
           control = lmerControl(optimizer = "bobyqa",
                                 optCtrl = list(maxfun = 100000)))
summary(m2)

# now slopes have a distribution (mean 0.3 and sd 0.16). intercepts mean 49.32 and sd 12.5


###--- Check model assumptions
check_model(m2)

###--- Plot predictions
plot_pred(tbl, m2)

###--- So how is this different than a regular linear model? 

###--- (1) Fit an OLS model with the same structure as model 2 (country level intercepts and slopes)

m3 <- lm(life_exp ~ country*year2, data = tbl)
summary(m3)

###--- (2) Compare the two models using BIC

BIC(m2, m3)
# m2 BIC is 7305.7, m3 BIC is 7698.7

###--- (3) Compare the precision of the estimates for the time trend (year) between the multilevel and the linear model. What would you do to increase precision in the linear model?

# t score of 20.48 (m2) vs 6.99 (m3). can be more precise if you only allow one slope for year.

m3_fe <- fixest::feols(life_exp ~ year2 | country,
                       data = tbl)
summary(m3_fe)

BIC(m3_fe) # BIC is 8286.6




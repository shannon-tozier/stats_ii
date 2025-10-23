

###--- Interactions

# 1) What is the effect of a variable X when there’s an interaction between X and something else in the model?

# 2) How can I interpret the interaction term?





###--- Libraries
library(gapminder)
library(hrbrthemes)
library(tidyverse)
library(marginaleffects)
library(interactions)
theme_set(theme_ipsum(base_size = 12, axis_title_size = 12))


###--- Load the data
data("gapminder")
tbl <- 
  gapminder |> 
  janitor::clean_names() |> 
  mutate(gdp = gdp_percap/1e3,
         year2 = year - min(year)) |> 
  filter(continent %in% c("Americas", "Europe", "Africa"))



###--- The effect of gdp on life expectancy varies by continent
m1 <- lm(life_exp ~ gdp*continent, data = tbl)
summary(m1)


###--- Questions:
# a) What is the effect of gdp on life expectancy according to this model?

# b) Using pen and paper, approximately draw a plot of predicted values with life expectancy on the y-axis and gdp on the x-axis.

# c) Now do the same plot in R using the results from the regression.

interact_plot(m1, pred = gdp, modx = continent)
plot_predictions(m1, by = c("gdp", "continent"))


###--- The effect of gdp on life expectancy varies by year
m2 <- lm(life_exp ~ gdp*year2, data = tbl)
modelsummary(m2)

# a) What is the effect of gdp on life expectancy now?

# b) Is the effect of gdp ever approximately 0? When?

# c) Like before, using pen and paper, draw the interaction 

# d) Plot the interaction effect.
plot_predictions(m2, by = c("gdp", "year2"))


###--- The effect of gdp on life expectancy varies by year and continent
tbl <- 
  tbl |> 
  mutate(year_80 = ifelse(year > 1980, 1, 0))

m3 <- lm(life_exp ~ gdp*year_80*continent, data = tbl)
modelsummary(m3)


# a) Like before, using pen and paper, draw the interaction. 
# (you can do two plots, one for years before 1980 and other for years after 1980)

# d) Plot the interaction effect.
plot_predictions(m3, by = c("gdp", "continent", "year_80"))

interact_plot(m3, pred = gdp, modx = continent, mod2 = year_80)




###--- Lab Week 3 ---###


###--- Matching

# What is the effect of maternal smoking on infant health?
# Data are from a subsample (N = 4642) of singleton births 
# in PA between 1989-1991. 
# See Almond et al. 2005. “The Costs of Low Birth Weight.”

###--- Libraries
library(tidyverse)
library(MatchIt)
library(hrbrthemes)
library(broom)

###--- 
theme_set(theme_ipsum())

###--- Read the data
d <- haven::read_dta("Assignments/Week_03/cattaneo2.dta")


d <-
  d |>  
  haven::zap_labels() |>             # remove Stata labels
  mutate( smoker = factor(mbsmoke, 
                          labels = c("Nonsmoker", "Smoker")) ,
          zweight = (bweight - mean(bweight)) / sd(bweight)) |> 
  select( smoker, mbsmoke, bweight, zweight, mmarried, alcohol, mrace)

###---  Describe the data
d |>  
  select_if(is.numeric) %>% 
  psych::describe(fast = TRUE)


###--- Raw differences between smokers and non-smokers
d |> 
  ggplot(aes(bweight, color = smoker, fill = smoker)) +
  geom_density(alpha = .5)


###--- Info on exact matching
vignette("matching-methods")
?method_exact

###--- Balance before matching
d |> 
  select(smoker, mmarried, alcohol, mrace) |> 
  pivot_longer(cols = c(mmarried:mrace), names_to = "preds", values_to = "value") |> 
  group_by(smoker, preds) |> 
  summarise(perc = mean(value)*100) |> 
  arrange(preds)


###--- Matching
ematch_out <- 
  matchit(mbsmoke ~ mmarried + mrace + alcohol, 
          data = d,
          method = "exact",
          estimand = "ATT")


###--- DO IN CLASS: draw the DAG




###--- Summary of matching
summary(ematch_out)
plot(summary(ematch_out)) # love plot


###--- Extract data with extra columns 
ematch_data <- match.data(ematch_out)


###--- Weights by subcategory
ematch_data |> 
  count(mmarried, alcohol, mrace, smoker, weights)


###----  TO DO in class: Calculate weights manually
# Formula: https://kosukeimai.github.io/MatchIt/reference/matchit.html#how-matching-weights-are-computed




###--- Subclassification table
(subclass_table <- 
  ematch_data |> 
  group_by(mmarried, alcohol, mrace) |>  
  summarize(
    n_t = sum(mbsmoke),                            # Ntreat
    n_c = sum(1-mbsmoke),                          # Ncon
    zbw_t = weighted.mean(zweight, w = mbsmoke),   # mean std bw for treated
    zbw_c = weighted.mean(zweight, w = 1-mbsmoke), # mean std bw for control
    row_diff = zbw_t - zbw_c,                      # mean treat-control diff
    wt_t = weighted.mean( weights, w = mbsmoke),   # mean treat weight
    wt_c = weighted.mean( weights, w = 1-mbsmoke))) # mean control weight


###--- Manually calculate ATT
subclass_table |> 
  ungroup() |> 
  mutate(att = n_t*row_diff/sum(n_t)) |> 
  summarise(att = sum(att))


###--- DO IN CLASS: manually calculate ATE




###--- ATT by weighted least squares (WLS)
m_att <- lm(zweight ~ mbsmoke, data = ematch_data,
            weights = weights)

tidy(m_att)




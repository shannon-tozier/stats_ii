library(tidyverse)
library(gssr)
library(gssrdoc)
library(broom)

# gss2022 <- gss_get_yr(2022)
# saveRDS(gss2022, "data/gss2022.rds")

gss2022 <- readRDS("data/gss2022.rds")

# alt + - for <-; Ctrl + Shift + m for |>

d2 <- gss2022 |>
  select(educ, paeduc, maeduc) |> 
  mutate(maxpared = pmax(paeduc, maeduc, na.rm = T)) |>
  drop_na(educ, maxpared)

ggplot(d,
       aes(x = educ)) +
  geom_bar() +
  theme_minimal()
  
ggplot(d,
       aes(x = maxpared)) +
  geom_bar() +
  theme_minimal()

d2 <- d2 |> 
  mutate(college = if_else(educ >= 16, 1, 0),
         parcol = if_else(maxpared >= 16, 1, 0))


table(d2$college, d2$parcol)

m2 <- glm(college ~ parcol,
          data = d2,
          family = binomial())
summary(m2)

tidy(m1)

# Simulates cat_chaos.csv for Lab 11 (Logistic Regression)
library(tidyverse)

set.seed(1121)
n <- 250

age_years <- round(pmin(0.5 + rgamma(n, 2, 0.35), 18), 1)
indoor    <- rbinom(n, 1, 0.6)
zoomies   <- pmin(rpois(n, pmax(3.2 - 0.15 * age_years, 0.3)), 8)   # zoomies per day

# 1 = knocked at least one thing off a counter or table this week
p_knock <- plogis(-1.0 - 0.12 * (age_years - 5) + 0.8 * indoor + 0.45 * (zoomies - 2))
knocked_off <- rbinom(n, 1, p_knock)

cat_chaos <- tibble(
  cat_id = sprintf("K%03d", 1:n),
  age_years = age_years,
  indoor = indoor,
  zoomies = zoomies,
  knocked_off = knocked_off
)

rio::export(cat_chaos, here::here("files", "data", "cat_chaos.csv"))

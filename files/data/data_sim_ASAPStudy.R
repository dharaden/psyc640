
library(MASS)
library(tidyverse)

set.seed(2025) 
n <- 400

# --- DATA GENERATION ENGINE 3.0 (Revised Metrics) ---

# 1. Demographics & Stable Traits
Sex <- rbinom(n, 1, 0.5) # 0=Male, 1=Female
SES <- rnorm(n, mean=0, sd=1)

# 2. Time 1 Generation (Baseline: Ages 7-9)
# Generate Age first (uniform dist between 7 and 9.9)
T1_Age <- runif(n, min=7.0, max=9.9)

# Puberty at T1 (Scale 0-8).
# At age 7-9, most are 0 or 1. Girls (Sex=1) slightly higher. Older kids higher.
T1_Puberty <- 0.5 * (T1_Age - 7) + 0.5 * Sex + rnorm(n, mean=3, sd=0.5)
T1_Puberty <- pmax(3, pmin(9, T1_Puberty)) # Bound between 3 and 9

# Generate other T1 variables correlated with Age/Puberty/SES
# Note: Sleep duration is higher for younger kids (approx 9-10 hours)
mu_T1 <- c(SleepDur=9.5, SleepQual=5, PeerStress=8, AcadStress=5, Dep=10, Anx=6)
sigma_T1 <- matrix(c(
  1.0, -0.6, -0.3,  0.0, -0.4, -0.2, # Sleep Dur
  -0.6,  2.0,  0.4,  0.1,  0.6,  0.4, # Sleep Qual
  -0.3,  0.4,  3.0,  0.5,  0.5,  0.3, # Peer Stress
  0.0,  0.1,  0.5,  3.0,  0.1,  0.2, # Acad Stress
  -0.4,  0.6,  0.5,  0.1,  5.0,  0.7, # Dep
  -0.2,  0.4,  0.3,  0.2,  0.7,  4.0  # Anx
), ncol=6)

T1_raw <- mvrnorm(n, mu_T1, sigma_T1)
colnames(T1_raw) <- paste0("T1_", c("SleepDur", "SleepQual", "PeerStress", "AcadStress", "Dep", "Anx"))

# Combine T1
df <- data.frame(ID=1:n, Sex, SES, T1_Age, T1_Puberty, T1_raw)

# 3. Time 2 & 3 (Growth & Effects)
# Function to advance puberty (Logistic-ish growth: faster in middle, slows near 8)
advance_puberty <- function(curr_pub, sex, age) {
  # Growth potential is higher if you are young and female
  growth <- runif(length(curr_pub), 0.5, 1.5) + (0.2 * sex)
  new_pub <- curr_pub + growth
  return(pmax(0, pmin(8, new_pub))) # Keep within 0-8 bounds
}

df <- df %>% mutate(
  # --- TIME 2 (Age 8-10) ---
  T2_Age = T1_Age + 1.0,
  
  T2_Puberty = advance_puberty(T1_Puberty, Sex, T1_Age),
  
  # Sleep drops as they get older (Age effect) + Stability
  T2_SleepDur = 9.5 - 0.3*(T2_Age - 8) + 0.6*(T1_SleepDur-9.5) + rnorm(n, 0, 0.8),
  
  T2_SleepQual = 5.0 + 0.5*(T1_SleepQual-5) + rnorm(n, 0, 1.2),
  
  # Mechanism: T2 Peer Stress driven by T1 Sleep + T2 Puberty (Social awakening)
  T2_PeerStress = 8 + 0.5*(T1_PeerStress-8) - 0.4*(T1_SleepDur-9.5) + 0.5*T2_Puberty + rnorm(n, 0, 1.5),
  
  T2_AcadStress = 5 + 0.6*(T1_AcadStress-5) + 0.2*(T2_Age-8) + rnorm(n, 0, 1.5),
  T2_Dep = 10 + 0.6*(T1_Dep-10) + 0.3*(T2_PeerStress-8) + rnorm(n, 0, 3),
  T2_Anx = 6  + 0.6*(T1_Anx-6) + rnorm(n, 0, 2.5),
  
  # --- TIME 3 (Age 9-11) ---
  T3_Age = T2_Age + 1.0,
  
  T3_Puberty = advance_puberty(T2_Puberty, Sex, T2_Age),
  
  T3_SleepDur = 9.0 - 0.3*(T3_Age - 9) + 0.6*(T2_SleepDur-9.5) + rnorm(n, 0, 0.8),
  T3_SleepQual = 5.0 + 0.5*(T2_SleepQual-5) + rnorm(n, 0, 1.2),
  
  T3_PeerStress = 8 + 0.5*(T2_PeerStress-8) + rnorm(n, 0, 1.5),
  T3_AcadStress = 5 + 0.6*(T2_AcadStress-5) + rnorm(n, 0, 1.5),
  
  # INTERACTION OUTCOME: T3 Dep driven by Stress * Puberty
  # Note: Centering the interaction components roughly around means
  T3_Dep = 10 + 0.5*(T2_Dep-10) + 
    0.4*(T2_PeerStress-8) + 
    0.30*((T2_PeerStress-8) * (T2_Puberty-3)) + # Interaction: Puberty amplifies stress
    rnorm(n, 0, 3),
  
  T3_Anx = 6  + 0.6*(T2_Anx-6) + rnorm(n, 0, 2.5)
)

# 4. Attrition (The Mess)
# Younger kids (T1) are compliant (parents bring them). Dropouts increase as they age.
df$Dropout_T2 <- rbinom(n, 1, 0.08) 
prob_T3_drop <- plogis(-2 - 0.5*df$SES + 0.1*df$T2_Puberty) # High puberty kids rebel/drop out?
df$Dropout_T3 <- rbinom(n, 1, prob_T3_drop)

asap_data_final <- df %>%
  mutate(
    across(starts_with("T2"), ~ ifelse(Dropout_T2 == 1, NA, .)),
    across(starts_with("T3"), ~ ifelse(Dropout_T2 == 1 | Dropout_T3 == 1, NA, .)), 
    across(is.numeric, round, digits = 2)
  ) %>%
  dplyr::select(ID, Sex, SES, starts_with("T1"), starts_with("T2"), starts_with("T3"))

# --- END GENERATION ---

rio::export(asap_data_final, here::here("files", "data", "asap_data.csv"))


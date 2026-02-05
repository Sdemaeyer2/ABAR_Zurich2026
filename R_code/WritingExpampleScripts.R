library(brms)
library(tidyverse)
library(here)

load(
  file = here(
    "Presentations", 
    "WritingData.RData")
)

M3 <- brm(
  SecondVersion ~ FirstVersion_GM + Experimental_condition + (1 + FirstVersion_GM |Class),
  data = WritingData,
  backend = "cmdstanr",
  cores = 4,
  control = list(adapt_delta = 0.9),
  seed = 1975 
)

WritingData <- WritingData %>%
  mutate(
    Control_Condition = 
      case_when(
        Condition == 2 ~ 1,
        Condition == 1 ~ 0
    )
  )



M_alternative <- brm(
  SecondVersion ~ -1 + Control_Condition + Experimental_condition + (1 |Class),
  data = WritingData,
  backend = "cmdstanr",
  cores = 4,
  control = list(adapt_delta = 0.9),
  seed = 1975 
)

summary(M_alternative)

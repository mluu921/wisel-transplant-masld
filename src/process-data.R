library(tidyverse)
library(pins)
library(haven)

board <- pins::board_folder(here::here('board'))

data <- readxl::read_excel(
  here::here(
    'data/CLN evaluated patients only 5.2025 (1).xlsx'
  ),
  guess_max = 10000
)

data <- data |>
  janitor::clean_names()

data <- data |>
  mutate(
    etiology_mash = factor(
      etiology_mash,
      levels = 0:1,
      labels = c('Non-MASLD', 'MASLD')
    )
  )

data <- data |>
  mutate(
    eval_outcome = ifelse(eval_outcome == 'NA', NA, eval_outcome),
    listing_outcome = ifelse(listing_outcome == 'NA', NA, listing_outcome)
  )

data <- data |>
  mutate(
    marital_status = case_when(
      marital_status == 'NA' | marital_status == 'Unknown' ~ NA,
      .default = marital_status
    )
  )

data <- data |>
  mutate(
    race_v2 = case_when(
      race_v2 == 'NA' | race_v2 == 'Unknown' ~ NA,
      .default = race_v2
    )
  )

data <- data |>
  mutate(
    sex = case_when(
      sex == 'NA' ~ NA,
      .default = sex
    )
  )

data$waitlist_removal_recode <- fct_collapse(
  factor(data$waitlist_removal_reaso),
  "Psychosocial" = c("alcohol/substance use related", "psychiatric illness"),
  "Medical Contraindication" = c(
    "condition deteriorated, too sick for tx",
    "frailty or deconditioning",
    "medical comorbidity: CAD, CHF, other cancer, etc",
    "surgical contraindication"
  ),
  "Condition Improved" = "condition improved, tx not needed",
  "Financial/Insurance" = "insurance/financial issue",
  "Social/Logistical" = "lack of social support or transportation",
  "Patient Choice" = "patient choice",
  other_level = "Other"
)

data$waitlist_removal_recode <- factor(
  data$waitlist_removal_recode,
  levels = c(
    "Psychosocial",
    "Medical Contraindication",
    "Condition Improved",
    "Financial/Insurance",
    "Social/Logistical",
    "Patient Choice"
  )
)

# collapse "Financial/Insurance", "Social/Logistical", into "Other"

data$waitlist_removal_recode <- fct_lump_min(
  data$waitlist_removal_recode,
  min = 30
)

# code time to event vars ------------------------------------------------

data <- data |>
  mutate(
    time_to_transplant_removal = case_when(
      listing_outcome == 'Transplanted' ~ time_to_transplant,
      listing_outcome == 'Waitlist removal' ~ time_to_waitlist_removal,
      listing_outcome == 'Still waiting' ~ time_to_last_follow_up
    ),
    competing_transplant_removal = case_when(
      listing_outcome == 'Transplanted' ~ 1,
      listing_outcome == 'Waitlist removal' ~ 2,
      listing_outcome == 'Still waiting' ~ 0
    ),
    time_to_transplant_removal = time_to_transplant_removal / 365.25,
    competing_transplant_removal = factor(
      competing_transplant_removal,
      levels = 0:2
    )
  )


# code the died on the waitlist patients ---------------------------------

data <- data |>
  mutate(
    listing_outcome = case_when(
      listing_outcome == 'Waitlist removal' &
        mortality_outcome == 1 &
        (time_to_mortality < time_to_waitlist_removal) ~
        'Died on waitlist',
      .default = listing_outcome
    )
  ) |>
  filter(listing_outcome == 'Died on waitlist')


# exlcude the outlier follow up - most likely clerical error -------------

data <- data |>
  filter(time_to_last_follow_up < 5000 | is.na(time_to_last_follow_up))

# save the data ----------------------------------------------------------

pins::pin_write(board, data, 'processed-data', type = 'rds', versioned = TRUE)

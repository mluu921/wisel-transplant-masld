library(tidyverse)
library(pins)
library(haven)
library(tidylog)

board <- pins::board_folder(here::here('board'))

data <- readxl::read_excel(
  here::here(
    'data/CLN evaluated patients only 5.2025 (1).xlsx'
  ),
  guess_max = 10000
)

data <- data |>
  janitor::clean_names()

# exlcude the outlier follow up - most likely clerical error -------------

data <- data |>
  filter(time_to_last_follow_up < 5000 | is.na(time_to_last_follow_up))

data <- data |>
  filter(time_to_transplant <= 5000 | is.na(time_to_transplant))

# code the vars ----------------------------------------------------------

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
    listing_outcome = ifelse(
      listing_outcome %in% c('NA', 'Still waiting'),
      NA,
      listing_outcome
    )
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
    "frailty or deconditioning",
    "medical comorbidity: CAD, CHF, other cancer, etc",
    "surgical contraindication"
  ),
  "Condition Deteriorated" = "condition deteriorated, too sick for tx",
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
    "Condition Deteriorated",
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

data <- data |>
  mutate(
    bmi_class = factor(
      bmi_class,
      levels = c(
        "1 - Underweight",
        "2 - Normal",
        "3 - overweight",
        "4 - obese",
        "5 - morbid obese"
      ),
      labels = c(
        "Underweight",
        "Normal",
        "Overweight",
        "Obese",
        "Morbidly Obese"
      )
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
    ),
    listing_outcome = factor(listing_outcome)
  )

data <- data |>
  mutate(
    eval_outcome = factor(eval_outcome)
  )

# code time to event vars ------------------------------------------------

data <- data |>
  mutate(
    time_to_listing = case_when(
      eval_outcome == 'Waitlisted' & is.na(time_to_listing) ~ time_to_selection,
      .default = time_to_listing
    )
  )

data <- data |>
  mutate(
    time_from_waitlist_to_event = case_when(
      eval_outcome == 'Waitlisted' & listing_outcome == 'Died on waitlist' ~
        time_to_mortality - time_to_listing,
      eval_outcome == 'Waitlisted' & listing_outcome == 'Waitlist removal' ~
        time_to_waitlist_removal - time_to_listing,
      eval_outcome == 'Waitlisted' & listing_outcome == 'Transplanted' ~
        time_to_transplant - time_to_listing,
      eval_outcome == 'Waitlisted' & is.na(listing_outcome) ~
        time_to_last_follow_up - time_to_listing,
    )
  )

data <- data |>
  mutate(
    competing_risk_waitlist = case_when(
      eval_outcome == 'Waitlisted' & is.na(listing_outcome) ~ 0,
      eval_outcome == 'Waitlisted' & listing_outcome == 'Died on waitlist' ~ 1,
      eval_outcome == 'Waitlisted' & listing_outcome == 'Waitlist removal' ~ 2,
      eval_outcome == 'Waitlisted' & listing_outcome == 'Transplanted' ~ 3
    ),
    competing_risk_waitlist = factor(competing_risk_waitlist)
  )

data <- data |>
  mutate(
    time_from_waitlist_removal_to_death = case_when(
      mortality_outcome == 1 ~ time_to_mortality - time_to_waitlist_removal,
      mortality_outcome == 0 ~ time_to_last_follow_up - time_to_waitlist_removal
    )
  ) |>
  mutate(
    time_from_transplant_to_death = case_when(
      mortality_outcome == 1 ~ time_to_mortality - time_to_transplant,
      mortality_outcome == 0 ~ time_to_last_follow_up - time_to_transplant
    )
  )

data <- data |>
  rowwise() |>
  mutate(
    across(contains('med_'), \(x) as.numeric(x)),
    total_comorbs = sum(c_across(contains('med_')), na.rm = TRUE),
    total_comorbs = factor(total_comorbs, levels = 0:4)
  ) |>
  ungroup()


# data |>
#   filter(time_from_waitlist_to_event > 20000) |>
#   select(
#     eval_outcome,
#     listing_outcome,
#     time_to_mortality,
#     time_to_listing,
#     time_to_last_follow_up,
#     time_to_transplant,
#     time_from_waitlist_to_event,
#     time_from_transplant_to_death
#   )

# summary(data$time_from_waitlist_to_event)

# check this to make sure it's correct
# data <- data |>
#   mutate(
#     time_to_selection = case_when(
#       eval_outcome == 'Waitlisted' & is.na(time_to_selection) ~ time_to_listing,
#       .default = time_to_selection
#     ),
#     time_eval_to_listing = case_when(
#       eval_outcome == 'Waitlisted' ~ time_to_selection - time_to_evaluation,
#       eval_outcome == 'Waitlisted' &
#         mortality_outcome == 1 &
#         (time_to_mortality < time_to_waitlist_removal) ~
#         time_to_mortality - time_to_evaluation,
#       eval_outcome %in% c('Declined', 'Deferred') ~
#         time_to_selection - time_to_evaluation,
#       .default = time_to_last_follow_up - time_to_evaluation
#     )
#   )

# save the data ----------------------------------------------------------

pins::pin_write(board, data, 'processed-data', type = 'rds', versioned = TRUE)

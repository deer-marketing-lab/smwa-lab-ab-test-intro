library(readr)
library(dplyr)
library(broom)
library(ggplot2)
library(tidyr)

## ab test - udacity
#'
#'
control <- 
    read_csv("data/Results _Control.csv") %>%
    mutate(treatment = "control")

treatment <- 
    read_csv("data/Results_ Experiment.csv") %>%
    mutate(treatment = "treatment")

udacity <- 
    rbind(control, treatment) %>%
    janitor::clean_names()

write_csv(udacity, 
          "data/udacity.csv")

udacity %>%
    group_by(treatment) %>%
    summarise(views =sum(pageviews))

# no na
udacity <- 
    udacity %>%
    na.omit()

## power analysis
n_cookies_per_day <- 40000
n_cookies_start_trial <- 3200
n_enroll <- 660
n_ctr_free <- 0.08
n_prob_enrol_click <- 0.20625
n_prob_payment_enroll <- 0.53
n_prob_payment_click <- 0.109313

## Sample size for gross conversion
# start with clicks
n_clicks_test_size <-
    2 *
    round(
    power.prop.test(
    p1 = n_prob_enrol_click,
    p2 = n_prob_enrol_click + 0.01,
    power = 0.8,
    sig.level = 0.05
    )$n
)
# from clicks to pageviews
pageviews_for_sample_gross_conversion <- n_clicks_test_size / n_ctr_free

## Sample size for retention
n_retention_test_size <-
    2 *
    round(
        power.prop.test(
            p1 = n_prob_payment_enroll,
            p2 = n_prob_payment_enroll + 0.01,
            power = 0.8,
            sig.level = 0.05
        )$n
    )
# enroll per pageview
pageviews_for_sample_retention <- round(n_retention_test_size / (n_enroll / n_cookies_per_day))

## Sample size for conversion
n_conversion_test_size <-
    2 *
    round(
        power.prop.test(
            p1 = n_prob_payment_click,
            p2 = n_prob_payment_click + 0.0075,
            power = 0.8,
            sig.level = 0.05
        )$n
    )
## clicks to pageviews
n_conversion_test_size / n_ctr_free

## what ratio to divert
# if we rely on conversion and divert 100% traffic:
ceiling(pageviews_for_sample_retention / n_cookies_per_day)
# too many days

# so rely on gross conversion
# 100 percent traffic is that realistic no
ceiling(pageviews_for_sample_gross_conversion / n_cookies_per_day)
# if we do 50% -- 33 days -- ok thats approx a month, seems reasonable
ceiling(pageviews_for_sample_gross_conversion / (0.7*n_cookies_per_day))

# remark: what does the data suggest -- 70% conversion approx!

# are we getting 50% split per day?
udacity_wide <-
    udacity %>%
    pivot_wider(names_from = treatment,
                values_from = c(pageviews, clicks, enrollments, payments)) %>%
    mutate(pct_view_treat = pageviews_treatment / (pageviews_treatment + pageviews_control))

## plot per day?
udacity %>%
    ggplot() +
    geom_line(aes(x=date, y=pageviews, group = treatment))

# randomization success?

tidy(lm(pageviews ~ treatment, data = udacity))
tidy(lm(clicks ~ treatment, data = udacity))

# eval
tidy(lm(log(enrollments) ~ treatment, data = udacity))

## eeeeek: ratio metrics
# gross conversion
tidy(lm(log(enrollments/clicks) ~ treatment, data = udacity))

# or
udacity %>%
    group_by(treatment) %>%
    summarise(clicks = sum(clicks),
              enrollments = sum(enrollments)
              ) %>%
    mutate(gross_clickthrough = enrollments / clicks,
           change_clicks = clicks - lag(clicks),
           change_enroll = enrollments - lag(enrollments),
           return = change_enroll / change_clicks
           ) %>%
    select(return) %>%
    na.omit()

# net conversion
tidy(lm(log(payments/clicks) ~ treatment, data = udacity))


# 
udacity %>%
    group_by(treatment) %>%
    summarise(clicks = sum(clicks),
              enrollments = sum(enrollments)
    ) %>%
    mutate(num = enrollments - lag(enrollments),
           den = clicks - lag(clicks)
           ) %>%
    mutate(num / den)

# bootstrap for ratio metrics
boot_df <-
    rsample::bootstraps(udacity, 
            times = 1e3)


estimate_ratio <- function(split){
    out <-
        rsample::analysis(split) %>%
        group_by(treatment) %>%
        summarise(clicks = sum(clicks),
                  enrollments = sum(enrollments),
                  payments = sum(payments)
        ) %>%
        mutate(gross_clickthrough = payments / clicks,
               change_clicks = clicks - lag(clicks),
               change_enroll = payments - lag(payments),
               return = change_enroll / change_clicks
        ) %>%
        select(return) %>%
        na.omit()
    return(out)
}

boot_models <-
    boot_df %>% 
    mutate(roi = purrr::map(splits, estimate_ratio)) %>%
    tidyr::unnest(roi)

boot_models %>%
    ggplot() +
    stat_density(aes(x = return)) + 
    theme_bw()

quantile(boot_models$return, 0.025)

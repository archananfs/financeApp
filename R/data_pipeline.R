# R/data_pipeline.R
# Shared data processing functions for app and Quarto report

library(dplyr)
library(lubridate)
library(readr)

CATEGORIES <- c(
        "Groceries", "Dining", "Transport", "Utilities",
        "Health", "Leisure", "Clothing", "Education",
        "Travel", "Rent", "Insurance", "Other"
)

CATEGORY_COLORS <- c(
        "Groceries"   = "#1D9E75",
        "Dining"      = "#EF9F27",
        "Transport"   = "#378ADD",
        "Utilities"   = "#7F77DD",
        "Health"      = "#D4537E",
        "Leisure"     = "#D85A30",
        "Clothing"    = "#97C459",
        "Education"   = "#5DCAA5",
        "Travel"      = "#AFA9EC",
        "Rent"        = "#F09995",
        "Insurance"   = "#85B7EB",
        "Other"       = "#888780"
)

DEFAULT_BUDGETS <- c(
        "Groceries"   = 300,
        "Dining"      = 200,
        "Transport"   = 150,
        "Utilities"   = 150,
        "Health"      = 100,
        "Leisure"     = 100,
        "Clothing"    = 80,
        "Education"   = 50,
        "Travel"      = 200,
        "Rent"        = 900,
        "Insurance"   = 150,
        "Other"       = 100
)

load_csv <- function(path) {
        df <- read_csv(path, show_col_types = FALSE)
        df <- df %>%
                mutate(
                        date     = as.Date(date),
                        amount   = as.numeric(amount),
                        month    = floor_date(date, "month"),
                        week     = floor_date(date, "week"),
                        year     = year(date),
                        currency = ifelse(is.na(currency), "EUR", currency),
                        source   = ifelse(is.na(source), "csv", source)
                )
        df
}

combine_expenses <- function(df_list) {
        bind_rows(df_list) %>%
                arrange(date) %>%
                distinct()
}

monthly_summary <- function(df) {
        df %>%
                group_by(month, category) %>%
                summarise(total = sum(amount, na.rm = TRUE), .groups = "drop")
}

category_summary <- function(df, period = NULL) {
        if (!is.null(period)) df <- df %>% filter(month == period)
        df %>%
                group_by(category) %>%
                summarise(
                        total       = sum(amount, na.rm = TRUE),
                        n_trans     = n(),
                        avg_trans   = mean(amount, na.rm = TRUE),
                        .groups     = "drop"
                ) %>%
                arrange(desc(total))
}

top_merchants <- function(df, n = 10) {
        df %>%
                group_by(merchant) %>%
                summarise(total = sum(amount, na.rm = TRUE), n = n(), .groups = "drop") %>%
                arrange(desc(total)) %>%
                head(n)
}

budget_status <- function(df, budgets, period = NULL) {
        if (!is.null(period)) df <- df %>% filter(month == period)
        spent <- category_summary(df)
        tibble(category = names(budgets), budget = unname(budgets)) %>%
                left_join(spent %>% select(category, total), by = "category") %>%
                mutate(
                        total   = replace_na(total, 0),
                        pct     = round(total / budget * 100, 1),
                        status  = case_when(pct >= 100 ~ "over", pct >= 80 ~ "warning", TRUE ~ "ok"),
                        remaining = budget - total
                )
}

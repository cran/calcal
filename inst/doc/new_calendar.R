## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(calcal)

## -----------------------------------------------------------------------------
gregorian_to_rd <- function(date) {
  result <- 365 * (date$year - 1) + # Ordinary days since day 0 to start of year
    (date$year - 1) %/% 4 - # Adjust for leap years
    (date$year - 1) %/% 100 + # Remove century leap years
    (date$year - 1) %/% 400 + # Add back 400-year leap years
    (367 * date$month - 362) %/% 12 # Add days in prior months this year
  # Adjust if a leap year
  adjustment <- (date$month > 2) * (leap_year(date$year) - 2)
  # Add days in current month
  result + adjustment + date$day
}
leap_year <- function(year) {
  (year %% 4 == 0) & !(year %% 400 %in% c(100, 200, 300))
}

## -----------------------------------------------------------------------------
rd_to_gregorian <- function(rd) {
  # Calculate the year
  d0 <- rd - 1
  n400 <- d0 %/% 146097 # Completed 400-year cycles
  d1 <- d0 %% 146097 # Prior days not in n400
  n100 <- d1 %/% 36524 # 100-year cycles not in n400
  d2 <- d1 %% 36524 # Prior days not in n400 or n100
  n4 <- d2 %/% 1461 # 4-year cycles not in n400 or n100
  d3 <- d2 %% 1461 # Prior days not in n400, n100, or n4
  n1 <- d3 %/% 365 # Years not in n400, n100, or n4
  year <- 400 * n400 + 100 * n100 + 4 * n4 + n1
  # leap year adjustment
  year <- year + !(n100 == 4 | n1 == 4)
  # Calculate the month
  jan1 <- gregorian_to_rd(list(year = year, month = 1, day = 1))
  mar1 <- gregorian_to_rd(list(year = year, month = 3, day = 1))
  correction <- (rd >= mar1) * (2 - leap_year(year))
  month <- (12 * (rd - jan1 + correction) + 373) %/% 367
  # Calculate the day by subtraction
  day1_of_month <- gregorian_to_rd(list(year = year, month = month, day = 1))
  day <- 1 + rd - day1_of_month
  # Return the dates as a list
  list(year = year, month = month, day = day)
}

## -----------------------------------------------------------------------------
validate_gregorian <- function(date) {
  if (any(date$month < 1 | date$month > 12, na.rm = TRUE)) {
    stop("month must be between 1 and 12")
  } else if (any(date$day > 30 & date$month %in% c(4, 6, 9, 11), na.rm = TRUE)) {
    stop("day must be between 1 and 30")
  } else if (any(date$day > 29 & date$month == 2, na.rm = TRUE)) {
    stop("days in February must be between 1 and 29")
  } else if (any(date$day > 28 & date$month == 2 & leap_year(date$year), na.rm = TRUE)) {
    stop("days in February must be between 1 and 28 when not a leap year")
  } else if (any(date$day < 1 | date$day > 31, na.rm = TRUE)) {
    stop("day must be between 1 and 31")
  }
}

## -----------------------------------------------------------------------------
format_gregorian <- function(rd) {
  date <- rd_to_gregorian(rd)
  date[["year"]] <- sprintf("%02d", date[["year"]])
  date[["month"]] <- month.name[date[["month"]]]
  date[["day"]] <- sprintf("%02d", date[["day"]])
  paste(date[["year"]], date[["month"]], date[["day"]], sep = "-")
}

## -----------------------------------------------------------------------------
Gcal <- new_calendar(
  name = "Gregorian",
  short_name = "G",
  granularities = c("year", "month", "day"),
  validate_granularities = validate_gregorian,
  format = format_gregorian,
  from_rd = rd_to_gregorian,
  to_rd = gregorian_to_rd
)

## -----------------------------------------------------------------------------
as_date("2026-01-01", calendar = Gcal) + 0:10
nd <- new_date(year = 2025, month = 7, day = 18:24, calendar = Gcal)
nd
tibble::tibble(
  greg = nd,
  RD = as.integer(nd)
)

## -----------------------------------------------------------------------------
granularity_names(Gcal)
granularity(nd, "day")
day_of_week(nd)
week_of_year(nd)
month_of_year(nd)

## -----------------------------------------------------------------------------
as_date(nd, calendar = cal_hebrew)
as_iso(nd)

## -----------------------------------------------------------------------------
as_Gregorian <- function(date) {
  as_date(date, calendar = Gcal)
}
as_Gregorian("2026-01-01")


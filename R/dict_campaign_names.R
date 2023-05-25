#' Get the campaign names
#'
#' @return a list with two elements -- `key`, a vector of keys, and `value`, a vector of values
#' @export
#'
#' @examples
#' dict_campaign_names()$key[2] # key value for 2018 Amazing Give
dict_campaign_names <- function() {
  d <- c(
    "2017_amazing_give", "2017 Amazing Give",
    "2018_amazing_give", "2018 Amazing Give",
    "2019_amazing_give", "2019 Amazing Give",
    "2020_amazing_give", "2020 Amazing Give",
    "2020_amazing_give_emergency_relief", "2020 Amazing Give Emergency Relief",
    "2020_end_of_year", "2020 End of Year",
    "2020_annual_campaign", "2020 Annual Campaign",
    "2021_amazing_give", "2021 Amazing Give",
    "2021_amazing give", "2021 Amazing Give",
    "2021_annual_campaign", "2021 Annual Campaign",
    "2021_belk", "2021 Belk",
    "2021_carwash", "2021 Carwash",
    "2021_end_of_year", "2021 End of Year",
    "2021_fall_festival", "2021 Festival",
    "2022_amazing_give", "2022 Amazing Give",
    "2022_annual_campaign", "2022 Annual Campaign",
    "2022_bike_day", "2022 Bike Day",
    "2022_carwash", "2022 Car Wash",
    "2022_end_of_year", "2022 End of Year",
    "2022_fall_festival", "2022 Fall Festival",
    "2022_satch_squared_rufc", "2022 Satch Squared RUFC",
    "2023_amazing_give", "2023 Amazing Give",
    "2023_amazon_smile", "2023 Amazon Smile",
    "2023_annual_campaign", "2023 Annual Campaign",
    "2023_bike_day", "2023 Bike Day",
    "2023_carwash", "2023 Car Wash",
    "2023_facebook", "2023 Facebook",
    "2023_fall_festival", "2023 Fall Festival",
    "2023_ufcc", "2023 UF Campaign for Charities",
    "2023_united_way", "2023 United Way",
    "2023_walgreens", "2023 Walgreens",
    "2024_amazing_give", "2024 Amazing Give",
    "2024_annual_campaign", "2024 Annual Campaign",
    "2024_bike_day", "2024 Bike Day",
    "2024_carwash", "2024 Car Wash",
    "2024_facebook", "2024 Facebook",
    "2024_fall_festival", "2024 Fall Festival",
    "2024_ufcc", "2024 UF Campaign for Charities",
    "2024_united_way", "2024 United Way",
    "2024_walgreens", "2024 Walgreens",
    "unknown", "Unknown"
  )
  list(key = d[c(TRUE, FALSE)], value = d[c(FALSE, TRUE)])
}

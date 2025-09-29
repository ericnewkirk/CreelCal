# generate survey dates based on start date and stratum specifications
gen_survey_dates <- function(start_date,
                             stratum_length,
                             stratum_unit,
                             n_strata) {
  .data <- rlang::.data

  int_fn <- switch(
    stratum_unit,
    "Days" = lubridate::days,
    "Weeks" = lubridate::weeks,
    "Months" = lubridate:::months.numeric
  )

  survey_length <- int_fn(stratum_length) * n_strata

  end_date <- start_date + survey_length

  strata_sd <- 0:(n_strata - 1) * int_fn(stratum_length) + start_date

  tibble::tibble(
    date = seq(start_date, end_date - 1, "day"),
    stratum = findInterval(.data$date, strata_sd),
    weekend = as.integer(
      weekdays(.data$date) %in% c("Saturday", "Sunday") |
        .data$date %in% holidays$date
    )
  )

}

# generate random survey times according to creelcal logic
gen_survey_times <- function(dates,
                             latitude,
                             longitude,
                             after_sunrise,
                             after_sunset,
                             surveys_per_day,
                             weekday_days_per_stratum,
                             weekend_days_per_stratum) {
  .data <- rlang::.data

  strata <- dates |>
    dplyr::group_by(.data$stratum) |>
    dplyr::summarize(
      total_days = dplyr::n(),
      wd_available = sum(.data$weekend == 0),
      unavailable = sum(.data$weekend == 2),
      mandatory = sum(.data$weekend == 3),
      we_available = sum(.data$weekend == 1) - sum(.data$weekend == 3),
      .groups = "drop"
    )

  sel_dates <- dates |>
    dplyr::inner_join(strata, by = "stratum") |>
    dplyr::group_by(.data$stratum, .data$weekend) |>
    dplyr::mutate(
      n_sample = dplyr::case_when(
        .data$weekend == 0 ~ dplyr::if_else(
          wd_available > weekday_days_per_stratum,
          weekday_days_per_stratum,
          wd_available
        ),
        .data$weekend == 1 ~ dplyr::if_else(
          we_available > weekend_days_per_stratum,
          weekend_days_per_stratum,
          we_available
        ),
        .data$weekend == 2 ~ 0L,
        .data$weekend == 3 ~ dplyr::n(),
      ),
      selected = .data$date %in% sample(.data$date, .data$n_sample[1])
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$selected)

  sun_times <- suncalc::getSunlightTimes(
    sel_dates$date,
    latitude,
    longitude,
    keep = c("sunrise", "sunset"),
    tz = Sys.timezone()
  ) |>
    dplyr::select(dplyr::all_of(c("sunrise", "sunset")))

  sel_dates |>
    dplyr::bind_cols(sun_times) |>
    dplyr::mutate(
      min_survey = lubridate::floor_date(
        .data$sunrise + lubridate::minutes(after_sunrise),
        "5 minutes"
      ),
      max_survey = lubridate::ceiling_date(
        .data$sunset + lubridate::minutes(after_sunset),
        "5 minutes"
      ),
      n_int_5 = as.numeric(
        difftime(.data$max_survey, .data$min_survey, units = "mins")
      ) %/% 5 + 1,
      int_avail = .data$n_int_5 %/% surveys_per_day,
      offset = purrr::map_int(
        .data$int_avail,
        ~ sample(seq(0, .x - 1, 1), 1) * 5
      ),
      rand_start = .data$min_survey + lubridate::minutes(.data$offset),
      survey_times = purrr::map2(
        .data$rand_start,
        .data$int_avail,
        \(rs, ia) {
          rs + lubridate::minutes(0:(surveys_per_day - 1) * ia * 5)
        }
      )
    ) |>
    tidyr::unnest_longer(
      dplyr::all_of("survey_times"),
      values_to = "survey_time"
    ) |>
    dplyr::select(1:3 | dplyr::all_of("survey_time")) |>
    dplyr::mutate(location = NA_character_)

}

summarize_daylength <- function(dates,
                                times,
                                latitude,
                                longitude,
                                after_sunrise,
                                after_sunset) {

  sun_times <- suncalc::getSunlightTimes(
    dates$date,
    latitude,
    longitude,
    keep = c("sunrise", "sunset"),
    tz = Sys.timezone()
  ) |>
    dplyr::select(dplyr::all_of(c("sunrise", "sunset"))) |>
    magrittr::set_names(c("fd_start", "fd_end"))

  survey_times <- times |>
    dplyr::group_by(.data$date) |>
    dplyr::summarize(
      first_count = min(.data$survey_time),
      last_count = max(.data$survey_time),
      .groups = "drop"
    )

  dates |>
    dplyr::bind_cols(sun_times) |>
    dplyr::mutate(
      sd_start = .data$fd_start + lubridate::minutes(after_sunrise),
      sd_end = .data$fd_end + lubridate::minutes(after_sunset)
    ) |>
    dplyr::left_join(survey_times, by = "date") |>
    dplyr::group_by(.data$stratum) |>
    dplyr::summarize(
      sfd = sub("^0", "", format(min(.data$date), "%m/%d/%y")),
      sld = sub("^0", "", format(max(.data$date), "%m/%d/%y")),
      fds = average_time(.data$fd_start),
      fde = average_time(.data$fd_end),
      fdl = average_duration(.data$fd_start, .data$fd_end),
      sds = average_time(.data$sd_start),
      sde = average_time(.data$sd_end),
      sdl = average_duration(.data$sd_start, .data$sd_end),
      cdl = average_duration(.data$first_count, .data$last_count, 5),
      .groups = "drop"
    ) |>
    dplyr::select(-dplyr::any_of("stratum"))

}

average_time <- function(times) {
  avg <- round(
    mean(
      lubridate::hour(times) * 60 + lubridate::minute(times),
      na.rm = TRUE
    )
  )
  sprintf("%d:%02d", avg %/% 60, avg %% 60)
}

average_duration <- function(starts, ends, rnd_min = 1) {
  avg <- round(
    mean(
      as.numeric(difftime(ends, starts, units = "mins")),
      na.rm = TRUE
    )
  )
  avg <- round(avg / rnd_min) * rnd_min
  sprintf("%d:%02d", avg %/% 60, avg %% 60)
}

# wyoming bounding box as sf object for setting leaflet bounds
wy_bbox <- sf::st_polygon(list(
  rbind(
    c(-111.05631, 40.99474),
    c(-111.05631, 45.00570),
    c(-104.05242, 45.00570),
    c(-104.05242, 40.99474),
    c(-111.05631, 40.99474)
  )
)) |>
  sf::st_sfc(crs = 4326) |>
  sf::st_as_sf()

# css for red asterisk after label when required input is empty
req_input_css_tag <- function() {

  shiny::tags$style(
    ".required-input label::after {
      color: red;
      content: '*';
      padding-left: 5px;
    }"
  )
}

# holidays through 2040
holidays <- tibble::tribble(
  ~ date, ~ holiday,
  as.Date("2023-01-02"), "New Year's Day (Observance)",
  as.Date("2023-01-16"), "Martin Luther King Jr./WY Equality Day",
  as.Date("2023-02-20"), "President's Day",
  as.Date("2023-05-29"), "Memorial Day",
  as.Date("2023-06-19"), "Juneteenth National Independence Day",
  as.Date("2023-07-04"), "Independence Day",
  as.Date("2023-09-04"), "Labor Day",
  as.Date("2023-11-10"), "Veterans Day (Observance)",
  as.Date("2023-11-23"), "Thanksgiving Day",
  as.Date("2023-12-25"), "Christmas Day",
  as.Date("2024-01-01"), "New Year's Day",
  as.Date("2024-01-15"), "Martin Luther King Jr./WY Equality Day",
  as.Date("2024-02-19"), "President's Day",
  as.Date("2024-05-27"), "Memorial Day",
  as.Date("2024-06-19"), "Juneteenth National Independence Day",
  as.Date("2024-07-04"), "Independence Day",
  as.Date("2024-09-02"), "Labor Day",
  as.Date("2024-11-11"), "Veterans Day",
  as.Date("2024-11-28"), "Thanksgiving Day",
  as.Date("2024-12-25"), "Christmas Day",
  as.Date("2025-01-01"), "New Year's Day",
  as.Date("2025-01-20"), "Martin Luther King Jr./WY Equality Day",
  as.Date("2025-02-17"), "President's Day",
  as.Date("2025-05-26"), "Memorial Day",
  as.Date("2025-06-19"), "Juneteenth National Independence Day",
  as.Date("2025-07-04"), "Independence Day",
  as.Date("2025-09-01"), "Labor Day",
  as.Date("2025-11-11"), "Veterans Day",
  as.Date("2025-11-27"), "Thanksgiving Day",
  as.Date("2025-12-25"), "Christmas Day",
  as.Date("2026-01-01"), "New Year's Day",
  as.Date("2026-01-19"), "Martin Luther King Jr./WY Equality Day",
  as.Date("2026-02-16"), "President's Day",
  as.Date("2026-05-25"), "Memorial Day",
  as.Date("2026-06-19"), "Juneteenth National Independence Day",
  as.Date("2026-07-03"), "Independence Day (Observance)",
  as.Date("2026-09-07"), "Labor Day",
  as.Date("2026-11-11"), "Veterans Day",
  as.Date("2026-11-26"), "Thanksgiving Day",
  as.Date("2026-12-25"), "Christmas Day",
  as.Date("2027-01-01"), "New Year's Day",
  as.Date("2027-01-18"), "Martin Luther King Jr./WY Equality Day",
  as.Date("2027-02-15"), "President's Day",
  as.Date("2027-05-31"), "Memorial Day",
  as.Date("2027-06-18"), "Juneteenth National Independence Day (Observance)",
  as.Date("2027-07-05"), "Independence Day (Observance)",
  as.Date("2027-09-06"), "Labor Day",
  as.Date("2027-11-11"), "Veterans Day",
  as.Date("2027-11-25"), "Thanksgiving Day",
  as.Date("2027-12-24"), "Christmas Day (Observance)",
  as.Date("2027-12-31"), "New Year's Day (Observance)",
  as.Date("2028-01-17"), "Martin Luther King Jr./WY Equality Day",
  as.Date("2028-02-21"), "President's Day",
  as.Date("2028-05-29"), "Memorial Day",
  as.Date("2028-06-19"), "Juneteenth National Independence Day",
  as.Date("2028-07-04"), "Independence Day",
  as.Date("2028-09-04"), "Labor Day",
  as.Date("2028-11-10"), "Veterans Day (Observance)",
  as.Date("2028-11-23"), "Thanksgiving Day",
  as.Date("2028-12-25"), "Christmas Day",
  as.Date("2029-01-01"), "New Year's Day",
  as.Date("2029-01-15"), "Martin Luther King Jr./WY Equality Day",
  as.Date("2029-02-19"), "President's Day",
  as.Date("2029-05-28"), "Memorial Day",
  as.Date("2029-06-19"), "Juneteenth National Independence Day",
  as.Date("2029-07-04"), "Independence Day",
  as.Date("2029-09-03"), "Labor Day",
  as.Date("2029-11-12"), "Veterans Day (Observance)",
  as.Date("2029-11-22"), "Thanksgiving Day",
  as.Date("2029-12-25"), "Christmas Day",
  as.Date("2030-01-01"), "New Year's Day",
  as.Date("2030-01-21"), "Martin Luther King Jr./WY Equality Day",
  as.Date("2030-02-18"), "President's Day",
  as.Date("2030-05-27"), "Memorial Day",
  as.Date("2030-06-19"), "Juneteenth National Independence Day",
  as.Date("2030-07-04"), "Independence Day",
  as.Date("2030-09-02"), "Labor Day",
  as.Date("2030-11-11"), "Veterans Day",
  as.Date("2030-11-28"), "Thanksgiving Day",
  as.Date("2030-12-25"), "Christmas Day",
  as.Date("2031-01-01"), "New Year's Day",
  as.Date("2031-01-20"), "Martin Luther King Jr./WY Equality Day",
  as.Date("2031-02-17"), "President's Day",
  as.Date("2031-05-26"), "Memorial Day",
  as.Date("2031-06-19"), "Juneteenth National Independence Day",
  as.Date("2031-07-04"), "Independence Day",
  as.Date("2031-09-01"), "Labor Day",
  as.Date("2031-11-11"), "Veterans Day",
  as.Date("2031-11-27"), "Thanksgiving Day",
  as.Date("2031-12-25"), "Christmas Day",
  as.Date("2032-01-01"), "New Year's Day",
  as.Date("2032-01-19"), "Martin Luther King Jr./WY Equality Day",
  as.Date("2032-02-16"), "President's Day",
  as.Date("2032-05-31"), "Memorial Day",
  as.Date("2032-06-18"), "Juneteenth National Independence Day (Observance)",
  as.Date("2032-07-05"), "Independence Day (Observance)",
  as.Date("2032-09-06"), "Labor Day",
  as.Date("2032-11-11"), "Veterans Day",
  as.Date("2032-11-25"), "Thanksgiving Day",
  as.Date("2032-12-24"), "Christmas Day (Observance)",
  as.Date("2032-12-31"), "New Year's Day (Observance)",
  as.Date("2033-01-17"), "Martin Luther King Jr./WY Equality Day",
  as.Date("2033-02-21"), "President's Day",
  as.Date("2033-05-30"), "Memorial Day",
  as.Date("2033-06-20"), "Juneteenth National Independence Day (Observance)",
  as.Date("2033-07-04"), "Independence Day",
  as.Date("2033-09-05"), "Labor Day",
  as.Date("2033-11-11"), "Veterans Day",
  as.Date("2033-11-24"), "Thanksgiving Day",
  as.Date("2033-12-26"), "Christmas Day (Observance)",
  as.Date("2034-01-02"), "New Year's Day (Observance)",
  as.Date("2034-01-16"), "Martin Luther King Jr./WY Equality Day",
  as.Date("2034-02-20"), "President's Day",
  as.Date("2034-05-29"), "Memorial Day",
  as.Date("2034-06-19"), "Juneteenth National Independence Day",
  as.Date("2034-07-04"), "Independence Day",
  as.Date("2034-09-04"), "Labor Day",
  as.Date("2034-11-10"), "Veterans Day (Observance)",
  as.Date("2034-11-23"), "Thanksgiving Day",
  as.Date("2034-12-25"), "Christmas Day",
  as.Date("2035-01-01"), "New Year's Day",
  as.Date("2035-01-15"), "Martin Luther King Jr./WY Equality Day",
  as.Date("2035-02-19"), "President's Day",
  as.Date("2035-05-28"), "Memorial Day",
  as.Date("2035-06-19"), "Juneteenth National Independence Day",
  as.Date("2035-07-04"), "Independence Day",
  as.Date("2035-09-03"), "Labor Day",
  as.Date("2035-11-12"), "Veterans Day (Observance)",
  as.Date("2035-11-22"), "Thanksgiving Day",
  as.Date("2035-12-25"), "Christmas Day",
  as.Date("2036-01-01"), "New Year's Day",
  as.Date("2036-01-21"), "Martin Luther King Jr./WY Equality Day",
  as.Date("2036-02-18"), "President's Day",
  as.Date("2036-05-26"), "Memorial Day",
  as.Date("2036-06-19"), "Juneteenth National Independence Day",
  as.Date("2036-07-04"), "Independence Day",
  as.Date("2036-09-01"), "Labor Day",
  as.Date("2036-11-11"), "Veterans Day",
  as.Date("2036-11-27"), "Thanksgiving Day",
  as.Date("2036-12-25"), "Christmas Day",
  as.Date("2037-01-01"), "New Year's Day",
  as.Date("2037-01-19"), "Martin Luther King Jr./WY Equality Day",
  as.Date("2037-02-16"), "President's Day",
  as.Date("2037-05-25"), "Memorial Day",
  as.Date("2037-06-19"), "Juneteenth National Independence Day",
  as.Date("2037-07-03"), "Independence Day (Observance)",
  as.Date("2037-09-07"), "Labor Day",
  as.Date("2037-11-11"), "Veterans Day",
  as.Date("2037-11-26"), "Thanksgiving Day",
  as.Date("2037-12-25"), "Christmas Day",
  as.Date("2038-01-01"), "New Year's Day",
  as.Date("2038-01-18"), "Martin Luther King Jr./WY Equality Day",
  as.Date("2038-02-15"), "President's Day",
  as.Date("2038-05-31"), "Memorial Day",
  as.Date("2038-06-18"), "Juneteenth National Independence Day (Observance)",
  as.Date("2038-07-05"), "Independence Day (Observance)",
  as.Date("2038-09-06"), "Labor Day",
  as.Date("2038-11-11"), "Veterans Day",
  as.Date("2038-11-25"), "Thanksgiving Day",
  as.Date("2038-12-24"), "Christmas Day (Observance)",
  as.Date("2038-12-31"), "New Year's Day (Observance)",
  as.Date("2039-01-17"), "Martin Luther King Jr./WY Equality Day",
  as.Date("2039-02-21"), "President's Day",
  as.Date("2039-05-30"), "Memorial Day",
  as.Date("2039-06-20"), "Juneteenth National Independence Day (Observance)",
  as.Date("2039-07-04"), "Independence Day",
  as.Date("2039-09-05"), "Labor Day",
  as.Date("2039-11-11"), "Veterans Day",
  as.Date("2039-11-24"), "Thanksgiving Day",
  as.Date("2039-12-26"), "Christmas Day (Observance)",
  as.Date("2040-01-02"), "New Year's Day (Observance)",
  as.Date("2040-01-16"), "Martin Luther King Jr./WY Equality Day",
  as.Date("2040-02-20"), "President's Day",
  as.Date("2040-05-28"), "Memorial Day",
  as.Date("2040-06-19"), "Juneteenth National Independence Day",
  as.Date("2040-07-04"), "Independence Day",
  as.Date("2040-09-03"), "Labor Day",
  as.Date("2040-11-12"), "Veterans Day (Observance)",
  as.Date("2040-11-22"), "Thanksgiving Day",
  as.Date("2040-12-25"), "Christmas Day"
)

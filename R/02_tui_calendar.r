# calendar display properties
tui_calendars <- tibble::tibble(
  id = 1:5,
  name = c("Weekday", "Weekend/Holiday", "Unavailable", "Mandatory", "Strata"),
  color = c(rep("white", 4), "black"),
  bgColor = c(
    "midnightblue", "seagreen", "firebrick", "black", "lightgray"
  ),
  borderColor = c(
    "midnightblue", "seagreen", "firebrick", "black", "black"
  )
)

# convert survey dates to toastui calendar format
survey_dates_to_tui <- function(dates) {
  .data <- rlang::.data

  cal_dates <- dates |>
    dplyr::transmute(
      calendarId = .data$weekend + 1,
      title = tui_calendars$name[.data$calendarId],
      body = title,
      recurrenceRule = NA_character_,
      start = .data$date,
      end = .data$date,
      category = "allday",
      location = NA_character_
    )

  cal_strata <- dates |>
    dplyr::group_by(.data$stratum) |>
    dplyr::summarize(
      start = min(.data$date),
      end = max(.data$date),
      .groups = "drop"
    ) |>
    dplyr::transmute(
      calendarId = 5,
      title = paste("Stratum", .data$stratum),
      body = title,
      recurrenceRule = NA_character_,
      start = .data$start,
      end = .data$end,
      category = "allday",
      location = NA_character_
    )

  cal_strata |>
    dplyr::bind_rows(cal_dates)

}

# convert survey times to toastui calendar format
survey_times_to_tui <- function(times, tz) {
  .data <- rlang::.data

  times |>
    dplyr::transmute(
      calendarId = .data$weekend + 1,
      body = sprintf(
        "Randomized survey time - Stratum %d - %s",
        .data$stratum,
        tui_calendars$name[.data$calendarId]
      ),
      recurrenceRule = NA_character_,
      start = lubridate::with_tz(.data$survey_time, tz),
      end = .data$start,
      category = "time",
      location = .data$location,
      title = dplyr::if_else(
        is.na(.data$location),
        format(.data$start, "%R"),
        paste(format(.data$start, "%R"), .data$location, sep = " - ")
      )
    ) |>
    dplyr::relocate(dplyr::all_of("title"), .after = 1)

}

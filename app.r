ui <- function(request) {

  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "Creel Survey App"),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        id = "tabs",
        shinydashboard::menuItem(
          "Create Survey Schedule",
          tabName = "new_cal"
        )
      ),
      collapsed = TRUE
    ),
    shinydashboard::dashboardBody(
      shinyjs::useShinyjs(),
      req_input_css_tag(),
      shiny::fluidRow(
        shiny::column(width = 2),
        shiny::column(
          width = 8,
          shinyglide::glide(
            screen1_ui,
            screen2_ui,
            screen3_ui,
            screen4_ui,
            screen5_ui,
            screen6_ui,
            screen7_ui
          )
        ),
        shiny::column(width = 2)
      )
    )
  )

}

server <- function(input, output, session) {
  .data <- rlang::.data

  ##############################################################################
  #
  # reactive values
  #
  ##############################################################################
  rct <- shiny::reactiveValues(
    # survey coordinates
    lat = 43.0760,
    lng = -107.2903,
    # survey date data
    dates = NULL,
    # survey time data
    times = NULL,
    # current active date in dates calendar
    def_date = NULL
  )

  rtz <- shiny::reactive({
    lutz::tz_lookup_coords(rct$lat, rct$lng, "accurate")
  })

  ##############################################################################
  #
  # validation functions
  #
  ##############################################################################
  # check that all inputs needed to generate survey dates are provided
  date_check <- shiny::reactive({

    date_inputs <- list(
      input$sd,
      input$int,
      input$int_units,
      input$n_int
    )

    all(sapply(date_inputs, shiny::isTruthy))

  })

  # check that all inputs needed to generate survey times are provided
  time_check <- shiny::reactive({

    time_inputs <- list(
      rct$dates,
      rct$lat,
      rct$lng,
      input$es,
      input$ls,
      input$spd,
      input$wd,
      input$we
    )

    all(sapply(time_inputs, shiny::isTruthy))

  })

  ##############################################################################
  #
  # input processing/data generation functions
  #
  ##############################################################################
  # generate survey dates based on input values
  shiny::observe({
    shiny::req(date_check())

    rct$dates <- gen_survey_dates(
      input$sd,
      input$int,
      input$int_units,
      input$n_int
    )

  })

  # generate survey times based on input values
  shiny::observe({
    shiny::req(time_check())

    rct$times <- gen_survey_times(
      rct$dates,
      rct$lat,
      rct$lng,
      input$es,
      input$ls,
      input$spd,
      input$wd,
      input$we
    )

  })

  # stratum length string
  sl_string <- shiny::reactive({
    shiny::req(input$int, input$int_units)

    paste(
      input$int,
      ifelse(input$int == 1, sub("s$", "", input$int_units), input$int_units)
    )

  })

  # table of all input settings
  settings_tbl <- shiny::reactive({
    shiny::req(date_check(), time_check())

    tibble::tribble(
      ~ Setting, ~ Value,
      "Survey Name", input$sname,
      "Latitude", as.character(rct$lat),
      "Longitude", as.character(rct$lng),
      "Start Date", format(input$sd, "%D"),
      "Stratum Length", sl_string(),
      "Number of Strata", as.character(input$n_int),
      "Counts per day", as.character(input$spd),
      "Weekdays to survey", as.character(input$wd),
      "Weekend days to survey", as.character(input$we),
      "Sunrise offset", paste(input$es, "minutes"),
      "Sunset offset", paste(input$ls, "minutes")
    )

  })

  # table of average day lengths
  daylength_tbl <- shiny::reactive({
    shiny::req(date_check(), time_check())

    summarize_daylength(
      rct$dates,
      rct$times,
      rct$lat,
      rct$lng,
      input$es,
      input$ls
    )

  })
  
  ##############################################################################
  #
  # screen 1 reactivity
  #
  ##############################################################################
  # survey location picker map
  output$ll_map <- leaflet::renderLeaflet({

    leaflet::leaflet(wy_bbox) |>
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopo) |>
      leaflet::addPolygons(group = "wy") |>
      leaflet::addMarkers(
        lng = -107.2903,
        lat = 43.0760,
        group = "creel_marker"
      ) |>
      leaflet::hideGroup("wy")

  })

  # update reactive values on map click
  shiny::observeEvent(input$ll_map_click, {

    rct$lat <- input$ll_map_click$lat
    rct$lng <- input$ll_map_click$lng

  })

  # update marker location in leaflet
  shiny::observe({

    leaflet::leafletProxy("ll_map") |>
      leaflet::clearGroup("creel_marker") |>
      leaflet::addMarkers(
        lng = rct$lng,
        lat = rct$lat,
        group = "creel_marker"
      )

  })

  # display click coordinates
  output$ll <- shiny::renderUI({

    shiny::div(
      shiny::span(
        shiny::tags$strong("Survey coordinates: "),
        paste(rct$lat, rct$lng, sep = ", ")
      ),
      style = "margin-top: 12px;"
    )

  })

  # show/hide asterisk when survey name changes
  shiny::observeEvent(input$sname, {

    shinyjs::toggleClass(
      "sname-req",
      "required-input",
      !(length(input$sname) > 0 && nchar(input$sname) > 0)
    )

  })

  ##############################################################################
  #
  # screen 2 reactivity
  #
  ##############################################################################

  # show/hide asterisk when start date changes
  shiny::observeEvent(input$sd, {

    rct$def_date <- input$sd

    shinyjs::toggleClass(
      "sd-req",
      "required-input",
      !(length(input$sd) > 0 && nchar(input$sd) > 0)
    )

  })

  # show/hide asterisk when stratum length changes
  shiny::observeEvent(input$int, {

    shinyjs::toggleClass(
      "int-req",
      "required-input",
      !(length(input$int) > 0 && input$int > 0)
    )

  })

  # show/hide asterisk when length unit changes and update sliders
  shiny::observeEvent(input$int_units, {

    shinyjs::toggleClass(
      "int_units-req",
      "required-input",
      !(length(input$int_units) > 0 && nchar(input$int_units) > 0)
    )

    shiny::req(input$int_units)

    # update number of strata slider
    sl_value <- dplyr::case_when(
      input$int_units == "Days" ~ 6L,
      input$int_units == "Weeks" ~ 6L,
      input$int_units == "Months" ~ 3L
    )

    sl_max <- dplyr::case_when(
      input$int_units == "Days" ~ 30L,
      input$int_units == "Weeks" ~ 26L,
      input$int_units == "Months" ~ 12L
    )

    shiny::updateSliderInput(
      session = session,
      "n_int",
      value = sl_value,
      max = sl_max
    )

    # update number of weekdays per strata slider
    sl_value <- dplyr::case_when(
      input$int_units == "Days" ~ 2L,
      input$int_units == "Weeks" ~ 2L,
      input$int_units == "Months" ~ 4L
    )

    sl_max <- dplyr::case_when(
      input$int_units == "Days" ~ 10L,
      input$int_units == "Weeks" ~ 15L,
      input$int_units == "Months" ~ 20L
    )

    shiny::updateSliderInput(
      session = session,
      "wd",
      value = sl_value,
      max = sl_max
    )

    # update number of weekend days per strata slider
    shiny::updateSliderInput(
      session = session,
      "we",
      value = sl_value,
      max = sl_max
    )

  })

  # no asterisk for sliders, can't be set to NULL/""/etc

  ##############################################################################
  #
  # screen 3 reactivity
  #
  ##############################################################################
  # survey date calendar
  output$survey_dates_cal <- toastui::renderCalendar({
    shiny::req(date_check())

    rct$dates |>
      survey_dates_to_tui() |>
      toastui::calendar(
        defaultDate = rct$def_date,
        navigation = TRUE,
        useDetailPopup = FALSE,
        navOpts = toastui::navigation_options(
          today_label = "Today",
          prev_label = "Previous Month",
          next_label = "Next Month",
          fmt_date = "MMM DD, YYYY"
        )
      ) |>
      toastui::cal_props(as.data.frame(tui_calendars)) |>
      toastui::cal_month_options(isAlways6Week = FALSE) |>
      toastui::cal_timezone(rtz()) |>
      toastui::cal_events(
        clickSchedule = htmlwidgets::JS(
          "function(event) {
            Shiny.setInputValue(
              'survey_dates_cal_click',
              {
                id: event.schedule.id,
                calendarId: event.schedule.calendarId,
                start: event.schedule.start._date
              }
            );
          }"
        )
      )

  })

  # modal for changing category
  shiny::observeEvent(input$survey_dates_cal_click, {
    shiny::req(input$survey_dates_cal_click$calendarId < 5)

    rct$def_date <- as.Date(input$survey_dates_cal_click$start)

    shiny::showModal(shiny::modalDialog(
      title = "Edit Category",
      shiny::selectInput(
        "date_cat",
        "New Category:",
        choices = tui_calendars |>
          dplyr::filter(.data$id < 5) |>
          dplyr::mutate(weekend = .data$id - 1) |>
          dplyr::pull(.data$weekend, name = .data$name),
        selected = 1
      ),
      shinyjs::hidden(shiny::actionButton(
        "save_date_cat",
        "Save Changes",
        width = "100%"
      ))
    ))

  })

  # show/hide save button in modal
  shiny::observe({
    shiny::req(input$survey_dates_cal_click$start)

    ex_value <- rct$dates |>
      dplyr::filter(date == as.Date(input$survey_dates_cal_click$start)) |>
      dplyr::pull(.data$weekend)

    shinyjs::toggle(
      "save_date_cat",
      condition = ex_value != as.integer(input$date_cat)
    )

  })

  # apply changes from modal
  shiny::observeEvent(input$save_date_cat, {
    shiny::req(input$date_cat)

    new_row <- rct$dates |>
      dplyr::filter(date == as.Date(input$survey_dates_cal_click$start)) |>
      dplyr::mutate(weekend = as.integer(input$date_cat))

    rct$dates <- rct$dates |>
      dplyr::rows_update(new_row, by = "date")

    shiny::removeModal()

  })

  ##############################################################################
  #
  # screen 5 reactivity
  #
  ##############################################################################
  # survey times calendar
  output$survey_times_cal <- toastui::renderCalendar({
    shiny::req(time_check())

    rct$times |>
      survey_times_to_tui(rtz()) |>
      toastui::calendar(
        defaultDate = input$sd,
        navigation = TRUE,
        useDetailPopup = TRUE,
        navOpts = toastui::navigation_options(
          today_label = "Today",
          prev_label = "Previous Month",
          next_label = "Next Month",
          fmt_date = "MMM DD, YYYY"
        )
      ) |>
      toastui::cal_props(as.data.frame(tui_calendars)) |>
      toastui::cal_month_options(isAlways6Week = FALSE) |>
      toastui::cal_timezone(rtz())

  })

  ##############################################################################
  #
  # screen 6 reactivity
  #
  ##############################################################################

  # starting location inputs (name & prob for each loc)
  output$loc <- shiny::renderUI({
    shiny::req(input$n_loc)

    if (input$n_loc > 1) {
      loc_rows <- purrr::map(
        seq(1, input$n_loc, 1),
        \(x) {
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::textInput(
                paste("loc_nm", x, sep = "_"),
                NULL,
                paste("Location", x),
                width = "100%"
              )
            ),
            shiny::column(
              width = 6,
              shiny::numericInput(
                paste("loc_prob", x, sep = "_"),
                NULL,
                1 / input$n_loc,
                min = 0,
                max = 1,
                step = 0.01,
                width = "100%"
              )
            )
          )
        }
      )
      shiny::tagList(
        shiny::fluidRow(
          shiny::column(width = 6, shiny::tags$label("Name")),
          shiny::column(width = 6, shiny::tags$label("Probability"))
        ),
        loc_rows
      )
    }

  })

  loc_ready <- shiny::reactive({
    shiny::req(input$n_loc)

    input$n_loc == 1 | input$n_loc == sum(grepl("^loc_nm", names(input)))

  })

  loc_tbl <- shiny::reactive({
    shiny::req(loc_ready())

    if (input$n_loc > 1) {
      tibble::tibble(
        name = purrr::map_chr(
          seq(1, input$n_loc, 1),
          \(x) {
            input[[paste("loc_nm", x, sep = "_")]]
          }
        ),
        prob = purrr::map_dbl(
          seq(1, input$n_loc, 1),
          \(x) {
            input[[paste("loc_prob", x, sep = "_")]]
          }
        )
      )
    }
    
  })

  shiny::observeEvent(loc_tbl(), {
    shiny::req(loc_ready(), rct$times)

    if (input$n_loc > 1) {
      locs <- rct$times |>
        dplyr::group_by(.data$date) |>
        dplyr::arrange(.data$survey_time) |>
        dplyr::slice(1) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          location = sample(
            loc_tbl()$name,
            length(unique(rct$times$date)),
            replace = TRUE,
            prob = loc_tbl()$prob
          )
        ) |>
        dplyr::select(dplyr::all_of(c("survey_time", "location")))
      rct$times <- rct$times |>
        dplyr::select(-dplyr::all_of("location")) |>
        dplyr::left_join(locs, by = "survey_time")
    } else {
      rct$times <- rct$times |>
        dplyr::mutate(location = NA_character_)
    }

  }, ignoreNULL = FALSE)

  ##############################################################################
  #
  # screen 7 reactivity
  #
  ##############################################################################
  # xlsx download
  output$dl_xl <- shiny::downloadHandler(
    filename = function() {
      paste0(gsub("\\W", "_", input$sname), ".xlsx")
    },
    content = function(file) {
      times_to_xl(rct$times, input$sname, rtz(), file)
    }
  )

  # pdf download
  output$dl_pdf <- shiny::downloadHandler(
    filename = function() {
      paste0(gsub("\\W", "_", input$sname), ".pdf")
    },
    content = function(file) {
      creelcal_pdf(
        calr_plots(rct$dates, rct$times, rtz()),
        settings_tbl(),
        daylength_tbl(),
        file
      )
    }
  )

  # ics download
  output$dl_ics <- shiny::downloadHandler(
    filename = function() {
      paste0(gsub("\\W", "_", input$sname), ".ics")
    },
    content = function(file) {
      times_to_ics(rct$times, input$sname, rtz(), out_file = file)
    }
  )

}

shiny::shinyApp(ui, server)

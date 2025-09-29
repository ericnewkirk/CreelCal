creelcal_pdf <- function(cal_plots, settings_tbl, daylength_tbl, out_file) {

  pdf(out_file, width = 11, height = 8.5)

  lapply(cal_plots, plot)
  grid::grid.newpage()

  grid::grid.draw(gridExtra::tableGrob(settings_tbl, rows = NULL))
  grid::grid.newpage()

  grid::grid.draw(dl_grob(daylength_tbl))
  dev.off()

}

dl_grob <- function(daylength_tbl) {

  header <- gridExtra::tableGrob(
    daylength_tbl[1, 1:4],
    rows = NULL,
    cols = c("Sample Strata", "Fishing Day", "Sample Day", "Count Day")
  )

  tbl <- gridExtra::tableGrob(
    daylength_tbl,
    rows = NULL,
    cols = c(
      "First Day", "Last Day",
      "Start", "End", "Length",
      "Start", "End", "Length",
      "Length"
    )
  )

  out <- gridExtra::gtable_combine(header[1, ], tbl, along = 2)
  out$layout[1:8, c("l", "r")] <- list(c(1, 3, 6, 9), c(2, 5, 8, 9))
  out$widths <- grid::unit(1 / 24, "npc") * c(3, 3, 2, 2, 2, 2, 2, 2, 3)

  out

}

library(stringr)
library("RPostgres")

options(error = function() {
  calls <- sys.calls()
  if (length(calls) >= 2L) {
    sink(stderr())
    on.exit(sink(NULL))
    cat("Backtrace:\n")
    calls <- rev(calls[-length(calls)])
    for (i in seq_along(calls)) {
      cat(i, ": ", deparse(calls[[i]], nlines = 1L), "\n", sep = "")
    }
  }
  if (!interactive()) {
    q(status = 1)
  }
})

format_duration <- function(duration) {
  sprintf("%.2f %s", duration, units(duration))
}

format_timestamp <- function(timestamp) {
  str_replace_all(timestamp, "[ :.]", "_")
}

check_one <- function(connection_supplier,
                      models,
                      root_directory,
                      report_root_directory,
                      input,
                      timestamp = Sys.time(),
                      input_count = 1,
                      input_total_count = 1) {
  connection <- NULL
  tryCatch({
    t0 <- Sys.time()
    connection <- do.call(connection_supplier, args = list())
    log_prefix <- paste0(input_count, "/", input_total_count, " - ")
    cat(sprintf("%sCheck input: %s\n", log_prefix, input))
    c <- import_context$new(
      models,
      root_directory,
      report_root_directory,
      input,
      connection = connection,
      timestamp = format_timestamp(timestamp)
    )
    cat(sprintf("%sLog file:    %s\n", log_prefix, c$log_file()))
    cat(sprintf("%sReport file: %s\n", log_prefix, c$report_file()))
    tryCatch(c$check(), finally = {
      duration <- difftime(Sys.time(), t0)
      c$set_check_duration(duration)
      cat(sprintf("%sSummary:      %s error(s), %s warning(s) (duration: %s)\n",
                  log_prefix, c$check_errors_count(), c$check_warnings_count(),
                  format_duration(duration)))
    })
    c
  },
    finally = {
      if (!is.null(connection)) {
        RPostgres::dbDisconnect(connection)
      }
    })
}

check_all <- function(connection_supplier,
                      models,
                      root_directory,
                      report_root_directory,
                      timestamp = Sys.time()) {
  report_content <- c()
  t0 <- Sys.time()
  report_file <- file.path(report_root_directory, paste0("report-", format_timestamp(timestamp), ".json"))
  cat("Report file: ", report_file, "\n")
  report_directory <- dirname(report_file)
  if (!dir.exists(file.path(report_directory))) {
    dir.create(file.path(report_directory), recursive = TRUE)
  }
  files <- list.files(root_directory,
                      recursive = TRUE,
                      pattern = "(.)+\\.xlsx$",
                      full.names = TRUE)
  input_count <- 0
  input_total_count <- length(files)
  min_duration <- NULL
  max_duration <- NULL
  summary <- list()
  for (input in files) {
    input_count <- input_count + 1
    c <- check_one(connection_supplier,
                   models,
                   root_directory,
                   report_root_directory,
                   input,
                   timestamp,
                   input_count,
                   input_total_count)
    total_duration <- difftime(Sys.time(), t0)
    c_duration <- c$check_duration()
    if (is.null(min_duration) || min_duration > c_duration) {
      min_duration <- c_duration
    }
    if (is.null(max_duration) || max_duration < c_duration) {
      max_duration <- c_duration
    }
    total_duration_str <- format_duration(total_duration)
    cat(sprintf("Checks done on %s/%s files(s) (duration: %s)\n",
                input_count, input_total_count, total_duration_str))

    c_summary <- c$report_content()$summary
    report_content[[input]] <- list(
      duration = sprintf("%.2f %s", c_duration, units(c_duration)),
      errors = c$check_errors_count(),
      warnings = c$check_warnings_count(),
      result_file = c$report_file(),
      log_file = c$log_file(),
      summary = c_summary
    )
    for (t in c("missing_code_list", "missing_data")) {
      t_summary <- c_summary[[t]]
      old_t_summary <- summary[[t]]
      if (is.null(old_t_summary)) {
        old_t_summary <- list()
      }
      for (i in names(t_summary)) {
        old_result <- old_t_summary[[i]]
        if (is.null(old_result)) {
          old_result <- list()
        }
        summary[[t]][[i]] <- unique(append(old_result, t_summary[[i]]))
      }
    }
    units(total_duration) <- "secs"
    write_json(
      c(files_count = input_count,
        total_duration = total_duration_str,
        file_min_duration = format_duration(min_duration),
        file_avg_duration = format_duration(total_duration / input_count),
        file_max_duration = format_duration(max_duration),
        summary = list(summary),
        files = list(report_content)),
      report_file,
      pretty = TRUE,
      auto_unbox = TRUE)
  }
}


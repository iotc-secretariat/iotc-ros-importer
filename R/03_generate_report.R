library(data.table)
library(rmarkdown)
library(knitr)
library(bookdown)
library(htmlwidgets)
library(fontawesome)
library(DT)

PAGE_LENGTH <- 20

out_dt <- function(data, name, error_count, warning_count, suffix = "") {
  dom <- ifelse(nrow(data) < PAGE_LENGTH, "Bfti", "lBftip")
  title <- ""
  if (error_count > 0) {
    title <- sprintf("E:%s", error_count)
  }
  if (warning_count > 0) {
    title <- str_squish(sprintf("%s W:%s", title, warning_count))
  }
  title <- ifelse(str_length(title) > 0, sprintf("%s (%s)", name, title), name)
  htmltools::div(id = paste0(name, suffix), class = "section level2",
                 htmltools::h2(title),
                 datatable(data,
                           autoHideNavigation = TRUE,
                           rownames = FALSE,
                           filter = "bottom",
                           lazyRender = TRUE,
                           fillContainer = FALSE,
                           extensions = "Buttons",
                           options = list(dom = dom, lengthMenu = list(c(10, 20, 50, 100, -1), c('10', '20', '50', '100', 'All')), buttons = list(list(extend = "csv", filename = paste0(name, suffix)))))
  )
}

out_dt_detail <- function(all_data, name) {
  data <- all_data[[name]]
  if (!is.null(data) && nrow(data) > 0) {
    error_count <- nrow(data[category == "ERROR"])
    warning_count <- nrow(data[category == "WARNING"])
    out_dt(data, name, error_count, warning_count)
  }
}

out_dt_summary <- function(all_data, name) {
  data <- all_data[[name]]
  if (!is.null(data) && nrow(data) > 0) {
    if (name == "META") {
      error_count <- nrow(data[category == "ERROR"])
      warning_count <- nrow(data[category == "WARNING"])
    } else {
      error_count <- data[category == "ERROR", .(x = sum(count))]
      error_count <- ifelse(nrow(error_count) == 1, error_count[1][1], 0)
      warning_count <- data[category == "WARNING", .(x = sum(count))]
      warning_count <- ifelse(nrow(warning_count) == 1, warning_count[1][1], 0)
    }
    out_dt(data, name, error_count, warning_count, "-summary")
  }
}

get_file_relative_path <- function(input_file) {
  dir <- dirname(input_file)
  while (basename(dirname(dir)) != "build") {
    dir <- dirname(dir)
  }
  result <- file.path(dirname(dirname(input_file)), basename(input_file))
  str_sub(result, str_length(dir) - 1)
}

generate_report <- function(input_file, template) {
  dir <- dirname(input_file)
  while (basename(dirname(dir)) != "build") {
    dir <- dirname(dir)
  }
  # used in the template
  relative_input_file <- get_file_relative_path(input_file)
  checks_directory <- file.path(dirname(input_file), "checks")
  check_data <- list()
  check_data_summary <- list()
  for (f in list.files(checks_directory,
                       pattern = "(.)+\\.csv",
                       full.names = TRUE)) {
    file_name <- basename(f)
    key <- str_replace(file_name, "\\.csv", "")
    file_data <- fread(f)
    check_data[[key]] <- file_data
    if (key == "META") {
      check_data_summary[[key]] <- file_data
    } else {
      summary <- file_data[, .N, .(column_name, column, value, category, message)][N > 0]
      names(summary) <- c("column_name", "column", "value", "category", "message", "count")
      check_data_summary[[key]] <- summary
    }
  }
  render(template,
         output_format = "html_document",
         output_file = "report.html",
         output_dir = dirname(input_file))
  list(relative_input_file = relative_input_file, summary = check_data_summary)
}

format_timestamp <- function(timestamp) {
  str_replace_all(timestamp, "[ :.]", "_")
}

generate_reports <- function(root_directory, template, force = FALSE) {
  files <- list.files(root_directory,
                      recursive = TRUE,
                      pattern = "(.)+\\.xlsx$",
                      full.names = TRUE)
  task_report <- task_report$new(file.path(root_directory, "03_generate-reports.json"), "Generate reports", "Generate report %3s/%3s : %s", files)
  result <- list()
  for (file in files) {
    task_report$start_task(file)
    input_directory <- dirname(file)
    report_file <- file.path(input_directory, "report.html")
    if (file.exists(report_file)) {
      if (!force) {
        task_report$skip_task(file)
        next
      }
      file.remove(report_file)
    }
    file_result <- suppressMessages(generate_report(file, template))
    for (i in names(file_result$summary)) {
      part_summary <- file_result$summary[[i]]
      if (i == "META") {
        part_summary <- part_summary[, file := file_result$relative_input_file][, .(file, meta_name, meta_value, category, message)]
      } else {
        part_summary <- part_summary[, file := file_result$relative_input_file][, .(file, column_name, column, value, category, message, count)]
      }
      if (i %in% names(result)) {
        result[[i]] <- rbind(result[[i]], part_summary)
      } else {
        result[[i]] <- part_summary
      }
    }
    task_report$end_task(file)
  }
  task_report$end()
  list(files = lapply(files, normalizePath), result = result)
}

generate_all_report <- function(files, result, root_directory, template) {
  suppressMessages(render(template,
         output_format = "html_document",
         output_file = "all-reports.html",
         output_dir = root_directory))
}

generate_LL_report <- function(input_file) {
  generate_report(input_file, "./RMDs/report-LL.Rmd", "./RMDs/all_report.Rmd")
}

generate_PS_report <- function(input_file) {
  generate_report(input_file, "./RMDs/report-PS.Rmd")
}

# result_ll <- generate_all_report("../iotc-ros-input-data/build/LL/2021/TAIWAN/AN_WEN_FA_NO_3_51812", "./RMDs/report-LL.Rmd", force = TRUE)
# result_ll <- generate_all_report("../iotc-ros-input-data/build/LL/2022/EU-FRANCE", "./RMDs/report-LL.Rmd", force = TRUE)
# generate_all_report("../iotc-ros-input-data/build/LL/2022/EU-FRANCE/ROS_LL_data_reporting_EU_FRA_REU_2022_v2", "./RMDs/report-LL.Rmd", force = TRUE)

# result_ll <- generate_reports("../iotc-ros-input-data/build/LL", "./RMDs/report-LL.Rmd", force = TRUE)
# generate_all_report(result_ll$files, result_ll$result, "../iotc-ros-input-data/build/LL", "./RMDs/all_report-LL.Rmd")
# result_ps <- generate_reports("../iotc-ros-input-data/build/PS", "./RMDs/report-PS.Rmd", force = TRUE)
# generate_all_report(result_ps$files, result_ps$result, "../iotc-ros-input-data/build/PS", "./RMDs/all_report-PS.Rmd")

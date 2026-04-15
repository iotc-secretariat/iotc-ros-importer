library(data.table)
library(rmarkdown)
library(knitr)
library(bookdown)
library(htmlwidgets)
library(fontawesome)
library(DT)

PAGE_LENGTH <- 20

out_dt <- function(data, name, error_count, warning_count, suffix = "") {
  dom <- ifelse(nrow(data) < PAGE_LENGTH, "Bfrti", "Bfrtip")
  title <- ""
  if (error_count > 0) {
    title <- sprintf("E:%s",  error_count)
  }
  if (warning_count > 0) {
    title <- str_squish(sprintf("%s W:%s", title, warning_count))
  }
  title <- ifelse (str_length(title)>0, sprintf("%s (%s)", name, title), name)
  htmltools::div(id = paste0(name, suffix), class = "section level2",
                 htmltools::h2(title),
                 datatable(data,
                           autoHideNavigation = TRUE,
                           rownames = FALSE,
                           filter = "bottom",
                           lazyRender = TRUE,
                           extensions = "Buttons",
                           options = list(dom = dom, buttons = list(list(extend = "csv", filename = paste0(name, suffix)))))
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
      summary <- data
      error_count <- nrow(summary[category == "ERROR"])
      warning_count <- nrow(summary[category == "WARNING"])
    } else {
      summary <- data[, .N, .(column_name, column, value, category, message)][N > 0]
      names(summary) <- c("column_name", "column", "value", "category", "message", "count")
      error_count <- summary[category == "ERROR", .(x = sum(count))]
      error_count <- ifelse (nrow(error_count)==1,error_count[1][1],0)
      warning_count <- summary[category == "WARNING", .(x = sum(count))]
      warning_count <- ifelse (nrow(warning_count)==1,warning_count[1][1],0)
    }
    out_dt(summary, name, error_count, warning_count, "-summary")
  }
}

generate_report <- function(input_file, template) {
  dir <- dirname(input_file)
  while (basename(dirname(dir)) != "build") {
    dir <- dirname(dir)
  }
  relative_input_file <- str_sub(input_file, str_length(dir) - 1)
  checks_directory <- file.path(dirname(input_file), "checks")
  check_data_summary <- list()
  check_data <- list()
  for (f in list.files(checks_directory,
                       pattern = "(.)+\\.csv",
                       full.names = TRUE)) {
    file_name <- basename(f)
    check_data[[str_replace(file_name, "\\.csv", "")]] <- fread(f)
  }
  render(template,
         output_format = "html_document",
         output_file = "report.html",
         output_dir = dirname(input_file))
}

generate_all_report <- function(root_directory, template, force = FALSE) {
  files <- list.files(root_directory,
                      recursive = TRUE,
                      pattern = "(.)+\\.xlsx$",
                      full.names = TRUE)
  task_report <- task_report$new(file.path(root_directory,"03_generate-reports.json"), "Generate reports", "Generate report %3s/%3s : %s", files)
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
    suppressMessages(generate_report(file, template))
    task_report$end_task(file)
  }
  task_report$end()
}

generate_LL_report <- function(input_file) {
  generate_report(input_file, "./RMDs/report-LL.Rmd")
}


generate_PS_report <- function(input_file) {
  generate_report(input_file, "./RMDs/report-PS.Rmd")
}

# generate_LL_report("../iotc-ros-input-data/build/LL/2021/TAIWAN/AN_WEN_FA_NO_3_51812/AN WEN FA NO.3(51812).xlsx")
# generate_all_report("../iotc-ros-input-data/build/LL/2022/EU-FRANCE/ROS_LL_data_reporting_EU_FRA_REU_2022_v2", "./RMDs/report-LL.Rmd", force = TRUE)

# generate_all_report("../iotc-ros-input-data/build/LL", "./RMDs/report-LL.Rmd", force = TRUE)
# generate_all_report("../iotc-ros-input-data/build/PS", "./RMDs/report-PS.Rmd", force = TRUE)
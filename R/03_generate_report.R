library(data.table)
library(rmarkdown)
library(knitr)
library(bookdown)
library(htmlwidgets)
library(fontawesome)
library(DT)

PAGE_LENGTH <- 20

compute_error_and_warning_summary <- function(error_count, warning_count) {
  title <- ""
  if (error_count > 0) {
    title <- sprintf("E:%s", error_count)
  }
  if (warning_count > 0) {
    title <- str_squish(sprintf("%s W:%s", title, warning_count))
  }
  if (str_length(title) > 0) {
    return(paste0(" (", title, ")"))
  }
  ""
}

compute_file_error_and_warning_summary <- function(file_count, error_count, warning_count) {
  title <- ""
  if (file_count > 0) {
    title <- sprintf("F:%s", file_count)
  }
  if (error_count > 0) {
    title <- str_squish(sprintf("%s E:%s", title, error_count))
  }
  if (warning_count > 0) {
    title <- str_squish(sprintf("%s W:%s", title, warning_count))
  }
  if (str_length(title) > 0) {
    return(paste0(" (", title, ")"))
  }
  ""
}

out_dt <- function(data, name, error_count, warning_count, suffix = "") {
  dom <- ifelse(nrow(data) < summary_title$get_length(), "Bfti", "lBftip")
  title <- compute_error_and_warning_summary(error_count, warning_count)
  title <- sprintf("%s%s", name, title)
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

summary_title <- R6Class(
  "SummaryTitle",
  public = list(
    initialize = function() {
    },
    set_suffix = function(suffix) {
      private$.suffix <- suffix
    },
    get_suffix = function() {
      private$.suffix
    },
    set_length = function(length) {
      private$.length <- length
      options(DT.options = list(pageLength = length))
    },
    get_length = function() {
      private$.length
    }),
  private = list(
    .suffix = "",
    .length = 20
  )
)
summary_title <- summary_title$new()

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
    out_dt(data, name, error_count, warning_count, paste0("-summary", summary_title$get_suffix()))
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

get_report_filename <- function(timestamp) {
  sprintf("report%s.html", timestamp)
}

generate_report <- function(domain, input_file, template, do_generate_parent_navigation = TRUE, timestamp = "") {
  summary_title$set_suffix("-global")
  summary_title$set_length(20)
  dir <- dirname(input_file)
  while (basename(dirname(dir)) != "build") {
    dir <- dirname(dir)
  }
  relative_input_file <- get_file_relative_path(input_file)
  checks_directory <- file.path(dirname(input_file), CHECKS_INPUT_DIRECTORY)
  check_data <- list()
  check_data_summary <- list()
  total_error_count <- 0
  total_warning_count <- 0
  if (do_generate_parent_navigation) {
    tmp_dir <- dirname(dirname(input_file))
    year <- basename(tmp_dir)
    tmp_dir <- dirname(tmp_dir)
    country <- basename(tmp_dir)
    year_page <- paste0("../", get_report_filename(timestamp))
    country_page <- paste0("../", year_page)
    domain_page <- paste0("../", country_page)
    report_title_prefix <- sprintf("Ros reports > [Domain `%s`](%s) > [Country `%s`](%s) > [Year `%s`](%s) > File `", domain, domain_page, country, country_page, year, year_page)
  } else {
    report_title_prefix <- sprintf("Ros reports > Domain `%s` > File `", domain)
  }
  report_title <- paste0(report_title_prefix, basename(input_file), "`")
  for (f in list.files(checks_directory,
                       pattern = "(.)+\\.csv",
                       full.names = TRUE)) {
    file_name <- basename(f)
    key <- str_replace(file_name, "\\.csv", "")
    file_data <- fread(f, na.strings = c("NA", ""))
    total_error_count <- total_error_count + nrow(file_data[category == "ERROR"])
    total_warning_count <- total_warning_count + nrow(file_data[category == "WARNING"])
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
         output_file = get_report_filename(timestamp),
         output_dir = dirname(input_file))
  list(relative_input_file = relative_input_file,
       summary = check_data_summary,
       total_error_count = as.integer(total_error_count),
       total_warning_count = as.integer(total_warning_count))
}

reports_abstract_navigation_context <- R6Class(
  "ReportsAbstractNavigationContext",
  public = list(
    root_directory = NULL,
    timestamp = NULL,
    files = NULL,
    result = NULL,
    global_result = NULL,
    files_error_count = NULL,
    files_warning_count = NULL,
    last_update = NULL,
    children = NULL,
    parent = NULL,
    initialize = function(root_directory, timestamp, files, result, files_error_count, files_warning_count) {
      stopifnot(!is.null(root_directory))
      stopifnot(!is.null(timestamp))
      stopifnot(!is.null(files))
      stopifnot(!is.null(result))
      stopifnot(!is.null(files_error_count))
      stopifnot(!is.null(files_warning_count))
      self$root_directory <- root_directory
      self$timestamp <- timestamp
      self$files <- files
      self$result <- result
      self$files_error_count <- files_error_count
      self$files_warning_count <- files_warning_count
      self$last_update <- format(Sys.time(), format = '%d %B %Y - %H:%M')
      global_result <- list()
      for (i in names(result)) {
        part_summary <- result[[i]]
        if (i == "META") {
          part_summary <- part_summary[, .(meta_name, meta_value, category, message)][, meta_value := ifelse(meta_value == "", NA, meta_value)]
          part2 <- part_summary[, .N, .(meta_name, meta_value, category, message)][N > 0]
          part2 <- part2[, .(total_count = sum(N)), by = .(meta_name, meta_value, category, message)]
          names(part2) <- c("meta_name", "meta_value", "category", "message", "count")
          setorder(part2, meta_name)
        } else {
          part_summary <- part_summary[, .(column_name, column, value, category, message, count)]
          part2 <- part_summary[, .(total_count = sum(count)), by = .(column_name, column, value, category, message)]
          names(part2) <- c("column_name", "column", "value", "category", "message", "count")
          setorder(part2, column)
        }
        global_result[[i]] <- part2
      }
      self$global_result <- global_result
    },
    report_file_name = function() { get_report_filename(self$timestamp) },
    report_title = function() { NULL },
    compute_children = function() { },
    get_all_nodes = function() {
      if (is.null(self$children)) {
        return(self)
      } else {
        result <- list()
        result <- append(result, self)
        for (i in lapply(self$children, function(x) x$get_all_nodes())) {
          result <- append(result, i)
        }
        result
      }
    },
    detail_prefix = function(level) { str_pad("", 2 * (level + 1)) },
    summary_template = function() { sprintf('report-%s-summary.Rmd', self$get_domain()) },
    type = function() {
      if (self$with_year()) {
        return("YEAR")
      }
      if (self$with_country()) {
        return("COUNTRY")
      }
      if (self$with_domain()) {
        return("DOMAIN")
      }
      NULL
    },
    set_children = function(children) { self$children <- children },
    set_parent = function(parent) { self$parent <- parent },
    with_domain = function() { !is.null(self$get_domain()) },
    get_domain = function() { NULL },
    with_country = function() { !is.null(self$get_country()) },
    get_country = function() { NULL },
    with_year = function() { !is.null(self$get_year()) },
    get_year = function() { NULL },
    get_result = function() { self$result },
    get_files_error_count = function() { self$files_error_count },
    get_files_warning_count = function() { self$files_warning_count },
    get_timestamp = function() { self$timestamp },
    get_root_directory = function() { self$root_directory },
    x_files_error_count = function() { as.integer(sum(as.vector(unlist(self$files_error_count)))) },
    x_files_warning_count = function() { as.integer(sum(as.vector(unlist(self$files_warning_count)))) },
    file_error_and_warning_summary = function() { compute_file_error_and_warning_summary(length(self$files), self$x_files_error_count(), self$x_files_warning_count()) })
)

reports_domain_navigation_context <- R6Class(
  "ReportsDomainNavigationContext", inherit = reports_abstract_navigation_context,
  public = list(
    domain = NULL,
    report_title_text = function() {
      sprintf("Domain `%s`", self$domain)
    },
    report_title = function() {
      sprintf("Ros reports > %s", self$report_title_text())
    },
    generate_details_navigation = function() {
      detail_prefix <- self$detail_prefix(0)
      for (x in names(self$children)) {
        x_file <- paste0("./", x, "/", self$report_file_name())
        x_context <- self$children[[x]]
        cat(sprintf("%s* [%s](%s)%s", detail_prefix, x_context$report_title_text(), x_file, x_context$file_error_and_warning_summary()), sep
          = "\n")
        x_context$generate_details_navigation(self$root_directory, 1)
      }
    },
    initialize = function(root_directory, timestamp, files, result, files_error_count, files_warning_count, domain) {
      super$initialize(root_directory, timestamp, files, result, files_error_count, files_warning_count)
      stopifnot(!is.null(domain))
      self$domain <- domain
    },
    compute_children = function() {
      countries <- unique(lapply(self$files, function(x) { str_split_1(str_sub(x, str_length(self$root_directory) + 2), "/")[[1]] }))
      children <- list()
      for (country in countries) {
        child_root_directory <- file.path(self$root_directory, country)
        child_files <- Filter(function(x) !is.na(x), unique(lapply(self$files, function(x) { ifelse(str_starts(x, child_root_directory), x, NA) })))
        relative_child_files <- lapply(child_files, get_file_relative_path)
        child_result <- lapply(self$result, function(x) x[file %in% relative_child_files])
        child_files_error_count <- self$files_error_count[unlist(lapply(names(self$files_error_count), function(name) name %in% child_files))]
        child_files_warning_count <- self$files_warning_count[unlist(lapply(names(self$files_warning_count), function(name) name %in% child_files))]
        child_context <- reports_country_navigation_context$new(
          child_root_directory,
          self$get_timestamp(),
          child_files,
          child_result,
          child_files_error_count,
          child_files_warning_count,
          self$domain,
          country)
        child_context$set_parent(self)
        children[[country]] <- child_context
        child_context$compute_children()
      }
      super$set_children(children)
    },
    get_domain = function() { self$domain }))

reports_country_navigation_context <- R6Class(
  "ReportsCountryNavigationContext", inherit = reports_domain_navigation_context,
  public = list(
    country = NULL,
    report_title_text = function() {
      sprintf("Country `%s`", self$country)
    },
    report_title = function() {
      sprintf("Ros reports > [%s](%s) > %s", self$parent$report_title_text(), paste0("../", self$report_file_name()), self$report_title_text())
    },
    generate_details_navigation = function(root_directory = NULL, level = 0) {
      if (is.null(root_directory)) {
        root_directory <- self$root_directory
      }
      detail_prefix <- self$detail_prefix(level)
      for (x in names(self$children)) {
        x_directory <- str_sub(file.path(self$root_directory, x), str_length(root_directory) + 2)
        x_file <- paste0("./", x_directory, "/", self$report_file_name())
        x_context <- self$children[[x]]
        cat(sprintf("%s* [%s](%s)%s", detail_prefix, x_context$report_title_text(), x_file, x_context$file_error_and_warning_summary()), sep = "\n")
        x_context$generate_details_navigation(root_directory, level + 1)
      }
    },
    initialize = function(root_directory, timestamp, files, result, files_error_count, files_warning_count, domain, country) {
      super$initialize(root_directory, timestamp, files, result, files_error_count, files_warning_count, domain)
      stopifnot(!is.null(country))
      self$country <- country
    },
    compute_children = function() {
      years <- unique(lapply(self$files, function(x) { str_split_1(str_sub(x, str_length(self$root_directory) + 2), "/")[[1]] }))
      children <- list()
      for (year in years) {
        child_root_directory <- file.path(self$root_directory, year)
        child_files <- Filter(function(x) !is.na(x), unique(lapply(self$files, function(x) { ifelse(str_starts(x, child_root_directory), x, NA) })))
        relative_child_files <- lapply(child_files, get_file_relative_path)
        child_result <- lapply(self$result, function(x) x[file %in% relative_child_files])
        child_files_error_count <- self$files_error_count[unlist(lapply(names(self$files_error_count), function(name) name %in% child_files))]
        child_files_warning_count <- self$files_warning_count[unlist(lapply(names(self$files_warning_count), function(name) name %in% child_files))]
        child_context <- reports_year_navigation_context$new(
          child_root_directory,
          self$get_timestamp(),
          child_files,
          child_result,
          child_files_error_count,
          child_files_warning_count,
          self$domain,
          self$country,
          year)
        child_context$set_parent(self)
        children[[year]] <- child_context
      }
      super$set_children(children)
    },
    get_country = function() { self$country }))

reports_year_navigation_context <- R6Class(
  "ReportsYearNavigationContext", inherit = reports_country_navigation_context,
  public = list(
    year = NULL,
    report_title_text = function() {
      sprintf("Year `%s`", self$year)
    },
    report_title = function() {
      sprintf("Ros reports > [%s](%s) > [%s](%s) > %s", self$parent$parent$report_title_text(), paste0("../../", self$report_file_name()), self$parent$report_title_text(), paste0("../", self$report_file_name()), self$report_title_text())
    },
    initialize = function(root_directory, timestamp, files, result, files_error_count, files_warning_count, domain, country, year) {
      super$initialize(root_directory, timestamp, files, result, files_error_count, files_warning_count, domain, country)
      stopifnot(!is.null(year))
      self$year <- year
    },
    generate_details_navigation = function(root_directory = NULL, level = 0) {
      if (is.null(root_directory)) {
        root_directory <- self$root_directory
      }
      detail_prefix <- self$detail_prefix(level)
      for (x in self$files) {
        x_directory <- str_sub(dirname(x), str_length(root_directory) + 2)
        x_file <- paste0("./", x_directory, "/", self$report_file_name())
        cat(sprintf("%s* [%s](%s)%s", detail_prefix, basename(x), x_file, compute_error_and_warning_summary(self$files_error_count[[x]], self$files_warning_count[[x]])), sep = "\n")
      }
    },
    get_year = function() { self$year }))

generate_domain_reports <- function(domain, root_directory, force = FALSE, do_generate_parent_navigation = TRUE, timestamp = format_timestamp(Sys.time())) {
  output_directory <- file.path(root_directory, domain)
  template <- sprintf("./RMDs/report-%s.Rmd", domain)
  files <- list.files(output_directory,
                      recursive = TRUE,
                      pattern = "(.)+\\.xlsx$",
                      full.names = TRUE)
  task_report <- task_report$new(output_directory, "4_reports", "Generate reports", "Generate report %3s/%3s : %s", files, timestamp)
  total_error_count <- list()
  total_warning_count <- list()
  result <- list()
  file_results <- task_report$run(function(file) {
    input_directory <- dirname(file)
    report_file <- file.path(input_directory, get_report_filename(timestamp))
    if (file.exists(report_file)) {
      if (!force) {
        return(list(skip = TRUE))
      }
      file.remove(report_file)
    }
    file_result <- suppressMessages(generate_report(domain, file, template, do_generate_parent_navigation, timestamp))
    return(list(skip = FALSE, result = file_result))
  })
  for (file in files) {
    file_result <- file_results[[file]]
    total_error_count[[file]] <- file_result$total_error_count
    total_warning_count[[file]] <- file_result$total_warning_count
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
  }
  x <- reports_domain_navigation_context$new(output_directory, timestamp, files, result, total_error_count, total_warning_count, domain)
  x$compute_children()
  x
}

generate_main_parent_report <- function(context, template = "./RMDs/all_report.Rmd") {
  nodes <- context$get_all_nodes()
  task_report <- task_report$new(context$root_directory, "4_reports_summary", "Generate summary reports", "Generate summary report %3s/%3s : %s", nodes, context$timestamp, to_string_file = function(x) x$report_title())
  task_report$run(function(context) {
    suppressMessages(render(template,
                            output_format = "html_document",
                            output_file = context$report_file_name(),
                            output_dir = context$root_directory))
    return(list(skip = FALSE, result = NULL))
  })
  invisible()
}

generate_LL_report <- function(input_file) {
  generate_report("LL", input_file, "./RMDs/report-LL.Rmd", FALSE)
}

generate_PS_report <- function(input_file) {
  generate_report("PS", input_file, "./RMDs/report-PS.Rmd", FALSE)
}

timestamp <- "-2026-04-20"
# result_ll <- generate_domain_reports("LL", "../iotc-ros-input-data/build", force = TRUE, do_generate_parent_navigation = TRUE, timestamp = timestamp)
# generate_main_parent_report(result_ll)
# result_ps <- generate_domain_reports("PS", "../iotc-ros-input-data/build", force = TRUE, do_generate_parent_navigation = TRUE, timestamp = timestamp)
# generate_main_parent_report(result_ps)

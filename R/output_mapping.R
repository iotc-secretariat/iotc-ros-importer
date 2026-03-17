library(R6)
library(jsonlite)
library(data.table)

ouput_mapping <- R6Class(
  "OutputMapping",
  public = list(
    initialize = function(domain, version, directory) {
      stopifnot(!is.na(domain), is.character(domain), nchar(domain) > 0)
      stopifnot(!is.na(version), is.character(version), nchar(version) > 0)
      stopifnot(!is.null(directory))
      private$.name <- paste(domain, version, sep = "-")
      file <- paste(directory, domain, version, "output-mapping.json", sep = "/")
      json = json = jsonlite::read_json(file)
      private$.meta_sheet <- json$META
      private$.sheets <- json$sheets
    },
    name = function() {
      private$.name
    },
    meta_sheet = function() {
      private$.meta_sheet
    },
    meta_cell_names = function() {
      names(private$.meta_sheet)
    },
    sheets = function() {
      private$.sheets
    },
    sheet_names = function() {
      names(private$.sheets)
    },
    sheet_columns = function(sheet_name) {
      private$.sheets[[sheet_name]]
    },
    print = function() {
      cat("OuputMapping: ", self$name(), " with ", length(self$sheets()), " sheet(s)\n", sep = "")
      cat("META:\n", sep = "")
      for (c in self$meta_cell_names()) {
        cat(c, " → ", private$.meta_sheet[[c]], "\n", sep = "")
      }
      cat("Sheets:\n", sep = "")
      for (sheet_name in self$sheet_names()) {
        sheet <- self$sheet_columns(sheet_name)
        cat(sheet_name, " - ", length(sheet), " column(s)", "\n", sep = "")
        for (cell in names(sheet)) {
          cat("  ", cell, " → ", sheet[[cell]], "\n", sep = "")
        }
      }
      invisible(self)
    }
  ),
  private = list(
    # sheet name
    .name = NULL,
    # meta sheet
    .meta_sheet = NULL,
    # columns of the sheet
    .sheets = NULL
  )
)


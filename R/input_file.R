library(R6)
library(jsonlite)
library(data.table)
library(openxlsx)

input_file <- R6Class(
  "InputFile",
  public = list(
    initialize = function(mapping, file) {
      stopifnot(!is.null(mapping))
      stopifnot(!is.null(file))
      private$.file <- file
      private$.mapping <- mapping
    },
    mapping = function() {
      private$.mapping
    },
    file = function() {
      private$.file
    },
    data = function() {
      private$.data
    },
    sheet_names = function() {
      names(self$data())
    },
    extract_meta_position = function(meta_name) {
      mapping <- self$mapping()$meta_sheet()
      mapping[[meta_name]]
    },
    extract_meta = function(meta_name, input_file_content) {
      position <- self$extract_meta_position(meta_name)
      split2 <- unlist(strsplit(position, ":"))
      row <- split2[[1]]
      column <- split2[[2]]
      input_file_content[[as.integer(row), as.integer(column)]]
    },
    load = function() {
      mapping <- private$.mapping
      sheet_names <- mapping$sheet_names()
      # The result (list of data.table, one per sheet)
      result <- list()

      # Load xsl meta sheet
      yata_frame <- openxlsx::read.xlsx(xlsxFile = private$.file,
                                        colNames = FALSE,
                                        skipEmptyCols = FALSE,
                                        detectDates = TRUE,
                                        sheet = "META")
      result[["META"]] <- as.data.table(yata_frame)
      # Load xsl frames, one per sheet
      yata_frame <- lapply(sheet_names,
                           openxlsx::read.xlsx,
                           xlsxFile = private$.file,
                           colNames = FALSE,
                           skipEmptyCols = FALSE,
                           detectDates = TRUE)
      sheets_size <- length(sheet_names)
      for (i in seq(1:sheets_size)) {
        sheet_name <- sheet_names[[i]]
        sheet_columns <- mapping$sheet_columns(sheet_name)
        sheet_content <- as.data.table(yata_frame[i])
        column_names <- unlist(sheet_columns)
        if (nrow(sheet_content) < 6) {
          # No data in this sheet
          sheet_content <- data.table(data.frame(
            matrix(nrow = 0, ncol = length(column_names))))
        } else {
          sheet_content <- sheet_content[6:nrow(sheet_content)]
        }
        names(sheet_content) <- column_names
        result[[sheet_name]] <- sheet_content
      }
      private$.data <- result
    }
  ),
  private = list(
    # mapping
    .mapping = NULL,
    # input file
    .file = NULL,
    # loaded data
    .data = NULL
  )
)


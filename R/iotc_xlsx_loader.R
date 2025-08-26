# importing the required library
library(openxlsx)
library(data.table)

#' Generic method to load xsl \code{file}, using the given \code{import_model} xls model.
#' @param file The file to import
#' @param import_model The xls column names model per sheet
#' @return the loaded xls
#' @export
load_xls <- function(file, import_model) {
  sheet_names <- import_model$sheet_names()
  sheet_models <- import_model$sheets()
  # Load xsl frames, one per sheet
  yata_frame <- lapply(sheet_names,
                       openxlsx::read.xlsx,
                       xlsxFile = file,
                       colNames = FALSE,
                       skipEmptyCols = FALSE,
                       detectDates = TRUE)
  # The result (list of data.table, one per sheet, if sheet is empty, then it won't be available in result)
  result <- list()
  sheets_size <- length(sheet_models)
  for (i in seq(1:sheets_size)) {
    sheet_model <- sheet_models[[i]]
    sheet_name <- sheet_model$name()
    sheet_content <- as.data.table(yata_frame[i])
    if (nrow(sheet_content) < 6) {
      # No data in this sheet
      sheet_content <- NULL
    } else {
      sheet_content <- sheet_content[6:nrow(sheet_content)]
      column_names <- unlist(sheet_model$column_names())
      names(sheet_content) <- column_names
    }
    result[[sheet_name]] <- sheet_content
  }
  result
}
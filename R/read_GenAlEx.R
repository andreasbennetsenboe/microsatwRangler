#' Import from GenAlEx
#'
#' Reads the subset of the fields in a GenAlEx-formatted .xlsx-file that are relevant for the package.
#'
#' @param path a character string specifying the path to the file that should be imported.
#'
#' @return a tibble
#' @export

read_GenAlEx <- function(path){
  readxl::read_excel(
    path,
    range = readxl::cell_limits(
      c(3,1),
      c(NA, NA)
      )
    )
}

#' Write pedigree file
#'
#' Create a .txt file that meets the input file requirements of MicroMerger.
#'
#' @param x a tibble formatted by msatwRangler::prepdata().
#' @param pedigree_filename the desired filename of the output file
#'
#' @return a .txt file placed in the directory.
#' @export
write_pedigree_file <- function(x, pedigree_filename) {
  utils::write.table(x,
              file = pedigree_filename,
              sep = ",",
              row.names = FALSE,
              col.names = FALSE,
              na = "",
              quote = FALSE)

  file_lines <- readLines(pedigree_filename)

  writeLines(gsub(",+$", "", file_lines),
             pedigree_filename)
}

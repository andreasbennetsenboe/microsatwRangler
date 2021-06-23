#' Reformat data to prepare it for writing functions
#'
#' What the title said
#'
#' @param x a tibble imported by read_GenAlEx().
#' @param lociprefix a character string specifying the first letters shared by all loci in the data used to identify locus columns.
#'
#' @return a tibble
#' @export
#'

prepdata <- function(x, lociprefix) {
  x %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with(lociprefix),
        ~ dplyr::case_when(
          . > 0 ~ paste0(.,
                         "-",
                         get(colnames(data)[which(colnames(data) == dplyr::cur_column()) + 1])),
          . == 0 ~ NA_character_
          )
        )
    ) %>%
    dplyr::select(!dplyr::starts_with("...")) %>%
    dplyr::mutate(
      Pop = as.integer(factor(Pop)),
      ID = as.integer(factor(IndID)),
      Gender = NA,
      Father = NA,
      Mother = NA,
      Twin = NA) %>%
    dplyr::select(
      Pop,
      ID,
      Mother,
      Father,
      Gender,
      Twin,
      dplyr::starts_with(lociprefix))
}







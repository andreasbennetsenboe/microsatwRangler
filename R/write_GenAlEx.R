#' Write a GenAlEx formated .csv file
#'
#' Create a GenAlEx formated .csv file in the directory from a dataset imported from Geneious.
#'
#' @param x A tibble with microsatellite data.
#' @param path The desired path for the output file.
#'
#' @return a .csv file placed in the directory
write_GenAlEx <- function(x, path) {
  write.table(
    data.frame(
      rbind(
        as.character(
          c(
            (x %>%
               select(starts_with("Mac")) %>%
               ncol())/2,
            x %>%
              nrow(),
            x %>%
              select(Pop) %>%
              n_distinct(),
            (x %>%
               select(Pop) %>%
               count(Pop))$n
          )
        ),
        c(
          "Allpops",
          "NA",
          "NA",
          unlist(
            x %>%
              select(Pop) %>%
              unique()
          )
        )
      )) %>%
      mutate(across(c(V2,V3), as.numeric)),
    file = path,
    sep = ",",
    row.names = FALSE,
    col.names = FALSE,
    quote = FALSE,
    na = " ")

  write.table(data.frame(t(str_replace(ifelse(str_detect(c(colnames(x)), "...2$"), NA, c(colnames(x))), "...1$", ""))),
              file = path,
              sep = ",",
              append=TRUE,
              row.names = FALSE,
              col.names = FALSE,
              quote = FALSE,
              na = " ")

  write.table(x,
              file=path,
              sep = ",",
              append=TRUE,
              row.names=FALSE,
              col.names = FALSE,
              quote = FALSE,
              na = "")
}

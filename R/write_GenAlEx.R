#' Write a GenAlEx formated .csv file
#'
#' Create a GenAlEx formated .csv file in the directory from a dataset imported from Geneious.
#'
#' @param path The desired path for the output file.
#'
#' @return a .csv file placed in the directory
write_GenAlEx <- function(path) {
  write.table(
    data.frame(
      rbind(
        as.character(
          c(
            (data %>%
               select(starts_with("Mac")) %>%
               ncol())/2,
            data %>%
              nrow(),
            data %>%
              select(Pop) %>%
              n_distinct(),
            (data %>%
               select(Pop) %>%
               count(Pop))$n
          )
        ),
        c(
          "Allpops",
          "NA",
          "NA",
          unlist(
            data %>%
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

  write.table(data.frame(t(str_replace(ifelse(str_detect(c(colnames(data)), "...2$"), NA, c(colnames(data))), "...1$", ""))),
              file = path,
              sep = ",",
              append=TRUE,
              row.names = FALSE,
              col.names = FALSE,
              quote = FALSE,
              na = " ")

  write.table(data,
              file=path,
              sep = ",",
              append=TRUE,
              row.names=FALSE,
              col.names = FALSE,
              quote = FALSE,
              na = "")
}

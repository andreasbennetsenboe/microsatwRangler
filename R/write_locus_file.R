#' Write locus file
#'
#'Create a .txt file that meets the input requirements for the locus file used in MicroMerger.
#'
#' @param x A tibble imported by msatwRangler::read_GenAlEx().
#' @param lociprefix  A character string specifying the first letters shared by all loci in the data used to identify locus columns.
#' @param locus_filename	The desired filename of the output file.
#'
#' @return A .txt file placed in the directory
#' @export
write_locus_file <- function(x, lociprefix, locus_filename) {

  locusnames <-
    unique(
      colnames(
        x %>%
          dplyr::select(
            dplyr::starts_with(lociprefix)
          )
      )
    )

  loci_data <-
    x %>%
    dplyr::select(
      dplyr::starts_with(lociprefix)
    )

  subset_list <- list()

  for (i in 1:length(locusnames)) {
    subset_list[[i]] <- loci_data %>%
      dplyr::select(i)
  }

  loci_list <- list()

  for (i in 1:length(subset_list)) {
    loci_list[[i]] <- list(
      a = c(
        paste(
          gsub('.{1}$',
               "",
               colnames(subset_list[[i]])
               )
          ),
      "AUTOSOME",
      dplyr::n_distinct(
        subset_list[[i]] %>%
          dplyr::select(dplyr::starts_with("Ma")) %>%
          tidyr::pivot_longer(cols = dplyr::starts_with("Ma")) %>%
          dplyr::select(value) %>%
          dplyr::filter(value != 0)
      )),
      b = subset_list[[i]] %>%
        dplyr::select(dplyr::starts_with("Ma")) %>%
        tidyr::pivot_longer(cols = dplyr::starts_with("Ma")) %>%
        dplyr::select(value) %>%
        dplyr::filter(value != 0) %>%
        dplyr::count(value) %>%
        dplyr::mutate(Freq = n/sum(n)) %>%
        dplyr::pull(value),
      c = subset_list[[i]] %>%
        dplyr::select(dplyr::starts_with("Ma")) %>%
        tidyr::pivot_longer(cols = dplyr::starts_with("Ma")) %>%
        dplyr::select(value) %>%
        dplyr::filter(value != 0) %>%
        dplyr::count(value) %>%
        dplyr::mutate(Freq = n/sum(n)) %>%
        dplyr::pull(Freq)
    )
  }

  for(i in 1:length(loci_list)){
    utils::write.table(t(data.frame(loci_list[[i]][1])),
                file=locus_filename,
                sep = ",",
                append=TRUE,
                row.names=FALSE,
                col.names = FALSE,
                quote = FALSE)
    utils::write.table(data.frame(loci_list[[i]][2:3]),
                file=locus_filename,
                sep = ",",
                append=TRUE,
                row.names=FALSE,
                col.names = FALSE,
                quote = FALSE)
  }
}

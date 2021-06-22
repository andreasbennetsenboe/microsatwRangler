library("tidyverse")
library("readxl")

####---- Define functions ----####

read_GenAlEx <- function(path){
  read_excel(path, range = cell_limits(c(3,1),c(NA, NA)))
}

prepdata <- function(x, lociprefix) {
  x %>%
    mutate(across(starts_with(lociprefix),
                  ~ case_when(. > 0 ~ paste0(.,
                                             "-",
                                             get(colnames(data)[which(colnames(data) == cur_column()) + 1])),
                              . == 0 ~ NA_character_
                  )
    )
    ) %>%
    select(!starts_with("...")) %>%
    mutate(Pop = as.integer(factor(Pop)),
           ID = as.integer(factor(IndID)),
           Gender = NA,
           Father = NA,
           Mother = NA,
           Twin = NA) %>%
    select(Pop, ID, Mother, Father, Gender, Twin, starts_with(lociprefix))
}

write_pedigree_file <- function(x, pedigree_filename) {
  write.table(x, 
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

write_locus_file <- function(x, lociprefix, locus_filename) {
  
  locusnames <- 
    unique(
      colnames(
        data %>% 
          select(
            starts_with(lociprefix)
          )
      )
    )
  
  loci_data <- 
    data %>% 
    select(
      starts_with(lociprefix)
    )
  
  subset_list <- list()
  
  for (i in 1:length(locusnames)) {
    subset_list[[i]] <- loci_data %>%
      select(i)
  }
  
  loci_list <- list()
  
  for (i in 1:length(subset_list)) {
    loci_list[[i]] <- list(
      a = c(paste(
        gsub('.{1}$',
             "",
             colnames(subset_list[[i]])
             )
      ),
      "AUTOSOME",
      n_distinct(
        subset_list[[i]] %>%
          select(starts_with("Ma")) %>%
          pivot_longer(cols = starts_with("Ma")) %>%
          select(value) %>%
          filter(value != 0)
      )),
      b = subset_list[[i]] %>% 
        select(starts_with("Ma")) %>%
        pivot_longer(cols = starts_with("Ma")) %>%
        select(value) %>%
        filter(value != 0) %>%
        count(value) %>%
        mutate(Freq = n/sum(n)) %>%
        pull(value),
      c = subset_list[[i]] %>% 
        select(starts_with("Ma")) %>%
        pivot_longer(cols = starts_with("Ma")) %>%
        select(value) %>%
        filter(value != 0) %>%
        count(value) %>%
        mutate(Freq = n/sum(n)) %>%
        pull(Freq)
    )
  }
  
  for(i in 1:length(loci_list)){
    write.table(t(data.frame(loci_list[[i]][1])),
                file=locus_filename, 
                sep = ",", 
                append=TRUE, 
                row.names=FALSE, 
                col.names = FALSE, 
                quote = FALSE)
    write.table(data.frame(loci_list[[i]][2:3]),
                file=locus_filename, 
                sep = ",", 
                append=TRUE, 
                row.names=FALSE, 
                col.names = FALSE, 
                quote = FALSE)
  }
}
library(devtools)
library("tidyverse")
library("fs")
getwd()
#####---- Create functions ----####
use_r("read_GenAlEx")

use_r("prepdata")

use_r("write_pedigree_file")

use_r("write_locus_file")

use_r("write_GenAlEx")

?prepdata
?read_GenAlEx
?write_pedigree_file
?write_locus_file

#####---- Build ----####

document()

load_all()

check()

install()

library(msatwRangler)

#####---- Test ----####

data <- read_GenAlEx(path = "hidden/genalextest.xlsx")

prepdata <- prepdata(data, lociprefix = "Ma")

write_pedigree_file(prepdata, pedigree_filename = "pedtest.txt")

write_locus_file(data, lociprefix = "Ma", locus_filename = "loctest.txt")


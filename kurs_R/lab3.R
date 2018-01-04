
require(stringi)
require(dplyr)

setwd("~/Downloads/ml-1m")
getwd()

movies_text <- stri_read_lines("movies.dat")
movies_text <- stri_replace_all_fixed(movies_text, "::", ";")
stri_write_lines(movies_text, "movies_1.dat")

movies <- read.csv("movies_1.dat", header=FALSE)
head(movies)

colnames(movies) <- c("Movie ID", "Title", "Category")
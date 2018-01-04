install.packages("stringi")
require(stringi)

nchar(c("Ala ma kota", NA))

# dlugosc napisu
x <- c("Ala ma kota", "Bardzo lubie programowac w R")
length(x)
stri_length(x)

stri_cmp("Ala", "Basia")
stri_cmp(c("Ala", "Basia", "Celina"), "Bartek")

stri_cmp(c("a", "b", "c", "A"), c("B"))

?stri_cmp

stri_sort(c(letters, LETTERS))

slowa <- c("hladny", "chladny", "cela", "dom", "hamak")
stri_sort(slowa, locale="pl_PL")

stri_trans_toupper("i", locale="pl_PL")
stri_trans_toupper("i", locale="tr_TR")

stri_paste()
stri_join(1:10, letters, "@", c("abc", "cde"), ".pl", sep="_")

stri_join(1:10, letters, "@", c("abc", "cde"), ".pl", collapse="###")

imiona <- c("Ala", "Bartek", "Darek", "Marek")

stri_paste("Witaj, ", imiona, " mamy super ofere")

stri_dup("abc", 3)

stri_paste(rep("abc",3), collapse="")

# Wyznaczanie / podmiana napisow
stri_sub(x)
?stri_sub
s <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
stri_sub(s, from=1:3*6, to=21)
stri_sub(s, from=c(1,7,13), length=5)
stri_sub(s, from=1, length=1:3)
stri_sub(s, -17, -7)
stri_sub(s, -5, length=4)
(stri_sub(s, 1, 5) <- "stringi")
(stri_sub(s, -6, length=5) <- ".")
(stri_sub(s, 1, 1:3) <- 1:2)

x <- c("Ala ma kota", "Bardzo lubie programowac w R", "Dzis mamy sloneczna pogode")
stri_detect_fixed(x, "a")
stri_detect_fixed(x, "ma")

# Ignorujemy wielkosc znakow
stri_detect_fixed(x, "R")
stri_detect_fixed(x, "R", case_insensitive = TRUE)

# Zliczane
stri_count_fixed(x, "ma") > 0
# Tylko detect bedzie szybszy (nie wolniejszy)
stri_detect_fixed(x, "ma")

# Pozwalamy aby wzorce na siebie nachodzily
y <- c("aBaBa", "aBBa")
stri_count_fixed(y, "aBa")
stri_count_fixed(y, "aBa", overlap = TRUE)

ile <- stri_count_fixed("Ala ma kota", letters, case_intesitive=TRUE)

names(ile) <- letters

# Lokaliizowanie - pierwsze, ostatnie i wszystkie
x
stri_locate_first_fixed(x, "ma")

gdzie <- stri_locate_all_fixed(x, c("ma", "R", "mam"))
gdzie <- stri_locate_first_fixed(x, c("ma", "R", "mam"))

stri_sub(x, gdzie)
stri_sub(x, gdzie[,1], gdzie[,2])

# Zastepowanie
stri_extract_first_fixed(x, c("ma", "R", "mam"), 1:3)

stri_replace_all_fixed(x, c("ma", "R", "mam"), 1:3, case_insensitive=TRUE)
stri_replace_all_fixed("abc", letters, LETTERS, vectorize_all = FALSE)

stri_replace_all_fixed("abcxyz123@", letters, LETTERS, vectorize_all = FALSE)
stri_replace_all_fixed("abcxyz123@", letters, LETTERS, vectorize_all = TRUE)

# Dzielenie
stri_split_fixed(x, " ")
stri_split_fixed("To jest krowa, a to kaczka!", " ")



# Zmienianie wielkosci
stri_trans_tolower("Ala MA Kota")
stri_trans_toupper("Ala MA Kota")
stri_trans_totitle("Ala MA Kota")


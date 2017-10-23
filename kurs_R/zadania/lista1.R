# ---- Zadanie 1 ----

# 1.1 a)
s <- sample(-10:10)
sum(as.integer(s > 0))
length(which(s>0))

# 1.1 b)
s<-rnorm(100,0.3)
indices <- round(s) %%3 == 0
ss1 <- sum(s[indices])
ss2 <- sum(round(s[indices]))

# 1.1 c)
ediff <- s-8

# 1.1 d) TODO
s<- 3:25
mini <- which(s %in% min(s))
maxi <- which(s %in% max(s))

# 1.1 e)
inds <- s > 5 | s < 2
s[inds]
sqaures <- s[inds] * s[inds]
sum(sqaures) / length(sqaures)

# 1.1 f) FIXME
k <- s
k[s %% 2] <- "Parzyste"
k[!(s %% 2)] <- "Nieparzyste"


# 1.1 g)
## see 1.1f

# 1.1 h)


x = 1:25
c("parzysta", "nieparzysta")[x %% 2 +1]
y <- character(length(x))
y[x %% 2==0] = "parzysta"
y[x %% 2==1] = "nieparzysta"



# zad 1.10
n <- 10^3
x <- runif(n, -1, 1)
y <- runif(n, -1, 1)

inside <- x^2+y^2 <= 1
sum(inside)/n
mean(inside)*4



# zad 1.4
x <- rnorm(100)
y <- 3 * x + rnorm(100)
cor(x,y)

# a)
diff <- function(x) { x - mean(x) }
licznik <- sum(diff(x)*diff(y))
mianownik <- sqrt(sum(diff(x)^2)*sum(diff(y)^2))

licznik/mianownik

# b)
licznik = sum(x*y)- length(x)*mean(x)*mean(y)
mianownik = 1
licznik/mianownik
# c)

# d)



# Srednia ruchoma
# popraw
sredniaRuchoma <- function(x,k) {
  stopifnot(is.numeric(x))
  n <- length(x)
  stopifnot(n > 0)
  stopifnot(k > 0 & k <= n)
  wynik <- numeric(n-k +1)
  
  for(i in seq_along(wynik)) {
    wynik[i] <- mean(x[(1:k)+i-1])
  }
  wynik
}

srediaRuchoma(1:10,3)
sredniaRuchoma(sample(1:10),3)

# Srednia ruchoma
# popraw
sredniaRuchoma2 <- function(x,k) {
  stopifnot(is.numeric(x))
  n <- length(x)
  stopifnot(n > 0)
  stopifnot(k > 0 & k <= n)
  
  wynik <- numeric(n-k +1)
  wynik[1] <- sum(x[1:k])
  
  for(i in 2:(n-k+1)) {
    wynik[i] <- wynik[i-1] - x[i-1] + x[k+i-1]
  }
  wynik/k
}

srediaRuchoma2(1:10,3)


x <- rnorm(1000)
require(microbenchmark)
microbenchmark(sredniaRuchoma(x,100), sredniaRuchoma2(x,100))


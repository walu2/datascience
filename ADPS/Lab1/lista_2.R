wektory <- list(1:5, -2:15, c(1,1,1,1,1))
#sum(wektory)
lapply(wektory, sum)
lapply

sapply
sapply(wektory, sum)

#mapply(wektory, 1:13, letters, c("-", "=")
 
mapply(rep, 1:4, 4:1)

rnorm(20)
runif(20)

hist(runif(1000))
hist(rnorm(1000))

## ---- funkcje ----
suma <- function(x) {
  return(sum(x))
}
(function(x) x^3)(1:5)
suma(1:20)

p <- 555

f1 <- function(x) {
  p<<- 22
  y <- x + 2
  cat("x = ", x, "y= ",y)
}
f1(5)

cat(p)

x<- 333

y <<- x
y <- 22

x <- sample(30)
x
seq_along(x)
head(x)
microbenchmark(seq_along(sample(30)), seq(1:30))


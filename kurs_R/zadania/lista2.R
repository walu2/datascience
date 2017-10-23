## --- Zadania lista nr 2 ---
sign_own <- function(arg) 
{
  res <- numeric(length(arg))
  res[arg < 0] <- -1
  res[arg > 0] <- 1
  res[arg == 0] <- 0
  
  res
}

oabs <- function(arg) {
  res <- arg
  res[arg < 0] <- -1*(arg[arg <0])
  
  return(res)
}

oall <- function(...) {
  as.logical(...)
}

logical(length=10)
#sign_own(-10:25)
oall(1:10)

oabs(-10:25)


x <- function(o) {
  p <- c(1,2,3)
  s <- list(p, o)
  return(s)
}


x(1:14)

# 2.3
return <- list <- c <- function(...) {
  cat("dummy")
}











# 2.5
cumsum(1:10)
cumsum_ <- function(args) {
  l <- length(args)
  ifelse(length(args) == 1, args, cumsum_(head(args,l-1))+ tail(args,1))
}

cumsum_(1:100)


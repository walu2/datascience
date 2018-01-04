# Laboratorium


n = 25; mi = 20; sigma = 1
y1 = rnorm(n, mi, sigma)
y2 = rnorm(n, mi, sigma)
y3 = rnorm(n, mi, sigma)
y4 = rnorm(n, mi, sigma)
y5 = rnorm(n, mi, sigma)

y = cbind.data.frame(y1, y2, y3, y4, y5)
boxplot(y)

dane_anova = stack(y)
names(dane_anova) <- c('dane', 'proba')

bartlett.test(dane~proba, data = dane_anova)

aov_res = aov(dane~proba, data = dane_anova)
summary(aov_res)

kruskal.test(dane~proba, dane_anova)

Tukey_res = TukeyHSD(aov_res)
print(Tukey_res)

plot(Tukey_res)


# DODAMY COS DO STALYCH
n = 25; mi = 20; sigma = 1; al_2 = 1 # lub 2 lub 4
y1 = rnorm(n, mi, sigma)
y2 = rnorm(n, mi, sigma) + al_2
y3 = rnorm(n, mi, sigma)
y4 = rnorm(n, mi, sigma)
y5 = rnorm(n, mi, sigma)

n = 25; mi = 20; sigma = 1; al_4 = -2
y1 = rnorm(n, mi, sigma)
y2 = rnorm(n+1, mi, sigma)
y3 = rnorm(n+2, mi, sigma)
y4 = rnorm(n+3, mi, sigma) + al_4
y5 = rnorm(n+4, mi, sigma)
dane_anova = data.frame( dane = c(y1, y2, y3, y4, y5),
                         proba = rep( c("y1", "y2", "y3", "y4", "y5"), 
                                      times = c(length(y1), length(y2), length(y3), length(y4), length(y5))) )



# REGRESJA 
n = 100; mi = 0; sigma = 2
x = rnorm(n, mi, sigma)
e = rnorm(n, 0, 1)
b0 = 1; b1 = 2
y = b1*x + b0 + e
plot(x, y)


lm_res = lm(y~x)
summary(lm_res)

arg = c(min(x), max(x))
out = coef(lm_res)[2]*arg + coef(lm_res)[1]
plot(x, y)
lines(arg, out, col = 'green')


# REGRESJA WIELOWYMIAROWA
n = 100
x0 = rep(1, n)
x1 = (1:n)/n
x2 = sin(1:n)
x3 = runif(n, -1, 1)
x = cbind(x0, x1, x2, x3)
b = c(1, -2, 3, -1)
e = rnorm(n, 0, 0.5)
y = x%*%b + e

plot(y, type = 'l')
lm_res = lm(y ~ 1 + x1 + x2 + x3)
summary(lm_res)

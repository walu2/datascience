# Test wartosci sredniej (z wariancja)
n = 30; mi = 1; sigma = 2
x = rnorm(n, mean = mi, sd = sigma)

mi_0 = 1; alfa = 0.05
Z = abs(mean(x) - mi_0)*sqrt(n)/sigma
c = qnorm(1-alfa/2)
p_val = 2*(1 - pnorm(Z))

library(TeachingDemos)
z.test(x, mu = mi_0, sigma, alternative = "two.sided")

## Bez wariancji

T = abs(mean(x) - mi_0)*sqrt(n)/sd(x)
c = qt(1-alfa/2,df = n-1)
p_val = 2*(1 - pt(T, df = n-1))

mi_0 = 3
t.test(x, mu = mi_0, alternative = "two.sided")


## Test zgodnosci Pearsona
n = 50; x = sample(1:6, n, replace = T)
ni_i = as.data.frame(table(factor(x, levels = 1:6)))$Freq
p_i = rep(1/6, 6)
T = sum((ni_i - n*p_i)^2 / (n*p_i))
r = 6
alfa = 0.05
c = qchisq(1 - alfa, r - 1)
p_val = (1 - pchisq(T, r - 1))

chisq.test(ni_i, p = p_i)


# Test Kolomogorowa = Smirnowa
n = 50; a = 2; b = 4
x = runif(n, min = a, max = b)
hist(x)
ks.test(x, 'punif', min = a, max = b)
ks.test(x, 'punif', a - 0.5, b - 0.5)
ks.test(x, 'punif', a - 0.5, b + 0.5)




# Hipoteza ze pochodza z rozkladu normalenego
ks.test(x, 'pnorm', mean = (a + b)/2, sd = sqrt((b - a)^2/12))

x = runif(500, min = a, max = b)
ks.test(x, 'pnorm', mean = (a + b)/2, sd = sqrt((b - a)^2/12))


n = 100; mi = 1; sigma = 2
x = rnorm(n, mean = mi, sd = sigma)
ks.test(x, 'punif', min = mi-sigma, max = mi+sigma)


# Test normalnosci
n = 100; mi = 1; sigma = 2
x = rnorm(n, mean = mi, sd = sigma)
ks.test(x, 'pnorm', mean = 1, sd = 2)
ks.test(x, 'pnorm', mean = 0, sd = 3)
shapiro.test(x)


# Test hipotezy dot rozkladu normalego
n = 100; a = 2; b = 4
x = runif(n, min = a, max = b)
shapiro.test(x)
ks.test(x, 'pnorm', mean = (a + b)/2, sd = sqrt((b - a)^2/12))



## Porownywanie srednich!
n_1 = 9; mi_1 = 0; sigma_1 = 1
n_2 = 12; mi_2 = 1; sigma_2 = 1
x_1 = rnorm(n_1, mean = mi_1, sd = sigma_1)
x_2 = rnorm(n_2, mean = mi_2, sd = sigma_2)



mean_1 = mean(x_1); mean_2 = mean(x_2)
s2_1 = var(x_1); s2_2 = var(x_2)
s2 = ((n_1 - 1)*s2_1 + (n_2 - 1)*s2_2)/(n_1 + n_2 - 2)
alfa = 0.05; c = qt(1 - alfa/2, n_1 + n_2 - 2)
T = abs(mean_1 - mean_2) / ( sqrt(s2*( n_1^(-1) + n_2^(-1) ) ) )
p_value = 2*(1 - pt(T, n_1 + n_2 - 2))

t.test(x_1, x_2, var.equal = T)


# Test bez zakladania rownosci wariancji

s2 = s2_1/n_1 + s2_2/n_2
d = (s2^2)/(((s2_1/n_1)^2)/(n_1-1) + ((s2_2/n_2)^2)/((n_2-1)))
c = qt(1 - alfa/2, d)
T = abs(mean_1 - mean_2)/sqrt(s2)
p_value = 2*(1 - pt(T, d))

t.test(x_1, x_2)


# Test Wilcoxa
wilcox.test(x_1, x_2)

plot(dwilcox(0:100, length(x_1), length(x_2)), type = 'l'); 
grid()




# Test niezaleznosci
x_1 = c(16, 25, 11)
x_2 = c(13, 32, 15)
x_3 = c(31, 43, 26)
xx = cbind(x_1, x_2, x_3)
I = 3
J = 3
n_i = x_1 + x_2 + x_3
n_j = c(sum(x_1), sum(x_2), sum(x_3))
N = sum(n_j)

T = 0
for (i in 1:I) {
  for (j in 1:J) {
    T = T + (N*xx[i,j] - n_i[i]*n_j[j])^2/(N*n_i[i]*n_j[j])
  }
}
alfa = 0.05
c = qchisq(1 - alfa, df = (I - 1)*(J - 1))
p_val = 1 - pchisq(T, df = (I - 1)*(J - 1))


chisq.test(xx)

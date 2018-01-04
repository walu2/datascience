library(MASS)

setwd("ADPS/Lab2")
getwd()

# -- A 
skrety = scan("skrety.txt")

# --- B
est <- fitdistr(skrety, "poisson")

print(est) # =>  lambda 3.8000000 (0.1125463)

l_mean = mean(skrety) # 3.8
l_var = var(skrety) 

# --- C

hist(skrety, freq=F)
lines(0:15, dpois(0:15, l_mean),type='l')

# -- D
K = 1000
n = 300
boot_res = replicate(K, {
  boot_dane = sample(skrety, n, replace = T)
  c(mean(boot_dane), var(boot_dane))
})
sd_mean = sd(boot_res[1,])
sd_var = sd(boot_res[2,])



## --- zadanie 2
df_LOTOS = read.csv("LOTOS.mst")
names(df_LOTOS) = c('ticker', 'date','open', 'high','low','close','vol')
df_LOTOS$date = as.Date.character(df_LOTOS$date, format = '%Y%m%d')
plot(open ~ date, df_LOTOS, type='l', col='blue', xlab='Data', ylab='Kurs otwarcia PLN', main='LOTOS')
df_LOTOS$open_ch= with(df_LOTOS, c(NA, 100*diff(open)/open[1:length(open)-1]))

#plot(open_ch ~ date, df_LOTOS, type='l', col='blue', xlab='Data', ylab='Procentowa zmiana kursu otwarcia [%]', main='LOTOS')

x2_mean = mean(df_LOTOS$open_ch, na.rm = T)
x2_var = var(df_LOTOS$open_ch, na.rm = T)
x2_sd = sd(df_LOTOS$open_ch, na.rm = T)

hist(df_LOTOS$open_ch, freq=F) # => Mozna
lines(-15:15, dnorm(-15:15, mean=x2_mean, sd=x2_sd),type='l')

# --- C
lev = 0.95# 0,99

w = x2_sd*qt((1+lev)/2, n-1)/sqrt(n)
ci_mean = c(x2_mean - w, x2_mean + w)
a = (1 - lev)/2
b = (1 - lev)/2
ci_var = c((n-1)*x2_sd^2/qchisq(1-b,n-1), (n-1)*x2_sd^2/qchisq(a,n-1))

## -- zadanie 4

x_g = scan("fotony.txt")

# metoda momentow
m1 = mean(x_g)
m2 = mean(x_g^2)
alpha_mom = m1^2/(m2 - m1^2)
beta_mom = (m2 - m1^2)/m1

# metoda najw wiar
est_nw = fitdistr(x_g, 'gamma', list(shape=1, scale=1), lower=0)

alpha_nw = as.numeric(est_nw$estimate[1])
beta_nw = as.numeric(est_nw$estimate[2])

# wykres

hist(fotony, freq=F)
lines(0:600, dgamma(0:600, shape=alpha_nw, scale=beta_nw))

# bootstrap
K = 100
boot_res = replicate(K, {
  boot_dane = rgamma(3935, shape = alpha_nw, scale = beta_nw)
  e <- fitdistr(boot_dane, 'gamma',list(shape=1, scale=1), lower=0)
  c(as.numeric(e$estimate[1]),as.numeric(e$estimate[2]))
  } )


sd_mean = sd(boot_res[1,])
sd_var = sd(boot_res[2,])

alpha = 0.95

quantile(boot_res[1,], probs=c(0.025,0.975))
quantile(boot_res[2,], probs=c(0.025,0.975))



### Laboratorium 3

# 1 test normalnosci (sharpiro ks)
# Test dot Warosci sredniej

# 2
# Czy dane naleza do rozkladu jednostanejgo
# Liczba dni, dzielona przez liczbe dni w roku 31/356, 28/365 
samobojstwa = c(1867, 1789, 1944, 2094, 2097, 1981, 1887, 2024, 1928, 2032, 1978, 1859)
dni = c(31,28,31,30,31,30,31,31, 30,31,30,31)
pp = dni/365
chisq.test(samobojstwa,p=pp)

# 3 
# Dane gieldowe
# Procentowe zmiany zamkniecia
# Czy dane maja rozklad normalny
getwd()
setwd("Lab1")

df_LOTOS = read.csv("LOTOS.mst")
df_PEKAO = read.csv("PEKAO.mst")

names(df_LOTOS) = c('ticker', 'date','open', 'high','low','close','vol')
names(df_PEKAO) = c('ticker', 'date','open', 'high','low','close','vol')
df_LOTOS$date = as.Date.character(df_LOTOS$date, format = '%Y%m%d')
df_PEKAO$date = as.Date.character(df_PEKAO$date, format = '%Y%m%d')

#plot(close ~ date, df_LOTOS, type='l', col='blue', xlab='Data', ylab='Kurs zamkniecia PLN', main='LOTOS')
#plot(close ~ date, df_PEKAO, type='l', col='blue', xlab='Data', ylab='Kurs zamkniecia PLN', main='PEKAO')

df_LOTOS$close_ch = with(df_LOTOS, c(NA, 100*diff(close)/close[1:length(close)-1]))
plot(close_ch ~ date, df_LOTOS, type='l', col='blue', xlab='Data', ylab='Procentowa zmiana kursu zamkniecia [%]', main='LOTOS')

df_PEKAO$close_ch = with(df_PEKAO, c(NA, 100*diff(close)/close[1:length(close)-1]))
plot(close_ch ~ date, df_PEKAO, type='l', col='blue', xlab='Data', ylab='Procentowa zmiana kursu zamkniecia [%]', main='PEKAO')

# b
hist(df_PEKAO$close_ch, breaks=50, prob=T, xlab='Zmiana kursu otwarcia [%]', ylab='Czestosc wystepowania', main = 'Histogram procentowaych zmian kursu PEAKO')
hist(df_LOTOS$close_ch, breaks=50, prob=T, xlab='Zmiana kursu otwarcia [%]', ylab='Czestosc wystepowania', main = 'Histogram procentowaych zmian kursu LOTOS')

shapiro.test(df_PEKAO$close_ch)
shapiro.test(df_LOTOS$close_ch)

# 4
# Wzory
getwd()
setwd("~/ADPS")
los1 <- read.table("los1.txt", header=F)
los2 <- read.table("los2.txt", header=F)

los1 <- as.matrix(los1)
los2 <- as.matrix(los2)

diff <- mean(los1) - mean(los2)
# Wyklad 4 slide 55
data <- as.matrix(rbind(los1,los2))
n_kw <- sd(data)
sq_los1 <- sum((los1-mean(los1))^2)
sq_los2 <- sum((los2-mean(los2))^2)
s_kw <- sqrt((sq_los1 + sq_los2)/ (length(los1)+length(los2)-2))

t.test(los1, los2, var.equal = T)

# 5 
# Test braku roznicy - test z roznica wartosci srednich.
#Zakladamy, ze sa to dane z rozkladu normalengo
lozyska <- read.csv("lozyska.txt", header=T)
t1 <- lozyska[,1]
t2 <- lozyska[,2]

t.test(t1, t2)
wilcox.test(t1, t2)

# 6
# Test niezaleznosci
# Macierz 4 na 3

# Test niezaleznosci
x_1 = c(62, 39, 32) # Ekstraklasa
x_2 = c(58, 31, 38) # Premier League
x_3 = c(57, 27, 41) # primiera Division
x_4 = c(55, 33, 29)   # Bundesliga

xx = cbind(x_1, x_2, x_3, x_4)
I = 3
J = 4
n_i = x_1 + x_2 + x_3 + x_4
n_j = c(sum(x_1), sum(x_2), sum(x_3), sum(x_4))
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


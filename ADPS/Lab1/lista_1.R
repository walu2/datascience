# --- Zadanie nr 1 ----
getwd() 
#unzip("mstall.zip", "LOTOS.mst")
#unzip("mstall.zip", "PEKAO.mst")

df_LOTOS = read.csv("LOTOS.mst")
df_PEKAO = read.csv("PEKAO.mst")

names(df_LOTOS) = c('ticker', 'date','open', 'high','low','close','vol')
names(df_PEKAO) = c('ticker', 'date','open', 'high','low','close','vol')
df_LOTOS$date = as.Date.character(df_LOTOS$date, format = '%Y%m%d')
df_PEKAO$date = as.Date.character(df_PEKAO$date, format = '%Y%m%d')

plot(close ~ date, df_LOTOS, type='l', col='blue', xlab='Data', ylab='Kurs zamkniecia PLN', main='LOTOS')
plot(close ~ date, df_PEKAO, type='l', col='blue', xlab='Data', ylab='Kurs zamkniecia PLN', main='PEKAO')

df_LOTOS$close_ch = with(df_LOTOS, c(NA, 100*diff(close)/close[1:length(close)-1]))
plot(close_ch ~ date, df_LOTOS, type='l', col='blue', xlab='Data', ylab='Procentowa zmiana kursu zamkniecia [%]', main='LOTOS')

df_PEKAO$close_ch = with(df_PEKAO, c(NA, 100*diff(close)/close[1:length(close)-1]))
plot(close_ch ~ date, df_PEKAO, type='l', col='blue', xlab='Data', ylab='Procentowa zmiana kursu zamkniecia [%]', main='PEKAO')

# b
hist(df_PEKAO$close_ch, breaks=50, prob=T, xlab='Zmiana kursu otwarcia [%]', ylab='Czestosc wystepowania', main = 'Histogram procentowaych zmian kursu PEAKO')
hist(df_LOTOS$close_ch, breaks=50, prob=T, xlab='Zmiana kursu otwarcia [%]', ylab='Czestosc wystepowania', main = 'Histogram procentowaych zmian kursu LOTOS')

# c
boxplot(df_PEKAO$close_ch, df_LOTOS$close_ch, col='green', xlab ='zmiany', ylab='Zmiana kursu otwarcia [%]', main='PEKAO i LOTOS')



# ---- zadanie nr 2 ----
setwd("~/ADPS/Lab1")
kat = read.csv('Airplane_Crashes_and_Fatalities_Since_1908.csv')
kat$Month = strftime(as.Date(kat$Date, '%m/%d/%Y'), '%m')
kat$Day = strftime(as.Date(kat$Date, '%m/%d/%Y'), '%d')
kat$Dayweek = strftime(as.Date(kat$Date, '%m/%d/%Y'), '%u')

kat$Year = strftime(as.Date(kat$Date, '%m/%d/%Y'), '%Y')

# a)
plot(table(kat$Month), type = 'h', col = 'blue', xlab = 'Miesiac', ylab = 'Liczba katastrof', main = 'Liczba katastrof w miesiacach')
# b)
plot(table(kat$Day), type = 'h', col = 'blue', xlab = 'Dzien', ylab = 'Liczba katastrof', main = 'Liczba katastrof w dniach')
# c)
plot(table(kat$Dayweek), type = 'h', col = 'blue', xlab = 'Weekday', ylab = 'Liczba katastrof', main = 'Liczba katastrof w dniach tyg')

kat$Saved = kat$Aboard-kat$Fatalities

uratowani_w_latach = aggregate(Saved ~ Year, kat, FUN = sum)
pasazerowie_w_latach = aggregate(Aboard ~ Year, kat, FUN = sum)
zabici_w_latach = aggregate(Fatalities ~ Year, kat, FUN = sum)

# 2a
plot(uratowani_w_latach, type = 'h', col = 'blue', xlab = 'Rok', ylab ='Liczba ofiar', main = 'Liczba uratowanych osob z katastrof, w roku:')
procenty = uratowani_w_latach$Saved*100/pasazerowie_w_latach$Aboard
lata <- unique(kat$Year)
ramka <- data.frame(x = lata, y = procenty)

plot(ramka, type = 'h', col = 'red', xlab = 'Uratowani', ylab ='Dany rok', main = 'Odsetek uratowanych osob z katastrof [%], w roku:')



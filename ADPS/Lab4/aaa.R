getwd() # Double check

# ustaw katalog
setwd("/home/users/pwalkows/ADPS/Lab4")

wczytaj_mst = function(plik_zip, plik_mst) {
  unzip(plik_zip, plik_mst)
  dane = read.csv(plik_mst)
  names(dane) = c('ticker', 'date', 'open', 'high', 'low', 'close', 'vol')
  dane$date = as.Date.character(dane$date,format = '%Y%m%d')
  dane
}

## Zadanie 1

# A (6 m-cy)
df_KGHM = wczytaj_mst('mstall.zip', 'KGHM.mst')


df_KGHM$close_ch = with(df_KGHM, c(NA, 100*diff(close)/close[1:length(close)-1])) # Analogicznie do poprzednich list

y1 <- with(df_KGHM, close_ch[format(date,'%Y-%m')=='2017-06'])
y2 <- with(df_KGHM, close_ch[format(date,'%Y-%m')=='2017-07'])
y3 <- with(df_KGHM, close_ch[format(date,'%Y-%m')=='2017-08'])
y4 <- with(df_KGHM, close_ch[format(date,'%Y-%m')=='2017-09'])
y5 <- with(df_KGHM, close_ch[format(date,'%Y-%m')=='2017-10'])
y6 <- with(df_KGHM, close_ch[format(date,'%Y-%m')=='2017-11'])

dane_anova <- data.frame(dane = c(y1,y2,y3,y4,y5,y6),
                         proba = rep(c("y1","y2","y3","y4","y5","y6"),
                                     times = c(length(y1),length(y2),length(y3),length(y4),length(y5),length(y6))
                                     )
                         )

names(dane_anova) <- c("dane","proba")

bartlett.test(dane~proba, data = dane_anova)  
# P-value > 0.5

aov_res = aov(dane~proba, data = dane_anova)
summary(aov_res)

kruskal.test(dane~proba, data = dane_anova)

Tukey_res = TukeyHSD(aov_res)
print(Tukey_res)
plot(Tukey_res)


# B (3 m-ce)

y4 <- with(df_KGHM, close_ch[format(date,'%Y-%m')=='2017-09'])
y5 <- with(df_KGHM, close_ch[format(date,'%Y-%m')=='2017-10'])
y6 <- with(df_KGHM, close_ch[format(date,'%Y-%m')=='2017-11'])

dane_anova <- data.frame(dane = c(y4,y5,y6),
                         proba = rep(c("y4","y5","y6"),
                                     times = c(length(y4),length(y5),length(y6))
                                     )
                         )


names(dane_anova) <- c("dane","proba")

bartlett.test(dane~proba, data = dane_anova) 
# p > 0,05 => var equal

aov_res = aov(dane~proba, data = dane_anova)
summary(aov_res)

kruskal.test(dane~proba, data = dane_anova)


## Zadanie 2

# WIG20
WIG20 = wczytaj_mst('mstall.zip', 'WIG20.mst')
WIG20 <- with(WIG20, close[format(date,'%Y')=='2017'])

ASSECO = wczytaj_mst('mstall.zip', 'ASSECOPOL.mst')  
ASSECO <- with(ASSECO, close[format(date,'%Y')=='2017'])

ENERGA = wczytaj_mst('mstall.zip', 'ENERGA.mst')
ENERGA <- with(ENERGA, close[format(date,'%Y')=='2017'])

EUROCASH =  wczytaj_mst('mstall.zip', 'EUROCASH.mst')
EUROCASH <- with(EUROCASH, close[format(date,'%Y')=='2017'])

ORANGE = wczytaj_mst('mstall.zip', 'ORANGEPL.mst')
ORANGE <- with(ORANGE, close[format(date,'%Y')=='2017'])

PKNORLEN = wczytaj_mst('mstall.zip', 'PKNORLEN.mst')
PKNORLEN <- with(PKNORLEN, close[format(date,'%Y')=='2017'])

PKOBP = wczytaj_mst('mstall.zip', 'PKOBP.mst')
PKOBP <- with(PKOBP, close[format(date,'%Y')=='2017'])

PZU = wczytaj_mst('mstall.zip', 'PZU.mst')
PZU <- with(PZU, close[format(date,'%Y')=='2017'])

PEKAO = wczytaj_mst('mstall.zip', 'PEKAO.mst')
PEKAO <- with(PEKAO, close[format(date,'%Y')=='2017'])

# Podpunkt a)
lm_res_pkt_a <- lm(WIG20~ASSECO+ENERGA+EUROCASH+ORANGE+PKNORLEN+PKOBP+PZU+PEKAO)
summary(lm_res_pkt_a)
# => WIG20 zalezy prawie od wszystkiego (gorzej Orange)

# Podpunkt b)
lm_res_pkt_b <- lm(WIG20~PKNORLEN+PKOBP+PZU+PEKAO)
summary(lm_res_pkt_b)
# => WIG20 pasuje

## Zadanie 3

dane = wczytaj_mst('mstzgr.zip', 'DJIA.mst')
DJIA_df = subset(dane, format(date, '%Y') == '2017', select =c('date', 'close'))
names(DJIA_df) = c('date', 'DJIA')

dane = wczytaj_mst('mstzgr.zip', 'FT-SE100.mst')
FT_SE100_df = subset(dane, format(date, '%Y') == '2017', select =c('date', 'close'))
names(FT_SE100_df) = c('date', 'FT_SE100')

dane = wczytaj_mst('mstzgr.zip', 'NIKKEI.mst')
NIKKEI_df = subset(dane, format(date, '%Y') == '2017', select =c('date', 'close'))
names(NIKKEI_df) = c('date', 'NIKKEI')


ALL_df = merge(DJIA_df, NIKKEI_df, by = 'date')
ALL_df = merge(ALL_df, FT_SE100_df, by = 'date')

## Zadanie 4

v <- c(33, 33, 49.1, 65.2, 78.5, 93)
y <- c(4.7, 4.1, 10.3, 22.3, 33.4, 44.4)

min_v <- min(v)
max_v <- max(v)
STEP = 0.01

plot(v, y, type = 'p')

lm_res_1 <- lm(y~v) # Droga (dlugosc) od predkosci
summary(lm_res_1)

points <- c(min_v, max_v)
values <- coef(lm_res_1)[2]*points + coef(lm_res_1)[1]
lines(points, values, col = 'red')

y_sqrt <- sqrt(y)
lm_res_2 <- lm(y_sqrt~v) # Sqrt droga (dlugosc) ~ predkosc
summary(lm_res_2)

points_2 <- seq(min_v, max_v, by = STEP)
values <- (coef(lm_res_2)[2]*points_2 + coef(lm_res_2)[1])
lines(points_2, values^2, col = 'green')

## Zadanie 5

dane <- read.csv("oldfaithful.txt")
names(dane) <- c("DAY","INTERVAL","DURATION")

# Podpunkt a)
plot(dane$DURATION, dane$INTERVAL)

lm_res_ge <- lm(INTERVAL~DURATION, dane)
summary(lm_res_ge)

arg <- c(min(dane$DURATION), max(dane$DURATION))
out <- coef(lm_res_ge)[2]*arg + coef(lm_res_ge)[1]
lines(arg, out, col = 'red')

# B
eruption_time <- 2
int_prognosis <- coef(lm_res_ge)[2]*eruption_time + coef(lm_res_ge)[1]

# Na okolo
sd(dane$INTERVAL[dane$DURATION == eruption_time] - int_prognosis)
sd(int_prognosis - dane$INTERVAL[dane$DURATION == eruption_time])

# C
eruption_time <- 4
int_prognosis <- coef(lm_res_ge)[2]*eruption_time + coef(lm_res_ge)[1]

# Na okolo
sd(dane$INTERVAL[dane$DURATION == eruption_time] - int_prognosis)
sd(int_prognosis - dane$INTERVAL[dane$DURATION == eruption_time])
                                                                        
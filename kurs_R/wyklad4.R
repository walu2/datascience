

## ---- Grafika ----

# ustawienie ziarna generatora liczb pseudolosowych
set.seed(42)
x <- rnorm(200, mean = 10, sd = 3)
x
## ---- Wizualizacja jednowymiarowa ----

#histogram
?hist
hist(x, 0:20, freq=FALSE)
hist(x, col="red")
hist(x, col="yellow")
hist(x, col=c("blue", "yellow", "green"))
hist(x, col=c("blue", "yellow", "green"), border="white")
hist(x, xlab="Os X", ylab="Os Y", main="Moj histogram")

h <- hist(x, plot=FALSE)
h
plot(h)
# funkcja hist zwraca obiekt klasy histogram, który jest automatycznie rysowany


# histogram domyślnie rysuje częstość (freq == frequency)
# ale można zmienić na gęstość


# liczba podziałów nam nie odpowiada? breaks


# kolor zły?

# może wypełnić liniami?
hist(x, density=c(5,10))
hist(x, axes=TRUE, xlab=NULL, ylab=NULL, main=NULL)
# ten tytuł też mi się nie podoba...

# to jeszcze osie popraw

# albo całkiem je wyrzuć

## określanie kolorów
# 1,nazwa
colors()[1:10]
length(colors())
hist(x, col=sample(colors(),5))
# 2.składowe RGB
# format: #rrggbb lub #rrggbbaa lub funkcja rgb()
hist(x, col="#fdcbda")

?rgb
colur <- rgb(0.1,0.7,0.4)
colur <- rgb(44,66,22, maxColorValue=255)
hist(x, col=colur)

par(mar=rep(0,4))
plot.new()
plot.window(c(0,255),c(0,1))
for(i in 0:255){
  lines(c(i,i), c(i,i), col=rgb(44,77,0,i, maxColorValue = 255), lwd=4)
}
par(mar=c(4,4,3,1))
par(mar=rep(0,4))
plot.new()
plot.window(c(0,255),c(0,255))
for(i in 0:255){
  for(j in 0:255)
    lines(c(i,i), c(j,j), col=rgb(i,j,0, maxColorValue = 255), lwd=4)
}

# wykres kołowy
probka <- sample(c("Audi","Subaru","Toyota","Fiat"), size=200, replace=TRUE,
                 prob = 1:4)
tab <- head(probka)

tab <- table(probka)

pie(tab, r=1.15)
for(i in 50:1) {
  pie(tab, edges=i, main = i)
}

pie(tab, init.angle = 270)

#wieksze koło!
?pie



colours()[1:4]
palette()
# faktycznie koło czy wielokąt?
rainbow(3)
par(mfrow=c(1,1), mar=c(1,1,1,1))
n <- 10
pie(rep(1,n), col = rainbow(n), radius = 0.4)

for(i in 50:1){
  pie(tab, edges=i, main = i)
  #Sys.sleep(1)
}

# obrót

# wykres słupkowy
par(mfrow=c(1,1), mar=c(5,4,3,2))
barplot(tab, col=rainbow(4), main="Slupki")
barplot(tab, density=5:8, angle=30+(0:3)*90)

# wykres skrzynkowy
boxplot(x)

# spłaszczony jak naleśnik, weź go obróć
boxplot(x, horizontal = TRUE)
## ---- Wizualizacja wielowymiarowa ----

mtcars

#wykresy skrzynkowe mogą być rysowana po grupach
boxplot(mtcars$mpg , horizontal = TRUE)

automat <- mtcars$mpg[mtcars$am=="a"]
manual <- mtcars$mpg[mtcars$am=="m"]

boxplot(automat , horizontal = TRUE)
boxplot(manual, horizontal = TRUE)
par(mfrow=c(1,1))
mtcars$am <- c("a", "m")[mtcars$am+1]
boxplot(mtcars$mpg ~ mtcars$am + mtcars$cyl , horizontal = TRUE)

mtcars %>% count(am, cyl)

# wykres rozrzutu (scatter plot)
plot(mtcars$mpg, mtcars$hp)
cor(mtcars$mpg, mtcars$hp)
abline(lm(mtcars$hp ~ mtcars$mpg))


# wykres liniowy
x <- seq(-5,5, by=0.5)
y <- sin(x) * x^2 / 2 + sqrt(abs(cos(x-pi)))

# różne typy wykresów
plot(x, y)
plot(x,y, type="l")
plot(x,y, type="b")
plot(x,y, type="c")
plot(x,y, type="h")


x <- 1:10
y <- 2 ^ x
# skala nie musi być liniowa
plot(x,y, type='l', log="x")

# wykresy w tytułach mogą mieć formuły matematyczne
x <- seq(-5,5, by=0.5)
y <- sin(x) * x^2 / 2 + sqrt(abs(cos(x-pi)))
plot(x,y, type='l',main="sin(x) * x^2 / 2 + sqrt(abs(cos(x-pi)))")
plot(x,y, type='l', main=expression(sqrt(sin(x) +frac(x^2,2))))


## wiele wykresów na jednym
x <- seq(-2*pi, 2*pi, by=0.1)
plot(x,sin(x))
plot(x,cos(x))

# funkcje lines i points rysują na aktualnym
plot(x, sin(x), type='l', col='red')
lines(x, cos(x)+1, type='l', col='green')

# co jak się nie zgadza?
plot(x, sin(x), type='l', col='red', ylim=c(-1,2))
lines(x, cos(x)+1, type='l', col='green')

# wygodniej matplot matlines matpoints
matplot(x, cbind(sin(x), cos(x)+1, atan(x)), type='l', lty=1)

matplot(x, cbind(sin(x), cos(x)+1, atan(x)), 
        type=c('l','b','s'), 
        lty=c(4,2,3), 
        col=rainbow(3), 
        pch=23:25,
        bg="black")

jpeg("nazwa.jpg")
matplot(x, cbind(sin(x), cos(x)+1, atan(x)), lty=1)
legend("bottomright", legend=c("sin", "cos+1", "atan"), col=rainbow(3), lty=c(4,2,3))
dev.off()
# zapis do pliku


## ---- pakiet ggplot2 ----
install.packages("ggplot2")
require(ggplot2)
require(dplyr)

mtcars$am <- factor(c("a","m")[mtcars$am+1])
mtcars$cyl <- factor(mtcars$cyl)


# podstawa to ggplot(dane, aes(co-x, co-y))
ggplot(mtcars, aes(am)) + geom_bar()

ggplot(mtcars, aes(am, fill=cyl)) + geom_bar(position = "fill")
ggplot(mtcars, aes(am, fill=cyl)) + geom_bar(position = "dodge", width=0.5)

# powiedzieliśmy co, teraz jak
ggplot(mtcars, aes(x=interaction(cyl,am))) + geom_bar(width=0.5)

# grupowanie


# za ciasno?

# rozdzielamy grupy

# dodajmy jakieś etykietki

mtcars + group_by(am, cyl) +
  summarise(hp = round(mean(hp))) -> dane


ggplot(aes(interaction(cyl,am)), y=hp, fill=2) +
  geom_bar(stat="identity")

# histogram
ggplot(mtcars, aes(hp)) + geom_histogram()

#gęstość
ggplot(mtcars, aes(hp)) + geom_density(kernel="rectangular")
ggplot(mtcars, aes(hp)) + geom_density(kernel="gauss")
ggplot(mtcars, aes(hp)) + geom_density()

# histogram i gęstość

## liniowe wykresy
x <- seq(-2*pi, 2*pi, by=0.1)
dane <- data.frame(x, sin=sin(x), cos=cos(x)*2.5, exp=exp(x))
ggplot(dane) + geom_line(aes(x,cos))+ylim(0, max(dane$cos)) 

# chcemy zero na osi?

# punkty
ggplot(dane, aes(x, cos)) + geom_line(aes(x,cos))+geom_point(aes(x, cos))
# dziedziczenie estetyki


# zmiana skali 
ggplot(dane, aes(x)) + geom_line(aes(y=sin))+geom_line(aes(y=cos))+ geom_line(aes(y=exp))+ scale_y_log10()

# wiele lini


# przecież mamy dziedziczenie!
ggplot(dane, aes(x)) + geom_line(aes(y=cos),col="red", linetype="dashed", size=1.5) + 
  geom_line(aes(y=sin),col="blue")+
  geom_point(aes(y=sin), size=4, shape=11, col="green")

# dodajemy opisy
ggplot(dane, aes(x)) + geom_line(aes(y=cos),col="red", linetype="dashed", size=1.5) + 
  geom_line(aes(y=sin),col="blue")+
  geom_point(aes(y=sin), size=4, shape=11, col="green") +
  ggtitle("przepiękny wykres") + xlab("To jest oś x")

## Drugi wariant
dane2 <- data.frame(x=rep(dane$x, 3),funkcja=rep(c("sin","cos", "atan"),each=nrow(dane)),
                    y=c(dane$sin, dane$cos, dane$atan))

ggplot(dane2, aes(x=x,y=y, fill=funkcja, shape=funkcja)) + geom_line() + geom_point() +
  scale_shape_manual(values=c(21, 24, 11)) + scale_fill_manual(values=c("black","green","blue"))


# wykres z pokolorowanym obszarem pod wykresem
ggplot(dane, aes(x,sin)) + geom_area()

# wykres składany
install.packages("gcookbook")
library(gcookbook) 
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + geom_area() +
  scale_fill_brewer("blue")

sps <- ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) +
  geom_point() +
  scale_colour_brewer(palette="Set1")
sps

sps + geom_smooth()
sps + geom_smooth(method=lm)
sps + geom_smooth(method=loess)

## zapis do pliku
# automatyczne dopasowanie typu pliku do podanego rozszerzenia
ggsave("ggplot.pdf", plot=sps)
ggsave("ggplot.svg")
#domyślnie zapisuje ostatni wykres używając:
last_plot()

## wielowymiarowy wykres rozproszenia
pairs(mtcars[,2:7])

# wykresy są symetryczne, szkoda marnować miejsca
pairs(mtcars[,2:7], upper.panel = panel.smooth)


ggplot(mpg, aes(hwy, cty)) +
  geom_point(aes(color = cyl)) +
  geom_smooth(method ="lm") +
  coord_cartesian() +
  scale_color_gradient() +
  theme_bw()



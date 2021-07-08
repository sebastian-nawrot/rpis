wybraneAuta <- head(cars, 10)

# Zad. 4
plot(x = wybraneAuta$speed, y = wybraneAuta$dist,
  xlab = "szybkosc",
  ylab = "dystans",
  main = "Wykres szybkosc vs dystans",
  col = "blue",
  pch = 16
)

abline(h = mean(wybraneAuta$dist), col = "red")
abline(v = mean(wybraneAuta$speed), col = "green", lty = "dashed")


# Zad. 5
najszybsze <- wybraneAuta$speed[wybraneAuta$speed > 9]
idNajszybsze <- which(wybraneAuta$speed > 9)
plot(wybraneAuta$speed,
  xlab = "numer auta",
  ylab = "szybkosc",
  main = "Wykres szybkosci",
  col = "blue",
  pch = 20
)

points(idNajszybsze, najszybsze, col = "red", pch = 17)
abline(h = mean(wybraneAuta$speed), col = "red")
text(2.5, 8.3, "sredniaszybkosc", col = "red")

najwolniejsze <- wybraneAuta$speed[wybraneAuta$speed < 7]
idNajwolniejsze <- which(wybraneAuta$speed < 7)
points(idNajwolniejsze, najwolniejsze, col = "purple", pch = 15)


# Zad. 6
wiekPrzypadkowychLudzi2 <- data.frame(
  grupa = c("kobieta", "mezczyzna", "dziecko"),
  wiek = c(27, 30, 5, 25, 35, 10, 29, 29, 7, 40, 42, 15)
)

attach(wiekPrzypadkowychLudzi2)
boxplot(wiek ~ grupa, data = wiekPrzypadkowychLudzi2, horizontal = TRUE)


# Zad. 7
par(mfrow=c(1, 2))
boxplot(wiek ~ grupa, data = wiekPrzypadkowychLudzi2)
stripchart(wiek ~ grupa, data = wiekPrzypadkowychLudzi2, pch = 16)
par(mfrow=c(1, 1))


# Zad. 8
library(plotrix)

pewneStatystyki <- data.frame(
  grupa = c("lubi czekolade", "nie lubi czekolady", "brak opinii"),
  licznosc = c(86,30,9)
)

pie3D(pewneStatystyki$licznosc, labels = pewneStatystyki$grupa)


# Zad. 9
procenty <- pewneStatystyki$licznosc / sum(pewneStatystyki$licznosc) * 100

pie(pewneStatystyki$licznosc, 
  radius = 0.6,
  main = "Jak bardzo lubimy czekolade!",
  labels = paste(procenty, "% ", c("osób lubi", "nie lubi", "nie ma zdania"), sep=""), 
  col = c("#99cc00", "#660099", "#0047b3")
)
legend("topright", 
  cex = 0.6,
  c("osoby lubiace czkolade", "osoby nie lubiace czkolady", "osoby bez zdania"), 
  fill = c("#99cc00", "#660099", "#0047b3"),
  title = "legenda"
)

# "C:\Program Files\R\R-4.0.4\bin\R.exe" CMD BATCH --vanilla "--args http://www.cs.put.poznan.pl/kgutowska/RPiS/dane/25-April-owid-covid-data.csv" .\RPiS_lab_05_zad1.r

data <- read.csv(commandArgs(trailingOnly=TRUE)[1])
ChinaData <- with(data, data[location == "China", ])
dates <- ChinaData["date"]

jpeg("zad1.jpg", width=800, height=800)
plot(as.Date(ChinaData$date), ChinaData$new_cases, col = "red", 
  main = "Dzienny przyrost zarazonych na SARS-COV-2 w Chinach",
  ylab = "Liczba nowych przypadkow",
  xlab = "Data",
  xlim = as.Date(c(dates[[1]][1], dates[[1]][length(dates[[1]])])))
dev.off()
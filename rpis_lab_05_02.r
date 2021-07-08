# "C:\Program Files\R\R-4.0.4\bin\R.exe" CMD BATCH --vanilla .\RPiS_lab_05_zad2.r

funkcja <- function() {
  oceny <- read.csv("oceny.csv") 

  jpeg("zad2.jpg", width=800, height=800)
  barplot(rowMeans(oceny),
          ylab = "Srednia",
          cex.names=0.8,
          names.arg=rowMeans(oceny),
          col = "lightpink",
          border = "red")
  dev.off()

  write.csv(
    cbind(oceny, srednia = rowMeans(oceny)),
    "oceny_ze_srednia.csv",
    quote = FALSE,
    row.names = FALSE)
}

funkcja()
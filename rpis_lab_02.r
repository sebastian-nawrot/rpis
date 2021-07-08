# Zad. 1
seq(10, 25, length.out=10)


# Zad. 2
sample(letters, 5)
# "trzeci argument replace okresla czy losujemy elementy ze zwracaniem"


# Zad. 3
sample(1:3, 20, replace=T, prob=c(0.1, 0.3, 0.6))


# Zad. 4
print(rep(c("ala ma kota", "ola nie ma kota", "ela ma psa"), 3))
cat(rep(c("ala ma kota", "ola nie ma kota", "ela ma psa"), 3))
# print wyswietla zawartosc w spos�b sformatowany, cat nie


# Zad. 5
if (1515 %% 2 == 0) {
  cat("parzysta")
} else {
  cat("nieparzysta")
}


# Zad. 6
x <- 1515
print(switch(class(x), factor="typ czynnikowy", logical="typ logiczny",
           numeric="typ liczbowy", "inny typ"))


# Zad. 7
for (i in 3:333) {
  if (i %% 3 == 0) {
    cat(i, "\n")
  }
}


# Zad. 8
for (i in 1:33) {
  if (33 %% i == 0) {
    cat(i, "\n")
  }
}


# Zad. 9
n1 <- 0
n2 <- 1
index <- 0
for (i in 0:15) {
  cat(n1, "\n")
  if (n1 == 233) {
    cat("indeks dla 233 to", i, "\n")
  }
  
  n2 <- n1 + n2
  n1 <- n2 - n1
}


# Zad. 10
x <- data.frame(id=1:10, sample(1:10, 10), sample(10:20, 10, replace=TRUE))

suma <- 0
for (i in 1:10) {
  suma = suma + x[[2]][i]
}
cat(suma/10, "\n")

suma <- 0
for (i in 1:10) {
  suma = suma + x[[3]][i]
}
cat(suma/10, "\n")
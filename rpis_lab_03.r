# Zad. 1
funkcja <- function(wektor, b) {
  wektor[wektor %% b == 0]
}

funkcja(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 4)


# Zad. 2
funkcja <- function(a, b, c) {
  delta <- b * b - 4 * a * c
  if (delta > 0) {
    cat("x1", (-b - sqrt(delta)) / (2 * a), "\n")
    cat("x2", (-b + sqrt(delta)) / (2 * a), "\n")
  } else if (delta == 0) {
    cat("x", -b / (2 * a), "\n")
  } else {
    cat("brak rozwiązań\n")
  }
}

funkcja(1, 2, -3)


# Zad. 3
funkcja <- function(wektor) {
  head(sort(wektor), 3)
}

funkcja(c(2, 1, 3, 7))


# Zad. 4
funkcja <- function(wektor) {
  if (length(wektor) < 3) {
    cat("za krótki wektor\n")
  }
  else {
    c(head(sort(wektor), 3), tail(sort(wektor), 3))
  }
}

funkcja(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))


# Zad. 5
funkcja <- function(argument1, argument2) {
  UseMethod("funkcja")
}

funkcja.numeric <- function(argument1, argument2) {
  argument1 * argument2
}

funkcja.matrix <- function(argument1, argument2) {
  argument1 %*% argument2
}

funkcja(c(1, 2, 3), c(4, 5, 6))
funkcja(matrix(1:6, 2, 3), matrix(1:3))
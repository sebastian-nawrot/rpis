# Narzędzie służące do przeprowadzenia podstawowej analizy statystycznej dla danych medycznych

if (!require(pacman))
  install.packages("pacman", repos = "https://cloud.r-project.org/", quiet = TRUE)

pacman::p_load(mice, outliers, ggpubr, dplyr, car, FSA)

unlink("output", recursive=TRUE)

if (length(commandArgs(trailingOnly=TRUE)) == 0) {
  print("Podaj nazwe pliku w parametrze wejsciowym")
  quit(status=1)
}

allData <- read.csv2(commandArgs(trailingOnly=TRUE)[1])


dir.create("output")
dir.create("output/korelacje_istniejace")
dir.create("output/brak_korelacji")


groups = split(allData, allData$grupa)
for(i in 1:length(groups)) {
  data <- groups[[i]]

  # 1. Przygotowanie danych wejściowych przez usunięcie braków danych. Zaraportowanie
  # wszystkich wprowadzonych zmian. Dodatkowo zaraportowanie informacji o wartościach
  # odstających dla wybranych parametrów.
  cat("> Found", sum(is.na(data)), "missing fields\n")
  # imputedData <- complete(mice(data, m = 1, printFlag = FALSE))

  imputedData <- data;
  for (c in 1:ncol(imputedData)) {
    if (is.numeric(imputedData[, c])) {
      imputedData[, c][is.na(imputedData[, c])] = mean(imputedData[,c], na.rm = TRUE);
    }
  }

  for(r in 1:nrow(data)) {
    for(c in 1:ncol(data)) {
      if (is.na(data[r, c])) {
        cat(data$grupa[1], "Brak w kolumnie", colnames(data)[c], "w wierszu", r,
            "został zastąpiony wartością", imputedData[r, c], "\n",
            file = "output/imputedValues.txt", append = TRUE)
      }
    }
  }


  # 2. Wykonanie charakterystyk dla badanych grup, zapisanie wyników w czytelnej
  # formie (polecana struktura tabelaryczna).
  write.csv2(do.call(cbind, lapply(Filter(is.numeric, imputedData), summary)),
    quote = FALSE, file = paste("output/", data$grupa[1], "_summary.csv", sep = ""))

  groups[[i]] <- imputedData
}

for (i in 1:ncol(groups[[1]])) {
  if (is.numeric(groups[[1]][[i]])) {
    png(paste("output/", colnames(groups[[1]])[i], "_outliers.png", sep = ""), width=1000, height=1000)
    par(mfrow = c(1, 3), cex.lab=1.5, cex.axis=1.5)

    for (n in 1:length(groups)) {
      boxplot(groups[[n]][[i]], ylab = "Values", xlab = groups[[n]]$grupa[1])
      outliers <- boxplot.stats(groups[[n]][[i]])$out
      mtext(paste("Outliers: ", paste(outliers, collapse = ", ")))
    }

    dev.off()
  }
}


write.csv2(allData, quote = FALSE, row.names = FALSE, file = "output/inputData.csv")
allData <- do.call(rbind, groups)
write.csv2(allData, quote = FALSE, row.names = FALSE, file = "output/imputedData.csv")



# 3. Wykonanie analizy porównawczej pomiędzy grupami, określenie czy istnieją
# istotne statystycznie różnice. Jeśli istnieją istotne statystyczne różnice
# pomiędzy grupami to zaraportowanie pomiędzy którymi grupami występują i jak
# istotne są to różnice.
for (c in 1:ncol(allData)) {
  if (is.numeric(allData[, c])) {
    normal_distribution <- shapiro.test(allData[, c])$p.value > 0.05
    variances_equality <- leveneTest(allData[[c]] ~ allData$grupa)$"Pr(>F)"[1] > 0.05

    if (length(groups) == 2) {
      if (normal_distribution) {
        result <- t.test(allData[[c]] ~ allData$grupa, var.equal = variances_equality)$p.value
        cat(if (variances_equality) "test t-Studenta dla kolumny"
            else "test Welcha dla kolumny",
            colnames(allData)[c], "p-value =", result,
            if (result < 0.05) "< 0.05 - są różnice pomiędzy grupami\n"
            else "> 0.05 - brak różnic pomiędzy grupami\n",
            file = "output/analysis.txt", append = TRUE)
      }
      else {
        result <- wilcox.test(allData[[c]] ~ allData$grupa)$p.value
        cat("test Wilcoxona dla kolumny", colnames(allData)[c], "p-value =", result,
            if (result < 0.05) "< 0.05 - są różnice pomiędzy grupami\n"
            else "> 0.05 - brak różnic pomiędzy grupami\n",
            file = "output/analysis.txt", append = TRUE)
      }
    }
    else {
      if (normal_distribution && variances_equality) {
        result <- summary(aov(allData[[c]] ~ allData$grupa))[[1]][["Pr(>F)"]][[1]]
        cat("test ANOVA dla kolumny", colnames(allData)[c], "p-value =", result,
            if (result < 0.05) "< 0.05 - są różnice pomiędzy grupami\n"
            else "> 0.05 - brak różnic pomiędzy grupami\n",
            file = "output/analysis.txt", append = TRUE)

        if (result < 0.05) {
          cat("test Tukeya (post hoc) dla kolumny", colnames(allData)[c], "\n",
              file = "output/analysis.txt", append = TRUE)
          result <- TukeyHSD(aov(allData[[c]] ~ allData$grupa))
          write.csv2(result[[1]], row.names = TRUE, quote = FALSE,
                      file = paste("output/", colnames(allData)[c],
                                    "_tukey_analysis.txt", sep = ""))

          png(paste("output/", colnames(allData)[c], "_tukey_analysis.png", sep = ""),
              width=1000, height=1000)
          plot(result)
          dev.off()
        }
      }
      else {
        result <- kruskal.test(allData[[c]] ~ allData$grupa)$p.value
        cat("test Kruskala-Wallisa dla kolumny", colnames(allData)[c], "p-value =", result,
            if (result < 0.05) "< 0.05 - są różnice pomiędzy grupami\n"
            else "> 0.05 - brak różnic pomiędzy grupami\n",
            file = "output/analysis.txt", append = TRUE)

        if (result < 0.05) {
          cat("test Dunna (post hoc) dla kolumny", colnames(allData)[c], "\n",
              file = "output/analysis.txt", append = TRUE)
          result <- dunnTest(allData[[c]], allData$grupa)
          write.csv2(result$res, row.names = TRUE, quote = FALSE,
                      file = paste("output/", colnames(allData)[c],
                                    "_dunna_analysis.txt", sep = ""))

          png(paste("output/", colnames(allData)[c], "_dunna_analysis.png", sep = ""),
              width=1000, height=1000)
          plot(result$res)
          dev.off()
        }
      }
    }
  }
}


# 4. Wykonanie analizy korelacji. Zaraportowanie pomiędzy którymi parametrami w
# obrębie jakich grup występują istotne statystycznie korelacje oraz określenie
# siły i kierunku korelacji.
columns = combn(colnames(select_if(allData, is.numeric)), 2, simplify = FALSE)
for (i in groups) {
  for (pair in columns) {
    normal_distribution <- shapiro.test(allData[, pair[1]])$p.value > 0.05 &&
                           shapiro.test(allData[, pair[2]])$p.value > 0.05

    variances_equality <- leveneTest(allData[, pair[1]] ~ allData$grupa)$"Pr(>F)"[1] > 0.05 &&
                          leveneTest(allData[, pair[2]] ~ allData$grupa)$"Pr(>F)"[1] > 0.05

    result <- cor.test(i[[pair[1]]], i[[pair[2]]],
      method = if (normal_distribution && variances_equality) "pearson" else "spearman")

    if (result$p.value < 0.05) {
      cat(i[[1]][1], "analiza korelacji kolumn", pair[1], "i", pair[2], "p-value =", result$p.value,
          "< 0.05 - korelacja istnieje, estimate =", result$estimate, 
          case_when(
            -1 < result$estimate && result$estimate < -0.7 ~ "bardzo silna korelacja ujemna",
            -0.7 < result$estimate && result$estimate < -0.5 ~ "silna korelacja ujemna",
            -0.5 < result$estimate && result$estimate < -0.3 ~ "korelacja ujemna o średnim natężeniu",
            -0.3 < result$estimate && result$estimate < -0.2 ~ "słaba korelacja ujemna",
            -0.2 < result$estimate && result$estimate < 0.2 ~ "brak korelacji",
            0.2 < result$estimate && result$estimate < 0.3 ~ "słaba korelacja dodatnia",
            0.3 < result$estimate && result$estimate < 0.5 ~ "korelacja dodatnia o średnim natężeniu",
            0.5 < result$estimate && result$estimate < 0.7 ~ "silna korelacja dodatnia",
            0.7 < result$estimate && result$estimate < 1 ~ "bardzo silna korelacja dodatnia"),
          "\n", file = "output/correlations.txt", append = TRUE)
    }
    else {
      cat(i[[1]][1], "analiza korelacji kolumn", pair[1], "i", pair[2], "p-value =", result$p.value,
          "> 0.05 - brak korelacji\n", file = "output/correlations.txt", append = TRUE)
    }

    plot <- ggscatter(i, x = pair[1], y = pair[2], add = "reg.line", conf.int = TRUE, cor.coef = TRUE,
              cor.method = if (normal_distribution && variances_equality) "pearson" else "spearman")

    ggsave(plot, file = paste(
      if (result$p.value < 0.05) "output/korelacje_istniejace/" else "output/brak_korelacji/",
      i[[1]][1], "_correlation_", pair[1], "_", pair[2], ".png", sep = ""))
  }
}
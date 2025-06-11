kordat <- read.table("variants13.txt", header = TRUE, sep = "\t", strip.white = TRUE, dec = ",", stringsAsFactors = FALSE)

kordat$Slope <- as.numeric(as.character(kordat$Slope))
kordat$Intercept <- as.numeric(as.character(kordat$Intercept))
kordat$adj.r.squared <- as.numeric(as.character(kordat$adj.r.squared))
kordat$f <- as.factor(kordat$f)
kordat$Average <- rowMeans(kordat[, c("Slope", "Intercept", "adj.r.squared")], na.rm = TRUE)

stdevF <- tapply(kordat$Average, kordat$f, sd)
cat("\nStandartnovirze pa f faktora līmeņiem:\n")
print(stdevF)

filtretiDati <- subset(kordat, adj.r.squared > 0.7)

if(nrow(filtretiDati) == 0) {
  cat("\nNav datu ar adj.r.squared > 0.7.\n")
} else {
  filtretiDati$Slope <- 1 - 1 / filtretiDati$Slope
}

cat("\nDati pēc apstrādes: ")
print(filtretiDati)

sink("results.txt")

svg("scatter.svg")
plot(kordat$MAD, kordat$Average, xlab = "MAD", ylab = "Average", main = "Izkliede: MAD pret Average", pch = 19, col = "blue")
dev.off()

svg("boxplot.svg")
boxplot(Intercept ~ f, data = kordat, xlab = "Faktors f", ylab = "Intercept", main = "Kastu diagramma: Intercept pa f faktoru", col = rainbow(length(levels(kordat$f))))
dev.off()

vektors <- unlist(strsplit(rownames(kordat), "\\."))
frekvencesVektors <- table(vektors)
popVektors <- names(frekvencesVektors)[which.max(frekvencesVektors)]
cat("\nVisbiežāk sastopamais līmenis:", popVektors, "\n")

izveletasRindas <- filtretiDati[grep(popVektors, rownames(filtretiDati)), , drop = FALSE]

cat("\nRindas ar līmeni", popVektors, ":\n")
print(izveletasRindas)

sink()
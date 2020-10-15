library(pcalg)
library(graph)
library(Rgraphviz)
kausal <-read.csv("D:/folder/hepatitis.csv", header=TRUE)
max = ncol(kausal)
par(mfrow=c(3,4))
for (i in 1:max) {
  hist(as.vector(unlist(kausal[i])), main = paste("Variabel ke", i, "-", names(kausal)[i]), xlab= "")
}
# Pemilihan variabel yang memiliki distribusi normal
keeps <- c(2,16,18,19)
dataku = kausal[keeps]

# GES Algorithm
score <- new("GaussL0penObsScore", dataku)
ges.fit <- ges(score)

# Plot graph
par(mfrow=c(1,2))
plot(ges.fit$essgraph, main="Estimated by GES")

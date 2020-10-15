library(pcalg)
library(graph)
library(Rgraphviz)
datasets <- read.csv("E:/Semester 3/Pemodelan Kausal/Project UAS/datasets.csv", header = TRUE )
max = ncol(datasets)
par(mfrow=c(3,4))
for (i in 1:max) {
  hist(as.vector(unlist(datasets[i])), main = paste("Variabel ke", i, "-", names(datasets)[i]), xlab= "")
}
  #Pemilihan variabel yang memiliki distribusi normal
keeps <- c(2,3,5,6)
data_used = datasets[keeps]
# GES Algorithm
library(pcalg)
score <- new("GaussL0penObsScore", data_used)
ges.fit <- ges(score)

# PC Algorithm
allStat <- list(C = cor(data_used), n = nrow(data_used))
pc.fit <- pc(suffStat = allStat,
             indepTest = gaussCItest,
             alpha=0.01, p=ncol(data_used))

# Plot graph
par(mfrow=c(1,2))
plot(pc.fit, labels=colnames(data_used), main = "Estimated by PC")
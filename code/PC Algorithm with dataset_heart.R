## Normal
library(pcalg)
Data <- read.csv("C:/Users/dikurr/Documents/heart_Rifai.csv", header = TRUE)
library(gplots)
#par(mfrow=c(5,10))
#for(i in 1:ncol(Data)) {
#  hist(Data[, i], main=paste("Variabel ke", i), xlab = "")
#}
hist(Data$ï..age) #1
hist(Data$sex) #2
hist(Data$cp) #3
hist(Data$trestbps) #4
hist(Data$chol) #5
hist(Data$fbs) #6
hist(Data$restecg) #7
hist(Data$thalach) #8
hist(Data$exang) #9
hist(Data$oldpeak) #10
hist(Data$slope) #11
hist(Data$ca) #12
hist(Data$thal) #13
hist(Data$target) #14

library(dplyr)
hapus<-select.list(Data, multiple = TRUE, -c(Data$ï..age, Data$trestbps))
#selec(Data, -c(Data$sex))
head(hapus)

library(pcalg)
hasil <- hapus


## CONTOH 1 PCA
library(pcalg)
library(Rgraphviz)
##########Komputasi GES
score <- new("GaussL0penObsScore", hasil)
show(hasil)
ges.fit <- ges(score)

##########Komputasi PC algorithm
allStat <- list(C = cor(hasil), n = nrow(hasil))
pc.fit <- pc(suffStat = allStat,
             indepTest = gaussCItest, 
             alpha=0.05, p=ncol(hasil))

##########Plot true model hasil PC algorihtm
par(mfrow=c(1,4))
plot(rDAG, main = "True DAG")
plot(ges.fit$essgraph, main = "Estimated by GES")
plot(pc.fit, labels=colnames(hasil), main = "Estimated by PC")

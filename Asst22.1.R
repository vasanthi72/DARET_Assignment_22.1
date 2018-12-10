epi_r <- read.csv("D:/Abhyayanam/DARET/Assignments/epi_r.csv", stringsAsFactors=FALSE)
sum(is.na(epi_r))
library(mice)
tempData <- mice(epi_r,m=5,maxit=5,meth='pmm',seed=500)
compepi_r <- complete(tempData,1)
head(compepi_r)
str(compepir_r)
par(mfrow=c(2,3))
(boxplot(compepi_r$rating)$out);(boxplot(compepi_r$calories)$out);(boxplot(compepi_r$protein)$out);(boxplot(compepi_r$fat)$out);(boxplot(compepi_r$sodium)$out)
qntcalories <- quantile(compepi_r$calories, probs=c(.25, .75))
qntprotein <- quantile(compepi_r$protein, probs=c(.25, .75))
qntfat <- quantile(compepi_r$fat, probs=c(.25, .75))
qntsodium <- quantile(compepi_r$sodium, probs=c(.25, .75))
Hcalories <- 1.5 * IQR(compepi_r$calories)
Hprotein <- 1.5 * IQR(compepi_r$protein)
Hfat <- 1.5 * IQR(compepi_r$fat)
Hsodium <- 1.5 * IQR(compepi_r$sodium)
outcompepi_r <- compepi_r
qntcalories[1]
qntcalories[2]
qntprotein[1]
qntprotein[2]
qntfat[1]
qntfat[2]
qntsodium[1]
qntsodium[2]
summary(compepi_r[,c(3,4,5,6)])
outcompepi_r$calories[outcompepi_r$calories < (qntcalories[1] - Hcalories)] <- qntcalories[1]
outcompepi_r$protein[outcompepi_r$protein < (qntprotein[1] - Hprotein)] <- qntprotein[1]
outcompepi_r$fat[outcompepi_r$fat < (qntfat[1] - Hfat)] <- qntfat[1]
outcompepi_r$sodium[outcompepi_r$sodium < (qntsodium[1] - Hsodium)] <- qntsodium[1]
outcompepi_r$sodium[outcompepi_r$sodium > (qntsodium[2] + Hsodium)] <- qntsodium[2]
outcompepi_r$fat[outcompepi_r$fat > (qntfat[2] + Hfat)] <- qntfat[2]
outcompepi_r$protein[outcompepi_r$protein > (qntprotein[2] + Hprotein)] <- qntprotein[2]
outcompepi_r$calories[outcompepi_r$calories > (qntcalories[2] + Hcalories)] <- qntcalories[2]
summary(outcompepi_r[,c(3,4,5,6)])
set.seed(1234)
ind = sample(1:nrow(outcompepi_r),0.8*nrow(outcompepi_r),replace = F)
df_train =outcompepi_r[ind,-1]
df_test = outcompepi_r[-ind,-1]
library(stats)
df_train1 <- scale(df_train)
sum(is.na(df_train1))
colnames(df_train1)[colSums(is.na(df_train1)) > 0]
which(colnames(df_train1)=="X.wasteless" )
which(colnames(df_train1)=="caviar" )
which(colnames(df_train1)=="crêpe" )
which(colnames(df_train1)=="marinade" )
which(colnames(df_train1)=="new.hampshire" )
which(colnames(df_train1)=="pot.pie" )
which(colnames(df_train1)=="sardine" )
compdf_train1 <- df_train1[,-c(7,111,167,364,403,493,547)]
colnames(compdf_train1)[,c(1,2,3,4,5)]
colnames(compdf_train1)[c(1,2,3,4,5)]
classcompdf_train1 <- compdf_train1[,c(1,2,3,4,5)]
str(classcompdf_train1)
head(classcompdf_train1, number = 6)
kmc1 = kmeans(classcompdf_train1,1)
kmc2 = kmeans(classcompdf_train1,2)
kmc3 = kmeans(classcompdf_train1,3)
kmc4 = kmeans(classcompdf_train1,4)
kmc5 = kmeans(classcompdf_train1,5)
classtotwithinss <- c(kmc1$tot.withinss, kmc2$tot.withinss, kmc3$tot.withinss,kmc4$tot.withinss, kmc5$tot.withinss)
plot(classtotwithinss, type = 'b', main = "Screeplot showing within group sum of squares")
df_train$cl <- as.numeric(kmc5$cluster)
dim(df_train)
library(dplyr)
aggregate(df_train[,1:679],list(df_train[,680]),mean)
table(compepi_r$title)
library(cluster)
clusplot(classcompdf_train1,df_train$cl, cex=0.9,colour=T, shade=T, labels=4, lines=0)
dim(df_train)
df_train <- df_train[,-680]
length(kmc3$cluster)
df_train$cl <- as.numeric(kmc3$cluster)
dim(df_train)
aggregate(df_train[,1:679],list(df_train[,680]),mean)
table(df_train$cl)
aggregate(data = df_train, protein ~ cl, mean)
aggregate(data = df_train, fat ~ cl, mean)
aggregate(data = df_train, calories ~ cl, mean)
aggregate(data = df_train, sodium ~ cl, mean)



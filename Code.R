setwd('/Users/Sofiane/Desktop/TND/Projet/data')
library(FactoMineR)
library(rmatio)
library(igraph)
library ( XLConnect )

# Loading TRANSFUSION as data frame
ds = as.data.frame(read.csv("transfusion.data"))[,1:4]

# Mounting the summary as a data frame
sumup = rbind(summary(ds[,1]), summary(ds[,2]), summary(ds[,3]), summary(ds[,4]))
sumup = as.data.frame(sumup)

# Adding the standard deviation
sumup["Std"] = c(sd(ds[,1]), sd(ds[,2]), sd(ds[,3]), sd(ds[,4]))

# Setting row names for readability
rownames(sumup) = c("Recency", "Frequency", "Monetary", "TimeMonths")

# Displaying histograms
hist(ds[,1], main="Recency", xlab="Recency")
hist(ds[,2], main="Recency", xlab="Recency")
hist(ds[,2], main="Frequency", xlab="Frequency")
hist(ds[,2], main="Monetary", xlab="Monetary")
hist(ds[,3], main="Monetary", xlab="Monetary")
hist(ds[,4], main="Time", xlab="Time")

# PCA
pca = PCA(ds, scale.unit = TRUE, ncp = 2)
plot.PCA(pca, label="none", choix = "ind")

# Classification
hc = hclust(dist(ds))
kmeans = kmeans(ds, 2)

# Confusion HC
confhc = as.data.frame(cbind(as.data.frame(read.csv("transfusion.data"))[,5],cutree(hc, 2)))
confhc$V2 = confhc$V2 - 1 # To match 1 / O

confhc["HIT"] = confhc$V1 == confhc$V2
confhc["TP"] = confhc$V1 & confhc["HIT"] # TRUE POSITIVES
confhc["TN"] = !confhc$V1 & confhc["HIT"] # TRUE NEGATIVES
confhc["FP"] = !confhc$V1 & !confhc["HIT"] # FALSE POSITIVES
confhc["FN"] = confhc$V1 & !confhc["HIT"] # FALSE NEGATIVES

# Confusion KMeans
confkmeans = as.data.frame(cbind(as.data.frame(read.csv("transfusion.data"))[,5],kmeans$cluster))
confkmeans$V2 = confkmeans$V2 - 1 # To match 1 / O

confkmeans["HIT"] = confkmeans$V1 == confkmeans$V2
confkmeans["TP"] = confkmeans$V1 & confkmeans["HIT"] # TRUE POSITIVES
confkmeans["TN"] = !confkmeans$V1 & confkmeans["HIT"] # TRUE NEGATIVES
confkmeans["FP"] = !confkmeans$V1 & !confkmeans["HIT"] # FALSE POSITIVES
confkmeans["FN"] = confkmeans$V1 & !confkmeans["HIT"] # FALSE NEGATIVES


# Projection dans le plan factoriel
plot(pca$ind$coord, col = cutree(hc, 2), pch = 20, main = "HC")
plot(pca$ind$coord, col = kmeans$cluster, pch = 20, main = "KMeans")

# --------------------------------------
# Loading WINES as data frame
ds = as.data.frame(read.csv("wine.data"))[,2:14]

# Mounting the summary as a data frame
sumup = rbind(summary(ds[,1]), summary(ds[,2]), summary(ds[,3]), summary(ds[,4]), summary(ds[,5]), summary(ds[,6]), summary(ds[,7]), summary(ds[,8]), summary(ds[,9]), summary(ds[,10]), summary(ds[,11]), summary(ds[,12]), summary(ds[,13]))
sumup = as.data.frame(sumup)

# Adding the standard deviation
sumup["Std"] = c(sd(ds[,1]), sd(ds[,2]), sd(ds[,3]), sd(ds[,4]), sd(ds[,5]), sd(ds[,6]), sd(ds[,7]), sd(ds[,8]), sd(ds[,9]), sd(ds[,10]), sd(ds[,11]), sd(ds[,12]), sd(ds[,13]))

# Displaying histograms
hist(ds[,1])
hist(ds[,2])
hist(ds[,3])
hist(ds[,4])
hist(ds[,5])
hist(ds[,6])
hist(ds[,7])
hist(ds[,8])
hist(ds[,9])
hist(ds[,10])
hist(ds[,11])
hist(ds[,12])
hist(ds[,13])

# PCA
pca = PCA(ds, scale.unit = TRUE, ncp = 3)
plot.PCA(pca, label="none", choix = "ind")

# Classification
hc = hclust(dist(ds))
kmeans = kmeans(ds, 3)

# Confusion HC
confhc = as.data.frame(cbind(as.data.frame(read.csv("wine.data"))[,1], cutree(hc, 3)))
names(confhc) = c("Label", "Prediction")
confhc$Prediction[confhc$Prediction == 3] = 0 # Inverting class 3 & 2 to match labels
confhc$Prediction[confhc$Prediction == 2] = 3
confhc$Prediction[confhc$Prediction == 0] = 2

confhc["HIT"] = confhc$Label == confhc$Prediction # To measure the accuracy

# 58 errors vs 119 success

# Confusion KMeans
confkmeans = as.data.frame(cbind(as.data.frame(read.csv("wine.data"))[,1],kmeans$cluster))
names(confkmeans) = c("Label", "Prediction")
confkmeans$Prediction[confkmeans$Prediction == 1] = 0
confkmeans$Prediction[confkmeans$Prediction == 3] = 1
confkmeans$Prediction[confkmeans$Prediction == 0] = 3
confkmeans$Prediction[confkmeans$Prediction == 2] = 0
confkmeans$Prediction[confkmeans$Prediction == 3] = 2
confkmeans$Prediction[confkmeans$Prediction == 0] = 3

confkmeans["HIT"] = confkmeans$Label == confkmeans$Prediction # To measure the accuracy

# 53 errors vs 124 success

# Projection dans le plan factoriel
plot(pca$ind$coord, col = cutree(hc, 3), pch = 20, main = "HC")
plot(pca$ind$coord, col = kmeans$cluster, pch = 20, main = "KMeans")

# ==============================================================================
# Loading DOCUMENTS data
ds = read.mat("classic30.mat")

# Classification
kmeans = kmeans(ds$dtm, 3)

table(ds$classid)
table(kmeans$cluster)

adj = as.matrix(ds$dtm) %*% t(as.matrix(ds$dtm))
adj[adj > 0] = 1
g = graph_from_adjacency_matrix(adj, mode = "undirected")
groups = cluster_fast_greedy(g)

compare = data.frame(class=ds$classid, groups=groups$membership, cluster=kmeans$cluster)
compare$groups[compare$groups == 1] = 0
compare$groups[compare$groups == 2] = 1
compare$groups[compare$groups == 0] = 2

compare["GROUPS_HIT"] = compare$class == compare$groups

table(compare$GROUPS_HIT) # 5 errors vs 25 success

plot(g, mark.groups = groups)

# ==============================================================================
# Loading POWER PLANTS DATA

# NOTE: The specified package is not available for my version of R.
ds = as.data.frame(readxl::read_xlsx("Folds5x2_pp.xlsx"))

sumup = rbind(summary(ds[,1]), summary(ds[,2]), summary(ds[,3]), summary(ds[,4]), summary(ds[,5]))
sumup = as.data.frame(sumup)

# Adding the standard deviation
sumup["Std"] = c(sd(ds[,1]), sd(ds[,2]), sd(ds[,3]), sd(ds[,4]), sd(ds[,5]))

# Setting row names for readability
rownames(sumup) = c("AT", "V", "AP", "RH", "PE")

# Computing correlations
cor = as.data.frame(round(cor(ds), 2))

# Displaying first 200 points
plot(ds[1:200,], pch=20)

# Linear regressions
lmAT = lm(PE ~ AT, ds)

plot(ds$AT, ds$PE, pch=20)
abline(lmAT$coefficients[[1]], lmAT$coefficients[[2]], col="red")

lmV = lm(PE ~ V, ds)

plot(ds$V, ds$PE, pch=20)
abline(lmV$coefficients[[1]], lmV$coefficients[[2]], col="red")

mseAT = round(mean(lmAT$residuals^2), 2)
mseV = round(mean(lmV$residuals^2), 2)

maeAT = round(mean(abs(lmAT$residuals)), 2)
maeV = round(mean(abs(lmV$residuals)), 2)







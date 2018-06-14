####################################################################
####################### DATA ANALYSIS - 2016 ####################### 
####################################################################

# The following steps are performed on the 2016 database:
#   1. Fueature ranking.
#   2. Feature selection.
#   3. Hierarchical clustering (using only selected features).
#   4. Visualization.

# Since each step is very compute-intensive, 
# it is advised that they are performed separately.

####################################################################
#                        0. INITIALIZATION                         #
####################################################################

# Add these commands as header to any of the analysis steps

# Install packages: execute only the first time.
#install.packages("mlbench")
#install.packages("caret")
#install.packages("tidyverse")
#install.packages("cluster")
#install.packages("factoextra")
#install.packages("dendextend")

# Load libraries
library(mlbench)
library(caret)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization

# Set the path where the project files can be retrieved/stored
path = '/home/drr342/bds/'

####################################################################
#                        1. FEATURE RANKING                        #
####################################################################

# Approx. execution time in HPC Prince Cluster: 6 hours

# Load the data
data2016 <- read.csv(paste(path, 'zip_2016_predictive.csv', sep = ''), 
                     na.strings = "NaN", 
                     header = TRUE, 
                     colClasses = c("VIOLENT." = "factor"))

# Handle missing values: replace with mean
fill_in_values <- function(data, strategy){
  numeric_cols <- sapply(data[1,], is.numeric)
  data[, numeric_cols] <- apply(data[ , numeric_cols], 2, function(x){
    is_na <- is.na(x)
    x[is_na] <- strategy(x[!is_na])
    x
  })
  data
}
data2016_mean <- fill_in_values(data2016, mean)[, 4:95]

# Find correlation matrix
cMatrix2016 <- cor(data2016_mean[-92])
highlyC2016 <- findCorrelation(cMatrix2016, cutoff = 0.75, exact = TRUE)
data2016_clean <- data2016_mean[, -highlyC2016]

# Fit Learning Vector Quantization (LVQ) model using repeated cross validation
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
model2016 <- train(VIOLENT. ~ ., 
                   data = data2016_clean, 
                   method = "lvq", 
                   preProcess = "scale", 
                   trControl = control)
importance2016 <- varImp(model2016, scale = FALSE)

# Save workspace
save.image(paste(path, 'ranking2016.RData', sep = ''))

####################################################################
#                       2. FEATURE SELECTION                       #
####################################################################

# Approx. execution time in HPC Prince Cluster: 13 hours

# Load workspace from feature ranking
load(paste(path, 'ranking2016.RData', sep = ''))

# Run Recursive Feature Elimination (RFE) with a Random Forest selection function
control_rfe <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
vf2016 <- dim(data2016_clean)[2]
results2016 <- rfe(data2016_clean[, 1:vf2016 - 1], 
                   data2016_clean[, vf2016], 
                   sizes = c(1:vf2016 - 1), 
                   rfeControl = control_rfe)
predictors2016 <- predictors(results2016)

# Save workspace
save.image(paste(path, 'selection2016.RData', sep = ''))

####################################################################
#                    3. HIERARCHICAL CLUSTERING                    #
####################################################################

# Approx. execution time in HPC Prince Cluster: 20 hours

# Load workspace from feature selection
load(paste(path, 'ranking2016.RData', sep = ''))
load(paste(path, 'selection2016.RData', sep = ''))

# Run Agnes (Agglomerative Nesting) hierarchical clustering algorithm
data2016_predictors <- scale(data2016_clean[, predictors2016])
agnes2016 <- agnes(data2016_predictors, method = "complete")

# Save workspace
save.image(paste(path, 'clustering2016.RData', sep = ''))

####################################################################
#                         4. VISUALIZATION                         #
####################################################################

# Load workspace from clustering
load(paste(path, 'ranking2016.RData', sep = ''))
load(paste(path, 'selection2016.RData', sep = ''))
load(paste(path, 'clustering2016.RData', sep = ''))

# Confusion matrix
get_golden <- function(x) x$VIOLENT.
test_indices_2016 <- createDataPartition(data2016$VIOLENT., p = 0.3)[[1]]
test_data2016 <- data2016_clean[test_indices_2016, ]
cm2016 <- confusionMatrix(predict(model2016, newdata=test_data2016), 
                          get_golden(test_data2016))
print(cm2016)
print(cm2016$overall['Accuracy'])
print(cm2016$byClass['F1'])

# Print highly correlated attributes
print(colnames(data2016_mean)[highlyC2016])

# Summarize features importance
print(importance2016)

# Plot features importance
plot(importance2016, top = 20)

# Summarize feature selection results
print(results2016)

# plot feature selection results
plot(results2016, type=c("g", "o"))

# List the chosen features
print(predictors2016)

# Plot Agnes dendrogram
pltree(agnes2016, cex = 0.6, hang = -1, main = "Dendrogram 2016")

# Cut Agnes tree into specific number of clusters
k2016 <- 4
clusters2016 <- cutree(as.hclust(agnes2016), k = k2016)

# Append Cluster column to original data
data2016_clusters <- data2016_clean
data2016_clusters$Cluster <- as.data.frame(clusters2016)$clusters2016

# Relationship cluster-label
cluster_label_2016 = matrix(nrow = k2016, ncol = 2,  dimnames = list(NULL, c("0", "1")))
for (k in 1:k2016){
  cluster_label_2016[k, 1] <- length(which(
    data2016_clusters[, ncol(data2016_clusters) - 1] == 0 &
      data2016_clusters[, ncol(data2016_clusters)] == k))
  cluster_label_2016[k, 2] <- length(which(
    data2016_clusters[, ncol(data2016_clusters) - 1] == 1 &
      data2016_clusters[, ncol(data2016_clusters)] == k))
}
cluster_label_2016 <- as.data.frame(cluster_label_2016)
print(cluster_label_2016)

# Number of members in each cluster
table(clusters2016)

# Plot Agnes dendrogram with border around clusters
plot(as.hclust(agnes2016), cex = 0.6)
rect.hclust(as.hclust(agnes2016), k = k2016, border = 2:(k2016 + 1))

# Scatter plot of the final clusters
fviz_cluster(list(data = data2016_clean[, -ncol(data2016_clean)], 
                  cluster = clusters2016))

# Save workspace
save.image(paste(path, 'analysis2016.RData', sep = ''))

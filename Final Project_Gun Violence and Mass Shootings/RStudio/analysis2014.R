####################################################################
####################### DATA ANALYSIS - 2014 ####################### 
####################################################################

# The following steps are performed on the 2014 database:
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
data2014 <- read.csv(paste(path, 'zip_2014_predictive.csv', sep = ''), 
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
data2014_mean <- fill_in_values(data2014, mean)[, 4:95]

# Find correlation matrix
cMatrix2014 <- cor(data2014_mean[-92])
highlyC2014 <- findCorrelation(cMatrix2014, cutoff = 0.75, exact = TRUE)
data2014_clean <- data2014_mean[, -highlyC2014]

# Fit Learning Vector Quantization (LVQ) model using repeated cross validation
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
model2014 <- train(VIOLENT. ~ ., 
                   data = data2014_clean, 
                   method = "lvq", 
                   preProcess = "scale", 
                   trControl = control)
importance2014 <- varImp(model2014, scale = FALSE)

# Save workspace
save.image(paste(path, 'ranking2014.RData', sep = ''))

####################################################################
#                       2. FEATURE SELECTION                       #
####################################################################

# Approx. execution time in HPC Prince Cluster: 13 hours

# Load workspace from feature ranking
load(paste(path, 'ranking2014.RData', sep = ''))

# Run Recursive Feature Elimination (RFE) with a Random Forest selection function
control_rfe <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
vf2014 <- dim(data2014_clean)[2]
results2014 <- rfe(data2014_clean[, 1:vf2014 - 1], 
                   data2014_clean[, vf2014], 
                   sizes = c(1:vf2014 - 1), 
                   rfeControl = control_rfe)
predictors2014 <- predictors(results2014)

# Save workspace
save.image(paste(path, 'selection2014.RData', sep = ''))

####################################################################
#                    3. HIERARCHICAL CLUSTERING                    #
####################################################################

# Approx. execution time in HPC Prince Cluster: 20 hours

# Load workspace from feature selection
load(paste(path, 'ranking2014.RData', sep = ''))
load(paste(path, 'selection2014.RData', sep = ''))

# Run Agnes (Agglomerative Nesting) hierarchical clustering algorithm
data2014_predictors <- scale(data2014_clean[, predictors2014])
agnes2014 <- agnes(data2014_predictors, method = "complete")

# Save workspace
save.image(paste(path, 'clustering2014.RData', sep = ''))

####################################################################
#                         4. VISUALIZATION                         #
####################################################################

# Load workspace from clustering
load(paste(path, 'ranking2014.RData', sep = ''))
load(paste(path, 'selection2014.RData', sep = ''))
load(paste(path, 'clustering2014.RData', sep = ''))

# Confusion matrix
get_golden <- function(x) x$VIOLENT.
test_indices_2014 <- createDataPartition(data2014$VIOLENT., p = 0.3)[[1]]
test_data2014 <- data2014_clean[test_indices_2014, ]
cm2014 <- confusionMatrix(predict(model2014, newdata=test_data2014), 
                get_golden(test_data2014))
print(cm2014)
print(cm2014$overall['Accuracy'])
print(cm2014$byClass['F1'])

# Print highly correlated attributes
print(colnames(data2014_mean)[highlyC2014])

# Summarize features importance
print(importance2014)

# Plot features importance
plot(importance2014, top = 20)

# Summarize feature selection results
print(results2014)

# plot feature selection results
plot(results2014, type=c("g", "o"))

# List the chosen features
print(predictors2014)

# Plot Agnes dendrogram
pltree(agnes2014, cex = 0.6, hang = -1, main = "Dendrogram 2014")

# Cut Agnes tree into specific number of clusters
k2014 <- 4
clusters2014 <- cutree(as.hclust(agnes2014), k = k2014)

# Append Cluster column to original data
data2014_clusters <- data2014_clean
data2014_clusters$Cluster <- as.data.frame(clusters2014)$clusters2014

# Relationship cluster-label
cluster_label_2014 = matrix(nrow = k2014, ncol = 2,  dimnames = list(NULL, c("0", "1")))
for (k in 1:k2014){
  cluster_label_2014[k, 1] <- length(which(
    data2014_clusters[, ncol(data2014_clusters) - 1] == 0 &
      data2014_clusters[, ncol(data2014_clusters)] == k))
  cluster_label_2014[k, 2] <- length(which(
    data2014_clusters[, ncol(data2014_clusters) - 1] == 1 &
      data2014_clusters[, ncol(data2014_clusters)] == k))
}
cluster_label_2014 <- as.data.frame(cluster_label_2014)
print(cluster_label_2014)

# Number of members in each cluster
table(clusters2014)

# Plot Agnes dendrogram with border around clusters
plot(as.hclust(agnes2014), cex = 0.6)
rect.hclust(as.hclust(agnes2014), k = k2014, border = 2:(k2014 + 1))

# Scatter plot of the final clusters
fviz_cluster(list(data = data2014_clean[, -ncol(data2014_clean)], 
                  cluster = clusters2014))

# Save workspace
save.image(paste(path, 'analysis2014.RData', sep = ''))

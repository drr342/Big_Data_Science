####################################################################
##################### DATA ANALYSIS - STANFORD ##################### 
####################################################################

# The following steps are performed on the Stanford database:
#   1. Fueature ranking.
#   2. Feature selection.
#   3. Hierarchical clustering (using only selected features).
#   4. Number of clusters optimization.
#   5. Visualization.

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

# Load the data
data_Stanford <- read.csv(paste(path, 'Stanford_MSA.csv', sep = ''),
                     na.strings = 'Unknown',
                     header = TRUE,
                     colClasses = c("LABEL" = "factor"))

# Make all features numeric
data_Stanford_numeric <- data_Stanford[, 3:ncol(data_Stanford)]
indx <- sapply(data_Stanford_numeric, is.factor)
indx[ncol(data_Stanford_numeric)] <- FALSE
data_Stanford_numeric[indx] <- lapply(data_Stanford_numeric[indx], 
                                      function(x) as.numeric(x))

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
data_Stanford_numeric <- fill_in_values(data_Stanford_numeric, mean)

# Find correlation matrix
cMatrix_Stanford <- cor(data_Stanford_numeric[-ncol(data_Stanford_numeric)])
highlyC_Stanford <- findCorrelation(cMatrix_Stanford, cutoff = 0.5, exact = TRUE)
data_Stanford_clean <- data_Stanford_numeric[, -highlyC_Stanford]

# Fit Learning Vector Quantization (LVQ) model using repeated cross validation
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
model_Stanford <- train(LABEL ~ ., 
                   data = data_Stanford_clean, 
                   method = "lvq", 
                   preProcess = "scale", 
                   trControl = control)
importance_Stanford <- varImp(model_Stanford, scale = FALSE)

# Generate test set and confusion matrix
get_golden <- function(x) x$LABEL
test_indices_Stanford <- createDataPartition(data_Stanford$LABEL, p = 0.3)[[1]]
test_data_Stanford <- data_Stanford_clean[test_indices_Stanford, ]
cm_Stanford <- confusionMatrix(predict(model_Stanford, newdata=test_data_Stanford), 
                               get_golden(test_data_Stanford))

####################################################################
#                       2. FEATURE SELECTION                       #
####################################################################

# Run Recursive Feature Elimination (RFE) with a Random Forest selection function
control_rfe <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
vf_Stanford <- dim(data_Stanford_clean)[2]
results_Stanford <- rfe(data_Stanford_clean[, 1:vf_Stanford - 1], 
                   data_Stanford_clean[, vf_Stanford], 
                   sizes = c(1:vf_Stanford - 1), 
                   rfeControl = control_rfe)
predictors_Stanford <- predictors(results_Stanford)

####################################################################
#                    3. HIERARCHICAL CLUSTERING                    #
####################################################################

# Run Agnes (Agglomerative Nesting) hierarchical clustering algorithm
data_Stanford_predictors <- scale(data_Stanford_clean[, predictors_Stanford])
agnes_Stanford <- agnes(data_Stanford_predictors, method = "complete")

# Cut Agnes tree into optimal number of clusters
k_Stanford <- 10
clusters_Stanford <- cutree(as.hclust(agnes_Stanford), k = k_Stanford)

# Append Cluster column to original data
data_Stanford_clusters <- data_Stanford_clean
data_Stanford_clusters$Cluster <- as.data.frame(clusters_Stanford)$clusters_Stanford

cluster_label_Stanford = matrix(nrow = k_Stanford, ncol = 2,  dimnames = list(NULL, c("0", "1")))
for (k in 1:k_Stanford){
  cluster_label_Stanford[k, 1] <- length(which(
          data_Stanford_clusters[, ncol(data_Stanford_clusters) - 1] == 0 &
          data_Stanford_clusters[, ncol(data_Stanford_clusters)] == k))
  cluster_label_Stanford[k, 2] <- length(which(
    data_Stanford_clusters[, ncol(data_Stanford_clusters) - 1] == 1 &
      data_Stanford_clusters[, ncol(data_Stanford_clusters)] == k))
}
cluster_label_Stanford <- as.data.frame(cluster_label_Stanford)

####################################################################
#                         4. VISUALIZATION                         #
####################################################################

# Print highly correlated attributes
print(colnames(data_Stanford_numeric)[highlyC_Stanford])

# Print confusion matrix
print(cm_Stanford)
print(cm_Stanford$overall['Accuracy'])
print(cm_Stanford$byClass['F1'])

# Summarize features importance
print(importance_Stanford)

# Plot features importance
plot(importance_Stanford, top = 20)

# Summarize feature selection results
print(results_Stanford)

# plot feature selection results
plot(results_Stanford, type=c("g", "o"))

# List the chosen features
print(predictors_Stanford)

# Plot Agnes dendrogram
pltree(agnes_Stanford, cex = 0.6, hang = -1, main = "Dendrogram 2014")

# Number of members in each cluster
table(clusters_Stanford)

# Relationship between clusters and labels
print(cluster_label_Stanford)

# Plot Agnes dendrogram with border around clusters
plot(as.hclust(agnes_Stanford), cex = 0.6)
rect.hclust(as.hclust(agnes_Stanford), k = k_Stanford, border = 2:(k_Stanford + 1))

# Scatter plot of the final clusters
fviz_cluster(list(data = data_Stanford_clean[, -ncol(data_Stanford_clean)], 
                  cluster = clusters_Stanford))
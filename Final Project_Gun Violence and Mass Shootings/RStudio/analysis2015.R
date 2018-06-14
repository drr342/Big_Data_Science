####################################################################
####################### DATA ANALYSIS - 2015 ####################### 
####################################################################

# The following steps are performed on the 2015 database:
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
data2015 <- read.csv(paste(path, 'zip_2015_predictive.csv', sep = ''), 
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
data2015_mean <- fill_in_values(data2015, mean)[, 4:95]

# Find correlation matrix
cMatrix2015 <- cor(data2015_mean[-92])
highlyC2015 <- findCorrelation(cMatrix2015, cutoff = 0.75, exact = TRUE)
data2015_clean <- data2015_mean[, -highlyC2015]

# Fit Learning Vector Quantization (LVQ) model using repeated cross validation
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
model2015 <- train(VIOLENT. ~ ., 
                   data = data2015_clean, 
                   method = "lvq", 
                   preProcess = "scale", 
                   trControl = control)
importance2015 <- varImp(model2015, scale = FALSE)

# Save workspace
save.image(paste(path, 'ranking2015.RData', sep = ''))

####################################################################
#                       2. FEATURE SELECTION                       #
####################################################################

# Approx. execution time in HPC Prince Cluster: 13 hours

# Load workspace from feature ranking
load(paste(path, 'ranking2015.RData', sep = ''))

# Run Recursive Feature Elimination (RFE) with a Random Forest selection function
control_rfe <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
vf2015 <- dim(data2015_clean)[2]
results2015 <- rfe(data2015_clean[, 1:vf2015 - 1], 
                   data2015_clean[, vf2015], 
                   sizes = c(1:vf2015 - 1), 
                   rfeControl = control_rfe)
predictors2015 <- predictors(results2015)

# Save workspace
save.image(paste(path, 'selection2015.RData', sep = ''))

####################################################################
#                    3. HIERARCHICAL CLUSTERING                    #
####################################################################

# Approx. execution time in HPC Prince Cluster: 20 hours

# Load workspace from feature selection
load(paste(path, 'ranking2015.RData', sep = ''))
load(paste(path, 'selection2015.RData', sep = ''))

# Run Agnes (Agglomerative Nesting) hierarchical clustering algorithm
data2015_predictors <- scale(data2015_clean[, predictors2015])
agnes2015 <- agnes(data2015_predictors, method = "complete")

# Save workspace
save.image(paste(path, 'clustering2015.RData', sep = ''))

####################################################################
#                         4. VISUALIZATION                         #
####################################################################

# Load workspace from clustering
load(paste(path, 'ranking2015.RData', sep = ''))
load(paste(path, 'selection2015.RData', sep = ''))
load(paste(path, 'clustering2015.RData', sep = ''))

# Confusion matrix
get_golden <- function(x) x$VIOLENT.
test_indices_2015 <- createDataPartition(data2015$VIOLENT., p = 0.3)[[1]]
test_data2015 <- data2015_clean[test_indices_2015, ]
cm2015 <- confusionMatrix(predict(model2015, newdata=test_data2015), 
                          get_golden(test_data2015))
print(cm2015)
print(cm2015$overall['Accuracy'])
print(cm2015$byClass['F1'])

# Print highly correlated attributes
print(colnames(data2015_mean)[highlyC2015])

# Summarize features importance
print(importance2015)

# Plot features importance
plot(importance2015, top = 20)

# Summarize feature selection results
print(results2015)

# plot feature selection results
plot(results2015, type=c("g", "o"))

# List the chosen features
print(predictors2015)

# Plot Agnes dendrogram
pltree(agnes2015, cex = 0.6, hang = -1, main = "Dendrogram 2015")

# Cut Agnes tree into specific number of clusters
k2015 <- 4
clusters2015 <- cutree(as.hclust(agnes2015), k = k2015)

# Append Cluster column to original data
data2015_clusters <- data2015_clean
data2015_clusters$Cluster <- as.data.frame(clusters2015)$clusters2015

# Relationship cluster-label
cluster_label_2015 = matrix(nrow = k2015, ncol = 2,  dimnames = list(NULL, c("0", "1")))
for (k in 1:k2015){
  cluster_label_2015[k, 1] <- length(which(
    data2015_clusters[, ncol(data2015_clusters) - 1] == 0 &
      data2015_clusters[, ncol(data2015_clusters)] == k))
  cluster_label_2015[k, 2] <- length(which(
    data2015_clusters[, ncol(data2015_clusters) - 1] == 1 &
      data2015_clusters[, ncol(data2015_clusters)] == k))
}
cluster_label_2015 <- as.data.frame(cluster_label_2015)
print(cluster_label_2015)

# Number of members in each cluster
table(clusters2015)

# Plot Agnes dendrogram with border around clusters
plot(as.hclust(agnes2015), cex = 0.6)
rect.hclust(as.hclust(agnes2015), k = k2015, border = 2:(k2015 + 1))

# Scatter plot of the final clusters
fviz_cluster(list(data = data2015_clean[, -ncol(data2015_clean)], 
                  cluster = clusters2015))

# Save workspace
save.image(paste(path, 'analysis2015.RData', sep = ''))

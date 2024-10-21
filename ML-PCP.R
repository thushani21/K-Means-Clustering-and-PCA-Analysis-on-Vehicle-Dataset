library(readxl)
library(openxlsx)
library(NbClust)
library(factoextra)
library(cluster)
library(purrr)
library(fpc)

#Importing the file
vehicles <- read_excel("/Users/thushanivasanthan/Downloads/Machine CW/Machine final/vehicles.xlsx")

vehicles %>% head()                     #returns the first 6 rows
vehicles %>% colnames()                 #all variable names
vehicles %>% summary()                  #minimum, maximum, median, mean, 1st quartile and 3rd quartile
vehicles[length(vehicles)] %>% unique() #classes

#removing the columns
#1st column
removeSample <- vehicles[,-1]           
colnames(removeSample)

#Last column
vehicles_updated <- removeSample[,-19]  

#all variable names
colnames(vehicles_updated)  

head(vehicles_updated)
boxplot(vehicles_updated,  main = "BOX PLOT after first and last column removed", xlab = "Attributes", ylab = "Data")

#Finding outliers
oldpar = par(mfrow = c(3,5))    # par() used for multiple graphs
for (i in 1:18 ) {
  boxplot(vehicles_updated[[i]])
  mtext(names(vehicles_updated)[i], 
        cex = 0.8, 
        side = 1, 
        line = 2)
}
par(oldpar) 

# Function to detect outliers
outliers <- function(x) {
  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  iqr = Q3-Q1
  upper_limit = Q3 + (iqr*1.5)
  lower_limit = Q1 - (iqr*1.5)
  x > upper_limit | x < lower_limit
}

# Function to remove outliers
remove_outliers <- function(df, cols = names(df)) {
  for (col in cols) {
    df <- df[!outliers(df[[col]]),]
  }
  df
}

column_names <- names(vehicles_updated)
vehicles_outliers_removed <- remove_outliers(vehicles_updated, c(column_names))
boxplot(vehicles_outliers_removed, main="After Removing Outliers")

summary(vehicles_outliers_removed)
nrow(vehicles_updated)
nrow(vehicles_outliers_removed)

# Scaling Data
vehicles_outliers_removed_copy = as.data.frame(vehicles_outliers_removed)

vehicles_scaled <- scale(vehicles_outliers_removed_copy)
vehicles_scaled_df <- as.data.frame(vehicles_scaled)
boxplot(vehicles_scaled_df, main="Scaled Data")

## NbClust Method
set.seed(1234)
vehicles_nbc <- NbClust(vehicles_scaled_df, min.nc = 2, method = "kmeans")
table(vehicles_nbc$Best.n[1,])

## Elbow Method
fviz_nbclust(vehicles_scaled_df, kmeans, method = "wss")

## Gap Statistics Method
set.seed(123)
vehicles_gap_stat <- clusGap(
  vehicles_scaled_df, 
  FUN = kmeans, 
  nstart = 25, 
  K.max = 15, 
  B = 50
)
fviz_gap_stat(vehicles_gap_stat)

## Silhouette Method
fviz_nbclust(vehicles_scaled_df, kmeans, method = "silhouette")

# Function to generate cluster plots
generate_cluster_plot <- function (cluster_name, scaled_data, title = "") {
  fviz_cluster(cluster_name, scaled_data, geom = c("point"), main = title)
}

#internal evaluations on each k
kmeans_analysis <- function(km) {
  cat("Ratio of BSS over TSS =" ,km$betweenss/km$totss, "\n")
  cat("Between Cluster Sums of Squares =" , km$betweenss, "\n")
  cat("Within Cluster Sums of Squares =" , sum(km$withinss), "\n")
}

# Analysis for k = 2
vehicles_k_2 <- kmeans(vehicles_scaled_df, centers = 2, nstart = 25)
generate_cluster_plot(vehicles_k_2, vehicles_scaled_df, "Cluster Plot for k=2")
kmeans_analysis(vehicles_k_2)

# Analysis for k = 3
vehicles_k_3 <- kmeans(vehicles_scaled_df, centers = 3, nstart = 25)
generate_cluster_plot(vehicles_k_3, vehicles_scaled_df, "Cluster Plot for k=3")
kmeans_analysis(vehicles_k_3)

# Analysis for k = 4
vehicles_k_4 <- kmeans(vehicles_scaled_df, centers = 4, nstart = 25)
generate_cluster_plot(vehicles_k_4, vehicles_scaled_df, "Cluster Plot for k=4")
kmeans_analysis(vehicles_k_4)


# Silhouette Plots
# Function to generate silhouette plots
generate_silhouette_plot <- function(cluster_name, scaled_data){
  num_clusters <- cluster_name$cluster
  silhouette_data <- silhouette(num_clusters, dist(scaled_data))
  fviz_silhouette(silhouette_data)
}

# Silhouette Plot - k = 2
generate_silhouette_plot(vehicles_k_2, vehicles_scaled_df)

# Silhouette Plot - k = 3
generate_silhouette_plot(vehicles_k_3, vehicles_scaled_df)

# Silhouette Plot - k = 4
generate_silhouette_plot(vehicles_k_4, vehicles_scaled_df)


#Done


# Perform PCA Analysis on the Scaled Data
vehicles_pca <- prcomp(vehicles_scaled_df, centre = TRUE, scale. = TRUE)

# Display eigenvalues and eigenvectors
vehicles_pca
summary(vehicles_pca)

# Calculate the cumulative score per PC
cumulative_score <- cumsum(vehicles_pca$sdev^2 / sum(vehicles_pca$sdev^2))
plot(cumulative_score, type = "b", xlab = "Principle Components", ylab = "Cumulative Score")

# Choosing PCs that provide at least cumulative score > 92%.
selected_pcs <- which(cumulative_score > 0.92)
vehicles_pca_df <- predict(vehicles_pca, newdata = vehicles_scaled_df)[, selected_pcs]
vehicles_pca_df_updated = as.data.frame(vehicles_pca_df)

# Determining the "new" Number of Clusters
# NbClust Method
set.seed(1234)

vehicles_pca_nbc <- NbClust(
  vehicles_pca_df_updated, 
  min.nc = 2, 
  method = "kmeans"
)
table(vehicles_nbc$Best.n[1,])

## Elbow Method
fviz_nbclust(vehicles_pca_df_updated, kmeans, method = "wss")

## Gap Statistics Method
set.seed(123)
vehicles_pca_gap_stat <- clusGap(
  vehicles_pca_df_updated, 
  FUN = kmeans, 
  nstart = 25, 
  K.max = 15, 
  B = 50
)
fviz_gap_stat(vehicles_pca_gap_stat)

## Silhouette Method
fviz_nbclust(vehicles_pca_df_updated, kmeans, method = "silhouette")

# K-Means Analysis - PCA
#Analysis - k = 1
vehicles_pca_k_1 <- kmeans(vehicles_pca_df_updated, centers = 1, nstart = 25)
generate_cluster_plot(vehicles_pca_k_1, vehicles_pca_df_updated, "Cluster Plot for k=1")

#Evaluation Metrics for k = 1
kmeans_analysis(vehicles_pca_k_1)

# Analysis - k = 2
vehicles_pca_k_2 <- kmeans(vehicles_pca_df_updated, centers = 2, nstart = 25)
generate_cluster_plot(vehicles_pca_k_2, vehicles_pca_df_updated, "Cluster Plot for k=2")

#Evaluation Metrics for k = 2
kmeans_analysis(vehicles_pca_k_2)

# Analysis - k = 3
vehicles_pca_k_3 <- kmeans(vehicles_pca_df_updated, centers = 3, nstart = 25)
generate_cluster_plot(vehicles_pca_k_3, vehicles_pca_df_updated, "Cluster Plot for k=3")

#Evaluation Metrics for k = 3
kmeans_analysis(vehicles_pca_k_3)

# Silhouette Plots - PCA
## Silhouette Plot k = 2
generate_silhouette_plot(vehicles_pca_k_2, vehicles_pca_df_updated)

## Silhouette Plot k = 3
generate_silhouette_plot(vehicles_pca_k_3, vehicles_pca_df_updated)

# Calinski-Harabasz Index
set.seed(23)
num_clusters <- vehicles_pca_k_2$cluster
calinhara(vehicles_pca_df_updated, num_clusters)

set.seed(23)
num_clusters <- vehicles_pca_k_3$cluster
calinhara(vehicles_pca_df_updated, num_clusters)






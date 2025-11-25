setwd("/cloud/project")

# Load data, standardize the 3 clustering variables, and run fviz_nbclust

# Load packages
#install.packages(c("factoextra","ggplot2","dplyr","readr")) # uncomment if needed
library(factoextra)
library(ggplot2)
library(dplyr)
library(readr)

Mall_Costumers <- read_csv("Mall_Customers_extended.csv")

# Identify the continuous variables for clustering: Age, Annual Income, Spending Score
# If your columns have exactly those names, the code will use them; otherwise it tries to find close matches.
cont_default <- c("Age", "Annual Income", "Spending Score")
cols <- names(Mall_Costumers)
found <- intersect(cont_default, cols)

if(length(found) < 3){
  age_col       <- cols[grepl("^Age$|age", cols, ignore.case=TRUE)][1]
  income_col    <- cols[grepl("Annual|Income", cols, ignore.case=TRUE)][1]
  spending_col  <- cols[grepl("Spending|Score", cols, ignore.case=TRUE)][1]
  cont_vars <- unique(c(age_col, income_col, spending_col))
} else {
  cont_vars <- cont_default
}
cont_vars <- cont_vars[1:3]  # ensure exactly 3
cat("Using continuous variables:\n"); print(cont_vars)

#Create numeric subset and standardize (z-score)
subset_df <- Mall_Costumers[, cont_vars]
subset_df <- data.frame(lapply(subset_df, function(x) as.numeric(as.character(x))))
rownames(subset_df) <- rownames(Mall_Costumers)

std_subset <- scale(subset_df)  # standardized matrix
head(std_subset)

# Use fviz_nbclust to pick k
# Elbow method (wss)
p1 <- fviz_nbclust(std_subset, kmeans, method = "wss") + ggtitle("Elbow method (wss)")


# Silhouette method
p2 <- fviz_nbclust(std_subset, kmeans, method = "silhouette") + ggtitle("Average silhouette")

#this section belongs to cass

k_choice <- 3   

set.seed(123)  # ensures same results every time (reproducible)
km.res <- kmeans(std_subset, centers = k_choice, nstart = 25)

# km.res now contains cluster assignments in km.res$cluster
#combine (cbind) the standardized data + cluster numbers


viz_df <- as.data.frame(std_subset)
viz_df$cluster <- factor(km.res$cluster)

# Create visualization dataset WITHOUT the cluster column
viz_df <- as.data.frame(std_subset)

# Plot
fviz_cluster(
  km.res,
  data = viz_df,        # numeric only!
  palette = rainbow(k_choice),
  ellipse.type = "euclid",
  star.plot = TRUE,
  repel = TRUE,
  ggtheme = theme_minimal()
) + 
  ggtitle(paste("K-Means Clusters (k =", k_choice, ")"))

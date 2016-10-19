---
  title: "Assignment 3: K Means Clustering"
---
  
####Assignment 5.rmd
### by Jiaxi Li

###install.packages("ggplot2") 
library(dplyr)
library(tidyr)
library(ggplot2)

### upload the file "Class_Motivation.csv" from the Assignment 3 Repository as a data frame called "K1""
K1 <- read.table("Class_Motivation.csv",header = TRUE, sep = ",")

### Since algorithm will treat each row as a value belonging to a person, so remove the ID column
K2 <- dplyr::select(K1, 2:6)

### omit NA values
K3 <- na.omit(K2)


### standardize the values so that they have the same range

K3 <- scale(K3)

### run the K-means clustering algorithm 
###1) The algorithm starts by randomly choosing some starting values 
###2) Associates all observations near to those values with them
###3) Calculates the mean of those clusters of values
###4) Selects the observation closest to the mean of the cluster
###5) Re-associates all observations closest to this observation
###6) Continues this process until the clusters are no longer changing

### kmeans with k3 dataset, and automatically have 3 cluster for grouping
fit <- kmeans(K3, 3) 

### See how each observations are divided into 3 clusters
fit$cluster

#Attach new column"cluster" to te original dataframe 
K4 <- data.frame(K3, fit$cluster)

#Change the names of the variables

names(K4) <- c("1", "2", "3", "4", "5", "cluster") 


###use tidyr to convert from wide to long format.

K5 <- tidyr::gather(K4, "week", "motivation", 1:5)### questions: why 1:5？



###use dplyr to average our motivation values by week and by cluster.


K6 <- K5 %>% group_by(week, cluster) #if dont have this step, then, the mean would be avg for all motivation no matter week number.
K6 <- summarise(K6, avg = mean(motivation))


###visualization

K6$week <- as.numeric(K6$week)

K6$cluster <- as.factor(K6$cluster)


###line plot
ggplot(K6, aes(week, avg, colour = cluster)) + geom_line() + xlab("Week") + ylab("Average Motivation")

###
K7 <- dplyr::count(K4, cluster)


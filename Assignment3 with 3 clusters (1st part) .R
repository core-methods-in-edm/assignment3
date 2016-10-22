---
  title: "Assignment 3: K Means Clustering"
---

  ### by Jiaxi Li

###install.packages("ggplot2") 
library(dplyr)
library(tidyr)
library(ggplot2)

### upload the file "Class_Motivation.csv" from the Assignment 3 Repository as a data frame called "K1""
P1 <- read.table("Class_Motivation.csv",header = TRUE, sep = ",")

### Since algorithm will treat each row as a value belonging to a person, so remove the ID column
P2 <- dplyr::select(P1, 2:6)

### omit NA values
P3 <- na.omit(P2)


### standardize the values so that they have the same range

P3 <- scale(P3)

### run the K-means clustering algorithm 
###1) The algorithm starts by randomly choosing some starting values 
###2) Associates all observations near to those values with them
###3) Calculates the mean of those clusters of values
###4) Selects the observation closest to the mean of the cluster
###5) Re-associates all observations closest to this observation
###6) Continues this process until the clusters are no longer changing

### kmeans with k3 dataset, and automatically have 2 cluster groups
fit <- kmeans(P3, 3) 

### See how each observations are divided into 2 clusters
fit$cluster

#Attach new column"cluster" to te original dataframe 
P4 <- data.frame(P3, fit$cluster)

#Change the names of the variables

names(P4) <- c("1", "2", "3", "4", "5", "cluster") 


###use tidyr to convert from wide to long format.

P5 <- tidyr::gather(P4, "week", "motivation", 1:5)### questions: why 1:5？



###use dplyr to average our motivation values by week and by cluster.


P6 <- P5 %>% group_by(week, cluster) #if dont have this step, then, the mean would be avg for all motivation no matter week number.
P6 <- summarise(P6, avg = mean(motivation))


###visualization

P6$week <- as.numeric(P6$week)

P6$cluster <- as.factor(P6$cluster)


###line plot
ggplot(P6, aes(week, avg, colour = cluster)) + geom_line() + xlab("Week") + ylab("Average Motivation")

###
P7 <- dplyr::count(P4, cluster)


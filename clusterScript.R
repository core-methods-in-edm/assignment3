library(dplyr)
library(tidyr)
library(ggplot2)
library(fpc)
library(cluster) 

#demo from prof.Lang
K1 <- read.table("Class_Motivation.csv", sep = ",", header = TRUE)
K2 <- dplyr::select(K1, 2:6)
K3 <- na.omit(K2)
K3 <- scale(K3)
fit <- kmeans(K3, 3) 
fit$cluster
K4 <- data.frame(K3, fit$cluster)
names(K4) <- c("1", "2", "3", "4", "5", "cluster") 
K5 <- tidyr::gather(K4, "week", "motivation", 1:5)
K6 <- K5 %>% group_by(week, cluster)
K6 <- summarise(K6, avg = mean(motivation))
K6$week <- as.numeric(K6$week)
K6$cluster <- as.factor(K6$cluster)
ggplot(K6, aes(week, avg, colour = cluster)) + geom_line() + xlab("Week") + ylab("Average Motivation")
K7 <- dplyr::count(K4, cluster)

#Assignment3
#mannually input latitude and longitude, mannually change title of column
T1 <- read.table("cluster-class-data.csv", sep = ",", header = TRUE)
T2 <- dplyr::select(T1, latitude,longitude.)
T3 <- na.omit(T2)
T3 <- scale(T3)
fit1 <- kmeans(T3, 3) 
fit1$cluster
T4 <- data.frame(T3, fit1$cluster)
names(T4) <- c("latitude", "longitude","cluster") 
T4$cluster <- as.factor(T4$cluster)

#reconstruct T5, join cluster info into T1
T1$ID<-seq.int(nrow(T1))
T4$ID<-seq.int(nrow(T4))
T5 <- dplyr::full_join(T1, T4,by="ID")

ggplot(T5, aes(longitude., latitude.x, colour = cluster)) + geom_line() + xlab("longitude") + ylab("latitude")
p <- ggplot(T5,aes(longitude., latitude.x))
p + geom_point(size=5,aes(colour=cluster))+theme(legend.position='top')
#two major groups in class: North America and Asia. 1 student from South America.

#lets look at whether sibling and class number are related
T6 <- T5 %>% group_by(sibling, cluster)
T6 <- summarise(T6, avg = mean(classes))
T6$cluster <- as.factor(T6$cluster)
ggplot(T6, aes(sibling, avg, colour = cluster)) + geom_line() + xlab("sibling number") + ylab("Average taking class number")
#it seems both cluster patterns (1 and 3) are similar: the students with more sibling tends to take more classes 
#(possible hypothesis: The kids grow up in bigger family tend to handel stress better? and this is cross cultural.)

#lets look at whether home from TC distance and class number are related
T7 <- T5 %>% group_by(classes, cluster)
T7 <- summarise(T7, avg = mean(travelStates))
T7$cluster <- as.factor(T7$cluster)
ggplot(T7, aes(classes, avg, colour = cluster)) + geom_line() + xlab("class number") + ylab("travel state numbers")
# the student who take less classes (=1&2) would travel least (most of them are not full time students, maybe they are working?)
#the student who take the most classes (=4) would travel less than those taking 3 classes (maybe full time students do not have time to travel?)
 
#construct a new cluster based on sibling and classes info
T8 <- dplyr::select(T1, sibling,classes)
fit2 <- kmeans(T8, 2) 
fit2$cluster
T9 <- data.frame(T8, fit2$cluster)
names(T9) <- c("sibling", "classes", "cluster2") 
T10 <- T9 %>% group_by(sibling, cluster2)
T11 <- summarise(T10, avg = mean(classes))
T11$sibling <- as.numeric(T11$sibling)
T11$cluster2 <- as.factor(T11$cluster2)
ggplot(T11, aes(sibling, avg, colour = cluster2)) + geom_line() + xlab("sibling") + ylab("average classes number taking")

#in order to compare clusters, we have to construct distance matrix, using Ward Hierarchical Clustering method
d <- dist(T1, method = "euclidean")
fit <- hclust(d, method="ward.D") 
# comparing 2 cluster solutions
cluster.stats(d, fit1$cluster, fit2$cluster)

#plot 2 clusters together
T9$ID<-seq.int(nrow(T9))
T12 <- dplyr::full_join(T5, T9,by="ID")
T12$cluster2 <- as.factor(T12$cluster2)
p <- ggplot(T12,aes(longitude., latitude.x))
p + geom_point(size=5,aes(colour=cluster,shape=cluster2))+theme(legend.position='top')

#try cat
p + geom_point(size=5,aes(colour=cluster,shape=cat))+theme(legend.position='top')




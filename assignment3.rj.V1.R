library(dplyr)
library(tidyr)
library(ggplot2)


K1 <- read.csv("Class_Motivation.csv")

## we will need to manipulate the data frame into a structure that can be analyzed by our clustering algorithm.

### The algorithm will treat each row as a value belonging to a person, so we need to remove the id variable.

K2 <- dplyr::select(K1, 2:6)

### It is important to think about the meaning of missing values when clustering. We could treat them as having meaning or we could remove those people who have them. Neither option is ideal. 
### What problems do you foresee if we recode or remove these values? Write your answers below:
  
### Removing values can lead to an inaccurate representation of the folks we're clustering.  It sckews the results

### We will remove people with missing values for this assignment, but keep in mind the issues that you have identified.

K3 <- na.omit(K2) #This command create a data frame with only those people with no missing values. It "omits" all rows with missing values, also known as a "listwise deletion". EG - It runs down the list deleting rows as it goes.
 
### this is how you standardize your dataset
K3 <- scale(K3)
### It is impossible to visualize 5 variables, the human meat brain maxes out at 3

fit <- kmeans(K3, 2) 
#We have created an object called "fit" that contains all the details of our clustering including which observations belong to each cluster.

#We can access the list of clusters by typing "fit$cluster", the top row corresponds to the original order the rows were in. Notice we have deleted some rows.

fit$cluster
#We can also attach these clusters to te original dataframe by using the "data.frame" command to create a new data frame called K4.

K4 <- data.frame(K3, fit$cluster)
#Have a look at the K4 dataframe. Lets change the names of the variables to make it more convenient with the names() command.

names(K4) <- c("1", "2", "3", "4", "5", "cluster") #c() stands for concatonate and it creates a vector of anything, in this case a vector of names.

### Visualizing the Cluster

## creatve visual for average motivation by cluster, by week.
## To do this we will need to convert our data from wide to long format. Remember your old friends tidyr and dplyr!

K5 <- tidyr::gather(K4, "week", "motivation", 1:5)

### Now lets use dplyr to average our motivation values by week and by cluster.

K6 <- K5 %>% group_by(week, cluster)
K6 <- summarise(K6, avg = mean(motivation))


### Week is designated by "chr". To convert it to numeric, we use the as.numeric command.

## ince "cluster" is not numeric but rather a categorical label we want to convert it from an "integer" format to a "factor" format so that ggplot does not treat it as a number. We can do this with the as.factor() command.

K6$week <- as.numeric(K6$week)

K6$cluster <- as.factor(K6$cluster)

### Now we can plot our line plot using the ggplot command, "ggplot()".

## The first argument in a ggplot is the dataframe we are using: K6
## Next is what is called an aesthetic (aes), the aesthetic tells ggplot which variables to use and how to use them. Here we are using the variables "week" and "avg" on the x and y axes and we are going color these variables using the "cluster" variable
## Then we are going to tell ggplot which type of plot we want to use by specifiying a "geom()", in this case a line plot: geom_line()
## Finally we are going to clean up our axes labels: xlab("Week") & ylab("Average Motivation")

ggplot(K6, aes(week, avg, colour = cluster)) + geom_line() + xlab("Week") + ylab("Average Motivation")

### What patterns do you see in the plot?

###  The two clusters appear to inverse of one ather but over time they meet a low point of 0 motivation and then switch.  This could have somethign to do with remove those values early on?

### how many people are in each cluster

K7 <- dplyr::count(K4, cluster)

fit2 <- kmeans(K3, 3)
fit2$cluster

J4 <- data.frame(K3, fit2$cluster)
names(J4) <- c("1", "2", "3", "4", "5", "cluster")

J5 <- tidyr::gather(J4, "week", "motivation", 1:5)

J6 <- J5 %>% group_by(week, cluster)
J6 <- summarise(J6, avg = mean(motivation))

J6$week <- as.numeric(J6$week)

J6$cluster <- as.factor(J6$cluster)

ggplot(J6, aes(week, avg, colour = cluster)) + geom_line() + xlab("Week") + ylab("Average Motivation")

J7 <- dplyr::count(J4, cluster)

### Look at the number of people in each cluster, now repeat this process for 3 rather than 2 clusters. Which cluster grouping do you think is more informative? 
### Write your answer below:

  ### I believe that the second set is more informative.  It gives you a lot more information about the crowd that was less motivated early on.  we see that there are two clusters of less motivated people.  Those that remain less motiveated through out and those who improve over time.  
  ### That being said the highly motitived people still continue on the same downward slope overtime.  


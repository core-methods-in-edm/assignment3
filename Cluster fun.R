--
  title: "Assignment 3: K Means Clustering"
---
  
  In this assignment we will be applying the K-means clustering algorithm we looked at in class. At the following link you can find a description of K-means, create a webpage index in Zotero and add some notes about the use and limitations of K-means clustering:
  
  https://www.cs.uic.edu/~wilkinson/Applets/cluster.html

You will need three packages to complete this assignment: "dplyr", "tidyr" and a new package called "ggplot2". Install ggplot2 and then load all three packages.

```{r}

install.packages("ggplot2") #If this doesn't work, use the "Packages" tab to install the package.

library(dplyr)
library(tidyr)
library(ggplot2)

```

Now, upload the file "Class_Motivation.csv" from the Assignment 3 Repository as a data frame called "K1""
```{r}

K1 <- read.table("~Class_motivation.csv", sep = ",", header =  TRUE)

```

D1 <- read.table("~/clusters/clusters.Rproj", sep =",", header = TRUE)
> > D1 <- read.table("online-tutor.csv", sep =",", header = TRUE)


K1<-"Users/Ben/Documents/Assignment 3.Rproj/Class_Motivation.csv"

K1 <- read.table("Class_Motivation.csv", sep = ",", header =  TRUE)




This file contains the self-reported motivation scores for a class over five weeks. We are going to look for patterns in motivation over this time and sort people into clusters based on those patterns.

But before we do that, we will need to manipulate the data frame into a structure that can be analyzed by our clustering algorithm.

The algorithm will treat each row as a value belonging to a person, so we need to remove the id variable.

```{r}

K2 <- dplyr::select(K1, 2:6)





```

It is important to think about the meaning of missing values when clustering. We could treat them as having meaning or we could remove those people who have them. Neither option is ideal. What problems do you foresee if we recode or remove these values? Write your answers below:

## I think that removing the values will be problematic because it ruins the equality of the variables you are examining. Individual's with fewer responses are being compared to individuals who did respond to every question. Depending on how the data is clustered, certain individual's may be innacurately clustered because of their differences which are no longer accounted for by the data. 

## If you recode the data then you have the problem of giving values to people who failed to respond. Because you are assigning the value yourself, it is likely that you mispresent their level of motivation. Clustering once again is problematic, not because differences are not accounted for by the data, but because the data misrepresents the variables at hand (individuals)


We will remove people with missing values for this assignment, but keep in mind the issues that you have identified.


```{r}

K3 <- na.omit(K2) 

#This command create a data frame with only those people with no missing values. It "omits" all rows with missing values, also known as a "listwise deletion". EG - It runs down the list deleting rows as it goes.

```

Another pre-processing step used in K-means is to standardize the values so that they have the same range. We do this because we want to treat each week as equally important - if we do not standardise then the week with the largest range will have the greatest impact on which clusters are formed. We standardise the values by using the "scales()" command.

```{r}

K3 <- scale(K3)

```{r}

```

```


Now we will run the K-means clustering algorithm we talked about in class. 
1) The algorithm starts by randomly choosing some starting values 
2) Associates all observations near to those values with them
3) Calculates the mean of those clusters of values
4) Selects the observation closest to the mean of the cluster
5) Re-associates all observations closest to this observation
6) Continues this process until the clusters are no longer changing

Notice that in this case we have 5 variables and in class we only had 2. It is impossible to visualise this process with 5 variables.

Also, we need to choose the number of clusters we think are in the data. We will start with 2.

View(K1)
View(K2)
View(K3)
```{r}

fit <- kmeans(K3, 2) 


#We have created an object called "fit" that contains all the details of our clustering including which observations belong to each cluster.

#We can access the list of clusters by typing "fit$cluster", the top row corresponds to the original order the rows were in. Notice we have deleted some rows.


fit2$cluster

fit$cluster

#We can also attach these clusters to te original dataframe by using the "data.frame" command to create a new data frame called K4.

K4 <- data.frame(K3, fit$cluster)

View(K4)

#Have a look at the K4 dataframe. Lets change the names of the variables to make it more convenient with the names() command.

names(K4) <- c("1", "2", "3", "4", "5", "cluster") 

#c() stands for concatonate and it creates a vector of anything, in this case a vector of names.

View(K4)

```

Now we need to visualize the clusters we have created. To do so we want to play with the structure of our data. What would be most useful would be if we could visualize average motivation by cluster, by week. To do this we will need to convert our data from wide to long format. Remember your old friends tidyr and dplyr!

First lets use tidyr to convert from wide to long format.
```{r}

K5 <- tidyr::gather(K4, "week", "motivation", 1:5)
View(K5)

```

Now lets use dplyr to average our motivation values by week and by cluster.

```{r}

K6 <- K5 %>% group_by(week, cluster)
K6 <- summarise(K6, avg = mean(motivation))

```

library(magrittr)

library(dplyr)

Now it's time to do some visualization! We are going to start using the ggplot2 package, a very popular visualization package in R. It is based on the "Grammar of Graphics" a theory of how visualizations work best. If you are interested, you can read more about it here: 

https://www.cs.uic.edu/~wilkinson/TheGrammarOfGraphics/GOG.html

And you can see the range of available graphics in ggplot here:

http://docs.ggplot2.org/current/

We are going to create a line plot similar to the one created in the school dropout paper we looked at in class (Bowers, 2010). It will have motivation on the Y-axis and weeks on the X-axis. To do this we will want our weeks variables to be treated as a number, but because it was created from a variable name it is currently being treated as a character variable. You can see this if you click on the arrow on the left of K6 in the Data pane. Week is designated by "chr". To convert it to numeric, we use the as.numeric command. 

Likewise, since "cluster" is not numeric but rather a categorical label we want to convert it from an "integer" format to a "factor" format so that ggplot does not treat it as a number. We can do this with the as.factor() command.

```{r}

K6$week <- as.numeric(K6$week)

K6$cluster <- as.factor(K6$cluster)

```

Now we can plot our line plot using the ggplot command, "ggplot()".

- The first argument in a ggplot is the dataframe we are using: K6
- Next is what is called an aesthetic (aes), the aesthetic tells ggplot which variables to use and how to use them. Here we are using the variables "week" and "avg" on the x and y axes and we are going color these variables using the "cluster" variable
- Then we are going to tell ggplot which type of plot we want to use by specifiying a "geom()", in this case a line plot: geom_line()
- Finally we are going to clean up our axes labels: xlab("Week") & ylab("Average Motivation")

```{r}

ggplot(K6, aes(week, avg, colour = cluster)) + geom_line() + xlab("Week") + ylab("Average Motivation")

```
library(ggplot2)
What patterns do you see in the plot?
## largest amount of variance in motivation on week 2 and week 4 

It would be useful to determine how many people are in each cluster. We can do this easily with dplyr.

```{r}
K7 <- dplyr::count(K4, cluster)
```

View(K7)
Look at the number of people in each cluster, now repeat this process for 3 rather than 2 clusters. Which cluster grouping do you think is more informative? Write your answer below:


fit2 <- kmeans(K3, 3) 

fit2$cluster

K8 <- data.frame(K3, fit2$cluster)

View(K8)


names(K8) <- c("1", "2", "3", "4", "5", "cluster")



K10 <- tidyr::gather(K8, "week", "motivation", 1:5)

View(K10)

K11 <- K10 %>% group_by(week, cluster)
K11 <- summarise(K11, avg = mean(motivation))



K11$week <- as.numeric(K11$week)

K11$cluster <- as.factor(K11$cluster)


ggplot(K11, aes(week, avg, colour = cluster)) + geom_line() + xlab("Week") + ylab("Average Motivation")




K11<-dplyr::count(K8, cluster)



View(K11)




K9 <- dplyr:: count(K8, fit2$cluster)


View(K9)


##In this case it is not easy to tell which clustering method was most effective. One strange thing about the clustering method with 3 was that they did not divide in roughly even ammounts. I thought that the clusters would end up relatively the same quantity but in this case the first 2 clusters only had 3 to 4 people and the second cluster captured the majority of the data with 16. Because there is a sharp difference between 1/2 and and 3 I feel that keeping it divided into 2 clusters is a better way to go. There was the sharpest variance in motivation per week in 1 which could support the idea of three clusters. Its so difficult to say. 


## Wow. My third graph looks a lot different from my second one. Did I do something different on accident? Or does the clustering with k-means have the potential to end up with a different result? 
Both my third and second graph were for 3 clusters while my first graph was for 2. In the case of the third graph it totally makes sense .
```


```{r}


Once you have done this, save both of your plots to the Assignment 5 file. Create a Zotero item to index your new computer program (Assignment 5.rmd) in Zotero. Then commit your assignment, push it to your Github account and then Pull request your version to the original assignment version so I can see it.

##Extension Exercise

Now, try to do the same for the data [collected in class](https://tccolumbia.qualtrics.com/SE/?SID=SV_6RRqlSAM6lZWYQt). Create two groups of clusters, the answers to the questions and regions where people grew up. Then create a visualization that shows the overlap between the two groups of clusters.


B1<-"Users/Ben/Documents/Assignment 3.Rproj/cluster-class-data.csv"


B1 <- read.table("cluster-class-data.csv", sep = ",", header =  TRUE)


View(B1)



Code Book:

Duration (in seconds)
Q1 - First Name
Q2 - Last Name
Q3 - Have you ever owned a cat?
Q4 - Do you pronounce "gif", with a J (j-iff) or a G (g-iff)?
Q5 - How many months have you lived in New York City?
Q6 - How many siblings (brothers/sisters) do you have?
Q7 - How many times do you play sport each week?
Q8 - How many miles do you travel from home to TC?
Q9 - Estimate how many of your friends own Android phones
Q10 - How many movies have you seen in the cinema this year?
Q11 - How many classes are you taking this semester?
Q12 - How many states have you visited in the US?
Q13 - What city/town did you grow up in?
Q14 - What state/province did you grow up in?
Q15 - What country did you grow up in?

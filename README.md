Assigment 3 
igraph requires data to be in a particular structure. There are several structures that it can use but we will be using a combination of an "edge list" and a "vertex list" in this assignment. As you might imagine the edge list contains a list of all the relationships between students and any characteristics of those edges that we might be interested in. There are two essential variables in the edge list a "from" variable and a "to" variable that describe the relationships between vertices. While the vertex list contains all the characteristics of those vertices, in our case gender and major.

 So let's convert our data into an edge list!

 @@ -28,17 +28,17 @@ library(dplyr)
 D2 <- select(D1, comment.to, comment.from) #select() chooses the columns
 ```

 Since our data represnts every time a student makes a comment there are multiple rows when the same student comments more than once on another student's video. We want to collapse these into a single row, with a variable that shows how many times a student-student pair appears.
 Since our data represents every time a student makes a comment there are multiple rows when the same student comments more than once on another student's video. We want to collapse these into a single row, with a variable that shows how many times a student-student pair appears.

 ```{r}

 EDGE <- count(D2, comment.to, comment.from)

 names(EDGE) <- c("from", "to", "count")
 names(EDGE) <- c("to", "from", "count")

 ```

 EDGE is your edge list. Now we need to make the vertex list, a list of all the students and their characteristics in our network. Because there are some students who only recieve comments and do not give any we will need to combine the comment.from and comment.to variables to produce a complete list.
 EDGE is your edge list. Now we need to make the vertex list, a list of all the students and their characteristics in our network. Because there are some students who only receive comments and do not give any we will need to combine the comment.from and comment.to variables to produce a complete list.

 ```{r}
 #First we will separate the commenters from our commentees
 @@ -77,7 +77,6 @@ Now we have both a Vertex and Edge list it is time to plot our graph!

 ```{r}
 #Load the igraph package

 library(igraph)

 #First we will make an object that contains the graph information using our two dataframes EDGE and VERTEX. Notice that we have made "directed = TRUE" - our graph is directed since comments are being given from one student to another.
 @@ -104,20 +103,106 @@ In Part II your task is to [look up](http://igraph.org/r/) in the igraph documen

 * Ensure that sizing allows for an unobstructed view of the network features (For example, the arrow size is smaller)
 * The vertices are colored according to major
 * The vertices are sized according to the number of comments they have recieved
 * The vertices are sized according to the number of comments they have received
 ```{r}
 V.TO.count <- select(V.TO, id)
 V.TO.count <- count(V.TO.count, id)

 plot(g,layout=layout.fruchterman.reingold, 
      vertex.color=VERTEX$major, vertex.label.cex = 0.8, vertex.size=(V.TO.count$n*5),
      edge.arrow.size = 0.5, edge.arrow.width = 0.5)
 ```

 ## Part III

 Now practice with data from our class. This data is real class data directly exported from Qualtrics and you will need to wrangle it into shape before you can work with it. Import it into R as a data frame and look at it carefully to identify problems.
 ```{r}
 library(dplyr)
 library(igraph)
 library(tidyr)
 library(stringr)

 DF1 <- read.csv("hudk4050-classes.csv", stringsAsFactors = FALSE, header = TRUE)
 DF2 <- DF1

 # Delete first two rows in DF1
 DF2 <- DF2[-c(1,2),]

 #Combine the first and last names
 DF2$name <- paste(DF2$Q8, DF2$Q9)
 DF2 <- DF2[-c(1,2)]
 DF2 <- subset(DF2, select = c(name, Q1:Q18))
 DF2$name <- as.factor(DF2$name)

 names(DF2) <- c("name", "class1", "class2", "class3", "class4", "class5", "class6", "interest")

 #Converting names to a consistent format

 #Remove unwanted characters 
 DF2$name <- str_replace (DF2$name, "`", "")
 #Make all names capitalized first letters only
 DF2$name <- str_to_title(DF2$name)
 DF2$name <- str_replace_all(DF2$name, "  ", " ")
 #Make all class letters capital letters only - choose columns 2-7
 DF2 <- DF2 %>% mutate_at (2:7, list(toupper))
 DF2 <- DF2 %>% mutate_at (2:7, str_replace_all, " ", "")

 ```

 Please create a **person-network** with the data set hudk4050-classes.csv. To create this network you will need to create a person-class matrix using the tidyr functions and then create a person-person matrix using `t()`. You will then need to plot a matrix rather than a to/from data frame using igraph.
 ```{r}
 #Create a dataframe with two variables 
 DF3 <- DF2 %>% gather(label, class, 2:7, na.rm = TRUE, convert = FALSE) %>% select(name, class)
 DF3$count <- 1

 #Remove blank classes and duplicates
 DF3 <- filter(DF3, class != "")
 DF3 <- unique(DF3)


 #Spread 1s across classes to create a student x class dataframe
 DF3 <- spread(DF3, class, count)
 rownames(DF3) <- DF3$name
 DF3 <- select(DF3, -name, -HUDK4050)

 DF3[is.na(DF3)] <- 0

 #Matrix

 DF4 <- as.matrix(DF3)
 #Create person-person matrix
 DF4 <- DF4 %*% t(DF4)


 ```

 Once you have done this, also [look up](http://igraph.org/r/) how to generate the following network metrics:

 * Betweeness centrality and dregree centrality. **Who is the most central person in the network according to these two metrics? Write a sentence or two that describes your interpretation of these metrics**

 * Color the nodes according to interest. Are there any clusters of interest that correspond to clusters in the network? Write a sentence or two describing your interpetation.

 ```{r}
 #Graphing

 g <- graph.adjacency(DF4, mode = "undirected", diag = FALSE)

 plot(g, layout=layout.fruchterman.reingold, vertex.size = 1, vertex.label.cex = 0.8, vertex.label.color = "black")
 ```

 ```{r}
 #Centrality

 #Degree Centrality
 sort(degree(g), decreasing = TRUE)
 #Eight people have the same degree centrality measure of 31, which means that they are directly connected to 31 other people. They are likely to hold the most information and are more likely to connect with the wider network. If one needs to reach out to people in other classes for information, these eight people are the best individuals to reach out to.

 #Betweenness Centrality
 sort(betweenness(g), decreasing = TRUE)
 #Yifei Zhang has the highest betweenness centrality with a measure of 260, which means that she is the person with the shortest path to other nodes in the network. She is the junction point and has more information passing through her than any other member in the group. Therefore, she is the most likely person to unify the network.
 ```


 ### To Submit Your Assignment

 Please submit your assignment by first "knitting" your RMarkdown document into an html file and then comit, push and pull request both the RMarkdown file and the html file.
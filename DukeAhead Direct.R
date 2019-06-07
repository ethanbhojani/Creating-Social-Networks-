ls()
rm(list=ls())

getwd()

setwd("C:/Users/ethan/Desktop/duke projects/1. duke md network with maria/trial 2/")
list.files()

#install package to read files.
#install.packages("readxl")
library("readxl")

data <- read_excel("SNA Dataset_Codebook_plusfield.xlsx", sheet = 2)

#filter out questions we want
question8 <- apply(cbind("Q8", 1:10, 1), 1, function(x)paste(x, collapse = "_"))

data8 <- data[, question8]

data8_trial <- data8
data8_trial[is.na(data8_trial)] <- 0
#delete first row
data8_3 <- data8_trial[-1, ]
#eliminated zeros, now create an empty matrix--- data8_trial is made up of characters, does that matter? attempt to convert to numerics
library(plyr)
library(dplyr)
data8_4 <- mutate_all(data8_3, function (x) as.numeric(x))
glimpse(data8_4)
str(data8_4)

###find combinations and create empty matrix

##emptry matrix-- how many people in the study?-- lets insert sheet 3 of data as the people

people <- read_excel("SNA Dataset_Codebook_plusfield.xlsx", sheet = 3)
#### looking at the data we can see there are some missing spots... or cells that say "<no one>". 
## i will just ignore them as removing them will mess up the numbering and if they do not exist no matches or pairs will be formed thus they will not be included in the network
adjacency <- matrix(0, nrow = nrow(people), ncol = nrow(people))

##empty matrix created. now to find combinations excluding "0"

##data8 5 has NA's, data 8 4 does not. 

## find combinations 
data8_4[1, ]
combn(data8_4[1,], 2, simplify = FALSE)
## subset few rows to experiment and create an I function
data8_4_trial <- data8_4[1:3, ]
data8_4_trial


as.data.frame(combn(data8_4[1,], 2, simplify = TRUE))

# now carry this out over every row
adding_pairs <- as.data.frame(matrix(0, nrow=2))

for (i in 1:nrow(data8_4)) {
  building_pairs <- as.data.frame(combn(data8_4[i,], 2, simplify =TRUE))
  adding_pairs <- cbind(adding_pairs, building_pairs)
}



edgelist1 <- as.data.frame(t(adding_pairs))
##fix row/col names

edgelist2 <- edgelist1
colnames(edgelist2) <- c("a", "b")
rownames(edgelist2) <- c(1:nrow(edgelist2))
##replace 0's with NAs

#install.packages("tidyr")
library("tidyr")
edgelist3 <- edgelist2
edgelist3[edgelist2 == 0] <- NA
#edgelist4 <- edgelist3
#edgelist4 <- edgelist3[complete.cases(edgelist3), ]
#error: Error in complete.cases(edgelist3) : invalid 'type' (list) of argument

##what is this... I see... it is a data frame storing  a list. I need to unlist. 
### The solution : data.frame(matrix(unlist(DF), nrow=nrow(DF)),stringsAsFactors=FALSE)


edgelist4 <- data.frame(matrix(unlist(edgelist3), nrow=nrow(edgelist3)),stringsAsFactors=FALSE)
edgelist5 <- edgelist4[complete.cases(edgelist4), ]

final_edgelist <- edgelist5
colnames(final_edgelist) <- c("a", "b")
rownames(final_edgelist) <- c(1:nrow(final_edgelist))

###bring in the names-- also attributes eventually, variable name - people
head(people)

##data cleaning complete... igraph time

#install.packages("igraph")
library("igraph")





##perhaps the final edge list has duplicate pairs.
## we must first order the edge list to find them

final_edgelist2 <- as.data.frame(t(apply(final_edgelist, 1, sort)))
final_edgelist3 <- final_edgelist2
colnames(final_edgelist3) <- c("a", "b")
rownames(final_edgelist3) <- c(1:nrow(final_edgelist3))

duplicated_edges <- final_edgelist2[which(duplicated(final_edgelist2)), ]

final_edgelist4 <- final_edgelist3[order(final_edgelist3[,1]), ]


##how many duplicate pairs

#ok now i finally have the matrix duplicated weights
final_edgelist5 <- ddply(final_edgelist4,.(a,b),nrow)
colnames(final_edgelist5) <- c("a", "b", "weight")

final_edgelist6 <- as.matrix(final_edgelist5)
final_edgelist7 <- as.data.frame(final_edgelist6)
people2 <- as.data.frame(people)
people3 <- people2[,c(2,1,3,4)]
colnames(people3) <- c("Code", "Name", "Field", "X_1")
people4 <- people3
people4$Field <- as.factor(people4$Field)
str(people4)
#install.packages("igraph")

#set color for the field factors
library("igraph")
col.rainbow <- rainbow(28)
palette(col.rainbow)
field_1 <- levels(people4$Field)
color_1 <- palette()
color_matching_frame <- cbind(field_1, color_1)

##trial for the i function
#people4[1,4] <- color_matching_frame[match(people4[1,3], color_matching_frame[,1]),2]

for (i in 1:nrow(people4)) {
  people4[i,4] <- color_matching_frame[match(people4[i,3], color_matching_frame[,1]),2]
  
}
colnames(people4)[4]<- "Color"




#########################

g <- graph_from_data_frame(d = final_edgelist7, vertices = people4, directed = FALSE)
#plot(g)
palette()
## composition of plot(g)
# black labels = vertex.label.color = "black"
#layout = layout.fruchterman.reingold(g)
#vertex.size=6
#vertex.label= 
#vertex.col 
testdegree <- degree(g, mode = "total")
testdegree1 <- testdegree + 1
testdegree2 <- log(testdegree1+.1) + 3
#vertex.size=.5* degree(g, mode = "total")


plot(g,edge.curved=.1,layout=layout.mds(g), edge.width = .3, edge.arrow.size=.1,vertex.label=NA,vertex.size = testdegree2 , vertex.color = people4$Color)

#edge.curved=.2, edge.width=.3, edge.arrow.size=.1, 
#edge.color="gray40"

edge.attributes(g)
vertex.attributes(g)

##Network Measures

#Density 0.02383028
edge_density(g, loops=F)

#Reciprocity 1 - Not a directed network so expected
reciprocity(g)

#Transitivity 0.5703605
transitivity(g, type="global")

# Diameter 5
diameter(g, directed=F)

#Mean out/all 4.408602

mean(degree(g, mode="out"))

#Isolates 62
sum(degree(g)==0)


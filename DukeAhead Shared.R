ls()
rm(list=ls())

getwd()

setwd("C:/Users/ethan/Desktop/duke projects/1. duke md network with maria/trial 2/")
list.files()

#install package to read files.
#install.packages("readxl")
library("readxl")
library("stringr")

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
library(tidyr)
data8_4 <- mutate_all(data8_3, function (x) as.numeric(x))
glimpse(data8_4)
str(data8_4)

#### now to create the edgelist...

###this is the 2nd one where having a shared nominee creates a link between the respondants. 

###first we want to create a long list of pairs 

##add first column to get initial
respondant_col <- as.data.frame(matrix(seq(1, 78, 1), ncol = 1, nrow = 78))
data8_5 <- cbind(data8_4, respondant_col)
data8_6 <- data8_5[,c(11, 1:10)]


initialize <- matrix(0, nrow = 1, ncol = 1)
colnames(initialize) <- "a"
for (i in 1:nrow(data8_6)) {
  adding_rows <- as.matrix(paste(c(as.numeric(data8_6[i,1])), c(as.matrix(data8_6[i, 2:ncol(data8_6)])), sep = ","))
  colnames(adding_rows) <- c("a")
  initialize <- rbind(initialize, adding_rows)
}

edgelist1 <- as.data.frame(initialize[-1,])
colnames(edgelist1) <- "a"
edgelist2 <- separate(edgelist1, "a", c("a", "b"))
edgelist3 <- edgelist2
edgelist3[edgelist3 == 0] <- NA

edgelist4 <- na.omit(edgelist3)

sum(is.na(edgelist3)) 
#math works out- NA's removed
  
edgelist4$a <- paste("R", edgelist4$a, sep="")
edgelist5 <- edgelist4[,c(2,1)]
edgelist5[,1] <- as.numeric(edgelist5[,1])
edgelist6 <- edgelist5[order(edgelist5[,1]), ]
#if you want first column to be a character
edgelist6$b <- as.numeric(edgelist6$b)
str(edgelist6)

##testing parts of the i function before creating it
length(edgelist6[which(edgelist6[,1] == 1), 2])
##condition for if then statement
sum(edgelist6[,1] == 9) >= 1
##pulling the corresponding things
edgelist6[which(edgelist6[,1] == 4), 2]
##creating combinations of this
combn(edgelist6[which(edgelist6[,1] == 4), 2], 2)
##test for loop conditions
for (i in 1:max(edgelist6$b)) {
  if((length(edgelist6[which(edgelist6[,1] == i), 2]) >= 2) == TRUE) {
    print("Working")
    
  } else {
    print("Skipping")
  }
  
  
}

##if else statements work-- input 

initialize <- matrix(0, nrow = 2, ncol = 1)

for (i in 1:max(edgelist6$b)) {
  if ((length(edgelist6[which(edgelist6[,1] == i), 2]) >= 2) == TRUE) {
    building_pairs <- as.matrix(combn(edgelist6[which(edgelist6[,1] == i), 2], 2, simplify = TRUE))
    initialize <- cbind(initialize, building_pairs)
  } else {
    building_pairs <- matrix(0, nrow = 2, ncol = 1)
    initialize <- cbind(initialize, building_pairs)
  }
  
}

##initalize complete
final_list <- as.data.frame(t(initialize))

##finalize edgelist for igraph
final_list2 <- final_list
final_list2[final_list2 == 0] <- NA
final_list3 <- data.frame(matrix(unlist(final_list2), nrow=nrow(final_list2)),stringsAsFactors=FALSE)
final_list4 <- final_list3[complete.cases(final_list3), ]

final_list5 <- final_list4
colnames(final_list5) <- c("a", "b")
rownames(final_list5) <- c(1:nrow(final_list5))

#add weight consider duplicates

low_high1 <- as.data.frame(lapply(final_list5, function(x) as.numeric(gsub("[^0-9]", "", x))))
colnames(low_high1) <- c("a", "b")
weighted_list <- ddply(low_high1,.(a,b),nrow)

final_list6 <- as.data.frame(lapply(weighted_list[,c(1,2)], function(x) paste("R", x, sep = "")))
final_list7 <- cbind(final_list6, weighted_list[,3])
colnames(final_list7) <- c("a", "b", "weight")

#bring a list of all the respondants as the survey did not ask respondant names
respondant <- apply(cbind("R", 1:78), 1, function(x)paste(x, collapse = "_"))
##get attributes to bind on
##gender 28, position 31, years experience 29--which columns? : gender q28 -- 197, position q31-- 201, years experience q29 -- 198

colnames(data)


total_list <- c(final_list5$a, final_list5$b)
condensed_list <- unique(total_list)
numbered_list <- as.numeric(gsub("[^0-9]", "", condensed_list))
ordered_list <- numbered_list[order(numbered_list)]
ordered_list2 <- paste("R", ordered_list, sep="")

removed_test_data <- data[2:nrow(data),]
info <- removed_test_data[ordered_list, c(197, 201, 198)]
info2 <- cbind(ordered_list2, info)
colnames(info2) <- c("Respondant Name", "Gender", "Position", "Years Experience")
##adding in attributes

#gender
info3 <- info2

gender_string <- as.numeric(info2$Gender)
gender_string[is.na(gender_string)] <- 0
gender_string

gender_string2 <- rep(0, length(gender_string)) 
  
for (i in 1:length(gender_string)) {
  if (gender_string[i] == 0) {
    gender_string2[i] = "Not Specified"
  } else if (gender_string[i] == 1) {
    gender_string2[i] = "Male"
  } else {
    gender_string2[i] = "Female"
  }
}

info3$Gender <- gender_string2

#position

position_string <- as.numeric(info2$Position)
position_string[is.na(position_string)] <- 0
position_string

position_string2 <- rep(0, length(position_string))

for(i in 1:length(position_string)) {
  if (position_string[1] == 0) {
    position_string2[i] = "Not Specified"
  } else if (position_string[i] == 1) {
    position_string2[i] = "Faculty"
  } else if (position_string[i] == 2) {
    position_string2[i] = "Staff"
  } else if (position_string[i] == 3) {
    position_string2[i] = "Resident/Fellow"
  } else if (position_string[i] == 4) {
    position_string2[i] = "Student"
  } else {
    position_string2[i] = "Other"
  }
}

position_string2

info3$Position <- position_string2

#years experience
years_string <- as.numeric(info3$`Years Experience`)
years_string[is.na(years_string)] <- "Not Specified"
info3$`Years Experience`<- years_string
  
str(info3)
info3$Position <- as.factor(info3$Position)
info3$`Respondant Name`<- as.character(info3$`Respondant Name`)
str(info3)

##need to add color for position

#set color for the field factors
#install.packages("igraph")
library("igraph")
col.rainbow <- rainbow(nlevels(info3$Position))
palette(col.rainbow)
field_1 <- levels(info3$Position)
color_1 <- palette()
color_matching_frame <- cbind(field_1, color_1)

empty_color <- rep(0, nrow(info3))
info4 <- cbind(info3, empty_color)
colnames(info4)[5] <- "Color"

for (i in 1:nrow(info4)) {
  info4[i,5] <- color_matching_frame[match(info3$Position[i], color_matching_frame[,1]),2]
  
}


#########################
class(final_list5)
g <- graph_from_data_frame(d = final_list7, vertices = info4, directed = FALSE)
#plot(g)
palette()
## composition of plot(g)
# black labels = vertex.label.color = "black"
#layout = layout.fruchterman.reingold(g)
#vertex.size=6
#vertex.label= 
#vertex.col

plot(g, edge.width = .1,vertex.label=info4[,1], vertex.label.cex = .8, vertex.label.dist = 1.2, vertex.label.degree = -pi/2, layout=layout.fruchterman.reingold(g), vertex.size=1.5* degree(g, mode = "total"), vertex.color = info4$Color)

#vertex.label.cex Font size (multiplication factor, device-dependent)
#vertex.label.dist Distance between the label and the vertex
#vertex.label.degree  The position of the label in relation to the vertex, where 0 is right, "pi" is left, "pi/2" is below, and "-pi/2" is above


edge.attributes(g)
vertex.attributes(g)

##Network Measures

#Density 0.1477833
edge_density(g, loops=F)

#Reciprocity 1
reciprocity(g)

#Transitivity 0.5278592 
transitivity(g, type="global")

# Diameter 6
diameter(g, directed=F)

#Mean out/all 4.137931

mean(degree(g, mode="out"))

#Isolates 0
sum(degree(g)==0)


##Clear the workspace
ls()
rm(list=ls())


##get working directory
getwd()
setwd("C:/Users/ethan/Desktop/duke projects/2. PMCI network/1. pre/data")
list.files()

##Install package to read the .dta files
#install.packages("readstata13")
library("readstata13")
library("dplyr")
#insert data
pre1<- read.dta13("PCMI_pre_2017_November+9,+2017_08.47.dta")
########################################################################################################################################################################
#examine data
str(pre1)
head(pre1)
class(pre1)
colnames(pre1)
#the most obvious step is to isolate the data that we wish to examine. We will complete the PRE network for researchers. The question appears to have no number but we assume it is the "rsrch" question in the data.
#I presume we will use some form the apply function to filter only questions with researchers. We can see that the questions are actually from 66-127 for researchers. 
#pre1[9,"rsrch_66_1"] is the sample. isolate all researchers

#we have successfully retrieved from 66:127 those who "have heard of". Will repeat for the rest of the 2 choices. Ask maria why simple doing pre2 <- pre1[, "rsrch_66_1":"rsrch_127_3"] did not subset the entire data frame and instead gave a error:NA/NaN argument   NAs introduced by coercion
rsrch1_colnames <- apply(cbind("rsrch", 66:127, 1), 1, function(x)paste(x, collapse = "_"))
pre2_1 <- pre1[, rsrch1_colnames]

rsrch2_colnames <- apply(cbind("rsrch", 66:127, 2), 1, function(x)paste(x, collapse = "_"))
pre2_2 <- pre1[, rsrch2_colnames]

rsrch3_colnames <- apply(cbind("rsrch", 66:127, 3), 1, function(x)paste(x, collapse = "_"))
pre2_3 <- pre1[, rsrch3_colnames]

##now we have 3 different data frames. lets check to see they are the same dim.
dim(pre2_1)
dim(pre2_2)
dim(pre2_3)

### we are going to use a if else function. We will relace all the NA's with zero, and any characters with 1. then we will add all three data.frames/ matrices together to get different levels. 
## after thinking that will not work. We do not know if people have checked yes for all the factors or just the highest one. We will do an if else else statement and create a new matrix with the highest number 


#try out experiments on pre2_1
head(pre2_1)
colnames(pre2_1)
rownames(pre2_1)
##pre2 1 matrix
pre2_1_matrix <- matrix(0, nrow = nrow(pre2_1), ncol = ncol(pre2_1))

for(i in 1:nrow(pre2_1)) {
  pre2_1_matrix[i, ] <-  sapply(pre2_1[i, ], function(x) if (is.na(x) == TRUE) {0} else {1})
}
##pre2 2 matrix
pre2_2_matrix <- matrix(0, nrow = nrow(pre2_2), ncol = ncol(pre2_2))

for(i in 1:nrow(pre2_2)) {
  pre2_2_matrix[i, ] <-  sapply(pre2_2[i, ], function(x) if (is.na(x) == TRUE) {0} else {2})
}
##pre2 3 matrix
pre2_3_matrix <- matrix(0, nrow = nrow(pre2_3), ncol = ncol(pre2_3))

for(i in 1:nrow(pre2_3)) {
  pre2_3_matrix[i, ] <-  sapply(pre2_3[i, ], function(x) if (is.na(x) == TRUE) {0} else {3})
}

###i have three different matrixes... time to combine them and select the highest value. how do I do this? also is there a way to label the matrix axis so i can remember the top is researches the side is respondents
##i will use the sapply function row by row and select the max just like I did above

### create empty matrix
pre_rsrch_matrix <- matrix(0, nrow = nrow(pre2_1), ncol = ncol(pre2_1))

pre_rsrch_matrix <- pmax(pre2_1_matrix, pre2_2_matrix, pre2_3_matrix)

#### with that we get the final matrix... the dimensions of it are:
dim(pre_rsrch_matrix)
### yielding 253x62

## get graduate. Research complete

grad1_colnames <- apply(cbind("Q64", 164:244, 1), 1, function(x)paste(x, collapse = "_"))
grad2_1 <- pre1[, grad1_colnames]

grad2_colnames <- apply(cbind("Q64", 164:244, 2), 1, function(x)paste(x, collapse = "_"))
grad2_2 <- pre1[, grad2_colnames]

grad3_colnames <- apply(cbind("Q64", 164:244, 3), 1, function(x)paste(x, collapse = "_"))
grad2_3 <- pre1[, grad3_colnames]

##now we have 3 different data frames. lets check to see they are the same dim.
dim(grad2_1)
dim(grad2_2)
dim(grad2_3)

##grad2 1 matrix
grad2_1_matrix <- matrix(0, nrow = nrow(grad2_1), ncol = ncol(grad2_1))

for(i in 1:nrow(grad2_1)) {
  grad2_1_matrix[i, ] <-  sapply(grad2_1[i, ], function(x) if (is.na(x) == TRUE) {0} else {1})
}
##grad2 2 matrix
grad2_2_matrix <- matrix(0, nrow = nrow(grad2_2), ncol = ncol(grad2_2))

for(i in 1:nrow(grad2_2)) {
  grad2_2_matrix[i, ] <-  sapply(grad2_2[i, ], function(x) if (is.na(x) == TRUE) {0} else {2})
}
##grad2 3 matrix
grad2_3_matrix <- matrix(0, nrow = nrow(grad2_3), ncol = ncol(grad2_3))

for(i in 1:nrow(grad2_3)) {
  grad2_3_matrix[i, ] <-  sapply(grad2_3[i, ], function(x) if (is.na(x) == TRUE) {0} else {3})
}

pre_grad_matrix <- matrix(0, nrow = nrow(grad2_1), ncol = ncol(grad2_1))

pre_grad_matrix <- pmax(grad2_1_matrix, grad2_2_matrix, grad2_3_matrix)
dim(pre_grad_matrix)
####grad complete.. do the undergrad.. net2, 
net_colnames <- apply(cbind("net2", 64:86, 1), 1, function(x)paste(x, collapse = "_"))
net2_1 <- pre1[, net_colnames]

net2_colnames <- apply(cbind("net2", 64:86, 2), 1, function(x)paste(x, collapse = "_"))
net2_2 <- pre1[, net2_colnames]


##now we have 3 different data frames. lets check to see they are the same dim.
dim(net2_1)
dim(net2_2)

##net2 1 matrix
net2_1_matrix <- matrix(0, nrow = nrow(net2_1), ncol = ncol(net2_1))

for(i in 1:nrow(net2_1)) {
  net2_1_matrix[i, ] <-  sapply(net2_1[i, ], function(x) if (is.na(x) == TRUE) {0} else {1})
}
##net2 2 matrix
net2_2_matrix <- matrix(0, nrow = nrow(net2_2), ncol = ncol(net2_2))

for(i in 1:nrow(net2_2)) {
  net2_2_matrix[i, ] <-  sapply(net2_2[i, ], function(x) if (is.na(x) == TRUE) {0} else {2})
}

### create empty matrix
pre_net_matrix <- matrix(0, nrow = nrow(net2_1), ncol = ncol(net2_1))

pre_net_matrix <- pmax(net2_1_matrix, net2_2_matrix)


######################
##lets combine and create a giant network
pre_matrix <- cbind(pre_rsrch_matrix, pre_grad_matrix, pre_net_matrix)
## check for dimensions for correct binding
dim(pre_rsrch_matrix)
dim(pre_grad_matrix)
dim(pre_net_matrix)
dim(pre_matrix)
## checks out, 253 observation of 62+81+23 names = 166
########################################################################################################################################################################
#######time to imput names from word document

##which packages reads excel
#install.packages("xlsx")
#install.packages("readxl")
library("readxl")
names_all <- read_excel("names pmci.xlsx", sheet = 4)
colnames(names_all)
## split up the excel sheet
#install.packages("stringr")
library("stringr")
names2 <- str_split_fixed(names_all$`all names`, ",", 2)
names2_1<- as.data.frame(names2)
class(names2_1)

names2_college <- names2_1[,2]
names2_college2 <- str_split_fixed(names2_college, "[(]", 2)
class(names2_college2)
names2_college3 <- as.data.frame(names2_college2)
names2_id <- names2_college3[,2]
names2_id2 <- gsub("[()]", "", names2_id)
names2_id3 <- as.numeric(names2_id2)

###make final names data frame

names_finaltrial <- cbind(names2_1, names2_college3, names2_id3)
head(names_finaltrial)
head(names_finaltrial[, c(1, 3, 5)])

final_names <- names_finaltrial[, c(1, 3, 5)]
#########and with this last step, I have assembled the entire name key with attributes for the top row or the names of the columns. 
##I will now assemble the row names or the respondent. Will need to reorder to make both sides identical.
## find the respondent names
colnames(pre1)
## important columns : [9]"ResponseId" [10] "RecipientLastName" [11]"RecipientFirstName"  [12]"RecipientEmail" 
## [50]  "gender" [535] "Institution_or_organization" [536] "Registrant_Type_Description" [537]"researcher"                 
##[538] "mentor"  [539]"teacher" [540] "gradstudent" [541] "undergrad"[542] "ufp" [543]  "researchfaculty"   also [18]appt    
     
head(pre1[,c(535, 536, 537, 538, 539, 540)])

respondant_data <- pre1[, c(10, 11)]

################################################################################################################
##experiment how to sort.. figured it out
#atest <- c("a" , "g", "d", "b")
#btest <- c(1, 2, 3, 4)
#dftest <- cbind(atest, btest)

#df_sort <- dftest[order(atest), ]

#sort(atest)
#order(atest)

################################################################################################################
## Need to combine first and last name columns before any sorting

### another round of testing 
#atest <- c("a" , "g", "d", "b")
#btest <- c(1, 2, 3, 4)
#dftest <- cbind(atest, btest)

#df_col_names <- c("letter", "number")
#colnames(dftest) <- df_col_names

#dftest %>% as.data.frame() %>% mutate(
#  full_columns = paste(dftest[,1], dftest[,2], sep = " ")
#  
#)
#
### took a while but figured it out

head(respondant_data)
colnames(respondant_data2)
respondant_data2 <- paste(respondant_data[,2], respondant_data[,1], sep = " ")
  

#fixed one column name

#######################################################trials, delete below for original


#tomorrow i will order them, and figure out where to go from there

head(respondant_data2)

head(final_names)


#order final names
colnames(final_names)
ordered_fn <- final_names[order(final_names[,1]), ]



###### test -- make pre_matrix into data frame and order everthing

pre_data_frame <- as.data.frame(pre_matrix)
#add the row/column names
pre_data2 <- pre_data_frame[c(2:nrow(pre_data_frame)), ]

pre_data3 <- pre_data2
colnames(pre_data3) <- final_names[,1]

rownames(pre_data3) <- respondant_data2[2:length(respondant_data2)]

#row/col added -- order both? next step?
col_pre3 <- colnames(pre_data3)
row_pre3 <- rownames(pre_data3)

#simpleCap <- function(x) {
#  s <- strsplit(x, " ")[[1]]
#  paste(toupper(substring(s, 1,1)), substring(s, 2),
#        sep="", collapse=" ")
#}
#
#name <- c("zip code", "state", "final count")

#sapply(name, simpleCap)

col_pre3_2 <- tolower(col_pre3)
row_pre3_2 <- tolower(row_pre3)

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}



col_pre3_3 <- sapply(col_pre3_2, simpleCap)
row_pre3_3 <- sapply(row_pre3_2, simpleCap)
##fix all special characters
col_pre3_4 <- as.vector(col_pre3_3)
row_pre3_4 <- as.vector(row_pre3_3)
#add column 121 fix: Dominik stoger
col_pre3_4[121] <- "Dominik Stoger"
## add row 13/204 fix Catherine Belin Dominik Stoger
row_pre3_4[13] <- "Catherine Belin"
row_pre3_4[204] <- "Dominik Stoger"
pre_data4 <- pre_data3
colnames(pre_data4) <- col_pre3_4
rownames(pre_data4) <- row_pre3_4


####With those lines, I have fixed the capitalization errors by first making them all lower case and then capitalizing the first letter of each word. Note that this leaves letters after dashes uncapitalized but that doesnt matter as long as the row and column names are uniform. 

###finding what is in the column names that isnt in the rows. AKA missspellings or stuff

missing_col_pre4 <- rep(NA, length(col_pre3_4))
for (i in 1:length(col_pre3_4)) {
  if (sum(str_detect(row_pre3_4, col_pre3_4[i])) == 0) {
    missing_col_pre4[i] <- col_pre3_4[i]
  }
}

missing_col_pre4
 
missing_col_pre5 <- missing_col_pre4[!is.na(missing_col_pre4)]


#lets start finding these mistakes
missing_col_pre5
#[1]"Gerard Ben Arous"   "Gaetan Borot"       "Sourav Chatterjee"  "Persi Diaconis"     "Yu Gu"             
#[6] "Vladislav Kargin"   "Pierre Le Doussal"  "Anna Maltsev"       "Govind Menon"       "Firas Rassoul-agha"
#[11] "Evgeny Strahov"     "Wei Wu"             "Arka Adhikari"      "Andrew Ahn"         "Jake Marcinek"   

#find gerald-- not there, add gerald
#find gaetan borot -- not there add gaetan
#find Sourav Chatterjee -- not there add
#find Persi Diaconis, -- row 161 wrong persi, move on need to add
#Find Yu Gu, not there need to add
#find Vladislav Kargin, not there need to add
#Pierre le Doussal- not there need to add
#Anna Maltsev -- not there add
#Govind Menon- not there add
#Firas Rassoul-agha-- not there add
#Evgeny Strahov -- not there add
#Wei Wu-- not there add
#Arka Adhikari-- not there add
#Andrew Ahn-- not there add
#Jake Marcinek-- not there

which(str_detect(row_pre4, "rka"))
pre_data3[which(str_detect(row_pre4, "ake")), 1:4]
pre_data3[23,1:4]
row_pre3

##test adding rows
#test_addrows <- cbind(c(1,2,3), c(4,5,6), c(7,8,9))
#rownames(test_addrows) <- c("a", "b", "c")
#dtest <- rep(0, ncol(test_addrows))
#test_addrows <- rbind(test_addrows, "d" = dtest)

##add rows
##empty data frame
missing_row_names_matrix <- matrix(0, nrow = length(missing_col_pre5), ncol = ncol(pre_data4))
missing_row_data <- as.data.frame(missing_row_names_matrix)
rownames(missing_row_data) <- missing_col_pre5
colnames(missing_row_data) <- col_pre3_4
empty_matrix <- matrix(0, 3, 3)

pre_data5 <- rbind(pre_data4, missing_row_data)

####sample code to check for existence of patterns
#which(str_detect(row_pre3, "rka"))
#pre_data3[which(str_detect(row_pre3, "onat")), 1:4]
#pre_data3[23,1:4]
#row_pre3

#check my work for adding rows there-- none missing
check_rows <- rep(NA, length(colnames(pre_data5)))

for (i in 1:length(colnames(pre_data5))) {
  if (sum(str_detect(rownames(pre_data5), colnames(pre_data5)[i])) == 0) {
    check_rows[i] <- colnames(pre_data5)[i]
  }
}

check_rows

check_rows2 <- check_rows[!is.na(check_rows)]
### we have added all missing rows, now to add the columns. 

##lets reiterate our variables and values so we dont forget
#pre_data4 is the updated table with correct capitalization
#col_pre3_3 is the colnames
#row_pre3_3 is the row names
#we wish to find now what is in the row names that is not reciprocated in the column names

missing_row_pre4 <- rep(NA, length(row_pre3_4))
for (i in 1:length(row_pre3_4)) {
  if (sum(str_detect(col_pre3_4, row_pre3_4[i])) == 0) {
    missing_row_pre4[i] <- row_pre3_4[i]
  }
}

missing_row_pre4

missing_row_pre5 <- missing_row_pre4[!is.na(missing_row_pre4)]

#lets start finding these mistakes
missing_row_pre5
#[1] "Florentino Elicegui"              "Bowen Yang"                       "Jennifer Moon"                   
#[4] "Cindy Ruhsam"                     "Anna Blinstein"                   "Catherine Belin"          
#[7] "David Martin"                     "Traci Jackson"                    "Nikolas Wojtalewicz"             
#[10] "Benjamin Krakoff"                 "Polina Belopashentseva"           "Diana Mcclean"                   
#[13] "Hannah Grant"                     "Jonathan Gonzalez Davila"         "Ho Lung Tsui"                    
#[16] "Kayleigh Rose"                    "George Santellano"                "Mary Velez"                      
#[19] "Adriana Morales Miranda"          "Erin Bossen"                      "Brooke Sossin"                   
#[22] "Trevor Karn"                      "Daniel Nissani"                   "Francesca Falzon"                
#[25] "Samuel Kaplan"                    "Abigail Kirchman"                 "Iris Nelson"                     
#[28] "Daniel Kang"                      "Gregory Taylor"                   "Greg Zanotti"                    
#[31] "Srivatsav Kunnawalkam Elayavalli" "Jiarui Chu"                       "Emilia Alvarez"                  
#[34] "Andrew Rodriguez"                 "Trang Tran"                       "Kate Carter"                     
#[37] "Carla Parker"                     "Kai Hugtenburg"                   "Phong Chau"                      
#[40] "Oscar E. González"                "Kimberly Elicker"                 "Jasper Deantonio"                
#[43] "Mark Sellke"                      "Genevieve Esmende"                "Dylan Galt"                      
#[46] "Linda Nguyen"                     "Felipe Pereira Debayle"           "Arundhati Velamur"               
#[49] "Hilda Prado"                      "Christopher Keane"                "Becky Bob-waksberg"              
#[52] "Sarah Melancon"                   "Brian Shay"                       "Sarah Yoseph"                    
#[55] "Linh Tran"                        "Marissa Walczak"                  "Gareth Chase"                    
#[58] "Sean Corey"                       "Jace Arends"                      "Ayesha Yacubic"                  
#[61] "Ryan Utke"                        "Johnson Nguyen"                   "Adelia Kadyralieva"              
#[64] "Kristy Gallo"                     "Julie Wright"                     "Naira Harutyunyan"               
#[67] "Natalie Miroshnichenko"           "Mengbing Li"                      "Tamunonye Cheetham-west"         
#[70] "Daniel Persia"                    "Matthew Rosenberg"                "Benjamin Walker"                 
#[73] "Vince Muccioli"                   "Alison Mall"                      "Brian Hsu"                       
#[76] "Cecily Santiago"                  "Benjamin Yellin"                  "Lee Melvin Peralta"              
#[79] "Angelina Morelli"                 "Caleb Dahlke"                     "Melissa Kammerman"               
#[82] "Lynde Nanson"                     "Jennifer Parker"                  "Dylan Cable"                     
#[85] "Elizabeth Mcgrath"                "Hemang Srikishan"                 "Anthony Utehs"                   
#[88] "Dylan Airey"                      "Ramona Fittipaldi"                "Carsten Peterson"                
#[91] "Melanie Brintnall"                "Ariel Kramer"                     "Oyinka Bruce"                    
#[94] "Siaka Kone"                       "Zheng Bian"                       "Sam Hilkey"                      
#[97] "Roy Snyder"                       "Cristina Jimenez-shawcroft"       "Mark Dittmer"                    
#[100] "Gabrielle Mathiesen"              "Justin Tse"            
colnames(pre_data5)

#add missing
missing_col_names_matrix <- matrix(0, nrow = nrow(pre_data5), ncol = length(missing_row_pre5))
missing_col_data <- as.data.frame(missing_col_names_matrix)
rownames(missing_col_data) <- rownames(pre_data5)
colnames(missing_col_data) <- missing_row_pre5
empty_matrix <- matrix(0, 3, 3)

pre_data6 <- cbind(pre_data5, missing_col_data)



#same size matrix-- now to organize 
#pre_data7 <- pre_data6[order(rownames(pre_data6)), ]
#pre_data8 <- pre_data7[order(colnames(pre_data7)), ]
pre_data7 <- pre_data6[order(rownames(pre_data6)),order(colnames(pre_data6))] 

pre_data8 <- as.matrix(pre_data7)
dim(pre_data8)
str(pre_data8)

#matrix completed- should be 267 by 267

rownames(pre_data6)
name_attr1 <- pre1[, c(10,11,18,21)]
name_attr2 <- name_attr1[-1, ]
head(name_attr1[,2 ], 20)
head(rownames(pre_data6), 20)
colnames(pre1)
##create network
library(igraph)

##what attributes do i want

#creating data frame of attributes
attribute_names <- rownames(pre_data6)[1:252]
attributes1 <- cbind(attribute_names, name_attr2[,3:4])
colnames(attributes1) <- c("Name", "Position", "University")

#add missing people from columns
missing_attr <- read_excel("attributes table.xlsx", sheet = 1, col_names = FALSE)
colnames(missing_attr) <- c("Name", "Position", "University")
attributes2 <- rbind(attributes1, missing_attr)
attributes3 <- data.frame(lapply(attributes2, as.character), stringsAsFactors=FALSE)

sum(rownames(pre_data6) == attributes3$Name)

attributes3[is.na(attributes3)] <- "Other"


str(attributes3)

##names match-- attributes complete
##make the positions a factor
attributes3$Position <- as.factor(attributes3$Position)


nlevels(attributes3$Position)


col.rainbow <- rainbow(nlevels(attributes3$Position))
palette(col.rainbow)
field_1 <- levels(attributes3$Position)
color_1 <- palette()
color_matching_frame <- cbind(field_1, color_1)

color_matching_frame
color_matching_frame[3,2] <- "#7A2813"

#adjust colors
color_fixed <- c("#2C76D6", "#E8C2C2", "#00F6FF", "#FF9000", "#F72500", "#F7E600", "#49E23B")
color_matching_frame[,2] <- color_fixed


empty_color_col <- rep(0, nrow(attributes3))
attributes4<- cbind(attributes3, empty_color_col)
colnames(attributes4)[4] <- "Color"

for (i in 1:nrow(attributes4)) {
  attributes4[i,4] <- color_matching_frame[match(attributes4[i,2], color_matching_frame[,1]),2]
  
}



#########################
g <- graph_from_adjacency_matrix(pre_data8, mode = c("directed"), weighted = TRUE, diag = FALSE, add.colnames = NULL, add.rownames = NA)



palette()

#vertex attributes
V(g)$Name <- attributes4$Name
V(g)$Position <- attributes4$Position 
V(g)$University <- attributes4$University
testdegree <- degree(g, mode = "total")
testdegree1 <- testdegree + 1
testdegree2 <- log(testdegree1+.1)/3 + 3
plot(g, edge.arrow.size=.1,edge.width = .1, vertex.label=NA, layout=layout.fruchterman.reingold(g), vertex.size=.1* degree(g, mode = "total"), vertex.color = attributes4$Color)
E(g)$weight = 0.01
#with isolates
z <- g

E(z)$weight <- 1 
z <- simplify(z, edge.attr.comb=list(weight="sum"))


plot(g,edge.curved = .3, edge.arrow.size=.1,edge.color="gray40", edge.width = .5, vertex.label=NA, layout=layout.mds(g), vertex.size=testdegree2, vertex.color = attributes4$Color)
#without isolates
plot(z, edge.arrow.size=.1,edge.width = .3,edge.color="gray40", vertex.label=NA, layout=layout.mds(g), vertex.size=1.5* log(degree(g, mode = "total")), vertex.color = attributes4$Color)
edge.attributes(g)
vertex.attributes(g)
#2* log(2 *testdegree1)
#plot(net.1p, vertex.label=NA, 
#vertex.size=V(prenw)$lidegree1p,
#edge.curved=.2, edge.width=.3, edge.arrow.size=.1, 
#edge.color="gray40", layout=layout.mds(net.1p), vertex.color=V(prenw)$color, vertex.shape=V(prenw)$shape)


#testdegree1[which(testdegree == 0)] <- .5
##Network Measures

#Density 0.03607333
edge_density(g, loops=F)

#Reciprocity 0.4434036
reciprocity(g)

#Transitivity 0.4689178
transitivity(g, type="global")

# Diameter 8
diameter(g, directed=F)

#Mean out/all 9.595506
mean(degree(g, mode="out"))

#Isolates 109
sum(degree(g)==0)

vertex_attr(g, "Position")


####plot with just grad/undergrad

attributes4 %% filter(Position == "Graduate student" )

needed_grad <- attributes4[which(attributes4$Position == "Graduate student"),1:2 ]
needed_under <- attributes4[which(attributes4$Position == "Undergraduate student"),1:2 ]

class(needed_grad)
minimal_names <- rbind(needed_grad, needed_under)

##now to subset matrix -- predata7

which(colnames(pre_data7) == c("Adel Faridani", "Alice Seneres"))

small_net <- pre_data7[ which(rownames(pre_data7) %in% minimal_names[,1]), which(colnames(pre_data7) %in% minimal_names[,1])]
small_net2 <- as.matrix(small_net)
## assign attributes
small_attributes <- attributes4[which(attributes4$Name %in% minimal_names[,1]), 1:2]

small_color_frame <- color_matching_frame[which(color_matching_frame[,1] %in% small_attributes[,2]) , ]
small_color_frame[1,2] <- "#FFA500"
small_color_frame[2,2] <- "#454545"


empty_small_color_col <- rep(0, nrow(small_attributes))
small_attributes2 <- cbind(small_attributes, empty_small_color_col)
colnames(small_attributes2)[3] <- "Color"

for (i in 1:nrow(small_attributes2)) {
  small_attributes2[i,4] <- small_color_frame[match(small_attributes2[i,2], small_color_frame[,1]),2]
  
}

small_attributes3 <- small_attributes2[order(small_attributes2[,1]),]

z <- graph_from_adjacency_matrix(small_net2, mode = c("directed"), weighted = TRUE, diag = FALSE, add.colnames = NULL, add.rownames = NA)
V(z)$Name <- small_attributes3$Name
V(z)$Position <- small_attributes3$Position 
small_testdegree <- degree(z, mode = "total")
small_testdegree1 <- small_testdegree + 1
small_testdegree2 <- log(small_testdegree1+.1)/3 + 3
E(z)$weight = 0.01
V(z)$shape <- ifelse(V(z)$Position == 1, "circle", "square")


plot(z,edge.curved = .3, edge.arrow.size=.1,edge.color="#909090", edge.width = .5, vertex.shape = V(z)$shape, vertex.label=NA, layout=layout.mds(g), vertex.size=small_testdegree2, vertex.color = small_attributes3[,4])
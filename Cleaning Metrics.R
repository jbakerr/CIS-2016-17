#install.packages("dplyr") # These packages need to be installed once, after that they can be called up using library. This package is for restructuring data
#install.packages("XLConnect") # This package is for loading / writing excel spreadsheets
#install.packages("tidyr")
#install.packages("plyr")
library(plyr) # ****IMPORTANT**** Load plyr before dplyr- they have some of the same named functions, and if you load in a different order it will cause problems
library(dplyr)
library(tidyr)
library(XLConnect)
###################################      SERVICES DATA CHECK      ##########################################
# Set the working directory to the local folder containing dataset. Can be done manually
# by going to Session -> Set Working Directory -> Choose Directory 
macdatawd <- "~/Google Drive/Data Files"
windowsdatawd <- "C:/Users/USER/Google Drive/Data Files"
if(file.exists(macdatawd)){
  setwd(file.path(macdatawd))
} else { 
  if(file.exists(windowsdatawd)){
    setwd(file.path(windowsdatawd))
  }
}


#Setting Vectors that will be used throughout program
metrics <- c("Math","Science","ELA", "Suspensions", "Attendance Rate")

elem <- c("Glenn Elementary School", "Eno Valley Elementary", "EK Powe Elementary School", "YE Smith Elementary")
high <- c("Neal Middle School", "Durham Performance Learning Center", "Hillside High School", "Southern High School", "Northern")




#Load Worksheet
progress<-readWorksheetFromFile('progress.xlsx', sheet=1, header = T, startRow = 1)

#Rename Grading Quarters
colnames(progress)[7:9] <- c("Q1", "Q2", "Q3")
#Removing Test Data
progress <- subset(progress, !progress$Case.Manager == "Nina Wilson")
progress <- subset(progress, !progress$Case.Manager == "Test Account")

#Changing Metric Title
progress$Metric[progress$Metric =='Core Course Grades: Eng/Lang Arts/Reading/Writing'] <- "ELA"

progress$Metric[progress$Metric =='Core Course Grades: Math 1'] <- "Math"

progress$Metric[progress$Metric =='Core Course Grades: Science'] <- "Science"


progress <- progress[ ! (progress$Metric =='Standardized test score: English / Language Arts'),]

progress <- progress[ ! (progress$Metric =='Standardized test score: Science'),]




#Dropping Unneeded Columns

progress$Latest.Progress <- NULL
progress$Target <- NULL

#for now dropping Q2 and Q3
progress$Q2 <- NULL
progress$Q3 <- NULL


#removing unwanted metrics

progress <- subset(progress, progress$Metric %in% metrics)


#Adjusting Attendance Rate from days to percentage NEED TO FIX TO ALLOW FOR MORE THAN 1 QUARTER OF DATA 

elem.adjust <- progress$Q1[progress$Metric == "Attendance Rate" & !is.na(progress$Q1) & (progress$School %in% elem) & progress$Q1 < 50]
elem.adjust <- (45-elem.adjust)/45
elem.adjust <- elem.adjust * 100

progress$Q1[progress$Metric == "Attendance Rate" & progress$School %in% elem & progress$Q1 < 50 & !is.na(progress$Q1)] <- elem.adjust

high.adjust <- progress$Q1[progress$Metric == "Attendance Rate" & !is.na(progress$Q1) & (progress$School %in% high) & progress$Q1 < 1]
high.adjust <- high.adjust *100

progress$Q1[progress$Metric == "Attendance Rate" & !is.na(progress$Q1) & (progress$School %in% high) & progress$Q1 < 1] <- high.adjust

#summary(subset(progress$Q1, progress$Metric == "Attendance Rate" & progress$School %in% high))


#Creating Names for each quarter / subject combination
#progress$Baseline <- NULL 


progress <- gather(progress, Period, Value, Baseline:Q1, factor_key = T)
quartersubject <- paste(progress$Period, " ", progress$Metric, sep = " ")
progress$quartersubject <- quartersubject
progress$Period <- NULL

#removing duplicates

progress <- progress[!duplicated(progress[,c("School", "Student.Name","Metric", "Value")]), ] # This is a soft option that just deletes one of the duplicates arbitrarily





#Creating a wide data frame
progress <- spread(progress[, ! colnames(progress) %in% c("Metric", "Period")], quartersubject, Value)


#Saving Data 











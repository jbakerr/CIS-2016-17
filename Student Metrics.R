#install.packages("dplyr") # These packages need to be installed once, after that they can be called up using library. This package is for restructuring data
#install.packages("XLConnect") # This package is for loading / writing excel spreadsheets
#install.packages("tidyr")
#install.packages("plyr")
library(plyr) # ****IMPORTANT**** Load plyr before dplyr- they have some of the same named functions, and if you load in a different order it will cause problems
library(dplyr)
library(tidyr)
library(XLConnect)
###################################      Generating Student Metrics      ##########################################
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


# Adding service aggregates to student list
stserv <- data %>% group_by(Student.ID) %>% summarize(Hours = sum(Hours), num_serv = length(Student.ID), service_date = tail(Support.Date, n =1 ))
#checkcounts <- data[data$checkin != 0 , ] %>% group_by(Student.ID) %>% summarize(num_check = n())
#parentcounts <- data[data$parent1on1 != 0, ] %>% group_by(Student.ID) %>% summarize(num_parent1on1 = n())
#anyfamcounts <- data[data$anyfamily != 0, ] %>% group_by(Student.ID) %>% summarize(num_anyfamily = n())





stlist <- merge(progress, stserv, by = "Student.ID", all = T)

#Create average grade metric

stlist$Q1_ELA <- as.numeric(stlist$Q1_ELA)
stlist$Q1_Math <- as.numeric(stlist$Q1_Math)
stlist$Q1_Science <- as.numeric(stlist$Q1_Science)


stlist$avg.grade.Q1 <- 0
stlist$avg.grade.Q1 <- rowMeans(stlist[, c("Q1_Science", "Q1_Math", "Q1_ELA")], na.rm =T)

# 
# #Aggregating Outcome info
# stlist$avgrade1 <- rowMeans(stlist[, c("Q1_Science", "Q1_Math", "Q1_ELA", "Q_", "Q_1 Lang. Arts")], na.rm =T)
# stlist$avgrade2 <- rowMeans(stlist[, c("Q_2 Science", "Q_2 Math", "Q_2 Writing", "Q_2 Reading", "Q_2 Lang. Arts")], na.rm =T)
# stlist$avgrade3 <- rowMeans(stlist[, c("Q_3 Science", "Q_3 Math", "Q_3 Writing", "Q_3 Reading", "Q_3 Lang. Arts")], na.rm =T)# will give an error before 3rd quarter outcomes are entered
# stlist$avgrade4 <- rowMeans(stlist[, c("Q_4 Science", "Q_4 Math", "Q_4 Writing", "Q_4 Reading", "Q_4 Lang. Arts")], na.rm =T)# will give an error before 4th quarter outcomes are entered
# stlist$avgrade <- rowMeans(stlist[, colnames(stlist) %in% c("avgrade1", "avgrade2", "avgrade3", "avgrade4")], na.rm = T)
# stlist$nogrades <- is.na(stlist$avgrade)
# stlist$nogrades1 <- is.na(stlist$avgrade1)
# stlist$nogrades2 <- is.na(stlist$avgrade2)
# stlist$nogrades3 <- is.na(stlist$avgrade3)
# stlist$nogrades4 <- is.na(stlist$avgrade4)




#This section creates a new variable, criteria, which calculates the number of eligibility criteria a student meets. #####





stlist$`Q_1 criteria` <- 0
stlist$`Q_2 criteria` <- 0
stlist$`Q_3 criteria` <- 0
stlist$`Q_4 criteria` <- 0
stlist$criteria <- 0



criteria_cats <- c(117:120)

abs_cats <- 9

susp_cats <- c(13)

q1_subjects <- c("Q1_Math","Q1_Science","Q1_ELA")


for(i in q1_subjects){
  
  stlist$`Q_1 criteria` <- ifelse(is.element(stlist$School, elem) & stlist$`Q_1 criteria` != 1 & (stlist[,i] <= 2 & !is.na(stlist[,i])), stlist$`Q_1 criteria`+ 1, stlist$`Q_1 criteria`)
  stlist$`Q_1 criteria` <- ifelse(is.element(stlist$School, high) & stlist$`Q_1 criteria` != 1 & (stlist[,i] < 70 & !is.na(stlist[,i])), stlist$`Q_1 criteria` + 1, stlist$`Q_1 criteria`)
  
}




stlist$`Q_1 criteria` <- ifelse(stlist$`Q1_Suspensions` == 0 | is.na(stlist$`Q1_Suspensions`) , stlist$`Q_1 criteria`, stlist$`Q_1 criteria` + 1)

stlist$`Q_1 criteria` <- ifelse(stlist$`Q1_Attendance Rate` > 90 | is.na(stlist$`Q1_Attendance Rate`), stlist$`Q_1 criteria`, stlist$`Q_1 criteria` + 1)


stlist$max_criteria <- pmax(stlist$`Q_1 criteria`, stlist$`Q_2 criteria`, stlist$`Q_3 criteria`, stlist$`Q_4 criteria`)


#Creates no-metric column

stlist$no_metrics <- FALSE

metrics_colums <- c("Q1_Science", "Q1_Math", "Q1_ELA","Q1_Suspensions", "Q1_Attendance Rate")


stlist$no_metrics <- (rowSums(is.na(stlist[, metrics_colums])) == length(metrics_colums))


write.csv(stlist, "studentlist.csv")


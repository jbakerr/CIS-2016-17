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


# Adding service aggregates to student list
stserv <- data %>% group_by(Student) %>% summarize(Hours = sum(Hours), num_serv = length(Student.ID), service_date = tail(Support.Date, n =1 ))
#checkcounts <- data[data$checkin != 0 , ] %>% group_by(Student.ID) %>% summarize(num_check = n())
#parentcounts <- data[data$parent1on1 != 0, ] %>% group_by(Student.ID) %>% summarize(num_parent1on1 = n())
#anyfamcounts <- data[data$anyfamily != 0, ] %>% group_by(Student.ID) %>% summarize(num_anyfamily = n())

colnames(stserv)[1] <- "Student.Name"
colnames(progress)[3] <- "Student.Name"


stlist <- merge(progress, stserv, by = "Student.Name", all = T)


#This section creates a new variable, criteria, which calculates the number of eligibility criteria a student meets. #####





stlist$`Q_1 criteria` <- 0
stlist$`Q_2 criteria` <- 0
stlist$`Q_3 criteria` <- 0
stlist$`Q_4 criteria` <- 0
stlist$criteria <- 0



criteria_cats <- c(117:120)

abs_cats <- 9

susp_cats <- c(13)

q1_subjects <- c(10,11,12)


for(i in q1_subjects){
  
  stlist$`Q_1 criteria` <- ifelse(is.element(stlist$School, elem) & stlist$`Q_1 criteria` != 1 & (stlist[,i] <= 2 & !is.na(stlist[,i])), stlist$`Q_1 criteria`+ 1, stlist$`Q_1 criteria`)
  stlist$`Q_1 criteria` <- ifelse(is.element(stlist$School, high) & stlist$`Q_1 criteria` != 1 & (stlist[,i] < 70 & !is.na(stlist[,i])), stlist$`Q_1 criteria` + 1, stlist$`Q_1 criteria`)
  
}




stlist$`Q_1 criteria` <- ifelse(stlist$`Q1   Suspensions` == 0 | is.na(stlist$`Q1   Suspensions`) | stlist$`Q1   Suspensions` == 0, stlist$`Q_1 criteria`, stlist$`Q_1 criteria` + 1)

stlist$`Q_1 criteria` <- ifelse(stlist$`Q1   Attendance Rate` > 90 | is.na(stlist$`Q1   Attendance Rate`), stlist$`Q_1 criteria`, stlist$`Q_1 criteria` + 1)


stlist$max_criteria <- pmax(stlist$`Q_1 criteria`, stlist$`Q_2 criteria`, stlist$`Q_3 criteria`, stlist$`Q_4 criteria`)



write.csv(stlist, "studentlist.csv")


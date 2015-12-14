#install.packages("dplyr") # These packages need to be installed once, after that they can be called up using library. This package is for restructuring data
#install.packages("XLConnect") # This package is for loading / writing excel spreadsheets
#install.packages("plyr")
library(plyr) # ****IMPORTANT**** Load plyr before dplyr- they have some of the same named functions, and if you load in a different order it will cause problems
library(dplyr)
library(tidyr)
library(XLConnect)
###################################      SERVICES DATA CHECK      ##########################################
# Set the working directory to the local folder containing dataset. Can be done manually
# by going to Session -> Set Working Directory -> Choose Directory 
setwd("~/Dropbox/CIS Data")

# This is the name I have been using for the detailed service dataset, however we may want to start using a different naming convention
# This is the "Tier II and III Support Detail" report. 
# **** IMPORTANT ***** delete the last row that sums all hours before saving the excel file. That row will cause problems
data<-readWorksheetFromFile('ServiceD1516.xls', sheet=1, header = T, startRow = 5)
colnames(data)[1] <- "Student.ID"
data <- data[!is.na(data$Student.ID), ] # get rid of accidental blank rows
data <- data[as.Date(data$Begin.Date) > as.Date("8aug2015","%d%b%Y" ), ]


# Create dataset of all observations with missing service providers for CIS staff (note- Service.Provider.Type for 1415)
noprovider <- data[data$Provider.Type == "CIS Staff" & is.na(data$Provider.Name), ]
data$flag.np <- data$Provider.Type == "CIS Staff" & is.na(data$Provider.Name)

# Create dataset of all observations with missing support names
nosupport <- data[is.na(data$Student.Support.Name), ]
data$flag.ns <- is.na(data$Student.Support.Name) # flag original data set

#flag individual hours entered through batch entry####
# Create dataset for same notes same hours on same day individual entry (ie- batch entry individual service)
indiv <- data[data$Recorded.As == "Individual", ]
attach(indiv)
indiv$dup <- duplicated(cbind(Notes, Home.School, Provider.Name, Begin.Date, End.Date))
indivbatch <- indiv[indiv$dup == TRUE, ]

indivbatchsum <- indiv %>% group_by(Provider.Name, Home.School, Begin.Date, End.Date, Student.Support.Name, Student.Support.Category, Notes) %>%
  summarize(numstudents = n())
indivbatchsum <- indivbatchsum[indivbatchsum$numstudents > 1, ]
detach(indiv)

attach(data)
#Quick fix, need to edit this later, this misses (doesn't flag) one observation for every batch entry
data$flag.ib <- Recorded.As == "Individual" & duplicated(cbind(Notes, Provider.Name, Begin.Date, End.Date)) # flag original dataset
detach(data)

# flag entries with different begin/end dates #####
# create dummy for start/end dates not the same
data$flag.bd <- data$Begin.Date != data$End.Date # flag original dataset
baddates <- data[data$flag.bd, ]
byentry <- data %>% group_by(Provider.Name, Begin.Date, End.Date, flag.bd, Recorded.As, Notes) %>% summarize(Hours = sum(Hours), number_entries = n() )
baddatesum <- byentry[byentry$flag.bd == TRUE, ]

# flag entries with too many individual hours for one provider on one day#####
# sum all individual / group hours in a given day
# Note: in 14-15 datasets "Provider" is called "Service.Provider" and must be changed below in group_by
bydays <- data %>% group_by(Provider.Name, Home.School, Begin.Date, End.Date, flag.bd, Recorded.As) %>% summarize(Hours = sum(Hours), baddate = sum(flag.bd))

# create dataset of days where there are more than 10 individual hours, not including those with bad dates (different start / end)
toomanydate <- bydays[bydays$Recorded.As == "Individual" &  bydays$Hours > 10  &  bydays$baddate ==FALSE,c("Provider.Name", "Home.School", "Begin.Date", "End.Date") ]
toomanyhours <- data[alply(data[ ,c("Provider.Name", "Home.School", "Begin.Date", "End.Date")], 1) %in% alply(toomanydate, 1),  ]
toomanyhourssum <- bydays[bydays$Hours > 10 & bydays$Recorded.As == "Individual", ]
data$flag.tmh <- alply(data[ ,c("Provider.Name", "Begin.Date", "End.Date")], 1) %in% alply(toomanydate, 1) # flag original dataset

#Creating groupsize, hoursspent, individual, group, etc. ####
d <- data %>% group_by(Home.School, Begin.Date, End.Date, Provider.Type, Provider.Name, Recorded.As, Student.Support.Category, 
                       Hours, Tier, Notes) %>% summarize(groupsize = n())
data <- merge(data, d, by = c("Home.School", "Begin.Date", "End.Date", "Provider.Type", "Provider.Name", "Recorded.As", "Student.Support.Category", 
                              "Hours", "Tier", "Notes"))
data$hoursspent <- data$Hours/data$groupsize
data$individual <-  0
data[(!is.na(data$Recorded.As)) & data$Recorded.As == "Individual", ]$individual = data[ (!is.na(data$Recorded.As)) & data$Recorded.As == "Individual", ]$Hours
data$group <- 0
data[(!is.na(data$Recorded.As)) & data$Recorded.As == "Group Setting", ]$group = data[ (!is.na(data$Recorded.As)) & data$Recorded.As == "Group Setting", ]$Hours

# Save and Reloading datasets to fix data class issues that I don't fully understand ********####
write.csv(baddates, "baddate.csv")
baddates <- read.csv("baddate.csv")

write.csv(toomanyhourssum, "toomanyhourssum.csv")
toomanyhourssum <- read.csv("toomanyhourssum.csv")

write.csv(indivbatchsum, "indivbatchsum.csv")
indivbatchsum <- read.csv("indivbatchsum.csv")

setwd("~/Dropbox/Data Checks")

oldfiles <- c("~/Dropbox/Data Checks/Old")
movefiles <- list.files(path = "~/Dropbox/Data Checks/", pattern =".xlsx", all.files = FALSE, recursive = FALSE, include.dirs = FALSE)
file.copy(from=movefiles, to=oldfiles, 
         overwrite = FALSE, recursive = FALSE, 
        copy.mode = TRUE)
file.remove(movefiles, recursive = FALSE)

# write datasets of problem issues for all schools to an excel spreadsheet ####
setwd("~/Dropbox/Data Checks")
SERV<-loadWorkbook (paste("Data Check ", as.character(Sys.Date()),".xlsx") , create = TRUE )
createSheet ( SERV, "No Service Provider")
writeWorksheet(SERV,noprovider,"No Service Provider")
createSheet ( SERV, "No Support")
writeWorksheet(SERV,nosupport,"No Support")
createSheet ( SERV, "Individual as Batch")
writeWorksheet(SERV,indivbatch,"Individual as Batch")
createSheet ( SERV, "Individual Batch Summary" )
writeWorksheet(SERV,indivbatchsum,"Individual Batch Summary")
createSheet ( SERV, "Too many ind. hours sum" )
writeWorksheet(SERV,toomanyhourssum,"Too many ind. hours sum")
createSheet ( SERV, "Too many ind. hours full" )
writeWorksheet(SERV,toomanyhours,"Too many ind. hours full")
createSheet(SERV, "Diff. Begin-End Dates Sum")
writeWorksheet(SERV, baddates, "Diff. Begin-End Dates Sum")
saveWorkbook(SERV)

#write data set of hillside's errors
hillside<-loadWorkbook (paste("Hillside Data Check", as.character(Sys.Date()),".xlsx"), create = TRUE)
createSheet(hillside, "No Service Provider")
writeWorksheet(hillside, subset(noprovider, Home.School == "Hillside High School"), "No Service Provider")
createSheet(hillside, "No Support")
writeWorksheet(hillside, subset(nosupport, Home.School == "Hillside High School"), "No Support")
createSheet(hillside, "Individual as Batch")
writeWorksheet(hillside, subset(indivbatch, Home.School == "Hillside High School"), "Individual as Batch")
createSheet(hillside, "Individual as Batch Summary")
writeWorksheet(hillside, subset(indivbatchsum, Home.School == "Hillside High School"), "Individual as Batch Summary")
createSheet(hillside, "Too many ind. hours sum")
writeWorksheet(hillside, subset(toomanyhourssum, Home.School == "Hillside High School"), "Too many ind. hours sum")
createSheet(hillside, "Too many ind. hours full")
writeWorksheet(hillside, subset(toomanyhours, Home.School == "Hillside High School"), "Too many ind. hours full")
createSheet(hillside, "Diff. Begin-End Dates Sum")
writeWorksheet(hillside, subset(baddates, Home.School == "Hillside High School"), "Diff. Begin-End Dates Sum")
saveWorkbook(hillside)

#write data set of plc's errors
plc<-loadWorkbook (paste("PLC Data Check", as.character(Sys.Date()),".xlsx"), create = TRUE)
createSheet(plc, "No Service Provider")
writeWorksheet(plc, subset(noprovider, Home.School == "Durham Performance Learning Center"), "No Service Provider")
createSheet(plc, "No Support")
writeWorksheet(plc, subset(nosupport, Home.School == "Durham Performance Learning Center"), "No Support")
createSheet(plc, "Individual as Batch")
writeWorksheet(plc, subset(indivbatch, Home.School == "Durham Performance Learning Center"), "Individual as Batch")
createSheet(plc, "Individual as Batch Summary")
writeWorksheet(plc, subset(indivbatchsum, Home.School == "Durham Performance Learning Center"), "Individual as Batch Summary")
createSheet(plc, "Too many ind. hours sum")
writeWorksheet(plc, subset(toomanyhourssum, Home.School == "Durham Performance Learning Center"), "Too many ind. hours sum")
createSheet(plc, "Too many ind. hours full")
writeWorksheet(plc, subset(toomanyhours, Home.School == "Durham Performance Learning Center"), "Too many ind. hours full")
createSheet(plc, "Diff. Begin-End Dates Sum")
writeWorksheet(plc, subset(baddates, Home.School == "Durham Performance Learning Center"), "Diff. Begin-End Dates Sum")
saveWorkbook(plc)

#write data set of Eno's errors
eno<-loadWorkbook (paste("Eno Valley Data Check", as.character(Sys.Date()),".xlsx"), create = TRUE)
createSheet(eno, "No Service Provider")
writeWorksheet(eno, subset(noprovider, Home.School == "Eno Valley Elementary"), "No Service Provider")
createSheet(eno, "No Support")
writeWorksheet(eno, subset(nosupport, Home.School == "Eno Valley Elementary"), "No Support")
createSheet(eno, "Individual as Batch")
writeWorksheet(eno, subset(indivbatch, Home.School == "Eno Valley Elementary"), "Individual as Batch")
createSheet(eno, "Individual as Batch Summary")
writeWorksheet(eno, subset(indivbatchsum, Home.School == "Eno Valley Elementary"), "Individual as Batch Summary")
createSheet(eno, "Too many ind. hours sum")
writeWorksheet(eno, subset(toomanyhourssum, Home.School == "Eno Valley Elementary"), "Too many ind. hours sum")
createSheet(eno, "Too many ind. hours full")
writeWorksheet(eno, subset(toomanyhours, Home.School == "Eno Valley Elementary"), "Too many ind. hours full")
createSheet(eno, "Diff. Begin-End Dates Sum")
writeWorksheet(eno, subset(baddates, Home.School == "Eno Valley Elementary"), "Diff. Begin-End Dates Sum")
saveWorkbook(eno)

#write data set of YE Smith's errors
ye<-loadWorkbook (paste("YE Smith Data Check", as.character(Sys.Date()),".xlsx"), create = TRUE)
createSheet(ye, "No Service Provider")
writeWorksheet(ye, subset(noprovider, Home.School == "YE Smith Elementary"), "No Service Provider")
createSheet(ye, "No Support")
writeWorksheet(ye, subset(nosupport, Home.School == "YE Smith Elementary"), "No Support")
createSheet(ye, "Individual as Batch")
writeWorksheet(ye, subset(indivbatch, Home.School == "YE Smith Elementary"), "Individual as Batch")
createSheet(ye, "Individual as Batch Summary")
writeWorksheet(ye, subset(indivbatchsum, Home.School == "YE Smith Elementary"), "Individual as Batch Summary")
createSheet(ye, "Too many ind. hours sum")
writeWorksheet(ye, subset(toomanyhourssum, Home.School == "YE Smith Elementary"), "Too many ind. hours sum")
createSheet(ye, "Too many ind. hours full")
writeWorksheet(ye, subset(toomanyhours, Home.School == "YE Smith Elementary"), "Too many ind. hours full")
createSheet(ye, "Diff. Begin-End Dates Sum")
writeWorksheet(ye, subset(baddates, Home.School == "YE Smith Elementary"), "Diff. Begin-End Dates Sum")
saveWorkbook(ye)


#write data set of Ek Powe's errors
ek<-loadWorkbook (paste("Ek Powe Data Check", as.character(Sys.Date()),".xlsx"), create = TRUE)
createSheet(ek, "No Service Provider")
writeWorksheet(ek, subset(noprovider, Home.School == "EK Powe Elementary School"), "No Service Provider")
createSheet(ek, "No Support")
writeWorksheet(ek, subset(nosupport, Home.School == "EK Powe Elementary School"), "No Support")
createSheet(ek, "Individual as Batch")
writeWorksheet(ek, subset(indivbatch, Home.School == "EK Powe Elementary School"), "Individual as Batch")
createSheet(ek, "Individual as Batch Summary")
writeWorksheet(ek, subset(indivbatchsum, Home.School == "EK Powe Elementary School"), "Individual as Batch Summary")
createSheet(ek, "Too many ind. hours sum")
writeWorksheet(ek, subset(toomanyhourssum, Home.School == "EK Powe Elementary School"), "Too many ind. hours sum")
createSheet(ek, "Too many ind. hours full")
writeWorksheet(ek, subset(toomanyhours, Home.School == "EK Powe Elementary School"), "Too many ind. hours full")
createSheet(ek, "Diff. Begin-End Dates Sum")
writeWorksheet(ek, subset(baddates, Home.School == "EK Powe Elementary School"), "Diff. Begin-End Dates Sum")
saveWorkbook(ek)

#write data set of Glenn's errors
glenn<-loadWorkbook (paste("Glenn Data Check", as.character(Sys.Date()),".xlsx"), create = TRUE)
createSheet(glenn, "No Service Provider")
writeWorksheet(glenn, subset(noprovider, Home.School == "Glenn Elementary School"), "No Service Provider")
createSheet(glenn, "No Support")
writeWorksheet(glenn, subset(nosupport, Home.School == "Glenn Elementary School"), "No Support")
createSheet(glenn, "Individual as Batch")
writeWorksheet(glenn, subset(indivbatch, Home.School == "Glenn Elementary School"), "Individual as Batch")
createSheet(glenn, "Individual as Batch Summary")
writeWorksheet(glenn, subset(indivbatchsum, Home.School == "Glenn Elementary School"), "Individual as Batch Summary")
createSheet(glenn, "Too many ind. hours sum")
writeWorksheet(glenn, subset(toomanyhourssum, Home.School == "Glenn Elementary School"), "Too many ind. hours sum")
createSheet(glenn, "Too many ind. hours full")
writeWorksheet(glenn, subset(toomanyhours, Home.School == "Glenn Elementary School"), "Too many ind. hours full")
createSheet(glenn, "Diff. Begin-End Dates Sum")
writeWorksheet(glenn, subset(baddates, Home.School == "Glenn Elementary School"), "Diff. Begin-End Dates Sum")
saveWorkbook(glenn)

#write data set of Northern's errors
northern<-loadWorkbook (paste("Northern Data Check", as.character(Sys.Date()),".xlsx"), create = TRUE)
createSheet(northern, "No Service Provider")
writeWorksheet(northern, subset(noprovider, Home.School == "Northern"), "No Service Provider")
createSheet(northern, "No Support")
writeWorksheet(northern, subset(nosupport, Home.School == "Northern"), "No Support")
createSheet(northern, "Individual as Batch")
writeWorksheet(northern, subset(indivbatch, Home.School == "Northern"), "Individual as Batch")
createSheet(northern, "Individual as Batch Summary")
writeWorksheet(northern, subset(indivbatchsum, Home.School == "Northern"), "Individual as Batch Summary")
createSheet(northern, "Too many ind. hours sum")
writeWorksheet(northern, subset(toomanyhourssum, Home.School == "Northern"), "Too many ind. hours sum")
createSheet(northern, "Too many ind. hours full")
writeWorksheet(northern, subset(toomanyhours, Home.School == "Northern"), "Too many ind. hours full")
createSheet(northern, "Diff. Begin-End Dates Sum")
writeWorksheet(northern, subset(baddates, Home.School == "Northern"), "Diff. Begin-End Dates Sum")
saveWorkbook(northern)

#write data set of Southern's errors
southern<-loadWorkbook (paste("Southern Data Check", as.character(Sys.Date()),".xlsx"), create = TRUE)
createSheet(southern, "No Service Provider")
writeWorksheet(southern, subset(noprovider, Home.School == "Southern High School"), "No Service Provider")
createSheet(southern, "No Support")
writeWorksheet(southern, subset(nosupport, Home.School == "Southern High School"), "No Support")
createSheet(southern, "Individual as Batch")
writeWorksheet(southern, subset(indivbatch, Home.School == "Southern High School"), "Individual as Batch")
createSheet(southern, "Individual as Batch Summary")
writeWorksheet(southern, subset(indivbatchsum, Home.School == "Southern High School"), "Individual as Batch Summary")
createSheet(southern, "Too many ind. hours sum")
writeWorksheet(southern, subset(toomanyhourssum, Home.School == "Southern High School"), "Too many ind. hours sum")
createSheet(southern, "Too many ind. hours full")
writeWorksheet(southern, subset(toomanyhours, Home.School == "Southern High School"), "Too many ind. hours full")
createSheet(southern, "Diff. Begin-End Dates Sum")
writeWorksheet(southern, subset(baddates, Home.School == "Southern High School"), "Diff. Begin-End Dates Sum")
saveWorkbook(southern)

#write data set of Neal's errors
neal<-loadWorkbook (paste("Neal Data Check", as.character(Sys.Date()),".xlsx"), create = TRUE)
createSheet(neal, "No Service Provider")
writeWorksheet(neal, subset(noprovider, Home.School == "Neal Middle School"), "No Service Provider")
createSheet(neal, "No Support")
writeWorksheet(neal, subset(nosupport, Home.School == "Neal Middle School"), "No Support")
createSheet(neal, "Individual as Batch")
writeWorksheet(neal, subset(indivbatch, Home.School == "Neal Middle School"), "Individual as Batch")
createSheet(neal, "Individual as Batch Summary")
writeWorksheet(neal, subset(indivbatchsum, Home.School == "Neal Middle School"), "Individual as Batch Summary")
createSheet(neal, "Too many ind. hours sum")
writeWorksheet(neal, subset(toomanyhourssum, Home.School == "Neal Middle School"), "Too many ind. hours sum")
createSheet(neal, "Too many ind. hours full")
writeWorksheet(neal, subset(toomanyhours, Home.School == "Neal Middle School"), "Too many ind. hours full")
createSheet(neal, "Diff. Begin-End Dates Sum")
writeWorksheet(neal, subset(baddates, Home.School == "Neal Middle School"), "Diff. Begin-End Dates Sum")
saveWorkbook(neal)


# Write the flaged data
attach(data)
data$flagged <- flag.np | flag.ns | flag.ib | flag.bd | flag.tmh
detach(data)

setwd("~/Dropbox/CIS Data")

unlink("ServiceD1516CL.csv", recursive = FALSE, force = FALSE)
write.csv(data, "ServiceD1516CL.csv")



##################################      OUTCOME DATA CHECK      ###############################



setwd("~/Dropbox/CIS Data")

cs<-readWorksheetFromFile('Caselist1516.xls', sheet=1, header = T, startRow = 4)

attend <- readWorksheetFromFile('Attendance1516.xls', sheet=1, header = T, startRow = 5)
attend <- attend[, c("Site", "Case.ID", "Name", "Grade.Level", "Outcome.Item","Report.Period", "Value")]

risk <- readWorksheetFromFile('TQS1516.xls', sheet=1, header=T, startRow = 3)
risk <- risk[, c("Case.ID", "X..Goals", "X..Risk.Factors")]


attend <- attend[attend$Outcome.Item %in% c("Unexcused Absence", "Excused Absence", "ISS", "OSS"), ]

# flag entries that have duplicated student, report period, outcome item #####
attend1$dup <- duplicated(attend1[, c("Site", "Case.ID", "Name", "Grade.Level", "Outcome.Item","Report.Period")])
attend1 <- attend1[order(!attend1$dup), ]
attend1$dup2 <- duplicated(attend1[, c("Site", "Case.ID", "Name", "Grade.Level", "Outcome.Item","Report.Period")])
attend1 <- attend1[attend1$dup | attend1$dup2, ]
write.csv(attend1, "attendance_duplicates.csv")
############### Above csv is for you and/or Sheri to check on duplicates with the GC's
attend <- attend[!duplicated(attend[,c("Site", "Case.ID", "Name", "Grade.Level", "Outcome.Item","Report.Period")]), ] # This is a soft option that just deletes one of the duplicates
attend <- spread(attend, Outcome.Item, Value)
attend <- attend[, c("Case.ID", "Excused Absence", "Unexcused Absence", "ISS", "OSS")] 

grades <-  readWorksheetFromFile('Grades1516.xls', sheet=1, header = T, startRow = 5)

grades <- grades[, c("Site", "Case.ID", "Name", "Grade.Level", "Outcome.Item","Report.Period", "Value")]

grades<- grades[ grades$Report.Period %in% c("1st Grading Period", "Baseline"), ]
########### flag entries that have duplicated student, report period, outcome item
grades1 <- grades
grades1$dup <- duplicated(grades1[, c("Site", "Case.ID", "Name", "Grade.Level", "Outcome.Item","Report.Period")])
grades1 <- grades1[order(!grades1$dup), ]
grades1$dup2 <- duplicated(grades1[, c("Site", "Case.ID", "Name", "Grade.Level", "Outcome.Item","Report.Period")])
grades1 <- grades1[grades1$dup | grades1$dup2, ]
write.csv(grades1, "grades_duplicates.csv")
############### Above csv is for you and/or Sheri to check on duplicates with the GC's


#Merging grades and attendance, and aggregate service info to stlist #####
grades <- spread(grades, Outcome.Item, Value)
grades <- grades[, c("Case.ID", "Lang. Arts","Math", "Other", "Reading", "Science", "Writing")]
stlist <- merge(cs, attend, by = "Case.ID", all = T)
stlist <- merge(stlist, grades, by = "Case.ID", all = T)
stlist <- merge(stlist, risk, by = "Case.ID", all = T)

stlist$avgrade <- rowMeans(stlist[, c("Science", "Math", "Writing", "Reading", "Lang. Arts")], na.rm =T)
stlist$nogrades <- is.na(stlist$avgrade)
stlist$totabs <- rowSums(stlist[, c("Excused Absence", "Unexcused Absence")], na.rm = T)

stlist$noabs <- is.na(stlist$"Excused Absence") & is.na(stlist$"Unexcused Absence")
stlist[stlist$noabs, ]$totabs<- NA 

stlist$suspended <- (is.na(stlist$ISS) & stlist$OSS > 0)|(is.na(stlist$OSS) & stlist$ISS > 0)|(stlist$ISS > 0 | stlist$OSS > 0)
stserv <- data %>% group_by(Student.ID) %>% summarize(Hours = sum(Hours), HoursSpent = sum(hoursspent), individual = sum(individual), group = sum(group), num_serv = length(Student.ID) )
colnames(stserv)[1] <- "Case.ID"

stlist <- merge(stlist, stserv, by = "Case.ID", all = T)

#Students with NA for Hours are students present on caseload and not present in our service data 
stlist[is.na(stlist$Hours), ]$Hours <- 0
stlist[is.na(stlist$HoursSpent), ]$HoursSpent <- 0
stlist[is.na(stlist$individual), ]$individual <- 0
stlist[is.na(stlist$group), ]$group <- 0


stlist <- stlist[!is.na(stlist$Name), ]

#This section creates a new variable, criteria, which calculates the number of eligibility criteria a student meets. #####
elem <- c("Glenn Elementary School", "Eno Valley Elementary", "EK Powe Elementary School", "YE Smith Elementary")
high <- c("Neal Middle School", "Durham Performance Learning Center", "Hillside High School", "Southern High School", "Northern")

stlist$criteria <- 0

stlist$criteria <-  ifelse(is.element(stlist$Site, elem) & (stlist$`Lang. Arts` <= 2 & !is.na(stlist$`Lang. Arts`)), stlist$criteria + 1, stlist$criteria)
stlist$criteria <-  ifelse(is.element(stlist$Site, elem) & stlist$criteria != 1 & (stlist$Math <= 2 & !is.na(stlist$Math)), stlist$criteria + 1, stlist$criteria)
stlist$criteria <-  ifelse(is.element(stlist$Site, elem) & stlist$criteria != 1 & (stlist$Science <= 2 & !is.na(stlist$Science)), stlist$criteria + 1, stlist$criteria)
stlist$criteria <- ifelse(stlist$Site == "YE Smith Elementary" & (stlist$Writing <=2 & !is.na(stlist$Math)), stlist$criteria + 1, stlist$criteria)

stlist$criteria <-  ifelse(is.element(stlist$Site, high) & (stlist$`Lang. Arts` < 70 & !is.na(stlist$`Lang. Arts`)), stlist$criteria + 1, stlist$criteria)
stlist$criteria <-  ifelse(is.element(stlist$Site, high) & stlist$criteria != 1 & (stlist$Math < 70 & !is.na(stlist$Math)), stlist$criteria + 1, stlist$criteria)
stlist$criteria <-  ifelse(is.element(stlist$Site, high) & stlist$criteria != 1 & (stlist$Science < 70  & !is.na(stlist$Science)), stlist$criteria + 1, stlist$criteria)


stlist$criteria <- ifelse(stlist$suspended == FALSE | is.na(stlist$suspended), stlist$criteria, stlist$criteria + 1)
stlist$criteria <- ifelse(stlist$totabs < 4 | is.na(stlist$totabs), stlist$criteria, stlist$criteria + 1)

#Write studentlist to the working directory ####
unlink("studentlist.csv", recursive = FALSE, force = FALSE)

write.csv(stlist, "studentlist.csv")


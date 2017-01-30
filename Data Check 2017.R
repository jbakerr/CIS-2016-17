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

# This is the name I have been using for the detailed service dataset, however we may want to start using a different naming convention
# This is the "Tier II and III Support Detail" report. 
# **** IMPORTANT ***** delete the last row that sums all hours before saving the excel file. That row will cause problems
data<-readWorksheetFromFile('ServiceD.xlsx', sheet=1, header = T, startRow = 2)
colnames(data)[1] <- "Student.ID"
data <- data[!is.na(data$Student.ID), ] # get rid of accidental blank rows


#removing Nina's test data
data <- subset(data, !data$Entered.By == "nina")





#Progress Monitoring

#Load Worksheet
progress<-readWorksheetFromFile('Progress-Monitoring.xlsx', sheet=1, header = T, startRow = 1)

#Rename Grading Quarters
colnames(progress)[7:9] <- c("Q1", "Q2", "Q3")
#Removing Test Data
progress <- subset(progress, !progress$Case.Manager == "Nina Wilson")

#Changing Metric Title
progress$Metric[progress$Metric =='Core Course Grades: Eng/Lang Arts/Reading/Writing'] <- "ELA"

progress$Metric[progress$Metric =='Core Course Grades: Math 1'] <- "Math"

progress$Metric[progress$Metric =='Core Course Grades: Science'] <- "Science"

progress$Metric[progress$Metric =='Standardized test score: English / Language Arts'] <- "ELA"

progress$Metric[progress$Metric =='Standardized test score: Science'] <- "Science"





metrics <- c("Math","Science","ELA", "Suspensions", "Attendance Rate")




elem <- c("Glenn Elementary School", "Eno Valley Elementary", "EK Powe Elementary School", "YE Smith Elementary")
high <- c("Neal Middle School", "Durham Performance Learning Center", "Hillside High School", "Southern High School", "Northern")

#Adjusting Attendance Rate from days to percentage
elem.adjust <- progress$Q1[progress$Metric == "Attendance Rate" & !is.na(progress$Q1) & (progress$School %in% elem) & progress$Q1 < 50]
elem.adjust <- (45-elem.adjust)/100

progress$Q1[progress$Metric == "Attendance Rate" & progress$School %in% elem & progress$Q1 < 50 & !is.na(progress$Q1)] <- elem.adjust

high.adjust <- progress$Q1[progress$Metric == "Attendance Rate" & !is.na(progress$Q1) & (progress$School %in% high) & progress$Q1 < 1]
high.adjust <- high.adjust *100

progress$Q1[progress$Metric == "Attendance Rate" & !is.na(progress$Q1) & (progress$School %in% high) & progress$Q1 < 1] <- high.adjust

summary(subset(progress$Q1, progress$Metric == "Attendance Rate" & progress$School %in% high))



progress_short <- subset(progress, progress$Metric %in% metrics)

progress_short <- progress_short[,1:7]
progress_short <- progress_short[,-6]



long_progress <- gather(progress_short, Period, Value, Baseline:Q1, factor_key = T)
quartersubject <- paste(long_progress$Period, long_progress$Metric, sep = " ")
long_progress$quartersubject <- quartersubject

wide_progress <- spread(long_progress[, ! colnames(long_progress) %in% c("Metric", "Period")], quartersubject, Value)


wide_progress <- spread(long_progress, Metric, Value)

write.csv(wide_progress, "studentlist.csv")



#Spreading
quartersubject <- paste("Q_", substr(progress$Report.Period, 1,1), " ", grades$Outcome.Item, sep = "")
quartersubjectgpa <- paste(grades$Outcome.Item," ",  "Q_",substr(grades$Report.Period, 1,1), sep = "")
grades$quartersubject <- quartersubject
grades$quartersubject[grades$Outcome.Item == "GPA"] <- quartersubjectgpa[grades$Outcome.Item == "GPA"]

grades <- spread(grades[, ! colnames(grades) %in% c("Outcome.Item", "Report.Period")], quartersubject, Value)

attend$quartersubject <- paste("Q_", substr(attend$Report.Period, 1,1), " ", attend$Outcome.Item, sep = "")
attend <- spread(attend[, ! colnames(attend) %in% c("Outcome.Item", "Report.Period", "Date")], quartersubject, Value)




#Flags

# #Group / Tier Mismatch
# data$flag <- FALSE
# ifelse(subset(data, (data$Tier == "Tier II" & data$Individual.or.Group != "Group"), data$flag <- TRUE))
#        
# subset(data$flag, (data$Tier == "Tier II" & data$Individual.or.Group != "Group"))
# 
# data$flag <- TRUE
# 
#        
#        | (data$Tier == "Tier III" & data$Individual.or.Group == "Group")
# length(subset(data, (data$Tier == "Tier II" & data$Individual.or.Group != "Group")))
#   


#data <- data[as.Date(data$Begin.Date) > as.Date("8aug2015","%d%b%Y"), ] #get rid of services before school year
#data[data$Tier == "Tier I", ]$Recorded.As <- "Group Setting"
#data[data$Provider.Type == "Volunteer",]$Provider.Name <- "Volunteer"
#data[data$Provider.Type == "Community Partner",]$Provider.Name <- "Community Partner"



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
data$flag.bd <- as.Date(data$Begin.Date) != as.Date(data$End.Date) # flag original dataset
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

#Flag entries that have Recorded.As unmatched with Tier####
data$Recorded.As[data$Tier == "Tier I"] <- "Tier I"
data$flag.tier2indiv <- FALSE
data$flag.tier2indiv[data$Recorded.As == "Individual" & data$Tier == "Tier II" ] <- TRUE
data$flag.tier3group <- FALSE
data$flag.tier3group[data$Recorded.As == "Group Setting" & data$Tier == "Tier III"] <- TRUE
#setting is the service setting where there isn't contradiction between Tier and Recorded.As
data$setting <- NA
data$setting[data$Recorded.As == "Group Setting" & data$Tier == "Tier II" ] <- "Group Setting"
data$setting[data$Recorded.As == "Individual" & data$Tier == "Tier III" ] <- "Individual"
data$setting[data$Recorded.As == "Tier I"] <- "Tier I"
data$setting[data$Student.Support.Name == "Progress Monitoring/Adjustment"] <- "Individual"
badsetting <- data[data$flag.tier3group | data$flag.tier2indiv, ]

#Creating groupsize, hoursspent, individual, group, check-ins, parent contacts  etc. ####
d <- data %>% group_by(Home.School, Begin.Date, End.Date, Provider.Type, Provider.Name, setting, Student.Support.Category, 
                       Hours, Tier, Notes) %>% summarize(groupsize = n())
d$groupsize[(!is.na(d$setting)) & d$setting == "Individual"] <- 1

data <- merge(data, d, by = c("Home.School", "Begin.Date", "End.Date", "Provider.Type", "Provider.Name", "setting", "Student.Support.Category", 
                              "Hours", "Tier", "Notes"))
data$hoursspent <- data$Hours/data$groupsize
#Creating individual, group, checkin, 1on1parent, anyfamily variables
data$individual <-  0
data[(!is.na(data$Recorded.As)) & data$Recorded.As == "Individual", ]$individual <- data[ (!is.na(data$Recorded.As)) & data$Recorded.As == "Individual", ]$Hours
data$group <- 0
data[(!is.na(data$Recorded.As)) & data$Recorded.As == "Group Setting", ]$group <- data[ (!is.na(data$Recorded.As)) & data$Recorded.As == "Group Setting", ]$Hours
data$tier1 <- 0
data$tier1[(!is.na(data$Recorded.As)) & data$Recorded.As == "Tier I"] <- data[ (!is.na(data$Recorded.As)) & data$Recorded.As == "Tier I", ]$Hours

checks <- c("Student Goal Setting/ Check-in Meeting", "Check and Connect", "Case Consultation")
parent <- c("Home Visit/Parent/Care Giver Contact", "Parent Contact/Conference", "Parent Phone Call")
anyfamily <- c("Home Visit/Parent/Care Giver Contact", "Parent Contact/Conference", "Parent Phone Call", "Family Focused Event")

data$checkin <- 0
data$checkin[data$Student.Support.Name %in% checks] <- data$Hours[data$Student.Support.Name %in% checks]
data$parent1on1 <- 0
data$parent1on1[data$Student.Support.Name %in% parent] <- data$Hours[data$Student.Support.Name %in% parent]
data$anyfamily <- 0
data$anyfamily[data$Student.Support.Name %in% anyfamily] <- data$Hours[data$Student.Support.Name %in% anyfamily]

# Now create a column that checks that our groupsizes match with unambiguous group settings
data$flag.groupsize <- FALSE
data$flag.groupsize[(!is.na(data$setting)) & data$groupsize > 1 & data$setting == "Individual"] <- TRUE
data$flag.groupsize[(!is.na(data$setting)) & data$groupsize == 1 & data$setting == "Group Setting"] <- TRUE
data$flag.groupsize[(!is.na(data$setting)) & data$groupsize == 1 & data$setting == "Group Setting"] <- TRUE

badgroupsize <- data[data$flag.groupsize, ]



# Save and Reloading datasets to fix data class issues, Dropbox File Management********####
write.csv(baddates, "baddate.csv")
baddates <- read.csv("baddate.csv")

write.csv(toomanyhourssum, "toomanyhourssum.csv")
toomanyhourssum <- read.csv("toomanyhourssum.csv")

write.csv(indivbatchsum, "indivbatchsum.csv")
indivbatchsum <- read.csv("indivbatchsum.csv")

mac_datacheck <- "~/Dropbox/Data Checks"
windows_datacheck <- "C:/Users/USER/Dropbox/Data Checks"

indivbatch <- indivbatch[indivbatch$Begin.Date > as.POSIXct("2016-01-22", "%Y-%m-%d", tz = "EST"),]
indivbatchsum <- indivbatchsum[indivbatchsum$Begin.Date > as.POSIXct("2016-01-22", "%Y-%m-%d", tz = "EST"),]
#noprovider <- noprovider[noprovider$Begin.Date > as.POSIXct("2016-01-22", "%Y-%m-%d", tz = "EST"),]
badsetting <- badsetting[badsetting$Begin.Date > as.POSIXct("2016-01-22", "%Y-%m-%d", tz = "EST"),]
nosupport <- nosupport[nosupport$Begin.Date > as.POSIXct("2016-01-22", "%Y-%m-%d", tz = "EST"),]
baddates <- baddates[baddates$Begin.Date > as.POSIXct("2016-01-22", "%Y-%m-%d", tz = "EST"),]
baddatesum <- baddatesum[baddatesum$Begin.Date > as.POSIXct("2016-01-22", "%Y-%m-%d", tz = "EST"),]
toomanyhours <- toomanyhours[toomanyhours$Begin.Date > as.POSIXct("2016-01-22", "%Y-%m-%d", tz = "EST"),]
toomanyhourssum <- toomanyhourssum[toomanyhourssum$Begin.Date > as.POSIXct("2016-01-22", "%Y-%m-%d", tz = "EST"),]

nohours <- data[data$Hours == 0,]  



if(file.exists(mac_datacheck)){
  setwd(file.path(mac_datacheck))
} else { 
  if(file.exists(windows_datacheck)){
    setwd(file.path(windows_datacheck))
  }
}


# File management within dropbox
if(file.exists(mac_datacheck)){
  oldfiles <- c("~/Dropbox/Data Checks/Old")
  movefiles <- list.files(path = "~/Dropbox/Data Checks/", pattern =".xlsx", all.files = FALSE, recursive = FALSE, include.dirs = FALSE)
  file.copy(from=movefiles, to=oldfiles, 
            overwrite = FALSE, recursive = FALSE, 
            copy.mode = TRUE)
  file.remove(movefiles, recursive = FALSE)
} else {
  if(file.exists(windows_datacheck)) { 
    oldfiles <- c("C:/Users/USER/Dropbox/Data Checks/Old")
    movefiles <- list.files(path = "C:/Users/USER/Dropbox/Data Checks/", pattern =".xlsx", all.files = FALSE, recursive = FALSE, include.dirs = FALSE)
    file.copy(from=movefiles, to=oldfiles, 
              overwrite = FALSE, recursive = FALSE, 
              copy.mode = TRUE)
    file.remove(movefiles, recursive = FALSE)
  }
}


schools <- unique(data$Home.School)

#for (i in schools){
# print(i)} 
#


# write datasets of problem issues for all schools to an excel spreadsheet ####
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
#createSheet(SERV, "Diff. Begin-End Dates Sum")
#writeWorksheet(SERV, baddates, "Diff. Begin-End Dates Sum")
createSheet(SERV, "Setting - Tier Mismatches")
writeWorksheet(SERV, badsetting, "Setting - Tier Mismatches" )
createSheet(SERV, "No Hours")
writeWorksheet(SERV, nohours, "No Hours")
saveWorkbook(SERV)


#write data set of hillside's errors
hillside<-loadWorkbook (paste("Hillside Data Check", as.character(Sys.Date()),".xlsx"), create = TRUE)
createSheet(hillside, "No Service Provider")
writeWorksheet(hillside, subset(noprovider, Home.School == "Hillside High School"), "No Service Provider")
createSheet(hillside, "No Support")
writeWorksheet(hillside, subset(nosupport, Home.School == "Hillside High School"), "No Support")
createSheet(hillside, "No Hours")
writeWorksheet(hillside, subset(nohours, Home.School == "Hillside High School"), "No Hours")
#createSheet(hillside, "Individual as Batch")
#writeWorksheet(hillside, subset(indivbatch, Home.School == "Hillside High School"), "Individual as Batch")
#createSheet(hillside, "Individual as Batch Summary")
#writeWorksheet(hillside, subset(indivbatchsum, Home.School == "Hillside High School"), "Individual as Batch Summary")
#createSheet(hillside, "Too many ind. hours sum")
#writeWorksheet(hillside, subset(toomanyhourssum, Home.School == "Hillside High School"), "Too many ind. hours sum")
#createSheet(hillside, "Too many ind. hours full")
#writeWorksheet(hillside, subset(toomanyhours, Home.School == "Hillside High School"), "Too many ind. hours full")
#createSheet(hillside, "Diff. Begin-End Dates Sum")
#writeWorksheet(hillside, subset(baddates, Home.School == "Hillside High School"), "Diff. Begin-End Dates Sum")
#createSheet(hillside, "Setting - Tier Mismatches")
#writeWorksheet(hillside, subset(badsetting, Home.School == "Hilside High School"), "Setting - Tier Mismatches" )
saveWorkbook(hillside)

#write data set of plc's errors
plc<-loadWorkbook (paste("PLC Data Check", as.character(Sys.Date()),".xlsx"), create = TRUE)
createSheet(plc, "No Service Provider")
writeWorksheet(plc, subset(noprovider, Home.School == "Durham Performance Learning Center"), "No Service Provider")
createSheet(plc, "No Support")
writeWorksheet(plc, subset(nosupport, Home.School == "Durham Performance Learning Center"), "No Support")
createSheet(plc, "No Hours")
writeWorksheet(plc, subset(nohours, Home.School == "Durham Performance Learning Center"), "No Hours")
#createSheet(plc, "Individual as Batch")
#writeWorksheet(plc, subset(indivbatch, Home.School == "Durham Performance Learning Center"), "Individual as Batch")
#createSheet(plc, "Individual as Batch Summary")
#writeWorksheet(plc, subset(indivbatchsum, Home.School == "Durham Performance Learning Center"), "Individual as Batch Summary")
#createSheet(plc, "Too many ind. hours sum")
#writeWorksheet(plc, subset(toomanyhourssum, Home.School == "Durham Performance Learning Center"), "Too many ind. hours sum")
#createSheet(plc, "Too many ind. hours full")
#writeWorksheet(plc, subset(toomanyhours, Home.School == "Durham Performance Learning Center"), "Too many ind. hours full")
#createSheet(plc, "Diff. Begin-End Dates Sum")
#writeWorksheet(plc, subset(baddates, Home.School == "Durham Performance Learning Center"), "Diff. Begin-End Dates Sum")
#createSheet(plc, "Setting - Tier Mismatches")
#writeWorksheet(plc, subset(badsetting, Home.School == "Durham Performance Learning Center"), "Setting - Tier Mismatches" )
saveWorkbook(plc)

#write data set of Eno's errors
eno<-loadWorkbook (paste("Eno Valley Data Check", as.character(Sys.Date()),".xlsx"), create = TRUE)
createSheet(eno, "No Service Provider")
writeWorksheet(eno, subset(noprovider, Home.School == "Eno Valley Elementary"), "No Service Provider")
createSheet(eno, "No Support")
writeWorksheet(eno, subset(nosupport, Home.School == "Eno Valley Elementary"), "No Support")
createSheet(eno, "No Hours")
writeWorksheet(eno, subset(nohours, Home.School == "Eno Valley Elementary"), "No Hours")
#createSheet(eno, "Individual as Batch")
##writeWorksheet(eno, subset(indivbatch, Home.School == "Eno Valley Elementary"), "Individual as Batch")
#createSheet(eno, "Individual as Batch Summary")
#writeWorksheet(eno, subset(indivbatchsum, Home.School == "Eno Valley Elementary"), "Individual as Batch Summary")
#createSheet(eno, "Too many ind. hours sum")
#writeWorksheet(eno, subset(toomanyhourssum, Home.School == "Eno Valley Elementary"), "Too many ind. hours sum")
#createSheet(eno, "Too many ind. hours full")
#writeWorksheet(eno, subset(toomanyhours, Home.School == "Eno Valley Elementary"), "Too many ind. hours full")
#createSheet(eno, "Setting - Tier Mismatches")
#writeWorksheet(eno, subset(badsetting, Home.School == "Eno Valley Elementary"), "Setting - Tier Mismatches" )
saveWorkbook(eno)

#write data set of YE Smith's errors
ye<-loadWorkbook (paste("YE Smith Data Check", as.character(Sys.Date()),".xlsx"), create = TRUE)
createSheet(ye, "No Service Provider")
writeWorksheet(ye, subset(noprovider, Home.School == "YE Smith Elementary"), "No Service Provider")
createSheet(ye, "No Support")
writeWorksheet(ye, subset(nosupport, Home.School == "YE Smith Elementary"), "No Support")
createSheet(ye, "No Hours")
writeWorksheet(ye, subset(nohours, Home.School == "YE Smith Elementary"), "No Hours")
#createSheet(ye, "Individual as Batch")
#writeWorksheet(ye, subset(indivbatch, Home.School == "YE Smith Elementary"), "Individual as Batch")
#createSheet(ye, "Individual as Batch Summary")
#writeWorksheet(ye, subset(indivbatchsum, Home.School == "YE Smith Elementary"), "Individual as Batch Summary")
#createSheet(ye, "Too many ind. hours sum")
#writeWorksheet(ye, subset(toomanyhourssum, Home.School == "YE Smith Elementary"), "Too many ind. hours sum")
#createSheet(ye, "Too many ind. hours full")
#writeWorksheet(ye, subset(toomanyhours, Home.School == "YE Smith Elementary"), "Too many ind. hours full")
#createSheet(ye, "Diff. Begin-End Dates Sum")
#writeWorksheet(ye, subset(baddates, Home.School == "YE Smith Elementary"), "Diff. Begin-End Dates Sum")
##createSheet(ye, "Setting - Tier Mismatches")
#writeWorksheet(ye, subset(badsetting, Home.School == "YE Smith Elementary"), "Setting - Tier Mismatches" )
saveWorkbook(ye)


#write data set of Ek Powe's errors
ek<-loadWorkbook (paste("Ek Powe Data Check", as.character(Sys.Date()),".xlsx"), create = TRUE)
createSheet(ek, "No Service Provider")
writeWorksheet(ek, subset(noprovider, Home.School == "EK Powe Elementary School"), "No Service Provider")
createSheet(ek, "No Support")
writeWorksheet(ek, subset(nosupport, Home.School == "EK Powe Elementary School"), "No Support")
createSheet(ek, "No Hours")
writeWorksheet(ek, subset(nohours, Home.School == "EK Powe Elementary School"), "No Hours")
#createSheet(ek, "Individual as Batch")
#writeWorksheet(ek, subset(indivbatch, Home.School == "EK Powe Elementary School"), "Individual as Batch")
##createSheet(ek, "Individual as Batch Summary")
#writeWorksheet(ek, subset(indivbatchsum, Home.School == "EK Powe Elementary School"), "Individual as Batch Summary")
##createSheet(ek, "Too many ind. hours sum")
#writeWorksheet(ek, subset(toomanyhourssum, Home.School == "EK Powe Elementary School"), "Too many ind. hours sum")
#createSheet(ek, "Too many ind. hours full")
#writeWorksheet(ek, subset(toomanyhours, Home.School == "EK Powe Elementary School"), "Too many ind. hours full")
#createSheet(ek, "Diff. Begin-End Dates Sum")
#writeWorksheet(ek, subset(baddates, Home.School == "EK Powe Elementary School"), "Diff. Begin-End Dates Sum")
#createSheet(ek, "Setting - Tier Mismatches")
#writeWorksheet(ek, subset(badsetting, Home.School == "EK Powe Elementary School"), "Setting - Tier Mismatches" )
saveWorkbook(ek)

#write data set of Glenn's errors
glenn<-loadWorkbook (paste("Glenn Data Check", as.character(Sys.Date()),".xlsx"), create = TRUE)
createSheet(glenn, "No Service Provider")
writeWorksheet(glenn, subset(noprovider, Home.School == "Glenn Elementary School"), "No Service Provider")
createSheet(glenn, "No Support")
writeWorksheet(glenn, subset(nosupport, Home.School == "Glenn Elementary School"), "No Support")
createSheet(glenn, "No Hours")
writeWorksheet(glenn, subset(nohours, Home.School == "Glenn Elementary School"), "No Hours")
#createSheet(glenn, "Individual as Batch")
#writeWorksheet(glenn, subset(indivbatch, Home.School == "Glenn Elementary School"), "Individual as Batch")
#createSheet(glenn, "Individual as Batch Summary")
#writeWorksheet(glenn, subset(indivbatchsum, Home.School == "Glenn Elementary School"), "Individual as Batch Summary")
#createSheet(glenn, "Too many ind. hours sum")
#writeWorksheet(glenn, subset(toomanyhourssum, Home.School == "Glenn Elementary School"), "Too many ind. hours sum")
#createSheet(glenn, "Too many ind. hours full")
#writeWorksheet(glenn, subset(toomanyhours, Home.School == "Glenn Elementary School"), "Too many ind. hours full")
#createSheet(glenn, "Diff. Begin-End Dates Sum")
#writeWorksheet(glenn, subset(baddates, Home.School == "Glenn Elementary School"), "Diff. Begin-End Dates Sum")
#createSheet(glenn, "Setting - Tier Mismatches")
#writeWorksheet(glenn, subset(badsetting, Home.School == "Glenn Elementary School"), "Setting - Tier Mismatches" )
saveWorkbook(glenn)

#write data set of Northern's errors
northern<-loadWorkbook (paste("Northern Data Check", as.character(Sys.Date()),".xlsx"), create = TRUE)
createSheet(northern, "No Service Provider")
writeWorksheet(northern, subset(noprovider, Home.School == "Northern"), "No Service Provider")
createSheet(northern, "No Support")
writeWorksheet(northern, subset(nosupport, Home.School == "Northern"), "No Support")
createSheet(northern, "No Hours")
writeWorksheet(northern, subset(nohours, Home.School == "Northern"), "No Hours")
#createSheet(northern, "Individual as Batch")
#writeWorksheet(northern, subset(indivbatch, Home.School == "Northern"), "Individual as Batch")
#createSheet(northern, "Individual as Batch Summary")
#writeWorksheet(northern, subset(indivbatchsum, Home.School == "Northern"), "Individual as Batch Summary")
#createSheet(northern, "Too many ind. hours sum")
#writeWorksheet(northern, subset(toomanyhourssum, Home.School == "Northern"), "Too many ind. hours sum")
#createSheet(northern, "Too many ind. hours full")
#writeWorksheet(northern, subset(toomanyhours, Home.School == "Northern"), "Too many ind. hours full")
#createSheet(northern, "Diff. Begin-End Dates Sum")
#writeWorksheet(northern, subset(baddates, Home.School == "Northern"), "Diff. Begin-End Dates Sum")
#createSheet(northern, "Setting - Tier Mismatches")
#writeWorksheet(northern, subset(badsetting, Home.School == "Northern"), "Setting - Tier Mismatches" )
saveWorkbook(northern)

#write data set of Southern's errors
southern<-loadWorkbook (paste("Southern Data Check", as.character(Sys.Date()),".xlsx"), create = TRUE)
createSheet(southern, "No Service Provider")
writeWorksheet(southern, subset(noprovider, Home.School == "Southern High School"), "No Service Provider")
createSheet(southern, "No Support")
writeWorksheet(southern, subset(nosupport, Home.School == "Southern High School"), "No Support")
createSheet(southern, "No Hours")
writeWorksheet(southern, subset(nohours, Home.School == "Southern High School"), "No Hours")
#createSheet(southern, "Individual as Batch")
#writeWorksheet(southern, subset(indivbatch, Home.School == "Southern High School"), "Individual as Batch")
#createSheet(southern, "Individual as Batch Summary")
#writeWorksheet(southern, subset(indivbatchsum, Home.School == "Southern High School"), "Individual as Batch Summary")
#createSheet(southern, "Too many ind. hours sum")
#writeWorksheet(southern, subset(toomanyhourssum, Home.School == "Southern High School"), "Too many ind. hours sum")
#createSheet(southern, "Too many ind. hours full")
#writeWorksheet(southern, subset(toomanyhours, Home.School == "Southern High School"), "Too many ind. hours full")
#createSheet(southern, "Diff. Begin-End Dates Sum")
#writeWorksheet(southern, subset(baddates, Home.School == "Southern High School"), "Diff. Begin-End Dates Sum")
#createSheet(southern, "Setting - Tier Mismatches")
#writeWorksheet(southern, subset(badsetting, Home.School == "Southern High School"), "Setting - Tier Mismatches" )
saveWorkbook(southern)

#write data set of Neal's errors
neal<-loadWorkbook (paste("Neal Data Check", as.character(Sys.Date()),".xlsx"), create = TRUE)
createSheet(neal, "No Service Provider")
writeWorksheet(neal, subset(noprovider, Home.School == "Neal Middle School"), "No Service Provider")
createSheet(neal, "No Support")
writeWorksheet(neal, subset(nosupport, Home.School == "Neal Middle School"), "No Support")
createSheet(neal, "No Hours")
writeWorksheet(neal, subset(nohours, Home.School == "Neal Middle School"), "No Hours")
#createSheet(neal, "Individual as Batch")
#writeWorksheet(neal, subset(indivbatch, Home.School == "Neal Middle School"), "Individual as Batch")
#createSheet(neal, "Individual as Batch Summary")
#writeWorksheet(neal, subset(indivbatchsum, Home.School == "Neal Middle School"), "Individual as Batch Summary")
#createSheet(neal, "Too many ind. hours sum")
#writeWorksheet(neal, subset(toomanyhourssum, Home.School == "Neal Middle School"), "Too many ind. hours sum")
#createSheet(neal, "Too many ind. hours full")
#writeWorksheet(neal, subset(toomanyhours, Home.School == "Neal Middle School"), "Too many ind. hours full")
#createSheet(neal, "Diff. Begin-End Dates Sum")
#writeWorksheet(neal, subset(baddates, Home.School == "Neal Middle School"), "Diff. Begin-End Dates Sum")
#createSheet(neal, "Setting - Tier Mismatches")
#writeWorksheet(neal, subset(badsetting, Home.School == "Neal Middle School"), "Setting - Tier Mismatches" )
saveWorkbook(neal)

data$improve <- ifelse(data$Student.ID %in% improve_criteria$Case.ID, data$improve <- TRUE, data$improve <- FALSE)


# Write the cleaned / flagged data ####
attach(data)
data$flagged <- flag.np | flag.ns | flag.ib | flag.bd | flag.tmh | flag.tier2indiv | flag.tier3group
detach(data)

if(file.exists(macdatawd)){
  setwd(file.path(macdatawd))
} else { 
  if(file.exists(windowsdatawd)){
    setwd(file.path(windowsdatawd))
  }
}

unlink("ServiceD1516CL.csv", recursive = FALSE, force = FALSE)
write.csv(data, "ServiceD1516CL.csv")


##################################      OUTCOME DATA CHECK      ###############################

if(file.exists(macdatawd)){
  setwd(file.path(macdatawd))
} else { 
  if(file.exists(windowsdatawd)){
    setwd(file.path(windowsdatawd))
  }
}


cs<-readWorksheetFromFile('Caselist1516.xls', sheet=1, header = T, startRow = 4)

cs <- cs[cs$X...Date.Inactivated > as.POSIXct("2016-01-01", "%Y-%m-%d", tz= "EST") | cs$Case.Status == "Active", ]


attend <- readWorksheetFromFile('Attendance1516.xls', sheet=1, header = T, startRow = 5)
attend <- attend[, c("Case.ID", "Outcome.Item","Report.Period", "Value", "Date")]
attend <- attend[attend$Outcome.Item %in% c("Unexcused Absence", "Excused Absence", "ISS", "OSS"), ]



risk <- readWorksheetFromFile('TQS1516.xls', sheet=1, header=T, startRow = 3)
risk <- risk[, c("Case.ID", "X..Goals", "X..Risk.Factors")]
risk <- risk[risk$Case.Status == "Active", ]

grades <-  readWorksheetFromFile('Grades1516.xls', sheet=1, header = T, startRow = 5)
# replace grade subjects
#grades$Outcome.Item[grades$Notes != ""] <- grades$Notes[grades$Notes != ""]
#gradesfull <- grades
#grades <- grades[grades$Outcome.Item %in% c("GPA", "Science", "Math", "Reading", "Writing", "Social Studies", "Lang. Arts")]
grades <- grades[, c("Case.ID", "Outcome.Item","Report.Period", "Value")]

# Flag entries that have incorrect dates

attenddates <- attend[as.Date(attend$Date, "%d-%b-%y") < as.Date("8aug2015","%d%b%Y") & as.Date(attend$Date, "%d-%b-%y") > as.Date("1aug2016", "%d%b%Y"), ]


# flag entries that have duplicated student, report period, outcome item #####
attend1 <- attend
attend1$dup <- duplicated(attend1[, c("Case.ID", "Outcome.Item","Report.Period")])
attend1 <- attend1[order(!attend1$dup), ]
attend1$dup2 <- duplicated(attend1[, c("Case.ID", "Outcome.Item","Report.Period")])
attend1 <- attend1[attend1$dup | attend1$dup2, ]
write.csv(attend1, "attendance_duplicates.csv")
############### Above csv is for you and/or Sheri to check on duplicates with the GC's

########### flag entries that have duplicated student, report period, outcome item
grades1 <- grades
grades1$dup <- duplicated(grades1[, c("Case.ID", "Outcome.Item","Report.Period")])
grades1 <- grades1[order(!grades1$dup), ]
grades1$dup2 <- duplicated(grades1[, c("Case.ID", "Outcome.Item","Report.Period")])
grades1 <- grades1[grades1$dup | grades1$dup2, ]
write.csv(grades1, "grades_duplicates.csv")
############### Above csv is for you and/or Sheri to check on duplicates with the GC's

# Replacing Baseline values with correct report period, if other entry for report period is not present.####
attend <- attend[!duplicated(attend[,c("Case.ID", "Outcome.Item","Report.Period")]), ] # This is a soft option that just deletes one of the duplicates arbitrarily
attend <- attend[order(attend$Report.Period), ] # ordering the observations based on the report period, so that all Baseline observations are last. Now when we find duplicates the Baseline observations will be the duplicates, and not the 1st grading period
attreport <- attend$Report.Period

# creating a vector "period" equal to the report period that the date of the entry corresponds to
dates <- c(as.Date("1sep2015","%d%b%Y"), as.Date("15jan2016", "%d%b%Y"), as.Date("24mar2016", "%d%b%Y"),as.Date("9jun2016", "%d%b%Y"),as.Date("1aug2016", "%d%b%Y"))
period <- rep(NA, nrow(attend))
attend$Date <- as.Date(attend$Date, "%d-%b-%y")
period[attend$Date < dates[2]] <- "1st Grading Period"
period[attend$Date > dates[2]] <- "2nd Grading Period"
period[attend$Date > dates[3]] <- "3rd Grading Period"
period[attend$Date > dates[4]] <- "4th Grading Period"

# creating a vector that is the report period with "baseline" replaced with the report period for the date entered.
for(i in 1:length(attreport)){
  if(attreport[i] == "Baseline"){attreport[i] <- period[i]}
}
dup <- duplicated(cbind(attend[,c("Case.ID", "Outcome.Item")], attreport)) # finding rows where report.period is baseline and there is already a 1st quarter entry
replace <- (!dup) & (attend$Report.Period == "Baseline") # rows to replace in data are baseline rows for which there is not another entry for the grading period corresponding to the entry date for the baseline entry

for(i in 1:nrow(attend)){
  if(replace[i]){attend$Report.Period[i] <- attreport[i] } # if the row is a baseline outcome and that student outcome combo doesn't have a value for the report period corresponding to the date of this entry, replace baseline with the report period for the date this was entered.
}


# Repeating for Grades, but only replacing Baseline with 1st Grading Period (grades don't supply the date for some reason)
grades <- grades[!duplicated(grades[,c("Case.ID", "Outcome.Item","Report.Period")]), ] # This is a soft option that just deletes one of the duplicates arbitrarily
grades <- grades[order(grades$Report.Period), ] # ordering the observations based on the report period, so that all Baseline observations are last. Now when we find duplicates the Baseline observations will be the duplicates, and not the 1st grading period
attreport <- grades$Report.Period
attreport[attreport == "Baseline"] <- "1st Grading Period"

dup <- duplicated(cbind(grades[,c("Case.ID", "Outcome.Item")], attreport)) # finding rows where report.period is baseline and there is already a 1st quarter entry
replace <- (!dup) & (grades$Report.Period == "Baseline") # rows to replace in data are baseline rows for which there is not another entry for the 1st grading period

grades$Report.Period[replace] <- "1st Grading Period"

badrp.grades <- grades[!grades$Report.Period %in% c("Baseline", "1st Grading Period", "2nd Grading Period", "3rd Grading Period", "4th Grading Period"), ]
badrp.attend <- attend[!attend$Report.Period %in% c("Baseline", "1st Grading Period", "2nd Grading Period", "3rd Grading Period", "4th Grading Period"), ]

#Spreading grades and attendance, replacing subject values  ####


#Spreading
quartersubject <- paste("Q_", substr(grades$Report.Period, 1,1), " ", grades$Outcome.Item, sep = "")
quartersubjectgpa <- paste(grades$Outcome.Item," ",  "Q_",substr(grades$Report.Period, 1,1), sep = "")
grades$quartersubject <- quartersubject
grades$quartersubject[grades$Outcome.Item == "GPA"] <- quartersubjectgpa[grades$Outcome.Item == "GPA"]

grades <- spread(grades[, ! colnames(grades) %in% c("Outcome.Item", "Report.Period")], quartersubject, Value)

attend$quartersubject <- paste("Q_", substr(attend$Report.Period, 1,1), " ", attend$Outcome.Item, sep = "")
attend <- spread(attend[, ! colnames(attend) %in% c("Outcome.Item", "Report.Period", "Date")], quartersubject, Value)

# merging grades and attendance, Aggregating Outcome info, and aggregate service info into stlist #####
attend$Case.ID <- as.numeric(attend$Case.ID) # These ID's get imported as strings for some reason, and some have leading zero's while others do not- "011" vs "11" should be the same student, but don't merge together unless we change them to numeric, where the are both 11
grades$Case.ID <- as.numeric(grades$Case.ID)
cs$Case.ID <- as.numeric(cs$Case.ID)
risk$Case.ID <- as.numeric(risk$Case.ID)
stlist <- merge(cs, attend, by = "Case.ID", all = T)
stlist <- merge(stlist, grades, by = "Case.ID", all = T)
stlist <- merge(stlist, risk, by = "Case.ID", all.x = T)

#Aggregating Outcome info
stlist$avgrade1 <- rowMeans(stlist[, c("Q_1 Science", "Q_1 Math", "Q_1 Writing", "Q_1 Reading", "Q_1 Lang. Arts")], na.rm =T)
stlist$avgrade2 <- rowMeans(stlist[, c("Q_2 Science", "Q_2 Math", "Q_2 Writing", "Q_2 Reading", "Q_2 Lang. Arts")], na.rm =T)
stlist$avgrade3 <- rowMeans(stlist[, c("Q_3 Science", "Q_3 Math", "Q_3 Writing", "Q_3 Reading", "Q_3 Lang. Arts")], na.rm =T)# will give an error before 3rd quarter outcomes are entered
stlist$avgrade4 <- rowMeans(stlist[, c("Q_4 Science", "Q_4 Math", "Q_4 Writing", "Q_4 Reading", "Q_4 Lang. Arts")], na.rm =T)# will give an error before 4th quarter outcomes are entered
stlist$avgrade <- rowMeans(stlist[, colnames(stlist) %in% c("avgrade1", "avgrade2", "avgrade3", "avgrade4")], na.rm = T)
stlist$nogrades <- is.na(stlist$avgrade)
stlist$nogrades1 <- is.na(stlist$avgrade1)
stlist$nogrades2 <- is.na(stlist$avgrade2)
stlist$nogrades3 <- is.na(stlist$avgrade3)
stlist$nogrades4 <- is.na(stlist$avgrade4)

#These are average grades in each subject. Science is made by taking the mean of every column that contains the text "Science". Google "regular expressions r" for more info on grep.
stlist$Science <- rowMeans(stlist[, grep("Science", colnames(stlist))], na.rm = T)
stlist$Math <- rowMeans(stlist[, grep("Math", colnames(stlist))], na.rm = T)
stlist$Writing <- rowMeans(stlist[, grep("Writing", colnames(stlist))], na.rm = T)
stlist$Reading <- rowMeans(stlist[, grep("Reading", colnames(stlist))], na.rm = T)
stlist$"Lang. Arts" <- rowMeans(stlist[, grep("Lang. Arts", colnames(stlist))], na.rm = T)
stlist$"Social Studies" <- rowMeans(stlist[, grep("Social Studies", colnames(stlist))], na.rm = T)

stlist$totabs1 <- rowSums(stlist[, c("Q_1 Excused Absence", "Q_1 Unexcused Absence")], na.rm = T)
stlist$totabs1[is.na(stlist$"Q_1 Excused Absence") & is.na(stlist$"Q_1 Unexcused Absence") ] <- NA
# These take the sums of any columns that contain the text "Q_2" and "Absence". 
stlist$totabs2 <- rowSums(cbind(stlist[, grep("Q_2.*Absence", colnames(stlist))], NA), na.rm = T)
#Replaces values with NA where 1) the number of NA values in the columns with "Q_2" and "Absence" is equal to the number of those columns or  2.) the number of columns with "Q_2" and "Absence" is equal to zero
stlist$totabs2[rowSums(cbind(is.na(stlist[, grep("Q_2.*Absence", colnames(stlist))]), 0)) == length(grep("Q_2.*Absence", colnames(stlist)))
               |(length(grep("Q_2.*Absence", colnames(stlist))) == 0) ] <- NA
stlist$totabs3 <- rowSums(cbind(stlist[, grep("Q_3.*Absence", colnames(stlist))], NA), na.rm = T)
stlist$totabs3[rowSums(cbind(is.na(stlist[, grep("Q_3.*Absence", colnames(stlist))]), 0)) == length(grep("Q_3.*Absence", colnames(stlist)))
               |(length(grep("Q_3.*Absence", colnames(stlist))) == 0) ] <- NA
stlist$totabs4 <- rowSums(cbind(stlist[, grep("Q_4.*Absence", colnames(stlist))], NA), na.rm = T)
stlist$totabs4[rowSums(cbind(is.na(stlist[, grep("Q_4.*Absence", colnames(stlist))]), 0)) == length(grep("Q_4.*Absence", colnames(stlist)))
               |(length(grep("Q_4.*Absence", colnames(stlist))) == 0) ] <- NA


stlist$totabs <- rowSums(cbind(stlist[, colnames(stlist) %in% c("totabs1", "totabs2", "totabs3", "totabs4")], NA), na.rm = T)

stlist$noabs <- is.na(stlist$totabs)
stlist$noabs1 <- is.na(stlist$totabs1)
stlist$noabs2 <- is.na(stlist$totabs2)
stlist$noabs3 <- is.na(stlist$totabs3)
stlist$noabs4 <- is.na(stlist$totabs4)

stlist[,grep("(ISS)|(OSS)", colnames(stlist))][is.na(stlist[, grep("(ISS)|(OSS)", colnames(stlist))])] <- 0
stlist$suspended <- rowSums(stlist[, grep("(ISS)|(OSS)", colnames(stlist))]) > 0
stlist$suspended[is.na(stlist$suspended)] <- FALSE
stlist$suspended <- as.numeric(stlist$suspended)

# Adding service aggregates to student list
stserv <- data %>% group_by(Student.ID) %>% summarize(Hours = sum(Hours), HoursSpent = sum(hoursspent), individual = sum(individual), group = sum(group), tier1 = sum(tier1), 
                                                      checkin = sum(checkin), parent1on1 = sum(parent1on1), anyfamily = sum(anyfamily), num_serv = length(Student.ID), service_date = tail(End.Date, n =1) )
checkcounts <- data[data$checkin != 0 , ] %>% group_by(Student.ID) %>% summarize(num_check = n())
parentcounts <- data[data$parent1on1 != 0, ] %>% group_by(Student.ID) %>% summarize(num_parent1on1 = n())
anyfamcounts <- data[data$anyfamily != 0, ] %>% group_by(Student.ID) %>% summarize(num_anyfamily = n())
stserv <- merge(stserv, checkcounts, by = "Student.ID", all = T)
stserv <- merge(stserv, parentcounts, by = "Student.ID", all = T)
stserv <- merge(stserv, anyfamcounts, by = "Student.ID", all = T)
colnames(stserv)[1] <- "Case.ID"

stlist <- merge(stlist, stserv, by = "Case.ID", all = T)

#Students with NA for Hours are students present on caseload and not present in our service data 
stlist[is.na(stlist$Hours), ]$Hours <- 0
stlist[is.na(stlist$HoursSpent), ]$HoursSpent <- 0
stlist[is.na(stlist$individual), ]$individual <- 0
stlist[is.na(stlist$group), ]$group <- 0
stlist[, c("tier1", "checkin", "parent1on1", "anyfamily", "num_serv", "num_check", "num_parent1on1", "num_anyfamily")][is.na(stlist[, c("tier1", "checkin", "parent1on1", "anyfamily", "num_serv", "num_check", "num_parent1on1", "num_anyfamily")])] <- 0


stlist <- subset(stlist, (Hours > 0 & Case.Status == "InActive") |Case.Status == "Active")

stlist <- stlist[!is.na(stlist$Name), ]

#This section creates a new variable, criteria, which calculates the number of eligibility criteria a student meets. #####
elem <- c("Glenn Elementary School", "Eno Valley Elementary", "EK Powe Elementary School", "YE Smith Elementary")
high <- c("Neal Middle School", "Durham Performance Learning Center", "Hillside High School", "Southern High School", "Northern")




stlist$`Q_1 criteria` <- 0
stlist$`Q_2 criteria` <- 0
stlist$`Q_3 criteria` <- 0
stlist$`Q_4 criteria` <- 0
stlist$criteria <- 0



criteria_cats <- c(117:120)

abs_cats <- 92:95

susp_cats <- c(15,16,19,20,23,24,27,28)

q1_subjects <- c(34,35,38)
q2_subjects <- c(41,42,45)
q3_subjects <- c(48,49,52)
q4_subjects <- c(55,56,59)




for(i in q1_subjects){
  
  stlist$`Q_1 criteria` <- ifelse(is.element(stlist$Site, elem) & stlist$`Q_1 criteria` != 1 & (stlist[,i] <= 2 & !is.na(stlist[,i])), stlist$`Q_1 criteria`+ 1, stlist$`Q_1 criteria`)
  stlist$`Q_1 criteria` <- ifelse(is.element(stlist$Site, high) & stlist$`Q_1 criteria` != 1 & (stlist[,i] < 70 & !is.na(stlist[,i])), stlist$`Q_1 criteria` + 1, stlist$`Q_1 criteria`)
  
}

for(i in q2_subjects){
  
  stlist$`Q_2 criteria` <- ifelse(is.element(stlist$Site, elem) & stlist$`Q_2 criteria` != 1 & (stlist[,i] <= 2 & !is.na(stlist[,i])), stlist$`Q_2 criteria`+ 1, stlist$`Q_2 criteria`)
  stlist$`Q_2 criteria` <- ifelse(is.element(stlist$Site, high) & stlist$`Q_2 criteria` != 1 & (stlist[,i] < 70 & !is.na(stlist[,i])), stlist$`Q_2 criteria` + 1, stlist$`Q_2 criteria`)
  
}

for(i in q3_subjects){
  
  stlist$`Q_3 criteria` <- ifelse(is.element(stlist$Site, elem) & stlist$`Q_3 criteria` != 1 & (stlist[,i] <= 2 & !is.na(stlist[,i])), stlist$`Q_3 criteria`+ 1, stlist$`Q_3 criteria`)
  stlist$`Q_3 criteria` <- ifelse(is.element(stlist$Site, high) & stlist$`Q_3 criteria` != 1 & (stlist[,i] < 70 & !is.na(stlist[,i])), stlist$`Q_3 criteria` + 1, stlist$`Q_3 criteria`)
  
}

for(i in q4_subjects){
  
  stlist$`Q_4 criteria` <- ifelse(is.element(stlist$Site, elem) & stlist$`Q_4 criteria` != 1 & (stlist[,i] <= 2 & !is.na(stlist[,i])), stlist$`Q_4 criteria`+ 1, stlist$`Q_4 criteria`)
  stlist$`Q_4 criteria` <- ifelse(is.element(stlist$Site, high) & stlist$`Q_4 criteria` != 1 & (stlist[,i] < 70 & !is.na(stlist[,i])), stlist$`Q_4 criteria` + 1, stlist$`Q_4 criteria`)
  
}


stlist$`Q_1 criteria` <- ifelse(stlist$`Q_1 ISS` == 0 | is.na(stlist$suspended) | stlist$`Q_1 OSS` == 0, stlist$`Q_1 criteria`, stlist$`Q_1 criteria` + 1)
stlist$`Q_2 criteria` <- ifelse(stlist$`Q_2 ISS` == 0 | is.na(stlist$suspended) | stlist$`Q_2 OSS` == 0, stlist$`Q_2 criteria`, stlist$`Q_2 criteria` + 1)
stlist$`Q_3 criteria` <- ifelse(stlist$`Q_3 ISS` == 0 | is.na(stlist$suspended) | stlist$`Q_3 OSS` == 0, stlist$`Q_3 criteria`, stlist$`Q_3 criteria` + 1)
stlist$`Q_4 criteria` <- ifelse(stlist$`Q_4 ISS` == 0 | is.na(stlist$suspended) | stlist$`Q_4 OSS` == 0, stlist$`Q_4 criteria`, stlist$`Q_4 criteria` + 1)

stlist$`Q_1 criteria` <- ifelse(stlist$totabs1 < 3 | is.na(stlist$totabs1), stlist$`Q_1 criteria`, stlist$`Q_1 criteria` + 1)
stlist$`Q_2 criteria` <- ifelse(stlist$totabs2 < 3 | is.na(stlist$totabs2), stlist$`Q_2 criteria`, stlist$`Q_2 criteria` + 1)
stlist$`Q_3 criteria` <- ifelse(stlist$totabs3 < 3 | is.na(stlist$totabs3), stlist$`Q_3 criteria`, stlist$`Q_3 criteria` + 1)
stlist$`Q_4 criteria` <- ifelse(stlist$totabs4 < 3 | is.na(stlist$totabs4), stlist$`Q_4 criteria`, stlist$`Q_4 criteria` + 1)



stlist$criteria <-  ifelse(is.element(stlist$Site, elem) & (stlist$`Lang. Arts` <= 2 & !is.na(stlist$`Lang. Arts`)), stlist$criteria + 1, stlist$criteria)
stlist$criteria <-  ifelse(is.element(stlist$Site, elem) & stlist$criteria != 1 & (stlist$Math <= 2 & !is.na(stlist$Math)), stlist$criteria + 1, stlist$criteria)
stlist$criteria <-  ifelse(is.element(stlist$Site, elem) & stlist$criteria != 1 & (stlist$Science <= 2 & !is.na(stlist$Science)), stlist$criteria + 1, stlist$criteria)
stlist$criteria <- ifelse(stlist$Site == "YE Smith Elementary" & (stlist$criteria != 1 & (stlist$Writing <=2 & !is.na(stlist$Math))), stlist$criteria + 1, stlist$criteria)

stlist$criteria <-  ifelse(is.element(stlist$Site, high) & (stlist$`Lang. Arts` < 70 & !is.na(stlist$`Lang. Arts`)), stlist$criteria + 1, stlist$criteria)
stlist$criteria <-  ifelse(is.element(stlist$Site, high) & stlist$criteria != 1 & (stlist$Math < 70 & !is.na(stlist$Math)), stlist$criteria + 1, stlist$criteria)
stlist$criteria <-  ifelse(is.element(stlist$Site, high) & stlist$criteria != 1 & (stlist$Science < 70  & !is.na(stlist$Science)), stlist$criteria + 1, stlist$criteria)


stlist$criteria <- ifelse(stlist$suspended == FALSE | is.na(stlist$suspended), stlist$criteria, stlist$criteria + 1)
stlist$criteria <- ifelse(stlist$totabs < 10 | is.na(stlist$totabs), stlist$criteria, stlist$criteria + 1)


eligible <- subset(stlist, stlist$`Q_1 criteria` > 0 |  stlist$`Q_2 criteria` > 0 | stlist$`Q_3 criteria` > 0 |  stlist$`Q_4 criteria` > 0)
eligible <- subset(eligible, (as.Date(eligible$X...Date.Inactivated) > as.Date("2016-05-20") & as.Date(eligible$CIS.Enroll.Date) < as.Date("2016-04-30")) | eligible$Case.Status == "Active")

improve_criteria <- subset(eligible, (eligible$`Q_3 criteria` == 0 & eligible$`Q_4 criteria` == 0) | eligible$`Q_4 criteria` == 0)

stlist$max_criteria <- pmax(stlist$`Q_1 criteria`, stlist$`Q_2 criteria`, stlist$`Q_3 criteria`, stlist$`Q_4 criteria`)



improve.math <- subset(stlist, stlist$Site %in% high & (stlist$`Q_2 Math` - stlist$`Q_1 Math`) >= 10 | (stlist$`Q_4 Math` - stlist$`Q_3 Math`) >=10)
improve.math <- subset(improve.math, !is.na(improve.math$Name))

improve.elm.math <- subset(stlist, stlist$Site %in% elem & ((stlist$`Q_2 Math` - stlist$`Q_1 Math`) + (stlist$`Q_3 Math` - stlist$`Q_2 Math`) + (stlist$`Q_4 Math` - stlist$`Q_3 Math`) >= 1.0 ))
improve.elm.math <- subset(improve.elm.math, !is.na(improve.elm.math$Name))

improve.la <- subset(stlist, stlist$Site %in% high & (stlist$`Q_2 Lang. Arts` - stlist$`Q_1 Lang. Arts`) >= 10 | (stlist$`Q_4 Lang. Arts` - stlist$`Q_3 Lang. Arts`) >= 10)
improve.la <- subset(improve.la, !is.na(improve.la$Name))

improve.elm.la <- subset(stlist, stlist$Site %in% elem & (stlist$`Q_2 Lang. Arts` - stlist$`Q_1 Lang. Arts`) + (stlist$`Q_3 Lang. Arts` - stlist$`Q_2 Lang. Arts`) + (stlist$`Q_4 Lang. Arts` - stlist$`Q_3 Lang. Arts`) >= 1.0 )
improve.elm.la <- subset(improve.elm.la, !is.na(improve.elm.la$Name))

improve.science <- subset(stlist, stlist$Site %in% high & (stlist$`Q_2 Science` - stlist$`Q_1 Science`) >= 10 | (stlist$`Q_4 Science` - stlist$`Q_3 Science`) >= 10)
improve.science <- subset(improve.science, !is.na(improve.science$Name))

improve.elm.science <- subset(stlist, stlist$Site %in% elem & (stlist$`Q_2 Science` - stlist$`Q_1 Science`) + (stlist$`Q_3 Science` - stlist$`Q_2 Science`) +  (stlist$`Q_4 Science` - stlist$`Q_3 Science`) >= 1.0 ) 
improve.elm.science <- subset(improve.elm.science, !is.na(improve.elm.science$Name))


improve.elem.attend <- subset(stlist, stlist$Site %in% elem  & ((stlist$totabs1 - stlist$totabs2) + (stlist$totabs2 - stlist$totabs3) + (stlist$totabs3 - stlist$totabs4) >= 3))
improve.elem.attend <- subset(improve.elem.attend, !is.na(improve.elem.attend$Name))
elem.attend.eligible <- subset(stlist, stlist$Site %in% elem  &  (stlist$totabs1 > 3 | stlist$totabs2 > 3 | stlist$totabs3 > 3 | stlist$totabs4 > 3))

improve.high.attend <- subset(stlist, stlist$Site %in% high & ((stlist$totabs1 - stlist$totabs2) + (stlist$totabs2 - stlist$totabs3) + (stlist$totabs3 - stlist$totabs4) >= 8))
improve.high.attend <- subset(improve.high.attend, !is.na(improve.high.attend$Name))
high.attend.eligible <- subset(stlist, stlist$Site %in% high  &  (stlist$totabs1 > 5 | stlist$totabs2 > 5 | stlist$totabs3 > 5 | stlist$totabs4 > 5))


improve.grades <- merge(improve.la, improve.science, all = TRUE)
improve.grades <- merge(improve.grades, improve.math, all= T)
improve.grades <- merge(improve.grades, improve.elm.science, all = T)
improve.grades <- merge(improve.grades, improve.elm.la, all = T)
improve.grades <- merge(improve.grades, improve.elm.math, all = T)
improve.grades$improve.grades <- TRUE

improve.attend <- merge(improve.high.attend, improve.elem.attend, all = T)
improve.attend$attned <- TRUE

improve <- merge(improve.grades,improve.attend, all = T)

write.csv(improve, "improve.csv")






stlist$improve_criteria <- ifelse(stlist$Case.ID %in% improve_criteria$Case.ID, stlist$improve_criteria <- TRUE, FALSE)
stlist$improve_grades <- ifelse(stlist$Case.ID %in% improve.grades$Case.ID, stlist$improve_grades <- TRUE, FALSE)
stlist$improve_attend <- ifelse(stlist$Case.ID %in% improve.attend$Case.ID, stlist$improve_attend <- TRUE, FALSE)

stlist$avg.duration <- stlist$Hours / stlist$num_serv

stlist.save <- stlist
stlist <- stlist.save

write.csv(improve_criteria, "improve criteria.csv")

#Write studentlist to the working directory ####
unlink("studentlist.csv", recursive = FALSE, force = FALSE)

write.csv(stlist, "studentlist.csv")

stlist$totabs4[stlist$Site == "Hillside High School"] <- stlist$totabs4[stlist$Site == "Hillside High School"] + 5
mean(ifelse(subset(stlist$totabs4, stlist$Site == "Hillside High School"), subset(stlist$totabs4, stlist$Site == "Hillside High School" ) + 0, ), na.rm = T)

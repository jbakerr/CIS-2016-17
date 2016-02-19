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
macdatawd <- "~/Dropbox/CIS Data"
windowsdatawd <- "C:/Users/USER/Dropbox/CIS Data"
if(file.exists(macdatawd)){
  setwd(file.path(macdatawd))
} else { 
  if(file.exists(windowsdatawd)){
    setwd(file.path(windowsdatawd))
  }
}


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

#stlist$noabs <- is.na(stlist$totabs)

stlist[,grep("(ISS)|(OSS)", colnames(stlist))][is.na(stlist[, grep("(ISS)|(OSS)", colnames(stlist))])] <- 0
stlist$suspended <- rowSums(stlist[, grep("(ISS)|(OSS)", colnames(stlist))]) > 0
stlist$suspended[is.na(stlist$suspended)] <- FALSE
stlist$suspended <- as.numeric(stlist$suspended)

# Adding service aggregates to student list
stserv <- data %>% group_by(Student.ID) %>% summarize(Hours = sum(Hours), HoursSpent = sum(hoursspent), individual = sum(individual), group = sum(group), tier1 = sum(tier1), 
                                                      checkin = sum(checkin), parent1on1 = sum(parent1on1), anyfamily = sum(anyfamily), num_serv = length(Student.ID) )
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

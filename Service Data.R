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


# This is the name I have been using for the detailed service dataset, however we may want to start using a different naming convention
# This is the "Tier II and III Support Detail" report. 
# **** IMPORTANT ***** delete the last row that sums all hours before saving the excel file. That row will cause problems
data<-readWorksheetFromFile('ServiceD1516.xls', sheet=1, header = T, startRow = 5)
colnames(data)[1] <- "Student.ID"
data <- data[!is.na(data$Student.ID), ] # get rid of accidental blank rows
data <- data[as.Date(data$Begin.Date) > as.Date("8aug2015","%d%b%Y"), ] #get rid of services before school year
data[data$Tier == "Tier I", ]$Recorded.As <- "Group Setting"

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

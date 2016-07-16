afterschool <- data[data$Student.Support.Name == "Afterschool (21st Century)",]

afterschool$day <- weekdays(as.Date(afterschool$Begin.Date))

afterschool <- afterschool[!is.na(afterschool$Home.School),]

eno.afterschool <- afterschool[afterschool$Home.School == "Eno Valley Elementary",]

ek.afterschool <- afterschool[afterschool$Home.School == "EK Powe Elementary School",]

aggregate(Freq, by = as.list(day), mean)


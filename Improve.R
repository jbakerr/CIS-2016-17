
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

improve.high.attend <- subset(stlist, stlist$Site %in% high & ((stlist$totabs1 - stlist$totabs2) + (stlist$totabs2 - stlist$totabs3) + (stlist$totabs3 - stlist$totabs4) >= 8))
improve.high.attend <- subset(improve.high.attend, !is.na(improve.high.attend$Name))

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




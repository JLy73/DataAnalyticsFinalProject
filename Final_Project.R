# DATA ANALYTICS FINAL PROJECT
library(randomForest)
library(e1071)
library(splitstackshape)

# read the file
INFO <- read.csv(file='/home/josephine/Dropbox/Semester\ 7/Data\ Analytics/EDU_ENRL_FIELD_26092019174437802.csv')

# CLEAN THE DATA
summary(INFO)
# Remove K, L, O, P, Q, R, S, T for they are all the same values in the data
INFO_clean1 <- INFO[-c(11, 12, 15, 16, 17, 18, 19, 20, 23)]
summary(INFO_clean1)
# Remove A, C, E, G, I, and M as they all equal the last column (will use later for labels)
INFO_clean2 <- INFO_clean1[c(1, 3, 5, 8, 9, 11, 13, 14)]
summary(INFO_clean2)
# Deal with Flags: Removing, x and/or M flags, or z
clean0 <- subset(INFO_clean2, !(INFO_clean2$Flag.Codes == "M" | INFO_clean2$Flag.Codes == "x" | INFO_clean2$Flag.Codes == "M; x" | INFO_clean2$Flag.Codes == "z"))
summary(clean0)
# Remove Rest of World -> redudant data
clean <- subset(clean0, (clean0$Country.of.origin == "World (all entities, including reference area, including IO)"))
summary(clean)
# Remove now unneeded country of origin and flage codes
clean <- clean[-c(4,8)]
summary(clean)

# SUBSETS
# level of education
clean5 <- subset(clean, clean$ISC11_LEVEL == "L5")
clean6 <- subset(clean, clean$ISC11_LEVEL  == "L6")
clean7 <- subset(clean, clean$ISC11_LEVEL  == "L7")
clean8 <- subset(clean, clean$ISC11_LEVEL  == "L8")
cleanTL <- subset(clean, clean$ISC11_LEVEL  == "L5T8")
# gender
cleanFF <- subset(clean, clean$SEX == "F")
cleanMM <- subset(clean, clean$SEX == "M")
cleanTT <- subset(clean, clean$SEX == "T")
# fields
cleanFT <- subset(clean, clean$FIELD == "T")
# field-level
cleanFL <- subset(clean, clean$FIELD == "T" & clean$ISC11_LEVEL  == "L5T8")
# field-country
cleanFC <- subset(clean, clean$FIELD == "T" & clean$COUNTRY == "OTO")
# level-country
cleanLC <- subset(clean, clean$ISC11_LEVEL  == "L5T8" & clean$COUNTRY == "OTO")
# broad fields of education
cleanBB <- subset(clean, clean$FIELD == "F00" | clean$FIELD == "F01" | clean$FIELD == "F02" | clean$FIELD == "F03" | clean$FIELD == "F04" | clean$FIELD == "F05" | clean$FIELD == "F06" | clean$FIELD == "F07" | clean$FIELD == "F08" | clean$FIELD == "F09" | clean$FIELD == "F10" | clean$FIELD == "F99")
# no totals (with broad)
cleanNT <- subset(cleanBB, !(cleanBB$COUNTRY == "OEU" | cleanBB$COUNTRY == "OTO") & !(cleanBB$SEX == "T") & !(cleanBB$ISC11_LEVEL == "L5T8"))
cleanNT <- subset(cleanNT, cleanNT$Value > 0)

# DISTRIBUTION ANALYSIS -> Original
# Value
boxplot(clean$Value)
hist(clean$Value, breaks = 50)
  # Country
AggC <- aggregate(clean$Value, list(clean$COUNTRY), sum)
head(AggC)
print(AggC)
barplot(AggC$x,names=AggC$Group.1, xlab = "Country", ylab = "Persons", main = "Country Distribution")
# Gender
AggS <- aggregate(clean$Value, list(clean$SEX), sum)
head(AggS)
print(AggS)
barplot(AggS$x,names=AggS$Group.1, xlab = "Sex", ylab = "Persons", main = "Gender Distribution")
# Field of Education
AggF <- aggregate(clean$Value, list(clean$FIELD), sum)
head(AggF)
print(AggF)
barplot(AggF$x,names=AggF$Group.1, xlab = "Field of Education", ylab = "Persons", main = "Field of Education Distribution")
# Level of Education
AggL <- aggregate(clean$Value, list(clean$ISC11_LEVEL), sum)
head(AggL)
print(AggL)
barplot(AggL$x,names=AggL$Group.1, xlab = "ISCED Level of Education", ylab = "Persons", main = "Level of Education Distribution")
# Year
AggY <- aggregate(clean$Value, list(clean$YEAR), sum)
head(AggY)
print(AggY)
barplot(AggY$x,names=AggY$Group.1, xlab = "Year", ylab = "Persons", main = "Year Distribution")

# TRANSFORM THE DATA
# Split the non-totaled dataset so each set is a pairing
# get 2017
cleanNT2 <- subset(cleanNT, cleanNT$YEAR == 2017)
# remove year as unneeded now
cleanNT2 <- cleanNT2[-c(5)]
# reset factors
cleanNT2$COUNTRY <- factor(cleanNT2$COUNTRY)
cleanNT2$SEX <- factor(cleanNT2$SEX)
cleanNT2$ISC11_LEVEL <- factor(cleanNT2$ISC11_LEVEL)
cleanNT2$FIELD <- factor(cleanNT2$FIELD)
# split the set (by each paring)
clean_split <- split(cleanNT2, list(cleanNT2$FIELD, cleanNT2$ISC11_LEVEL))
summary(clean_split)

# split by variable
F00L5 <- clean_split$F00.L5
F01L5 <- clean_split$F01.L5
F02L5 <- clean_split$F02.L5
F03L5 <- clean_split$F03.L5
F04L5 <- clean_split$F04.L5
F05L5 <- clean_split$F05.L5
F06L5 <- clean_split$F06.L5
F07L5 <- clean_split$F07.L5
F08L5 <- clean_split$F08.L5
F09L5 <- clean_split$F09.L5
F10L5 <- clean_split$F10.L5
F99L5 <- clean_split$F99.L5

F00L6 <- clean_split$F00.L6
F01L6 <- clean_split$F01.L6
F02L6 <- clean_split$F02.L6
F03L6 <- clean_split$F03.L6
F04L6 <- clean_split$F04.L6
F05L6 <- clean_split$F05.L6
F06L6 <- clean_split$F06.L6
F07L6 <- clean_split$F07.L6
F08L6 <- clean_split$F08.L6
F09L6 <- clean_split$F09.L6
F10L6 <- clean_split$F10.L6
F99L6 <- clean_split$F99.L6

F00L7 <- clean_split$F00.L7
F01L7 <- clean_split$F01.L7
F02L7 <- clean_split$F02.L7
F03L7 <- clean_split$F03.L7
F04L7 <- clean_split$F04.L7
F05L7 <- clean_split$F05.L7
F06L7 <- clean_split$F06.L7
F07L7 <- clean_split$F07.L7
F08L7 <- clean_split$F08.L7
F09L7 <- clean_split$F09.L7
F10L7 <- clean_split$F10.L7
F99L7 <- clean_split$F99.L7

F00L8 <- clean_split$F00.L8
F01L8 <- clean_split$F01.L8
F02L8 <- clean_split$F02.L8
F03L8 <- clean_split$F03.L8
F04L8 <- clean_split$F04.L8
F05L8 <- clean_split$F05.L8
F06L8 <- clean_split$F06.L8
F07L8 <- clean_split$F07.L8
F08L8 <- clean_split$F08.L8
F09L8 <- clean_split$F09.L8
F10L8 <- clean_split$F10.L8
F99L8 <- clean_split$F99.L8

# expand rows and add to total
EXP_F00L5 <- expandRows(F00L5, "Value") 
IDX_ <- sample(1:nrow(EXP_F00L5), min(nrow(EXP_F00L5), 50000))
EXP_F00L5 <- EXP_F00L5[IDX_, ]

EXP_F01L5 <- expandRows(F01L5, "Value") 
IDX_ <- sample(1:nrow(EXP_F01L5), min(nrow(EXP_F01L5), 50000))
EXP_F01L5 <- EXP_F01L5[IDX_, ]
TOTAL <- rbind(EXP_F00L5, EXP_F01L5)
rm(EXP_F00L5, EXP_F01L5)

EXP_F02L5 <- expandRows(F02L5, "Value") 
IDX_ <- sample(1:nrow(EXP_F02L5), min(nrow(EXP_F02L5), 50000))
EXP_F02L5 <- EXP_F02L5[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F02L5)
rm(EXP_F02L5)

EXP_F03L5 <- expandRows(F03L5, "Value") 
IDX_ <- sample(1:nrow(EXP_F03L5), min(nrow(EXP_F03L5), 50000))
EXP_F03L5 <- EXP_F03L5[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F03L5)
rm(EXP_F03L5)

EXP_F04L5 <- expandRows(F04L5, "Value") 
IDX_ <- sample(1:nrow(EXP_F04L5), min(nrow(EXP_F04L5), 50000))
EXP_F04L5 <- EXP_F04L5[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F04L5)
rm(EXP_F04L5)

EXP_F05L5 <- expandRows(F05L5, "Value") 
IDX_ <- sample(1:nrow(EXP_F05L5), min(nrow(EXP_F05L5), 50000))
EXP_F05L5 <- EXP_F05L5[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F05L5)
rm(EXP_F05L5)

EXP_F06L5 <- expandRows(F06L5, "Value") 
IDX_ <- sample(1:nrow(EXP_F06L5), min(nrow(EXP_F06L5), 50000))
EXP_F06L5 <- EXP_F06L5[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F06L5)
rm(EXP_F06L5)

EXP_F07L5 <- expandRows(F07L5, "Value") 
IDX_ <- sample(1:nrow(EXP_F07L5), min(nrow(EXP_F07L5), 50000))
EXP_F07L5 <- EXP_F07L5[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F07L5)
rm(EXP_F07L5)

EXP_F08L5 <- expandRows(F08L5, "Value") 
IDX_ <- sample(1:nrow(EXP_F08L5), min(nrow(EXP_F08L5), 50000))
EXP_F08L5 <- EXP_F08L5[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F08L5)
rm(EXP_F08L5)

EXP_F09L5 <- expandRows(F09L5, "Value") 
IDX_ <- sample(1:nrow(EXP_F09L5), min(nrow(EXP_F09L5), 50000))
EXP_F09L5 <- EXP_F09L5[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F09L5)
rm(EXP_F09L5)

EXP_F99L5 <- expandRows(F99L5, "Value") 
IDX_ <- sample(1:nrow(EXP_F99L5), min(nrow(EXP_F99L5), 50000))
EXP_F99L5 <- EXP_F99L5[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F99L5)
rm(EXP_F99L5)

EXP_F10L5 <- expandRows(F10L5, "Value") 
IDX_ <- sample(1:nrow(EXP_F10L5), min(nrow(EXP_F10L5), 50000))
EXP_F10L5 <- EXP_F10L5[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F10L5)
rm(EXP_F10L5)

EXP_F00L6 <- expandRows(F00L6, "Value") 
IDX_ <- sample(1:nrow(EXP_F00L6), min(nrow(EXP_F00L6), 50000))
EXP_F00L6 <- EXP_F00L6[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F00L6)
rm(EXP_F00L6)

EXP_F01L6 <- expandRows(F01L6, "Value") 
IDX_ <- sample(1:nrow(EXP_F01L6), min(nrow(EXP_F01L6), 50000))
EXP_F01L6 <- EXP_F01L6[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F01L6)
rm(EXP_F01L6)

EXP_F02L6 <- expandRows(F02L6, "Value") 
IDX_ <- sample(1:nrow(EXP_F02L6), min(nrow(EXP_F02L6), 50000))
EXP_F02L6 <- EXP_F02L6[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F02L6)
rm(EXP_F02L6)

EXP_F03L6 <- expandRows(F03L6, "Value") 
IDX_ <- sample(1:nrow(EXP_F03L6), min(nrow(EXP_F03L6), 50000))
EXP_F03L6 <- EXP_F03L6[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F03L6)
rm(EXP_F03L6)

EXP_F04L6 <- expandRows(F04L6, "Value") 
IDX_ <- sample(1:nrow(EXP_F04L6), min(nrow(EXP_F04L6), 50000))
EXP_F04L6 <- EXP_F04L6[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F04L6)
rm(EXP_F04L6)

EXP_F05L6 <- expandRows(F05L6, "Value") 
IDX_ <- sample(1:nrow(EXP_F05L6), min(nrow(EXP_F05L6), 50000))
EXP_F05L6 <- EXP_F05L6[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F05L6)
rm(EXP_F05L6)

EXP_F06L6 <- expandRows(F06L6, "Value") 
IDX_ <- sample(1:nrow(EXP_F06L6), min(nrow(EXP_F06L6), 50000))
EXP_F06L6 <- EXP_F06L6[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F06L6)
rm(EXP_F06L6)

EXP_F07L6 <- expandRows(F07L6, "Value") 
IDX_ <- sample(1:nrow(EXP_F07L6), min(nrow(EXP_F07L6), 50000))
EXP_F07L6 <- EXP_F07L6[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F07L6)
rm(EXP_F07L6)

EXP_F08L6 <- expandRows(F08L6, "Value") 
IDX_ <- sample(1:nrow(EXP_F08L6), min(nrow(EXP_F08L6), 50000))
EXP_F08L6 <- EXP_F08L6[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F08L6)
rm(EXP_F08L6)

EXP_F09L6 <- expandRows(F09L6, "Value") 
IDX_ <- sample(1:nrow(EXP_F09L6), min(nrow(EXP_F09L6), 50000))
EXP_F09L6 <- EXP_F09L6[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F09L6)
rm(EXP_F09L6)

EXP_F10L6 <- expandRows(F10L6, "Value") 
IDX_ <- sample(1:nrow(EXP_F10L6), min(nrow(EXP_F10L6), 50000))
EXP_F10L6 <- EXP_F10L6[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F10L6)
rm(EXP_F10L6)

EXP_F99L6 <- expandRows(F99L6, "Value") 
IDX_ <- sample(1:nrow(EXP_F99L6), min(nrow(EXP_F99L6), 50000))
EXP_F99L6 <- EXP_F99L6[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F99L6)
rm(EXP_F99L6)

EXP_F00L7 <- expandRows(F00L7, "Value") 
IDX_ <- sample(1:nrow(EXP_F00L7), min(nrow(EXP_F00L7), 50000))
EXP_F00L7 <- EXP_F00L7[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F00L7)
rm(EXP_F00L7)

EXP_F01L7 <- expandRows(F01L7, "Value") 
IDX_ <- sample(1:nrow(EXP_F01L7), min(nrow(EXP_F01L7), 50000))
EXP_F01L7 <- EXP_F01L7[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F01L7)
rm(EXP_F01L7)

EXP_F02L7 <- expandRows(F02L7, "Value") 
IDX_ <- sample(1:nrow(EXP_F02L7), min(nrow(EXP_F02L7), 50000))
EXP_F02L7 <- EXP_F02L7[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F02L7)
rm(EXP_F02L7)

EXP_F03L7 <- expandRows(F03L7, "Value") 
IDX_ <- sample(1:nrow(EXP_F03L7), min(nrow(EXP_F03L7), 50000))
EXP_F03L7 <- EXP_F03L7[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F03L7)
rm(EXP_F03L7)

EXP_F04L7 <- expandRows(F04L7, "Value") 
IDX_ <- sample(1:nrow(EXP_F04L7), min(nrow(EXP_F04L7), 50000))
EXP_F04L7 <- EXP_F04L7[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F04L7)
rm(EXP_F04L7)

EXP_F05L7 <- expandRows(F05L7, "Value") 
IDX_ <- sample(1:nrow(EXP_F05L7), min(nrow(EXP_F05L7), 50000))
EXP_F05L7 <- EXP_F05L7[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F05L7)
rm(EXP_F05L7)

EXP_F06L7 <- expandRows(F06L7, "Value") 
IDX_ <- sample(1:nrow(EXP_F06L7), min(nrow(EXP_F06L7), 50000))
EXP_F06L7 <- EXP_F06L7[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F06L7)
rm(EXP_F06L7)

EXP_F07L7 <- expandRows(F07L7, "Value") 
IDX_ <- sample(1:nrow(EXP_F07L7), min(nrow(EXP_F07L7), 50000))
EXP_F07L7 <- EXP_F07L7[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F07L7)
rm(EXP_F07L7)

EXP_F08L7 <- expandRows(F08L7, "Value") 
IDX_ <- sample(1:nrow(EXP_F08L7), min(nrow(EXP_F08L7), 50000))
EXP_F08L7 <- EXP_F08L7[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F08L7)
rm(EXP_F08L7)

EXP_F09L7 <- expandRows(F09L7, "Value") 
IDX_ <- sample(1:nrow(EXP_F09L7), min(nrow(EXP_F09L7), 50000))
EXP_F09L7 <- EXP_F09L7[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F09L7)
rm(EXP_F09L7)

EXP_F10L7 <- expandRows(F10L7, "Value") 
IDX_ <- sample(1:nrow(EXP_F10L7), min(nrow(EXP_F10L7), 50000))
EXP_F10L7 <- EXP_F10L7[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F10L7)
rm(EXP_F10L7)

EXP_F99L7 <- expandRows(F99L7, "Value") 
IDX_ <- sample(1:nrow(EXP_F99L7), min(nrow(EXP_F99L7), 50000))
EXP_F99L7 <- EXP_F99L7[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F99L7)
rm(EXP_F99L7)

EXP_F00L8 <- expandRows(F00L8, "Value") 
IDX_ <- sample(1:nrow(EXP_F00L8), min(nrow(EXP_F00L8), 50000))
EXP_F00L8 <- EXP_F00L8[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F00L8)
rm(EXP_F00L8)

EXP_F01L8 <- expandRows(F01L8, "Value") 
IDX_ <- sample(1:nrow(EXP_F01L8), min(nrow(EXP_F01L8), 50000))
EXP_F01L8 <- EXP_F01L8[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F01L8)
rm(EXP_F01L8)

EXP_F02L8 <- expandRows(F02L8, "Value") 
IDX_ <- sample(1:nrow(EXP_F02L8), min(nrow(EXP_F02L8), 50000))
EXP_F02L8 <- EXP_F02L8[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F02L8)
rm(EXP_F02L8)

EXP_F03L8 <- expandRows(F03L8, "Value") 
IDX_ <- sample(1:nrow(EXP_F03L8), min(nrow(EXP_F03L8), 50000))
EXP_F03L8 <- EXP_F03L8[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F03L8)
rm(EXP_F03L8)

EXP_F04L8 <- expandRows(F04L8, "Value") 
IDX_ <- sample(1:nrow(EXP_F04L8), min(nrow(EXP_F04L8), 50000))
EXP_F04L8 <- EXP_F04L8[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F04L8)
rm(EXP_F04L8)

EXP_F05L8 <- expandRows(F05L8, "Value") 
IDX_ <- sample(1:nrow(EXP_F05L8), min(nrow(EXP_F05L8), 50000))
EXP_F05L8 <- EXP_F05L8[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F05L8)
rm(EXP_F05L8)

EXP_F06L8 <- expandRows(F06L8, "Value") 
IDX_ <- sample(1:nrow(EXP_F06L8), min(nrow(EXP_F06L8), 50000))
EXP_F06L8 <- EXP_F06L8[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F06L8)
rm(EXP_F06L8)

EXP_F07L8 <- expandRows(F07L8, "Value") 
IDX_ <- sample(1:nrow(EXP_F07L8), min(nrow(EXP_F07L8), 50000))
EXP_F07L8 <- EXP_F07L8[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F07L8)
rm(EXP_F07L8)

EXP_F08L8 <- expandRows(F08L8, "Value") 
IDX_ <- sample(1:nrow(EXP_F08L8), min(nrow(EXP_F08L8), 50000))
EXP_F08L8 <- EXP_F08L8[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F08L8)
rm(EXP_F08L8)

EXP_F09L8 <- expandRows(F09L8, "Value") 
IDX_ <- sample(1:nrow(EXP_F09L8), min(nrow(EXP_F09L8), 50000))
EXP_F09L8 <- EXP_F09L8[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F09L8)
rm(EXP_F09L8)

EXP_F10L8 <- expandRows(F10L8, "Value") 
IDX_ <- sample(1:nrow(EXP_F10L8), min(nrow(EXP_F10L8), 50000))
EXP_F10L8 <- EXP_F10L8[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F10L8)
rm(EXP_F10L8)

EXP_F99L8 <- expandRows(F99L8, "Value") 
IDX_ <- sample(1:nrow(EXP_F99L8), min(nrow(EXP_F99L8), 50000))
EXP_F99L8 <- EXP_F99L8[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_F99L8)
rm(EXP_F99L8)


# distributions ->colored for poster
summary(TOTAL)
barplot(table(TOTAL$COUNTRY), col = rainbow(40), xlab = "Country", ylab = "Persons", main = "Country Distribution")
barplot(table(TOTAL$SEX), col = c("#F073E4","#73B6F0"), xlab = "Sex", ylab = "Persons", main = "Gender Distribution")
barplot(table(TOTAL$SEX), col = rainbow(2), xlab = "Sex", ylab = "Persons", main = "Gender Distribution")
barplot(table(TOTAL$ISC11_LEVEL),  col = c("#4800FF", "#7300FF","#F200FF", "#C800FF"), xlab = "ISCED Level of Education", ylab = "Persons", main = "Level of Education Distribution")
barplot(table(TOTAL$ISC11_LEVEL),  col = rainbow(4), xlab = "ISCED Level of Education", ylab = "Persons", main = "Level of Education Distribution")
barplot(table(TOTAL$FIELD),  col = rainbow(12), xlab = "Field of Education", ylab = "Persons", main = "Field of Education Distribution")

# non-colored distribtuions
barplot(table(TOTAL$COUNTRY), xlab = "Country", ylab = "Persons", main = "Country Distribution")
barplot(table(TOTAL$SEX),  xlab = "Sex", ylab = "Persons", main = "Gender Distribution")
barplot(table(TOTAL$ISC11_LEVEL), xlab = "ISCED Level of Education", ylab = "Persons", main = "Level of Education Distribution")
barplot(table(TOTAL$FIELD), xlab = "Field of Education", ylab = "Persons", main = "Field of Education Distribution")

# MODEL APPLICATION
# make test and training
trainingRows <- sample(1:nrow(TOTAL), 0.01*nrow(TOTAL))
training <- TOTAL[trainingRows, ]
temp <- TOTAL[-trainingRows, ]
testRows <- sample(1:nrow(temp), 0.01*nrow(temp))
test <- temp[testRows, ]
summary(training)
summary(test)

# plot training -> ensure distribution matches the whole
barplot(table(training$COUNTRY), names.arg = NULL, xlab = "Country", ylab = "Persons", main = "Country Distribution")
barplot(table(training$SEX), names.arg = NULL, xlab = "Sex", ylab = "Persons", main = "Gender Distribution")
barplot(table(training$ISC11_LEVEL),  names.arg = NULL, xlab = "ISCED Level of Education", ylab = "Persons", main = "Level of Education Distribution")
barplot(table(training$FIELD),  names.arg = NULL, xlab = "Field of Education", ylab = "Persons", main = "Field of Education Distribution")

# plot test -> take a look at distribution (cannot edit)
barplot(table(test$COUNTRY), names.arg = NULL, xlab = "Country", ylab = "Persons", main = "Country Distribution")
barplot(table(test$SEX), names.arg = NULL, xlab = "Sex", ylab = "Persons", main = "Gender Distribution")
barplot(table(test$ISC11_LEVEL),  names.arg = NULL, xlab = "ISCED Level of Education", ylab = "Persons", main = "Level of Education Distribution")
barplot(table(test$FIELD),  names.arg = NULL, xlab = "Field of Education", ylab = "Persons", main = "Field of Education Distribution")

# MODEL 1: SVM
model1F <- svm(FIELD ~ COUNTRY + SEX, 
              data = training)
model1L <- svm(ISC11_LEVEL ~ COUNTRY + SEX, 
              data = training)
# check if adding in level helped
model1F <- svm(FIELD ~ ., 
               data = training)
model1L <- svm(ISC11_LEVEL ~ ., 
              data = training)

# run test and training and save test results
model1F
predTrain <- predict(model1F, training, type = "class")
table(predTrain, training$FIELD)
mean(predTrain != training$FIELD)
predValidSVM <- predict(model1F, test, type = "class")
table(predValidSVM, test$FIELD)
mean(predValidSVM == test$FIELD)
model1L
predTrain <- predict(model1L, training, type = "class")
table(predTrain, training$ISC11_LEVEL)
mean(predTrain != training$ISC11_LEVEL)
predValidSVM2 <- predict(model1L, test, type = "class")
table(predValidSVM2, test$ISC11_LEVEL)
mean(predValidSVM2 == test$ISC11_LEVEL)

# MODEL 2: Random Forest
model2F <- randomForest(FIELD ~ COUNTRY + SEX, 
                       data = training, ntree = 100, 
                       importance = TRUE)
model2L <- randomForest(ISC11_LEVEL ~ COUNTRY + SEX, 
                       data = training, ntree = 100, 
                       importance = TRUE)
# check if adding level helped
  model2F <- randomForest(FIELD ~ ., 
                          data = training, ntree = 100, 
                          importance = TRUE)
  model2L <- randomForest(ISC11_LEVEL ~ ., 
                          data = training, ntree = 100, 
                          importance = TRUE)
  
# run test and training and save test results
model2F
predTrain <- predict(model2F, training, type = "class")
table(predTrain, training$FIELD)
mean(predTrain != training$FIELD)
predValidRF <- predict(model2F, test, type = "class")
table(predValidRF, test$FIELD)
mean(predValidRF == test$FIELD)
model2L
predTrain <- predict(model2L, training, type = "class")
table(predTrain, training$ISC11_LEVEL)
mean(predTrain != training$ISC11_LEVEL)
predValidRF2 <- predict(model2L, test, type = "class")
table(predValidRF2, test$ISC11_LEVEL)
mean(predValidRF2 == test$ISC11_LEVEL)

# Plot the different models
# field
test2 <- rbind(table(test$FIELD), table(predValidSVM))
fieldF <- rbind(test2, table(predValidRF))
#barplot(test2, beside = T, col = c("#3bf340", "#9a00ff"), xlab = "Field of Education", ylab = "Persons", main = "Field of Education Results", legend = c("Actual","SVM Prediction"))
barplot(fieldF, beside = T, col = c("#3bf340", "#9a00ff","#ffa200"), xlab = "Field of Education", ylab = "Persons", main = "Field of Education Results", legend = c("Actual","SVM Prediction","Random Forest Prediction"))
# level
test22 <- rbind(table(test$ISC11_LEVEL), table(predValidSVM2))
levelF <- rbind(test22, table(predValidRF2))
#barplot(test22, beside = T, col = c("#3bf340", "#9a00ff"), xlab = "ISCED Level of Education", ylab = "Persons", main = "Level of Education Results", legend = c("Actual","SVM Prediction"))
barplot(levelF, beside = T, col = c("#3bf340", "#9a00ff","#ffa200"), xlab = "ISCED Level of Education", ylab = "Persons", main = "Level of Education Results", legend = c("Actual","SVM Prediction","Random Forest Prediction"))
  
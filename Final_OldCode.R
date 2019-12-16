### THE CODE BELOW IS ALL CODE THAT WAS
### ONCE USED IN THE PROJECT. IT IS NO
### LONGER USED AND WAS MOVED TO THIS
### FILE SO THAT OLDER PROCESSES COULD
### BE REPEATED AS NECESSARY.

# Degree field and location effects on degree type
ggplot(INFO_clean2, aes(x = INFO_clean2$Field.of.education, y = INFO_clean2$Country)) + geom_point(aes(color = INFO_clean2$Level.of.education))
ggplot(clean, aes(x = clean$Field.of.education, y = clean$Country)) + geom_point(aes(color = clean$Level.of.education))
ggplot(clean5, aes(x = clean5$Field.of.education, y = clean5$Country)) + geom_point(aes(color = clean5$Level.of.education))
ggplot(clean6, aes(x = clean6$Field.of.education, y = clean6$Country)) + geom_point(aes(color = clean6$Level.of.education))
ggplot(clean7, aes(x = clean7$Field.of.education, y = clean7$Country)) + geom_point(aes(color = clean7$Level.of.education))
ggplot(clean8, aes(x = clean8$Field.of.education, y = clean8$Country)) + geom_point(aes(color = clean8$Level.of.education))

# Step 2: looking into gender differences by field DO NOT USE
cleanMF <- subset(clean, !(clean$Sex == "Total"))
counts <- table(cleanMF$Sex, cleanMF$Field.of.education)
barplot(counts, col=c("blue","green", "red"))
print(counts)
summary(clean)
unique(clean$Country)

# STEP 3: digging into one country at a time DO NOT USE
AUS <- subset(clean, clean$Country == "Australia")
summary(AUS)
AUSmf <- subset(AUS, !(AUS$Sex == "Total"))
countsAUS <- table(AUSmf$Sex, AUSmf$Field.of.education)
barplot(countsAUS, col=c("blue","green", "red"))
print(countsAUS)
summary(AUS$Field.of.education == "Agriculture")
AUSagr13 <- subset(AUS, AUS$Field.of.education == "Agriculture" & AUS$Year == "2013" & AUS$Sex != "Total" & AUS$Level.of.education == "Bachelor’s or equivalent level (ISCED2011 level 6)")
summary(AUSagr13)
countsAUS13 <- table(AUSagr13$Sex, AUSagr13$Field.of.education)
barplot(countsAUS13, col=c("blue","green", "red"))

# Step 4: make some damn tables 
# Table 1: one country, sum of each field type
AUSagg <- aggregate(AUS$Value, list(AUS$Field.of.education), sum)
head(AUSagg)
print(AUSagg)
barplot(AUSagg$x,names=AUSagg$Group.1)
# Tables 2-4: splitting by gender
AUSF <- subset(AUS, AUS$Sex == "Women")
summary(AUSF)
AUSFagg <- aggregate(AUSF$Value, list(AUSF$Field.of.education), sum)
head(AUSFagg)
print(AUSFagg)
barplot(AUSFagg$x,names=AUSFagg$Group.1)

AUSM <- subset(AUS, AUS$Sex == "Men")
summary(AUSM)
AUSMagg <- aggregate(AUSM$Value, list(AUSM$Field.of.education), sum)
head(AUSMagg)
print(AUSMagg)
barplot(AUSMagg$x,names=AUSMagg$Group.1)

Diff <- AUSMagg$x - AUSFagg$x
head(Diff)
print(Diff)
print(AUSMagg$Group.1)
barplot(Diff,names=AUSMagg$Group.1, names.arg = NULL) # More men is positive

# Main ones that are high
cDiff[15] # Education -> Female
cDiff[16] # Engineering -> Male
cDiff[17] # Engineering -> Male
cDiff[25] # Health -> Female
cDiff[26] # Health and Welfare-> Female
cDiff[31] # Information and Communication Technologies -> Male
cDiff[60] # Total -> Female


clean13 <- clean13[c(1, 2, 3, 5, 7)]
C13 <- data.table(clean13)
C13[, clean13$Field := ifelse(clean13$Field.of.education %in% c("Information and Communication Technologies", "Interdisciplinary programmes involving broad field 06"), "ICT",
                              ifelse(clean13$Field.of.education %in% c("Arts and humanities"), "Arts/Humanities",
                                     NA))]

clean13$Field <- NA
clean13$Field[(clean13$Field.of.education == "Arts and humanities")|(clean13$Field.of.education == "Arts")|(clean13$Field.of.education == "Arts and humanities not elsewhere classified")] <- 1
clean13$Field[(clean13$Field.of.education == "Arts and humanities not further defined")|(clean13$Field.of.education == "Humanities (except languages)")|(clean13$Field.of.education == "Interdisciplinary programmes involving broad field 02")|(clean13$Field.of.education == "Languages")] <- 1
clean13$Field[(clean13$Field.of.education == "Social and behavioural sciences")|(clean13$Field.of.education == "Social sciences, journalism and information")|(clean13$Field.of.education == "Social sciences, journalism and information n.f.d.")] <- 2
clean13$Field[(clean13$Field.of.education == "Social sciences, journalism and information not elsewhere classified")|(clean13$Field.of.education == "Interdisciplinary programmes involving broad field 03")|(clean13$Field.of.education == "Journalism and information")] <- 2
clean13$Field[(clean13$Field.of.education == "Business and administration")|(clean13$Field.of.education == "Business, administration and law")|(clean13$Field.of.education == "Business, administration and law not elsewhere classified")] <- 3
clean13$Field[(clean13$Field.of.education == "Business, administration and law not further defined")|(clean13$Field.of.education == "Interdisciplinary programmes involving broad field 04")|(clean13$Field.of.education == "Law")] <- 3
clean13$Field[(clean13$Field.of.education == "")|(clean13$Field.of.education == "")|(clean13$Field.of.education == "Natural sciences, mathematics and statistics")|(clean13$Field.of.education == "Natural sciences, mathematics and statistics n.e.c.")|(clean13$Field.of.education == "Mathematics and statistics")] <- 4
clean13$Field[(clean13$Field.of.education == "Biological and related sciences")|(clean13$Field.of.education == "Environment")|(clean13$Field.of.education == "Interdisciplinary programmes involving broad field 05")|(clean13$Field.of.education == "Physical sciences")] <- 4
clean13$Field[(clean13$Field.of.education == "Architecture and construction")|(clean13$Field.of.education == "Engineering and engineering trades")|(clean13$Field.of.education == "Engineering, manufacturing and construction")|(clean13$Field.of.education == "Engineering, manufacturing and construction n.e.c.")] <- 5
clean13$Field[(clean13$Field.of.education == "Engineering, manufacturing and construction n.f.d.")|(clean13$Field.of.education == "Interdisciplinary programmes involving broad field 07")|(clean13$Field.of.education == "Manufacturing and processing")] <- 5
clean13$Field[(clean13$Field.of.education == "Veterinary")|(clean13$Field.of.education == "Forestry")|(clean13$Field.of.education == "Interdisciplinary programmes involving broad field 08")|(clean13$Field.of.education == "Agriculture, forestry, fisheries and veterinary")] <- 6
clean13$Field[(clean13$Field.of.education == "Fisheries")|(clean13$Field.of.education == "Agriculture")|(clean13$Field.of.education == "Agriculture, forestry, fisheries and veterinary n.e.c.")|(clean13$Field.of.education == "Agriculture, forestry, fisheries and veterinary n.f.d.")] <- 6
clean13$Field[(clean13$Field.of.education == "Health")|(clean13$Field.of.education == "Health and welfare")|(clean13$Field.of.education == "Interdisciplinary programmes involving broad field 09")] <- 7
clean13$Field[(clean13$Field.of.education == "Welfare")|(clean13$Field.of.education == "Health and welfare not elsewhere classified")|(clean13$Field.of.education == "Health and welfare not further defined")] <- 7
clean13$Field[(clean13$Field.of.education == "Personal services")|(clean13$Field.of.education == "Hygiene and occupational health services")|(clean13$Field.of.education == "Transport services")|(clean13$Field.of.education == "Services not further defined")] <- 8
clean13$Field[(clean13$Field.of.education == "Services")|(clean13$Field.of.education == "Interdisciplinary programmes involving broad field 10")|(clean13$Field.of.education == "Security services")|(clean13$Field.of.education == "Services not elsewhere classified")] <- 8
clean13$Field[(clean13$Field.of.education == "Information and Communication Technologies")|(clean13$Field.of.education == "Interdisciplinary programmes involving broad field 06")] <- 9
clean13$Field[(clean13$Field.of.education == "Interdisciplinary programmes involving broad field 01")|(clean13$Field.of.education == "Generic programmes and qualifications")|(clean13$Field.of.education == "Education")] <- 10
clean13$Field[clean13$Field.of.education == "Field unknown"] <- 11
unique(clean13$Field.of.education)
summary(class(clean13$Country))
summary(clean13)
clean13 <- clean13[c(1,2,4,5,6)]
clean13

clean14 <- subset(clean, !(clean$Sex == "Total" | clean$Field.of.education == "Total: All fields of education" | clean$Level.of.education == "Total tertiary education (ISCED2011 levels 5 to 8)" | clean$Country == "OECD - Europe" | clean$Country == "OECD - Total"| clean$Year != "2014"))
clean15 <- subset(clean, !(clean$Sex == "Total" | clean$Field.of.education == "Total: All fields of education" | clean$Level.of.education == "Total tertiary education (ISCED2011 levels 5 to 8)" | clean$Country == "OECD - Europe" | clean$Country == "OECD - Total"| clean$Year != "2015"))
clean16 <- subset(clean, !(clean$Sex == "Total" | clean$Field.of.education == "Total: All fields of education" | clean$Level.of.education == "Total tertiary education (ISCED2011 levels 5 to 8)" | clean$Country == "OECD - Europe" | clean$Country == "OECD - Total"| clean$Year != "2016"))
clean17 <- subset(clean, !(clean$Sex == "Total" | clean$Field.of.education == "Total: All fields of education" | clean$Level.of.education == "Total tertiary education (ISCED2011 levels 5 to 8)" | clean$Country == "OECD - Europe" | clean$Country == "OECD - Total"| clean$Year != "2017"))


# MODEL 3: time analysis
cleanEng <- subset(clean, !clean$Level.of.education == "Total tertiary education (ISCED2011 levels 5 to 8)" & clean$Field.of.education == "Engineering, manufacturing and construction" & !clean$Sex == "Total")
# group by country and gender
Agg <- aggregate(cleanEng$Value, list(cleanEng$Country, cleanEng$Sex, cleanEng$Year, cleanEng$Level.of.education), sum)
summary(Agg)
cleanEng <- subset(cleanEng, (cleanEng$Country == "United States"))
cleanEng2 <- subset(cleanEng, (cleanEng$Country == "Russia"))
summary(cleanEng)
ggplot(Agg, aes(x = Agg$Group.3, y = Agg$x)) + geom_point(aes(color = Agg$Group.2))  + labs(x = "Year") + labs(color = "Country") + labs(y = "Persons in Program") #+ geom_text(aes(label = cleanEng$Country)) 

# filter on engineering


# PRELIMINARY ANALYSIS
# Gender
# ALL fields, sum of each country type
Agg <- aggregate(cleanLT$Value, list(cleanLT$Country), sum)
head(Agg)
print(Agg)
barplot(Agg$x,names=Agg$Group.1)
# Female
cleanF <- subset(cleanLT, cleanLT$Sex == "Women")
AggF <- aggregate(cleanF$Value, list(cleanF$Country), sum)
head(AggF)
print(AggF)
barplot(AggF$x,names=AggF$Group.1)
# Male
cleanM <- subset(cleanLT, cleanLT$Sex == "Men")
AggM <- aggregate(cleanM$Value, list(cleanM$Country), sum)
head(AggM)
print(AggM)
barplot(AggM$x,names=AggM$Group.1)
# Diff (Male-Female)
cDiff <- AggM$x - AggF$x
head(cDiff)
print(cDiff)
print(AggM$Group.1)
barplot(cDiff,names=AggM$Group.1, names.arg = NULL, ylab="Difference in Males/Females",cex.lab = 1.5) # More men is positive

# ALL country, sum of each field type
Agg <- aggregate(cleanLT$Value, list(cleanLT$Field.of.education), sum)
head(Agg)
print(Agg)
barplot(Agg$x,names=Agg$Group.1)
# Female
cleanF <- subset(cleanLT, cleanLT$Sex == "Women")
AggF <- aggregate(cleanF$Value, list(cleanF$Field.of.education), sum)
head(AggF)
print(AggF)
barplot(AggF$x,names=AggF$Group.1)
# Male
cleanM <- subset(cleanLT, cleanLT$Sex == "Men")
AggM <- aggregate(cleanM$Value, list(cleanM$Field.of.education), sum)
head(AggM)
print(AggM)
barplot(AggM$x,names=AggM$Group.1)
# Diff (Male-Female)
cDiff <- AggM$x - AggF$x
head(cDiff)
print(cDiff)
print(AggM$Group.1)
barplot(cDiff,names=AggM$Group.1, names.arg = NULL, ylab="Difference in Males/Females", cex.lab = 1.5) # More men is positive

# ALL level, sum of each field type
Agg <- aggregate(cleanFT$Value, list(cleanFT$Level.of.education), sum)
Agg <- Agg[c(4,1,3,2,5),]
head(Agg)
print(Agg)
barplot(Agg$x,names=Agg$Group.1)
# Female
cleanF <- subset(cleanFT, cleanFT$Sex == "Women")
AggF <- aggregate(cleanF$Value, list(cleanF$Level.of.education), sum)
AggF <- AggF[c(4,1,3,2,5),]
head(AggF)
print(AggF)
barplot(AggF$x,names=AggF$Group.1)
# Male
cleanM <- subset(cleanFT, cleanFT$Sex == "Men")
AggM <- aggregate(cleanM$Value, list(cleanM$Level.of.education), sum)
AggM <- AggM[c(4,1,3,2,5),]
head(AggM)
print(AggM)
barplot(AggM$x,names=AggM$Group.1)
# Diff (Male-Female)
cDiff <- AggM$x - AggF$x
head(cDiff)
print(cDiff)
print(AggM$Group.1)
barplot(cDiff,names=AggM$Group.1, names.arg = NULL, ylab="Difference in Males/Females", cex.lab = 1.5) # More men is positive

# Time versus gender

# Countries
# all fields and all levels
cleanFLT <- subset(clean, clean$Level.of.education == "Total tertiary education (ISCED2011 levels 5 to 8)" & clean$Field.of.education == "Total: All fields of education" & clean$Sex == "Total")
cleanFLT <- subset(cleanFLT, !(cleanFLT$Country == "OECD - Europe" | cleanFLT$Country == "OECD - Total"))
summary(cleanFLT)
ggplot(cleanFLT, aes(x = cleanFLT$Year, y = cleanFLT$Value)) + geom_point(aes(color = cleanFLT$Country)) + labs(x = "Year") + labs(color = "Country") + labs(y = "Persons in Program") #+ geom_text(aes(label = cleanFLT$Country))

# Engineering
cleanEng <- subset(clean, clean$Level.of.education == "Total tertiary education (ISCED2011 levels 5 to 8)" & clean$Field.of.education == "Engineering and engineering trades" & clean$Sex == "Total")
cleanEng <- subset(cleanEng, !(cleanEng$Country == "OECD - Europe" | cleanEng$Country == "OECD - Total"))
summary(cleanEng)
ggplot(cleanEng, aes(x = cleanEng$Year, y = cleanEng$Value)) + geom_point(aes(color = cleanEng$Country))  + labs(x = "Year") + labs(color = "Country") + labs(y = "Persons in Program") #+ geom_text(aes(label = cleanEng$Country)) 

# Doctoral
cleanDoc <- subset(clean, clean$Level.of.education == "Doctoral or equivalent level  (ISCED2011 level 8)" & clean$Field.of.education == "Total: All fields of education" & clean$Sex == "Total")
cleanDoc <- subset(cleanDoc, !(cleanDoc$Country == "OECD - Europe" | cleanDoc$Country == "OECD - Total"))
summary(cleanDoc)
ggplot(cleanDoc, aes(x = cleanDoc$Year, y = cleanDoc$Value)) + geom_point(aes(color = cleanDoc$Country)) + labs(x = "Year") + labs(color = "Country") + labs(y = "Persons in Program") #+ geom_text(aes(label = cleanDoc$Country))



# MODEL 1: k-prototype
summary(clean)
cleanAll <- subset(clean, !(clean$Sex == "Total" | clean$Field.of.education == "Total: All fields of education" | clean$Level.of.education == "Total tertiary education (ISCED2011 levels 5 to 8)" | clean$Country == "OECD - Europe" | clean$Country == "OECD - Total"))
cleanGroup <- subset(cleanAll, (cleanAll$Field.of.education == "Generic programmes and qualifications")|(cleanAll$Field.of.education == "Education")|(cleanAll$Field.of.education == "Information and Communication Technologies")|(cleanAll$Field.of.education == "Health and welfare")|(cleanAll$Field.of.education == "Services")|(cleanAll$Field.of.education == "Engineering, manufacturing and construction")|(cleanAll$Field.of.education == "Agriculture, forestry, fisheries and veterinary")|(cleanAll$Field.of.education == "Business, administration and law")|(cleanAll$Field.of.education == "Natural sciences, mathematics and statistics")|(cleanAll$Field.of.education == "Arts and humanities")|(cleanAll$Field.of.education == "Social sciences, journalism and information"))
summary(cleanGroup)
cleanGroupSub <- cleanGroup[c(1, 2, 3, 5, 6, 7)]
summary(cleanGroupSub)
cleanGroupSub$Field.of.education <- factor(cleanGroupSub$Field.of.education)
cleanGroupSub$Sex <- factor(cleanGroupSub$Sex)
cleanGroupSub$Country <- factor(cleanGroupSub$Country)
cleanGroupSub$Level.of.education <- factor(cleanGroupSub$Level.of.education)

clean13 <- subset(cleanGroupSub, (cleanGroupSub$Year == "2013"))
clean14 <- subset(cleanGroupSub, (cleanGroupSub$Year == "2014"))
clean15 <- subset(cleanGroupSub, (cleanGroupSub$Year == "2015"))
clean16 <- subset(cleanGroupSub, (cleanGroupSub$Year == "2016"))
clean17 <- subset(cleanGroupSub, (cleanGroupSub$Year == "2017"))
summary(clean13)

clean13 <- clean13[-c(5)]
clean14 <- clean14[-c(5)]
clean15 <- clean15[-c(5)]
clean16 <- clean16[-c(5)]
clean17 <- clean17[-c(5)]

model1 <- kproto(cleanAll, 4, iter.max = 10000, nstart = 1) #na.rm = TRUE
clprofiles(model1, cleanAll)
model13 <- kproto(clean13, 10) #na.rm = TRUE 4, 10,11,38,40
summary(model13)
clprofiles(model13, clean13)
model14 <- kproto(clean14, 10) #na.rm = TRUE
summary(model14)
clprofiles(model14, clean14)
model15 <- kproto(clean15, 10) #na.rm = TRUE
summary(model15)
clprofiles(model15, clean15)
model16 <- kproto(clean16, 10) #na.rm = TRUE
summary(model16)
clprofiles(model16, clean16)

model17 <- kproto(clean17, 4) #na.rm = TRUE
summary(model17)
clprofiles(model17, clean17)
model17 <- kproto(clean17, 10) #na.rm = TRUE
summary(model17)
clprofiles(model17, clean17)
model17 <- kproto(clean17, 15) #na.rm = TRUE
summary(model17)
clprofiles(model17, clean17)
model17 <- kproto(clean17, 20) #na.rm = TRUE
summary(model17)
clprofiles(model17, clean17)
model17 <- kproto(clean17, 40) #na.rm = TRUE
summary(model17)
clprofiles(model17, clean17)


# MODEL 2: multinomial logistic regression
summary(clean17)
sum(clean17$Value)
colnames(clean17) <- c("Country", "Sex", "Field", "Level", "Value")
#expanded <- expandRows(clean17, "Value") #nope
#summary(expanded)
#colnames(clean17) <- make.names(colnames(clean17))
trainingRows <- sample(1:nrow(clean17), 0.1*nrow(clean17))
training <- clean17[trainingRows, ]
test <- clean17[-trainingRows, ]
summary(training)
summary(test)

# want to make one "person" for each value
#train.expanded <- training[rep(row.names(training), training$Value), 1:4]
#summary(train.expanded)
texpanded <- expandRows(training, "Value") #nope
summary(texpanded)
trainingRows2 <- sample(1:nrow(texpanded), 0.01*nrow(texpanded))
training2 <- texpanded[trainingRows2, ]
test2 <- texpanded[-trainingRows2, ]
summary(training2)
summary(test2)

trainingRows3 <- sample(1:nrow(test2), 0.01*nrow(test2))
test3 <- test2[trainingRows3, ]
summary(test3)


model23 <- multinom(Field ~., data = texpanded)
predicted <- predict(model23, texpanded, type = "class")
summary(predicted)
table(predicted,training$Field)
model23 <- multinom(Field ~.)
predicted <- predict(model23, training)
summary(predicted)
summary(training$Field)
table(predicted,training$Field)


# Random Forest
model1 <- randomForest(Field ~ ., data = training, importance = TRUE) #train
model2 <- randomForest(Field ~ ., data = training, ntree = 500, mtry = 4, importance = TRUE)

model1 <- randomForest(Field ~ ., data = training2, importance = TRUE) #train
model2 <- randomForest(Field ~ ., data = training2, ntree = 500, mtry = 3, importance = TRUE)

predTrain <- predict(model1, training2, type = "class")
table(predTrain, training2$Field)
mean(predTrain == training2$Field)
predValid <- predict(model1, test3, type = "class")
table(predValid, test3$Field)
mean(predValid == test3$Field)
predTrain <- predict(model2, training2, type = "class")
table(predTrain, training2$Field)
mean(predTrain == training2$Field)
predValid <- predict(model2, test3, type = "class")
table(predValid, test3$Field)
mean(predValid == test3$Field)

importance(model2)
varImpPlot(model2)

predTrain <- predict(model1, training, type = "class")
table(predTrain, training$Field)
predValid <- predict(model1, test, type = "class")
table(predValid, test$Field)

predTrain <- predict(model2, training, type = "class")
summary(predTrain)
table(predTrain, training$Field)
predValid <- predict(model2, test, type = "class")
table(predValid, test$Field)


model_dt <- train(Field ~ ., data=training, method = "rpart")
model_dt_1 = predict(model_dt, data=training)
table(model_dt_1, training$Field)
mean(model_dt_1 == training$Fieldn)

# MODEL 1: k-prototype
summary(clean)
cleanAll <- subset(clean, !(clean$Sex == "Total" | clean$Field.of.education == "Total: All fields of education" | clean$Level.of.education == "Total tertiary education (ISCED2011 levels 5 to 8)" | clean$Country == "OECD - Europe" | clean$Country == "OECD - Total"))
cleanGroup <- subset(cleanAll, (cleanAll$Field.of.education == "Generic programmes and qualifications")|(cleanAll$Field.of.education == "Education")|(cleanAll$Field.of.education == "Information and Communication Technologies")|(cleanAll$Field.of.education == "Health and welfare")|(cleanAll$Field.of.education == "Services")|(cleanAll$Field.of.education == "Engineering, manufacturing and construction")|(cleanAll$Field.of.education == "Agriculture, forestry, fisheries and veterinary")|(cleanAll$Field.of.education == "Business, administration and law")|(cleanAll$Field.of.education == "Natural sciences, mathematics and statistics")|(cleanAll$Field.of.education == "Arts and humanities")|(cleanAll$Field.of.education == "Social sciences, journalism and information"))
summary(cleanGroup)
cleanGroupSub <- cleanGroup[c(1, 2, 3, 5, 6, 7)]
summary(cleanGroupSub)
cleanGroupSub$Field.of.education <- factor(cleanGroupSub$Field.of.education)
cleanGroupSub$Sex <- factor(cleanGroupSub$Sex)
cleanGroupSub$Country <- factor(cleanGroupSub$Country)
cleanGroupSub$Level.of.education <- factor(cleanGroupSub$Level.of.education)

clean13 <- subset(cleanGroupSub, (cleanGroupSub$Year == "2013"))
clean14 <- subset(cleanGroupSub, (cleanGroupSub$Year == "2014"))
clean15 <- subset(cleanGroupSub, (cleanGroupSub$Year == "2015"))
clean16 <- subset(cleanGroupSub, (cleanGroupSub$Year == "2016"))
clean17 <- subset(cleanGroupSub, (cleanGroupSub$Year == "2017"))
summary(clean13)

clean13 <- clean13[-c(5)]
clean14 <- clean14[-c(5)]
clean15 <- clean15[-c(5)]
clean16 <- clean16[-c(5)]
clean17 <- clean17[-c(5)]

model1 <- kproto(cleanAll, 4, iter.max = 10000, nstart = 1) #na.rm = TRUE
clprofiles(model1, cleanAll)
model13 <- kproto(clean13, 10) #na.rm = TRUE 4, 10,11,38,40
summary(model13)
clprofiles(model13, clean13)
model14 <- kproto(clean14, 10) #na.rm = TRUE
summary(model14)
clprofiles(model14, clean14)
model15 <- kproto(clean15, 10) #na.rm = TRUE
summary(model15)
clprofiles(model15, clean15)
model16 <- kproto(clean16, 10) #na.rm = TRUE
summary(model16)
clprofiles(model16, clean16)

model17 <- kproto(clean17, 4) #na.rm = TRUE
summary(model17)
clprofiles(model17, clean17)
model17 <- kproto(clean17, 10) #na.rm = TRUE
summary(model17)
clprofiles(model17, clean17)
model17 <- kproto(clean17, 15) #na.rm = TRUE
summary(model17)
clprofiles(model17, clean17)
model17 <- kproto(clean17, 20) #na.rm = TRUE
summary(model17)
clprofiles(model17, clean17)
model17 <- kproto(clean17, 40) #na.rm = TRUE
summary(model17)
clprofiles(model17, clean17)


# MODEL 2: multinomial logistic regression
summary(clean17)
sum(clean17$Value)
colnames(clean17) <- c("Country", "Sex", "Field", "Level", "Value")
#expanded <- expandRows(clean17, "Value") #nope
#summary(expanded)
#colnames(clean17) <- make.names(colnames(clean17))
trainingRows <- sample(1:nrow(clean17), 0.1*nrow(clean17))
training <- clean17[trainingRows, ]
test <- clean17[-trainingRows, ]
summary(training)
summary(test)

# want to make one "person" for each value
#train.expanded <- training[rep(row.names(training), training$Value), 1:4]
#summary(train.expanded)
texpanded <- expandRows(training, "Value") #nope
summary(texpanded)
trainingRows2 <- sample(1:nrow(texpanded), 0.01*nrow(texpanded))
training2 <- texpanded[trainingRows2, ]
test2 <- texpanded[-trainingRows2, ]
summary(training2)
summary(test2)

trainingRows3 <- sample(1:nrow(test2), 0.01*nrow(test2))
test3 <- test2[trainingRows3, ]
summary(test3)


model23 <- multinom(Field ~., data = texpanded)
predicted <- predict(model23, texpanded, type = "class")
summary(predicted)
table(predicted,training$Field)
model23 <- multinom(Field ~.)
predicted <- predict(model23, training)
summary(predicted)
summary(training$Field)
table(predicted,training$Field)


# Random Forest
model1 <- randomForest(Field ~ ., data = training, importance = TRUE) #train
model2 <- randomForest(Field ~ ., data = training, ntree = 500, mtry = 4, importance = TRUE)

model1 <- randomForest(Field ~ ., data = training2, importance = TRUE) #train
model2 <- randomForest(Field ~ ., data = training2, ntree = 500, mtry = 3, importance = TRUE)

predTrain <- predict(model1, training2, type = "class")
table(predTrain, training2$Field)
mean(predTrain == training2$Field)
predValid <- predict(model1, test3, type = "class")
table(predValid, test3$Field)
mean(predValid == test3$Field)
predTrain <- predict(model2, training2, type = "class")
table(predTrain, training2$Field)
mean(predTrain == training2$Field)
predValid <- predict(model2, test3, type = "class")
table(predValid, test3$Field)
mean(predValid == test3$Field)

importance(model2)
varImpPlot(model2)

predTrain <- predict(model1, training, type = "class")
table(predTrain, training$Field)
predValid <- predict(model1, test, type = "class")
table(predValid, test$Field)

predTrain <- predict(model2, training, type = "class")
summary(predTrain)
table(predTrain, training$Field)
predValid <- predict(model2, test, type = "class")
table(predValid, test$Field)


model_dt <- train(Field ~ ., data=training, method = "rpart")
model_dt_1 = predict(model_dt, data=training)
table(model_dt_1, training$Field)
mean(model_dt_1 == training$Fieldn)

clean_split <- split(cleanNT2, list(cleanNT2$COUNTRY, cleanNT2$SEX))
summary(clean_split)

# each individual pair
AUSF <- clean_split$AUS.F
AUTF <- clean_split$AUT.F
BELF <- clean_split$BEL.F
BRAF <- clean_split$BRA.F
CANF <- clean_split$CAN.F
CHEF <- clean_split$CHE.F
CHLF <- clean_split$CHL.F
COLF <- clean_split$COL.F
CRIF <- clean_split$CRI.F
CZEF <- clean_split$CZE.F
DEUF <- clean_split$DEU.F
DNKF <- clean_split$DNK.F
ESPF <- clean_split$ESP.F
ESTF <- clean_split$EST.F
FINF <- clean_split$FIN.F
FRAF <- clean_split$FRA.F
GBRF <- clean_split$GBR.F
HUNF <- clean_split$HUN.F
IRLF <- clean_split$IRL.F
ISLF <- clean_split$ISL.F
ISRF <- clean_split$ISR.F
ITAF <- clean_split$ITA.F
KORF <- clean_split$KOR.F
LTUF <- clean_split$LTU.F
LUXF <- clean_split$LUX.F
LVAF <- clean_split$LVA.F
MEXF <- clean_split$MEX.F
NLDF <- clean_split$NLD.F
NORF <- clean_split$NOR.F
POLF <- clean_split$POL.F
PRTF <- clean_split$PRT.F
RUSF <- clean_split$RUS.F
SVKF <- clean_split$SVK.F
SVNF <- clean_split$SVN.F
SWEF <- clean_split$SWE.F
TURF <- clean_split$TUR.F
USAF <- clean_split$USA.F

AUSM <- clean_split$AUS.M
AUTM <- clean_split$AUT.M
BELM <- clean_split$BEL.M
BRAM <- clean_split$BRA.M
CANM <- clean_split$CAN.M
CHEM <- clean_split$CHE.M
CHLM <- clean_split$CHL.M
COLM <- clean_split$COL.M
CRIM <- clean_split$CRI.M
CZEM <- clean_split$CZE.M
DEUM <- clean_split$DEU.M
DNKM <- clean_split$DNK.M
ESPM <- clean_split$ESP.M
ESTM <- clean_split$EST.M
FINM <- clean_split$FIN.M
FRAM <- clean_split$FRA.M
GBRM <- clean_split$GBR.M
HUNM <- clean_split$HUN.M
IRLM <- clean_split$IRL.M
ISLM <- clean_split$ISL.M
ISRM <- clean_split$ISR.M
ITAM <- clean_split$ITA.M
KORM <- clean_split$KOR.M
LTUM <- clean_split$LTU.M
LUXM <- clean_split$LUX.M
LVAM <- clean_split$LVA.M
MEXM <- clean_split$MEX.M
NLDM <- clean_split$NLD.M
NORM <- clean_split$NOR.M
POLM <- clean_split$POL.M
PRTM <- clean_split$PRT.M
RUSM <- clean_split$RUS.M
SVKM <- clean_split$SVK.M
SVNM <- clean_split$SVN.M
SWEM <- clean_split$SWE.M
TURM <- clean_split$TUR.M
USAM <- clean_split$USA.M

# create expanded versions of each
EXP_AUSF <- expandRows(AUSF, "Value") 
IDX_ <- sample(1:nrow(EXP_AUSF), 0.30*nrow(EXP_AUSF))
EXP_AUSF <- EXP_AUSF[IDX_, ]

EXP_AUTF <- expandRows(AUTF, "Value") 
IDX_ <- sample(1:nrow(EXP_AUTF), 0.30*nrow(EXP_AUTF))
EXP_AUTF <- EXP_AUTF[IDX_, ]
TOTAL <- rbind(EXP_AUSF, EXP_AUTF)
rm(EXP_AUSF, EXP_AUTF)

EXP_BELF <- expandRows(BELF, "Value") 
IDX_ <- sample(1:nrow(EXP_BELF), 0.30*nrow(EXP_BELF))
EXP_BELF <- EXP_BELF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_BELF)
rm(EXP_BELF)

EXP_BRAF <- expandRows(BRAF, "Value") 
IDX_ <- sample(1:nrow(EXP_BRAF), 0.30*nrow(EXP_BRAF))
EXP_BRAF <- EXP_BRAF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_BRAF)
rm(EXP_BRAF)

EXP_CANF <- expandRows(CANF, "Value") 
IDX_ <- sample(1:nrow(EXP_CANF), 0.30*nrow(EXP_CANF))
EXP_CANF <- EXP_CANF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_CANF)
rm(EXP_CANF)

EXP_CHEF <- expandRows(CHEF, "Value") 
IDX_ <- sample(1:nrow(EXP_CHEF), 0.30*nrow(EXP_CHEF))
EXP_CHEF <- EXP_CHEF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_CHEF)
rm(EXP_CHEF)

EXP_CHLF <- expandRows(CHLF, "Value") 
IDX_ <- sample(1:nrow(EXP_CHLF), 0.30*nrow(EXP_CHLF))
EXP_CHLF <- EXP_CHLF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_CHLF)
rm(EXP_CHLF)

EXP_COLF <- expandRows(COLF, "Value") 
IDX_ <- sample(1:nrow(EXP_COLF), 0.30*nrow(EXP_COLF))
EXP_COLF <- EXP_COLF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_COLF)
rm(EXP_COLF)

EXP_CRIF <- expandRows(CRIF, "Value") 
IDX_ <- sample(1:nrow(EXP_CRIF), 0.30*nrow(EXP_CRIF))
EXP_CRIF <- EXP_CRIF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_CRIF)
rm(EXP_CRIF)

EXP_CZEF <- expandRows(CZEF, "Value") 
IDX_ <- sample(1:nrow(EXP_CZEF), 0.30*nrow(EXP_CZEF))
EXP_CZEF <- EXP_CZEF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_CZEF)
rm(EXP_CZEF)

EXP_DEUF <- expandRows(DEUF, "Value") 
IDX_ <- sample(1:nrow(EXP_DEUF), 0.30*nrow(EXP_DEUF))
EXP_DEUF <- EXP_DEUF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_DEUF)
rm(EXP_DEUF)

EXP_DNKF <- expandRows(DNKF, "Value") 
IDX_ <- sample(1:nrow(EXP_DNKF), 0.30*nrow(EXP_DNKF))
EXP_DNKF <- EXP_DNKF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_DNKF)
rm(EXP_DNKF)

EXP_ESPF <- expandRows(ESPF, "Value") 
IDX_ <- sample(1:nrow(EXP_ESPF), 0.30*nrow(EXP_ESPF))
EXP_ESPF <- EXP_ESPF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_ESPF)
rm(EXP_ESPF)

EXP_ESTF <- expandRows(ESTF, "Value") 
IDX_ <- sample(1:nrow(EXP_ESTF), 0.30*nrow(EXP_ESTF))
EXP_ESTF <- EXP_ESTF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_ESTF)
rm(EXP_ESTF)

EXP_FINF <- expandRows(FINF, "Value") 
IDX_ <- sample(1:nrow(EXP_FINF), 0.30*nrow(EXP_FINF))
EXP_FINF <- EXP_FINF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_FINF)
rm(EXP_FINF)

EXP_FRAF <- expandRows(FRAF, "Value") 
IDX_ <- sample(1:nrow(EXP_FRAF), 0.30*nrow(EXP_FRAF))
EXP_FRAF <- EXP_FRAF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_FRAF)
rm(EXP_FRAF)

EXP_GBRF <- expandRows(GBRF, "Value") 
IDX_ <- sample(1:nrow(EXP_GBRF), 0.30*nrow(EXP_GBRF))
EXP_GBRF <- EXP_GBRF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_GBRF)
rm(EXP_GBRF)

EXP_HUNF <- expandRows(HUNF, "Value") 
IDX_ <- sample(1:nrow(EXP_HUNF), 0.30*nrow(EXP_HUNF))
EXP_HUNF <- EXP_HUNF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_HUNF)
rm(EXP_HUNF)

EXP_IRLF <- expandRows(IRLF, "Value") 
IDX_ <- sample(1:nrow(EXP_IRLF), 0.30*nrow(EXP_IRLF))
EXP_IRLF <- EXP_IRLF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_IRLF)
rm(EXP_IRLF)

EXP_ISLF <- expandRows(ISLF, "Value") 
IDX_ <- sample(1:nrow(EXP_ISLF), 0.30*nrow(EXP_ISLF))
EXP_ISLF <- EXP_ISLF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_ISLF)
rm(EXP_ISLF)

EXP_ISRF <- expandRows(ISRF, "Value") 
IDX_ <- sample(1:nrow(EXP_ISRF), 0.30*nrow(EXP_ISRF))
EXP_ISRF <- EXP_ISRF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_ISRF)
rm(EXP_ISRF)

EXP_ITAF <- expandRows(ITAF, "Value") 
IDX_ <- sample(1:nrow(EXP_ITAF), 0.30*nrow(EXP_ITAF))
EXP_ITAF <- EXP_ITAF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_ITAF)
rm(EXP_ITAF)

EXP_KORF <- expandRows(KORF, "Value") 
IDX_ <- sample(1:nrow(EXP_KORF), 0.30*nrow(EXP_KORF))
EXP_KORF <- EXP_KORF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_KORF)
rm(EXP_KORF)

EXP_LTUF <- expandRows(LTUF, "Value") 
IDX_ <- sample(1:nrow(EXP_LTUF), 0.30*nrow(EXP_LTUF))
EXP_LTUF <- EXP_LTUF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_LTUF)
rm(EXP_LTUF)

EXP_LUXF <- expandRows(LUXF, "Value") 
IDX_ <- sample(1:nrow(EXP_LUXF), 0.30*nrow(EXP_LUXF))
EXP_LUXF <- EXP_LUXF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_LUXF)
rm(EXP_LUXF)

EXP_LVAF <- expandRows(LVAF, "Value")
IDX_ <- sample(1:nrow(EXP_LVAF), 0.30*nrow(EXP_LVAF))
EXP_LVAF <- EXP_LVAF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_LVAF)
rm(EXP_LVAF)

EXP_MEXF <- expandRows(MEXF, "Value")
IDX_ <- sample(1:nrow(EXP_MEXF), 0.30*nrow(EXP_MEXF))
EXP_MEXF <- EXP_MEXF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_MEXF)
rm(EXP_MEXF)

EXP_NLDF <- expandRows(NLDF, "Value") 
IDX_ <- sample(1:nrow(EXP_NLDF), 0.30*nrow(EXP_NLDF))
EXP_NLDF <- EXP_NLDF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_NLDF)
rm(EXP_NLDF)

EXP_POLF <- expandRows(POLF, "Value")
IDX_ <- sample(1:nrow(EXP_POLF), 0.30*nrow(EXP_POLF))
EXP_POLF <- EXP_POLF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_POLF)
rm(EXP_POLF)

EXP_PRTF <- expandRows(PRTF, "Value") 
IDX_ <- sample(1:nrow(EXP_PRTF), 0.30*nrow(EXP_PRTF))
EXP_PRTF <- EXP_PRTF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_PRTF)
rm(EXP_PRTF)

EXP_RUSF <- expandRows(RUSF, "Value") 
IDX_ <- sample(1:nrow(EXP_RUSF), 0.30*nrow(EXP_RUSF))
EXP_RUSF <- EXP_RUSF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_RUSF)
rm(EXP_RUSF)

EXP_SVKF <- expandRows(SVKF, "Value")
IDX_ <- sample(1:nrow(EXP_SVKF), 0.30*nrow(EXP_SVKF))
EXP_SVKF <- EXP_SVKF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_SVKF)
rm(EXP_SVKF)

EXP_SVNF <- expandRows(SVNF, "Value") 
IDX_ <- sample(1:nrow(EXP_SVNF), 0.30*nrow(EXP_SVNF))
EXP_SVNF <- EXP_SVNF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_SVNF)
rm(EXP_SVNF)

EXP_SWEF <- expandRows(SWEF, "Value") 
IDX_ <- sample(1:nrow(EXP_SWEF), 0.30*nrow(EXP_SWEF))
EXP_SWEF <- EXP_SWEF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_SWEF)
rm(EXP_SWEF)

EXP_TURF <- expandRows(TURF, "Value") 
IDX_ <- sample(1:nrow(EXP_TURF), 0.30*nrow(EXP_TURF))
EXP_TURF <- EXP_TURF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_TURF)
rm(EXP_TURF)

EXP_USAF <- expandRows(USAF, "Value") 
IDX_ <- sample(1:nrow(EXP_USAF), 0.30*nrow(EXP_USAF))
EXP_USAF <- EXP_USAF[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_USAF)
rm(EXP_USAF)

EXP_AUSM <- expandRows(AUSM, "Value") 
IDX_ <- sample(1:nrow(EXP_AUSM), 0.30*nrow(EXP_AUSM))
EXP_AUSM <- EXP_AUSM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_AUSM)
rm(EXP_AUSM)

EXP_AUTM <- expandRows(AUTM, "Value")
IDX_ <- sample(1:nrow(EXP_AUTM), 0.30*nrow(EXP_AUTM))
EXP_AUTM <- EXP_AUTM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_AUTM)
rm(EXP_AUTM)

EXP_BELM <- expandRows(BELM, "Value") 
IDX_ <- sample(1:nrow(EXP_BELM), 0.30*nrow(EXP_BELM))
EXP_BELM <- EXP_BELM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_BELM)
rm(EXP_BELM)

EXP_BRAM <- expandRows(BRAM, "Value") 
IDX_ <- sample(1:nrow(EXP_BRAM), 0.30*nrow(EXP_BRAM))
EXP_BRAM <- EXP_BRAM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_BRAM)
rm(EXP_BRAM)

EXP_CANM <- expandRows(CANM, "Value") 
IDX_ <- sample(1:nrow(EXP_CANM), 0.30*nrow(EXP_CANM))
EXP_CANM <- EXP_CANM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_CANM)
rm(EXP_CANM)

EXP_CHEM <- expandRows(CHEM, "Value")
IDX_ <- sample(1:nrow(EXP_CHEM), 0.30*nrow(EXP_CHEM))
EXP_CHEM <- EXP_CHEM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_CHEM)
rm(EXP_CHEM)

EXP_CHLM <- expandRows(CHLM, "Value") 
IDX_ <- sample(1:nrow(EXP_CHLM), 0.30*nrow(EXP_CHLM))
EXP_CHLM <- EXP_CHLM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_CHLM)
rm(EXP_CHLM)

EXP_COLM <- expandRows(COLM, "Value") 
IDX_ <- sample(1:nrow(EXP_COLM), 0.30*nrow(EXP_COLM))
EXP_COLM <- EXP_COLM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_COLM)
rm(EXP_COLM)

EXP_CRIM <- expandRows(CRIM, "Value") 
IDX_ <- sample(1:nrow(EXP_CRIM), 0.30*nrow(EXP_CRIM))
EXP_CRIM <- EXP_CRIM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_CRIM)
rm(EXP_CRIM)

EXP_CZEM <- expandRows(CZEM, "Value") 
IDX_ <- sample(1:nrow(EXP_CZEM), 0.30*nrow(EXP_CZEM))
EXP_CZEM <- EXP_CZEM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_CZEM)
rm(EXP_CZEM)

EXP_DEUM <- expandRows(DEUM, "Value")
IDX_ <- sample(1:nrow(EXP_DEUM), 0.30*nrow(EXP_DEUM))
EXP_DEUM <- EXP_DEUM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_DEUM)
rm(EXP_DEUM)

EXP_DNKM <- expandRows(DNKM, "Value") 
IDX_ <- sample(1:nrow(EXP_DNKM), 0.30*nrow(EXP_DNKM))
EXP_DNKM <- EXP_DNKM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_DNKM)
rm(EXP_DNKM)

EXP_ESPM <- expandRows(ESPM, "Value") 
IDX_ <- sample(1:nrow(EXP_ESPM), 0.30*nrow(EXP_ESPM))
EXP_ESPM <- EXP_ESPM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_ESPM)
rm(EXP_ESPM)

EXP_ESTM <- expandRows(ESTM, "Value") 
IDX_ <- sample(1:nrow(EXP_ESTM), 0.30*nrow(EXP_ESTM))
EXP_ESTM <- EXP_ESTM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_ESTM)
rm(EXP_ESTM)

EXP_FINM <- expandRows(FINM, "Value") 
IDX_ <- sample(1:nrow(EXP_FINM), 0.30*nrow(EXP_FINM))
EXP_FINM <- EXP_FINM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_FINM)
rm(EXP_FINM)

EXP_FRAM <- expandRows(FRAM, "Value") 
IDX_ <- sample(1:nrow(EXP_FRAM), 0.30*nrow(EXP_FRAM))
EXP_FRAM <- EXP_FRAM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_FRAM)
rm(EXP_FRAM)

EXP_GBRM <- expandRows(GBRM, "Value") 
IDX_ <- sample(1:nrow(EXP_GBRM), 0.30*nrow(EXP_GBRM))
EXP_GBRM <- EXP_GBRM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_GBRM)
rm(EXP_GBRM)

EXP_HUNM <- expandRows(HUNM, "Value") 
IDX_ <- sample(1:nrow(EXP_HUNM), 0.30*nrow(EXP_HUNM))
EXP_HUNM <- EXP_HUNM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_HUNM)
rm(EXP_HUNM)

EXP_IRLM <- expandRows(IRLM, "Value") 
IDX_ <- sample(1:nrow(EXP_IRLM), 0.30*nrow(EXP_IRLM))
EXP_IRLM <- EXP_IRLM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_IRLM)
rm(EXP_IRLM)

EXP_ISLM <- expandRows(ISLM, "Value") 
IDX_ <- sample(1:nrow(EXP_ISLM), 0.30*nrow(EXP_ISLM))
EXP_ISLM <- EXP_ISLM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_ISLM)
rm(EXP_ISLM)

EXP_ISRM <- expandRows(ISRM, "Value") 
IDX_ <- sample(1:nrow(EXP_ISRM), 0.30*nrow(EXP_ISRM))
EXP_ISRM <- EXP_ISRM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_ISRM)
rm(EXP_ISRM)

EXP_ITAM <- expandRows(ITAM, "Value")
IDX_ <- sample(1:nrow(EXP_ITAM), 0.30*nrow(EXP_ITAM))
EXP_ITAM <- EXP_ITAM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_ITAM)
rm(EXP_ITAM)

EXP_KORM <- expandRows(KORM, "Value") 
IDX_ <- sample(1:nrow(EXP_KORM), 0.30*nrow(EXP_KORM))
EXP_KORM <- EXP_KORM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_KORM)
rm(EXP_KORM)

EXP_LTUM <- expandRows(LTUM, "Value") 
IDX_ <- sample(1:nrow(EXP_LTUM), 0.30*nrow(EXP_LTUM))
EXP_LTUM <- EXP_LTUM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_LTUM)
rm(EXP_LTUM)

EXP_LUXM <- expandRows(LUXM, "Value") 
IDX_ <- sample(1:nrow(EXP_LUXM), 0.30*nrow(EXP_LUXM))
EXP_LUXM <- EXP_LUXM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_LUXM)
rm(EXP_LUXM)

EXP_LVAM <- expandRows(LVAM, "Value") 
IDX_ <- sample(1:nrow(EXP_LVAM), 0.30*nrow(EXP_LVAM))
EXP_LVAM <- EXP_LVAM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_LVAM)
rm(EXP_LVAM)

EXP_MEXM <- expandRows(MEXM, "Value") 
IDX_ <- sample(1:nrow(EXP_MEXM), 0.30*nrow(EXP_MEXM))
EXP_MEXM <- EXP_MEXM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_MEXM)
rm(EXP_MEXM)

EXP_NLDM <- expandRows(NLDM, "Value") 
IDX_ <- sample(1:nrow(EXP_NLDM), 0.30*nrow(EXP_NLDM))
EXP_NLDM <- EXP_NLDM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_NLDM)
rm(EXP_NLDM)

EXP_POLM <- expandRows(POLM, "Value") 
IDX_ <- sample(1:nrow(EXP_POLM), 0.30*nrow(EXP_POLM))
EXP_POLM <- EXP_POLM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_POLM)
rm(EXP_POLM)

EXP_PRTM <- expandRows(PRTM, "Value") 
IDX_ <- sample(1:nrow(EXP_PRTM), 0.30*nrow(EXP_PRTM))
EXP_PRTM <- EXP_PRTM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_PRTM)
rm(EXP_PRTM)

EXP_RUSM <- expandRows(RUSM, "Value") 
IDX_ <- sample(1:nrow(EXP_RUSM), 0.30*nrow(EXP_RUSM))
EXP_RUSM <- EXP_RUSM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_RUSM)
rm(EXP_RUSM)

EXP_SVKM <- expandRows(SVKM, "Value") 
IDX_ <- sample(1:nrow(EXP_SVKM), 0.30*nrow(EXP_SVKM))
EXP_SVKM <- EXP_SVKM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_SVKM)
rm(EXP_SVKM)

EXP_SVNM <- expandRows(SVNM, "Value") 
IDX_ <- sample(1:nrow(EXP_SVNM), 0.30*nrow(EXP_SVNM))
EXP_SVNM <- EXP_SVNM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_SVNM)
rm(EXP_SVNM)

EXP_SWEM <- expandRows(SWEM, "Value") 
IDX_ <- sample(1:nrow(EXP_SWEM), 0.30*nrow(EXP_SWEM))
EXP_SWEM <- EXP_SWEM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_SWEM)
rm(EXP_SWEM)

EXP_TURM <- expandRows(TURM, "Value") 
IDX_ <- sample(1:nrow(EXP_TURM), 0.30*nrow(EXP_TURM))
EXP_TURM <- EXP_TURM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_TURM)
rm(EXP_TURM)

EXP_USAM <- expandRows(USAM, "Value") 
IDX_ <- sample(1:nrow(EXP_USAM), 0.30*nrow(EXP_USAM))
EXP_USAM <- EXP_USAM[IDX_, ]
TOTAL <- rbind(TOTAL, EXP_USAM)
rm(EXP_USAM)

# clear up some space
rm(AUSF, AUSM, AUTF, AUTM, BELF, BELM, BRAF, BRAM, CANF, CANM, CHEF, CHEM, CHLF, CHLM, COLF, COLM, CRIF, CRIM, CZEF, CZEM, DEUF, DEUM, DNKF, DNKM, ESPF, ESPM, ESTF, ESTM, FINF, FINM, FRAF, FRAM, GBRF, GBRM, HUNF, HUNM, IRLF, IRLM, ISLF, ISLM, ISRF, ISRM, ITAF, ITAM, KORF, KORM, LTUF, LTUM, LUXF, LUXM, LVAF, LVAM, MEXF, MEXM)
rm(NLDF, NLDM, NORF, NORM, POLF, POLM, PRTF, PRTM, RUSF, RUSM, SVKF, SVKM, SVNF, SVNM, SWEF, SWEM, TURF, TURM, USAF, USAM)

# PRELIMINARY ANALYSIS
# Gender-Country
# Female
cleanF <- subset(cleanFL, cleanFL$SEX == "F")
AggF <- aggregate(cleanF$Value, list(cleanF$COUNTRY), sum)
head(AggF)
print(AggF)
barplot(AggF$x,names=AggF$Group.1)
# Male
cleanM <- subset(cleanFL, cleanFL$SEX == "M")
AggM <- aggregate(cleanM$Value, list(cleanM$COUNTRY), sum)
head(AggM)
print(AggM)
barplot(AggM$x,names=AggM$Group.1)
# Diff (Male-Female)
cDiff <- AggM$x - AggF$x
head(cDiff)
print(cDiff)
print(AggM$Group.1)
barplot(cDiff,names=AggM$Group.1, names.arg = NULL, ylab="Difference in Males/Females",cex.lab = 1.5) # More men is positive

# Gender-Field
Agg <- aggregate(cleanLC$Value, list(cleanLC$FIELD), sum)
head(Agg)
print(Agg)
barplot(Agg$x,names=Agg$Group.1)
# Female
cleanF <- subset(cleanLC, cleanLC$SEX == "F")
AggF <- aggregate(cleanF$Value, list(cleanF$FIELD), sum)
head(AggF)
print(AggF)
barplot(AggF$x,names=AggF$Group.1)
# Male
cleanM <- subset(cleanLC, cleanLC$SEX == "M")
AggM <- aggregate(cleanM$Value, list(cleanM$FIELD), sum)
head(AggM)
print(AggM)
barplot(AggM$x,names=AggM$Group.1)
# Diff (Male-Female)
cDiff <- AggM$x - AggF$x
head(cDiff)
print(cDiff)
print(AggM$Group.1)
barplot(cDiff,names=AggM$Group.1, names.arg = NULL, ylab="Difference in Males/Females", cex.lab = 1.5) # More men is positive

# Gender-Level
Agg <- aggregate(cleanFC$Value, list(cleanFC$ISC11_LEVEL), sum)
summary(Agg)
Agg <- Agg[c(1,3,4,5,2),]
head(Agg)
print(Agg)
barplot(Agg$x,names=Agg$Group.1)
# Female
cleanF <- subset(cleanFC, cleanFC$SEX == "F")
AggF <- aggregate(cleanF$Value, list(cleanF$ISC11_LEVEL), sum)
AggF <- AggF[c(1,3,4,5,2),]
head(AggF)
print(AggF)
barplot(AggF$x,names=AggF$Group.1)
# Male
cleanM <- subset(cleanFC, cleanFC$SEX == "M")
AggM <- aggregate(cleanM$Value, list(cleanM$ISC11_LEVEL), sum)
AggM <- AggM[c(1,3,4,5,2),]
head(AggM)
print(AggM)
barplot(AggM$x,names=AggM$Group.1)
# Diff (Male-Female)
cDiff <- AggM$x - AggF$x
head(cDiff)
print(cDiff)
print(AggM$Group.1)
barplot(cDiff,names=AggM$Group.1, names.arg = NULL, ylab="Difference in Males/Females", cex.lab = 1.5) # More men is positive

# Countries
# all fields and all levels
cleanFLT <- subset(clean, clean$ISC11_LEVEL == "L5T8" & clean$FIELD == "T" & clean$SEX == "T")
cleanFLT <- subset(cleanFLT, !(cleanFLT$COUNTRY == "OEU" | cleanFLT$COUNTRY == "OTO"))
summary(cleanFLT)
ggplot(cleanFLT, aes(x = cleanFLT$YEAR, y = cleanFLT$Value))  + geom_line(aes(color = cleanFLT$COUNTRY))  + geom_point(aes(color = cleanFLT$COUNTRY)) + labs(x = "Year") + labs(color = "Country") + labs(y = "Persons in Program") #+ geom_text(aes(label = cleanFLT$Country))

# Engineering
cleanEng <- subset(clean, clean$ISC11_LEVEL == "L5T8" & clean$FIELD == "F07" & clean$SEX == "T")
cleanEng <- subset(cleanEng, !(cleanEng$COUNTRY == "OEU" | cleanEng$COUNTRY == "OTO"))
summary(cleanEng)
ggplot(cleanEng, aes(x = cleanEng$YEAR, y = cleanEng$Value)) + geom_line(aes(color = cleanEng$COUNTRY)) + geom_point(aes(color = cleanEng$COUNTRY))  + labs(x = "Year") + labs(color = "Country") + labs(y = "Persons in Program") #+ geom_text(aes(label = cleanEng$Country)) 

# Doctoral
cleanDoc <- subset(clean, clean$ISC11_LEVEL == "L8" & clean$FIELD == "T" & clean$SEX == "T")
cleanDoc <- subset(cleanDoc, !(cleanDoc$COUNTRY == "OEU" | cleanDoc$COUNTRY == "OTO"))
summary(cleanDoc)
ggplot(cleanDoc, aes(x = cleanDoc$YEAR, y = cleanDoc$Value)) + geom_line(aes(color = cleanDoc$COUNTRY))  + geom_point(aes(color = cleanDoc$COUNTRY)) + labs(x = "Year") + labs(color = "Country") + labs(y = "Persons in Program") #+ geom_text(aes(label = cleanDoc$Country))

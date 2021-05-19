Accidental_Drug_Related_Deaths_2012.2018 <- read.csv("D:/RUTGERS SPRING 2020/DATA 101/Final Project Analyzing and Discussing Real-World Data/Accidental_Drug_Related_Deaths_2012-2018.csv")


Accidental_Drug_Related_Deaths_2012.2018$Year <- substr(Accidental_Drug_Related_Deaths_2012.2018$Date,7,10)
yearcount <- table(Accidental_Drug_Related_Deaths_2012.2018$Year, exclude = "")
yearcount

plot(yearcount, main='ACCIDENTAL DRUG RELATED DEATHS', xlab = 'Year', ylab='Deaths', col='blue', type='b')


Year <- c(2012, 2013, 2014, 2015, 2016, 2017, 2018)
Deaths <- c(355,  490,  558,  727,  917, 1038, 1018)
yearly.data <- data.frame(Year, Deaths)

yearly.lm = lm(Deaths~Year, data=yearly.data)
abline(yearly.lm, col="red")
summary(yearly.lm)
predicted <- predict(yearly.lm, data.frame(Year=c(2019:2025)))
predicted

yearly.data.with2019 <- yearly.data
yearly.data.with2019[nrow(yearly.data.with2019) + 1,] = c(2019, 1200)


DMwR::regr.eval(predicted[1], yearly.data.with2019[8,]$Deaths)

Prescriptions <- c(NA, 5990233, 6064563, 6249637, 6545550, 6724447, 6908152)
yearly.data$Prescriptions <- Prescriptions
Prescriptionswith2019 <- c(NA, 5990233, 6064563, 6249637, 6545550, 6724447, 6908152, 7089918)
yearly.data.with2019$Predcriptions <- Prescriptionswith2019

prescriptions.lm = lm(Deaths~Prescriptions, data=yearly.data, na.action=na.omit)
summary(prescriptions.lm)
predicted1 <- predict(prescriptions.lm, data.frame(Year=c(2019), Prescriptions=c(7089918)))
predicted1

DMwR::regr.eval(predicted1[1], yearly.data.with2019[8,]$Deaths)

sexcount <- table(Accidental_Drug_Related_Deaths_2012.2018$Sex, exclude = "")
sexcount
barplot(sexcount, ylim = range(1:5000), main = "Males Had the Highest Accidental Drug Related Deaths by Sex", font.main = 4, col.main = "purple", xlab = "Sex", ylab = "Deaths", col = c("lightblue", "mistyrose"))

racecount <- table(Accidental_Drug_Related_Deaths_2012.2018$Race, exclude = "")
racecount
racecount <- sort(racecount, decreasing = TRUE)

lbls <- paste(names(racecount), "\n", racecount, sep="")
pie(racecount[1:3], labels = lbls,
    main="Non-Hispanic Whites Had the Highest of\n Accidental Drug Related Mortality by Race/Ethnicity")

locationcount <- table(Accidental_Drug_Related_Deaths_2012.2018$Location, exclude = "")
locationcount
barplot(locationcount, ylim = range(1:3000), main = "Most Accidental Drug Related Deaths Occured at a Residence", font.main = 4, col.main = "purple", xlab = "Location", ylab = "Deaths", col = c("lightblue", "mistyrose"))



attach(Accidental_Drug_Related_Deaths_2012.2018)
Accidental_Drug_Related_Deaths_2012.2018$agecat[Age >= 15 & Age <= 17] <- "15-17"
Accidental_Drug_Related_Deaths_2012.2018$agecat[Age >= 18 & Age <= 24] <- "18-24"
Accidental_Drug_Related_Deaths_2012.2018$agecat[Age >= 25 & Age <= 34] <- "25-34"
Accidental_Drug_Related_Deaths_2012.2018$agecat[Age >= 35 & Age <= 44] <- "35-44"
Accidental_Drug_Related_Deaths_2012.2018$agecat[Age >= 45 & Age <= 54] <- "45-54"
Accidental_Drug_Related_Deaths_2012.2018$agecat[Age >= 55 & Age <= 64] <- "55-64"
Accidental_Drug_Related_Deaths_2012.2018$agecat[Age >= 65] <- "65+"
detach(Accidental_Drug_Related_Deaths_2012.2018) 

agecount <- table(Accidental_Drug_Related_Deaths_2012.2018$agecat, exclude = "")
agecount

barplot(agecount[1:7], ylim = range(1:1500), main = "Accidental Drug Related Deaths Was Highest in Ages 25-54", font.main = 4, col.main = "purple", xlab = "Age-group", ylab = "Deaths", col = c("lightblue", "mistyrose"))




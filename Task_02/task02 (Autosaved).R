setwd('~/Desktop/Evolution/Tasks/Task_02')
Data <- read.csv ('http://jonsmitchell.com/data/beren.csv', stringsAsFactors=F)
write.csv(Data, 'raedata.csv', quote=F)
length(Data)
nrow(Data)
ncol(Data)
colnames(Data)
head(Data)
Data[1,]
Data[2,]
Data[1:3,]
Data[1:3, 4]
Data[1:5, 1:3]
Feeds <- which(Data[9,]== 'bottle')
berenMilk <- Data[Feeds,]
head(berenMilk)
Feeds <- which(Data[,'event']== 'bottle')
Feeds <- which(Data$event == 'bottle')
dayID <- apply(Data, 1, function(x) paste(x[1:3], collaspe='-'))
dateID <- sapply(dayID, as.Date, format = '%Y-%m-%d', origin = '2019-04-18')
Data$age <- dateID - dateID[which(Data$event == 'birth')]
head(Data)
beren2 <- Data
beren3 <- beren2[order(beren2$age),]
write.csv(beren3, 'beren_new.csv', quote=F, row.names=FALSE)
Feeds <- which(beren3$event == "bottle")
avgMilk <- mean(beren3$value[Feeds])
avgFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
varFeeds <- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeeds <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value[Feeds], beren3$age[Feeds], length)
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds])
summary(berenCor)
berenANOVA <- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
boxplot( beren3$value[Feeds] ~ beren3$caregiver[Feeds], xlab="who gave the bottle", ylab="amount of milk consumed (oz)")
par(las=1, mar=c(5,5,1,1), mgp=c(2. 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
pdf('r02b-totalMilkByDay.pdf', height = 4, width = 4)
par(las=1, mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(names(totalFeeds)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
dev.off()
Naps <- which (Data[,9] == 'nap')
Beren4 <- Data [Naps,]
head (Beren4)
Naps <- which (Data [,'event'] == 'nap')
Naps <- which (Data$event == 'nap')
startHour <- (beren4$start_hour)
startMin <- (Beren4$start_minute)
stopHour <- (Beren4$end_hour)
stopMin <- (Beren4$end_minute)
startHour
startMin
stopHour
stopMin
Beren4$SleepAmount <- ((stopHour - startHour) * 60 + (stopMin - startMin))
head (Beren4)
TotalSleepTime<-tapply(Beren4$SleepAmount, Beren4age, sum)
TotalSleepTime
par(las = 1, mar= c (5,5,1,1), mgp= c (2,0.5,0), tck = -0.01)
plot(as.numeric (names (TotalSleepTime)), TotalSleepTime, type = "b", pch = 16, xlab = "Age in days", ylab = "Time Asleep in minutes")
cor.test (Beren4$start_hour, Beren4$TotalSleepTime)
Beren4Cor <- cor.test (Beren4$start_hour, Beren4$SleepAmount)
pdf ( 'r02b-sleeptime.pdf', height = 4, width = 4)
Naps <- which (Data[,9] == 'nap')
Beren4 <- Data [Naps,]
head (Beren4)
Naps <- which (Data [,'event'] == 'nap')
Naps <- which (Data$event == 'nap')
startHour <- (Beren4$start_hour)
startMin <- (Beren4$start_minute)
stopHour <- (Beren4$end_hour)
stopMin <- (Beren4$end_minute)
startHour
startMin
stopHour
stopMin
Beren4$SleepAmount <- ((stopHour - startHour) *60 = (stopMin - startMin))
head (Beren4)
TotalSleepTime<-tapply(Beren4$SleepAmount, Beren4$age, sum)
TotalSleepTime
par(las = 1, mar=c (5,5,1,1), mgp = c (2,0.5,0), tck = -0.01)
plot(as.numeric (names (TotalSleepTime)), TotalSleepTime, type = "b", pch = 16, xlab = "Age in days". ylab = "Time Asleep in minutes")
cor.test (Beren4$start_hour, Beren4$TotalSleepTime)
Beren4Cor <- cor.test (Beren4$start_hour, Beren4$SleepAmount)
pdf ('r02b-sleeptime.pdf', height = 4, width = 4)
getwd()
#'My hypothesis is that Beren's overall length will increase over time.'
Length <- which (Data[,9] == 'trait_length')
berenLength <- Data[Length ,]
Length <- which(Data[,'event'] == 'trait_length')
length <- which (Data$event == 'trait_length')
Length <- which (berenLength$event == 'trait_length')
avgLength <- mean (berenLength$value [Length])
avgLength <- tapply (berenLength$value [Length], berenLength$age [Length], mean)
totalLength <- tapply (berenLength$value [Length], berenLength$age [Length], sum)
numLength <- tapply (berenLength$value [Length], berenLength$age [Length], length)
cor (berenLength$value [Length], berenLength$age [Length])
cor.test (berenLength$value [Length], berenLength$age [Length], method ="spearm", alternative="greater")
lengthCor <- cor.test (berenLength$value [Length], berenLength$age [Length], method = "spearm", alternative = "greater")
summary (lengthCor)
par (las = 1, mar = c (5,5,1,1), mgp = c (2, 0.5, 0), tck = -0.01)
plot (as.numeric (names (totalLength)), totalLength, type ="b", pch = 16, xlab="Age in Days", ylab="Length in Centimeters")
abline (h=mean (totalLength), lty=2, col='red')
pdf ('r02b-BerenLength, height = 4, width = 4')
dev.off()

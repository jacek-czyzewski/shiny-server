
setwd("C:/Users/dgt/Documents/R Programming Certificate/GDPR data/Spotify")
library(jsonlite)
library(ggplot2)
library(magrittr)
library(timevis)


JsonData <- fromJSON(txt="StreamingHistory.json")
colnames(JsonData)
head(JsonData)
View(JsonData)

#Convert artistName column to include only the name of the band in order to obtain more meaningful statistics
JsonData[,1] <- sub(',.*$','', JsonData[,1])
head(JsonData)
tail(JsonData)

#Show bands with listening count greater than 25
rank <- Filter(function(x) x > 25, with(JsonData, table(artistName)))

ylabels <- names(rank)
par(mar=c(5.1, max(4.1,max(nchar(ylabels))/1.8) ,4.1 ,2.1))
barplot(sort(rank), horiz=TRUE, las=2) 

rank1 <- as.data.frame(table(JsonData[,1]))
rank1 <- rank1[order(rank1$Freq, decreasing=TRUE),]
ggplot(rank1[rank1$Freq > 25,], aes(x=reorder(Var1,Freq), y=Freq)) + geom_bar(stat="identity") + coord_flip() + labs(x="Artist", y="Play count")


#Show how many tracks were listened daily
JsonData1 <- JsonData
JsonData1[,3] <- sub(' .*$','', JsonData[,3])
hist(table(JsonData1[,3]))
rank2 <- Filter(function(x) x > 25, with(JsonData1, table(time)))
plot(rank2)

#plot timelines

data_ex <- data.frame(
  id      = 1:4,
  content = c("Item one", "Item two",
              "Ranged item", "Item four"),
  start   = c("2016-01-10", "2016-01-11",
              "2016-01-20", "2016-02-14 15:00:00"),
  end     = c(NA, NA, "2016-02-04", NA)
)

timevis(data_ex)

data_u <- unique(JsonData1[,3])

data_band <- JsonData1[JsonData1$artistName=="MGMT",]

data_ex2 <- data.frame(
  id      = c(1:dim(data_band)[1]),
  content = c(data_band$trackName),
  start   = c(data_band$time),
  end     = c(rep(NA, times=length(data_band$time)))
  )

timevis(data_ex2)

showModal(modalDialog(
  title = "Important message",
  "This is an important message!"
))
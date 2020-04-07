#Geo validation approaches:
data <- read.csv("./data/tweets.randomized.csv")

#Approach 1
#Remove all tweets with consequence score value V and not located in municipalities Kalamata, Messene, Laconi
data.remove <- subset(data, data$svm.value.tweets.consequences == 5 &(
                        data$municipality.x != "Καλαμάτας" | data$municipality.x != "Μεσσήνης" | data$municipality.x != "Σπάρτης"))

data.correct <- subset(data, !(data$id %in% data.remove$id))

#Approach 2
#Remove of consequence score values IV and V in municipalities with Rain identification < 10 or flood identification < 5
table.flood <- as.data.frame(summary(data$municipality.x[data$svm.value.flood.ident == "Y"]))
table.flood <- subset(table.flood, table.flood$`summary(data$municipality.x[data$svm.value.flood.ident == "Y"])` < 5)

table.rain <- as.data.frame(summary(data$municipality.x[data$svm.value.rain.ident == "Y"]))
table.rain <- subset(table.rain, table.rain$`summary(data$municipality.x[data$svm.value.rain.ident == "Y"])` < 10)

data.remove <- subset(data, (data$svm.value.tweets.consequences == 4 |  data$svm.value.tweets.consequences == 5) &(
  data$municipality.x %in% row.names(table.flood) | data$municipality.x %in% row.names(table.rain)))

data.correct <- subset(data, !(data$id %in% data.remove$id))

#Approach 3
#Remove all georeferenced tweets at precision level greater than III
data.correct <- subset(data, as.numeric(data$precision) < 4)

#Approach 4
#Remove all consequence score values in tweets that include the words Kalamata or Messini or Sparta or Laconia and are not located
#within those municipalities
data.remove <- subset(data, data$svm.value.tweets.consequences != "N" & (grepl("καλαμάτα", data$Text) | grepl("μεσσήνη", data$Text) | grepl("σπάρτη", data$Text))
                      & (data$municipality.x != "Καλαμάτας" | data$municipality.x != "Σπάρτης" | data$municipality.x != "Μεσσήνης"))
data.correct <- subset(data, !(data$id %in% data.remove$id))

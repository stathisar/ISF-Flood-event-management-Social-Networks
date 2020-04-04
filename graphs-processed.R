require("tm")
require("RWeka")
require("stringr")
require("tidyr")
require("qdapRegex")
require("ggplot2")
require("lubridate")
setwd("~/Desktop/data")

tweets.processed <- read.csv("./tweets.randomized.final.mar.31.csv")
tweets.processed$unix.time <- lubridate::parse_date_time(tweets.processed$X.M..object_posted_time.., "%m-%d%y %H:%M:%S %p")
tweets.processed$unix.time <- as.numeric(tweets.processed$unix.time)

#selection of tweets with timestamp since 7th of September 2016
tweets.processed.time.intervals <- subset(tweets.processed, tweets.processed$unix.time > 1473207914)

#time interval per hour indicator
tweets.processed.time.intervals$interval <- NA
tweets.processed.time.intervals$interval <- as.numeric(tweets.processed.time.intervals$interval)
b <- min(tweets.processed.time.intervals$unix.time)
#c <- round((max(tweets.processed.time.intervals$unix.time) - min(tweets.processed.time.intervals$unix.time))/3600) + 1

for (i in 1:120){
  tweets.processed.time.intervals[ tweets.processed.time.intervals$unix.time < b + 3600 
                                   & is.na(tweets.processed.time.intervals$interval), 
                                   NCOL(tweets.processed.time.intervals)]  <- as.numeric(i)
  b <- b + 3600
}

#day/night condition indicator
days <- data.frame(matrix(nrow = 10, ncol = 3))
names(days) <- c("unix.time", "id", "condition")
days[1, 1] <- as.numeric(1473231600 - 10800)
days[2, 1] <- as.numeric(1473318000 - 10800)
days[3, 1] <- as.numeric(1473404400 - 10800)
days[4, 1] <- as.numeric(1473490800 - 10800)
days[5, 1] <- as.numeric(1473577200 - 10800)
days[1:5 , 2] <- c(1, 3 , 5 , 7 , 9)
days[1:5 ,3] <- "day"
days[6, 1] <- as.numeric(1473277440 - 10800)
days[7, 1] <- as.numeric(1473363840 - 10800)
days[8, 1] <- as.numeric(1473450240 - 10800)
days[9, 1] <- as.numeric(1473536640 - 10800)
days[10, 1] <- as.numeric(1473623040 - 10800)
days[6:10 , 2] <- c(2, 4, 6, 8, 10)
days[6:10 ,3] <- "night"
days <- days[order(days[, 2]), ]
tweets.processed.time.intervals$condition <- NA

#days$unix.time[1]
for (i in 1:NROW(days)){
  tweets.processed.time.intervals$condition[tweets.processed.time.intervals$unix.time   > days$unix.time[i]
                                            & tweets.processed.time.intervals$unix.time < days$unix.time[i+1]] <- days$condition[i]
}
tweets.processed.time.intervals$condition[is.na(tweets.processed.time.intervals$condition)] <- "night"


#120h
write.csv(subset(tweets.processed.time.intervals, 
                 tweets.processed.time.intervals$interval < 121), "./processed.120h.csv")




#dataframe for scatterplots
  scatter.data <- as.data.frame(matrix(nrow = 240, ncol = 12))
names(scatter.data) <- c("id", "rain", "flood", "cons", "irony", "volunt",
                         "emotions", "disman", "weather", "sitov", 
                         "interval", "condition")
tweets.processed <- data.frame(tweets.processed.time.intervals$svm.value.rain.ident, tweets.processed.time.intervals$svm.value.flood.ident,
                               tweets.processed.time.intervals$svm.value.tweets.consequences, tweets.processed.time.intervals$svm.value.tweets.irony,
                               tweets.processed.time.intervals$svm.value.tweets.volunteers, tweets.processed.time.intervals$svm.value.tweets.emotions,
                               tweets.processed.time.intervals$svm.value.disaster.management, tweets.processed.time.intervals$svm.value.tweets.weather.warning,
                               tweets.processed.time.intervals$svm.value.tweets.situation.overview, tweets.processed.time.intervals$interval, 
                               tweets.processed.time.intervals$condition)

scatter.data$id <- 1:240
scatter.data$interval <- 1:240
for (j in 2: 10){
  for (i in seq(from = 1, to = 240, by = 2)){
 #   for (z in 1:2){
    k <- round(i / 2 + 0.1)
    scatter.data[i,j] <- as.numeric(NROW(subset(tweets.processed, 
                                                tweets.processed$tweets.processed.time.intervals.interval == as.numeric(k)
                                                & tweets.processed[ , j] != "N"
                                                & tweets.processed$tweets.processed.time.intervals.condition == unique(tweets.processed$tweets.processed.time.intervals.condition)[1])))
    scatter.data$interval[i] <- as.numeric(k)
    scatter.data$condition[i] <- as.character(unique(tweets.processed$tweets.processed.time.intervals.condition)[1])
    scatter.data[i+1,j] <- as.numeric(NROW(subset(tweets.processed, 
                                                tweets.processed$tweets.processed.time.intervals.interval == as.numeric(k)
                                                & tweets.processed[ , j] != "N"
                                                & tweets.processed$tweets.processed.time.intervals.condition == unique(tweets.processed$tweets.processed.time.intervals.condition)[2])))
    scatter.data$interval[i+1] <- as.numeric(k)
    scatter.data$condition[i+1] <- as.character(unique(tweets.processed$tweets.processed.time.intervals.condition)[2])

    
    
    #scatter.data$condition[i] <- as.character(unique(tweets.processed$tweets.processed.time.intervals.condition)[z])
  #  }
    }
}

write.csv(scatter.data, "~/Desktop/data/scatter.data.csv")
#rain
scatter.data.rain <- subset(scatter.data, scatter.data$rain > 0)
r = ggplot(scatter.data.rain, aes(scatter.data.rain$interval, scatter.data.rain$rain, 
                             colour = scatter.data.rain$condition
)) + geom_point()
r = r + labs(title = NULL, y = "Rain identification", x = "Hours") 
r = r + labs(colour = "Emotions")
r

#flood
scatter.data.flood <- subset(scatter.data, scatter.data$flood > 0)

f = ggplot(scatter.data.flood, aes(scatter.data.flood$interval, scatter.data.flood$flood, 
                             colour = scatter.data.flood$condition
)) + geom_point()
f = f + labs(title = NULL, y = "Flood identification", x = "Hours") 
f = f + labs(colour = "Emotions")
f

#consequences
scatter.data.cons <- subset(scatter.data, scatter.data$cons > 0)
c = ggplot(scatter.data.cons, aes(scatter.data.cons$interval, scatter.data.cons$cons, 
                                  colour = scatter.data.cons$condition
)) + geom_point()
c = c + labs(title = NULL, y = "Extracted Consequence scores", x = "Hours") 
c = c + labs(colour = "Day / Night Condition")
c


#irony
scatter.data.irony <- subset(scatter.data, scatter.data$irony > 0)

i = ggplot(scatter.data.irony, aes(scatter.data.irony$interval, scatter.data.irony$irony, 
                                   colour = scatter.data.irony$condition
)) + geom_point()
i = i + labs(title = NULL, y = "Irony", x = "Hours") 
i = i + labs(colour = "Day / Night Condition")
i

#emotions
scatter.data.emotions <- subset(scatter.data, scatter.data$emotions > 0)

e = ggplot(scatter.data.emotions, aes(scatter.data.emotions$interval, scatter.data.emotions$emotions, 
                             colour = scatter.data.emotions$condition
)) + geom_point()
e = e + labs(title = NULL, y = "Emotions", x = "Hours") 
e = e + labs(colour = "Day / Night Condition")
e

#volunteers
v = ggplot(scatter.data, aes(scatter.data$interval, scatter.data$volunt, 
                             colour = scatter.data$condition
)) + geom_point()
v = v + labs(title = NULL, y = "Volunteers", x = "Hours") 
v = v + labs(colour = "Volunteers")
v

#disaster management
scatter.data.dm <- subset(scatter.data, scatter.data$disman > 0)

d = ggplot(scatter.data.dm, aes(scatter.data.dm$interval, scatter.data.dm$disman, 
                             colour = scatter.data.dm$condition
)) + geom_point()
d = d + labs(title = NULL, y = "Disaster Management", x = "Hours") 
d = d + labs(colour = "")
d

#
scatter.data.sit <- subset(scatter.data, scatter.data$sitov > 0)
s = ggplot(scatter.data.sit, aes(scatter.data.sit$interval, scatter.data.sit$sitov, 
                             colour = scatter.data.sit$condition
)) + geom_point()
s = s + labs(title = NULL, y = "Situation Overview", x = "Hours") 
s = s + labs(colour = "Emotions")
s

require(ggpubr)
allplot.1 = ggarrange(r, f, c, i, labels = c("A", "B", "C", "D"), 
                    nrow = 2, ncol = 2, common.legend = TRUE)
allplot.1

allplot.2 = ggarrange(e, v, d, s, labels = c("E", "F", "G", "H"), 
                    nrow = 3, ncol = 2, common.legend = TRUE)

allplot.2
#counting words per hrs
#barplot(scatter.data$rain, breaks == scatter``)
hist(scatter.data.rain$rain, xlim = range(1:120))
hist(scatter.data.cons$cons, xlim = range(1:120))
unique

?hist
?boxplot
#word frequency




avisos.8.hours <- scan("~/Desktop/data/results/text.corpus.120.hours.txt", what="character")
#avisos.8.hours <- rm_url(avisos.8.hours, pattern=pastex("@rm_twitter_url", "@rm_url"), extract=TRUE, replacement = "")
avisos.8.hours <- str_remove_all(avisos.8.hours, '[[:punct:]]')
avisos.8.hours <- gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "", avisos.8.hours)
#check <- rm_twitter_url(tweets.processed.time.intervals$Text)
#check <- gsub("@\\w+ *", "", check)
#check <- gsub("rt", "", check)
#check <- str_remove_all(check, '[[:punct:]]')
#check <- gsub("  ", " ", check)
#write.csv(check, "~/Desktop/corpus.txt", row.names = FALSE)
#?write.csv
#View(avisos)


avisos1 <- tolower(avisos.8.hours)
avisos2 <- strsplit(avisos1, "\\W")
avisos3 <- unlist(avisos2)
freq<-table(avisos3)
View(freq)
write.csv(freq, "~/Desktop/data/results/120hoursfreq.csv")


freq1<-sort(freq, decreasing=TRUE)
temple.sorted.table<-paste(names(freq1), freq1, sep="\\t")
View(temple.sorted.table)
cat("Word\tFREQ", temple.sorted.table, file="~/Desktop/corpus2.txt", sep="\n")
View(freq1)
#install.packages("qdapRegex")

write.csv(check, "~/Desktop/corpus.txt")
View(check)

View(check)
View(freq)
FreqMat <- data.frame(ST = rownames(myTdm), 
                      Freq = rowSums(myTdm), 
                      row.names = NULL)

avisos.4 <- mutate(text = str_remove_all(avisos, '[[:punct:]]'))
avisos <- str_remove_all(avisos,'[[:punct:]]' )

?FreqMat
data.frame(text = avisos) %>% 
  mutate(text = tolower(text)) %>% 
  mutate(text = str_remove_all(text, '[[:punct:]]')) %>% 
  mutate(tokens = str_split(text, "\\s+")) %>%
  unnest() %>% 
  count(tokens) %>% 
  filter(!tokens %in% stop_words) %>% 
  mutate(freq = n / sum(n)) %>% 
  arrange(desc(n))

mutate()
View(count(a1, vars = "lst"))


my.corpus <- Corpus(VectorSource( tweets.processed.time.intervals$Text))

my.corpus <- TermDocumentMatrix(my.corpus)
FreqMat <- data.frame(ST = rownames(my.corpus),
                      Freq = rowSums(my.corpus),
                      row.names = NULL)

?data
data(tweets.processed)
myTdm <- as.matrix(TermDocumentMatrix(crude))
FreqMat <- data.frame(ST = rownames(myTdm), 
                      Freq = rowSums(myTdm), 
                      row.names = NULL)
head(FreqMat, 10)

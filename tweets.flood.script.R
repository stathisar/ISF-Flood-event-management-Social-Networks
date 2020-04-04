    #require("devtools")
    #install_github("r-spatial/sf")
    #install.packages("rgdal")          
    rm(list = ls())
              #required libraries
                require("RTextTools")
                require("splitstackshape")
                require("lubridate")
                require("spdep")
                require("splancs")
                require("rgdal")
                require("plyr")
                require("rgeos")
                require("rgdal")
              require("geoR")
              setwd("./")
              source("./scripts/create_matrix2.R")
              start.time <- Sys.time()
              
              
              #load of data
              tweets <- read.csv("./data/tweets.dataset.csv")
  
                          #select tweets written in greek and english
              tweets <- subset(tweets, 
                               tweets$X.M..language.. == "el" | tweets$X.M..language.. == "en" | tweets$X.M..language.. == "es")
              
              rownames(tweets) <- 1:NROW(tweets)
        
              tweets$detected <- NA
              tweets$value <- NA
              tweets$Text <- as.character(tolower(tweets$Text))
              
              #creation of dataframe in which classified values will be stored
              tweets.classified <- data.frame(matrix(nrow = 0, 
                                                     ncol = NCOL(tweets)))
              names(tweets.classified) <- names(tweets)
              
             
              #creation of prediction.data dataframe that will be used for SVM
              prediction.data <- tweets
              #import of training dataset for svm classification
              training.dataset <- read.csv("./data/training.datasets/training.dataset.csv")
              
              #creation of dtMatrix based on Text of training dataset
              training.matrix <- create_matrix(training.dataset["Text"])
              #creation of containers
                    #container flood identification
             container.flood.ident <- create_container(training.matrix, 
                                                        training.dataset$flood.identification, 
                                                        trainSize = 1:NROW(training.dataset), 
                                                        virgin = FALSE)
             
             #container rain identification
             container.rain.ident <- create_container(training.matrix, 
                                                       training.dataset$rain.identification, 
                                                       trainSize = 1:NROW(training.dataset), 
                                                       virgin = FALSE)
             
             #container disaster management
             container.disaster.management <- create_container(training.matrix, 
                                                      training.dataset$disaster.management, 
                                                      trainSize = 1:NROW(training.dataset), 
                                                      virgin = FALSE)
             
             #container consequences
             training.dataset.consequences <- read.csv("./training.datasets/consequences.td.csv")
             training.matrix.consequences <- create_matrix(training.dataset.consequences["Text"])
                container.consequences <- create_container(training.matrix.consequences,
                                                        training.dataset.consequences$consequence.score,
                                                        trainSize = 1:NROW(training.dataset.consequences), 
                                                        virgin = FALSE)
             
             #container irony
             container.irony <- create_container(training.matrix, 
                                                        training.dataset$irony.politics, 
                                                        trainSize = 1:NROW(training.dataset), 
                                                        virgin = FALSE)
             
             #container emotions
             container.emotions <- create_container(training.matrix,
                                                    training.dataset$emotional..positive.or.negative.,
                                                    trainSize = 1:NROW(training.dataset),
                                                    virgin = FALSE)
             #container effects on social life
             container.effects.on.social.life <- create_container(training.matrix,
                                                                  training.dataset$effects.on.social.life,
                                                                  trainSize = 1:NROW(training.dataset),
                                                                  virgin = FALSE)
             
             #container weather warning
             container.weather.warning <- create_container(training.matrix,
                                                           training.dataset$weather.warning,
                                                           trainSize = 1:NROW(training.dataset),
                                                           virgin = FALSE)
             #container situation overview
             container.situation.overview <- create_container(training.matrix,
                                                              training.dataset$situation.overview,
                                                              trainSize = 1:NROW(training.dataset),
                                                              virgin = FALSE)
              
            #container volunteers
             container.volunteers <- create_container(training.matrix,
                                                              training.dataset$volunteers,
                                                              trainSize = 1:NROW(training.dataset),
                                                              virgin = FALSE)
    #creation of SVM Models
             #SVM flood identification
              model.flood.ident <- train_model(container.flood.ident, "SVM",
                                               kernel="linear",
                                               cost=1)
              
             # model.flood.ident.glmnet <- train_model(container.flood.ident, "GLMNET", cost = 1)
             #SVM rain identification
              model.rain.identification <- train_model(container.rain.ident, "SVM",
                                               kernel="linear",
                                               cost=1)
              #SVM disaster management
              model.disaster.management <- train_model(container.disaster.management, "SVM",
                                                       kernel="linear",
                                                       cost=1)
              
              #SVM consequences
              model.consequences <- train_model(container.consequences, "SVM",
                                                       kernel="linear",
                                                       cost=1)
              
              #SVM irony
              model.irony <- train_model(container.irony, "SVM",
                                                kernel="linear",
                                                cost=1)
              
              #SVM emotions
              model.emotions <- train_model(container.emotions, "SVM",
                                                kernel="linear",
                                                cost=1)
              
              #SVM effects on social life
              model.effects.social.life <- train_model(container.effects.on.social.life, "SVM",
                                                kernel="linear",
                                                cost=1)
              
              #SVM weather warning
              model.weather.warning <- train_model(container.weather.warning, "SVM",
                                                kernel="linear",
                                                cost=1)
              
              #SVM situation overview
              model.situation.overview <- train_model(container.situation.overview, "SVM",
                                                kernel="linear",
                                                cost=1)
              #SVM volunteers
              model.volunteers <- train_model(container.volunteers, "SVM",
                                                      kernel="linear",
                                                      cost=1)
              
              
              
    #creation of prediction matrixes
              #probably one prediction matrix for all apart from consequences
              prediction.matrix <- create_matrix(prediction.data, 
                                                       originalMatrix = training.matrix)
              
              prediction.matrix.consequences <- create_matrix(prediction.data, 
                                                 originalMatrix = training.matrix.consequences)
              
            
    #creation of prediction containers
              #probably one predition container for all apart from consequences
              prediction.container <- create_container(prediction.matrix, 
                                                                   labels = rep(0, NROW(prediction.data)), 
                                                                   testSize = 1:NROW(prediction.data), 
                                                                   virgin = FALSE)
              
              prediction.container.consequences <- create_container(prediction.matrix.consequences, 
                                                       labels = rep(0, NROW(prediction.data)), 
                                                       testSize = 1:NROW(prediction.data), 
                                                       virgin = FALSE)
              
    #prediction of results
              
              #results flood identification+
              tweets.flood.identification.svm <- classify_model(prediction.container, 
                                                 model.flood.ident)
              #results rain identification
              tweets.rain.identification.svm <- classify_model(prediction.container,
                                                               model.rain.identification)
              #results disaster management
              tweets.disaster.management.svm <- classify_model(prediction.container,
                                                               model.disaster.management)
              #results consequences
              tweets.consequences.svm <- classify_model(prediction.container.consequences,
                                                               model.consequences)
              #results irony
              tweets.irony.svm <- classify_model(prediction.container, 
                                                 model.irony)
              #results emotions
              tweets.emotions.svm <- classify_model(prediction.container,
                                                               model.emotions)
              #results effects on social life
              tweets.effects.social.life.svm <- classify_model(prediction.container,
                                                               model.effects.social.life)
              #results weather warning
              tweets.weather.warning.svm <- classify_model(prediction.container,
                                                               model.weather.warning)
              #results situation overview
              tweets.situation.overview.svm <- classify_model(prediction.container,
                                                               model.situation.overview)
              
              #results volunteers
              tweets.volunteers.svm <- classify_model(prediction.container,
                                                      model.volunteers)
              #creation of a logical structure for the prediction output
              tweets.flood.identification.svm$id <- row.names(tweets.flood.identification.svm)
              names(tweets.flood.identification.svm) <- c("svm.value.flood.ident", "svm.prob.flood.ident", "id")
              
              tweets.rain.identification.svm$id <- row.names(tweets.rain.identification.svm)
              names(tweets.rain.identification.svm) <- c("svm.value.rain.ident", "svm.prob.rain.ident", "id")
              
              tweets.disaster.management.svm$id <- row.names(tweets.disaster.management.svm)
              names(tweets.disaster.management.svm) <- c("svm.value.disaster.management", "svm.prob.disaster.management", "id")
              
              tweets.consequences.svm$id <- row.names(tweets.consequences.svm)
              names(tweets.consequences.svm) <- c("svm.value.tweets.consequences", "svm.prob.tweets.consequences", "id")
              
              tweets.irony.svm$id <- row.names(tweets.irony.svm)
              names(tweets.irony.svm) <- c("svm.value.tweets.irony", "svm.prob.tweets.irony", "id")
              
              tweets.emotions.svm$id <- row.names(tweets.emotions.svm)
              names(tweets.emotions.svm) <- c("svm.value.tweets.emotions", "svm.prob.tweets.emotions", "id")
              
              tweets.effects.social.life.svm$id <- row.names(tweets.effects.social.life.svm)
              names(tweets.effects.social.life.svm) <- c("svm.value.tweets.effects.social.l", 
                                                         "svm.prob.tweets.effects.social.l", "id")
              
              tweets.weather.warning.svm$id <- row.names(tweets.weather.warning.svm)
              names(tweets.weather.warning.svm) <- c("svm.value.tweets.weather.warning", 
                                                         "svm.prob.tweets.weather.warning", "id")
              
              tweets.situation.overview.svm$id <- row.names(tweets.situation.overview.svm)
              names(tweets.situation.overview.svm) <- c("svm.value.tweets.situation.overview", 
                                                     "svm.prob.tweets.situation.overview", "id")
              
              tweets.volunteers.svm$id <- row.names(tweets.volunteers.svm)
              names(tweets.volunteers.svm) <- c("svm.value.tweets.volunteers", 
                                                        "svm.prob.tweets.volunteers", "id")
              
              prediction.data$id <- row.names(prediction.data)
              
              #tweets total filtered
              data.list <- list(tweets.disaster.management.svm, tweets.consequences.svm, tweets.effects.social.life.svm,
                                tweets.emotions.svm, tweets.flood.identification.svm, tweets.irony.svm,
                                tweets.rain.identification.svm, tweets.situation.overview.svm, tweets.weather.warning.svm,
                                tweets.volunteers.svm,
                                prediction.data)
              my.merge <- function(df1, df2){
                merge(df1, df2, by = "id")
              }
              
              output <- Reduce(my.merge, data.list)
              
              #tweets.total.filtered <- merge( tweets.flood.identification.svm, tweets.rain.identification.svm,
              #                                tweets.disaster.management.svm, tweets.consequences.svm, 
              #                                tweets.irony.svm, tweets.emotions.svm, tweets.effects.social.life.svm,
              #                                tweets.weather.warning.svm, tweets.situation.overview.svm, by = "id")
    
                      
              #export data
              write.csv(output, "./classified.csv")  
          #import data
         #output <- read.csv("./classified.march24.csv")  
          
          
          #georeferencing of classified tweets
        
          #import of geolocations
              geolocations <- read.csv("./data/geolocations.csv")
              geolocations$name <- as.character(tolower(geolocations$name))
              output$reference <- 0
              output$x <- 0
              output$y <- 0
              output$precision <- 0
              output$comment <- NA
              
              #creation of dataframe in which the georeferencing output will be stored
              tweets.georeferenced <- data.frame(matrix(nrow = 0, 
                                                        ncol = NCOL(output)))
          #    require("rgdal")
              #loop that adds lat lon information and replicates tweets according to geolocation word detection
              for (i in 1:NROW(geolocations)){
                tweets.test <- output[ grepl(geolocations$name[i], 
                                                             output$Text), ]
                if(NROW(tweets.test) == 0){
                }else{
                  tweets.test$x <- geolocations$Long[i]
                  tweets.test$y <- geolocations$Lat[i]
                  tweets.test$detected <- geolocations$name[i]
                  tweets.test$precision <- geolocations$Precision[i]
                  tweets.test$comment <- geolocations$duplicates[i]
                  tweets.georeferenced <- rbind(tweets.georeferenced, tweets.test)
                }
              }
              
              
              #export geo-referenced prerandomized dataset
              write.csv(tweets.georeferenced, 
                        "./georeferenced.prerandomized.csv")
              #start of randomization part
              final.results.prerandomized <- tweets.georeferenced
              #removal of non needed dataframes
              
              rm(tweets.test, tweets.georeferenced)
            
            #keeping only data with coordinates
            final.results.prerandomized <- subset(final.results.prerandomized, y > 0)
    

            #import of area.map shapefile
            area.map <- readOGR(dsn = "./shape.files/kallikratis.shp", 
                                layer = "kallikratis")
            #creation of unique id to area.map
            area.map@data$id <- as.numeric(row.names(area.map@data)) + 1
            
       
            
            
            #creation of spatial points dataframe
            spdf.prerandomize <- SpatialPointsDataFrame(coords = final.results.prerandomized[ ,c("x", "y")], 
                                                       data = final.results.prerandomized, 
                                                       proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
            
            
            #spatial join of municipality name on spdfprerandomize
            spdf.prerandomize@data$NAME <- NA
            spdf.prerandomize@data$id <- NA
            spdf.prerandomize@data[, c("NAME", "id")] = over(spdf.prerandomize, 
                                                                                   area.map[, c("NAME", "id")])
            
            point.count <- summary(spdf.prerandomize@data$NAME)
            point.count <- as.data.frame(point.count)
            point.count$NAME <- row.names(point.count)
            
            map.count <- merge(area.map, point.count, by.area.map = NAME, 
                               by.point.count = NAME, 
                               all.area.map = TRUE)
            #aligning ids
            map.count@data$id <- as.numeric(map.count@data$id)
            map.count@data$id <- as.numeric(map.count@data$id + 1)
            rownames(map.count@data) <- map.count@data$id
            map.count@data$point.count[is.na(map.count@data$point.count)] <- 0 
            #create randompoints and spdf in which randompoints will be stored
            
            generated.random.points <- SpatialPoints(data.frame(x = 0, y = 0))[-1,]
            map.count@data$point.count  <- as.numeric(map.count@data$point.count)
            
            for (i in 1:nrow(map.count)) {
              if (map.count@data$point.count[i] == 0){
                i = i+1}
              else {
                generated.random.points <- append(generated.random.points, 
                                                  spsample(map.count[i, ], 
                                                           n=map.count@data$point.count[i], 
                                                           "random"))
                i <- i+1
              }
            }
            
            #put all random coords in a dataframe
            
            random.points <- data.frame()
            random.points$x <- as.numeric()
            random.points$y <- as.numeric()
            
            #add Counter by column
            #table$Counter <- with(table, ave(seq_along(NAME), NAME, FUN = seq_along))
            
            for (i in 1:length(generated.random.points)){
              random.points <- rbind(random.points, generated.random.points[[i]]@coords)
              i <- i+1
            }
            write.csv(random.points, file = "./randompointswithoutvalues.csv")
            
            #remove data that are not used any more
            rm(generated.random.points, point.count, spdf.prerandomize)
            #creating a uniqueid of finalresultsprerandomize
            final.results.prerandomized$munname <- NA
            final.results.prerandomized$counter <- 0
            final.results.prerandomized$uniqueid <- NA
            spdf.prerandomized <- SpatialPointsDataFrame(coords = final.results.prerandomized[ , c("x", "y")], 
                                                         data = final.results.prerandomized, 
                                                         proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
            
            final.results.prerandomized$munname <- over(spdf.prerandomized, area.map[, "NAME"])
            final.results.prerandomized$municipality <- paste(as.character(unlist(final.results.prerandomized$munname)))
            final.results.prerandomized$counter <-  with(final.results.prerandomized, 
                                                         ave(seq_along(municipality), 
                                                             municipality, 
                                                             FUN=seq_along))
            final.results.prerandomized$uniqueid <- paste(final.results.prerandomized$municipality, 
                                                          final.results.prerandomized$counter, 
                                                          sep = "")
            rm(spdf.prerandomized)
            
            #creating unique id of randompoints
            random.points$munname <- NA
            random.points$counter <- 0
            random.points$uniqueid <- NA
            random.points$distance.from.epic <- NA
            spdf.random.points <- SpatialPointsDataFrame(coords = random.points[ ,c("x", "y")], 
                                                         data = random.points, 
                                                         proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
            random.points$munname <- over(spdf.random.points, area.map[ , "NAME"])
      #      random.points$distance.from.epic <- over(spdf.random.points, area.map[ , "distance.from.epic"])
            random.points$municipality <- paste(as.character(unlist(random.points$munname)))
            random.points$counter <- with(random.points, 
                                          ave(seq_along(municipality), 
                                              municipality, 
                                              FUN = seq_along))
            random.points$uniqueid <- paste(random.points$municipality, random.points$counter, sep = "")
            rm(spdf.random.points)
            #merge finalresultsprerandomized and randompoints
            final.results.prerandomized.2 <- final.results.prerandomized
            final.results.prerandomized <- merge(final.results.prerandomized, 
                                                 random.points, 
                                                 by = "uniqueid", 
                                                 all.final.results.prerandomized = TRUE)
            
            
            #unlist results
            for (i in 1:length(final.results.prerandomized)){
              final.results.prerandomized[ , i] <- paste(unlist(final.results.prerandomized[ ,i]))
              i <- i + 1
            }
            write.csv(final.results.prerandomized, file = "./tweets.randomized.final.csv")
          
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

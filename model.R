rm(list=ls())

# Create dataframe for training set
train_df <- read.csv("train.csv", stringsAsFactors=F, header=T, sep=',')
test_df <- read.csv("test.csv", stringsAsFactors=F, header=T, sep=',')

################# Data Exploration ###############################
str(train_df)
summary(train_df)

# trip_duration    
#  Min.   :      1  
#  1st Qu.:    397  
#  Median :    662  
#  Mean   :    962  
#  3rd Qu.:   1075  
#  Max.   :3526282 

# Not likely that there is min of 1 second, so defintely have some outliers

avgTrip <- mean(train_df$trip_duration)
sdTrip <- sd(train_df$trip_duration)

library(dplyr)

train_df %>% filter(trip_duration >= (avgTrip - 2*sdTrip)) %>%
             filter(trip_duration <= (avgTrip + 2*sdTrip)) -> train_df

summary(train_df)
# haversine distance 

# city_long_border = (-74.03, -73.75)
# city_lat_border = (40.63, 40.85)
## Clean up coordinates -- only city limits
train_df %>% filter(pickup_longitude <= -73.75) %>%
             filter(pickup_longitude >= -74.03) %>%
             filter(pickup_latitude <= 40.85) %>%
             filter(pickup_latitude >= 40.63) %>%
             filter(dropoff_longitude <= -73.75) %>% 
             filter(dropoff_longitude >= -74.03) %>%
             filter(dropoff_latitude <= 40.85) %>%
             filter(dropoff_latitude >= 40.63) -> train_df

library(lubridate)
## Change format of date variables
train_df %>% mutate(pickup_datetime, pickup_datetime = ymd_hms(pickup_datetime)) %>%
             mutate(dropoff_datetime, dropoff_datetime= ymd_hms(dropoff_datetime)) -> train_df

train_df %>% mutate(pickup_date = date(pickup_datetime)) %>%
  mutate(dropoff_date= date(dropoff_datetime)) -> train_df

test_df %>% mutate(pickup_datetime, pickup_datetime = ymd_hms(pickup_datetime)) -> test_df
test_df %>% mutate(pickup_date = date(pickup_datetime)) -> test_df

# not normal -- bad
hist(train_df$trip_duration, breaks = 100)

train_df %>% mutate(trip_duration, logTripDuration = log(trip_duration +1)) -> train_df
hist(train_df$logTripDuration, breaks = 100)



## Plot Time Series of Data 
train_df %>% count(date = pickup_date) -> train_date_counts
test_df %>% count(date = pickup_date) -> test_date_counts

plot(train_date_counts, ylim=c(0,max(train_date_counts$n) ))
lines(train_date_counts,type = 'l', lty = 2, col='blue') 
par(new=TRUE)
plot(test_date_counts, axes = F, ylim=c(0,max(train_date_counts$n)))
lines(test_date_counts,type = 'l', lty = 1, col='darkgreen') 

train_df %>% group_by(vendor_id)%>%
          summarise(MeanVendor = mean(trip_duration), .groups = 'keep')

train_df %>% group_by(store_and_fwd_flag)%>%
  summarise(MeanVendor = mean(trip_duration), .groups = 'keep')


train_df %>% group_by(passenger_count)%>%
  summarise(MeanVendor = mean(trip_duration), .groups = 'keep')

train_df %>% count(passenger_count)
test_df %>% count(passenger_count)

test_df <- filter(test_df,test_df$passenger_count < 9)

## LOCATIONS

# visulaize coordinates using scatter plot 


city_long_border <- c(-74.03, -73.75)
city_lat_border <- c(40.63, 40.85)


ggplot(mtcars, aes(x = mpg, y = drat)) +
  geom_point(aes(color = factor(gear)))


library(geosphere)

train_df %>% mutate(dist_haversine = distHaversine(cbind(pickup_longitude,pickup_latitude),
                                           cbind(dropoff_longitude,dropoff_latitude))/1000) -> train_df
test_df %>% mutate(dist_haversine = distHaversine(cbind(pickup_longitude,pickup_latitude),
                                                   cbind(dropoff_longitude,dropoff_latitude))/1000) -> test_df

train_df %>% mutate(distance_dummy_manhattan = distHaversine(cbind(pickup_longitude,pickup_latitude),
                                                   cbind(dropoff_longitude,pickup_latitude))/1000 +
                                    distHaversine(cbind(pickup_longitude,pickup_latitude),
                                    cbind(pickup_longitude,dropoff_latitude))/1000) -> train_df
test_df %>% mutate(distance_dummy_manhattan = distHaversine(cbind(pickup_longitude,pickup_latitude),
                                                             cbind(dropoff_longitude,pickup_latitude))/1000 +
                      distHaversine(cbind(pickup_longitude,pickup_latitude),
                                    cbind(pickup_longitude,dropoff_latitude))/1000) -> test_df                                     

train_df %>% mutate(direction = bearing(cbind(pickup_longitude,pickup_latitude),
                                                   cbind(dropoff_longitude,dropoff_latitude))) -> train_df
test_df %>% mutate(direction = bearing(cbind(pickup_longitude,pickup_latitude),
                                        cbind(dropoff_longitude,dropoff_latitude))) -> test_df

## create Neighborhoods

coords <- rbind(cbind(train_df$pickup_latitude,train_df$pickup_longitude),
                cbind(train_df$dropoff_latitude, train_df$dropoff_longitude))

randomSamples <- sample(1:nrow(coords),10000)
randomCoords <- coords[randomSamples,]
km.out <- kmeans(randomCoords, centers = 100)

library(clue)
cl_predict(km.out,newdata = coords)
train_df$pickup_cluster <- cl_predict(km.out,cbind(train_df$pickup_latitude,train_df$pickup_longitude))
test_df$pickup_cluster <- cl_predict(km.out,cbind(test_df$pickup_latitude,test_df$pickup_longitude))

train_df$dropoff_cluster <- cl_predict(km.out,cbind(train_df$dropoff_latitude, train_df$dropoff_longitude))
test_df$dropoff_cluster <- cl_predict(km.out,cbind(test_df$dropoff_latitude, test_df$dropoff_longitude))



train_df$Month <- month(train_df$pickup_date)
test_df$Month <- month(test_df$pickup_date)

train_df$Day <- day(train_df$pickup_date)
test_df$Day <- day(test_df$pickup_date)

train_df$Hour <- hour(train_df$pickup_datetime)
test_df$Hour <- hour(test_df$pickup_datetime)

train_df$DayOfWeek <- as.factor(weekdays(train_df$pickup_date))
test_df$DayOfWeek <- as.factor(weekdays(test_df$pickup_date))


train_df %>% count(Month)
test_df %>% count(Month)


dim(train_df)
dim(test_df)

# change to factors
train_df$vendor_id <- as.factor(train_df$vendor_id)
test_df$vendor_id <- as.factor(test_df$vendor_id)

train_df$passenger_count <- as.factor(train_df$passenger_count)
test_df$passenger_count <- as.factor(test_df$passenger_count)

train_df$store_and_fwd_flag <- as.factor(train_df$store_and_fwd_flag)
test_df$store_and_fwd_flag <- as.factor(test_df$store_and_fwd_flag)

train_df$pickup_cluster <- as.factor(train_df$pickup_cluster)
test_df$pickup_cluster <- as.factor(test_df$pickup_cluster)
train_df$dropoff_cluster <- as.factor(train_df$dropoff_cluster)
test_df$dropoff_cluster <- as.factor(test_df$dropoff_cluster)

train_df$Month <- as.factor(train_df$Month)
test_df$Month <- as.factor(test_df$Month)
train_df$Day <- as.factor(train_df$Day)
test_df$Day <- as.factor(test_df$Day)
train_df$DayOfWeek <- as.factor(train_df$DayOfWeek)
test_df$DayOfWeek <- as.factor(test_df$DayOfWeek)
train_df$Hour <- as.factor(train_df$Hour)
test_df$Hour <- as.factor(test_df$Hour)

str(test_df)

dim(train_df)
dim(test_df)
# https://www.kaggle.com/oscarleo/new-york-city-taxi-with-osrm?select=fastest_routes_test.csv
fr1 <- read.csv('fastest_routes_train_part_1.csv')[,c('id', 'total_distance', 'total_travel_time',  'number_of_steps')]
fr2 <- read.csv('fastest_routes_train_part_2.csv')[,c('id', 'total_distance', 'total_travel_time', 'number_of_steps')]
test_street_info = read.csv('fastest_routes_test.csv')[,
                               c('id', 'total_distance', 'total_travel_time', 'number_of_steps')]

train_street_info <- rbind(fr1,fr2)

train_df <- left_join(train_df,train_street_info,by="id")
test_df <- left_join(test_df,test_street_info,by="id")

train_df %>% select(c('dist_haversine','distance_dummy_manhattan',
                      'direction','pickup_cluster','dropoff_cluster',
                      'total_distance','total_travel_time','number_of_steps',
                      'vendor_id','passenger_count','store_and_fwd_flag',
                      'Month','Day','Hour','DayOfWeek','trip_duration')) -> Train_Master

test_df %>% select(c('dist_haversine','distance_dummy_manhattan',
                      'direction','pickup_cluster','dropoff_cluster',
                      'total_distance','total_travel_time','number_of_steps',
                      'vendor_id','passenger_count','store_and_fwd_flag',
                      'Month','Day','Hour','DayOfWeek')) -> Test_Master

dim(Train_Master)
dim(Test_Master)

sum(is.na(Test_Master))
Train_Master <- na.omit(Train_Master)

library(caret)
train_val_split <- createDataPartition(Train_Master$trip_duration, p = .8, list = F)
XMatrix <- sparse.model.matrix(trip_duration ~ ., data = Train_Master)

Train.X <- data.matrix(subset(Train_Master[train_val_split,],select = -trip_duration))
Train.Y <- log(Train_Master$trip_duration)[train_val_split]
dtrain <- xgb.DMatrix(Train.X, label = Train.Y)

Val.X <-  data.matrix(subset(Train_Master[-train_val_split,],select = -trip_duration))
Val.Y <- log(Train_Master$trip_duration)[-train_val_split]
dval <- xgb.DMatrix(Val.X, label = Val.Y)

Test.X <- data.matrix(Test_Master)

library(xgboost)


# Train an xgboost model
watchlist <- list(train = dtrain, val = dval)
param <- list(eta = .5, # learning rate
             nthread = -1, # number of paralell threads 
             object="reg:linear", # regression (for classification, specify "binary:logistic")
             max.depth=10, # number of splits
             verbose = 1,
             subsample = 0.9,
             booster ='gbtree',
             lambda = 1,
             colsample_bytree = 0.9,
             maximize = F,
             min_child_weight = 1) # print training error)

# xgboostTrip <- xgboost(data=XMatrix, # X features have to be in matrix form
#                        label= Train.Y, # Y is a vector
#                        eta = .5, # learning rate
#                        nthread = -1, # number of paralell threads 
#                        nround = 10, # number of rounds of predictions
#                        object="reg:linear", # regression (for classification, specify "binary:logistic")
#                        n.trees=500, # number of trees
#                        max.depth=6, # number of splits
#                        verbose = 1,
#                        subsample = 0.9,
#                        booster ='gbtree',
#                        early_stopping_rounds = 2,
#                        lambda = 1,
#                        eval_metric = 'rmse',
#                        colsample_bytree = 0.9,
#                        maximize = F,
#                        min_child_weight = 1,
#                        watchlist) # print training error

xgboostTrip <- xgb.train(params = param,
                         data = dtrain,
                         nrounds = 1000, 
                         watchlist = watchlist, 
                         early_stopping_rounds = 50,
                         eval_metric = 'rmse')

xgboostTrip$evaluation_log$val_rmse
# Take start time to measure time of random search algorithm
start.time <- Sys.time()

# Create empty lists
lowest_error_list = list()
parameters_list = list()

# Create 10,000 rows with random hyperparameters
set.seed(20)
for (iter in 1:10000){
  param <- list(booster = "gbtree",
                objective = "reg:linear",
                max_depth = sample(3:10, 1),
                eta = runif(1, .01, .3),
                subsample = runif(1, .7, 1),
                colsample_bytree = runif(1, .6, 1),
                min_child_weight = sample(0:10, 1)
  )
  parameters <- as.data.frame(param)
  parameters_list[[iter]] <- parameters
}

# Create object that contains all randomly created hyperparameters
parameters_df = do.call(rbind, parameters_list)

# Use randomly created parameters to create 10,000 XGBoost-models
for (row in 1:nrow(parameters_df)){
  set.seed(20)
  mdcv <- xgb.train(data=dtrain,
                    booster = "gbtree",
                    objective = "reg:linear",
                    max_depth = parameters_df$max_depth[row],
                    eta = parameters_df$eta[row],
                    subsample = parameters_df$subsample[row],
                    colsample_bytree = parameters_df$colsample_bytree[row],
                    min_child_weight = parameters_df$min_child_weight[row],
                    nrounds= 300,
                    eval_metric = "rmse",
                    early_stopping_rounds= 30,
                    print_every_n = 100,
                    watchlist = list(train= dtrain, val= dval)
  )
  lowest_error <- as.data.frame(min(mdcv$evaluation_log$val_error))
  lowest_error_list[[row]] <- lowest_error
}

# Create object that contains all accuracy's
lowest_error_df = do.call(rbind, lowest_error_list)

# Bind columns of accuracy values and random hyperparameter values
randomsearch = cbind(lowest_error_df, parameters_df)

# Quickly display highest accuracy
max(randomsearch$`1 - min(mdcv$evaluation_log$val_error)`)

# Stop time and calculate difference
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

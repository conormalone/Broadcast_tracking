rm(list=ls())
library(tidyverse)
library(StatsBombR)
library(lubridate)
library(keras)
#game_list<-FreeMatches(FreeCompetitions())
#laligagames_1920 <- game_list %>% filter(season.season_name == "2019/2020" & competition.competition_name == "La Liga")
df <-StatsBombFreeEvents()

#match 1 18/12/19 RM v BAR 0 - 0
events_data_3442 <-df %>% filter(match_id == "303470")
events_data_3442$timestamp <- round(hms(events_data_3442$timestamp),1)
tracking_data_3442 <- read_json("3442_structured_data.json", simplifyVector = T)
tracking_data_3442 <-tracking_data_3442[complete.cases(tracking_data_3442$time),]
tracking_data_3442$time <- ms(tracking_data_3442$time)
tracking_data_3442$time <-if_else(tracking_data_3442$period == 1, tracking_data_3442$time, tracking_data_3442$time-minutes(45))
match_data_3442 <- read_json("3442_match_data.json", simplifyVector = T)

#match 2 1/3/20 BAR V RM 2 - 0
events_data_2841 <- df %>% filter(match_id == "303596")
events_data_2841$timestamp <- round(hms(events_data_2841$timestamp),1)
tracking_data_2841<- read_json("2841_structured_data.json", simplifyVector = T)
tracking_data_2841<-tracking_data_2841[complete.cases(tracking_data_2841$time),]
tracking_data_2841$time <-ms(tracking_data_2841$time) 
tracking_data_2841$time <- if_else(tracking_data_2841$period == 1, tracking_data_2841$time, tracking_data_2841$time-minutes(45))
match_data_2841 <- read_json("2841_match_data.json", simplifyVector = T)

rm(df)


events_data_3442$timestamp[100] - tracking_data_3442$time[101]

events_data_2841$play_pattern.name <- as.factor(events_data_2841$play_pattern.name)
levels(events_data_2841$play_pattern.name)
events_data_3442$play_pattern.name <- as.factor(events_data_3442$play_pattern.name)
levels(events_data_3442$play_pattern.name)
events_data_2841$type.name <- as.factor(events_data_2841$type.name)
levels(events_data_2841$type.name)
events_data_3442$type.name <- as.factor(events_data_3442$type.name)
levels(events_data_3442$type.name)

events_data_2841$pass.type.name <- as.factor(events_data_2841$pass.type.name)
levels(events_data_2841$pass.type.name)
events_data_3442$pass.type.name <- as.factor(events_data_3442$pass.type.name)
levels(events_data_3442$pass.type.name)

events_data_2841 <- events_data_2841 %>% mutate(
  category = case_when(
    pass.type.name == "Free Kick" ~ 'FK',
    pass.type.name == "Goak Kick" ~ 'GK',
    pass.type.name == "Corner" ~ 'Corner',
    pass.type.name == "Interception" ~ 'Int',
    pass.type.name == "Kick Off" ~ 'Kick_Off',
    pass.type.name == "Recovery" ~ 'Recovery',
    pass.type.name == "Throw-in" ~ 'Throw',
    is.na(pass.type.name) & type.name == "Pass" ~ 'Pass',
    type.name == "Shot" ~ 'Shot')
)

events_data_3442 <- events_data_3442 %>% mutate(
  category = case_when(
    pass.type.name == "Free Kick" ~ 'FK',
    pass.type.name == "Goak Kick" ~ 'GK',
    pass.type.name == "Corner" ~ 'Corner',
    pass.type.name == "Interception" ~ 'Int',
    pass.type.name == "Kick Off" ~ 'Kick_Off',
    pass.type.name == "Recovery" ~ 'Recovery',
    pass.type.name == "Throw-in" ~ 'Throw',
    is.na(pass.type.name) & type.name == "Pass" ~ 'Pass',
    type.name == "Shot" ~ 'Shot')
)
events_data_3442$category[is.na(events_data_3442$category)] <-"Unknown"
events_data_2841$category[is.na(events_data_2841$category)] <-"Unknown"
merge2841 <- tracking_data_2841 %>%  left_join(events_data_2841, by = c("time" = "timestamp", "period" = "period"))
merge3442 <- tracking_data_3442 %>%  left_join(events_data_3442, by = c("time" = "timestamp", "period" = "period"))

sum(complete.cases(merge2841$category))
table(merge2841$category)

limited_2841 <- merge2841 %>%  select(c(time, period, category, data)) 

limited_3442 <- merge3442 %>%  select(c(time, period, category, data)) 
#add home/away team to data
match_data_3442$players$group_name <- ifelse(match_data_3442$players$team_id == match_data_3442$home_team$id, "home team", "away team")
match_data_2841$players$group_name <- ifelse(match_data_2841$players$team_id == match_data_2841$home_team$id, "home team", "away team")                                     

final_3442 <- limited_3442 %>% unnest_wider(data)
final_2841 <- limited_2841 %>% unnest_wider(data)

rm(merge2841)
rm(merge3442)
rm(events_data_2841)
rm(events_data_3442)
rm(tracking_data_2841)
rm(tracking_data_3442)
rm(limited_2841)
rm(limited_3442)


train_A <- final_2841 %>% filter(period ==1) %>% training_prep_function
train_B <-final_3442 %>% filter(period ==2) %>% training_prep_function
valid_3442 <-final_3442 %>% filter(period ==1) %>% training_prep_function
test_3442 <-final_2841 %>% filter(period ==2) %>% training_prep_function
train_2841 <- c(train_A, train_B)

y_train_2841 <- train_2841 %>% y_split_function
x_train_2841 <- train_2841 %>% x_split_function
y_valid_3442 <- valid_3442 %>% y_split_function
x_valid_3442 <- valid_3442 %>% x_split_function
x_3442_test <- test_3442 %>% x_split_function
y_3442_test <- test_3442 %>% y_split_function
#get 1 in ten frames
train_one_in_ten <- seq(1, length(y_train_2841), 10)
valid_one_in_ten <- seq(1, length(y_valid_3442), 10)
#sampling unknowns from training
train_not_unknown <- which(y_train_2841 != "Unknown")
train_just_unknown_to_sample <- which(y_train_2841 == "Unknown")
train_just_unknown<- sample(train_just_unknown_to_sample, 2000)
train_selected_samples <-c(train_not_unknown, train_just_unknown)
#sampling unknowns from valid
valid_not_unknown <- which(y_valid_3442 != "Unknown")
valid_just_unknown_to_sample <- which(y_valid_3442 == "Unknown")
valid_just_unknown<- sample(valid_just_unknown_to_sample, 1000)
valid_selected_samples <-c(valid_not_unknown, valid_just_unknown)
#sampling unknowns finishes
x <-x_long[selected_samples]
y <- y_long[selected_samples]
#from project to do sampling
N<-length(y)
trainset<-sort(sample(1:N,size=floor(N*0.75)))
validset<-setdiff(1:N,trainset)

x_train <- x_train_2841[train_selected_samples]
x_valid <- x_valid_3442[valid_selected_samples]
y_train_subset<- y_train_2841[train_selected_samples]
y_train_factor <- as.factor(unlist(y_train_subset))
y_train <- to_categorical(as.numeric(y_train_factor), 10)
#y_train <- y_train[,-1]
y_valid_subset <- y_valid_3442[valid_selected_samples]
y_valid_factor <- as.factor(unlist(y_valid_subset))
y_valid <- to_categorical(as.numeric(y_valid_factor), 10)
#y_valid <- y_valid[,-1]
y_test_factor <- as.factor(unlist(y_3442_test))
y_test <- to_categorical(as.numeric(y_test_factor), 10)
#y_test <- y_test[,-1]
#to make array
library(abind)
x_train_array <- abind(x_train, along =0)
x_valid_array <- abind(x_valid, along =0)
x_test_array<- abind(x_3442_test, along =0)
dim(x_train_array)


model <- keras_model_sequential() %>%
  layer_conv_2d(filters =64,kernel_size =2,activation ="relu",
                input_shape =c(23,3,10))%>%
  layer_max_pooling_2d(pool_size =2)%>%
  ## fully connected layers
  layer_flatten()%>%
  layer_dense(units = 480, activation = "relu") %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 240, activation = "relu") %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 120, activation = "relu") %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 60, activation = "relu") %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 10, activation = "softmax") %>%
  compile(
    loss = "categorical_crossentropy", metrics = "accuracy",
    optimizer = optimizer_rmsprop(),
  )
#fit the model
N <- length(x_train)
bs <- round(N * 0.01)
fit <- model %>% fit(
  x = x_train_array, y = y_train,
  validation_data = list(x_valid_array, y_valid),
  epochs = 100,
  batch_size = bs,
  verbose = 1,
  callbacks = callback_early_stopping(monitor = "val_accuracy", patience = 20)
  )



keras::evaluate(model, x_test_array,y_test)

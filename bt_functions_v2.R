##to do
# DONE have function take in separate halves, 
# DONE pad start and ends x5
#MAYBEDONE finish convoluter function
#finish one hot encoding categories, add to lists
#make it a function
#blerk <-dummyVars(~ category, data = df)
#data.frame(predict(blerk, newdata = df))

#convoluter function
convoluter_function <-function(x){
  function_results <-vector(mode = "list", length = (length(x)-5))
  for(i in 5:(length(x)-5)){
    thearray <- array(c(x[i+5][[1]]$thematrix,x[i+4][[1]]$thematrix,x[i+3][[1]]$thematrix,x[i+2][[1]]$thematrix,x[i+1][[1]]$thematrix,x[i][[1]]$thematrix,x[i-1][[1]]$thematrix,x[i-2][[1]]$thematrix,x[i-3][[1]]$thematrix,x[i-4][[1]]$thematrix), dim = c(23,3,10))
    
    allcategories <- c(x[i+5][[1]][[1]],x[i+4][[1]][[1]],x[i+3][[1]][[1]],x[i+2][[1]][[1]],x[i+1][[1]][[1]],x[i][[1]][[1]],x[i-1][[1]][[1]],x[i-2][[1]][[1]],x[i-3][[1]][[1]],x[i-4][[1]][[1]])
    goodcategories <-ifelse(sum(!is.na(allcategories)) ==0,"Unknown",allcategories[!is.na(allcategories)])
    function_results[[i]]$category <- goodcategories
    function_results[[i]]$thematrix <- thearray
  }
  return(function_results)}
####prep function
lean_training_prep_function <- function(dataframe){
biggest_result <- list()
x_result <- NULL
y_result <- NULL
z_result <- NULL

#add padding rows before and after each period
d <- data.frame(time = NA,period = NA, category = NA, y  = NA , x = NA, trackable_object = NA, track_id = NA, group_name = NA, z = NA)
d$y <-list(matrix(nrow =23,ncol=1))
d$x <-list(matrix(nrow =23,ncol=1))  
d$z <-list(matrix(nrow =23,ncol=1))
before_rows <- do.call("rbind", replicate(4, d, simplify = FALSE))
after_rows <- do.call("rbind", replicate(5, d, simplify = FALSE))
#theloop for training an val
for(p in 1:max(dataframe$period)){

df <- dataframe %>% filter(period ==p) 
df <- rbind(before_rows, df, after_rows)
df$z[sapply(df$z, is.null) ] <- NA
biggest_result <- foreach(i = 1:nrow(df),.combine=c) %dopar% {
    big_result<- list()
    rl_result<- list()
    rw_result<- list()
    ra_result<- list()
    df$x[i] <- ifelse(is.null(df$x[i][[1]]),df$x[i-1],df$x[i])
    df$y[i] <- ifelse(is.null(df$y[i][[1]]),df$y[i-1],df$y[i])
  x_result <- mapply(function(x) {
    length(x) <- 23 
    return(x)
  }, df$x[i])
  y_result <-mapply(function(x) {
    length(x) <- 23 
    return(x)
  }, df$y[i])
  z_result <-unlist(ifelse(is.na(df$z[i][[1]][[1]]), list(matrix(nrow =23,ncol=1)),mapply(function(x) {
    length(x) <- 23
    return(x)
  }, df$z[i]) ))
  x_result[is.na(x_result)]<-0
  y_result[is.na(y_result)]<-0
  z_result[is.na(z_result)]<-0
  thematrix <- matrix(c(x_result, y_result, z_result),ncol =3,nrow =23)
  
  y_reverse <-y_result*-1
  x_reverse <-x_result*-1
  reversewidth <- matrix(c(x_result, y_reverse, z_result),ncol =3,nrow =23)
  reverselength <- matrix(c(x_reverse, y_result, z_result),ncol =3,nrow =23)
  reverseboth <-thematrix*-1
  #add straightforward
  big_result[i]  <- c(df[i,3])
  big_result[[i]]$thematrix <- thematrix
  #add reversed versions to augment data
  rl_result[i] <- c(df[i,3])
  rl_result[[i]]$thematrix <- reverselength
  rw_result[i]  <- c(df[i,3])
  rw_result[[i]]$thematrix <- reversewidth
  ra_result[i] <- c(df[i,3])
  ra_result[[i]]$thematrix <- reverseboth
  #print(rl_result[[i]] )
  #print(rw_result[[i]] )
  #print(ra_result[[i]] )
  #print(big_result[[i]] )
  appendo_list <- c(big_result[i],rl_result[i], rw_result[i],ra_result[i])
  biggest_result <- append(biggest_result, appendo_list)
  
  }
}

final_results <- convoluter_function(biggest_result)
return(final_results)
}

y_split_function <- function(x){
  y_train <- list()
  for(i in 1:length(x)){
    y_train[[i]] <- x[[i]]$category
  }
  return(y_train)
}
x_split_function <- function(x){
  x_train <- list()
  for(i in 1:length(x)){
    x_train[[i]] <- x[[i]]$thematrix
  }
  return(x_train)
}
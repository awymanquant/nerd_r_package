extract_consecutive_negatives <- function(vector) {
  consecutive_negatives <- list()
  current_sequence <- c()
  
  for (i in 1:length(vector)) {
    if (vector[i] < 0) {
      current_sequence <- c(current_sequence, vector[i])
    } else {
      if (length(current_sequence) > 1) {
        consecutive_negatives[[length(consecutive_negatives) + 1]] <- current_sequence
      }
      current_sequence <- c()
    }
  }
  
  if (length(current_sequence) > 1) {
    consecutive_negatives[[length(consecutive_negatives) + 1]] <- current_sequence
  }
  
  consecutive_negatives
}

extract_locations <- function(vector) {
  negative_indices <- c()
  current_start_index <- NA
  
  for (i in 1:(length(vector) - 1)) {
    if (vector[i] < 0 && vector[i + 1] < 0) {
      if (is.na(current_start_index)) {
        current_start_index <- i
      }
    } else {
      if (!is.na(current_start_index)) {
        negative_indices <- c(negative_indices, i)
        current_start_index <- NA
      }
    }
  }
  
  if (!is.na(current_start_index)) {
    negative_indices <- c(negative_indices, length(vector))
  }
  
  negative_indices
}

makeNA <- function(anger=NULL, sadness=NULL, fear=NULL, id_vector=NULL, method=c("max","mean"), extra_emotions = NULL){
  if (length(method) > 1){
    stop("Only one method can be selected at a time. Choose either max or mean.")
  }
  if (is.null(anger)==TRUE){
    stop("At least anger and sadness variables are required for negative affect.")
  }
  if (is.null(sadness)==TRUE){
    stop("At least anger and sadness variables are required for negative affect.")
  }
  if (is.null(id_vector)==TRUE){
    stop("A vector or dataframe of IDs must be provided.")
  }
  
  if (is.null(fear)==TRUE){
    if (is.null(extra_emotions)==TRUE){
      
      ids <- unique(id_vector)
      N <- seq_len(length((ids)))
      
      data <- data.frame(anger, sadness, id_vector)
      
      if (method == "max"){
        all.negative <- NULL
        for (n in N) {
          participant <- data[data$id_vector==ids[n],]
          negative <- matrix(NaN, nrow(participant), 2)
          for (t in seq_len(nrow(participant))) {
            max.neg <- max(participant$anger[t], participant$sadness[t])
            negative[t,1] <- max.neg
            negative[t,2] <- ids[n]
          }
          
          all.negative <- rbind(all.negative, negative)
        }
        colnames(all.negative) <- c("negative_affect", "id")
        all.negative <- as.data.frame(all.negative)
        return(all.negative)
      }
      
      if (method == "mean"){
        all.negative <- NULL
        for (n in N) {
          participant <- data[data$id_vector==ids[n],]
          negative <- matrix(NaN, nrow(participant), 2)
          for (t in seq_len(nrow(participant))) {
            max.neg <- mean(participant$anger[t], participant$sadness[t])
            negative[t,1] <- max.neg
            negative[t,2] <- ids[n]
          }
          
          all.negative <- rbind(all.negative, negative)
        }
        colnames(all.negative) <- c("negative_affect", "id")
        all.negative <- as.data.frame(all.negative)
        return(all.negative)
      }
    }
    
    
    
    if(is.null(extra_emotions)==FALSE){
      ids <- unique(id_vector)
      N <- seq_len(length((ids)))
      
      data <- data.frame(id_vector, anger, sadness, extra_emotions)
      if (method == "max"){
        all.negative <- NULL
        for (n in N) {
          participant <- data[data$id_vector==ids[n],]
          negative <- matrix(NaN, nrow(participant), 2)
          for (t in seq_len(nrow(participant))) {
            max.neg <- max(participant[t,-1])
            negative[t,1] <- max.neg
            negative[t,2] <- ids[n]
          }
          
          all.negative <- rbind(all.negative, negative)
        }
        colnames(all.negative) <- c("negative_affect", "id")
        all.negative <- as.data.frame(all.negative)
        return(all.negative)
      }
      
      if (method == "mean"){
        all.negative <- NULL
        for (n in N) {
          participant <- data[data$id_vector==ids[n],]
          negative <- matrix(NaN, nrow(participant), 2)
          for (t in seq_len(nrow(participant))) {
            max.neg <- rowMeans(participant[t,-1])
            negative[t,1] <- max.neg
            negative[t,2] <- ids[n]
          }
          
          all.negative <- rbind(all.negative, negative)
        }
        colnames(all.negative) <- c("negative_affect", "id")
        all.negative <- as.data.frame(all.negative)
        return(all.negative)
      }
      
    }else{
      if(is.vector(extra_emotions)==TRUE){
        ids <- unique(id_vector)
        N <- seq_len(length((ids)))
        
        data <- data.frame(id_vector, anger, sadness, extra_emotions)
        if (method == "max"){
          all.negative <- NULL
          for (n in N) {
            participant <- data[data$id_vector==ids[n],]
            negative <- matrix(NaN, nrow(participant), 2)
            for (t in seq_len(nrow(participant))) {
              max.neg <- max(participant[t,-1])
              negative[t,1] <- max.neg
              negative[t,2] <- ids[n]
            }
            
            all.negative <- rbind(all.negative, negative)
          }
          colnames(all.negative) <- c("negative_affect", "id")
          all.negative <- as.data.frame(all.negative)
          return(all.negative)
        }
        
        if (method == "mean"){
          all.negative <- NULL
          for (n in N) {
            participant <- data[data$id_vector==ids[n],]
            negative <- matrix(NaN, nrow(participant), 2)
            for (t in seq_len(nrow(participant))) {
              max.neg <- rowMeans(participant[t,-1])
              negative[t,1] <- max.neg
              negative[t,2] <- ids[n]
            }
            
            all.negative <- rbind(all.negative, negative)
          }
          colnames(all.negative) <- c("negative_affect", "id")
          all.negative <- as.data.frame(all.negative)
          return(all.negative)
        }
      }else{
        stop("Additional emotions must be a vector or dataframe.")
      }
    }
  }else{
    
    
    if (is.null(extra_emotions)==TRUE){
      
      ids <- unique(id_vector)
      N <- seq_len(length((ids)))
      
      data <- data.frame(anger, sadness, fear, id_vector)
      
      if (method == "max"){
        all.negative <- NULL
        for (n in N) {
          participant <- data[data$id_vector==ids[n],]
          negative <- matrix(NaN, nrow(participant), 2)
          for (t in seq_len(nrow(participant))) {
            max.neg <- max(participant$anger[t], participant$sadness[t],
                           participant$fear[t])
            negative[t,1] <- max.neg
            negative[t,2] <- ids[n]
          }
          
          all.negative <- rbind(all.negative, negative)
        }
        colnames(all.negative) <- c("negative_affect", "id")
        all.negative <- as.data.frame(all.negative)
        return(all.negative)
      }
      
      if (method == "mean"){
        all.negative <- NULL
        for (n in N) {
          participant <- data[data$id_vector==ids[n],]
          negative <- matrix(NaN, nrow(participant), 2)
          for (t in seq_len(nrow(participant))) {
            max.neg <- mean(participant$anger[t], participant$sadness[t],
                            participant$fear[t])
            negative[t,1] <- max.neg
            negative[t,2] <- ids[n]
          }
          
          all.negative <- rbind(all.negative, negative)
        }
        colnames(all.negative) <- c("negative_affect", "id")
        all.negative <- as.data.frame(all.negative)
        return(all.negative)
      }
    }
    
    
    
    if(is.null(extra_emotions)==FALSE){
      ids <- unique(id_vector)
      N <- seq_len(length((ids)))
      
      data <- data.frame(id_vector, anger, sadness, fear, extra_emotions)
      if (method == "max"){
        all.negative <- NULL
        for (n in N) {
          participant <- data[data$id_vector==ids[n],]
          negative <- matrix(NaN, nrow(participant), 2)
          for (t in seq_len(nrow(participant))) {
            max.neg <- max(participant[t,-1])
            negative[t,1] <- max.neg
            negative[t,2] <- ids[n]
          }
          
          all.negative <- rbind(all.negative, negative)
        }
        colnames(all.negative) <- c("negative_affect", "id")
        all.negative <- as.data.frame(all.negative)
        return(all.negative)
      }
      
    }else{
      if(is.vector(extra_emotions)==TRUE){
        ids <- unique(id_vector)
        N <- seq_len(length((ids)))
        
        data <- data.frame(id_vector, anger, sadness, fear, extra_emotions)
        if (method == "mean"){
          all.negative <- NULL
          for (n in N) {
            participant <- data[data$id_vector==ids[n],]
            negative <- matrix(NaN, nrow(participant), 2)
            for (t in seq_len(nrow(participant))) {
              max.neg <- rowMeans(participant[t,-1])
              negative[t,1] <- max.neg
              negative[t,2] <- ids[n]
            }
            
            all.negative <- rbind(all.negative, negative)
          }
          colnames(all.negative) <- c("negative_affect", "id")
          all.negative <- as.data.frame(all.negative)
          return(all.negative)
        }
      }else{
        stop("Additional emotions must be a vector or dataframe.")
      }
    }
  }
}


format <- function(data_vector=NULL, id_vector=NULL){
  if (is.null(data_vector)==TRUE){
    stop("1 dimensional data frame or vector must be provided")
  }
  if (is.null(id_vector)==TRUE){
    stop("1 dimensional data frame or vector must be provided")
  }
  
  ids <- unique(id_vector)
  data <- data.frame(data_vector, id_vector)
  
  
  data_list <- list()
  for (i in 1:length(ids)) {
    participant_data <- data[data$id_vector==ids[i],]
    data_list[[i]] <- participant_data$data_vector
  }
  
  formatted_data <- data_list
  return(formatted_data)
}

get_intensity <- function(data=NULL, window.size=2, log.transform=TRUE, threshold.type=c("sample","individual","fixed"), threshold=0.75){
  # Check messages
  if (is.null(data)==TRUE){
    stop("Data must be provided in a list object. Use format() for assistance.")
  }
  if (is.list(data)==FALSE){
    stop("Data must be provided in a list object. Use format() for assistance.")
  }
  if (window.size < 0){
    stop("window size must be a positive integer")
  }
  if (is.numeric(window.size)==FALSE){
    stop("Window size must be a positive integer")
  }
  if (is.logical(log.transform)==FALSE){
    stop("log.transform must be set to TRUE or FALSE")
  }
  if (threshold <= 0){
    stop("Threshold must be a positive integer")
  }
  if (threshold < 0.3){
    warning("Threshold is concerningly small. Results may be uninformative.")
  }
  
  # Set parameters
  ids <- length(data)
  N <- seq_len(ids)
  
  if (threshold.type == "sample"){
    all.ts <- NULL
    
    for (n in N) {
      participant <- data[[n]]
      var <- participant
      var.ts <- ts(as.vector(var))
      all.ts <- c(all.ts, var.ts)
    }
    
    variableX <- quantile(all.ts, threshold)
  }else{
    variableX <- NULL
  }
  
  if (window.size == 0){
    warning("Window size has been set to 0. No filter will be used.")
    
    intensity.all <- NULL
    for (n in N) {
      participant <- data[[n]]
      var <- participant
      var.ts <- ts(as.vector(var))
      
      individual.threshold <- quantile(var.ts, threshold)
      variableX <- ifelse(is.null(variableX)==T, 
                          individual.threshold, 
                          variableX)
      
      loc <- var.ts >= threshold
      time_above <- mean(ifelse(loc==TRUE, 1, 0))
      weights <- sum(ifelse(loc==TRUE, ts.MA - threshold, 0))
      log_weights <- ifelse(weights > 0, log(weights), 0)
      
      log_weights <- ifelse(log.transform==T, 
                            log_weights, 
                            weights)
      intensity <- time_above * log_weights
      intensity.all <- rbind(intensity.all, intensity)
    }
    
  }else{
    q <- window.size
    
    intensity.all <- NULL
    
    for (n in N) {
      participant <- data[[n]]
      var <- participant
      var.ts <- ts(as.vector(var))
      
      MA.filter <- rep(1/(2*q+1), 2*q+1)
      ts.MA <- filter(var.ts, MA.filter)
      ts.MA <- ts.MA[-c(1:q,(length(ts.MA)-q):length(ts.MA))]
      var.ts <- ts(ts.MA)
      
      individual.threshold <- quantile(var.ts, threshold)
      variableX <- ifelse(is.null(variableX)==T, 
                          individual.threshold, 
                          variableX)
      
      loc <- var.ts >= threshold
      time_above <- mean(ifelse(loc==TRUE, 1, 0))
      weights <- sum(ifelse(loc==TRUE, ts.MA - threshold, 0))
      log_weights <- ifelse(weights > 0, log(weights), 0)
      
      log_weights <- ifelse(log.transform==T, 
                            log_weights, 
                            weights)
      intensity <- time_above * log_weights
      intensity.all <- rbind(intensity.all, intensity)
    }
    
    
  }
  
  intensity.all <- as.data.frame(intensity.all)
  colnames(intensity.all) <- "Intensity"
  return(intensity.all)
  
}


get_lability <- function(data=NULL, window.size=2, minseglen=10, penalty=0.2){
  # Check messages
  if (is.null(data)==TRUE){
    stop("Data must be provided in a list object. Use format() for assistance.")
  }
  if (is.list(data)==FALSE){
    stop("Data must be provided in a list object. Use format() for assistance.")
  }
  if (window.size < 0){
    stop("window size must be a positive integer")
  }
  if (is.numeric(window.size)==FALSE){
    stop("Window size must be a positive integer")
  }
  if (penalty < 0){
    stop("Penalty must be an integer between 0 and 1.")
  }
  if (penalty > 1){
    stop("Penalty must be an integer between 0 and 1.")
  }
  if (penalty == 1){
    warning("No penalty is being applied.")
  }
  if (penalty == 0){
    warning("A severe penalty is being applied.")
  }
  
  # Set parameters
  ids <- length(data)
  N <- seq_len(ids)
  
  
  if (window.size == 0){
    warning("Window size has been set to 0. No filter will be used.")
    
    lability.all <- NULL
    for (n in N) {
      participant <- data[[n]]
      var <- participant
      var.ts <- ts(as.vector(var))
      
      require(EnvCpt)
      fit_envcpt <- envcpt(var.ts, minseglen = minseglen, models = "meanar1cpt", verbose=FALSE)
      cpts <- fit_envcpt$meanar1cpt@cpts
      slope.pts <- fit_envcpt$meanar1cpt@param.est$beta
      frequency <- length(cpts)/length(var.ts)
      magnitude <- sum(abs(slope.pts[,2])) / length(cpts)
      
      lability <- frequency + penalty*magnitude
      lability.all <- rbind(lability.all, lability)
    }
    
  }else{
    q <- window.size
    
    lability.all <- NULL
    
    for (n in N) {
      participant <- data[[n]]
      var <- participant
      var.ts <- ts(as.vector(var))
      
      MA.filter <- rep(1/(2*q+1), 2*q+1)
      ts.MA <- filter(var.ts, MA.filter)
      ts.MA <- ts.MA[-c(1:q,(length(ts.MA)-q):length(ts.MA))]
      var.ts <- ts(ts.MA)
      
      require(EnvCpt)
      fit_envcpt <- envcpt(var.ts, minseglen = minseglen, models = "meanar1cpt", verbose=FALSE)
      cpts <- fit_envcpt$meanar1cpt@cpts
      slope.pts <- fit_envcpt$meanar1cpt@param.est$beta
      frequency <- length(cpts)/length(var.ts)
      magnitude <- sum(abs(slope.pts[,2])) / length(cpts)
      
      lability <- frequency + penalty*magnitude
      lability.all <- rbind(lability.all, lability)
      
      
    }
    
  }
  
  lability.all <- as.data.frame(lability.all)
  colnames(lability.all) <- "Lability"
  return(lability.all)
  
}


get_latency <- function(data=NULL, window.size=2){
  # Check messages
  if (is.null(data)==TRUE){
    stop("Data must be provided in a list object. Use format() for assistance.")
  }
  if (is.list(data)==FALSE){
    stop("Data must be provided in a list object. Use format() for assistance.")
  }
  if (window.size < 0){
    stop("window size must be a positive integer")
  }
  if (is.numeric(window.size)==FALSE){
    stop("Window size must be a positive integer")
  }
  
  # Set parameters
  ids <- length(data)
  N <- seq_len(ids)
  
  if (window.size == 0){
    warning("Window size has been set to 0. No filter will be used.")
    
    latency.all <- NULL
    for (n in N) {
      participant <- data[[n]]
      var <- participant
      var.ts <- ts(as.vector(var))
      
      ts.slopes <- diff(var.ts, differences = 1)
      first.derivatives <- extract_consecutive_negatives(ts.slopes)
      
      second.diff <- NULL
      for (i in 1:length(first.derivatives)) {
        current.diff <- diff(first.derivatives[[i]][c(1,length(first.derivatives[[i]]))], 
                             differences=1) / (length(first.derivatives[[i]])-1)
        second.diff <- rbind(second.diff, current.diff)
      }
      
      end.y <- extract_locations(ts.slopes)
      mean.y <- mean(var.ts)
      weights <- mean.y - end.y
      latency <- sum(weights*second.diff[,1] / sum(weights))
      latency.all <- rbind(latency.all, latency)
    }
    
  }else{
    q <- window.size
    
    latency.all <- NULL
    
    for (n in N) {
      participant <- data[[n]]
      var <- participant
      var.ts <- ts(as.vector(var))
      
      MA.filter <- rep(1/(2*q+1), 2*q+1)
      ts.MA <- filter(var.ts, MA.filter)
      ts.MA <- ts.MA[-c(1:q,(length(ts.MA)-q):length(ts.MA))]
      var.ts <- ts(ts.MA)
      
      ts.slopes <- diff(var.ts, differences = 1)
      first.derivatives <- extract_consecutive_negatives(ts.slopes)
      
      second.diff <- NULL
      for (i in 1:length(first.derivatives)) {
        current.diff <- diff(first.derivatives[[i]][c(1,length(first.derivatives[[i]]))], 
                             differences=1) / (length(first.derivatives[[i]])-1)
        second.diff <- rbind(second.diff, current.diff)
      }
      
      end.y <- extract_locations(ts.slopes)
      mean.y <- mean(var.ts)
      weights <- mean.y - end.y
      latency <- sum(weights*second.diff[,1] / sum(weights))
      latency.all <- rbind(latency.all, latency)
    }
    
  }
  
  latency.all <- as.data.frame(latency.all)
  colnames(latency.all) <- "Latency"
  return(latency.all)
  
}
















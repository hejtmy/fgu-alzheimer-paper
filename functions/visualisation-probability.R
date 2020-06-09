create_cross_probability_matrix <- function(obj, to = "left"){
  res <- get_cross_times(obj, to)
  res <- n_cross_matrix(res, accuracy = 1)
  res <- cross_probability_matrix(res)
  return(res)
}

#' Returns a data frame with times of crosses
#'
#' @param obj Type avoidance.multiple
#'
#' @return
#' @export
#'
#' @examples
get_cross_times <- function(obj, to = "left"){
  df_crosses <- data.frame(stringsAsFactors = FALSE)
  for(anim in names(obj)){
    crosses <- fgu.avoidance::collect_crosses(obj[[anim]])
    times <- crosses[crosses$to == to, "time"]
    n_crosses <- length(times)
    if(n_crosses < 1) next
    res <- data.frame(animal = rep(anim, n_crosses), i_cross = 1:n_crosses,
                      crossing_times=times, stringsAsFactors = FALSE)
    df_crosses <- rbind(df_crosses, res)
  }
  return(df_crosses)
}

#' Returns a dummy coded matrix determining each animal present at given time
#'
#' @param obj avoidance.multiple object
#'
#' @return matrix with each column being a timeseries for an animal determining which area was the animal in
#' at given time. Each row is a timepoint (currently at 1s intervals)
#' @export
#'
#' @examples
#' TODO - check the rounding 
#' TODO - check the max time and situation with not all animals havin ghte same length
area_presence_matrix <- function(obj){
  ## TODO - redo this to factors to radically change the size and speed
  df_crosses <- data.frame(stringsAsFactors = FALSE)
  animals <- names(obj)
  for(anim in animals){
    ## This is cut from the plot area presence - potentially include it in the package as its own function
    crosses <- fgu.avoidance::collect_crosses(obj[[anim]])
    ordered <- crosses[order(crosses$time),]
    # if there were no crosses altogether, we create a fake crosses with the start area
    if(nrow(ordered) < 1){
      start_area <- get_position_table(obj[[anim]])[1, 'area']
      ordered <- data.frame(time = 0, from = start_area, 
                            to = start_area, stringsAsFactors = FALSE)
    }
    time_start <- c(0, ordered$time)
    time_end <- c(ordered$time, tail(obj[[anim]]$position$data$timestamp, 1))
    location <- c(ordered$from, tail(ordered$to, 1))
    df <- data.frame(animal = anim, where = location, start = time_start, 
                     end = time_end, stringsAsFactors = FALSE)
    df$duration <- df$end - df$start
    df_crosses <- rbind(df_crosses, df)
  }
  df_crosses$duration <- round(df_crosses$duration)
  max_time <- ceiling(max(df_crosses$end))
  res <- matrix(0, nrow = max_time, ncol = length(animals))
  for(i_animal in 1:length(animals)){
    animal <- animals[i_animal]
    df_anim <- df_crosses[df_crosses$animal == animal, ]
    area_presence <- rep(df_anim$where, df_anim$duration)
    last_area <- tail(area_presence, 1)
    need_to_fill <- max_time - length(area_presence)
    if(need_to_fill > 0){
      area_presence <- c(area_presence, rep(last_area, max_time - length(area_presence)))
    }
    # can happen due to the rounding errors - basically we ned to drop few last elements
    if(need_to_fill < 0){
      area_presence <- area_presence[1:(length(area_presence) + need_to_fill)]
    }
    res[, i_animal] <- area_presence
  }
  return(res)
}

#' Creates a probability data.frame with ........
#'
#' @param mat_area_presence 
#'
#' @return
#' @export
#'
#' @examples
area_presence_probability <- function(mat_area_presence){
  areas <- unique(as.vector(mat_area_presence))
  prob <- matrix(NA, nrow = nrow(mat_area_presence), ncol = length(areas))
  for(i_area in 1:length(areas)){
    prob_area <- rowSums(mat_area_presence == areas[i_area])/ncol(mat_area_presence)
    prob[, i_area] <- prob_area
  }
  prob <- data.frame(prob)
  colnames(prob) <- areas
  prob_long <- reshape(prob, varying = colnames(prob),
                       v.names="probability", timevar="area",
                       idvar = "time", direction = "long")
  prob_long$area <- areas[prob_long$area]
  return(prob_long)
}

#' Converts the table from get_cross_times to a dummy matrix 
#'
#' @param df df_crosses 
#' @param accuracy 
#'
#' @return matrix of number of crosses (time x animal). each column contains a vector of
#' a total number of crosses for a particular animal. Each row signifies a time
#' @export
#'
#' @examples
n_cross_matrix <- function(df, accuracy = 1){
  df$crossing_times <- round(df$crossing_times, 0) 
  max_time <- max(df$crossing_times)
  animals <- unique(df$animal)
  res <- matrix(0, nrow = max_time, ncol = length(animals))
  for(i_animal in 1:length(animals)){
    animal <- animals[i_animal]
    times <- df$crossing_times[df$animal == animal]
    for(time in times){
      res[time:max_time, i_animal] <- res[time:max_time, i_animal] + 1
    }
  }
  return(res)
}

#' Creates a matrix of probability of presence
#'
#' @param res dummy cross matrix as created by dymmy_cross_matrix
#'
#' @return data.frame with 
#' @export
#'
#' @examples
cross_probability_matrix <- function(res){
  max_crossings <- max(res)
  prob <- matrix(0, nrow(res), max_crossings)
  for(i in 1:max_crossings){
    prob_n <- rowSums(res >= i)/ncol(res)
    prob[, i] <- prob_n
  }
  prob <- data.frame(prob)
  prob_long <- reshape(prob, varying = colnames(prob), 
                       v.names="probability", timevar = "n_crosses",
                       idvar = "time", direction = "long")
  return(prob_long)
}

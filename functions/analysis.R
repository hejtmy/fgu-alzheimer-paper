#' returns freezes and some summative stats
#'
#' @param obj avoidance.single object
#'
#' @return
#' @export
#'
#' @examples
analyze_freezing <- function(obj, min_duration, speed_threshold){
  res <- list()
  res$data <- collect_freezes(obj, min_duration = min_duration, 
                                 speed_threshold = speed_threshold)
  res$summary <- list(
    n_freezes = length(res$data$time),
    freeze_time = sum(res$data$duration),
    freeze_ratio = sum(res$data$duration)/tail(obj$position$data$time_since_start, 1)
  )
  return(res)
}
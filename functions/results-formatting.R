library(knitr)
session_results_output <- function(results){
  txt <- ""
  txt <- paste0(txt, results_line("Animal", results$animal))
  txt <- paste0(txt, results_line("Distance", results$distance))
  txt <- paste0(txt, results_line("Time in left zone", results$time_left))
  txt <- paste0(txt, results_line("Time in right zone", results$time_right))
  txt <- paste0(txt, results_line("Time in central zone", results$time_center))
  txt <- paste0(txt, results_line("Crosses to the right", results$crosses_right))
  txt <- paste0(txt, results_line("Crosses to the left", results$crosses_left))
  txt <- paste0(txt, results_line("Number of freezes", results$n_freezes))
  txt <- paste0(txt, results_line("Total freeze time", results$freeze_time))
  txt <- paste0(txt, results_line("Freeze time ratio", results$freeze_ratio))
  txt <- paste0(txt, "")
  return(txt)
}

results_line <- function(name, result, dec = 2){
  if(is.numeric(result) & !is.null(round)){
    result <- round(result, dec)
  }
  txt <- paste0("<br>", name,  ": <div class='pull-right'>", result, "</div>")
}
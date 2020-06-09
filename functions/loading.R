load_data <- function(folder){
  res <- list()
  folder_runs <- list.dirs(folder, recursive = FALSE, full.names = TRUE)
  for(folder_run in folder_runs){
    folders_sex <- list.dirs(folder_run, recursive = FALSE, full.names = TRUE)
    for(folder_sex in folders_sex){
      ls_sex_run <- get_sex_run(folder_sex)
      if(is_habituation(ls_sex_run$run)){
        dat <- load_folder(folder_sex)
        dat <- add_areas(dat)
        res[[ls_sex_run$run]][[ls_sex_run$sex]] <- dat
      } else {
        folders_group <- list.dirs(folder_sex, recursive = FALSE, full.names = TRUE)
        for(folder_group in folders_group){
          dat <- load_folder(folder_group)
          dat <- add_areas(dat)
          res[[ls_sex_run$run]][[ls_sex_run$sex]][[basename(folder_group)]] <- dat
        }
      }
    }
  }
  return(res)
}

get_sex_run <- function(folder){
  name <- basename(folder)
  sex <- sub("(.+)\\s{1}(.+)", "\\1", name)
  run <- sub("(.+)\\s{1}(.+)", "\\2", name)
  return(list(run = run, sex = sex))
}

is_habituation <- function(run){
  return(grepl("hab", run))
}

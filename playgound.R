library(fgu.avoidance)
source("functions/visualisation-probability.R")
source("functions/loading.R")
data_folder <- file.path("data", "")

obj <- load_data(data_folder)


animal <- obj$hab1$females$animal_10
plot_speed(animal) +
  xlim(0,100)

plot_path(animal)
freezes <- collect_freezes(animal, min_duration = 2, speed_threshold = 1)
str(freezes)
freeze_times <- rbind(freezes$time, freezes$time+freezes$duration)

plot_path(animal) +
  geom_navr_path_events(animal$position, freeze_times)


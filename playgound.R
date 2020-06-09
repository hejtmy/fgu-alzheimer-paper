library(fgu.avoidance)
source("functions/visualisation-probability.R")
source("functions/loading.R")
data_folder <- file.path("data", "")

obj <- load_data(data_folder)

groups <- list()
results <- list()

for(group in unique(df_animals$group)){
  animals <- df_animals$new_code[df_animals$group == group]
  groups[[group]] <- obj[names(obj) %in% animals]
  results[[group]] <- data.frame()
  class(groups[[group]]) <- append("avoidance.multiple", class(groups[[group]]))
}

## Probability plots ------
prob <- create_cross_probability_matrix(obj)

ggplot(prob_long[prob$n_crosses < 10, ], aes(time, probability, color = factor(n_crosses))) +
  geom_line()

## Area presence ------
area_presence <- area_presence_matrix(obj)
presence <- area_presence_probability(area_presence)

ggplot(presence, aes(time, probability, color = area)) + geom_line()
ggplot(presence[presence$area == "right", ], aes(time, probability)) + geom_line() + theme_minimal()

prob_group <- data.frame()
for(group in names(groups)){
  area_presence <- area_presence_matrix(groups[[group]])
  prob <- area_presence_probability(area_presence)
  prob$group <- group
  prob_group <- rbind(prob_group, prob)
}

ggplot(prob_group, aes(time, probability, color = factor(area))) +
  geom_line(size = 1.25) + theme_minimal() + geom_smooth() + facet_wrap(~group) +
  labs(color = "Area", 
       x = "Experiment time",
       y = "Ratio of animals present in the area")

ggplot(prob_group[prob_group$area == "left", ], aes(time, probability, color = group)) + geom_line() + facet_wrap(~group)

library(fgu.avoidance)
source("functions/loading.R")
data_folder <- file.path("data", "")

all_obj <- load_data(data_folder)

for(session_name in c("hab1", "hab2", "hab3")){
  rmarkdown::render("report.Rmd", params=list(session_name=session_name,
                                              data=all_obj[[session_name]]),
                    output_dir = "docs/", output_file = session_name)
}

for(session_name in c("trial0", "trial1")){
  rmarkdown::render("report-conditions.Rmd", params=list(session_name=session_name,
                                                         data=all_obj[[session_name]]),
                    output_dir = "docs/", output_file = session_name)
}

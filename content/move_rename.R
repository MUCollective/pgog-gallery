
library(tidyverse)


base_dir <- "./Probabilistic visualizations/"

f_rename <- function(f){
    if (f == "") {
    return()
  }
  
  f_md = paste0(base_dir, f, ".md") 
  f_dir = paste0(base_dir, f, "/", "index", ".md")
  
  if (file.exists(f_md)){
    file.rename(f_md, f_dir)
  }
}


files_folders <- 
  list.dirs("./Probabilistic visualizations/", full.names = FALSE) 

sapply(files_folders, f_rename)


all_files <- list.files("./Probabilistic visualizations/", recursive = TRUE)


rename_to_from <- all_files %>%
  enframe(name = "dir", value = "from") %>%
  # excludes all *.md files
  filter(str_detect(from, "^[^.]+$|\\.(?!(md|Rmd)$)([^.]+$)") == TRUE) %>%
  separate(from, c("dir", "image"), sep = "/", remove = FALSE) 
  
 
if (length(unique(rename_to_from$image)) == 1){
  stop("Processed")
}
  
  
rename_to_from <-  rename_to_from %>%
  mutate(image = "cover.png") %>%
  unite(to, dir:image, sep = "/", remove = FALSE) %>%
  mutate_at(vars(to, from), ~ paste0(base_dir, .))

rename_to_from %>%
  walk2(
    .x = .$from, 
    .y = .$to, 
    .f = ~file.rename(.x, .y)
    )
  

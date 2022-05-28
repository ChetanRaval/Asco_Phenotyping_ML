# load functions from Ido's gist
devtools::source_gist("7f63547158ecdbacf31b54a58af0d1cc", filename = "util.R")

# install/load packages in one-liner
library(pacman)
# load/install from CRAN/BioConductor
p_load(tidyverse, janitor, ggstatsplot, ggside)

# load/install from GitHub repos
p_load_gh(char = c("Mikata-Project/ggthemr"))
 

# Read processed image data ####
results_files <- list.files("output", pattern = "._Phenotyping_processed_data.csv",
                            full.names = TRUE)

processed_data <- map_dfr(results_files, ~ read_csv(.x) %>% mutate(date=as.Date(date))) %>% 
  clean_names() %>% mutate(across(c("pot", "plant", "image_num"), as.integer))
summarised_data <- processed_data %>% group_by(bioassay, pot) %>% 
  summarise(across(where(is.double), ~mean(.x, na.rm=TRUE)),
            plant_num = n())

# Assess correlation between visual and computed LAD #####
# plot data
pale_theme <- ggthemr(palette = "pale", set_theme = FALSE, text_size = 16)
# ggplot(processed_data, aes(x=symptomatic_mean , y=lad_percent))
ggscatterstats(
  data = summarised_data,
  x = symptomatic_mean,
  y = lad_percent,
  bf.message = FALSE
)

ggscatterstats(
  data = summarised_data,
  x = symptomatic_mean,
  y = lad_percent,
  bf.message = FALSE
)

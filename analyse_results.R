# load functions from Ido's gist
devtools::source_gist("7f63547158ecdbacf31b54a58af0d1cc", filename = "util.R")

# install/load packages in one-liner
library(pacman)
# load/install from CRAN/BioConductor
p_load(tidyverse, janitor, ggstatsplot, ggside, GGally, ggcorrplot, scales)

# load/install from GitHub repos
p_load_gh(char = c("Mikata-Project/ggthemr"))
 

# Read processed image data ####
results_files <- list.files("output", pattern = "._Phenotyping_processed_data.csv",
                            full.names = TRUE)

processed_data <- map_dfr(results_files, ~ read_csv(.x) %>% mutate(date=as.Date(date))) %>% 
  clean_names() %>% mutate(across(c("pot", "plant", "image_num"), as.integer)) %>% 
  write_xlsx(., glue::glue("output/{paste(unique(processed_data$bioassay), collapse = '_')}_processed_data.xlsx"), sheet = glue::glue("Pot{paste(min(processed_data$pot), max(processed_data$pot), sep = '-')}"), overwritesheet = TRUE)
summarised_data <- processed_data %>% group_by(bioassay, pot) %>% 
  summarise(symptomatic_SD=sd(symptomatic_mean, na.rm=TRUE),
            symptomatic_mean=mean(symptomatic_mean, na.rm=TRUE), 
            lad_percent_mean=mean(lad_percent, na.rm=TRUE), 
            lad_percent_SD=sd(lad_percent, na.rm=TRUE),
            leaf_score_mean=mean(leaf_score, na.rm=TRUE), 
            leaf_score_SD=sd(leaf_score, na.rm=TRUE),
            stem_score_mean=mean(stem_score, na.rm=TRUE), 
            stem_score_SD=sd(stem_score, na.rm=TRUE),
            plant_num = n(),
            image_num = sum(image_num)) %>% 
  relocate(symptomatic_SD, .after = symptomatic_mean) %>% 
  arrange(bioassay, pot) %>% 
  write_xlsx(., glue::glue("output/{paste(unique(processed_data$bioassay), collapse = '_')}_processed_data.xlsx"), sheet = glue::glue("Pot{paste(min(processed_data$pot), max(processed_data$pot), sep = '-')}_summary"), overwritesheet = TRUE)

# Assess correlation between visual and computed LAD #####
# Check correlation between all traits 
cor_data <- summarised_data %>% select(symptomatic_mean, lad_percent_mean, 
                                       leaf_score_mean, stem_score_mean) %>% 
  setNames(snakecase::to_any_case(names(.), case = "title", abbreviations = "LAD"))
ggpairs(cor_data) #+ plot_theme(def_theme = "grey", baseSize = 14) 
ggsave(filedate(glue::glue("output/{paste(unique(processed_data$bioassay), collapse = '_')}_pairwise_correlation"), ext = ".pdf", outdir = "output"), 
       width = 7, height = 7)
# plot correlation matrix
ggcorrmat(
  data = cor_data,
  type = "parametric", # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("darkred", "white", "steelblue") # change default colors
)
ggsave(filedate(glue::glue("output/{paste(unique(processed_data$bioassay), collapse = '_')}_correlation_mat"), ext = ".pdf", outdir = "output"), 
       width = 6, height = 5)
# plot summarised data
pale_theme <- ggthemr(palette = "pale", set_theme = FALSE, text_size = 14)
# ggplot(processed_data, aes(x=symptomatic_mean , y=lad_percent))
ggscatterstats(
  data = summarised_data,
  x = symptomatic_mean,
  y = lad_percent_mean,
  bf.message = FALSE
) + pale_theme$theme +
  # geom_errorbar(mapping = aes(ymin = lad_percent_mean - lad_percent_SD, 
  #                             ymax = lad_percent_mean + lad_percent_SD)) +
  # geom_errorbarh(mapping = aes(xmin = symptomatic_mean - symptomatic_SD, 
  #                             xmax = symptomatic_mean + symptomatic_SD)) +
  labs(y = "LAD (%)", x = "Symptomatic area (%)")
ggsave(filedate(glue::glue("{paste(unique(processed_data$bioassay), collapse = '_')}_image_LAD_correlation"), ext = ".pdf", outdir = "output"), 
       width = 9, height = 7)

# correlation between LAD and mean leaf score
ggscatterstats(
  data = summarised_data,
  x = leaf_score_mean,
  y = lad_percent_mean,
  bf.message = FALSE
) + pale_theme$theme +
  # geom_errorbar(mapping = aes(ymin = lad_percent_mean - lad_percent_SD, 
  #                             ymax = lad_percent_mean + lad_percent_SD)) +
  # geom_errorbarh(mapping = aes(xmin = symptomatic_mean - symptomatic_SD, 
  #                             xmax = symptomatic_mean + symptomatic_SD)) +
  labs(y = "LAD (%)", x = "Mean Leaf Score")
ggsave(filedate(glue::glue("{paste(unique(processed_data$bioassay), collapse = '_')}_leaf_LAD_correlation"), ext = ".pdf", outdir = "output"), 
       width = 9, height = 7)

# correlation between LAD and mean stem score
ggscatterstats(
  data = summarised_data,
  x = stem_score_mean,
  y = lad_percent_mean,
  bf.message = FALSE
) + pale_theme$theme +
  # geom_errorbar(mapping = aes(ymin = lad_percent_mean - lad_percent_SD, 
  #                             ymax = lad_percent_mean + lad_percent_SD)) +
  # geom_errorbarh(mapping = aes(xmin = symptomatic_mean - symptomatic_SD, 
  #                             xmax = symptomatic_mean + symptomatic_SD)) +
  labs(y = "LAD (%)", x = "Mean Stem Score")
ggsave(filedate(glue::glue("{paste(unique(processed_data$bioassay), collapse = '_')}_stem_LAD_correlation"), ext = ".pdf", outdir = "output"), 
       width = 9, height = 7)

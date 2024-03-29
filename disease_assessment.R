# TODO
# Read in phenotyping data from SharePoint and attach isolate and host information for each pot - done!
# implement gravity in avg_bgcolor
# combine duplicate values in phenotyping data - done!

# load functions from Ido's gist
devtools::source_gist("7f63547158ecdbacf31b54a58af0d1cc", filename = "util.R")

# install/load packages in one-liner
library(pacman)
# load/install from GitHub repos
p_load_gh(char = c("TiagoOlivoto/pliman", 
                   "DavisVaughan/furrr", 
                   "HenrikBengtsson/progressr"))
# load/install from CRAN/BioConductor
p_load(tidyverse, magick, tictoc, EBImage, Microsoft365R, janitor, ISOweek)
h <- image_import("./palette/h.png")
s <- image_import("./palette/s.png")
b <- image_import("./palette/b.JPEG")

plan(multisession, workers = future::availableCores()-2 ) # automatically detect available cores

# read phenotypic data and pivot (1 row per plant)
# read phenotyping data from SharePoint

# setup access to SharePoint
# after password change - clear cache and restart R session (or computer if needed)
# AzureAuth::clean_token_directory()
# AzureGraph::delete_graph_login(tenant="common")
options(microsoft365r_use_cli_app_id=TRUE)

# list_sharepoint_sites()
site <- get_sharepoint_site("GRDC A. rabiei Project GRI2007-001RTX") #,
# app="04b07795-8ddb-461a-bbee-02f9e1bf7b46" , auth_type="device_code"
#,tenant = "common")
# Microsoft365R::list_sharepoint_sites()

# app="04b07795-8ddb-461a-bbee-02f9e1bf7b46")


# app="04b07795-8ddb-461a-bbee-02f9e1bf7b46")
# default document library
drv <- site$get_drive()

# a drive has the same methods as for OneDrive above
# drv$list_items()
drv$download_file("Ascochyta rabiei phenotyping (blind design) (2).xlsx",
                  dest = "bioassay_test/Ascochyta_rabiei_phenotyping_form_data.xlsx",
                  overwrite = TRUE)


discard_cols <- c("ID", "Start time", "Completion time", "Email", "Name", "Comments", "Images")
phenotyping_data <- readxl::read_excel("bioassay_test/Ascochyta_rabiei_phenotyping_form_data.xlsx") %>% 
  mutate(date = as.Date(`Start time`), week = ISOweek(date)) %>% 
  select(-one_of(discard_cols)) %>% 
  pivot_longer(contains("Plant"), names_to = "Trait", values_to = "Score") %>% 
  mutate(plant = sub(pattern= "Plant ", "", x = str_extract(Trait, "Plant \\d")), 
         Trait = sub(pattern= "\\s*Plant \\d\\s*", "", Trait),
         Score = as.numeric(Score)) %>%
  clean_names() %>% 
  group_by(bioassay_id, date, pot_number, trait, plant) %>% 
  summarise(score = mean(score, na.rm = TRUE)) %>% 
  pivot_wider(names_from = trait, values_from = score) %>% 
  rename(bioassay = bioassay_id, pot=pot_number)


# PLIMAN ####
# test <- image_import("input_images/test4.JPG")
# image_combine(test, h, s, b)

source("src/functions.R")

# Tests ####

# run the function for 1 image (only cropping)
process_image_pliman(image_file = "../../Image Capture/20220429_Phenotyping/B001_POT13_PL1_00002.JPG", 
                     out_folder = "output/processed_images", 
                     assess_disease = FALSE,
                     trans = FALSE,
                     trim_bottom=230, trim_top=0,  # crop dimensions 
                     trim_left=0, trim_right=400,
                     h_pal = h, s_pal = s, b_pal = b)

# run the function for 1 image (only cropping, no saving file)
process_image_pliman(image_file = "./input_images/test4.JPG", 
                     out_folder = "output/processed_images", 
                     assess_disease = FALSE,
                     save_cropped = FALSE,
                     trans = FALSE,
                     h_pal = h, s_pal = s, b_pal = b)

# run the function for 1 image (cropping and assessing)
process_image_pliman(image_file = "./input_images/test4.JPG", 
                     out_folder = "output/processed_images", 
                     assess_disease = TRUE,
                     save_cropped = FALSE,
                     trans = FALSE,
                     h_pal = h, s_pal = s, b_pal = b)

# run the function for 1 image (just removing background)
process_image_pliman(image_file = "./input_images/test4.JPG", 
                     out_folder = "output/processed_images", 
                     assess_disease = FALSE,
                     crop = FALSE, 
                     save_cropped = FALSE,
                     trans = TRUE,
                     h_pal = h, s_pal = s, b_pal = b)

# run the function for 1 image (trans and assessing)
process_image_pliman(image_file = "./input_images/test4.JPG", 
                     out_folder = "output/processed_images", 
                     assess_disease = TRUE,
                     crop = FALSE, 
                     save_cropped = FALSE,
                     trans = TRUE,
                     h_pal = h, s_pal = s, b_pal = b)

# run the function for 1 image (removing background and cropping)
process_image_pliman(image_file = "./input_images/test4.JPG", 
                     out_folder = "output/processed_images", 
                     assess_disease = FALSE,
                     crop = TRUE, 
                     save_cropped = FALSE, # should save the cropped file even if FALSE!
                     trans = TRUE,
                     h_pal = h, s_pal = s, b_pal = b)

# run the function for 1 image (removing background, cropping and assessing)
process_image_pliman(image_file = "./input_images/test4.JPG", 
                     out_folder = "output/processed_images", 
                     assess_disease = TRUE,
                     crop = TRUE, 
                     save_cropped = FALSE, # should save the cropped file even if FALSE!
                     trans = TRUE,
                     h_pal = h, s_pal = s, b_pal = b)

# run the function for 1 image (only assessing)
process_image_pliman(image_file = "./input_images/test4.JPG", 
                     out_folder = "output/processed_images", 
                     assess_disease = TRUE,
                     crop = FALSE, 
                     save_cropped = FALSE, # should save the cropped file even if FALSE!
                     trans = FALSE,
                     h_pal = h, s_pal = s, b_pal = b)




# run the function for 5 images in folder

# test crop area and transparent bg ####

# crop only on one image
process_image_pliman(image_file = "../../Image Capture/20220429_Phenotyping/B001_POT13_PL1_00002.JPG",
                      out_folder = "output/processed_images",
                      assess_disease = FALSE,
                      trans = FALSE,
                      trim_bottom=230, trim_top=0,  # crop dimensions
                      trim_left=0, trim_right=400,
                      h_pal = h, s_pal = s, b_pal = b)
# find average colour (from NE corner this time)
avg_bgcolor("./output/processed_images/B001_POT13_PL1_00002_cropped.jpg", "200x200+5300+100" ) #DAD1C8

# just removing bg for 1 image

process_image_pliman(image_file = "./output/processed_images/B001_POT13_PL1_00002_cropped.jpg", 
                     out_folder = "output/processed_images", 
                     assess_disease = FALSE,
                     crop = FALSE, 
                     save_cropped = FALSE,
                     trans = TRUE,
                     set_fuzz = 18,
                     reference = "#DAD1C8",
                     start_point = "+5400+3500",
                     h_pal = h, s_pal = s, b_pal = b)

bioassay_test <- list.files("../../Image Capture/20220429_Phenotyping", "B001_POT13_.+.JPG", full.names = TRUE)

bioassay_files <- tibble(original_file = list.files("../../Image Capture/20220429_Phenotyping", "B001_POT.+.JPG", full.names = TRUE)) %>% 
  mutate(meta_string=tools::file_path_sans_ext(basename(original_file))) %>% 
  separate(meta_string, into = c("bioassay", "pot", "plant", "replicate")) %>% 
  mutate(pot=as.integer(sub("POT", "", pot, ignore.case = TRUE)), plant=sub("PL", "", plant, ignore.case = TRUE))

# PROCESS THESE! #####
# subset a range of pots (figure our cropping region);10
bioassay_test <- bioassay_files %>% filter(pot<60) %>% .$original_file
# for this set the crop parameters are: trim_bottom=230, trim_top=0,  trim_left=0, trim_right=400

step_size <- 20

steps <- seq(1,nrow(bioassay_files), by = step_size)
combined_disease_assessment_results <- vector("list", length = length(steps))
for(i in steps){
  # i =421 
  
  next_step <- min(c(i+step_size-1, nrow(bioassay_files)))
  LogMsg(glue::glue("Processing images {i} to {next_step}, please be patient..."))
   # %>% process_image_pliman()
  tic() # start timer
  with_progress({
    p <- progressor(steps = length(bioassay_files$original_file[i:next_step])) # 
    combined_disease_assessment_results[[which(steps==i)]] <- bioassay_files$original_file[i:next_step] %>% 
      future_map_dfr(.f = ~{
        res_tibble <- process_image_pliman(image_file = .x, 
                                           out_folder = "../../Image Capture/20220429_Phenotyping/processed_images", 
                                           assess_disease = TRUE,
                                           crop = TRUE, 
                                           trim_bottom=350, trim_top=0,
                                           trim_left=0, trim_right=230,
                                           save_cropped = TRUE,
                                           trans = TRUE,
                                           set_fuzz = 18,
                                           reference="#DAD1C8",
                                           start_point = "+5400+3500",
                                           h_pal = h, s_pal = s, b_pal = b)
        p() # because we put this last in the future_map_dfr, the function returned the progressor step instead of the tibble!
        return(res_tibble) # this should fix it...
      }, .options = furrr_options(seed = TRUE)) # this removes the annoying warning about the seed (though I don't think we're generating any random numbers)
  })
  toc() 
  
}
# combine results

  # process disease assessment results ####
combined_results_df <- map_dfr(combined_disease_assessment_results, bind_rows) %>% # check results and extract metadata from filename
  mutate(meta_string=tools::file_path_sans_ext(basename(original_file))) %>% 
  separate(meta_string, into = c("bioassay", "pot", "plant", "replicate")) %>% 
  mutate(pot=sub("POT", "", pot, ignore.case = TRUE), plant=sub("PL", "", plant, ignore.case = TRUE)) %>% 
  group_by(bioassay, pot, plant) %>% 
  summarise(symptomatic_mean = mean(symptomatic, na.rm = TRUE), 
            assessment_SD=sd(symptomatic, na.rm = TRUE), 
            image_num=n()) %>% 
  left_join(phenotyping_data) %>% relocate(date, .before = 1) %>% 
  write_csv("output/0220429_Phenotyping_processed_data.csv")


tic() # start timer
with_progress({
  p <- progressor(steps = length(bioassay_test)) # 
  disease_assessment_table <- bioassay_test %>% 
    future_map_dfr(.f = ~{
      p()
      res_tibble <- process_image_pliman(image_file = .x, 
                                         out_folder = "../../Image Capture/20220429_Phenotyping/processed_images", 
                                         assess_disease = TRUE,
                                         crop = TRUE, 
                                         trim_bottom=350, trim_top=0,
                                         trim_left=0, trim_right=230,
                                         save_cropped = TRUE,
                                         trans = TRUE,
                                         set_fuzz = 18,
                                         reference="#DAD1C8",
                                         start_point = "+5400+3500",
                                         h_pal = h, s_pal = s, b_pal = b)
      # because we put this last in the future_map_dfr, the function returned the progressor step instead of the tibble!
      return(res_tibble) # this should fix it...
    }, .options = furrr_options(seed = TRUE)) # this removes the annoying warning about the seed (though I don't think we're generating any random numbers)
})
toc() # 248.672 sec elapsed
# run the same for only cropped files

# cropped_files <- list.files("../../Image Capture/20220429_Phenotyping/processed_images", "B001_POT13_.+_cropped.jpg", full.names = TRUE)
# 
# tic() # start timer
# with_progress({
#   p <- progressor(steps = length(cropped_files)) # 
#   disease_assessment_cropped <- cropped_files %>% 
#     future_map_dfr(.f = ~{
#       p()
#       process_image_pliman(image_file = .x, 
#                                          out_folder = "../../Image Capture/20220429_Phenotyping/processed_images", 
#                                          assess_disease = TRUE,
#                                          crop = FALSE, 
#                                          trim_bottom=230, trim_top=0,
#                                          trim_left=0, trim_right=400,
#                                          save_cropped = TRUE,
#                                          trans = FALSE,
#                                          set_fuzz = 18,
#                                          reference="#DAD1C8",
#                                          start_point = "+5400+3500",
#                                          h_pal = h, s_pal = s, b_pal = b)
#       # because we put this last in the future_map_dfr, the function returned the progressor step instead of the tibble!
#       # return(cropped_res_tibble) # this should fix it...
#     }, .options = furrr_options(seed = TRUE)) # this removes the annoying warning about the seed (though I don't think we're generating any random numbers)
# })
# toc()

# process disease assessment results ####
image_disease_data <- disease_assessment_table %>% # check results and extract metadata from filename
  mutate(meta_string=tools::file_path_sans_ext(basename(original_file))) %>% 
  separate(meta_string, into = c("bioassay", "pot", "plant", "replicate")) %>% 
  mutate(pot=sub("POT", "", pot, ignore.case = TRUE), plant=sub("PL", "", plant, ignore.case = TRUE)) %>% 
  group_by(bioassay, pot, plant) %>% 
  summarise(symptomatic_mean = mean(symptomatic, na.rm = TRUE), assessment_SD=sd(symptomatic, na.rm = TRUE), 
            image_num=n()) %>% 
  left_join(phenotyping_data ) %>% relocate(date, week, .before = 1) %>% 
  write_csv("output/B001_POT13-59_data.csv")


# PROCESS POTS 60-100 ####
# subset a range of pots (60-100, figure out cropping region)
bioassay_test_2 <- bioassay_files %>% filter(between(pot, 60, 100)) %>% .$original_file
# for this set the crop parameters are: trim_bottom=230, trim_top=0,  trim_left=0, trim_right=400
# TEST FUZZ
process_image_pliman(image_file = bioassay_test_2[2], 
                     out_folder = "../../Image Capture/20220429_Phenotyping/processed_images", 
                     assess_disease = FALSE,
                     crop = TRUE, 
                     trim_bottom=300, trim_top=100,
                     trim_left=155, trim_right=270,
                     save_cropped = TRUE,
                     trans = TRUE,
                     set_fuzz = 17,
                     reference="#DAD1C8",
                     start_point = "+5400+3500",
                     h_pal = h, s_pal = s, b_pal = b)
# bioassay_test <- list.files("../../Image Capture/20220429_Phenotyping", "B001_POT13_.+.JPG", full.names = TRUE)

tic() # start timer
with_progress({
  p <- progressor(steps = length(bioassay_test_2)) # 
  disease_assessment_table <- bioassay_test_2 %>% 
    future_map_dfr(.f = ~{
      res_tibble <- process_image_pliman(image_file = .x, 
                                         out_folder = "../../Image Capture/20220429_Phenotyping/processed_images", 
                                         assess_disease = TRUE,
                                         crop = TRUE, 
                                         trim_bottom=300, trim_top=100,
                                         trim_left=155, trim_right=270,
                                         save_cropped = TRUE,
                                         trans = TRUE,
                                         set_fuzz = 15,
                                         reference="#DAD1C8",
                                         start_point = "+5400+3500",
                                         h_pal = h, s_pal = s, b_pal = b)
      p() # because we put this last in the future_map_dfr, the function returned the progressor step instead of the tibble!
      return(res_tibble) # this should fix it...
    }, .options = furrr_options(seed = TRUE)) # this removes the annoying warning about the seed (though I don'tthink we're generating any random numbers)
})
toc()

# process disease assessment results ####
image_disease_data <- disease_assessment_table %>% # check results and extract metadata from filename
  mutate(meta_string=tools::file_path_sans_ext(basename(original_file))) %>% 
  separate(meta_string, into = c("bioassay", "pot", "plant", "replicate")) %>% 
  mutate(pot=sub("POT", "", pot, ignore.case = TRUE), plant=sub("PL", "", plant, ignore.case = TRUE)) %>% 
  group_by(bioassay, pot, plant) %>% 
  summarise(symptomatic_mean = mean(symptomatic, na.rm = TRUE), assessment_SD=sd(symptomatic, na.rm = TRUE), 
            image_num=n()) %>% 
  left_join(phenotyping_data ) %>% relocate(date, week, .before = 1) %>% 
  write_csv("output/B001_POT60-100_data.csv")

# crop, transparent and assess whole folder

bioassay_test <- list.files("../../Image Capture/20220429_Phenotyping", ".JPG", full.names = TRUE)

tic() # start timer
with_progress({
  p <- progressor(steps = length(bioassay_test)) # 
  disease_assessment_table <- bioassay_test %>% 
    future_map_dfr(.f = ~{
      res_tibble <- process_image_pliman(image_file = .x, 
                                         out_folder = "../../Image Capture/20220429_Phenotyping/processed_images", 
                                         assess_disease = TRUE,
                                         crop = TRUE, 
                                         trim_bottom=230, trim_top=0,
                                         trim_left=0, trim_right=400,
                                         save_cropped = TRUE,
                                         trans = TRUE,
                                         set_fuzz = 18,
                                         reference="#DAD1C8",
                                         start_point = "+5400+3500",
                                         h_pal = h, s_pal = s, b_pal = b)
      p() # because we put this last in the future_map_dfr, the function returned the progressor step instead of the tibble!
      return(res_tibble) # this should fix it...
    }, .options = furrr_options(seed = TRUE)) # this removes the annoying warning about the seed (though I don'tthink we're generating any random numbers)
})
toc()




# process disease assessment results ####
image_disease_data <- disease_assessment_table %>% # check results and extract metadata from filename
  mutate(meta_string=tools::file_path_sans_ext(basename(original_file))) %>% 
  separate(meta_string, into = c("bioassay", "pot", "plant", "replicate")) %>% 
  mutate(pot=sub("POT", "", pot, ignore.case = TRUE), plant=sub("PL", "", plant, ignore.case = TRUE)) %>% 
  group_by(bioassay, pot, plant) %>% 
  summarise(symptomatic_mean = mean(symptomatic, na.rm = TRUE), assessment_SD=sd(symptomatic, na.rm = TRUE), 
            image_num=n()) %>% 
  left_join(phenotyping_data ) %>% relocate(date, week, .before = 1) %>% 
  write_csv("output/B001_POT60-100_data.csv")

# 73 sec

# measure disease assessment when run serially (quicker in parallel for a large number of images)
# if we expect the output as a dataframe/tibble built row-by-row we need to use map_dfr()
# if there's no expected output we need to use walk() - I changed it now so that we always get a tibble output, even if not assessing
# tic()
# disease_assessment_table <- image_files %>% map_dfr(.f = ~process_image_pliman(image_file = .x,
#                                                                                out_folder = "output/processed_images",
#                                                                                assess_disease = TRUE,
#                                                                                h_pal = h, s_pal = s, b_pal = b))
# toc()
# 132 sec

# trim images based on drawn bounding box

test <- pliman::image_import("input_images/test_rect.jpg")
autocrop_test <- pliman::analyze_objects(test, index = "B", ) # used image_binary() to discover "index" argument
plot_measures(autocrop_test, measure = 'id', col = "black")
measures <- get_measures(autocrop_test, dpi = 350)
max(measures$perimeter)



# blue border image crop dimensions

trim_top = 450
trim_right = 470
trim_bottom = 725
trim_left = 480

# 20220524_Phenotyping image crop dimensions

process_image_pliman(image_file = "../../Image Capture/20220524_Phenotyping/B002_POT1_PL1_00001.JPG",
                     out_folder = "output/processed_images",
                     assess_disease = FALSE,
                     trans = FALSE,
                     trim_bottom=350, trim_top=273,
                     trim_left=245, trim_right=300,
                     h_pal = h, s_pal = s, b_pal = b)
# find average colour (from NE corner this time)
avg_bgcolor("./output/processed_images/B002_POT1_PL1_00001_cropped.jpg", "200x200+5300+100" ) #DAD1C8


bioassay_test <- list.files("../../Image Capture/20220524_Phenotyping/", ".JPG", full.names = TRUE)[1:20]

# test on one image

process_image_pliman(image_file = bioassay_test[1], 
                     out_folder = "../../Image Capture/20220524_Phenotyping/processed_images", 
                     assess_disease = FALSE,
                     crop = TRUE, 
                     trim_bottom=350, trim_top=273,
                     trim_left=245, trim_right=300,
                     save_cropped = TRUE,
                     trans = TRUE,
                     set_fuzz = 35,
                     reference="#DAD1C8",
                     start_point = "+5400+3500",
                     h_pal = h, s_pal = s, b_pal = b)

tic() # start timer
with_progress({
  p <- progressor(steps = length(bioassay_test)) # 
  disease_assessment_table <- bioassay_test %>% 
    future_map_dfr(.f = ~{
      p()
      res_tibble <- process_image_pliman(image_file = .x, 
                                         out_folder = "../../Image Capture/20220524_Phenotyping/processed_images", 
                                         assess_disease = TRUE,
                                         crop = TRUE, 
                                         trim_bottom=350, trim_top=273,
                                         trim_left=245, trim_right=300,
                                         save_cropped = TRUE,
                                         trans = TRUE,
                                         set_fuzz = 18,
                                         reference="#D7CEC5",
                                         start_point = "+5400+3500",
                                         h_pal = h, s_pal = s, b_pal = b)
       # because we put this last in the future_map_dfr, the function returned the progressor step instead of the tibble!
      return(res_tibble) # this should fix it...
    }, .options = furrr_options(seed = TRUE)) # this removes the annoying warning about the seed (though I don'tthink we're generating any random numbers)
})
toc()






# Process an entire folder of images #####
# make sure to test the fuzz and crop area parameters on a few files before! 








# create transparent image ####

#use list.files to import all images from input_images/cropped
#use map() function to import all images into a list
#pass that object into input_folder or input_images

create_trans_img <- function(input_img_file, output_folder="input_images/trans", 
                             bg_color="transparent", 
                             reference="#F7F4EF", 
                             set_fuzz=30, 
                             # trans=TRUE,
                             start_point="+20+20"){
  # read image from file and strip its directory and extension
  input_img <- image_read(input_img_file)
  image_base <- tools::file_path_sans_ext(basename(input_img_file))
  
  # if (isTRUE(trans)) {
  if(!dir.exists(output_folder)) dir.create(output_folder)
  
  # }
  trans_image <- magick::image_fill(input_img, 
                            color = bg_color,
                            refcolor = reference,
                            fuzz = set_fuzz,
                            point = start_point) %>% 
    image_write(file.path(output_folder, paste0(image_base, "_transparent.png")),
                format = 'png') # this is where you had the error, you need to paste the output_folder to the file name with a / separator (or use file.path function to do it for you like I've done)
  # return(trans_image)
    
}

#run function for 1 image
create_trans_img("palette/test4.JPG", output_folder = "input_images/trans")

magick_images <- list.files("input_images", "_cropped.jpg", full.names = TRUE)[1:10]

# measure the time when run serially
tic()
magick_images %>% walk(.f = ~create_trans_img(input_img_file = .x, 
                                              output_folder = "input_images/trans"))
toc() 

# measure the time in parallel (with furrr)
tic()
with_progress({

  p <- progressor(steps = length(magick_images))

  magick_images %>% future_walk(.f = ~{
    create_trans_img(input_img_file = .x,
                     output_folder = "input_images/trans")

  p()
  }, .options = furrr_options(seed = 123))

})
toc()



#test image_fill()
# test_image <- image_read("./palette/test4.JPG")
# trans_img <- image_fill(input_img, color = "transparent", refcolor = "#F7F4EF", fuzz = 30, 
#                         point = "+20+20") %>%
#   image_write("output/transparent_image.jpg")

# write results to file

write_csv(disease_assessment_table, "bioassay_test/disease_assessment_table.csv")

# object contour ####
leaf <- image_import("output/test4_cropped.jpg")
cont <- object_contour(leaf, watershed = TRUE, show_image = FALSE)

plot(leaf)
plot_contour(cont, col = "red", lwd = 3)

# MAGICK ####

crop_test <- image_read("input_images/test4.JPG")

# trim image to usable area (with magick)
trimmed_image <- magick::image_crop(crop_test, "5250x3625+400+0")



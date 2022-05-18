# TODO
# Read in phenotyping data from SharePoint and attach isolate and host information for each pot

# install/load packages in one-liner
library(pacman)
# load/install from GitHub repos
p_load_gh(char = c("TiagoOlivoto/pliman", 
                           "DavisVaughan/furrr", 
                           "HenrikBengtsson/progressr" ))
# load/install from CRAN/BioConductor
p_load(tidyverse, magick, tictoc, EBImage)
h <- image_import("./palette/h.png")
s <- image_import("./palette/s.png")
b <- image_import("./palette/b.JPEG")

plan(multisession, workers = future::availableCores()-1 ) # automatically detect available cores

# PLIMAN ####
test <- image_import("input_images/test4.JPG")
# image_combine(test, h, s, b)

# get avg bg color ####
# get average colour for transparent reference colour argument
avg_bgcolor <- function(image_file, ref_area="200x200+0"){
  if (!grepl("\\d+x\\d+(\\+\\d+){0,2}$", ref_area)) stop(sprintf("The string provided to avg_bgcolor(), '%s', in not a valid Magick crop geometry, see details in https://docs.ropensci.org/magick/reference/geometry.html", ref_area))
  # image_file="input_images/test4.JPG"
  sample <- image_read(image_file)
  crop <- magick::image_crop(sample, ref_area) 
  crop_array <- as.integer(crop[[1]]) # no need to write to disk and read again, it's very inefficient, see https://stackoverflow.com/a/46736582/5346827
  # dim(crop_array)
  red <- mean(crop_array[,,1]) # mean of all red colour channels
  green <- mean(crop_array[,,2]) # mean of all green colour channels
  blue <- mean(crop_array[,,3]) # mean of all blue colour channels
  
  return(rgb(red, green, blue, maxColorValue = 255))
  
}

# test function
avg_bgcolor("./input_images/T001_POT22_PL5_00002_cropped.jpg")

# pliman measure disease ####
process_image_pliman <- function(image_file, out_folder, 
                                 assess_disease=TRUE,
                                 trim_bottom=375, trim_top=0,  # crop dimensions 
                                 trim_left=400, trim_right=350,  # crop dimensions
                                 crop = TRUE, # crop image?
                                 save_cropped = TRUE, # save cropped image?
                                 trans = TRUE, # remove background? if yes we must save to a file!
                                 show=FALSE,   # show image?
                                 h_pal, s_pal, b_pal,
                                 bg_color="transparent", 
                                 reference="200x200+0", # a hex code or a valid Magick::Geometry string
                                 set_fuzz=30, 
                                 start_point="+20+20"){
  original_file <- image_file

  # image cropping
  if (isTRUE(crop)) {
    plant_image <- image_import(image_file)
    processed_image <- pliman::image_trim(image = plant_image,
                                        bottom = trim_bottom,
                                        top = trim_top,
                                        left = trim_left,
                                        right = trim_right,
                                        plot = show) 
    image_basename <- tools::file_path_sans_ext(basename(image_file))
    
    # save cropped file to file
    if (isTRUE(save_cropped) | isTRUE(trans)) {
      if (!dir.exists(out_folder)) dir.create(out_folder) # create output folder if not exists
      # Reset image filename to the cropped one if we cropped
      image_file <- file.path(out_folder, paste0(image_basename, "_cropped.jpg"))
      pliman::image_export(processed_image, image_file)
    }
      
       
    
  }
  if (isTRUE(trans)) {
    if (!dir.exists(out_folder)) dir.create(out_folder)
    image_base <- tools::file_path_sans_ext(basename(image_file))
    input_img <- image_read(image_file)
    # calculate reference background colour (if defined as region)
    if (!grepl("^#\\w{6}", reference)) {
    }  reference <- avg_bgcolor(image_file = image_file, ref_area = reference)
    # Reset image filename to the transparent one (must be png to be transparent!)
    image_file <- file.path(out_folder, paste0(image_base, "_transparent.png"))
    magick::image_fill(input_img, 
                       color = bg_color,
                       refcolor = reference,
                       fuzz = set_fuzz,
                       point = start_point) %>% 
      image_write(image_file, format = 'png') # (must be png to be transparent!)

  }  
  # save results as a row in a tibble
  results <- tibble(original_file=original_file, processed_file=ifelse(original_file==image_file, NA, image_file), 
                    cropped=crop, save_cropped = save_cropped,  remove_bg=trans, disease_assessed=assess_disease)
  
  if (!isTRUE(assess_disease)) return(results) # exit the function if disease assessment is not needed
  # pliman measure disease #
  if (!exists("processed_image")) processed_image <- image_import(image_file)
  disease_assessment <- measure_disease(
    img = processed_image,
    img_healthy = h_pal,
    img_symptoms = s_pal,
    img_background = b_pal,
    show_image = show
  )
  
  return(bind_cols(results, as_tibble(disease_assessment$severity)))

}

# Tests ####

# run the function for 1 image (only cropping)
process_image_pliman(image_file = "./input_images/test4.JPG", 
                     out_folder = "output/processed_images", 
                     assess_disease = FALSE,
                     trans = FALSE,
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

bioassay_test <- list.files("bioassay_test/test/", ".JPG", full.names = TRUE)

tic() # start timer
with_progress({
  p <- progressor(steps = length(bioassay_test)) # 
  disease_assessment_table <- bioassay_test %>% 
    future_map_dfr(.f = ~{
      res_tibble <- process_image_pliman(image_file = .x, 
                                         out_folder = "bioassay_test/output/", 
                                         assess_disease = TRUE,
                                         crop = TRUE, 
                                         trim_bottom=300, trim_top=0,
                                         trim_left=400, trim_right=275,
                                         save_cropped = TRUE,
                                         trans = TRUE,
                                         set_fuzz = 28,
                                         reference="200x200+0",
                                         h_pal = h, s_pal = s, b_pal = b)
      p() # because we put this last in the future_map_dfr, the function returned the progressor step instead of the tibble!
      return(res_tibble) # this should fix it...
    }, .options = furrr_options(seed = TRUE)) # this removes the annoying warning about the seed (though I don'tthink we're generating any random numbers)
})
toc()

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

# process disease assessment results ####
disease_assessment_table %>% # check results and extract metadata from filename
  mutate(meta_string=tools::file_path_sans_ext(basename(original_file))) %>% 
  separate(meta_string, into = c("bioassay", "pot", "plant", "replicate")) %>% 
  mutate(pot=as.integer(sub("POT", "", pot, ignore.case = TRUE)), plant=as.integer(sub("PL", "", plant, ignore.case = TRUE)))




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



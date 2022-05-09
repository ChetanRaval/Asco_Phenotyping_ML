# install/load packages in one-liner
library(pacman)
# load/install from GitHub repos
p_load_current_gh(char = c("TiagoOlivoto/pliman", 
                           "DavisVaughan/furrr", 
                           "HenrikBengtsson/progressr" ))
# load/install from CRAN/BioConductor
p_load(tidyverse, magick, tictoc)
h <- image_import("./palette/h.png")
s <- image_import("./palette/s.png")
b <- image_import("./palette/b.JPEG")

plan(multisession, workers = 6)

# PLIMAN ####
test <- image_import("./palette/test4.JPG")
# image_combine(test, h, s, b)

# pliman measure disease ####
process_image_pliman <- function(image_file, out_folder, 
                                 trim_bottom=375, trim_top=0,  # crop dimensions 
                                 trim_left=400, trim_right=350,  # crop dimensions
                                 crop = TRUE, # crop image?
                                 trans = TRUE, # remove background?
                                 show=FALSE,   # show image?
                                 h_pal, s_pal, b_pal){
  plant_image <- image_import(image_file)
  cropped_image <- plant_image
  if (isTRUE(crop)) {
    if(!dir.exists(out_folder)) dir.create(out_folder) # create output folder if not exists
    cropped_image <- pliman::image_trim(image = plant_image,
                                        bottom = trim_bottom,
                                        top = trim_top,
                                        left = trim_left,
                                        right = trim_right,
                                        plot = show) 
    image_basename <- tools::file_path_sans_ext(basename(image_file))
    pliman::image_export(cropped_image, 
                         file.path(out_folder, paste0(image_basename, "_cropped.jpg")))
  }
  # pliman measure disease #
  disease_assessment <- measure_disease(
    img = cropped_image,
    img_healthy = h_pal,
    img_symptoms = s_pal,
    img_background = b_pal,
    show_image = show
  )
  
  
  return(as_tibble(disease_assessment$severity) %>% mutate(filename=basename(image_file)) %>% 
           relocate(filename, .before = "healthy"))
  
  
}

# run the function for 1 image
process_image_pliman(image_file = "./palette/test4.JPG", 
                     out_folder = "input_images/cropped)", 
                     h_pal = h, s_pal = s, b_pal = b)

# run the function for all images in folder (make it parallel and with progress bar with furrr)

image_files <- list.files("input_images/", ".jpg", full.names = TRUE)
with_progress({
  p <- progressor(steps = 5) # length(image_files)
  
  disease_assessment_table <- image_files[1:5] %>% 
    future_map_dfr(.f = ~{
      process_image_pliman(image_file = .x, out_folder = "input_images/cropped",
                           crop = TRUE, h_pal = h, s_pal = s, b_pal = b)
      p()
    })
})



#create transparent image ####

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

write_csv(disease_assessment_table, "input_images/disease_assessment_table.csv")


leaf <- image_import("output/test4cropped.jpg")
cont <- object_contour(leaf, watershed = TRUE, show_image = FALSE)

plot(leaf)
plot_contour(cont, col = "red", lwd = 3)

# MAGICK ####

crop_test <- image_read("./palette/test4.JPG")

# trim image to usable area
trimmed_image <- image_crop(crop_test, "5250x3625+400+0")

# pliman measure disease ####

measure_disease(
  img = trimmed_image,
  img_healthy = h,
  img_symptoms = s,
  img_background = b,
  show_image = TRUE
)

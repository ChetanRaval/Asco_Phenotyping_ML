library(pliman)
library(magick)
library(tidyverse)

h <- image_import("./palette/h.png")
s <- image_import("./palette/s.png")
b <- image_import("./palette/b.JPEG")



#PLIMAN ####
test <- image_import("./palette/test4.JPG")
# image_combine(test, h, s, b)
process_image_pliman <- function(image_file, trim_bottom=375, trim_top=0, trim_left=400, trim_right=350, 
                                 show=FALSE, h_pal, s_pal, b_pal){
  plant_image <- image_import(image_file)
  cropped_image <- pliman::image_trim(image = plant_image,
                                     bottom = trim_bottom,
                                     top = trim_top,
                                     left = trim_left,
                                     right = trim_right,
                                     plot = show) 
  image_basename <- tools::file_path_sans_ext(basename(image_file))
  pliman::image_export(cropped_image, file.path("output", paste0(image_basename, "_cropped.jpg")))
  
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
process_image_pliman(image_file = "./palette/test4.JPG", h_pal = h, s_pal = s, b_pal = b)

# run the function for all images in folder (make it parallel and with progress bar with furrr)
disease_assessment_table <- list.files("input_images/", ".JPG", full.names = TRUE) %>% 
  map_dfr(.f = ~process_image_pliman(image_file = .x, h_pal = h, s_pal = s, b_pal = b))

# MAGICK ####

crop_test <- image_read("./palette/test4.JPG")

# trim image to usable area
trimmed_image <- image_crop(crop_test, "5250x3625+400+0")

#remove white background
trans_img <- image_transparent(trimmed_image, "#F4F1EC", 50) %>% image_write("output/test4_cropped_trans.jpg")

# pliman measure disease ####

measure_disease(
  img = trimmed_image,
  img_healthy = h,
  img_symptoms = s,
  img_background = b,
  show_image = TRUE
)

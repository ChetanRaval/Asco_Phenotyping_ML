library(pliman)
library(magick)
library(tidyverse)
library(furrr)
library(progressr)
library(EBImage)

h <- image_import("./palette/h.png")
s <- image_import("./palette/s.png")
b <- image_import("./palette/b.JPEG")

plan(multisession, workers = 6)

#PLIMAN####
test <- image_import("./palette/test4.JPG")
# image_combine(test, h, s, b)

process_image_pliman <- function(image_file, out_folder, trim_bottom=375, trim_top=0, trim_left=400, 
                                 trim_right=350,
                                 crop = TRUE,
                                 show=FALSE, h_pal, s_pal, b_pal){
  plant_image <- image_import(image_file)
  cropped_image <- plant_image
  if (isTRUE(crop)) {
    if(!dir.exists(out_folder)) dir.create(out_folder)
    cropped_image <- pliman::image_trim(image = plant_image,
                                        bottom = trim_bottom,
                                        top = trim_top,
                                        left = trim_left,
                                        right = trim_right,
                                        plot = show) 
    image_basename <- tools::file_path_sans_ext(basename(image_file))
    pliman::image_export(cropped_image, file.path(out_folder, paste0(image_basename, "_cropped.jpg")))
    
    
    
  }
  
  
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


create_trans_img <- function(input_img, output_folder, bg_color="transparent", 
                             reference="#F7F4EF", 
                             set_fuzz=25, 
                             start_point="+20+20"){
  
  transparent_img <- image_read(input_img)
  trans_img <- transparent_img
  
  
  
  trans_image <- magick::image_fill(transparent_img, 
                            color = bg_color,
                            refcolor = reference,
                            fuzz = set_fuzz,
                            point = start_point)
    
    image_base <- tools::file_path_sans_ext(basename(input_img))
    pliman::image_export(trans_img, file.path(output_folder, paste0(image_base, "_transparent.jpg")))
    #EBImage::writeImage(trans_img, file.path(output_folder, paste0(image_base, "_transparent.jpg")))
    #EBImage::writeImage(trans_image, output_folder)
    
}

#run function for 1 image
create_trans_img("palette/test4.JPG", output_folder = "output")

magick_images <- list.files("input_images/cropped/", ".jpg", full.names = TRUE)
with_progress({
  
  p <- progressor(steps = 5)
  
  create_trans_img(input_img = , output_folder = "output")
  
  p()
  
})


#test image_fill()
test_image <- image_read("./palette/test4.JPG")
trans_img <- image_fill(test_image, color = "transparent", refcolor = "#F7F4EF", fuzz = 25, 
                        point = "+20+20") %>%
  image_write("output/")

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

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
# avg_bgcolor("./output/processed_images/B001_POT13_PL1_00002_cropped.jpg", "200x200+5300+100" )

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
                                 reference="200x200+3000+100", # a hex code or a valid Magick::Geometry string
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
    if (!grepl("^#\\w{6}", reference)) reference <- avg_bgcolor(image_file = image_file, ref_area = reference)
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
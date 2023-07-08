#function to find images and set paths
image_gallery <- function() {
  #find the images
  images <- list.files(paste0(shiny::resourcePaths()[["extr"]], '/img/', the$language))
  #create the image path
  image_path <- paste0("extr/img/",the$language, "/", images)
  #remove the file extension
  images <- stringr::str_remove(images, ".png")
  #remove the numbering from image_names
  images <- stringr::str_remove(images, "[:digit:][:digit:]_")

  names <- list(images = images,
                image_path = image_path)
}
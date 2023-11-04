#function to find images and set paths
image_gallery <- function() {
  #find the images
  images <- list.files(system.file("app/www/img", package = "Spectran"))
  #create the image path
  image_path <- paste0("extr/img/", images)
  #fetch the image names
  images <- 180:188 %>% purrr::map_vec(lang$ui)

  names <- list(images = images,
                image_path = image_path)
}

#unused function to get rid of dependencies in the package-check-process
unused <- function() {
  chromote::get_chrome_args()
  magrittr::add()
  pagedown::chrome_print()
  pkgload::check_dep_version()
  spacesXYZ::adaptLab()
  webshot2::appshot()
  waiter::attendantBar()
}
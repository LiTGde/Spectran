## code to prepare `ColorP` dataset goes here
# library(cooltools)
#cooltools is no longer on cran

#Color palette
Rainbow <- c(rev(rainbow(150, start = 0.4, end = 0.77)),
                rev(rainbow(31, start = 0.20, end = 0.4)),
                rev(rainbow(145, start = 0, end = 0.20)),
                rev(rainbow(75, start = 0.96, end = 1)))

names(Rainbow) <- seq_along(Rainbow) +379

#Lang Colors
Lang <- read.csv("data-raw/Lang_colors.csv")
Lang <- Lang %>% tibble::deframe()
Lang_bright <- read.csv("data-raw/Lang_bright_colors.csv") %>% tibble::deframe()
Dan_Bruton <- read.csv("data-raw/Dan_Bruton.csv") %>% tibble::deframe()
# Dan_Bruton <- wavelength2col(380:780) <- removed due to cooltools no longer being on cran

#Color Rendering
Color_Rendering <- c(
  "#DEAAB1",
  "#C8B28A",
  "#AFC06E",
  "#84C3A1",
  "#8EC0DC",
  "#97B5F9",
  "#C6A7F9",
  "#E3A6EB",
  "#D63950",
  "#FDF472",
  "#4EA98C",
  "#245AB9",
  "#FBE9DC",
  "#6F7C54"
)

names(Color_Rendering) <- paste0("R", seq_along(Color_Rendering))

ColorP <- list(Rainbow = Rainbow,
               Lang = Lang,
               Lang_bright = Lang_bright,
               Dan_Bruton = Dan_Bruton,
               Color_Rendering = Color_Rendering)

# usethis::use_data(ColorP, overwrite = TRUE)

#Footnotes for plots

footnote <- function() {
  foottext <- paste0(lang$server(43),"**LiTG Spectran** (Zauner, 2023)")
  list(
  ggplot2::labs(caption = foottext),
  ggplot2::theme(plot.caption = ggtext::element_markdown())
  )
  }
#' Create hex colour
#'
#' @param mat matrix of rgb colours defining the palette
#'
#' @export
pal <- function(mat) {
  rgb(mat[,1], mat[,2], mat[,3], maxColorValue=255)
}

#' Gretchen Albrecht Colour Palettes
#'
#'A collection of colour palettes inspired by the paintings of NZ abstract expressionist Gretchen Albrecht:
#' rocker
#' black_plain
#' flood_tide
#' last_rays
#' red_sky_golden_cloud
#' winged_spill
#' pink_cloud
#' winter_light
#'
#'@examples
#'
#' # Print out the palettes available
#' gretchenalbrecht_palettes
#'
#' # Make an x-y plot using the rocker palette
#' library(tidyverse)
#' data(diamonds)
#' diamonds_small <- diamonds %>% sample_n(1000)
#' ggplot(diamonds_small,
#'   aes(x=carat, price, colour=cut, shape=cut)) +
#'   geom_point(size=4, alpha=0.5) +
#'   scale_colour_gretchenalbrecht(palette="rocker") +
#'   theme_bw() + theme(aspect.ratio=1)
#'
#' # Make a histogram using the pink_cloud palette
#' ggplot(diamonds_small, aes(x=price, fill=cut)) + geom_histogram() +
#'   scale_fill_gretchenalbrecht(palette="pink_cloud") + theme_bw()
#'
#' @export
gretchenalbrecht_palettes <- list(
  # 1975 acrylic on canvas
  rocker = pal(rbind(c(110,3,40), c(53,111,44), c(250,100,0), c(44,32,30), c(2,6,186), c(54,30,54), c(200,71,78))),
  black_plain = pal(rbind(c(0,82,50), c(40,30,30), c(164,38,94), c(253,190,0), c(255,240,0), c(160,0,17))),
  # 2016 watercolour
  flood_tide = pal(rbind(c(45,56,86), c(77,101,186), c(236,229,221), c(250,208,169), c(253,236,130))),
  last_rays = pal(rbind(c(60,60,60),c(120,150,180),c(230,230,220),c(250,170,114), c(190,92,85))),
  red_sky_golden_cloud = pal(rbind(c(0,40,25), c(68,111,4), c(0,91,205), c(242,200,210), c(255,150,0), c(255,43,0), c(77,50,150))),
  winged_spill = pal(rbind(c(90,60,75),c(232,112,75),c(115,100,133),c(253,238,98),c(98,133,138), c(188,68,82))),
  pink_cloud = pal(rbind(c(27,27,27),c(130,110,96),c(240,164,0),c(220,220,220),c(179,77,103))),
  winter_light = pal(rbind(c(218,212,60),c(242,238,195), c(146,38,31), c(68,33,86)))
)

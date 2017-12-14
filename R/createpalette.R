#' Get k colors from a named color palette 
#' 
#' @param K number of colors
#' @param palette name of a palette
#' @return dataframe with columns k (from 1 to K) and column col
#' @export
#' @examples 
#' ch <- get_pal(10)
#' ch %>% ggplot() + 
#'    geom_tile(aes(x=1, y = 1, fill=col)) + 
#'    scale_fill_identity() + facet_wrap(~k)
#'
#' # colours from a luminous shade:
#' ls <- get_pal(5, "luminous shade")
#' ls %>% ggplot() + 
#'    geom_tile(aes(x=1, y = 1, fill=col)) + 
#'    scale_fill_identity() + facet_wrap(~k)
#'    
#' # colours from rocker:
#' ro <- get_pal(6, "rocker")
#' ro %>% ggplot() + 
#'    geom_tile(aes(x=1, y = 1, fill=col)) + 
#'    scale_fill_identity() + facet_wrap(~k)
#' 
#' # colours from rosa concorde:
#' rc <- get_pal(6, "rosa concorde")
#' rc %>% ggplot() + 
#'    geom_tile(aes(x=1, y = 1, fill=col)) + 
#'    scale_fill_identity() + facet_wrap(~k)
#'
#'
#' ggplot(diamonds_small,
#'   aes(x=carat, price, colour=cut)) +
#'   geom_point(size=4) +
#'   scale_colour_manual(values = ro$col[-4]) +
#'   theme_bw() + theme(aspect.ratio=1)
#'
#' ggplot(diamonds_small,
#'   aes(x=carat, price, colour=cut)) +
#'   geom_point(size=4) +
#'   scale_colour_gretchenalbrecht("rocker") +
#'   theme_bw() + theme(aspect.ratio=1)
get_pal <- function(K, palette = "changes") {
  library(tidyverse)
  load("data/ga_pal_cont.rda")
  
  if (is.character(palette)) {
    # check that palette is a valid name
    clust <- ga_pal_cont[[palette]]$clust
    subframe <- ga_pal_cont[[palette]]$data
  }
  if (is.list(palette)) {
    # check that these sub-objects actually exist
    clust <- palette$clust
    subframe <- palette$data
  }
# get cluster object by name
subframe$k <- cutree(clust, k=K)
tiles <- subframe %>% group_by(k) %>% summarize(
  red=mean(red),
  blue=mean(blue),
  green=mean(green)
) %>% mutate(
  col = rgb(red/255, green/255, blue/255)
)

tiles
}


#' Create a palette of (almost) arbitrary size from given image
#' 
#' Up to `limit` colours are extracted from the image and clustered.
#' @param jpgfile name of the image from which the color palette is to be extracted
#' @param limit number of sample points to be extracted from the image
#' @return cluster object
#' @export
#' @examples 
#' rc <- make_palette("https://s3.amazonaws.com/img.aasd.com.au/05574461.jpg")
#' rc$name <- "rosa concorde"
#' 
#' gsw <- make_palette("https://s3.amazonaws.com/img.aasd.com.au/52775868.jpg")
#' gsw$name <- "grey sky winter"
#' 
#' get_pal(5, gsw) %>% ggplot() + 
#'    geom_tile(aes(x=1, y = 1, fill=col)) + 
#'    scale_fill_identity() + facet_wrap(~k)
#'    
#' gs <- make_palette("https://s3.amazonaws.com/img.aasd.com.au/52333415.jpg")
#' gs$name <- "golden shadow"
#' 
#' get_pal(10, gs) %>% ggplot() + 
#'    geom_tile(aes(x=1, y = 1, fill=col)) + 
#'    scale_fill_identity() + facet_wrap(~k)

#' # now add to internal data object to make it accessible for future use
make_palette <- function(jpgfile, limit=1000) {
  library(raster)
  library(rgdal)
  
  cat("# reading red, green, and blue bands ...\n")
  red <- raster(jpgfile, band=1)
  green <- raster(jpgfile, band=2)
  blue <- raster(jpgfile, band=3)
  #par(mfrow=c(3,1))
  #plot(red)
  #plot(green)
  #plot(blue)
  
  clframe <- data.frame(red=as.vector(red), blue=as.vector(blue), green=as.vector(green))
  dims <- dim(clframe) 
  factor <- floor(nrow(clframe)/limit) # 1,000 as upper boundary for colours might not be too strong a restriction
  subframe <- clframe
  if (factor > 1) 
    subframe <- clframe[factor*1:(nrow(clframe) %/% factor),] 
  
  cat("# determining distances between colours ...\n")
  dframe <- dist(subframe)
 
  cat("# create cluster object ...\n")
  list(clust = hclust(dframe), data = subframe)
}


#' Internal function for adding or updating a palette to the data file
#' 
#' Already existing palettes will be updated, new palettes will be added to the `ga_pal_cont` object in the right format.
#' @param jpegfile link to the file - use a local file if possible
#' @param palette name for the palette
#' @examples
#' gretchenalbrecht:::add_palette("pictures/gretchen-albrecht-changes.jpg", "changes")
#' gretchenalbrecht:::add_palette("pictures/gretchen-albrecht-luminous-shade-811.jpg", "luminous shade")
#' gretchenalbrecht:::add_palette("pictures/gretchen-albrecht-rocker.jpg", "rocker")
#' gretchenalbrecht:::add_palette("pictures/gretchen-albrecht-rocker.jpg", "rocker")
#' gretchenalbrecht:::add_palette("pictures/gretchen-albrecht-rosa-concorde.jpg", "rosa concorde")
add_palette <- function(jpegfile, palette) {
  pal <- make_palette(jpegfile)
  load("data/ga_pal_cont.rda")
  if (!is.null(ga_pal_cont[[palette]])) #update existing colour palette
    ga_pal_cont[[palette]] <- pal
  else {
    n = length(ga_pal_cont)
    ga_pal_cont[[n+1]] <- pal
    names(ga_pal_cont)[n+1] <- palette
  }
  save(ga_pal_cont, file="data/ga_pal_cont.rda")
}

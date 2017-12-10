#' gretchenalbrecht palette with ramped colours
#'
#' @param palette Choose from 'gretchenalbrecht_palettes' list
#'
#' @param alpha transparency
#'
#' @param reverse If TRUE, the direction of the colours is reversed.
#'
#' @examples
#' library(scales)
#' show_col(ga_pal()(10))
#'
#' @export
ga_pal <- function(palette="rocker", alpha = 1, reverse = FALSE) {
    pal <- gretchenalbrecht_palettes[[palette]]
    if (reverse){
        pal <- rev(pal)
    }
    return(colorRampPalette(pal, alpha))
}

#' Setup colour palette for ggplot2
#'
#' @rdname scale_color_gretchenalbrecht
#'
#' @param palette Choose from 'gretchenalbrecht_palettes' list
#'
#' @param reverse logical, Reverse the order of the colours?
#'
#' @param alpha transparency
#'
#' @param discrete whether to use a discrete colour palette
#'
#' @param ... additional arguments to pass to scale_color_gradientn
#'
#' @inheritParams viridis::scale_color_viridis
#'
#' @importFrom ggplot2 scale_colour_manual
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point(aes(colour = factor(cyl))) +
#'   scale_colour_gretchenalbrecht(palette="rocker")
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point(aes(colour = hp)) +
#'   scale_colour_gretchenalbrecht(palette="rocker", discrete = FALSE)
#' ggplot(data = mpg) +
#'   geom_point(mapping = aes(x = displ, y = hwy, color = class)) +
#'   scale_colour_gretchenalbrecht(palette="rocker")
#' ggplot(diamonds) + geom_bar(aes(x = cut, fill = clarity)) +
#'   scale_fill_gretchenalbrecht()
#' library(ggthemes)
#' data(nz_cart_census)
#' ggplot(nz_cart_census, aes(x=long, y=lat, group=group,
#'  fill=MedianIncome2013)) +
#'  scale_fill_gretchenalbrecht(palette="winter_light",
#'                              discrete=FALSE) +
#'  geom_polygon() + theme_map() + theme(legend.position="right")

#' @export
#'
#' @importFrom ggplot2 discrete_scale scale_color_gradientn
scale_color_gretchenalbrecht <- function(..., palette="rocker",
                              discrete = TRUE, alpha = 1, reverse = FALSE) {
   if (discrete) {
       discrete_scale("colour", "gretchenalbrecht", palette=ga_pal(palette, alpha = alpha, reverse = reverse))
   } else {
       scale_color_gradientn(colours = ga_pal(palette, alpha = alpha, reverse = reverse, ...)(256))
   }
    #scale_colour_manual(values=gretchenalbrecht_palettes[[palette]])
}

#' @rdname scale_color_gretchenalbrecht
#' @export
scale_colour_gretchenalbrecht <- scale_color_gretchenalbrecht

#' #' Setup fill palette for ggplot2
#'
#' @param palette Choose from 'gretchenalbrecht_palettes' list
#'
#' @inheritParams viridis::scale_fill_viridis
#' @inheritParams ga_pal
#'
#' @param discrete whether to use a discrete colour palette
#'
#' @param ... additional arguments to pass to scale_color_gradientn
#'
#' @importFrom ggplot2 scale_fill_manual discrete_scale scale_fill_gradientn
#'
#' @export
scale_fill_gretchenalbrecht <- function(..., palette="rocker",
                              discrete = TRUE, alpha=1, reverse = TRUE) {
    if (discrete) {
        discrete_scale("fill", "gretchenalbrecht", palette=ga_pal(palette, alpha = alpha, reverse = reverse))
    }
    else {
        scale_fill_gradientn(colours = ga_pal(palette, alpha = alpha, reverse = reverse, ...)(256))
    }
}

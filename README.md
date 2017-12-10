# gretchenalbrecht

This package is a set of palettes for R based on the work of New Zealand expressionist painter [Gretchen Albrecht](https://en.wikipedia.org/wiki/Gretchen_Albrecht). To install it:

```devtools::install_github("dicook/gretchenalbrecht")```

and to try it out:

```
library(ggthemes)
data(nz_cart_census)
ggplot(nz_cart_census, aes(x=long, y=lat, group=group,
 fill=MedianIncome2013)) +
 scale_fill_gretchenalbrecht(palette="winter_light",
                             discrete=FALSE) +
 geom_polygon() + theme_map() + theme(legend.position="right")
```

# Library statements ------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(scales)
library(rlang)
library(grid)
library(scico) # dev version: devtools::install_github("thomasp85/scico")
library(raster)
library(sp)
library(ggtext)

# Data import -------------------------------------------------------------

individuals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/individuals.csv')
locations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/locations.csv')

## Get elevation data -----------------------------------------------------

# I don't know what I'm doing =)
seed <- SpatialPointsDataFrame(
  data.frame(
    x = locations$longitude,
    y = locations$latitude
  ),
  locations,
  proj4string = CRS(projargs = "+proj=longlat")
)
# Gives many many warnings, but I don't know what I'm doing wrong
dat_elevation <- elevatr::get_elev_raster(seed, z = 7)

# Functions ---------------------------------------------------------------

## Constructors -----------------------------------------------------------

# Constructor for hex-binned vector calculations
stat_hexvec <- 
  function(mapping = NULL, data = NULL, 
           geom = "vector", position = "identity",
           ..., bins = 30, binwidth = NULL, na.rm = FALSE,
           show.legend = NA, inherit.aes = TRUE) {
    layer(data = data,
          mapping = mapping,
          stat = StatHexVec,
          geom = geom,
          position = position,
          show.legend = show.legend,
          inherit.aes = inherit.aes,
          params = list(
            bins = bins,
            binwidth = binwidth,
            na.rm = na.rm,
            ...
          )
    )
  }

# A geom constructor for arrows
geom_vector <- 
  function(
    mapping = NULL, data = NULL, stat = "identity", position = "identity",
    ...,
    arrow = arrow(), arrow.fill = NULL, lineend = "butt", linejoin = "round",
    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE
  ) {
    layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomVector,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        arrow = arrow, arrow.fill = arrow.fill,
        lineend = lineend, linejoin = linejoin, na.rm = na.rm,
        mult = 1,
        ...
      )
    )
  }

## Extras ------------------------------------------------------------------

draw_key_vector <- function(data, params, size) {
  segmentsGrob(
    x0 = 0, 
    y0 = 0, 
    x1 = 1, 
    y1 = 1, 
    arrow = arrow(angle = 15, type = "closed", length = unit(0.15, "inches")),
    gp = gpar(
      col = alpha(data$colours %||% data$fill %||% "black", data$alpha),
      fill = alpha(data$colours %||% data$fill %||% "black", data$alpha),
      lwd = (data$size %||% 0.5) * .pt,
      lty = data$lintype %||% 1,
      lineend = "butt"
    )
  )
}


## ggprotos ---------------------------------------------------------------

# Stat for calculating hex-binned vectors
# It relies heavily on internal ggplot2 code, so might be unstable if developers
# decide to change this. Current version is v3.3.2
StatHexVec <- ggproto(
  "StatHexVec", Stat,
  required_aes = c("x", "y", "xend", "yend"),
  default_aes = aes(radius = after_stat(mag)),
  compute_group = function(data, scales, binwidth = NULL, bins = 30,
                           na.rm = FALSE) {
    ggplot2:::try_require("hexbin", "stat_hexvec")
    binwidth <- binwidth %||% ggplot2:::hex_binwidth(bins, scales)
    
    if (length(binwidth) == 1) {
      binwidth <- rep(binwidth, 2)
    }
    # Setup x bins
    xbnds <- ggplot2:::hex_bounds(data$x, binwidth[1])
    xbins <- diff(xbnds)/binwidth[1]
    
    # Setup y bins
    ybnds <- ggplot2:::hex_bounds(data$y, binwidth[2])
    ybins <- diff(ybnds)/binwidth[2]
    
    # Calculate bins
    bins <- hexbin::hexbin(data$x, xbnds = xbnds, xbins = xbins,
                           data$y, ybnds = ybnds,
                           shape = ybins/xbins, IDs = TRUE)
    
    # Number of entries for each bin
    count <- tapply(data$x, bins@cID, length)
    
    # Calculate magnitudes
    magx <- tapply(data$xend - data$x, bins@cID, mean)
    magy <- tapply(data$yend - data$y, bins@cID, mean)
    mag <- sqrt(magx^2 + magy^2)
    
    # Calculate angle
    angle <- atan2(magy, magx)
    
    # Format in data.frame
    new <- ggplot2:::new_data_frame(
      c(hexbin::hcell2xy(bins), 
        list(
          mag = as.vector(mag), angle = as.vector(angle), 
          count = as.vector(count)
        ))
    )
    return(new)
  }
)

# A geom for vector arrows from origin-angle-radius pairs
GeomVector <- ggproto(
  "GeomVector", Geom,
  required_aes = c("x", "y", "radius", "angle"),
  non_missing_aes = c("linetype", "size"),
  default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),
  extra_params = c("na.rm", "mult"),
  draw_panel = function(data, panel_params, coord, arrow = NULL, 
                        arrow.fill = NULL, lineend = "butt", linejoin = "round",
                        na.rm = FALSE, mult = 1) {
    data <- remove_missing(data, na.rm = na.rm,
                           c("x", "y", "angle", "radius", "linetype", "size"),
                           name = "geom_vector")
    if (NROW(data) == 0) return(zeroGrob())
    
    # Set max radius to 1
    data$radius <- data$radius / max(data$radius)
    res <- resolution(data$x)
    
    if (inherits(arrow, "arrow")) {
      # Scale arrow proportional to radius
      arlen <- convertUnit(arrow$length, "native", valueOnly = TRUE)
      arlen <- arlen * data$radius * res * 0.01
      arrow$length <- unit(arlen, "native")
    }
    # Multiply radius by resolution and multiplicative factor
    data$radius <- data$radius * res * mult
    
    # Convert polar to (centered) cartesian
    data <- transform(data,
                      xend = x + cos(angle) * radius,
                      yend = y + sin(angle) * radius
    )
    data <- transform(data,
                      x = x - cos(angle) * radius,
                      y = y - sin(angle) * radius
    )
    
    # Finish grob
    coord <- coord$transform(data, panel_params)
    arrow.fill <- arrow.fill %||% coord$colour
    grob <- segmentsGrob(coord$x, coord$y, coord$xend, coord$yend,
                         default.units = "native",
                         gp = gpar(
                           col = alpha(coord$colour, coord$alpha),
                           fill = alpha(arrow.fill, coord$alpha),
                           lwd = coord$size * .pt,
                           lty = coord$linetype,
                           lineend = lineend,
                           linejoin = linejoin
                         ),
                         arrow = arrow)
    grob
  },
  draw_key = draw_key_vector
)

# Data wrangling ----------------------------------------------------------

## Wrangle caribous -------------------------------------------------------

# Order by time for each animal
locations <- locations %>%
  group_by(animal_id) %>%
  arrange(timestamp, .by_group = TRUE) %>%
  ungroup()

# Couple each observation to the next, can use easy head/tail operations to get
# the next one since we already ordered by animal and timestamp
loc <- locations %>%
  head(-1) %>%
  mutate(
    long_end = tail(locations$longitude, -1),
    lat_end  = tail(locations$latitude, -1),
    animal_check = tail(locations$animal_id, -1)
  ) %>%
  # Now we just need to make sure that the next observation is indeed the same
  # animal.
  filter(animal_id == animal_check)


## Wrangle elevation ------------------------------------------------------

# Set bounding box
longrange <- expand_range(range(loc$longitude), 0.05)
latrange <- expand_range(range(loc$latitude), 0.05)

# Convert to data.frame and enforce bounding box
rast <- rasterToPoints(dat_elevation, spatial = FALSE)
rast <- as.data.frame(rast) %>%
  filter(x >= longrange[1] & x <= longrange[2]) %>%
  filter(y >= latrange[1]  & y <= latrange[2]) %>%
  rename(longitude = x, latitude = y, height = layer)

# Plotting ----------------------------------------------------------------

# Compute aspect ratio for correct binwidth calculations
# Deduced from ggplot2::coord_quickmap()
x_cen <- sum(longrange) / 2
y_cen <- sum(latrange) / 2
x_dist <- ggplot2:::dist_central_angle(x_cen + c(-0.5, 0.5), rep(y_cen, 2))
y_dist <- ggplot2:::dist_central_angle(rep(x_cen, 2), y_cen + c(-0.5, 0.5))
ratio <- y_dist / x_dist
asp_ratio <- diff(latrange) / diff(longrange) * ratio

plt <- ggplot(loc, aes(longitude, latitude)) +
  # Elevation map
  geom_raster(data = rast, aes(fill = height), show.legend = FALSE) +
  # The vector field
  stat_hexvec(aes(yend = lat_end, xend = long_end,
                  alpha = after_stat(count),
                  radius = after_stat(mag)),
              arrow = arrow(type = "closed", angle = 20), 
              size = 1, 
              binwidth = c(0.12 * asp_ratio, 0.12)) +
  continuous_scale("radius", "rad_scale",
                   rescale_pal(range = c(0.4, 1)),
                   trans = "sqrt", guide = "none") +
  scale_alpha_continuous(trans = "log10", 
                         name = "# Sightings",
                         breaks = c(1, 10, 100, 1000, 10000),
                         limits = c(1, NA),
                         labels = number_format(1, justify = "left")) +
  scale_x_continuous(name = "", expand = c(0,0),
                     labels = number_format(suffix = "°E", accuracy = 1)) +
  scale_y_continuous(name = "", expand = c(0,0),
                     labels = number_format(suffix = "°N", accuracy = 1)) +
  scale_fill_gradientn(colours = c("grey80", "white")) +
  facet_grid(~ season) +
  labs(
    title = "<span style ='font-weight: heavy;'>Woodland *Caribou* Movements</span>",
    subtitle = "Location tags represented as *vector fields*.",
    caption = c("**Data |** Movebank")
  ) +
  theme(
    aspect.ratio = asp_ratio,
    text = element_text(size = 25, family = "Lato"),
    axis.text = element_text(size = rel(0.6)),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    legend.key = element_blank(),
    strip.background = element_blank(),
    legend.position = c(0,0),
    legend.title = element_text(face = "bold", size = rel(0.8)),
    legend.justification = c(0,0),
    legend.box.margin = margin(10,10,10,10),
    legend.text = element_text(size = rel(0.6)),
    legend.text.align = 1,
    legend.background = element_rect(fill = NA, colour = NA),
    plot.title = element_markdown(face = "bold"),
    plot.subtitle = element_markdown(),
    plot.caption = element_markdown(),
    panel.spacing = unit(10, "pt")
  )

# Save figure -------------------------------------------------------------

ragg::agg_png("figures/2020_06_23_Caribous.png", width = 2000, height = 2000, res = 150)
print(plt)
dev.off()

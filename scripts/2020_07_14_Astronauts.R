# Library statements ------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(scales)
library(elementalist)
library(grid)
library(gtable)
library(ggtext)
library(systemfonts)

# Functions ---------------------------------------------------------------

## Geom constructor -------------------------------------------------------

geom_stars <- function(
  mapping = NULL, data = NULL,
  stat = "identity", position = "identity", ..., na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat, 
    geom = GeomStar,
    position = position, 
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm)
  )
}

## Geom ggproto -----------------------------------------------------------

GeomStar <- ggproto(
  "GeomStar", GeomPoint,
  draw_panel = function(self, data, panel_params, coord, na.rm = FALSE) {
    grob <- ggproto_parent(GeomPoint, self)$draw_panel(
      data = data, panel_params = panel_params, coord = coord,
      na.rm = na.rm
    )
    class(grob) <- c("stargrob", class(grob))
    grob
  }
)

# Geom renderer -----------------------------------------------------------

# Star prototype
star <- structure(c(0.499823633156966, 0.617283950617284, 1, 0.68994708994709, 
                    0.808818342151675, 0.499823633156966, 0.190828924162257, 0.31005291005291, 
                    0, 0.382363315696649, 0.499823633156966, 0, 0.364021164021164, 
                    0.36331569664903, 0.587301587301587, 0.950970017636684, 0.725573192239859, 
                    0.950970017636684, 0.587301587301587, 0.36331569664903, 0.364021164021164, 
                    0), .Dim = c(11L, 2L))

makeContext.stargrob <- function(x) {
  xnew <- convertUnit(x$x, "mm", valueOnly = TRUE)
  ynew <- convertUnit(x$y, "mm", valueOnly = TRUE)
  
  xstar <- rep(star[, 1] - 0.5, length(xnew)) * 1.5
  ystar <- rep(star[, 2] - 0.5, length(ynew)) * 1.5
  id <- rep(seq_along(xnew), each = nrow(star))
  
  xnew <- xstar + xnew[id]
  ynew <- ystar + ynew[id]
  
  gp <- x$gp
  gp$fill <- gp$col
  gp$col <- NA
  
  polygonGrob(
    x = unit(xnew, "mm"),
    y = unit(ynew, "mm"),
    id = id,
    gp = gp
  )
}

## File importer ----------------------------------------------------------

jpegurl2raster <- function(url, width = 1, height = 1) {
  download.file(
    url,
    tmp <- tempfile(fileext = ".jpg", pattern = "globe"), method = "curl"
  )
  on.exit(unlink(tmp))
  
  raster <- jpeg::readJPEG(tmp)
  raster <- matrix(rgb(
    r = raster[, , 1],
    g = raster[, , 2],
    b = raster[, , 3]
  ), nrow = dim(raster)[1], ncol = dim(raster)[2])
  raster[raster == "#000000"] <- "#00000000"
  raster[raster == "#010101"] <- "#01010100"
  raster <- rasterGrob(raster, width = width, height = height)
  return(raster)
}

# Data import -------------------------------------------------------------

astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

globe <- jpegurl2raster(
  "https://c7.uihere.com/files/405/868/11/astronomy-cosmic-cosmos-dark.jpg",
  width = 0.25, height = 0.25
)

moon <- jpegurl2raster(
  "https://upload.wikimedia.org/wikipedia/commons/0/01/Lunar_Surface_%28AS16-121-19449%29.jpg",
  width = 1, height = 1
)
moon$raster <- moon$raster[1:500,]
moon$y <- unit(0, "npc")
moon$just <- "bottom"

# Data wrangling ----------------------------------------------------------

lut <- c(
  Australia = "Other", Bulgaria = "Europe", Canada = "America",
  China = "Asia", France = "Europe", Germany = "Europe", Italy = "Europe",
  Japan = "Asia", Sweden = "Europe", Switzerland = "Europe", `U.K.` = "Europe",
  `U.K./U.S.` = "Other", `U.S.S.R/Russia` = "Russia", `U.S.` = "America"
)

missions <- astronauts %>%
  select(name, year_of_mission, eva_hrs_mission, nationality) %>%
  rename(year = year_of_mission, eva_hrs = eva_hrs_mission) %>%
  mutate(type = "mission")

selection <- astronauts %>%
  select(name, year_of_selection, nationality) %>%
  rename(year = year_of_selection) %>%
  group_by(name) %>%
  summarise(year = year[1], eva_hrs = 0, 
            nationality = nationality[1], type = "selection")

df <- rbind(missions, selection) %>%
  group_by(name) %>%
  filter(!all(eva_hrs == 0)) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(last = c(rep(FALSE, n() - 1), TRUE),
         eva_hrs = cumsum(eva_hrs),
         n = seq_len(n()),
         part = lut[nationality])


# Plot settings -----------------------------------------------------------

colours <- c("#00E6FF", "#2ABEFF", "#8F9FFF", 
             "#CF77F0", "#FD006A")

labels <- sort(unique(lut))
labels <- grid::textGrob(
  x = 0.05, y = unit((5:1)/3, "cm"),  
  label = labels,
  hjust = 0,
  gp = grid::gpar(col = colours, fontsize = 6),
  just = "left"
)

# Plot --------------------------------------------------------------------

g <- ggplot(df, 
       aes(year, sqrt(eva_hrs), 
           group = name, colour = part)) +
  geom_line(aes(alpha = n, colour = part),
            show.legend = FALSE, size = 0.2) +
  geom_stars(data = filter(df, last), 
             aes(alpha = n),
             size = 3.5) +
  # I wanted the unicode star here but that didn't work with the device
  # geom_point(data = filter(df, last), 
  #            aes(slpha = n),
  #            shape = "A",# "\u2605", 
  #            size = 3.5) +
  annotate("text", x = 1960, y = c(0, sqrt(c(6, 24, 48, 72))),
           label = paste0(" ", c(0, 6, 24, 48, 72), "h"),
           hjust = 0, size = 3, colour = "grey50", family = "mono") +
  scale_x_continuous(limits = c(1960, 2020), oob = oob_keep,
                     name = "") +
  scale_y_continuous(limits = c(-5, NA),
                     breaks = c(0, sqrt(c(6, 24, 48, 72, 96, 120))),
                     labels = NULL,
                     expand = c(0,0),
                     name = "") +
  scale_colour_manual(values = colours, guide = "none") +
  scale_alpha(guide = "none") +
  coord_polar() +
  labs(
    title = "Spacewalks",
    subtitle = "Cumulative duration of spacewalks\nin the missions of an astronaut.",
    caption = "Data | *Mariya Stavnichuk* and *Tatsuya Corlett*"
  ) +
  theme(
    text = element_text(family = "Baloo Thambi 2"),
    plot.margin = margin(0,0,0,0),
    plot.title = element_text(colour = "white", family = "Space Age",
                              size = 24, margin = margin(t = 10)),
    plot.subtitle = element_text(colour = "grey50", margin = margin(t = 5)),
    plot.caption = element_markdown(colour = "black", margin = margin(5,5,5,5),
                                    face = "bold"),
    plot.background = element_blank(),
    panel.background = element_blank(),
    axis.text = element_text(colour = "grey50"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = c(rep("grey20", 5), rep("#00000000", 3)),
                                      size = 0.2),
    panel.grid.minor.x = element_line_multicolour(colour = c("#00000000", "#00000000", "grey15", "grey20", "black")),
    panel.grid.minor.y = element_blank(),
    axis.ticks = element_blank()
  )

# Editing -----------------------------------------------------------------

gt <- ggplotGrob(g)
gt <- gtable_add_grob(
  gt, list(globe, moon, labels), 
  t = c(7, 8, 8), l = 5, b = c(7, 12, 8), z = 0,
  clip = "off"
)
gt$layout$z[1] <- -Inf

gt <- gt[, 5]
gt$grobs[[7]]$children[[1]]$x <- unit(0.05, "npc")
gt$grobs[[6]]$children[[1]]$x <- unit(0.05, "npc")
gt$grobs[[8]]$children[[1]]$x <- unit(0.95, "npc")

# Saving ------------------------------------------------------------------

ragg::agg_supertransparent("figures/2020_07_14_Astronauts.png", 
              width = 1125, height = 1500,
              background = "black", res = 300)
grid.newpage(); grid.draw(gt)
dev.off()


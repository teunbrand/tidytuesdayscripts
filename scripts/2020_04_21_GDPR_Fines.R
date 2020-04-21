# Library statements ------------------------------------------------------

library(tidyverse)
library(data.table)
library(magick)
library(rsvg)
library(rlang)
library(igraph)
library(ggraph)
library(ggtext) # remotes::install_github("wilkelab/ggtext")
library(gridtext)
library(gtable)
library(grid)

# Data import -------------------------------------------------------------

vio <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')
txt <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_text.tsv')

# Data prewrangling -------------------------------------------------------

flags <- setNames(vio$picture, vio$name)
flags <- flags[!duplicated(flags)]
flags <- lapply(flags, function(source) {
  as.raster(image_read_svg(source, width = 100))
})

# Graphic functions -------------------------------------------------------

## Grid -------------------------------------------------------------------

flagGrob <- function(
  countries,
  x = unit(0.5, "npc"),
  y = unit(0.5, "npc"),
  width = unit(1, "cm"),
  height = unit(1, "cm"),
  just = "centre",
  hjust = NULL,
  vjust = NULL,
  default.units = "npc",
  name = NULL,
  gp = gpar(),
  vp = NULL
) {
  width <- width %||% rep(list(NULL), length(countries))
  height <- width %||% rep(list(NULL), length(countries))
  if (is.unit(width)) {
    width <- lapply(seq_along(width), function(i){width[i]})
  }
  if (is.unit(height)) {
    height <- lapply(seq_along(height), function(i){height[i]})
  }
  
  groblist <- mapply(function(country, x, y, w, h) {
    rasterGrob(flags[[country]], x = x, y = y, 
               width = w, height = h, 
               just = just, hjust = hjust, vjust = vjust,
               interpolate = FALSE, default.units = default.units, 
               name = NULL, gp = gp, vp = vp)
  }, country = countries, x = x, y = y, w = width, h = height,
  SIMPLIFY = FALSE)
  groblist <- do.call(gList, groblist)
  return(groblist)
}

## ggplot2 ----------------------------------------------------------------

### Geom ------------------------------------------------------------------

geom_flag <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomFlag,
        position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...))
}

### ggproto ---------------------------------------------------------------

GeomFlag <- ggproto(
  "GeomFlag", GeomPoint,
  required_aes = c("x", "y", "country"),
  default_aes = aes(shape = 19, colour = "black", size = 10, 
                    fill = NA, alpha = NA, stroke = 0.5),
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    coords <- coord$transform(data, panel_params)
    size <- unit(coords$size * .pt, "pt")
    flagGrob(as.character(coords$country), coords$x, coords$y, 
             size, size)
  }
)

# Data wrangling ----------------------------------------------------------

## Make graph -------------------------------------------------------------

arts <- lapply(strsplit(vio$article_violated, "\\|"),
               function(x) {
                 x <- strsplit(gsub("Art\\.?[ ]+", "", x), "[ ]|\\(")
                 x <- sapply(x, `[`, 1)
                 as.numeric(x)
               })

# Make edgelist
elist <- data.frame(country = vio$name, fine = vio$price)
elist$arts <- arts
elist$fine <- elist$fine / lengths(elist$arts)
elist <- elist %>% unnest(arts)
elist$arts[is.na(elist$arts)] <- "Other"
elist <- elist[elist$fine > 0,]
setDT(elist)
elist <- elist[, list(fine = sum(fine)), by = c("country", "arts")]

# Populate graph
gr <- graph_from_edgelist(as.matrix(elist)[, c(1, 2)])
E(gr)$fine <- elist$fine
V(gr)$type <- V(gr)$name %in% elist$country

# Choose bipartite graph
gglayout <- create_layout(gr, layout = "igraph", algorithm = "sugiyama")
gglayout$x[gglayout$name %in% c("32", "Other")] <-
  gglayout$x[gglayout$name %in% c("32", "Other")] - c(1, 0.5)

## Sum fines per country --------------------------------------------------

dtvio <- as.data.table(vio)
dtvio <- dtvio[, list(total_fine = sum(price)), by = "name"]
dtvio <- dtvio[order(total_fine, decreasing = T),]
dtvio[, x := gglayout$x[match(name, gglayout$name)]]
dtvio <- dtvio[total_fine > 0,]

# Plotting ----------------------------------------------------------------

## Annotations ------------------------------------------------------------

# Make fancy title
title <- paste0(
  "<br>**General Data<br>Protection Regulation**<br><br>",
  "*Fines divided<br>by article*"
)
title <- annotate("richText", x = -Inf, y = -Inf, label = title,
                  fill = NA, label.color = NA, colour = "white",
                  size = 6)

# Convenience function to plot countries different from articles
filter_nodes <- function(test = NULL) {
  test <- substitute(test)
  function(layout) {
    fun <- get_nodes()
    nodes <- fun(layout)
    subset.data.frame(nodes, eval(test))
  }
}

# Italy
italy <- vio %>%
  filter(name == "Italy") %>%
  filter(price == max(price))
italy <- strsplit(italy$summary, "\\.")[[1]][1]
italy <- gridtext::textbox_grob(
  paste0("*", italy, "*"), 
  x = unit(0.1, "npc"), y = unit(0.15, "npc"),
  maxwidth = unit(0.2, "npc"),
  gp = grid::gpar(cex = 0.9)
)

# France
france <- vio %>%
  filter(name == "France") %>%
  filter(price == max(price))
france <- strsplit(france$summary, "\\.")[[1]][1]
france <- gridtext::textbox_grob(
  paste0("*", france, "*"),
  x = unit(0.88, "npc"), y = unit(0.12, "npc"),
  maxwidth = unit(0.4, "npc"), halign = 1,
  gp = grid::gpar(cex = 0.9)
)

## Plot code --------------------------------------------------------------

plot <- ggraph(gglayout) +
  geom_edge_diagonal(aes(edge_width = fine/1e6, edge_colour = fine/1e6)) +
  geom_flag(aes(x = x, y = y, country = name),
            data = filter_nodes(type == TRUE)) +
  annotate("rect", 
           xmin = -Inf, xmax = Inf, 
           ymin = -Inf, ymax = 0.995, 
           fill = '#002A64FF', colour = '#002A64FF') +
  title +
  geom_text(aes(x = x, y = y, label = name), 
            data = filter_nodes(type == FALSE),
            position = position_nudge(y = -0.1),
            colour = "#FFEA46FF", size = 4.5) +
  annotate("text", x = dtvio$x, y = 2.3,
           label = scales::dollar(dtvio$total_fine/1e3, suffix = "k",
                                  prefix = "\u20AC", accuracy = 1)) +
  scale_edge_colour_viridis(
    limits = c(0, NA), oob = scales::squish,
    direction = -1, option = "E",
    name = "Fine (Million \u20AC)",
    guide = guide_colorbar(available_aes = "edge_colour",
                           title.position = "top", title.hjust = 0.5, 
                           barwidth = unit(0.7, "npc"))
  ) +
  guides(edge_width = "none") +
  scale_y_continuous(expand = c(0,0.6,0,0), limits = c(0.9, 2),
                     oob = scales::rescale_none) +
  scale_x_continuous(expand = c(0,0.3)) +
  coord_polar(start = pi/2.8, clip = "off") +
  theme(panel.background = element_blank(),
        legend.position = "bottom",
        plot.margin = margin())


# Gtable ------------------------------------------------------------------

gt <- ggplotGrob(plot)
gt <- gtable_add_grob(gt, italy, t = 1, l = 1, b = nrow(gt), r = ncol(gt),
                      name = "italy")
gt <- gtable_add_grob(gt, france, t = 1, l = 1, b = nrow(gt), r = ncol(gt),
                      name = "france", clip = "off")

png("figures/2020_04_21_GDPR_Fines.png", 
    width = 900, height = 800, antialias = "subpixel")
grid.newpage(); grid.draw(gt)
dev.off()


# Library statements ------------------------------------------------------

library(tidyverse)
library(countrycode)
library(grid)
library(gridtext)

# Data import -------------------------------------------------------------

volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
# events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')
# tree_rings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/tree_rings.csv')
# sulfur <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/sulfur.csv')

# Functions ---------------------------------------------------------------

## Layer function ---------------------------------------------------------

# Function for calculating how many times a space is occupied by a set of ranges
# Based on https://github.com/teunbrand/ggnomics/blob/BioC/R/stat_coverage.R

stat_coverage <- 
  function(mapping = NULL, data = NULL,
           geom = "area", position = "identity",
           ..., na.rm = FALSE, orientation = NA, show.legend = NA,
           inherit.aes = TRUE) {
    layer(
      data = data,
      mapping = mapping,
      stat = StatCoverage,
      geom = geom,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        orientation = orientation,
        ...
      )
    )
  }

## ggproto ----------------------------------------------------------------

StatCoverage <- ggproto(
  "StatCoverage",
  Stat,
  required_aes = c("x", "xend"),
  default_aes = aes(y = after_stat(coverage),
                    xend = x + 1,
                    fill = NA, weight = 1L),
  setup_params = function(data, params) {
    # unix <- unique(data$x, data$xend)
    # params[["unix"]] <- unix
    params
  },
  compute_group = function(data, scales, unix) {
    weight <- data$weight %||% rep_len(1L, length.out = {n <- NROW(data)})
    
    # Order all relevant positions
    pos <- with(data, c(pmin(x, xend), pmax(x, xend)))
    # upos <- setdiff(pos, unix)
    # pos <- c(pos, upos)
    o <- order(pos)
    pos <- pos[o]
    
    # Order weights; end-positions have negative weight
    w <- c(weight, -weight)[o]#, rep(0, length(upos)))[o]
    # Sum identical positions
    w <- vapply(split(w, pos), sum, numeric(1))
    # Cumulative sum over + and - weights yields coverage
    w <- cumsum(w)
    
    # To account for having vapplied over 'w'
    pos <- unique(pos)
    
    pos <- rep(pos, each = 2)
    w <- c(0, head(rep(w, each = 2), -1))
    
    structure(list(
      x = pos, coverage = w, scaled = w / max(w), n = n,
      density = w / sum(abs(data$xend - data$x) * weight)
    ), class = "data.frame", row.names = .set_row_names(length(pos)))
  }
)

# Data wrangling ----------------------------------------------------------

df <- left_join(eruptions, volcano, by = "volcano_number") %>%
  mutate(start = ISOdate(start_year, start_month, start_day),
         end = ISOdate(end_year, end_month, end_day)) %>%
  filter(start_year > 1900 & !is.na(end) & !is.na(start) & !is.na(country)) %>%
  filter(eruption_category == "Confirmed Eruption") %>%
  mutate(region = fct_lump_min(factor(region), 25)) %>%
  mutate(country = fct_lump_min(fct_infreq(country), 25))
  # mutate(country = lvls_revalue(country, str_wrap(levels(country), 15)))


# Plotting ----------------------------------------------------------------



g <- ggplot(df) +
  stat_coverage(aes(x = start, xend = end + 1, fill = after_stat(n))) +
  facet_grid(country ~ ., scales = "free_y", space = "free_y") +
  scale_y_continuous(
    name = "Active Eruptions",
    breaks = c(0, 5, 10), minor_breaks = 0 + .Machine$double.eps,
    expand = c(0, 0),
    limits = function(x) {c(0, max(x, 5))}
  ) +
  scale_fill_viridis_c(option = "magma", # How could I not?
                       name = "Total   \nEruptions   ",
                       breaks = c(25, 50, 100, 200, 400),
                       trans = "log10", direction = -1,
                       guide = guide_colourbar(barwidth = unit(0.5, "npc"),
                                              barheight = unit(5, "mm"),
                                              title.vjust = 1,
                                              title.hjust = 0.9),
                       limits = c(25, NA)) +
  scale_x_datetime(date_breaks = "10 years", date_labels = "%Y",
                   expand = c(0,0), limits = c(ISOdate(1900, 1, 1), NA),
                   name = "") +
  coord_cartesian(clip = "off") +
  labs(title = "Volcano eruptions", subtitle = "Since 1900",
       caption = "Confirmed eruptions with start- and end-dates | Data from Smithsonian Institution.") +
  theme(strip.text.y = element_text(angle = 0, hjust = 0, vjust = 0),
        text = element_text(colour = "black"),
        axis.text = element_text(colour = "black"),
        axis.line = element_line(colour = "black", size = 0.2),
        axis.ticks = element_line(colour = "black", size = 0.2),
        strip.background = element_blank(),
        strip.text = element_text(colour = "black", vjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor.y = element_line(colour = "black", 
                                          linetype = 1, size = 0.2),
        plot.title = element_text(colour = "#7E03A8", face = "bold"),
        plot.subtitle = element_text(colour = "#B63679"),
        plot.caption = element_text(colour = "black", hjust = 1),
        legend.margin = margin(t = -1, unit = "lines"),
        legend.position = "bottom")

# Tweak plot --------------------------------------------------------------


gt <- ggplotGrob(g)

# Set panel sizes to phi
panel_r <- panel_rows(gt)$t
panel_c <- panel_cols(gt)$l
gt <- gtable::gtable_add_cols(gt, unit(50, "pt"), panel_c)
gt$layout$r <- gt$layout$r + ifelse(gt$layout$r == panel_c, 1, 0)
gt$widths[panel_c] <- unit(sum(unlist(gt$heights[panel_r])), "null") * ((1 + sqrt(5))/2)
gt$widths[panel_c + 1] <- unit(5.5 * (length(panel_r) -1), "pt") * ((1 + sqrt(5))/2)
gt$respect <- TRUE

# Add annotations
indonesia <- richtext_grob(
  text = "**13**<br><i style='color:#CC4678'>*simultaneous*</i><br>eruptions!",
  x = 1, rot = -30, gp = gpar(fontsize = 8)
)
arrow <- curveGrob(0.94, 0.6, 0.825, 0, arrow = arrow(length = unit(0.1, "inches"),
                                                      type = "closed"),
                   angle = 90, square = FALSE, curvature = 1,
                   ncp = 1, shape = -1)

gt <- gtable::gtable_add_grob(gt, list(indonesia, arrow), 
                               t = c(3, 3), l = 5, r = 6, b = 4, 
                               name = c("indonesia", "arrow"),
                               clip = "off")

# Set strip clipping off
strips <- which(grepl("strip", gt$layout$name))
gt$grobs[strips] <- lapply(gt$grobs[strips], function(x){x$layout$clip <- "off"; x})
gt$layout$r[gt$layout$name == "caption"] <- 7

# Save figure -------------------------------------------------------------

ggsave("figures/2020_05_12_Volcanos.png", gt,
       width = 8, height = 5)


# Library statements ------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggh4x)
library(grid)
library(tweenr)
library(rlang)
library(ggtext) # Now on CRAN!

# Data import -------------------------------------------------------------

# firsts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')
science <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/science.csv')

# Data annotation ---------------------------------------------------------

# Manual annotation, read `occupation_s` and `inventions_accomplishments` with
# some Wikipedia rabbitholes on the side.
# I'm sorry if I got somebody misclassified/oversimplified, it's hard to know 
# whether the studying the hydrodynamics of jellyfish is biology or physics
cat <- tibble::tribble(
  ~ field,
  "Biology", "Physics", "Mathematics", "Engineering", "Inventor", "Chemistry", 
  "Other/Multiple", "Mathematics", "Computer Science", "Medicine", "Inventor", 
  "Inventor", "Medicine", "Mathematics", "Medicine", "Mathematics", "Inventor", 
  "Engineering", "Inventor", "Physics", "Medicine", "Engineering", "Chemistry", 
  "Biology", "Inventor", "Inventor", "Inventor", "Medicine", "Physics", 
  "Biology", "Engineering", "Other/Multiple", "Other/Multiple", 
  "Other/Multiple", "Engineering", "Chemistry", "Engineering", "Chemistry", 
  "Computer Science", "Medicine", "Biology", "Computer Science", 
  "Computer Science", "Engineering", "Chemistry", "Other/Multiple", 
  "Physics", "Inventor", "Computer Science", "Medicine", "Biology", 
  "Other/Multiple", "Engineering", "Medicine", "Chemistry", "Chemistry", 
  "Inventor", "Chemistry", "Computer Science", "Engineering", "Physics", 
  "Biology", "Inventor", "Inventor", "Inventor", "Inventor", "Chemistry", 
  "Biology", "Biology", "Medicine", "Inventor", "Computer Science", "Medicine", 
  "Chemistry", "Medicine", "Inventor", "Chemistry", "Inventor", "Engineering",
  "Other/Multiple", "Inventor", "Physics", "Inventor", "Inventor", "Inventor", 
  "Chemistry", "Other/Multiple", "Computer Science", "Chemistry", "Inventor", 
  "Medicine", "Physics", "Chemistry", "Medicine", "Engineering", "Chemistry", 
  "Inventor", "Engineering", "Inventor", "Other/Multiple", "Mathematics", 
  "Computer Science", "Inventor", "Medicine", "Biology", "Medicine", 
  "Mathematics", "Engineering", "Physics", "Inventor", "Physics", "Inventor", 
  "Physics", "Medicine", "Mathematics", "Other/Multiple", "Inventor", 
  "Medicine", "Medicine", "Biology"
)
science$field <- cat$field
science$id <- seq_len(nrow(science))

# Plotting funcitons ------------------------------------------------------

# Custom theme element for colour gradients in lines
element_link <- function(
  colour = NULL, colour2 = NULL, 
  size = NULL, linetype = NULL, lineend = NULL, arrow = NULL, 
  inherit.blank = FALSE
) {
  if (is.null(arrow)) {
    arrow = FALSE
  }
  if (is.null(colour2)) {
    colour2 <- alpha(colour, 0.1)
  }
  structure(
    list(
      colour = colour, colour2 = colour2, size = size, linetype = linetype,
      lineend = lineend, arrow = arrow, inherit.blank = inherit.blank
    ),
    class = c("element_link", "element_line", "element")
  )
}

# Probably not very performant, but should work
# Inspired by ggforce::geom_link()
element_grob.element_link <- function(
  element, x= 0:1, y = 0:1, colour = NULL, colour2 = NULL, size = NULL,
  linetype = NULL, lineend = NULL, default.units = "npc",
  id.lengths = NULL, id = NULL, ...
) {
  
  # Identify seperate lines
  id <- if (is.null(id)) {
    rep(seq_along(id.lengths), id.lengths)
  } else {
    id
  }
  
  # Colour from function call or element?
  colour <- colour %||% element$colour
  colour2 <- colour2 %||% element$colour2
  
  # Interpolate stuff
  df <- ggplot2:::new_data_frame(
    list(x = x, y = y)
  )
  df <- split(df, id)
  df <- lapply(df, tween_t, n = 100)
  df <- lapply(df, function(dat) {
    ggplot2:::new_data_frame(list(
      x0 = head(dat[[1]], -1),
      x1 = tail(dat[[1]], -1),
      y0 = head(dat[[2]], -1),
      y1 = tail(dat[[2]], -1)
    ))
  })
  df <- lapply(df, function(x){
    x$col <- tween_t(list(c(colour, colour2)), n = 99)[[1]]
    x
  })
  df <- do.call(rbind, df)
  
  # Finish off stuff
  gp <- gpar(col = df$col, fill = colour, lwd = ggplot2:::len0_null(size * .pt),
             lty = linetype, lineend = lineend)
  element_gp <- gpar(col = element$colour, fill = element$colour,
                     lwd = ggplot2:::len0_null(element$size * .pt),
                     lty = element$linetype, lineend = element$lineend)
  arrow <- if (is.logical(element$arrow) && !element$arrow) {
    NULL
  }
  else {
    element$arrow
  }
  
  segmentsGrob(x0 = df$x0, x1 = df$x1, y0 = df$y0, y1 = df$y1,
               default.units = default.units,
               gp = ggplot2:::modify_list(element_gp, gp),
               arrow = arrow, ...)
}

# Draft plot --------------------------------------------------------------

g <- ggplot(science) +
  geom_rect(
    aes(xmin = birth, xmax = ifelse(is.na(death), year(Sys.Date()), death),
        ymin = 0.3, ymax = 0.7, group = id),
    position = position_disjoint_ranges()
  ) +
  facet_wrap(~ field)

# Recycle layer -----------------------------------------------------------

# Grab rectangle data layer
layers <- layer_data(g, 1)
layers$label <- science$name[layers$group]

# Rebuild data
recycled <- science[layers$group,]
recycled$ymin <- layers$ymin
recycled$ymax <- layers$ymax
# Obviously, this is a simplification but who doesn't want Charles Brookes to be
# immortal?
recycled$xmax <- ifelse(is.na(recycled$death), year(Sys.Date()), recycled$death)

# Add colours
set.seed(1865)
colours <- wesanderson::wes_palette("Darjeeling1", 5)
recycled$colours <- sample(colours, nrow(recycled), replace = TRUE)

# Proper plot -------------------------------------------------------------

plt <- ggplot(recycled) +
  geom_rect(aes(xmin = birth, xmax = xmax,
                ymin = ymin - 0.1, ymax = ymax + 0.1,
                fill = colours),
            alpha = 0.3) +
  geom_text(aes(label = name, colour = colours,
                x = (birth + xmax)/2,
                y = (ymin + ymax)/2),
            hjust = 0.5, vjust = 0.5, size = 1.5) +
  scale_y_continuous(limits = c(0, 14),
                     expand = c(0,0), breaks = NULL,
                     name = "") +
  scale_x_continuous(expand = c(0, 0, 0, 0),
                     limits = c(1800, NA), oob = scales::squish,
                     name = "") +
  scale_colour_identity(aesthetics = c("colour","fill"),
                        #  colours = colours,
                        guide = guide_none()) +
  facet_wrap(~ field) +
  labs(caption = "Data from Wikipedia\nhttps://en.wikipedia.org/wiki/List_of_African-American_inventors_and_scientists",
       title = "Notable <i style='color:#5BBCD6'>**African-American**</i>  inventors and scientists",
       subtitle = "between 1800-now and when they lived") +
  theme_dark() +
  theme(plot.background = element_rect(fill = "grey10", colour = NA),
        strip.text = element_text(colour = colours[2], face = "bold.italic"),
        strip.background = element_part_rect(side = "b", colour = colours[4],
                                             fill = NA),
        panel.grid.minor = element_link(colour = alpha(colours[2], 0),
                                        colour2  = alpha(colours[5], 0.5)),
        panel.grid.major = element_link(colour2 = alpha(colours[4], 0),
                                        colour  = alpha(colours[3], 0.5)),
        axis.text = element_text(colour = "grey50"),
        panel.background = element_blank(),
        plot.caption = element_text(colour = "grey90", hjust = 0),
        plot.title = element_markdown(colour = "grey90"),
        plot.subtitle = element_text(colour = "grey50"),
        aspect.ratio = 1/(1 + sqrt(5)))


# Saving ------------------------------------------------------------------

ggsave("figures/2020_06_09_African_American_Achievements.png",
       height = 5, width = 5 * (1 + sqrt(5))/2,
       plt, bg = "grey10")

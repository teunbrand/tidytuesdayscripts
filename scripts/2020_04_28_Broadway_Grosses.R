# Library statements ------------------------------------------------------

library(ggplot2)
library(ggh4x) # https://github.com/teunbrand/ggh4x
library(patchwork)
library(tidyverse)
library(ggtext) # https://github.com/wilkelab/ggtext

# Data import -------------------------------------------------------------

grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 40000)
# synopses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/synopses.csv')
# cpi <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/cpi.csv')
# pre_1985_starts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/pre-1985-starts.csv')

# Functions ---------------------------------------------------------------

## Layer function ---------------------------------------------------------

# Function for making a rolling gaussian filter to get decent trendlines without
# modelling to many bells and whistles

stat_rollgaus <- function(
  mapping = NULL, data = NULL, geom = "line", position = "identity", ...,
  kernel = "norm", n = 256, rel_width = 0.02, na.rm = FALSE, 
  orientation = NA, show.legend = NA,
  inherit.aes = TRUE
) {
  layer(data = data, mapping = mapping, stat = StatRollgaus,
        geom = geom, position = position, show.legend = show.legend,
        inherit.aes = inherit.aes, 
        params = list(n = n, na.rm = na.rm, 
                      orientation = orientation, 
                      ...))
}

# ggproto -----------------------------------------------------------------

StatRollgaus<- ggproto(
  "StatRungaus", Stat,
  compute_group = function(data, scales, n = 256, rel_width = 0.02,
                           na.rm = FALSE, flipped_aes = FALSE) {
    # Throw away unusable values
    data <- data[is.finite(data$x) & is.finite(data$y),]
    # Doesn't make sense to do this for 2 datapoints
    if (NROW(data) < 3) {
      return(data.frame(x = numeric(), y = numeric()))
    }
    
    # Set data and panel range
    dat_range <- range(data$x, na.rm = TRUE)
    lim_range <- range(scales[["x"]]$dimension())
    
    # Sequence over data range
    seq_range <- seq(dat_range[1], dat_range[2], length.out = n)
    # Set standard deviation of kernel
    sd <- diff(lim_range) * rel_width
    
    # Make kernel
    kernel <- outer(data$x, seq_range, dnorm, sd = sd)
    kernel <- t(t(kernel) / colSums(kernel))
    # Train kernel on data
    kernel <- kernel * data$y
    # Collapse kernel
    out <- colSums(kernel)
    
    data.frame(x = seq_range, y = out)
  }
)

# Data wrangling ----------------------------------------------------------

## Decide what shows to include -------------------------------------------

# I want the shows that run more than a year with less than 20% interruptions.
# I know nothing about musicals, so do the same shows in different theaters 
# count as different musicals? Anyway, I'm assuming that is the case for the 
# time being

showlen <- grosses %>%
  group_by(interaction(show, theatre)) %>%
  summarise(show = show[1],
            theatre = theatre[1],
            start = min(week_ending), 
            end = max(week_ending), 
            max_break = max(c(1e-18, as.numeric(diff(week_ending)))),
            n = n(),
            len = as.numeric(diff(range(week_ending)))) %>%
  mutate(prop_break = max_break / len) %>%
  filter(prop_break < 0.2, len > 365 - 7)

include <- with(showlen, paste(show, theatre))

# Apply this filter to main data
shows_filt <- grosses %>%
  filter(paste(show, theatre) %in% include)

# Choose a few shows to highlight
decreasing_shows <- c("Aida", "Jersey Boys", "The Book of Mormon")
stable_shows = c("Mamma Mia!")
increasing_shows <- c("The Lion King", "Cats", "The Phantom of the Opera")
highlight_shows <- c(decreasing_shows, stable_shows, increasing_shows)

# Compare starts versus ends ----------------------------------------------

startend_data <- shows_filt %>%
  group_by(show, theatre) %>%
  # Dont know if the ordering is need but you new know
  mutate(ord = order(week_ending)) %>%
  summarise(
    start_price = mean(head(avg_ticket_price[ord], 20)),
    ref_price = median(avg_ticket_price),
    end_price = mean(tail(avg_ticket_price[ord], 20)),
    n = n()
  ) %>% 
  mutate(start_price = start_price / ref_price,
         end_price = end_price / ref_price,
         increasing = end_price > start_price) %>%
  filter(!is.na(increasing)) # Mostly the free musicals?


# Plotting ----------------------------------------------------------------

## Preamble ---------------------------------------------------------------

# Colours from https://twitter.com/palitra_color/status/1254144837361811457
colours <- c("#00E6FF", "#00D5FF", "#2ABEFF", "#8F9FFF", 
             "#CF77F0", "#F546B4", "#FD006A")

# A theme
my_theme <- theme(plot.background = element_rect(fill = "grey20", colour =  NA),
                  text = element_text(colour = "grey95"),
                  line = element_line(colour = "grey95"),
                  axis.line = element_line(colour = "grey95"),
                  axis.text = element_text(colour = "grey95"),
                  axis.title = element_text(colour = "grey95"),
                  axis.ticks = element_line(colour = "grey95"),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(colour = "grey25", size = 0.2),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  legend.background = element_blank(),
                  legend.margin = margin(0,0,0,0),
                  legend.text = element_text(size = rel(0.8)),
                  legend.key = element_blank(),
                  legend.position = c(1, 0.02),
                  legend.just = c(1, 0))

## Timeline plot -----------------------------------------------------------

# Have something to observe about the data
my_note <- paste(
  "<span style='color:#00E6FF;'>The Phantom of the Opera</span>",
  "<span style='color:white;'>goes against the trend,",
  "but went from one of the",
  "most expensive musicals",
  "to one of the", 
  "cheaper musicals",
  "while increasing the price</span>",
  sep = "<br>"
  )

# I want the legend text to be the colour guide itself
legendtext <- paste0(
  "<span style='color:", rev(colours),";'>", highlight_shows, "</span>"
)

# Manually make a legend for in the plot itself
legend_anno <- data.frame(
  show = head(highlight_shows, -1),
  week_ending = as.Date(paste0(c(
    2002, 2008, 2015, 2002, 2018, 1990
  ), "-01-01")),
  avg_ticket_price = c(50, 140, 240, 100, 200, 35)
)

# Make plot
timeplot <- ggplot(shows_filt, 
                   aes(week_ending, 
                       avg_ticket_price, 
                       group = interaction(show, theatre))) +
  # We'll first do the unhighlighted shows (lowlighted?)
  # I know there is a gghighlight package that might greatly ease this, but 
  # since I'm not highlighting often, I'll do it manually.
  geom_point(data = ggsubset(!(show %in% highlight_shows)),
             colour = "black", alpha = 0.5, shape = ".") +
  # Apply our new stat for trendline
  stat_rollgaus(colour = "black") +
  # Do the same for highlighted data
  geom_point(data = ggsubset(show %in% highlight_shows),
             aes(colour = show),
             alpha = 0.5, shape = ".") +
  stat_rollgaus(data = ggsubset(show %in% highlight_shows),
               aes(colour = show)) +
  # Manual legend
  geom_text(data = legend_anno, 
            aes(label = show, group = -1, colour = show),
            size = 3) +
  # Annotate the bit of text plus an arrow
  annotate("textBox", x = as.Date("1985-01-01"), y = 200, 
           label = my_note, hjust = 0, size = 3, fill = NA, colour = NA) +
  annotate("curve", 
          x = as.Date("1990-01-01"), xend = as.Date("1995-01-01"),
          y = 120, yend = 60, colour = colours[1], size = 0.3,
          arrow = arrow(length = unit(2, "mm"), type = "closed", angle = 15)) +
  # Set scale to something sensible
  scale_y_continuous(trans = "log10", 
                     name = "Average Ticket Price",
                     limits = c(20, 300),
                     oob = scales::squish,
                     expand = c(0,0),
                     guide = "axis_logticks") +
  scale_x_date(name = "") +
  scale_colour_manual(values = rev(colours), 
                      breaks = highlight_shows,
                      limits = highlight_shows, 
                      labels = legendtext,
                      na.value = "black",
                      guide = "none",
                      name = "") +
  my_theme + theme(aspect.ratio = 1, 
                   axis.ticks.length = unit(5, "pt"))

# Start versus end plot ---------------------------------------------------

startend <- 
  ggplot(startend_data, aes(group = interaction(show, theatre))) +
  geom_segment(
    aes(x = 1, xend = 2,
        y = start_price, yend = end_price,
        alpha = log(n),
        alt_colour = end_price - start_price)
  ) +
  # Because the data is parameterised with start- and end- columns for the 
  # segment geom, we'll need 2 violins
  geom_violin(aes(x = 0.89, y = start_price, 
                  group = increasing,
                  colour = increasing,
                  fill = after_scale(colour)),
              width = 0.2, scale = "count") +
  geom_violin(aes(x = 2.11, y = end_price, 
                  group = increasing,
                  colour = increasing,
                  fill = after_scale(colour)),
              width = 0.2, show.legend = FALSE, scale = "count") +
  # Because we're using the colour two times
  scale_listed(scalelist = list(
    scale_colour_gradientn(aesthetics = "alt_colour",
                           colours = rev(colours),
                           limits = c(-0.3, 0.3), oob = scales::squish,
                           guide = "none"),
    scale_colour_manual(values = c(tail(colours, 1), head(colours, 1)),
                        name = "", labels = c("Decreasing", "Increasing"),
                        guide = "none")
  ), replaces = c("colour", "colour")) +
  # Set sensible axes
  scale_x_continuous(name = "", breaks = c(1, 2),
                     labels = c("First 20 shows", "Last 20 shows")) +
  scale_y_continuous(name = "Average price relative to median",
                     labels = scales::percent_format(),
                     breaks = c(0.6, 0.8, 1, 1.2, 1.4)) +
  guides(alpha = "none") +
  my_theme +  theme(aspect.ratio = (1 + sqrt(5))/2)

# Patchwork combining -----------------------------------------------------

title <- paste0(
  "<span style='color:", "white", ";'>", "Musicals ", "</span>",
  "<span style='color:", colours[1], ";'>", "**become more expensive**", 
  "</span>",
  "<span style='color:", "white", ";'>", " as shows get cheaper", "</span>"
)

subtitle <- paste0(
  "<span style='color:", "white", ";'>", "Long term price trend increases",
  ", but aging shows trend to </span>",
  "<span style='color:", colours[7], ";'>", "**become cheaper**", "</span>", 
  "<span style='color:", "white", ";'>.</span>"
)

pw <- (timeplot | startend) +
  plot_annotation(
    title = title,
    subtitle = subtitle,
    caption = "Shows running >1 year in the same theatre without large interruptions | Data from Playbill",
    theme = theme(plot.background = element_rect(fill = "grey20", colour = NA),
                  plot.subtitle = element_markdown(),
                  plot.caption = element_text(colour = "grey95"),
                  plot.title = element_markdown())
  )

# Saving ------------------------------------------------------------------

ggsave("figures/2020_04_28_Broadway_Grosses.png", bg = "grey20")

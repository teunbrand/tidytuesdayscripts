# Library statements ------------------------------------------------------

library(tidyverse)
library(countrycode)
library(ggtext)

# File import -------------------------------------------------------------

transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

# Functions ---------------------------------------------------------------

# Custom guide functions for truncating the bits past the last breaks

# Constructor
guide_truncated <- function(...) {
  guide <- guide_axis(...)
  class(guide) <- c("guide", "axis_trunc", "axis")
  guide
}

# Grob generator
guide_gengrob.axis_trunc <- function(guide, theme) {
  newguide <- NextMethod()
  if (guide$position %in% c("bottom", "top")) {
    newguide$children[[1]]$x <-
      unit(range(guide$key[[1]]), "npc")
  } else {
    newguide$children[[1]]$y <-
      unit(range(guide$key[[1]]), "npc")
  }
  newguide
}

# Data cleaning -----------------------------------------------------------

df <- transit_cost %>%
  mutate(country = ifelse(country == "UK", "GB", country),
         tunnel_per = as.numeric(gsub("\\%", "", tunnel_per)),
         start_year = as.numeric(start_year),
         end_year   = as.numeric(end_year),
         real_cost  = as.numeric(real_cost))

# Adding continent information --------------------------------------------

df <- codelist %>%
  select(c("ecb", "country.name.en", 
           "continent", "un.regionintermediate.name")) %>%
  left_join(df, ., by = c("country" = "ecb")) %>%
  mutate(continent = ifelse(is.na(un.regionintermediate.name), 
                            continent, un.regionintermediate.name),
         continent = ifelse(continent == "Americas", 
                            "North America", continent))

# Filtering and arranging -------------------------------------------------

df <- df %>%
  filter(!is.na(country) & !is.na(start_year) & !is.na(end_year)) %>%
  arrange(start_year, cost_km_millions) %>%
  group_by(continent) %>%
  mutate(ID = match(e, unique(e)))

# Plotting ----------------------------------------------------------------

g <- ggplot(df, aes(start_year, ID)) +
  geom_rect(aes(xmin = start_year, xmax = end_year,
                ymin = ID - 0.5, ymax = ID + 0.5, fill = cost_km_millions)) +
  scale_fill_distiller(palette = "BuPu", direction = 1,
                       limits = c(0, 400), breaks = seq(0, 400, by = 100),
                       labels = c(0, 100, 200, 300, expression("">=400)),
                       oob = scales::oob_squish,
                       name = "Cost per km<br>*Million $*") +
  scale_x_continuous(limits = c(2000, NA), name = "Year",
                     guide = "truncated",
                     oob = scales::oob_keep) +
  # Need to expand y-axis to not have strip labels clipped
  scale_y_continuous(
    expand = c(0, 5), name = NULL,
    breaks = function(x) {x + c(5.5, -5.5)},
    labels = function(x) {c("", x[2])},
    guide = "truncated"
  ) +
  facet_grid(fct_infreq(continent) ~ ., 
             scales = "free_y", space = "free_y", switch = "y") +
  labs(
    title = "Urban Transport Projects",
    subtitle = "Their lifespans and costs",
    caption = 'Data | *Transit Costs Project*'
  ) +
  theme_classic() +
  theme(
    axis.line = element_line(lineend = "round"),
    axis.text = element_text(colour = "black"),
    axis.title.x.bottom = element_text(hjust = 0),
    axis.ticks = element_line(colour = "black"),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0, hjust = 1),
    strip.background = element_blank(),
    legend.justification = "top",
    legend.title = element_markdown(lineheight = unit(1.2, "pt")),
    text = element_text(family = "Noto Sans"),
    plot.title = element_text(family = "Roboto", face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_text(face = "italic"),
    plot.caption.position = "plot",
    plot.caption = element_markdown()
  )

# Saving ------------------------------------------------------------------

ragg::agg_png("figures/2020_01_04_Transit_Costs.png",
              width = 1200, height = 1600,
              res = 300)
print(g)
dev.off()




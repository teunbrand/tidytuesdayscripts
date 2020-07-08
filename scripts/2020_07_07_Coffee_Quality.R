# Library statements ------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)
library(Matrix)
library(igraph)
library(Rtsne)
library(elementalist)
library(ggh4x)
library(patchwork)
library(systemfonts)
library(grid)


# Bioconductor
library(BiocNeighbors)

# Data import -------------------------------------------------------------

coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

# Functions ---------------------------------------------------------------

cos_sim <- function(x) {
  x <- x / sqrt(Matrix::rowSums(x * x))
  x <- Matrix::tcrossprod(x, x)
  x
}

cos_dist <- function(x) {
  as.dist(1 - cos_sim(x))
}

run_tSNE <- function(x, perplexity = 30, eta = 200, max_iter = 1000) {
  init <- x[, 1:2]
  dist <- cos_dist(x)
  tsne <- Rtsne::Rtsne(
    dist, is_distance = TRUE, Y_init = x[, 1:2],
    perplexity = perplexity, eta = eta, max_iter = max_iter
  )
  tsne <- as.data.frame(tsne$Y)
  colnames(tsne) <- c("tsne_x", "tsne_y")
  return(tsne)
}

run_SNN_graph <- function(x, k) {
  knn <- findKNN(x, k = k, BNPARAM = KmknnParam(), 
                 get.distance = FALSE, warn.ties = FALSE)
  snn <- scran:::build_snn_rank(knn$index)
  edges <- snn[[1]]
  weights <- snn[[2]]
  g <- make_graph(edges, directed = FALSE)
  E(g)$weights <- weights
  g <- simplify(g, edge.attr.comb = "first")
  return(g)
}


# Data wrangling ----------------------------------------------------------

# Filter out 1 weird coffee
coffee_ratings <- coffee_ratings %>% 
  filter(rank(total_cup_points) > 1)

raw_metrics <- coffee_ratings %>%
  select(aroma:moisture) 

# Data processing ---------------------------------------------------------

metrics <- raw_metrics
metrics[] <- lapply(metrics, function(x){scales::squish(scale(x)[,1],
                                                        c(-4, 4))})
mat <- as.matrix(metrics)
mat <- preprocessCore::normalize.quantiles(mat)

# PCA ---------------------------------------------------------------------

pca <- prcomp(mat, scale = FALSE, center = FALSE)
varexp <- pca$sdev^2/sum(pca$sdev^2)
pca <- pca$x[, 1:7]

# tSNE --------------------------------------------------------------------

set.seed(0)
knn <- findKNN(pca, k = 31, BNPARAM = KmknnParam(), 
               get.distance = TRUE, warn.ties = FALSE)

tsne <- Rtsne_neighbors(knn$index, knn$distance, perplexity = 30, eta = 50,
                        Y_init = pca[, 1:2], max_iter = 5000)
tsne <- data.frame(tsne_x = tsne$Y[, 1], tsne_y = tsne$Y[, 2])

# Clustering --------------------------------------------------------------

graph <- run_SNN_graph(pca, 10)
clust <- cluster_louvain(graph, weights = E(graph)$weights)$membership

# Setup plots -------------------------------------------------------------

aliases <- c(A = "Best", B = "Dry", C = "Medium", D = "Flavourful", 
             E = "Tasteless", F = "Non-Uniform", G = "Robusta")

dat <- cbind(coffee_ratings, tsne, clust = LETTERS[clust]) %>%
  select(colnames(metrics), tsne_x, tsne_y, clust, species, total_cup_points) %>%
  mutate(alias = aliases[clust])

tsnescales <- list(
  scale_x_continuous(name = 'tSNE 1', breaks = NULL),
  scale_y_continuous(name = 'tSNE 2', breaks = NULL),
  coord_equal(clip = "off")
)

my_theme <- theme(
  text = element_text(family = "Kalam", colour = "#4B1F01"),
  panel.background = element_rect_wiggle(1, fill = "#FCDAB4AA", 
                                         colour = "#E9C8A1"),
  strip.text = element_text(colour = "#4B1F01"),
  strip.background = element_blank(),
  axis.ticks = element_line(colour = "#4B1F01"),
  axis.text = element_text(colour = "#4B1F01"),
  axis.title.x.bottom = element_text(vjust = 0),
  axis.title = element_text(colour = "#4B1F01"),
  panel.grid.minor = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.major.x = element_line_wiggle(1, colour = "#E9C8A1"),
  plot.background = element_rect(fill = NA, colour = NA)
)

# tSNE plot ---------------------------------------------------------------

tsneplot <- ggplot(dat, aes(tsne_x, tsne_y)) +
  geom_point(aes(colour = alias), size = 1) +
  scale_colour_brewer(guide = "none", aesthetics = c("colour", "fill"),
                      palette = "Set1") +
  tsnescales +
  my_theme +
  theme( # Wiggle a bit more
    panel.background = element_rect_wiggle(5, fill = "#FCDAB4AA", 
                                           colour = "#E9C8A1"),
  )

# Violin plot -------------------------------------------------------------

# Pivot interesting columns
df <- dat %>% pivot_longer(aroma:moisture, names_to = "metric") %>%
  filter(metric %in% c("acidity", "aroma", "flavor", "moisture", "uniformity"))

violins <- ggplot(df, aes(value, alias, colour = alias, fill = alias)) +
  geom_violin(scale = "width", adjust = 1.5, size = 0.1) +
  scale_colour_brewer(guide = "none", aesthetics = c("colour", "fill"),
                      palette = "Set1") +
  facet_grid(~ tools::toTitleCase(metric), scales = "free_x") +
  scale_y_discrete(name = "") +
  scale_x_continuous(name = "Score", n.breaks = 4) +
  coord_cartesian(clip = "off") +
  force_panelsizes(rows = 2, cols = 1, respect = TRUE) +
  my_theme

# Combine plots -----------------------------------------------------------

combined <- tsneplot / violins + plot_layout(heights = c(1, 0.5)) +
  plot_annotation(caption = "Data | Coffee Quality Institute",
                  title = "All kinds of coffee!", 
                  subtitle = "Grouped based on quality metrics") &
  theme(plot.background = element_rect(fill = NA, colour = NA)) +
  theme(text = element_text(colour = "#4B1F01", family = "Kalam"),
        plot.title = element_text(family = "Permanent Marker", size = rel(1.5)),
        plot.subtitle = element_text(face = "bold"))
grob <- patchwork:::as_grob.patchwork(combined)


# Download background coffee ----------------------------------------------

download.file(
  "https://upload.wikimedia.org/wikipedia/commons/c/c5/Roasted_coffee_beans.jpg",
  tmp <- tempfile(fileext = ".jpg", pattern = "coffee"), method = "curl"
)
image <- jpeg::readJPEG(tmp)

# Blend image array with intended background
bg_rgb <- col2rgb("#E9C8A1")/255
image <- matrix(
  rgb(
    red = (image[, , 1] + bg_rgb[1])/2,
    green = (image[, , 2] + bg_rgb[2])/2,
    blue = (image[, , 3] + bg_rgb[3])/2
  ), dim(image)[1], dim(image)[2]
)
image <- t(image)
image[] <- alpha(image, alpha = 1 - scales::rescale(as.vector(row(image)), to = c(0.2, 0.8)))
rastr <- rasterGrob(image)


# Combine background with plots -------------------------------------------

final <- gtable::gtable_add_grob(
  grob, rastr, t = 1, l = 1, b = 41, r = 17, z = 0
)

# Save plots --------------------------------------------------------------

ragg::agg_supertransparent(
  "figures/2020_07_07_Coffee.png",
  width = 1200,
  height = 1600,
  res = 300,
  background = "#E9C8A1"
)
grid.newpage(); grid.draw(final)
dev.off()

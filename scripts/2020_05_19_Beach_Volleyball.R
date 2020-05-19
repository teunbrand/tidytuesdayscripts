# Libraries ---------------------------------------------------------------

library(tidyverse)
library(igraph)
library(ggraph)
library(ggforce)
library(scales)
library(patchwork)

# Data import -------------------------------------------------------------

vb_matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000)


# Split winners and losers ------------------------------------------------

win_teams <- vb_matches %>%
  mutate(p1 = pmin(w_player1, w_player2), # Sort alphabetically
         p2 = pmax(w_player1, w_player2)) %>%
  group_by(p1, p2) %>%
  summarise(n = n(), gender = unique(gender))

lose_teams <- vb_matches %>%
  mutate(p1 = pmin(l_player1, l_player2), # Sort alphabetically
         p2 = pmax(l_player1, l_player2)) %>%
  group_by(p1, p2) %>%
  summarise(n = -1 * n(), gender = unique(gender)) # Losses are encoded by negative values


# Combine winners and losers ----------------------------------------------

teams <- full_join(win_teams, lose_teams, by = c("p1", "p2")) %>%
  mutate(
    n.x = ifelse(is.na(n.x), 0, n.x),
    n.y = ifelse(is.na(n.y), 0, n.y),
    gender = ifelse(is.na(gender.x), gender.y, gender.x),
    gender.y = NULL, gender.x = NULL
  ) %>%
  mutate(net_wins = n.x + n.y,
         matches = n.x - n.y,
         prop_wins = n.x / matches) %>%
  filter(matches > 5)

# Individualise results ---------------------------------------------------

players <- teams %>%
  pivot_longer(cols = c("p1", "p2")) %>%
  group_by(player = value) %>%
  summarise(
    n.x = sum(n.x), 
    n.y = sum(n.y),
    gender = unique(gender)
  ) %>%
  mutate(net_wins = n.x + n.y,
         matches = n.x - n.y,
         prop_wins = n.x / matches)

# Create graph ------------------------------------------------------------

edgelist <- matrix(c(teams$p1, teams$p2), ncol = 2)
graph <- graph_from_edgelist(edgelist, directed = FALSE)

# Add edge attributes
E(graph)$weight <- teams$matches
E(graph)$prop <- teams$prop_wins
E(graph)$wins <- teams$n.x
E(graph)$losses <- teams$n.y
E(graph)$gender <- teams$gender

# Add vertex attributes
players <- players[match(V(graph)$name, players$player),]
V(graph)$wins <- players$n.x
V(graph)$losses <- players$n.y
V(graph)$gender <- players$gender

# Clustering --------------------------------------------------------------

# Weight by the log # of matches
louvain <- cluster_louvain(graph, weights = log10(E(graph)$weight))
V(graph)$community <- louvain$membership

# Get edges by community
num_edgelist <- get.edgelist(graph, names = FALSE)
left <- V(graph)$community[num_edgelist[, 1]]
right <- V(graph)$community[num_edgelist[, 2]]
teams$within_community <- E(graph)$community <- ifelse(
  left == right, left, 0
)

# Summarise clusters
clusters <- teams %>%
  filter(within_community != 0) %>%
  group_by(within_community) %>%
  summarise(
    n_teams = n(),
    wins = sum(n.x),
    losses = sum(n.y),
    matches = sum(matches),
    median_prop_wins = median(prop_wins),
    prop_wins = wins / matches,
    gender = unique(gender)
  )

# Decompose subgraphs -----------------------------------------------------

# Order largest subgraphs
subgraphs <- decompose(graph)
subgraph_len <- vapply(subgraphs, function(x){length(V(x))}, integer(1))
subgraphs <- subgraphs[order(subgraph_len, decreasing = TRUE)]

# Subgraph communities
communities <- c(V(subgraphs[[1]])$community, V(subgraphs[[2]])$community)
communities <- clusters %>%
  filter(within_community %in% communities,
         n_teams > 10) %>%
  group_by(gender) %>%
  summarise(
    best = within_community[which.max(median_prop_wins)],
    worst = within_community[which.min(median_prop_wins)]
  )

# Plot builder ------------------------------------------------------------

# Colours by the esteemed @RobinWeide
colours <- rev(c("#009BEF", "#38B1F2", "#71C7F6", "#AADDF9", "#E2F3FD", 
                 "#FFECEA", "#FFC8C2", "#FFA499", "#FF8071", "#FF5C49"))

build_graph <- function(graph, top) {
  
  ggraph(graph, layout = "stress") +
    geom_mark_ellipse(
      data = get_nodes(),
      aes(x = x, y = y, group = community,
          filter = community %in% c(top$best, top$worst)),
      fill = "#fed500", colour = NA, alpha = 0.1,
      expand = unit(0, "mm")
    ) +
    geom_edge_link(aes(colour = prop, alpha = weight),
                   edge_width = 0.2) +
    geom_mark_ellipse(
      data = get_nodes(),
      aes(x = x, y = y, group = community,
          filter = community %in% c(top$best, top$worst),
          label = paste0(ifelse(community %in% top$best, "Best", "Worst"),
                         " Median\nWin Proportion")),
      label.fill = "transparent", label.colour = "grey90", label.fontsize = 5,
      label.buffer = unit(1, "mm"),
      con.colour = "#fed500", colour = "#fed500", con.size = 0.2, size = 0.2,
      expand = unit(0, "mm"), con.cap = unit(0, "mm"), con.type = "straight"
    ) +
    scale_edge_colour_gradientn(
      name = "Matches\nWon",
      colours = colours,
      limits = c(0.3, 0.7),
      oob = scales::squish,
      breaks = c(0.3, 0.5, 0.7),
      labels = c(expression(""<="30%"), "50%", expression("">="70%")),
      guide = guide_colourbar(available_aes = "edge_colour")
    ) +
    scale_edge_alpha_continuous(trans = "log10", guide = "none") +
    coord_equal()
}

# Make graphs -------------------------------------------------------------

# Checked that this subgraph is male
males <- build_graph(subgraphs[[1]], top = communities) + 
  ggtitle("Men's teams") +
  scale_x_continuous(expand = c(0.05, 0, 0.2, 0))

females <- build_graph(subgraphs[[2]], top = communities) + 
  ggtitle("Women's teams") +
  theme(legend.position = "none")

# Combine graphs ----------------------------------------------------------

combined <- females + males + plot_layout(guides = "collect") +
  plot_annotation(
    title = "Team players in beach volleyball",
    subtitle = "Largest networks of players having participated in a team",
    caption = "Two player teams participating in at least three matches together | Data from FIVB tournaments & AVP",
    theme = theme(
      plot.background = element_rect(fill = "grey10", colour = NA),
      plot.title = element_text(size = rel(2), face = "bold"),
      plot.subtitle = element_text(size = rel(1.5), face = "italic")
    )
  ) &
  theme(plot.background = element_rect(fill = "grey10", colour = NA),
        panel.background = element_rect(fill =  "grey10", colour = NA),
        legend.background = element_rect(fill = "grey10"),
        text = element_text(colour = "grey90", size = 8))

ggsave("figures/2020_05_19_Beach_Volleyball.png", combined, "png", 
       width = 7, height = 4, 
       bg = "grey10")

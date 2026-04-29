## -----------------------------------------------------------------------------
##
## [ PROJ ] Electoral Violence Networks
## [ FILE ] data_analysis.R
## [ AUTH ] Maya A. Dalton; mad6821@psu.edu
## [ INIT ] 13 March 2025
##
## -----------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - The Pennsylvania State University/Dissertation/(2) Networks/scripts")

## libraries
libs <- c("haven", "readxl", "dplyr", "tidyverse", "ggplot2",
          "network", "sna", "intergraph", "igraph", "ggraph", 
          "tidygraph", "stringr", "latentnet", "vegan", "amen",
          "sbm","blockmodels", "pheatmap", "knitr", "kableExtra",
          "PRROC", "pROC")
sapply(libs, require, character.only = TRUE)

## paths (./scripts as working directory)
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, file.path(".."), args)
dat_dir <- file.path(root, "data")
#fig_dir <- file.path(root, "figures")
fig_path <- "~/Dropbox/Apps/Overleaf/Dalton EV Networks (CH. 2)/Figures"

# Load in ECAV dataset
ecav <- read_csv(file.path(dat_dir, "ecav_main.csv"))

## -----------------------------------------------------------------------------
## cleaning and subsetting ecav
## -----------------------------------------------------------------------------

# Create EV sum variable
ecav.c <- ecav %>%
  group_by(ActorNameNew, TargetNameNew) %>% # only actor/targets
  mutate(Actor1Type = case_when( # Recode NAs
           Actor1Type == -99 ~ NA,
           .default = as.numeric(Actor1Type)
         ),
         Target1Type = case_when( # Recode NAs
           Target1Type == -99 ~ NA,
           .default = as.numeric(Target1Type)
         )) %>%
  distinct() %>%
  drop_na(Actor1Type)

# Remove property/buildings/events
ecav.c <- ecav.c %>%
  subset(TargetNameNew != c("Building") & TargetNameNew != c("Property") & 
           TargetNameNew != c("Unknown") & ActorNameNew != c("Unknown") & 
           TargetNameNew != c("Event") & ActorNameNew != c("Event") &
           TargetNameNew != c("Civilians") & ActorNameNew != c("Civilians") &
           TargetNameNew != c("Protestors") & ActorNameNew != c("Protestors") &
           TargetNameNew != c("Students") & ActorNameNew != c("Students"))

## -------------------------------------
## list of country-years in ECAV
## -------------------------------------
ecav.temp <- ecav.c %>%
  mutate(
    election_year = format(as.Date(Electiondate, format="%Y/%m/%d"),"%Y"),
    election_year = as.numeric(election_year))

ecav.temp <- ecav.temp %>%
  group_by(country) %>%
  summarize(years = paste0(min(election_year), "–", max(election_year))) %>%
  ungroup()

cyears <- paste0(ecav.temp$country, " (", ecav.temp$years, ")")
cyears.file <- paste(cyears, collapse = ", ")

cat(cyears.file, file=file.path(fig_path, "cyears.tex"))

## -------------------------------------
## list of actors in ECAV
## -------------------------------------
nodes <- intersect(ecav.c$ActorNameNew, ecav.c$TargetNameNew)
nodes <- sort(nodes)
nodes.file <- paste(nodes, collapse = ", ")
cat(nodes, file=file.path(fig_path, "node-names.tex"))

## -----------------------------------------------------------------------------
## create network object
## -----------------------------------------------------------------------------

# Get unique actors and targets
actors <- unique(ecav.c$ActorNameNew)
targets <- unique(ecav.c$TargetNameNew)
all_nodes <- unique(c(actors, targets))

# Create empty adjacency matrix
ecav_adjc <- matrix(0, nrow = length(all_nodes), ncol = length(all_nodes),
                     dimnames = list(all_nodes, all_nodes))

# Fill in the adjacency matrix based on ev_count
for (i in 1:nrow(ecav.c)) {
  row_index <- match(ecav.c$ActorNameNew[i], all_nodes)
  col_index <- match(ecav.c$TargetNameNew[i], all_nodes)
  ecav_adjc[row_index, col_index] <- ecav.c$ev_count[i]
}

# Create directed igraph network
ecavG <- graph_from_adjacency_matrix(ecav_adjc, mode = "directed", weighted = TRUE)
V(ecavG)$name <- all_nodes

# Make the network object using `network`
ecavNet <- network(ecav_adjc, directed = TRUE)

## -----------------------------------------------------------------------------
## network visualization and centrality
## -----------------------------------------------------------------------------

## Basic Network -------------------------------------------
set.seed(5)
xy_cool <- layout_with_fr(ecavG, niter = 500)

ecavG_tbl <- as_tbl_graph(ecavG) # --- Convert to tidygraph ---
xy_df <- as.data.frame(xy_cool) # --- Add node data ---
colnames(xy_df) <- c("x", "y")

ecavG_tbl <- ecavG_tbl %>%
  activate(nodes) %>%
  mutate(
    x = xy_df$x,
    y = xy_df$y,
    name = V(ecavG)$name,
    label_wrap = str_wrap(V(ecavG)$name, width = 15),
  )

# --- Plot ---
png(file.path(fig_path, "ev_network.png"), width = 8, height = 8, units = "in", res = 300)
ggraph(ecavG_tbl, layout = "manual", x = x, y = y) +
  geom_edge_link(alpha = 0.6, color = "grey80") +  
  geom_node_point(size = 10, color = "black", shape = 21, fill = "seagreen1") +
  geom_node_text(
    aes(label = label_wrap),
    size = 2,
    color = "black",
    lineheight = 0.75
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA)
  )
dev.off()

## Degree Centrality -------------------------------------------
deg <- degree(ecavG, mode = "all")

set.seed(5)
xy_cool <- layout_with_fr(ecavG, niter = 500)

ecavG_tbl <- as_tbl_graph(ecavG) # --- Convert to tidygraph ---
xy_df <- as.data.frame(xy_cool) # --- Add node data ---
colnames(xy_df) <- c("x", "y")

ecavG_tbl <- ecavG_tbl %>%
  activate(nodes) %>%
  mutate(
    x = xy_df$x,
    y = xy_df$y,
    name = all_nodes,
    degree = deg,
    label_wrap = str_wrap(all_nodes, width = 15)
  )

# --- Plot ---
png(file.path(fig_path, "ev_centrality.png"), width = 8, height = 8, units = "in", res = 300)
ggraph(ecavG_tbl, layout = "manual", x = x, y = y) +
  geom_edge_link(alpha = 0.3, color = "grey80") +
  geom_node_point(
    aes(size = degree / 2),
    color = "black",
    fill = "seagreen3",
    shape = 21,
    stroke = 0.5
  ) +
  geom_node_text(
    aes(label = label_wrap),
    size = 2,
    color = "black",
    lineheight = 0.75,   # tighter wrapped text spacing
  ) +
  scale_size_continuous(range = c(2, 10)) +  # control node size scaling
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA)
  )
dev.off()

## Eigenvector Centrality -------------------------------------------
eigen <- evcent(ecavG) # --- Compute eigenvector centrality ---

set.seed(5)
xy_cool <- layout_with_fr(ecavG, niter = 500)

ecavG_tbl <- as_tbl_graph(ecavG) # --- Convert to tidygraph ---
xy_df <- as.data.frame(xy_cool)  # --- Add node data ---
colnames(xy_df) <- c("x", "y")

xy_df2 <- xy_df[which(V(ecavG)$name %in% actors), ]
rownames(xy_df2) <- NULL  # ensure proper alignment

ecavG_tbl <- ecavG_tbl %>% 
  filter(name %in% actors)

ecavG_tbl <- ecavG_tbl %>%
  activate(nodes) %>%
  mutate(
    x = xy_df2$x,
    y = xy_df2$y,
    name = actors,
    eigen_centrality = eigen$vector[V(ecavG)$name %in% actors],
    label_wrap = str_wrap(actors, width = 15)
  )

# --- Plot ---
png(file.path(fig_path, "ev_eigen.png"), width = 8, height = 8, units = "in", res = 300)
ggraph(ecavG_tbl, layout = "manual", x = x, y = y) +
  geom_edge_link(alpha = 0.3, color = "grey80") +
  geom_node_point(
    aes(size = eigen_centrality * 10),
    color = "black",
    fill = "seagreen3",
    shape = 21,
    stroke = 0.5
  ) +
  geom_node_text(
    aes(label = label_wrap),
    size = 2,
    color = "black",
    lineheight = 0.75
  ) +
  scale_size_continuous(range = c(2, 12)) +  # smooth scaling for eigenvector size
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA)
  )
dev.off()

## -----------------------------------------------------------------------------
## modeling - SBM
## -----------------------------------------------------------------------------

# Prepare adjacency matrix
A <- ifelse(ecav_adjc > 0, 1, 0)

# Fit directed SBM
set.seed(123)

fit_sbm <- estimateSimpleSBM(
  netMat = A,
  model = "bernoulli",
  directed = TRUE,
  estimOptions = list(nbBlocks = 3)
)

# Extract ICL values across K for appendix table
icl_vals <- fit_sbm$storedModels[, c("nbBlocks", "ICL")]
colnames(icl_vals) <- c("K", "ICL")

icl_tab <- icl_vals %>%
  kable(
    col.names = c("\\textit{K} (Blocks)", "ICL"),
    caption = "ICL Values Across SBM Specifications ($K = 2$ to $K = 6$)",
    align = c("c", "r"),
    booktabs = TRUE,
    escape = FALSE,
    format = "latex"
  )

writeLines(icl_tab, file.path(fig_path, "sbm_icl_tab.tex"))

# Extract group memberships - parsing to 3 blocks bc ICL reporting shows 4 is not substantively meaningful
membership_3 <- fit_sbm$memberships
membership_3[membership_3 == 4] <- 3

membership_df <- data.frame(
  node = rownames(A),
  block = membership_3
)

### --------------------------------------------------------------------------

# Convert igraph to tidygraph & attach SBM block membership
ecavG_tbl <- as_tbl_graph(ecavG) %>%
  activate(nodes) %>%
  mutate(block = as.character(membership_df$block[match(name, membership_df$node)]),
         label_wrap = str_wrap(network::get.vertex.attribute(ecavNet, "vertex.names"), width = 12))

# Prepare block centroids (as character too)
num_blocks <- length(unique(ecavG_tbl %N>% pull(block)))
theta <- seq(0, 2*pi, length.out = num_blocks + 1)[- (num_blocks + 1)]
block_centroids <- data.frame(
  block = as.character(1:num_blocks),  # make character to match nodes
  x = cos(theta),
  y = sin(theta)
)

set.seed(123)

# Join centroid positions to nodes
ecavG_tbl <- ecavG_tbl %>%
  activate(nodes) %>%
  left_join(block_centroids, by = "block") %>%
  mutate(
    x_jitter = x + rnorm(n(), sd = 0.1),
    y_jitter = y + rnorm(n(), sd = 0.1)
  )


layout_manual <- create_layout(ecavG_tbl, layout = "manual",
                               x = ecavG_tbl %N>% pull(x_jitter),
                               y = ecavG_tbl %N>% pull(y_jitter))

### Table ------------------------------------------------------------------
label_cluster_tbl <- ecavG_tbl %>%
  activate(nodes) %>%
  as_tibble() %>%
  select(name, block) %>%
  arrange(block)

sbm_clusters_tab <- label_cluster_tbl %>%
  kable(
    col.names = c("Actor", "Block"),
    caption = "Actor Labels and SBM Blocks",
    align = c("l", "c"),
    booktabs = TRUE
  )

writeLines(sbm_clusters_tab, file.path(fig_path, "sbm_clusters_tab.tex")) 

## Plot --------------------------------------------------
png(file.path(fig_path, "ev_SBM.png"), width = 8, height = 8, units = "in", res = 300)
ggraph(layout_manual) +
  geom_edge_link(alpha = 0.3, color = "grey70") +
  geom_node_point(
    aes(fill = block, color = block,
        shape = factor(block)),
    size = 5,
    stroke = 0.8
  ) +
  geom_node_text(aes(label = label_wrap), size = 1.8) +  
  scale_shape_manual(
    values = c(21, 22, 24, 25, 23),
    name = "SBM Block"
  ) +
  scale_fill_brewer(palette = "Set2", name = "SBM Block") +
  scale_color_brewer(palette = "Set2", name = "SBM Block") +
  theme_void() + 
  theme(legend.position = "bottom")
dev.off()

## -----------------------------------------------------------------------------
## modeling - Cross-block exposure (actor-level hybridity)
## -----------------------------------------------------------------------------

# extract memberships
block <- membership_3
K <- max(block)
nodes <- rownames(A)

# Cross-block exposure matrix
exposure <- matrix(0, nrow = nrow(A), ncol = K)
rownames(exposure) <- nodes
colnames(exposure) <- paste0("Block_", 1:K)

for (i in seq_len(nrow(A))) {
  neighbors <- which(A[i, ] == 1)
  if (length(neighbors) > 0) {
    exposure[i, ] <- table(factor(block[neighbors], levels = 1:K))
  }
}

# Normalize to proportions
exposure <- exposure / rowSums(exposure)
exposure[is.na(exposure)] <- 0

colnames(exposure) <- c("Operational", "Strategic", "Structural")

## Heatmap Plot --------------------------------------------------
# This shows how much each actor interacts with each block.
# Dark red (i.e., Taliban) - behavior is tightly focused within one functional domain
# Blended boxes (i.e., party youth) - These actors occupy intermediate positions between blocks
# 2/3 boxes - interacting heavily with both political and militant actors
png(file.path(fig_path, "cross_block_heatmap.png"), width = 8, height = 8, units = "in", res = 300)
pheatmap(
  exposure,
  cluster_rows = TRUE,
  cluster_cols = FALSE,
  color = colorRampPalette(c("white", "firebrick"))(50),
  fontsize_row = 6
  #main = "Cross-Block Exposure (Actor-Level Hybridity)"
)
dev.off()

## Entropy Plot --------------------------------------------------
# This compresses hybridity into one interpretable number per actor.
entropy <- -rowSums(exposure * log(exposure + 1e-10))

entropy_df <- data.frame(
  node = nodes,
  entropy = entropy,
  block = factor(block)
)

entropy_df <- entropy_df %>%
  mutate(block = case_when(
    block == 1 ~ "Operational",
    block == 2 ~ "Structural",
    block == 3 ~ "Strategic",
  ))

png(file.path(fig_path, "entropy_boxplot.png"), width = 8, height = 8, units = "in", res = 300)
ggplot(entropy_df, aes(x = block, y = entropy, fill = block)) +
  geom_boxplot(alpha = 0.7, outlier.size = 1) +
  theme_minimal() +
  labs(
    #title = "Actor-Level Role Hybridity by SBM Block",
    x = "SBM Block",
    y = "Cross-Block Exposure Entropy"
  ) +
  theme(legend.position = "none")
dev.off()

# Wilcoxon test: structural vs operational block entropy
structural_entropy <- entropy_df$entropy[entropy_df$block == "Structural"]
operational_entropy <- entropy_df$entropy[entropy_df$block == "Operational"]

wilcox_result <- wilcox.test(structural_entropy, operational_entropy,
                             alternative = "greater")
print(wilcox_result)

# IQR summary by block
entropy_df %>%
  group_by(block) %>%
  summarize(
    median_entropy = median(entropy),
    q25 = quantile(entropy, 0.25),
    q75 = quantile(entropy, 0.75)
  )

## Stacked Bar Plot --------------------------------------------------
# Party-linked actors as bridges - 
top_entropy_nodes <- entropy_df %>%
  arrange(desc(entropy)) %>%
  slice(1:15) %>%
  pull(node)

exposure_long <- exposure[top_entropy_nodes, ] %>%
  as.data.frame() %>%
  mutate(node = rownames(.)) %>%
  pivot_longer(
    cols = c("Operational", "Strategic", "Structural"),
    names_to = "block_exposed",
    values_to = "share"
  )

png(file.path(fig_path, "cross_block_bar.png"), width = 8, height = 8, units = "in", res = 300)
ggplot(exposure_long, aes(x = node, y = share, fill = block_exposed)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(
    #title = "Cross-Block Exposure of Hybrid Actors",
    x = "",
    y = "Share of Interactions",
    fill = "Block"
  ) + 
  scale_fill_brewer(palette = "Set2")
dev.off()

ggplot(exposure_long, aes(x = node, y = share, fill = factor(block_exposed))) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +  # black border for contrast
  coord_flip() +
  theme_minimal() +
  labs(
    x = "",
    y = "Share of Interactions",
    fill = "Block"
  ) + 
  scale_fill_grey(start = 0.9, end = 0.3)  # light to dark grey for blocks

## Electoral violence is structured around latent roles, but these roles are actively 
## bridged by party-linked, activist, and security actors who operate across organizational and coercive domains.

## -----------------------------------------------------------------------------
## modeling - LSM no covariates
## -----------------------------------------------------------------------------

network::set.vertex.attribute(ecavNet, "ev_count", ecav.c$ev_count) # ev count

### Fit model with Latent Effects (no covars) -----------------------------
set.seed(5)
system.time(est2d <- ergmm(ecavNet ~ euclidean(d=2), 
                           control=ergmm.control(sample.size=40000, interval=10)))

mcmc.diagnostics(est2d)
summary(est2d)

# Extract posterior mean latent positions
Z_df <- as.data.frame(est2d$mkl$Z)
colnames(Z_df) <- c("x", "y")

# k-means on posterior means
set.seed(12345)
est.km <- kmeans(Z_df, centers = 3)

# Add clusters to latent positions
Z_df <- Z_df %>%
  mutate(
    cluster = factor(est.km$cluster)
  )

# Cluster centers
centers_df <- as.data.frame(est.km$centers)
colnames(centers_df) <- c("x", "y")
centers_df$cluster <- factor(1:nrow(centers_df))

# Attach latent positions + clusters to the graph
ecavG_tbl <- as_tbl_graph(ecavNet) %>%
  activate(nodes) %>%
  mutate(
    x = Z_df$x,
    y = Z_df$y,
    cluster = Z_df$cluster,
    ev_count = network::get.vertex.attribute(ecavNet, "ev_count"),
    label_wrap = str_wrap(
      network::get.vertex.attribute(ecavNet, "vertex.names"),
      width = 12
    )
  )


### Plotting ------------------------------------------------------------------
png(file.path(fig_path, "ev_LSM.png"), width = 8, height = 8, units = "in", res = 300)
ggraph(ecavG_tbl, layout = "manual", x = x, y = y) +
  
  # Edges
  geom_edge_link(color = "grey80", alpha = 0.3) +
  
  # Nodes colored by k-means cluster
  geom_node_point(
    aes(size = ev_count, fill = cluster),
    shape = 21,
    color = "black",
    stroke = 0.4,
    alpha = 0.9
  ) +
  
  # Cluster centers
  geom_point(
    data = centers_df,
    aes(x = x, y = y),
    inherit.aes = FALSE,
    shape = 4,          # "+"
    size = 6,
    stroke = 1.2,
    color = "black"
  ) +
  
  # Labels
  geom_node_text(
    aes(label = label_wrap),
    size = 2.5,
    lineheight = 0.75,
    repel = TRUE
  ) +
  
  scale_size_continuous(range = c(3, 10)) +
  scale_fill_brewer(palette = "Set2") +
  
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  
  labs(
    title = "Posterior Mean Latent Positions (2D LSM)",
    subtitle = "Nodes colored by k-means clusters; crosses denote cluster centers"
  )
dev.off()



### Visualize GOF --------------------------------------------------
par(mfrow=c(2,2))
plot(gof(est2d,GOF=~idegree+odegree+distance+esp))

### Confusion Matrix --------------------------------------------------
y_star <- predict(est2d, type="pmean")
y <- as.matrix(summary(est2d)$model$Yg)
cm <- table(prediction = y_star[lower.tri(y_star)] > 0.5,
            truth = y[lower.tri(y)] == 1)
print(cm)

## -----------------------------------------------------------------------------
## modeling - LSM with covariates
## -----------------------------------------------------------------------------

### Fit model with Latent Effects (covars) -----------------------------

system.time(est2dc <- ergmm(ecavNet ~ euclidean(d=2) + nodecov("ev_count"),
                            control = ergmm.control(sample.size=50000,interval=10)))
#mcmc.diagnostics(est2dc)
summary(est2dc)

# Extract posterior mean latent positions
Z_df <- as.data.frame(est2dc$mkl$Z)
colnames(Z_df) <- c("x", "y")

# k-means on posterior means
set.seed(12345)
est.km <- kmeans(Z_df, centers = 3)

# Add clusters to latent positions
Z_df <- Z_df %>%
  mutate(
    cluster = factor(est.km$cluster)
  )

# Cluster centers
centers_df <- as.data.frame(est.km$centers)
colnames(centers_df) <- c("x", "y")
centers_df$cluster <- factor(1:nrow(centers_df))

# Attach latent positions + clusters to the graph
ecavG_tbl <- as_tbl_graph(ecavNet) %>%
  activate(nodes) %>%
  mutate(
    x = Z_df$x,
    y = Z_df$y,
    cluster = Z_df$cluster,
    ev_count = network::get.vertex.attribute(ecavNet, "ev_count"),
    label_wrap = str_wrap(
      network::get.vertex.attribute(ecavNet, "vertex.names"),
      width = 12
    )
  )


### Table ------------------------------------------------------------------
label_cluster_tbl <- ecavG_tbl %>%
  activate(nodes) %>%
  as_tibble() %>%
  select(name, cluster) %>%
  arrange(cluster)

lsm_clusters_tab <- label_cluster_tbl %>%
  kable(
    col.names = c("Actor", "Cluster"),
    caption = "Actor Labels and K-Means Cluster Assignments",
    align = c("l", "c"),
    booktabs = TRUE
  )

writeLines(lsm_clusters_tab, file.path(fig_path, "lsm_clusters_tab.tex")) 


### Plotting ------------------------------------------------------------------
png(file.path(fig_path, "ev_LSM_cov.png"), width = 8, height = 8, units = "in", res = 300)
ggraph(ecavG_tbl, layout = "manual", x = x, y = y) +
  
  # Edges 
  geom_edge_link(color = "grey80", alpha = 0.3) +
  
  # Nodes colored by k-means cluster
  geom_node_point(
    aes(size = ev_count, fill = cluster, 
        shape = factor(cluster)),
    color = "black",
    stroke = 0.4,
    alpha = 0.9
  ) +
  
  # Cluster centers
  geom_point(
    data = centers_df,
    aes(x = x, y = y),
    inherit.aes = FALSE,
    shape = 4,          # "+"
    size = 6,
    stroke = 1.2,
    color = "black"
  ) +
  
  # Labels
  geom_node_text(
    aes(label = label_wrap),
    size = 2.5,
    lineheight = 0.75,
    repel = TRUE
  ) +
  
  scale_size_continuous(range = c(3, 10)) +
  scale_shape_manual(values = c(21, 22, 24, 25, 23)) +
  scale_fill_brewer(palette = "Set2") +
  
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  
  labs(
    title = "Posterior Mean Latent Positions (2D LSM with Covariates)",
    subtitle = "Nodes colored by k-means clusters; crosses denote cluster centers"
  )
dev.off()



### Visualize GOF --------------------------------------------------
par(mfrow=c(2,2))
plot(gof(est2dc,GOF=~idegree+odegree+distance+esp))

### Confusion Matrix --------------------------------------------------
y_star <- predict(est2dc, type="pmean")
y <- as.matrix(summary(est2dc)$model$Yg)
cm <- table(prediction = y_star[lower.tri(y_star)] > 0.5,
            truth = y[lower.tri(y)] == 1)
print(cm)

### Comparing Model 1 and 2
est.bic <- sapply(list(est2d, est2dc), bic.ergmm)
colnames(est.bic) <- paste("Model", 1:2)
est.bic

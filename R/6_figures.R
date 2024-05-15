## FIGURES: FOOD WEB AMP NAMUNCURÁ-BANCO BURDWOOD
# Author: Tomás Ignacio Marina
# Date: 18/08/2022 - 14/11/2023


# Load packages ----------------------------------------------------------------

packages <- c("tidyverse", "ggplot2", "ggjoy", "ggpubr", "scales", 
              "multiweb", "igraph", "grid")
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
ipak(packages)


# Load previous results ---------------------------------------------------

load("results/summary_results_jul23.rda")
load("data/foodweb-data_jul23.rda")


# Figures -----------------------------------------------------------------

## Figure 2 ----

col <- RColorBrewer::brewer.pal(11, "RdYlGn") # PuOr, RdBu, RdYlBu
par(cex.axis=0.8, cex.lab=0.8)
figure_2 <- plot_troph_level(g, vertexLabel = F, edge.arrow.size = .1, modules = F,
                             tk = F, bpal = col, ylab = "Trophic level")


## Figure 3 ----

### Network degree ----
degree <- as.data.frame(degree(g, mode = "total"))
plot_totdeg <- ggplot(degree, aes(degree[,1])) +
  geom_histogram(bins = 100, alpha = 0.3, color = "black") +
  labs(x = "Degree (total interactions)", y = "Frequency") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10))

### Predator degree ----
# Generality (generalistas vs especialistas)
plot_preddeg <- spp_total %>% 
  filter(NumPrey != 0) %>% 
  ggplot(., aes(x = NumPrey)) +
  geom_histogram(stat = "count") +
  labs(x = "Predators", y = "Number of prey") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10)) #+
# annotate("text", x = Inf, y = Inf, label = paste("Prey per predator = ", round(prop_topol$Generality, 2), sep = ""),
#          size = 5, vjust=1, hjust=1))

### Prey degree ----
# Vulnerability (energy flows)
plot_preydeg <- spp_total %>% 
  filter(NumPred != 0) %>% 
  ggplot(., aes(x = NumPred)) +
  geom_histogram(stat = "count") +
  labs(x = "Prey", y = "Number of predators") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10))

# Arrange figure
plot_grpdeg <- spp_total %>% 
  add_count(FunctionalGroup) %>% 
  filter(n > 2) %>% 
  group_by(FunctionalGroup) %>% 
  mutate(TL_fg = mean(TL)) %>% 
  mutate(color = case_when(TL_fg <= 1 ~ "#006837",
                           between(TL_fg, 1.1, 1.95) ~ "#1A9850",
                           between(TL_fg, 1.98, 2.05) ~ "#66BD63",
                           between(TL_fg, 2.06, 2.25) ~ "#A6D96A",
                           between(TL_fg, 2.26, 2.5) ~ "#D9EF8B",
                           between(TL_fg, 2.51, 3.0) ~ "#FFFFBF",
                           between(TL_fg, 3.001, 3.25) ~ "#FEE08B",
                           between(TL_fg, 3.001, 3.25) ~ "#FDAE61",
                           between(TL_fg, 3.51, 4.0) ~ "#F46D43",
                           between(TL_fg, 4.01, 4.49) ~ "#D73027",
                           TL_fg >= 4.5 ~ "#A50026")) %>% 
  ggplot(., aes(x = TotalDegree, y = reorder(FunctionalGroup, TL_fg))) + 
  geom_joy(aes(scale = 3.75, fill = color)) +
  scale_fill_identity() +
  labs(x = "Number of interactions", y = "Functional group", tag="D") +
  theme_joy(grid = FALSE) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10),
        plot.tag = element_text(face = "bold", size = 10),
        plot.tag.position = c(0.05, 1))

# Arrange figure
a <- plot_totdeg
b <- plot_preddeg
c <- plot_preydeg
d <- plot_grpdeg

# Nested ggarrange
figure_3 <- ggarrange(a,                                                                                         # First row
                      ggarrange(b, c, ncol = 2, labels = c("B", "C"), font.label=list(color="black",size=10)),   # Second row
                      ggarrange(d, ncol = 1),                                                                    # Third row
                      nrow = 3, labels = "A",
                      font.label=list(color="black",size=10))


## Figure 4 ----

### Betweenness ----
scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}
plot_sp_bt <- spp_total %>% 
  filter(TL >= 2) %>% 
  ggplot(., aes(x = TL, y = Between)) +
  geom_point(size=0.3) +
  scale_y_log10(label = scientific_10) +
  #scale_y_continuous(label = scientific_10)
  xlim(2, 5) +
  geom_smooth(method = "loess") +
  labs(x = "Trophic level", y = "Betweenness") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 10, face = "bold"))

### Closeness ----
plot_sp_cl <- spp_total %>% 
  filter(TL >= 2) %>%
  ggplot(., aes(x = TL, y = Close)) +
  geom_point(size=0.3) +
  xlim(2, 5) +
  geom_smooth(method = "loess") +
  labs(x = "Trophic level", y = "Closeness") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 10, face = "bold"))

### Trophic similarity ----
plot_sp_ts <- spp_total %>% 
  filter(TL >= 2) %>%
  ggplot(., aes(x = TL, y = meanTrophicSimil)) +
  geom_point(size=0.3) +
  xlim(2, 5) +
  geom_smooth(method = "loess") +
  labs(x = "Trophic level", y = "Trophic similarity") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 10, face = "bold"))

### Topological role ----
top.role.col <- hcl.colors(4, palette = "Purple-Green")
TRColor <- setNames(top.role.col, levels(as.factor(V(g_up)$TopRole)))
TRColor <- as.data.frame(TRColor) %>% 
  mutate(TopRole = c("hubcon", "modcon", "modhub", "modspe"))
data_TRColor <- spp_total %>% 
  dplyr::select(TopRole, everything()) %>% 
  left_join(TRColor)

cols <- c("netcon"="#492050", "modspe"="#023903", "modhub"="#A5CEA5", "modcon"="#DAB8E2")
plot_sp_tr <- spp_total %>% 
  mutate(TopRole = case_when(TopRole == "hubcon" ~ "netcon", TRUE ~ TopRole)) %>% 
  ggplot(., aes(x = TopRole, y = TL)) +
  geom_boxplot(aes(color = factor(TopRole), fill = factor(TopRole)), alpha=0.9) +
  geom_jitter(size=0.3, aes(color = factor(TopRole), fill = factor(TopRole))) +
  scale_fill_manual(values=cols) +
  ylim(1, 5) +
  labs(x = "Topological role", y = "Trophic level") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 10, face = "bold"),
        legend.position = "none")

# Arrange figure
figure_ab <- ggarrange(plot_sp_ts + rremove("xlab"), plot_sp_cl + rremove("xlab"),
                       labels = c("A","B"), ncol=2,
                       font.label = list(size = 10, color = "black"))
figure_ab <- annotate_figure(figure_ab, bottom = textGrob("Trophic level", gp = gpar(fontface="bold", col="black", fontsize=10)))
figure_4 <- ggarrange(figure_ab,                                                                                         # First row
                      ggarrange(plot_sp_bt, plot_sp_tr, ncol = 2, labels = c("C", "D"), font.label=list(color="black",size=10)),                                            
                      nrow = 2)


# Miscellaneous ----

# Most connected spp by FG
spp_group <- spp_total %>% 
  group_by(FunctionalGroup) %>% 
  slice_max(TotalDegree, n = 3)

# Red trófica por grupo funcional 'FG'
# spp_total <- spp_total %>% 
#   dplyr::select(FunctionalGroup, everything()) %>% 
#   left_join(unique_FG)
# g_up <- g_up %>% 
#   set_vertex_attr("FGColor", value = spp_total$FGColor)
# 
# plot_troph_level(g_up, vertex.color = V(g_up)$FGColor)
# legend("bottom", legend=unique_FG$FunctionalGroup, col = unique_FG$FGColor, ncol = 5,
#        bty = "n", pch=20 , pt.cex = 3, cex = 1, inset=c(-0.2,0))


### Species-level ----

### Degree distribution
spp_nt_12 <- length(spp_total$TL[spp_total$TL < 2])
spp_nt_13 <- length(spp_total$TL[spp_total$TL >= 2 & spp_total$TL < 3])
spp_nt_14 <- length(spp_total$TL[spp_total$TL < 4])

plot_sp_tl <- ggplot(spp_total, aes(x = reorder(TrophicSpecies, TL), y = TotalDegree)) +
  geom_point() +
  # geom_smooth(aes(as.numeric(reorder(TrophicSpecies, TL)), Degree), method = "loess") +
  geom_vline(xintercept = c(spp_nt_12+1, spp_nt_12+spp_nt_13+1, spp_nt_14+1), linetype = "longdash", colour = "red") +
  labs(x = "Species (increasing TL)", y = "Number of interactions") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 15))
plot_sp_tl

ggplot(spp_total, aes(x = TL, y = TotalDegree)) +
  geom_point()

### Prey & predator ----
data_plot_sp_tl <- spp_total %>% 
  dplyr::select(-c(Omn, meanTrophicSimil)) %>% 
  gather(type, count, NumPrey:NumPred) 

#hue_pal()(2)  # get color code
plot_sp_tl_bar <- ggplot(data_plot_sp_tl, aes(x = reorder(TrophicSpecies, TL), y = count, 
                                              fill = forcats::fct_rev(type))) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("#F8766D","#00BFC4"), labels = c("Prey", "Predator")) +
  labs(x = "Species (increasing TL)", y = "Number of interactions") +
  guides(fill = guide_legend(title = "Number of:")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_blank())
plot_sp_tl_bar

# Interactive plot
library(plotly)
pr_pred_int <- spp_total %>% 
  filter(TotalDegree > 30) %>%
  plot_ly(., x = ~reorder(TrophicSpecies, -TotalDegree), y = ~NumPrey, type = 'bar', name = 'NumPrey') %>%
  add_trace(y = ~NumPred, name = 'NumPred') %>%
  layout(yaxis = list(title = 'Number of interactions'), 
         xaxis = list(title = 'Species'), barmode = 'stack')
pr_pred_int

## FIGURES: RED TRÓFICA AMP NAMUNCURÁ-BANCO BURDWOOD
# Autor: Tomás Ignacio Marina
# Fecha: 18/08/2022


# Paquetes ----

packages <- c("tidyverse", "ggplot2", "ggjoy", "ggpubr", "scales", 
              "multiweb", "igraph")
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
ipak(packages)


# Cargar datos ----

load("results/summary_results_sep22.rda")


# Figuras ----

## Escala global ----

# Red trófica por degree
plot_troph_level(g_up, ylab = "Trophic level")

# Distribución de grado
# Histograma
degree <- as.data.frame(degree(g, mode = "total"))
(plot_distdegree <- ggplot(degree, aes(degree[,1])) +
    geom_histogram(bins = 100, alpha = 0.3, color = "black") +
    labs(x = "Degree", y = "Frequency") +
    theme_bw() +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(face = "bold", size = 16),
          axis.title.y = element_text(face = "bold", size = 16)))

# Generality (generalistas vs especialistas)
plot_indeg <- spp_total %>% 
    filter(NumPrey != 0) %>% 
    ggplot(., aes(x = NumPrey)) +
    geom_histogram(stat = "count") +
    labs(x = "Predators", y = "Number of prey") +
    theme_bw() +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(face = "bold", size = 16),
          axis.title.y = element_text(face = "bold", size = 16)) #+
# annotate("text", x = Inf, y = Inf, label = paste("Prey per predator = ", round(prop.topol$Generality, 2), sep = ""),
#          size = 5, vjust=1, hjust=1))
plot_indeg

# Vulnerability (flujos de energía)
plot_outdeg <- spp_total %>% 
    filter(NumPred != 0) %>% 
    ggplot(., aes(x = NumPred)) +
    geom_histogram(stat = "count") +
    labs(x = "Prey", y = "Number of predators") +
    theme_bw() +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(face = "bold", size = 16),
          axis.title.y = element_text(face = "bold", size = 16)) #+
# annotate("text", x = Inf, y = Inf, label = paste("Predators per prey = ", round(prop.topol$Vulnerability, 2), sep = ""),
#          size = 5, vjust=1, hjust=1))
plot_outdeg

gen_vul_plot <- ggarrange(plot_indeg, plot_outdeg,
                      labels = c("A", "B"), ncol=2, nrow=1)
gen_vul_plot

# Distribución interacciones por GF

load("data/foodweb-data_sep22.rda")

# Definir colores para cada GF
un.prey <- as.data.frame(unique(int_good_res$PreyGroup))  # prey groups
un.pred <- as.data.frame(unique(int_good_res$PredGroup))  # pred groups
colnames(un.prey) <- "Group"
colnames(un.pred) <- "Group"
unique_FG <- unique(bind_rows(un.pred, un.prey))  # 35 groups

col_palette <- scales::hue_pal()(nrow(unique_FG))
FGColors <- setNames(col_palette, levels(unique_FG$Group))
unique_FG <- cbind(unique_FG, as.data.frame(FGColors))
colnames(unique_FG) <- c("FunctionalGroup", "FGColor")

plot_distdegree_fg <- spp_total %>% 
    add_count(FunctionalGroup) %>% 
    filter(n > 2) %>% 
    ggplot(., aes(x = TotalDegree, y = FunctionalGroup, fill = FunctionalGroup)) + 
    geom_joy(scale = 3.75) +
    scale_fill_manual(values = FGColors) +
    labs(x = "Number of interactions", y = "Functional group") +
    theme_joy(grid = FALSE) +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(face = "bold", size = 16),
          axis.title.y = element_text(face = "bold", size = 16))
plot_distdegree_fg


# Especies más conectadas por grupo
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


## Escala especie ----

### Distribución de grado ----

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


### Presas y depredadores ----

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

# Gráfico interactivo
library(plotly)
pr_pred_int <- spp_total %>% 
  filter(TotalDegree > 30) %>%
  plot_ly(., x = ~reorder(TrophicSpecies, -TotalDegree), y = ~NumPrey, type = 'bar', name = 'NumPrey') %>%
  add_trace(y = ~NumPred, name = 'NumPred') %>%
  layout(yaxis = list(title = 'Number of interactions'), 
         xaxis = list(title = 'Species'), barmode = 'stack')
pr_pred_int


### Betweenness & Closeness ----

plot_sp_bt <- ggplot(spp_total, aes(x = TL, y = Between)) +
  geom_point() +
  #geom_vline(xintercept = c(spp_nt_12+1, spp_nt_12+spp_nt_13+1, spp_nt_14+1), linetype = "longdash", colour = "red") +
  labs(x = "Trophic level", y = "Betweenness") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))
plot_sp_bt

plot_sp_cl <- ggplot(spp_total, aes(x = TL, y = Close)) +
  geom_point() +
  #geom_vline(xintercept = c(spp_nt_12+1, spp_nt_12+spp_nt_13+1, spp_nt_14+1), linetype = "longdash", colour = "red") +
  labs(x = "Trophic level", y = "Closeness") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))
plot_sp_cl

btw_clo_plot <- ggarrange(plot_sp_bt, plot_sp_cl,
                          labels = c("A", "B"), ncol=2, nrow=1)
btw_clo_plot


### Trophic similarity ----

plot_sp_ts <- ggplot(spp_total, aes(x = TL, y = meanTrophicSimil)) +
  geom_point() +
  #geom_vline(xintercept = c(spp_nt_12+1, spp_nt_12+spp_nt_13+1, spp_nt_14+1), linetype = "longdash", colour = "red") +
  labs(x = "Trophic level", y = "Trophic similarity") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))
plot_sp_ts


### Roles topológicos ----

top.role.col <- hcl.colors(4, palette = "Purple-Green")
TRColor <- setNames(top.role.col, levels(as.factor(V(g_up)$TopRole)))
TRColor <- as.data.frame(TRColor) %>% 
  mutate(TopRole = c("hubcon", "modcon", "modhub", "modspe"))
show_col(top.role.col)  # shows color & code
data_TRColor <- spp_total %>% 
  dplyr::select(TopRole, everything()) %>% 
  left_join(TRColor)
g_up_c <- g_up %>% 
  set_vertex_attr("TRColor", value = data_TRColor$TRColor)

plot_troph_level(g_up_c, weights = NA, vertex.color = V(g_up_c)$TRColor)

sp_con <- spp_total %>% 
  filter(TopRole == "hubcon" | TopRole == "modcon")



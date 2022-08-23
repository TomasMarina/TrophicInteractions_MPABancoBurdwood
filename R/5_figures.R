## FIGURES: RED TRÓFICA AMP NAMUNCURÁ-BANCO BURDWOOD
# Autor: Tomás Ignacio Marina
# Fecha: 18/08/2022


# Paquetes ----

packages <- c("tidyverse", "ggplot2", "igraph", "multiweb", "scales",
              "ggjoy")
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
ipak(packages)


# Cargar datos ----

load("results/summary_results.rda")


# Figuras ----

## Escala global ----

# Red trófica por degree
plot_troph_level(g_up)

# Generality (generalistas vs especialistas)
(plot_indeg <- spp_total %>% 
    filter(NumPrey != 0) %>% 
    ggplot(., aes(x = NumPrey)) +
    geom_histogram(stat = "count") +
    labs(x = "Predators", y = "Number of prey") +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(face = "bold", size = 16),
          axis.title.y = element_text(face = "bold", size = 16)) +
annotate("text", x = Inf, y = Inf, label = paste("Prey per predator = ", round(prop.topol$Generality, 2), sep = ""),
size = 6, vjust=1, hjust=1))

# Vulnerability (flujos de energía)
(plot_outdeg <- spp_total %>% 
    filter(NumPred != 0) %>% 
    ggplot(., aes(x = NumPred)) +
    geom_histogram(stat = "count") +
    labs(x = "Prey", y = "Number of predators") +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(face = "bold", size = 16),
          axis.title.y = element_text(face = "bold", size = 16)) +
annotate("text", x = Inf, y = Inf, label = paste("Predators per prey = ", round(prop.topol$Vulnerability, 2), sep = ""),
         size = 6, vjust=1, hjust=1))



## Escala subgrupos ----

load("data/foodweb-data_ago22.rda")

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
    labs(x = "Número de interacciones", y = "Grupo funcional") +
    theme_joy(grid = FALSE) +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(face = "bold", size = 16),
          axis.title.y = element_text(face = "bold", size = 16))
plot_distdegree_fg

# Red trófica por grupo funcional 'FG'
spp_total <- spp_total %>% 
  dplyr::select(FunctionalGroup, everything()) %>% 
  left_join(unique_FG)
g_up <- g_up %>% 
  set_vertex_attr("FGColor", value = spp_total$FGColor)
#vertex_attr_names(g)
plot_troph_level(g_up, vertex.color = V(g_up)$FGColor)
legend("bottom", legend=unique_FG$FunctionalGroup, col = unique_FG$FGColor, ncol = 5,
       bty = "n", pch=20 , pt.cex = 3, cex = 1, inset=c(-0.2,0))

# Roles topológicos
plot_troph_level(g_up, weights = NA, vertex.color = as.factor(V(g_up)$TopRole.y))


## Escala especie ----

### Distribución de grado ----
spp_nt_12 <- length(spp_total$TL[spp_total$TL < 2])
spp_nt_13 <- length(spp_total$TL[spp_total$TL >= 2 & spp_total$TL < 3])
spp_nt_14 <- length(spp_total$TL[spp_total$TL < 4])

(plot_sp_tl <- ggplot(spp_total, aes(x = reorder(TrophicSpecies, TL), y = TotalDegree)) +
    geom_point() +
    # geom_smooth(aes(as.numeric(reorder(TrophicSpecies, TL)), Degree), method = "loess") +
    geom_vline(xintercept = c(spp_nt_12+1, spp_nt_12+spp_nt_13+1, spp_nt_14+1), linetype = "longdash", colour = "red") +
    labs(x = "Species (increasing TL)", y = "Number of interactions") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.title = element_text(size = 18, face = "bold"),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 15)))


### Presas y depredadores ----
data_plot_sp_tl <- spp_total %>% 
  dplyr::select(-c(Omn, meanTrophicSimil)) %>% 
  gather(type, count, NumPrey:NumPred) 

#hue_pal()(2)  # get color code
(plot_sp_tl_bar <- ggplot(data_plot_sp_tl, aes(x = reorder(TrophicSpecies, TL), y = count, 
                                               fill = forcats::fct_rev(type))) +
    geom_bar(stat="identity") +
    scale_fill_manual(values = c("#F8766D","#00BFC4"), labels = c("Prey", "Predator")) +
    labs(x = "Species (increasing TL)", y = "Number of interactions") +
    guides(fill = guide_legend(title = "Number of:")) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.title = element_text(size = 12, face = "bold"),
          axis.text.x = element_blank()))

# Gráfico interactivo
library(plotly)
pr_pred_int <- spp_total %>% 
  filter(TotalDegree > 30) %>%
  plot_ly(., x = ~reorder(TrophicSpecies, -TotalDegree), y = ~NumPrey, type = 'bar', name = 'NumPrey') %>%
  add_trace(y = ~NumPred, name = 'NumPred') %>%
  layout(yaxis = list(title = 'Number of interactions'), 
         xaxis = list(title = 'Species'), barmode = 'stack')
pr_pred_int






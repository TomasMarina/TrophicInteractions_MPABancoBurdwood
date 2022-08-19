## FOOD WEB ANALYSIS: RED TRÓFICA AMP NAMUNCURÁ-BANCO BURDWOOD
# Autor: Tomás Ignacio Marina
# Fecha: 09/11/2021


# Paquetes ----

packages <- c("tidyverse", "ggplot2", "igraph", "multiweb", "cheddar",
              "NetIndices", "univariateML", "fitdistrplus", "gamlss")
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
ipak(packages)


# Cargar datos ----

load("data/cleaned-data_ago22.rda")
load("data/foodweb-data_ago22.rda")


# Análisis ----

## Escala global ----

# Propiedades topológicas
prop.topol <- multiweb::calc_topological_indices(g)

# Prop. sp B, I y T
prop.bas <- prop.topol$Basal/prop.topol$Size
prop.top <- prop.topol$Top/prop.topol$Size
prop.int <- 1 - (prop.bas + prop.top)
prop.bas+prop.top+prop.int  # chequear sum(prop)=1

# Distribución de grado
# Histograma
degree <- as.data.frame(degree(g, mode = "total"))
(plot_distdegree <- ggplot(degree, aes(degree[,1])) +
    geom_histogram(bins = 100, alpha = 0.3, color = "black") +
    labs(x = "Cantidad de interacciones", y = "Frecuencia") +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(face = "bold", size = 16),
          axis.title.y = element_text(face = "bold", size = 16)))

# Función para evaluar ajuste con AIC y BIC
x <- degree[,1]
aic.result <- c(AIC(mlunif(x), mlexp(x), mlpower(x), mllnorm(x), mlnorm(x), mlgamma(x)))
deg_dist_fit <- bind_cols(aic.result) %>% 
  mutate(Model = c("Uniform", "Exponential", "Power-law", "log-Normal", "Normal", "Gamma"),
         deltaAIC = AIC - min(AIC)) %>% 
  arrange(deltaAIC) %>% 
  dplyr::select(Model, df, AIC, deltaAIC)
deg_dist_fit

# Comparar gráficamente datos y distribución
ggplot(data = degree) +
  geom_histogram(aes(x = degree[,1], y = after_stat(density)), bins = 100, alpha = 0.3, color = "black") +
  geom_rug(aes(x = degree[,1])) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlnorm(degree[,1]))},
                aes(color = "Normal"), size = 1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlexp(degree[,1]))},
                aes(color = "Exponential"), size = 1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlunif(degree[,1]))},
                aes(color = "Uniform"), size = 1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mllnorm(degree[,1]))},
                aes(color = "Log-Normal"), size = 1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlpower(degree[,1]))},
                aes(color = "Power-law"), size = 1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlgamma(degree[,1]))},
                aes(color = "Gamma"), size = 1) +
  labs(title = "Degree Distribution", color = "Model",
       x = "Degree (k)", y = "p(k)")


## Escala subgrupos ----

# Modularidad
mod <- multiweb::calc_modularity(g, weights = NULL)
modulos <- cluster_spinglass(g)  # módulos
# Roles topológicos
top.role <- multiweb::calc_topological_roles(g, nsim = 100, ncores = 4)
clas.role <- multiweb::classify_topological_roles(top.role, g, plt = TRUE)
top.role.df <- clas.role %>% mutate(module = modulos$membership[node])


## Escala especie ----

# Nivel trófico y Omnivoría
adj_mat <- as_adjacency_matrix(g, sparse = TRUE)
tl <- round(TrophInd(as.matrix(adj_mat)), digits = 3)
V(g)$TL <- tl$TL
V(g)$Omn <- tl$OI

# Degree
V(g)$TotalDegree <- degree(g, mode = "total")
V(g)$InDegree <- degree(g, mode = "in")
V(g)$OutDegree <- degree(g, mode = "out")

vertex.attributes(g)
vertex_attr_names(g)

# Similitud trófica (Trophic similarity)
source("R/igraph_cheddar.R")  # load function to convert igraph to cheddar object
igraph_to_cheddar(g)

cc <- LoadCommunity("Community")
ts <- TrophicSimilarity(cc)
mts <- tibble(TrophicSpecies = rownames(ts), meanTrophicSimil = colMeans(ts))  # data frame

# Guardar como data frame y .csv
spp_id <- as.data.frame(1:prop.topol$Size)
spp_name <- as.data.frame(V(g)$name)
spp_fg <- as.data.frame(V(g)$FunctionalGroup)
spp_totdegree <- as.data.frame(V(g)$TotalDegree)
spp_indegree <- as.data.frame(V(g)$InDegree)
spp_outdegree <- as.data.frame(V(g)$OutDegree)
spp_tl <- as.data.frame(V(g)$TL)
spp_omn <- as.data.frame(V(g)$Omn)
spp_total <- bind_cols(spp_id, spp_name, spp_fg, spp_totdegree, spp_indegree, 
                        spp_outdegree, spp_tl, spp_omn)
colnames(spp_total) <- c("ID", "TrophicSpecies", "FunctionalGroup", "TotalDegree", 
                          "NumPrey", "NumPred", "TL", "Omn")
spp_total <- spp_total %>% 
  left_join(mts)

#write_csv(spp_total, file = "results/spp_prop_ago22.csv")


# Guardar datos ----

save(g, deg_dist_fit, prop.topol, top.role, top.role.df, spp_total,
     file = "results/summary_results.rda")


# Distribución de grado por sp y NT
spp_nt_12 <- length(data_total$TL[data_total$TL < 2])
spp_nt_13 <- length(data_total$TL[data_total$TL >= 2 & data_total$TL < 3])
spp_nt_14 <- length(data_total$TL[data_total$TL < 4])

(plot_sp_tl <- ggplot(data_total, aes(x = reorder(TrophicSpecies, TL), y = Degree)) +
  geom_point() +
  # geom_smooth(aes(as.numeric(reorder(TrophicSpecies, TL)), Degree), method = "loess") +
  geom_vline(xintercept = c(spp_nt_12+1, spp_nt_12+spp_nt_13+1, spp_nt_14+1), linetype = "longdash", colour = "red") +
  labs(x = "Especies (nivel trófico ascendente)", y = "Número de interacciones") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 15)))

# Grafico interactivo
# library(plotly)
# plot_ly(data = data_total, x = ~TrophicSpecies, y = ~NumPrey, type = 'bar', name = 'NumPrey') %>%
#   add_trace(y = ~NumPred, name = 'NumPred') %>%
#   layout(yaxis = list(title = 'Count'), barmode = 'stack')

data_plot_sp_tl <- data_total %>% 
  dplyr::select(-Omn) %>% 
  gather(type, count, NumPrey:NumPred) 

(plot_sp_tl_bar <- ggplot(data_plot_sp_tl, aes(x = reorder(TrophicSpecies, TL), y = count, 
                                fill = forcats::fct_rev(type))) +
  geom_bar(stat="identity") +
  labs(x = "Trophic species (ascending TL)", y = "Number of interactions") +
  guides(fill = guide_legend(title = "Interaction type")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_blank()))

# Distribucion de interacciones por grupo funcional
data_plot_fg <- data_total %>% 
  add_count(FunctionalGroup) %>% 
  filter(n > 2)

# Definir colores para cada GF
un.prey <- as.data.frame(unique(int_raw$PreyGroup))  # prey groups
un.pred <- as.data.frame(unique(int_raw$PredGroup))  # pred groups
colnames(un.prey) <- "Group"
colnames(un.pred) <- "Group"
unique_FG <- unique(bind_rows(un.pred, un.prey))  # 35 groups

col_palette <- scales::hue_pal()(nrow(unique_FG))
FGColors <- setNames(col_palette, levels(unique_FG$Group))
unique_FG <- cbind(unique_FG, as.data.frame(FGColors))
colnames(unique_FG) <- c("FunctionalGroup", "FGColor")

(plot_distdegree_fg <- ggplot(data_plot_fg, aes(x = Degree, y = FunctionalGroup, fill = FunctionalGroup)) + 
    geom_joy(scale = 3.75) +
    scale_fill_manual(values = FGColors) +
    labs(x = "Número de interacciones", y = "Grupo funcional") +
    theme_joy(grid = FALSE) +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(face = "bold", size = 16),
          axis.title.y = element_text(face = "bold", size = 16)))


## Gen / Vul ----

# Generality (generalistas vs especialistas)
data_indeg <- as.data.frame(V(g_giant)$indegree) 
data_indeg <- data_indeg %>% 
  filter(V(g_giant)$indegree != 0) %>% 
  rename(n = "V(g_giant)$indegree")
(plot_indeg <- ggplot(data_indeg, aes(x = n)) +
    geom_histogram(stat = "count") +
    labs(x = "Depredadores", y = "Cantidad de presas") +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(face = "bold", size = 16),
          axis.title.y = element_text(face = "bold", size = 16))) # +
# annotate("text", x = Inf, y = Inf, label = paste("Presas por depredador = ", gen.fun(g_giant), sep = ""), 
# size = 6, vjust=1, hjust=1))

gen.fun <- function(g){
  pred <- degree(g, mode = "in") > 0
  G_avg <- round(mean(data_indeg$n), 2)
  G_med <- round(median(data_indeg$n), 2)
  
  return(c("mean" = G_avg, "median" = G_med))
}
gen.fun(g_giant)

# Vulnerability (flujos de energía)
data_outdeg <- as.data.frame(V(g_giant)$outdegree) 
data_outdeg <- data_outdeg %>% 
  filter(V(g_giant)$outdegree != 0) %>% 
  rename(n = "V(g_giant)$outdegree")
(plot_outdeg <- ggplot(data_outdeg, aes(x = n)) +
    geom_histogram(stat = "count") +
    labs(x = "Presas", y = "Cantidad de depredadores") +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(face = "bold", size = 16),
          axis.title.y = element_text(face = "bold", size = 16))) # +
# annotate("text", x = Inf, y = Inf, label = paste("Depredadores por presa = ", vul.fun(g_giant), sep = ""), 
#          size = 6, vjust=1, hjust=1))

vul.fun <- function(g){
  prey <- degree(g, mode = "out") > 0
  V_avg <- round(mean(data_outdeg$n), 2)
  V_med <- round(median(data_outdeg$n), 2)
  
  return(c("mean" = V_avg, "median" = V_med))
}
vul.fun(g_giant)


# Graficar por tipo de sp
layout_trophic <- matrix(nrow = length(V(g_giant)), ncol = 2)
layout_trophic[, 1] <- runif(length(V(g_giant)))
layout_trophic[, 2] <- V(g_giant)$TL
plot.igraph(g_giant,
            vertex.size = degree*0.25,
            vertex.label = NA,
            vertex.color = ifelse(V(g_giant)$indegree == 0, "green", ifelse(V(g_giant)$outdegree == 0, "red", "orange")),
            layout = layout_trophic,
            edge.width = .75, edge.curved = 0.3,
            edge.arrow.size = 0.15)

# Graficar por grupo funcional
data_plot_fg <- data_plot_fg %>% 
  inner_join(unique_FG, by = "FunctionalGroup")

g_giant <- g_giant %>% 
  set_edge_attr("FGColor", value = data_plot_fg$FGColor)
vertex.attributes(g_giant)

plot.igraph(g_giant,
            vertex.size = degree*0.25,
            vertex.label = NA,
            vertex.color = V(g_giant)$FGColor,
            layout = layout_trophic,
            edge.width = .75, edge.curved = 0.3,
            edge.arrow.size = 0.15)
plot_troph_level(g_giant)

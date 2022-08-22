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


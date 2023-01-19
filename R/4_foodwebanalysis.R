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

load("data/cleaned-data_jan23.rda")
load("data/foodweb-data_jan23.rda")


# Análisis ----

## Escala global ----

# Propiedades topológicas
prop_topol <- multiweb::calc_topological_indices(g)

# Prop. sp B, I y T
prop.bas <- prop_topol$Basal/prop_topol$Size
prop.top <- prop_topol$Top/prop_topol$Size
prop.int <- 1 - (prop.bas + prop.top)
prop.bas+prop.top+prop.int  # chequear sum(prop)=1

# Distribución de grado
degree <- as.data.frame(degree(g, mode = "total"))
# Función para evaluar ajuste con AIC y BIC
x <- degree[,1]
aic.result <- c(AIC(mlunif(x), mlexp(x), mlpower(x), mllnorm(x), mlnorm(x), mlgamma(x)))
deg_dist_fit <- bind_cols(aic.result) %>% 
  mutate(Model = c("Uniform", "Exponential", "Power-law", "log-Normal", "Normal", "Gamma"),
         deltaAIC = AIC - min(AIC)) %>% 
  arrange(deltaAIC) %>% 
  dplyr::select(Model, df, AIC, deltaAIC)

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
  labs(color = "Model", x = "Degree (k)", y = "p(k)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(face = "bold", size = 16),
        axis.title.y = element_text(face = "bold", size = 16))


## Escala subgrupos ----

# Modularidad
mod <- multiweb::calc_modularity(g, weights = NULL)
modulos <- cluster_spinglass(g)  # módulos
# Roles topológicos
tictoc::tic()
top.role <- multiweb::calc_topological_roles(g, nsim = 100, ncores = 4)
tictoc::toc()
clas.role <- multiweb::classify_topological_roles(top.role, g, plt = TRUE)
top.role.df <- clas.role %>% 
  mutate(Module = modulos$membership[node]) %>% 
  rename(TrophicSpecies = name, TopRole = type) %>% 
  dplyr::select(TrophicSpecies, TopRole)
top.role.count <- top.role.df %>% 
  count(TopRole)
# Incluir en objeto g
df_g <- igraph::as_data_frame(g, 'both')
df_g$vertices <- df_g$vertices %>% 
  left_join(top.role.df, c('name' = 'TrophicSpecies'))
g_up <- graph_from_data_frame(df_g$edges, directed = TRUE, vertices = df_g$vertices)


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

# Betweenness
V(g)$Btw <- betweenness(g, directed = TRUE, cutoff = -1)

# Closeness
V(g)$Close <- closeness(g, mode = "all", cutoff = -1)

vertex_attr_names(g)

# Similitud trófica (Trophic similarity)
source("R/igraph_cheddar.R")  # load function to convert igraph to cheddar object
igraph_to_cheddar(g)

cc <- LoadCommunity("Community")
ts <- TrophicSimilarity(cc)
mts <- tibble(TrophicSpecies = rownames(ts), meanTrophicSimil = colMeans(ts))  # data frame

# Guardar como data frame y .csv
spp_id <- as.data.frame(1:prop_topol$Size)
spp_name <- as.data.frame(V(g)$name)
spp_fg <- as.data.frame(V(g)$FunctionalGroup)
spp_totdegree <- as.data.frame(V(g)$TotalDegree)
spp_indegree <- as.data.frame(V(g)$InDegree)
spp_outdegree <- as.data.frame(V(g)$OutDegree)
spp_btw <- as.data.frame(V(g)$Btw)
spp_cls <- as.data.frame(V(g)$Close)
spp_tl <- as.data.frame(V(g)$TL)
spp_omn <- as.data.frame(V(g)$Omn)
spp_total <- bind_cols(spp_id, spp_name, spp_fg, spp_totdegree, spp_indegree, 
                        spp_outdegree, spp_btw, spp_cls, spp_tl, spp_omn)
colnames(spp_total) <- c("ID", "TrophicSpecies", "FunctionalGroup", "TotalDegree", 
                          "NumPrey", "NumPred", "Between", "Close", "TL", "Omn")
spp_total <- spp_total %>% 
  left_join(mts) %>% 
  left_join(top.role.df)

# Agregar Habitat
spp_total <- spp_total %>% 
  mutate(Habitat = case_when(FunctionalGroup == "SeaBirds" ~ "Land-based",
                             TrophicSpecies %in% c("Arctocephalus_australis", "Otaria_flavescens") ~ "Land-based",
                             FunctionalGroup %in% c("Porifera", "Polyplacophora",
                                                  "Polychaeta", "Echinodermata",
                                                  "Gastropoda", "Cnidaria",
                                                  "Ascidiacea", "Bryozoa",
                                                  "Brachiopoda", "Benthos_Misc") ~ "Benthic",
                             FunctionalGroup %in% c("Zooplankton", "Ciliates",
                                                  "Cephalopoda", "Fish_Pel",
                                                  "MarineMammals_1", "MarineMammals_3") ~ "Pelagic",
                             TrophicSpecies %in% c("Lagenorhynchus_australis", 
                                                   "Lagenorhynchus_cruciger") ~ "Benthopelagic",
                             FunctionalGroup %in% c("Fish_BenPel") ~ "Benthopelagic",
                             FunctionalGroup %in% c("Fish_Demersal") ~ "Demersal"))


write_csv(spp_total, file = "results/spp_prop_jan23.csv")


# Resolución de la red ----

res_sp <- as.data.frame(V(g_up)$name)
good_res_sp <- sum(str_detect(res_sp[,1], "_"))
gen_count <- sum(str_detect(res_sp[,1], "_sp"))
sp_count <- good_res_sp - gen_count
sp_prop <- sp_count/vcount(g_up)


# Guardar datos ----

save(g_up, g, deg_dist_fit, prop_topol, spp_total, top.role, top.role.df,
     file = "results/summary_results_jan23.rda")

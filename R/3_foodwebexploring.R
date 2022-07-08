## FOOD WEB EXPLORING: RED TRÓFICA AMP NAMUNCURÁ-BANCO BURDWOOD
# Autor: Tomás Ignacio Marina
# Fecha: 09/11/2021


## Paquetes ----

packages <- c("tidyverse", "ggplot2", "naniar", "igraph", "multiweb",
              "NetIndices", "ggjoy")
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
ipak(packages)


## Cargar datos ----

load("data/cleaned-data_jul22.rda")


## Red trófica ----

# Interacciones de baja resolución (*)
int_need_res <- int_raw %>% filter_at(.vars = vars(Prey, Predator),
                                  .vars_predicate = any_vars(str_detect(., "\\*$")))
# int_raw$Prey <- sub(" .*", "", int_raw$Prey)
# int_raw$Predator <- sub(" .*", "", int_raw$Predator)

# Omitir interacciones de baja resolución
int_good_res <- int_raw %>% 
  filter(!str_detect(Prey, "\\*$")) %>% 
  filter(!str_detect(Predator, "\\*$"))

int_good_res <- int_good_res[,1:6] %>% 
  relocate(any_of(c("Prey", "Predator", "PreyGroup", "PredGroup", "PredStrategy", "FoodSource")))

# Crear objeto igraph y asignar atributos
#load("../../IMDEA/R/BalticFW.Rdata")
g <- graph_from_edgelist(as.matrix(int_good_res[,1:2]), directed = TRUE)

sp_raw_fg <- sp_raw[,c("TrophicSpecies", "FunctionalGroup", "Zone")] %>% 
  mutate(across(where(is.factor), as.character)) %>% 
  relocate(any_of(c("FunctionalGroup", "Zone", "TrophicSpecies"))) %>% rename(id = TrophicSpecies)

g <- g %>% 
  set_edge_attr("Prey Group", value = int_ok[,3]) %>% 
  set_edge_attr("Pred Group", value = int_ok[,4]) %>% 
  set_edge_attr("Pred Strategy", value = int_ok[,5]) %>% 
  set_edge_attr("Food Source", value = int_ok[,6])

df_g <- igraph::as_data_frame(g, 'both')
df_g$vertices <- df_g$vertices %>% 
  left_join(sp_raw_fg, c('name' = 'id'))
g <- graph_from_data_frame(df_g$edges, directed = TRUE, vertices = df_g$vertices)
vertex.attributes(g)

g_dec <- decompose(g, mode = "weak")
g_giant <- g_dec[[1]]  # 1 sola componente

# Distribucion de spp en GF
group_sp <- as.data.frame((V(g)$name)) %>% 
  mutate(as.data.frame(V(g)$FunctionalGroup)) %>%
  rename("TrophicSpecies" = 1, "FunctionalGroup" = 2)

group_sp <- group_sp %>%
  count(FunctionalGroup) %>%
  mutate(FunctionalGroup = fct_reorder(FunctionalGroup, n, .desc = TRUE))
sum(group_sp$n)
(plot_group <- ggplot(group_sp, aes(x = FunctionalGroup, y = n, fill = FunctionalGroup)) + 
    geom_bar(stat = "identity") +
    labs(x = "Grupos funcionales", y = "Especies tróficas") +
    theme(legend.position = "none",
          axis.title.y = element_text(face = "bold", size = 14),
          axis.title.x = element_text(face = "bold", size = 14),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 14)))

# Resolución de la red: spp e interacciones
res_sp <- as.data.frame(V(g_giant)$name)
sp_count <- sum(str_detect(res_sp[,1], "_"))
sp_prop <- sp_count/vcount(g_giant)  # spp
int_prop <- ((nrow(int_raw) - nrow(int_need))/nrow(int_raw))  # interacciones

# Calcular NT y Omn
adj_mat <- get.adjacency(g_giant, sparse = FALSE)
tl <- round(TrophInd(adj_mat), digits = 3)
degree <- degree(g_giant, mode = "total")
V(g_giant)$TL <- tl$TL
V(g_giant)$Omn <- tl$OI
V(g_giant)$totdegree <- degree
vertex.attributes(g_giant)

# Calcular presas y depredadores por sp
out.deg <- degree(g_giant, mode = "out")
V(g_giant)$outdegree <-  out.deg
in.deg <- degree(g_giant, mode = "in")
V(g_giant)$indegree <-  in.deg

# Graficar

plotTrophLevel(g_giant)

layout_trophic <- matrix(nrow = length(V(g_giant)), ncol = 2)
layout_trophic[, 1] <- runif(length(V(g_giant)))
layout_trophic[, 2] <- tl$TL
plot_fw <- plot.igraph(g_giant,
                       vertex.size = degree*0.25,
                       vertex.label = NA,
                       layout = layout_trophic,
                       edge.width = .5,
                       edge.arrow.size = 0.15, edge.curved = 0.3)


## Save data ----

save(g, g_giant, int_ok,
     file = "data/foodweb-data_jul22.rda")

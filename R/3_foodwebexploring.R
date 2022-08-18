## FOOD WEB EXPLORING: RED TRÓFICA AMP NAMUNCURÁ-BANCO BURDWOOD
# Autor: Tomás Ignacio Marina
# Fecha: 09/11/2021


# Paquetes ----

packages <- c("tidyverse", "ggplot2", "igraph", "multiweb", "NetIndices")
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
ipak(packages)


# Cargar datos ----

load("data/cleaned-data_ago22.rda")


# Red trófica ----

# Interacciones de baja resolución (*)
int_need_res <- int_raw %>% filter_at(.vars = vars(Prey, Predator),
                                  .vars_predicate = any_vars(str_detect(., "\\*$")))
# int_raw$Prey <- sub(" .*", "", int_raw$Prey)
# int_raw$Predator <- sub(" .*", "", int_raw$Predator)

# Omitir interacciones de baja resolución
int_good_res <- int_raw %>% 
  filter(!str_detect(Prey, "\\*$")) %>% 
  filter(!str_detect(Predator, "\\*$")) %>% 
  dplyr::select(Prey, Predator, PreyGroup, PredGroup, PredStrategy, FoodSource) %>% 
  distinct(Prey, Predator, .keep_all = TRUE)


## Objeto igraph ----

g <- graph_from_edgelist(as.matrix(int_good_res[,1:2]), directed = TRUE)

sp_raw_fg <- sp_raw[,c("TrophicSpecies", "FunctionalGroup", "Zone")] %>% 
  mutate(across(where(is.factor), as.character)) %>% 
  relocate(any_of(c("FunctionalGroup", "Zone", "TrophicSpecies"))) %>% rename(id = TrophicSpecies)

g <- g %>% 
  set_edge_attr("Prey Group", value = int_good_res[,3]) %>% 
  set_edge_attr("Pred Group", value = int_good_res[,4]) %>% 
  set_edge_attr("Pred Strategy", value = int_good_res[,5]) %>% 
  set_edge_attr("Food Source", value = int_good_res[,6])
edge.attributes(g)

df_g <- igraph::as_data_frame(g, 'both')
df_g$vertices <- df_g$vertices %>% 
  left_join(sp_raw_fg, c('name' = 'id'))
g <- graph_from_data_frame(df_g$edges, directed = TRUE, vertices = df_g$vertices)
vertex.attributes(g)

# Chequear número componentes
g_dec <- decompose(g, mode = "weak")  # 1 sola componente
g_dec


## Distribucion de spp en GF ----

group_sp <- as.data.frame((V(g)$name)) %>% 
  mutate(as.data.frame(V(g)$FunctionalGroup)) %>%
  rename("TrophicSpecies" = 1, "FunctionalGroup" = 2)

group_sp <- group_sp %>%
  count(FunctionalGroup) %>%
  mutate(FunctionalGroup = fct_reorder(FunctionalGroup, n, .desc = TRUE))
sum(group_sp$n)  # spp totales
(plot_group <- ggplot(group_sp, aes(x = FunctionalGroup, y = n, fill = FunctionalGroup)) + 
    geom_bar(stat = "identity") +
    labs(x = "Grupos funcionales", y = "Especies tróficas") +
    theme(legend.position = "none",
          axis.title.y = element_text(face = "bold", size = 14),
          axis.title.x = element_text(face = "bold", size = 14),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 14)))


## Resolución de la red ----

res_sp <- as.data.frame(V(g)$name)
sp_count <- sum(str_detect(res_sp[,1], "_"))
sp_prop <- sp_count/vcount(g)  # spp
int_prop <- ((nrow(int_raw) - nrow(int_need_res))/nrow(int_raw))  # interacciones


## Graficar ----

plotTrophLevel(g)


# Save data ----

save(g, int_need_res, int_good_res,
     file = "data/foodweb-data_ago22.rda")

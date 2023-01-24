## FOOD WEB ANALYSIS: RED TRÓFICA AMP NAMUNCURÁ-BANCO BURDWOOD
# Autor: Tomás Ignacio Marina
# Fecha: 24/01/2023


# Paquetes ----

packages <- c("dplyr", "ggplot2", "olsrr")
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
ipak(packages)


# Datos ----
load("results/summary_results_jan23.rda")


# Linear Regression ----

## Betweenness ----
plot_sp_bt <- spp_total %>% 
  filter(TL >= 2) %>% 
  ggplot(., aes(x = TL, y = Between)) +
  geom_point() +
  geom_smooth(method = "lm") +
  #geom_vline(xintercept = c(spp_nt_12+1, spp_nt_12+spp_nt_13+1, spp_nt_14+1), linetype = "longdash", colour = "red") +
  labs(x = "Trophic level", y = "Betweenness") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 26, face = "bold"))

# Test regression significance
btw_reg <- lm(Between ~ TL, data = spp_total)
summary(btw_reg)
ols_test_normality(btw_reg)  # check normality of residuals
ols_plot_resid_qq(btw_reg)  # Q-Q plot


## Closeness ----
plot_sp_cl <- spp_total %>% 
  filter(TL >= 2) %>%
  ggplot(., aes(x = TL, y = Close)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Trophic level", y = "Closeness") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 26, face = "bold"))

# Test regression significance
clo_reg <- lm(Close ~ TL, data = spp_total)
summary(clo_reg)
ols_test_normality(clo_reg)  # check normality of residuals
ols_plot_resid_qq(clo_reg)  # Q-Q plot


## Trophic similarity ----
plot_sp_ts <- spp_total %>% 
  filter(TL >= 2) %>%
  ggplot(., aes(x = TL, y = meanTrophicSimil)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Trophic level", y = "Trophic similarity") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 26, face = "bold"))

# Test regression significance
ts_reg <- lm(meanTrophicSimil ~ TL, data = spp_total)
summary(ts_reg)
ols_test_normality(ts_reg)  # check normality of residuals
ols_plot_resid_qq(ts_reg)  # Q-Q plot


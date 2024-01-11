## FOOD WEB ANALYSIS: RED TRÓFICA AMP NAMUNCURÁ-BANCO BURDWOOD
# Autor: Tomás Ignacio Marina
# Fecha: 24/01/2023, 8/01/2024 (update)


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
load("results/summary_results_jul23.rda")


# Data fit ----

## Betweenness ----
# Check linear regression assumptions
lm.btw <- lm(Between ~ TL, data=spp_total)
ols_test_normality(lm.btw)
btw.res.hist <- ols_plot_resid_hist(lm.btw)
btw.res.fit <- ols_plot_resid_fit(lm.btw)
btw.res.qq <- ols_plot_resid_qq(lm.btw)
figure_btw <- ggarrange(btw.res.hist, btw.res.fit, btw.res.qq,
                       ncol=2, nrow=2,
                       font.label = list(size = 10, color = "black"))
annotate_figure(figure_btw,
                top = text_grob("Betweenness ~ TL model assumptions", color = "red", face = "bold", size = 14))

# Log y axis to see better the trend. highest TLs (top predators) show btw=0 due to methodology
plot_sp_bt <- spp_total %>% 
  filter(TL >= 2) %>% 
  ggplot(., aes(x = TL, y = Between)) +
  geom_point() +
  scale_y_log10(label = scientific_10) +
  geom_smooth(method = "loess") +
  #geom_smooth(method = "glm", method.args = list(family = "Gamma"), se = FALSE) +
  #geom_vline(xintercept = c(spp_nt_12+1, spp_nt_12+spp_nt_13+1, spp_nt_14+1), linetype = "longdash", colour = "red") +
  labs(x = "Trophic level", y = "Betweenness") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 26, face = "bold"))

## Closeness ----
# Check linear regression assumptions
lm.clo <- lm(Close ~ TL, data=spp_total)
ols_test_normality(lm.clo)
clo.res.hist <- ols_plot_resid_hist(lm.clo)
clo.res.fit <- ols_plot_resid_fit(lm.clo)
clo.res.qq <- ols_plot_resid_qq(lm.clo)
figure_clo <- ggarrange(clo.res.hist, clo.res.fit, clo.res.qq,
                        ncol=2, nrow=2,
                        font.label = list(size = 10, color = "black"))
annotate_figure(figure_clo,
                top = text_grob("Closeness ~ TL model assumptions", color = "red", face = "bold", size = 14))

plot_sp_cl <- spp_total %>% 
  filter(TL >= 2) %>%
  ggplot(., aes(x = TL, y = Close)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(x = "Trophic level", y = "Closeness") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 26, face = "bold"))

## Trophic similarity ----
# Check linear regression assumptions
lm.ts <- lm(meanTrophicSimil ~ TL, data=spp_total)
ols_test_normality(lm.ts)
ts.res.hist <- ols_plot_resid_hist(lm.ts)
ts.res.fit <- ols_plot_resid_fit(lm.ts)
ts.res.qq <- ols_plot_resid_qq(lm.ts)
figure_ts <- ggarrange(ts.res.hist, ts.res.fit, ts.res.qq,
                        ncol=2, nrow=2,
                        font.label = list(size = 10, color = "black"))
annotate_figure(figure_ts,
                top = text_grob("Trophic simil. ~ TL model assumptions", color = "red", face = "bold", size = 14))


plot_sp_ts <- spp_total %>% 
  filter(TL >= 2) %>%
  ggplot(., aes(x = TL, y = meanTrophicSimil)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(x = "Trophic level", y = "Trophic similarity") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 26, face = "bold"))

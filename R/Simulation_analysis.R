# Simulations -------------------------------------------------------------------

library(ggplot2)
library(ggpubr)
library(tidyr)
library(car)


#import the result
simu<-read.csv("Simulation_results.csv", sep = " ")
colnames(simu)


#fill the index_tree
simu <- simu %>%
  fill(index_tree)


#organize the data frames for the figures.

#Alpha__________________________________________________________________________

df_alpha <- data.frame(simu$alpha_quad_lat_antes, simu$alpha_quad_lat_depois)


df_alpha_quad_lat <- gather(df_alpha, key = "Model", value = "Alpha_models")

#
df_alpha_quad_lat <- df_alpha_quad_lat %>%
  mutate(Model = ifelse(Model == "simu.alpha_quad_lat_antes", "alpha_quad_lat_antes", "alpha_quad_lat_depois"))


#Beta___________________________________________________________________________
df_beta2 <- data.frame(simu$beta2_quad_lat_antes, simu$beta2_quad_lat_depois)


df_beta2_quad_lat <- gather(df_beta2, key = "Model", value = "Beta2_models")

#
df_beta2_quad_lat <- df_beta2_quad_lat %>%
  mutate(Model = ifelse(Model == "simu.beta2_quad_lat_antes", "beta2_quad_lat_antes", "beta2_quad_lat_depois"))


#R²___________________________________________________________________________
df_R2 <- data.frame(simu$R2_quad_lat_antes, simu$R2_quad_lat_depois)


df_R2_quad_lat <- gather(df_R2, key = "Model", value = "R2_models")

#
df_R2_quad_lat <- df_R2_quad_lat %>%
  mutate(Model = ifelse(Model == "simu.R2_quad_lat_antes", "R2_quad_lat_antes", "R2_quad_lat_depois"))



# Boxplot ----------------------------------------------------------------------


#_______________________________________________________________________________

R2.quad.lat <-ggplot(df_R2_quad_lat) + aes(x = Model, y = R2_models, fill = Model) + 
  geom_point(aes(color = Model), position = position_jitter(w = .15),
             size = 0.5,alpha = 0.15, show.legend = F) +
  geom_boxplot(width = .24,  outlier.shape = NA,
               alpha = 0.5, show.legend = F) +
  geom_flat_violin(position = position_nudge(x = .2), trim = TRUE, 
                   alpha = 0.7, scale = "width", show.legend = F)  +
  theme_test(base_size = 10) +
  scale_fill_brewer(palette = "Pastel1", type = "qual")+
  ylab("Adjusted R²") +
  xlab("") +
  scale_x_discrete(labels = c(expression(paste("R²"[Before])), 
                              expression(paste("R²"[After]))))

R2.quad.lat

#_______________________________________________________________________________


alfa.quad.lat <-ggplot(df_alpha_quad_lat) + aes(x = Model, y = Alpha_models, fill = Model) + 
  geom_point(aes(color = Model), position = position_jitter(w = .15),
             size = 0.5,alpha = 0.15, show.legend = F) +
  geom_boxplot(width = .24,  outlier.shape = NA,
               alpha = 0.5, show.legend = F) +
  geom_flat_violin(position = position_nudge(x = .2), trim = TRUE, 
                   alpha = 0.7, scale = "width", show.legend = F)  +
  theme_test(base_size = 10) +
  scale_fill_brewer(palette = "Pastel1", type = "qual")+
  ylab("α") +
  xlab("") +
  scale_x_discrete(labels = c(expression(paste(α[Before])), 
                              expression(paste(α[After]))))


alfa.quad.lat

#_______________________________________________________________________________


beta.quad.lat <-ggplot(df_beta2_quad_lat) + aes(x = Model, y = Beta2_models, fill = Model) + 
  geom_point(aes(color = Model), position = position_jitter(w = .15),
             size = 0.5,alpha = 0.15, show.legend = F) +
  geom_boxplot(width = .24,  outlier.shape = NA,
               alpha = 0.5, show.legend = F) +
  geom_flat_violin(position = position_nudge(x = .2), trim = TRUE, 
                   alpha = 0.7, scale = "width", show.legend = F)  +
  theme_test(base_size = 10) +
  scale_fill_brewer(palette = "Pastel1", type = "qual")+
  ylab("β") +
  xlab("") +
  scale_x_discrete(labels = c(expression(paste(β[Before])), 
                              expression(paste(β[After]))))

beta.quad.lat

#_______________________________________________________________________________

{tiff("Figure.coeficientes.tiff", width = 169, height = 60, res = 600, units = "mm")
  
  figure.coeficientes <- ggarrange(R2.quad.lat, alfa.quad.lat, beta.quad.lat, ncol = 3, labels = c("(a)", "(b)","(c)"),
                                   font.label = list(size = 10, face = "plain", color ="black"))}
figure.coeficientes

dev.off()



# Anova -------------------------------------------------------------------


#remove the models that were not adjusted
simu_corais <- simu[complete.cases(simu), ]

#Adjusted R-squared maximum and minimum

min(simu_corais$R2_quad_lat_antes)
max(simu_corais$R2_quad_lat_antes)


#Adjusted R-squared maximum and minimum

min(simu_corais$R2_quad_lat_depois)
max(simu_corais$R2_quad_lat_depois)

#ANOVA of the beta of the quadratic model of latitude (before minus after)

#_____________________________com um lm_________________________________________
modelo.lm<-lm ((beta2_quad_lat_depois - beta2_quad_lat_antes) ~ (as.character(index_tree)), 
               data = simu_corais)

anova(modelo.lm)
summary(modelo.lm)

install.packages("survival")
install.packages("survminer")
install.packages("rmarkdown")
install.packages("broom")
install.packages("rmarkdown", type = "source")
install.packages("remotes")
install.packages("ggeffects")
install.packages("sjPlot")



remotes::install_github("jasp-stats/jaspDescriptives")
remotes::install_github("jasp-stats/jaspBase")
remotes::install_github("jasp-stats/jaspGraphs")
remotes::install_github("jasp-stats/jaspTools")
remotes::install_github("jasp-stats/jaspResults")

library(tidyverse)
library(readxl)
library(ggplot2)
library(ggpubr)
library(installr)
library(AICcmodavg)
library(skimr)
library(gridExtra)
library(survival)
library(survminer)
library(dplyr)
library(rmarkdown)
library(broom)
library(jaspDescriptives)
library(ggeffects)
library(sjPlot)

##Base original 
tromb <- read.csv("~/Rstudio_jobs/Cristian/Tromb_final.csv")
setwd("~/Rstudio_jobs/Cristian/")

#DESCRIPTIVOS TODAS LAS VARIABLES 
skim(tromb)

##Estimación de Dx a muerte (SUPERVIVENCIA GLOBAL)
tromb.surv <- Surv(tromb$D_DX_DEF, tromb$delta_DEF)  #Sustituir por D_DX_DEF para cálculo por días 
tromb.km <- survfit(tromb.surv ~ 1, data = tromb, type = "kaplan-meier") 
summary(tromb.km)
skim(tromb.surv)


#Gráfica ROJO 
ROJO <- ggsurvplot(fit = tromb.km, data = tromb, conf.int = T, color = "#F4320B", conf.int.fill = "#9E918F", title = "Curva de Supervivencia Global", 
                   xlab = "Dx a defunción (días)", ylab = "Supervivencia Global", legend.title = "Estimación", 
                   legend.labs = "Kaplan-Meier") 

##Guardar imagen 
ROJO$plot
ggsave("ROJO.tiff", ROJO$plot, dpi = 300)



##Estimación del dx Ca a tromb 
tromb.surv2 <- Surv(tromb$D_DX_TROM_M, tromb$delta_TROMB)  #Sustituir por D_DX_TROM para cálculo por días
tromb.km2 <- survfit(tromb.surv2 ~ 1, data = tromb, type = "kaplan-meier") 
summary(tromb.km2)
#skim(tromb.km2)


#Gráfica Dx a Tromb 
AZUL <- ggsurvplot(fit = tromb.km2, data = tromb, conf.int = T,  color = "#1322D4", conf.int.fill = "#9E918F", title = "Curva de Supervivencia de Dx a 1er trombosis", 
                   xlab = "Tiempo transcurrido del Dx de Ca a 1er trombosis (meses)", ylab = "Probabilidad de Trombosis después del Dx de Ca", legend.title = "Estimación", 
                   legend.labs = "Kaplan-Meier")
##Guardar
AZUL$plot
ggsave("AZUL.tiff", AZUL$plot, dpi = 300)


#write.csv(tromb.surv, file = "tromb.surv.csv")
#write.csv(tromb.km, file = "tromb.km.csv")
#write.csv(tromb.surv2, file = "tromb.surv2.csv")
#write.csv(tromb.km2, file = "tromb.km2.csv")

#rmarkdown::render("supervivencia.Rmd")
#dftromb <- broom::tidy(trombR)


##-------------------------
##Separar por grupo 
##-------------------------

fit_Dx <- survfit(
  Surv(tromb$D_DX_DEF_M, tromb$delta_DEF) ~ Dx,
  data = tromb
)

tromb$Dx <- factor(tromb$Dx)


survdiff(Surv(tromb$D_DX_DEF_M, tromb$delta_DEF) ~ Dx, data = tromb)

TODOS <- ggsurvplot(
  fit_Dx,
  data = tromb,
  conf.int.alpha = 0.15,   # sombra más tenue
  size = 1.3, 
  conf.int = TRUE,
  pval = TRUE,
  risk.table = TRUE,
  palette = c("#C45161","#72B043","#F8CC1B"),
  title = "Curvas de Supervivencia por Grupo",
  xlab = "Tiempo del Dx a Defunción",
  ylab = "Supervivencia Global",
  legend.title = "Ca",
  legend.labs = c("Ovario", "Cervix", "Endometrio")
)

TODOS$plot


##Filtrar para generar curvas individuales de cada Dx
trombDx1 <- tromb %>% 
  filter(Dx == 1)
tromb.survDx1 <- Surv(trombDx1$D_DX_TROMB, trombDx1$delta_TROMB)  #Creando objeto tipo Surv
tromb.kmDx1 <- survfit(tromb.survDx1 ~ 1, data = trombDx1, type = "kaplan-meier") 
summary(tromb.kmDx1)
skim(trombDx1)


#Gráfica
ggsurvplot(fit = tromb.kmDx1, data = tromb, conf.int = T, title = "Curva de Supervivencia Dx1", 
           xlab = "Tiempo transcurrido del Dx de Ca a 1er trombosis", ylab = "Probabilidad de supervivencia", legend.title = "Estimación", 
           legend.labs = "Kaplan-Meier")


##2da trombosis

tromb_2da <- tromb %>% 
  filter(tromb_1a2 == 2)
skim(tromb_2da)

##Estadística JASP en R 
jaspDescriptives::Descriptives(
  version = "0.18",
  formula =  ~ tromb_1a2,
  median = TRUE,
  range = TRUE,
  splitBy = "Ca")



##Gráfica de barras
ggplot(data=tromb, aes(x=Ca, y=tromb_1a2, group=Ca, color =Ca, fill= Ca)) +
  geom_point(size = 3) + 
  scale_color_manual(values = c ("#C45161","#72B043","#F8CC1B")) +
  geom_col(position="dodge") +
  #geom_rect(aes(xmin=0, xmax=5, ymin=0.31, ymax=0.32), fill="white", linewidth = 1) +
  #scale_y_continuous(limits=c(0,NA), breaks=trans(yticks), labels=yticks) +
  labs(y="2da trombosis") + 
  #my_theme2 + 
  theme_classic() + 
  scale_y_continuous(breaks = c(1, 2))

#-------------------------------------
##Violin plot para edad por tipo de Ca 
#-------------------------------------

##filtrar sólo para las que tuvieron trombosis 
#edad_filtrada_trom <- tromb %>% 
#  filter(delta_TROMB == 1)

edad_plot <- ggviolin(tromb, x = "Ca", y = "Edad",
                      add = c("jitter", "mean_sd"), color = "Ca", palette = c("#C45161","#72B043","#F8CC1B"), 
                      size =.5, panel.labs = NULL, short.panel.labs = FALSE, label.rectangle = FALSE,
                      add.params = list(size = 1.5, alpha = 0.5),
                      #ylim = c(-500, 4000),
                      #scale_y_continuous(breaks = c(1, 2)), 
                      xlab = "Tipo de Ca",
                      ylab = "Edad",
                      font.x = c(20),
                      font.y = c(20),
                      font.tickslab = c(15,"bold")) 

plot(edad_plot)



##Estimación de curva con covariablaes 
cox_mod <- coxph(
  Surv(tromb$D_DX_DEF_M, tromb$delta_DEF) ~ Dx + Edad + Grado_Dif + , data = tromb)


cox_mod2 <- coxph(
  Surv(tromb$D_DX_TROM_M, tromb$delta_TROMB) ~ Dx + Edad + Grado_Dif , data = tromb)


#Gráfica de curva COX 
ggadjustedcurves(
  cox_mod2,
  data = tromb,
  method = "average",
  conf.int = TRUE,
  title = "Supervivencia global ajustada por covariables",
  xlab = "Tiempo de seguimiento",
  ylab = "Probabilidad de supervivencia"
)




##Modelo de regresión lineal para efecto de la edad por grupo 
modelo <- lm(
  D_DX_DEF_M ~ Edad + Ca, # + edad + sexo,
  data = tromb
)

pred <- ggpredict(modelo, terms = c("Edad", "Ca"))

plot(pred) + 
  geom_line(linewidth = 1.5) + 
  labs(
    x = "Edad",
    y = "SG ajustada",
    title = "Efecto ajustado de Edad sobre SG"
  ) + 
  theme_classic() + 
  scale_color_manual(values = c("#C45161", "#72B043", "#F8CC1B")) +
  scale_fill_manual(values  = c("#C45161", "#72B043", "#F8CC1B")) 

summary(modelo)



##Modelo de regresión lineal para SG general
modeloG <- lm(
  D_DX_DEF_M ~ Edad, # + edad + sexo,
  data = tromb
)

predG <- ggpredict(modeloG, terms = "Edad")

ggplot(
  predG,
  aes(x = x, y = predicted)
) +
  geom_line(
    color = "#F4320B",
    linewidth = 1.5
  ) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high),
    fill = "#F4320B",
    alpha = 0.15
  ) +
  labs(
    x = "Edad",
    y = "SG ajustada",
    title = "Efecto ajustado de Edad sobre SG"
  ) +
  theme_classic()# + 
 # scale_color_manual(values = c("#C45161", "#72B043", "#F8CC1B")) +
#  scale_fill_manual(values  = c("#C45161", "#72B043", "#F8CC1B"))

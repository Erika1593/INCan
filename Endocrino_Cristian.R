library(tidyverse)
library(readxl)
library(ggplot2)
library(ggpubr)
library(installr)
library(skimr)
library(gridExtra)
library(survival)
library(survminer)
library(jaspDescriptives)
library(ggeffects)
library(sjPlot)

##Base original 
Endo <- read.csv("~/Rstudio_jobs/Cristian/Endocrino.csv")

#DESCRIPTIVOS TODAS LAS VARIABLES 
skim(Endo)

##Estimación de RHP a ultima visita (SUPERVIVENCIA GLOBAL)
endo.surv <- Surv(Endo$D_RHP_VIS_M, Endo$DELTA_RHP_VIS)  
endo.km <- survfit(endo.surv ~ 1, data = Endo, type = "kaplan-meier") 
summary(endo.km)
skim(endo.surv)


#Gráfica ROJO 
ROJO <- ggsurvplot(fit = endo.km, data = Endo, conf.int = T, color = "#F4320B", conf.int.fill = "#9E918F", title = "Curva de Supervivencia Global", 
                   xlab = "Dx (RHP) a ùltima visita (meses)", ylab = "Supervivencia Global", legend.title = "Estimación", 
                   legend.labs = "Kaplan-Meier") 

##Guardar imagen 
ROJO$plot
ggsave("ROJO.tiff", ROJO$plot, dpi = 300)


##-------------------------
##Separar por grupo 
##-------------------------

fit_Dx_endo <- survfit(
  Surv(Endo$D_RHP_VIS_M, Endo$DELTA_RHP_VIS) ~ Localizacion,
  data = Endo
)

Endo$Localizacion <- factor(Endo$Localizacion)


survdiff(Surv(Endo$D_RHP_VIS_M, Endo$DELTA_RHP_VIS) ~ Localizacion, data = Endo)

TODOS_endo <- ggsurvplot(
  fit_Dx_endo,
  data = Endo,
  conf.int.alpha = 0,   # sombra más tenue
  size = 1.3, 
  conf.int = TRUE,
  pval = TRUE,
  risk.table = TRUE,
  palette = c("#A327F5","#252D69","#E04BEB", "#345E36", "#4BEB53"),
  title = "Curvas de Supervivencia por Grupo",
  xlab = "Tiempo del RHP a Última visita",
  ylab = "Supervivencia Global",
  legend.title = "Ca",
  legend.labs = c("Gástrico", "Intestino delgado", "Páncreas", "Colorrectal", "Hìgado y Vías biliares")
)

TODOS_endo$plot


##Separar por grado de diferenciación 

fit_gd_endo <- survfit(
  Surv(Endo$D_RHP_VIS_M, Endo$DELTA_RHP_VIS) ~ Grado_dif,
  data = Endo
)

Endo$Grado_dif <- factor(Endo$Grado_dif)


survdiff(Surv(Endo$D_RHP_VIS_M, Endo$DELTA_RHP_VIS) ~ Grado_dif, data = Endo)

Gd_endo <- ggsurvplot(
  fit_gd_endo,
  data = Endo,
  conf.int.alpha = 0,   # sombra más tenue
  size = 1.3, 
  conf.int = TRUE,
  pval = TRUE,
  risk.table = TRUE,
  palette = c("#49EB09","#091CEB","#EB0998"),
  title = "Curvas de Supervivencia por Grado de Diferenciación",
  xlab = "Tiempo del RHP a Última visita",
  ylab = "Supervivencia Global",
  legend.title = "Grado de Diferenciación",
  legend.labs = c("NE", "G1", "G2")
)

Gd_endo$plot



##Estimación de curva con covariablaes 
cox_mod_gd <- coxph(
  Surv(Endo$D_RHP_VIS_M, Endo$DELTA_RHP_VIS) ~ Grado_dif + Localizacion, data = Endo)


#Gráfica de curva COX 
ggadjustedcurves(
  cox_mod_gd,
  data = Endo,
  method = "average",
  conf.int = TRUE,
  title = "Supervivencia global ajustada por covariables",
  xlab = "Tiempo de seguimiento",
  ylab = "Probabilidad de supervivencia"
)




##Modelo de regresión lineal para efecto del grado de diferenciación por grupo 
modelo_gd <- lm(
  D_RHP_VIS_M ~ Grado_dif + Localizacion, # + edad + sexo,
  data = Endo
)

pred_gd <- ggpredict(modelo_gd, terms = c("Grado_dif", "Localizacion"))

plot(pred_gd) + 
  geom_line(linewidth = 1.5) + 
  labs(
    x = "Grado de diferenciación",
    y = "SG ajustada",
    title = "Efecto ajustado de GD sobre SG"
  ) + 
  theme_classic() + 
  scale_color_manual(values = c("#49EB09","#091CEB","#EB0998", "#345E36", "#A327F5")) +
  scale_fill_manual(values  = c("#49EB09","#091CEB","#EB0998", "#345E36", "#A327F5")) 

summary(modelo_gd)



##Modelo de regresión lineal para SG general
mmodelo_gd <- lm(
  D_RHP_VIS_M ~ Grado_dif + Localizacion, # + edad + sexo,
  data = Endo
)

pred_gd <- ggpredict(modelo_gd, terms = c("Grado_dif", "Localizacion"))

ggplot(
  pred_gd,
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
    x = "Grado de diferenciación",
    y = "SG ajustada",
    title = "Efecto ajustado de GD sobre SG"
  ) +
  theme_classic()# + 
 # scale_color_manual(values = c("#C45161", "#72B043", "#F8CC1B")) +
#  scale_fill_manual(values  = c("#C45161", "#72B043", "#F8CC1B"))


#-------------------------------------
##Violin plot para edad y metástasis por tipo de Ca 
#-------------------------------------


edad_endo <- ggviolin(Endo, x = "Localizacion", y = "Edad",
                      add = c("jitter", "mean_sd"), color = "Localizacion", palette = c("#A327F5","#252D69","#E04BEB", "#345E36", "#4BEB53", "#49EB09","#091CEB"), 
                      size =.5, panel.labs = NULL, short.panel.labs = FALSE, label.rectangle = FALSE,
                      add.params = list(size = 1.5, alpha = 0.5),
                      #ylim = c(-500, 4000),
                      #scale_y_continuous(breaks = c(1, 2)), 
                      xlab = "Tipo de Ca",
                      ylab = "Edad",
                      font.x = c(20),
                      font.y = c(20),
                      font.tickslab = c(15,"bold")) 

plot(edad_endo)

## METÁSTASIS 


metsh_endo <- ggviolin(Endo, x = "Localizacion", y = "METS_HIG",
                      add = c("jitter", "mean_sd"), color = "Localizacion", palette = c("#A327F5","#252D69","#E04BEB", "#345E36", "#4BEB53", "#49EB09","#091CEB"), 
                      size =.5, panel.labs = NULL, short.panel.labs = FALSE, label.rectangle = FALSE,
                      add.params = list(size = 1.5, alpha = 0.5),
                      #ylim = c(-500, 4000),
                      #scale_y_continuous(breaks = c(1, 2)), 
                      xlab = "Tipo de Ca",
                      ylab = "Metástasis a Hígado",
                      font.x = c(20),
                      font.y = c(20),
                      font.tickslab = c(15,"bold")) 

plot(metsh_endo)

metsgl_endo <- ggviolin(Endo, x = "Localizacion", y = "METS_GL_NOREG",
                       add = c("jitter", "mean_sd"), color = "Localizacion", palette = c("#A327F5","#252D69","#E04BEB", "#345E36", "#4BEB53", "#49EB09","#091CEB"), 
                       size =.5, panel.labs = NULL, short.panel.labs = FALSE, label.rectangle = FALSE,
                       add.params = list(size = 1.5, alpha = 0.5),
                       #ylim = c(-500, 4000),
                       #scale_y_continuous(breaks = c(1, 2)), 
                       xlab = "Tipo de Ca",
                       ylab = "Metástasis a Ganglios no regionales",
                       font.x = c(20),
                       font.y = c(20),
                       font.tickslab = c(15,"bold")) 

plot(metsgl_endo)


metso_endo <- ggviolin(Endo, x = "Localizacion", y = "METS_OTROS",
                        add = c("jitter", "mean_sd"), color = "Localizacion", palette = c("#A327F5","#252D69","#E04BEB", "#345E36", "#4BEB53", "#49EB09","#091CEB"), 
                        size =.5, panel.labs = NULL, short.panel.labs = FALSE, label.rectangle = FALSE,
                        add.params = list(size = 1.5, alpha = 0.5),
                        #ylim = c(-500, 4000),
                        #scale_y_continuous(breaks = c(1, 2)), 
                        xlab = "Tipo de Ca",
                        ylab = "Metástasis a otros sitios",
                        font.x = c(20),
                        font.y = c(20),
                        font.tickslab = c(15,"bold")) 

plot(metso_endo)


tx1ra_endo <- ggviolin(Endo, x = "Localizacion", y = "prim_linea_tx",
                       add = c("jitter", "mean_sd"), color = "Localizacion", palette = c("#A327F5","#252D69","#E04BEB", "#345E36", "#4BEB53", "#49EB09","#091CEB"), 
                       size =.5, panel.labs = NULL, short.panel.labs = FALSE, label.rectangle = FALSE,
                       add.params = list(size = 1.5, alpha = 0.5),
                       #ylim = c(-500, 4000),
                       #scale_y_continuous(breaks = c(1, 2)), 
                       xlab = "Tipo de Ca",
                       ylab = "1ra línea de tx",
                       font.x = c(20),
                       font.y = c(20),
                       font.tickslab = c(15,"bold")) 

plot(tx1ra_endo)


#curva de SG ajustada a 1ra línea de tx 

fit_tx_endo <- survfit(
  Surv(Endo$D_RHP_VIS_M, Endo$DELTA_RHP_VIS) ~ prim_linea_tx,
  data = Endo
)

Endo$prim_linea_tx <- factor(Endo$prim_linea_tx)


survdiff(Surv(Endo$D_RHP_VIS_M, Endo$DELTA_RHP_VIS) ~ prim_linea_tx, data = Endo)

tx_endo <- ggsurvplot(
  fit_tx_endo,
  data = Endo,
  conf.int.alpha = 0,   # sombra más tenue
  size = 1.3, 
  conf.int = TRUE,
  pval = TRUE,
  risk.table = TRUE,
  palette = c("#49EB09","#091CEB","#EB0998", "#7209EB", "#09EBB2", "#94A19D"),
  title = "Curvas de Supervivencia por 1ra línea de tx",
  xlab = "Tiempo del RHP a Última visita",
  ylab = "Supervivencia Global",
  legend.title = "1ra línea de tx",
  legend.labs = c("ARS", "CAPE", "EVERO", "EP", "OTRAS", "NINGUNA")
)

tx_endo$plot



##Estimación de curva con covariablaes 
cox_mod_tx <- coxph(
  Surv(Endo$D_RHP_VIS_M, Endo$DELTA_RHP_VIS) ~ prim_linea_tx + Localizacion, data = Endo)


#Gráfica de curva COX 
ggadjustedcurves(
  cox_mod_tx,
  data = Endo,
  method = "average",
  conf.int = TRUE,
  title = "Supervivencia global ajustada por covariables",
  xlab = "Tiempo de seguimiento",
  ylab = "Probabilidad de supervivencia"
)




##Modelo de regresión lineal para efecto del grado de diferenciación por grupo 
modelo_tx <- lm(
  D_RHP_VIS_M ~ prim_linea_tx + Localizacion, # + edad + sexo,
  data = Endo
)

pred_tx <- ggpredict(modelo_tx, terms = c("prim_linea_tx", "Localizacion"))

plot(pred_tx) + 
  geom_line(linewidth = 1.5) + 
  labs(
    x = "Grado de diferenciación",
    y = "SG ajustada",
    title = "Efecto ajustado de Tx sobre SG"
  ) + 
  theme_classic() + 
  scale_color_manual(values = c("#49EB09","#091CEB","#EB0998", "#345E36", "#A327F5")) +
  scale_fill_manual(values  = c("#49EB09","#091CEB","#EB0998", "#345E36", "#A327F5")) 

summary(modelo_tx)



--------------------------------------------------------------------------------
#2da versión del análisis                         
--------------------------------------------------------------------------------


##Base original 
Endo_n <- read.csv("~/Rstudio_jobs/Cristian/Endocrino_n.csv", stringsAsFactors = FALSE)

#DESCRIPTIVOS TODAS LAS VARIABLES 
skim(Endo)

##Estimación de RHP a ultima visita (SUPERVIVENCIA GLOBAL)
endo.surv <- Surv(Endo$D_RHP_VIS_M, Endo$DELTA_RHP_VIS)  
endo.km <- survfit(endo.surv ~ 1, data = Endo, type = "kaplan-meier") 
summary(endo.km)
skim(endo.surv)


#Gráfica ROJO 
ROJO <- ggsurvplot(fit = endo.km, data = Endo, conf.int = T, color = "#F4320B", conf.int.fill = "#9E918F", title = "Curva de Supervivencia Global", 
                   xlab = "Dx (RHP) a ùltima visita (meses)", ylab = "Supervivencia Global", legend.title = "Estimación", 
                   legend.labs = "Kaplan-Meier") 

##Guardar imagen 
ROJO$plot
ggsave("ROJO.tiff", ROJO$plot, dpi = 300)


##-------------------------
##Separar por grupo 
##-------------------------

fit_Ki_endo <- survfit(
  Surv(Endo$D_RHP_VIS_M, Endo$DELTA_RHP_VIS) ~ Ki,
  data = Endo
)

Endo$Ki <- factor(Endo$Ki)


survdiff(Surv(Endo$D_RHP_VIS_M, Endo$DELTA_RHP_VIS) ~ Ki, data = Endo)

Ki_endo <- ggsurvplot(
  fit_Ki_endo,
  data = Endo,
  conf.int.alpha = 0,   # sombra más tenue
  size = 1.3, 
  conf.int = TRUE,
  pval = TRUE,
  risk.table = TRUE,
  palette = c("#A327F5","#252D69","#E04BEB", "#345E36"),
  title = "Curva de Supervivencia por Grado de diferenciación",
  xlab = "Tiempo del RHP a Última visita",
  ylab = "Supervivencia Global",
  legend.title = "Ki",
  legend.labs = c("NE", "G1", "G2", "G3")
)

Ki_endo$plot


##Separar por grado de diferenciación 

fit_ki_endo <- survfit(
  Surv(Endo$D_RHP_VIS_M, Endo$DELTA_RHP_VIS) ~ Ki,
  data = Endo
)

Endo$Ki <- factor(Endo$Ki)


survdiff(Surv(Endo$D_RHP_VIS_M, Endo$DELTA_RHP_VIS) ~ Ki, data = Endo)

ki_endo <- ggsurvplot(
  fit_ki_endo,
  data = Endo,
  conf.int.alpha = 0,   # sombra más tenue
  size = 1.3, 
  conf.int = TRUE,
  pval = TRUE,
  risk.table = TRUE,
  palette = c("#49EB09","#091CEB","#EB0998","#252D69"),
  title = "Curvas de Supervivencia por Grado de Diferenciación",
  xlab = "Tiempo del RHP a Última visita",
  ylab = "Supervivencia Global",
  legend.title = "Grado de Diferenciación",
  legend.labs = c("NE", "G1", "G2", "G3")
)

ki_endo$plot



##Estimación de curva con covariablaes 
cox_mod_ki <- coxph(
  Surv(Endo$D_RHP_VIS_M, Endo$DELTA_RHP_VIS) ~ Ki, data = Endo)


#Gráfica de curva COX 
ggadjustedcurves(
  cox_mod_ki,
  data = Endo,
  method = "average",
  conf.int = TRUE,
  title = "Supervivencia global ajustada por covariables",
  xlab = "Tiempo de seguimiento",
  ylab = "Probabilidad de supervivencia"
)




##Modelo de regresión lineal para efecto del grado de diferenciación por grupo 
modelo_ki <- lm(
  D_RHP_VIS_M ~ Ki, # + edad + sexo,
  data = Endo
)

pred_ki <- ggpredict(modelo_ki, terms = c("Ki"))

plot(pred_ki) + 
  geom_line(linewidth = 1.5) + 
  labs(
    x = "Grado de diferenciación",
    y = "SG ajustada",
    title = "Efecto ajustado de GD sobre SG"
  ) + 
  theme_classic() + 
  scale_color_manual(values = c("#49EB09","#091CEB","#EB0998", "#345E36", "#A327F5")) +
  scale_fill_manual(values  = c("#49EB09","#091CEB","#EB0998", "#345E36", "#A327F5")) 

summary(modelo_ki)



##Modelo de regresión lineal para SG general
modelo_ki <- lm(
  D_RHP_VIS_M ~ Ki, # + edad + sexo,
  data = Endo
)

pred_ki <- ggpredict(modelo_ki, terms = c("Ki"))

ggplot(
  pred_ki,
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
    x = "Grado de diferenciación",
    y = "SG ajustada",
    title = "Efecto ajustado de GD sobre SG"
  ) +
  theme_classic()# + 
 # scale_color_manual(values = c("#C45161", "#72B043", "#F8CC1B")) +
#  scale_fill_manual(values  = c("#C45161", "#72B043", "#F8CC1B"))


#-------------------------------------
##Violin plot para edad y metástasis por tipo de Ca 
#-------------------------------------

##quitar NAs
edad_endo <- ggviolin(
  Endo_n[!is.na(Endo_n$Edad) & !is.na(Endo_n$Localizacion_Ca), ],
  x = "Localizacion_Ca",
  y = "Edad",
  add = c("jitter", "mean_sd"),
  color = "Localizacion_Ca",
  palette = c("#A327F5", "#252D69", "#E04BEB", "#345E36", "#4BEB53"),
  size = 0.5,
  add.params = list(size = 1.5, alpha = 0.5),
  xlab = "Tipo de Ca",
  ylab = "Edad",
  font.x = c(20),
  font.y = c(20),
  font.tickslab = c(15, "bold")
) +
  coord_cartesian(ylim = range(Endo_n$Edad, na.rm = TRUE))


plot(edad_endo)




## METÁSTASIS 

##Gráfica de barras
#higado
Endo_n$METS_HIG_f <- factor(
  Endo_n$METS_HIG,
  levels = c(1, 2, 3, 4, 5)
)


ggplot(
  Endo_n[!is.na(Endo_n$METS_HIG_f) & !is.na(Endo_n$Localizacion_Ca), ],
  aes(
    x = Localizacion_Ca,
    fill = METS_HIG_f
  )
) +
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("#A327F5", "#252D69", "#E04BEB", "#345E36", "#4BEB53")
  ) +
  labs(
    y = "Frecuencia",
    fill = "Metástasis a Hígado"
  ) +
  theme_classic()


#GANGLIOS NO REGIONALES 
Endo_n$METS_GL_NOREG_f <- factor(
  Endo_n$METS_GL_NOREG,
  levels = c(1, 2, 3, 4, 5)
)


ggplot(
  Endo_n[!is.na(Endo_n$METS_GL_NOREG_f) & !is.na(Endo_n$Localizacion_Ca), ],
  aes(
    x = Localizacion_Ca,
    fill = METS_GL_NOREG_f
  )
) +
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("#E04BEB", "#345E36")
  ) +
  labs(
    y = "Frecuencia",
    fill = "Metástasis a Ganglios no regionales"
  ) +
  theme_classic()




#OTROS SITIOS
Endo_n$MET_OTROS_f <- factor(
  Endo_n$MET_OTROS,
  levels = c(1, 2, 3, 4, 5)
)


ggplot(
  Endo_n[!is.na(Endo_n$MET_OTROS_f) & !is.na(Endo_n$Localizacion_Ca), ],
  aes(
    x = Localizacion_Ca,
    fill = MET_OTROS_f
  )
) +
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("#4BEB53", "#091CEB")
  ) +
  labs(
    y = "Frecuencia",
    fill = "Metástasis a otros sitios"
  ) +
  theme_classic()





#OTratamientos
Endo_n$prim_linea_tx_f <- factor(
  Endo_n$prim_linea_tx,
  levels = c(1, 2, 3, 4, 5)
)


ggplot(
  Endo_n[!is.na(Endo_n$prim_linea_tx_f) & !is.na(Endo_n$Localizacion_Ca), ],
  aes(
    x = Localizacion_Ca,
    fill = prim_linea_tx_f
  )
) +
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("#A327F5", "#252D69", "#E04BEB", "#345E36", "#4BEB53")
  ) +
  labs(
    y = "Frecuencia",
    fill = "1ra línea de Tx"
  ) +
  theme_classic()




tx1ra_endo <- ggviolin(Endo, x = "Localizacion", y = "prim_linea_tx",
                       add = c("jitter", "mean_sd"), color = "Localizacion", palette = c("#A327F5","#252D69","#E04BEB", "#345E36", "#4BEB53", "#49EB09","#091CEB"), 
                       size =.5, panel.labs = NULL, short.panel.labs = FALSE, label.rectangle = FALSE,
                       add.params = list(size = 1.5, alpha = 0.5),
                       #ylim = c(-500, 4000),
                       #scale_y_continuous(breaks = c(1, 2)), 
                       xlab = "Tipo de Ca",
                       ylab = "1ra línea de tx",
                       font.x = c(20),
                       font.y = c(20),
                       font.tickslab = c(15,"bold")) 

plot(tx1ra_endo)


#curva de SG ajustada a 1ra línea de tx 

fit_tx_endo <- survfit(
  Surv(Endo$D_RHP_VIS_M, Endo$DELTA_RHP_VIS) ~ prim_linea_tx,
  data = Endo
)

Endo$prim_linea_tx <- factor(Endo$prim_linea_tx)


survdiff(Surv(Endo$D_RHP_VIS_M, Endo$DELTA_RHP_VIS) ~ prim_linea_tx, data = Endo)

tx_endo <- ggsurvplot(
  fit_tx_endo,
  data = Endo,
  conf.int.alpha = 0,   # sombra más tenue
  size = 1.3, 
  conf.int = TRUE,
  pval = TRUE,
  risk.table = TRUE,
  palette = c("#49EB09","#091CEB","#EB0998", "#7209EB", "#09EBB2", "#94A19D"),
  title = "Curvas de Supervivencia por 1ra línea de tx",
  xlab = "Tiempo del RHP a Última visita",
  ylab = "Supervivencia Global",
  legend.title = "1ra línea de tx",
  legend.labs = c("ARS", "CAPE", "EVERO", "EP", "OTRAS", "NINGUNA")
)

tx_endo$plot



##Estimación de curva con covariablaes 
cox_mod_tx <- coxph(
  Surv(Endo$D_RHP_VIS_M, Endo$DELTA_RHP_VIS) ~ prim_linea_tx + Localizacion, data = Endo)


#Gráfica de curva COX 
ggadjustedcurves(
  cox_mod_tx,
  data = Endo,
  method = "average",
  conf.int = TRUE,
  title = "Supervivencia global ajustada por covariables",
  xlab = "Tiempo de seguimiento",
  ylab = "Probabilidad de supervivencia"
)




##Modelo de regresión lineal para efecto del grado de diferenciación por grupo 
modelo_tx <- lm(
  D_RHP_VIS_M ~ prim_linea_tx + Localizacion, # + edad + sexo,
  data = Endo
)

pred_tx <- ggpredict(modelo_tx, terms = c("prim_linea_tx", "Localizacion"))

plot(pred_tx) + 
  geom_line(linewidth = 1.5) + 
  labs(
    x = "Grado de diferenciación",
    y = "SG ajustada",
    title = "Efecto ajustado de Tx sobre SG"
  ) + 
  theme_classic() + 
  scale_color_manual(values = c("#49EB09","#091CEB","#EB0998", "#345E36", "#A327F5")) +
  scale_fill_manual(values  = c("#49EB09","#091CEB","#EB0998", "#345E36", "#A327F5")) 

summary(modelo_tx)


 

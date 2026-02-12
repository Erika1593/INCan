library(tidyverse)
library(readxl)
library(ggplot2)
library(ggpubr)
library(installr)
library(skimr)
library(gridExtra)
library(survival)
library(survminer)
#library(jaspDescriptives)
library(ggeffects)
library(sjPlot)

##Cargar base de datos TESIS JAVIER 
TESIS <- read.csv("~/Rstudio_jobs/Tesis_Javier/TESIS.csv")

##Filtrar para triple negativo 
TESIS_3N <- TESIS %>% 
  filter(Inmunofenotipo == 4)

##SLE 
SLE <- Surv(TESIS_3N$Tiempo_SLE, TESIS_3N$Super_libre_evento)  #Sustituir por D_DX_DEF para cálculo por días 
T_SLE <- survfit(SLE ~ 1, data = TESIS_3N, type = "kaplan-meier") 
summary(T_SLE)


#Gráfica SLE 

ROJO <- ggsurvplot(fit = T_SLE, data = TESIS_3N, conf.int = T, palette = c("#ED0C0C"), conf.int.fill = c("#BFC9CA"), title = "SLE", 
                   xlab = "meses", ylab = "SLE", legend.title = "Estimación", 
                   legend.labs = c("SLE")) 



##Guardar imagen 
ROJO$plot
ggsave("ROJO.tiff", ROJO$plot, dpi = 300)

##SLE Estratificado 

TESIS_3N$Estatus <- factor(TESIS_3N$Estatus, levels = c(0,1),
                          labels = c("Negativo", "Positivo"))

SLE <- Surv(TESIS_3N$Tiempo_SLE, TESIS_3N$Super_libre_evento)  #Sustituir por D_DX_DEF para cálculo por días 
T_SLE_E <- survfit(SLE ~ Estatus, data = TESIS_3N, type = "kaplan-meier") 
summary(T_SLE_E)


#Gráfica SLE Estratificado
ROSA <- ggsurvplot(fit = T_SLE_E, data = TESIS_3N, conf.int = T, palette = c("#ff5aa4", "#2E86C1"), conf.int.fill = c("#BFC9CA"), title = "SLE", 
                   xlab = "meses", ylab = "SLE", legend.title = "Estimación", 
                   legend.labs = c("irAE presente", "irAE ausente")) 


##Guardar imagen 
ROSA$plot
ggsave("ROSA.tiff", ROSA$plot, dpi = 300)


##SG
SG <- Surv(TESIS_3N$Tiempo_SG, TESIS_3N$Vivo_muerto)  
T_SG <- survfit(SG ~ 1, data = TESIS_3N, type = "kaplan-meier") 
summary(T_SG)


#Gráfica SG

AZUL <- ggsurvplot(fit = T_SG, data = TESIS_3N, conf.int = T, palette = c("#100CED"), conf.int.fill = c("#BFC9CA"), title = "SG", 
                   xlab = "meses", ylab = "SG", legend.title = "Estimación", 
                   legend.labs = c("SG")) 



##Guardar imagen 
AZUL$plot
ggsave("AZUL.tiff", AZUL$plot, dpi = 300)


##SG Estratificado 

TESIS_3N$Estatus <- factor(TESIS_3N$Estatus, levels = c(0,1),
                           labels = c("Negativo", "Positivo"))

SG_E <- Surv(TESIS_3N$Tiempo_SG, TESIS_3N$Vivo_muerto) 
T_SG_E <- survfit(SG ~ Estatus, data = TESIS_3N, type = "kaplan-meier") 
summary(T_SG_E)


#Gráfica SLE Estratificado
AZULC <- ggsurvplot(fit = T_SG_E, data = TESIS_3N, conf.int = T, palette = c("#ff5aa4", "#2E86C1"), conf.int.fill = c("#BFC9CA"), title = "SG", 
                   xlab = "meses", ylab = "SG", legend.title = "Estimación", 
                   legend.labs = c("irAE presente", "irAE ausente")) 


##Guardar imagen 
AZULC$plot
ggsave("AZULC.tiff", AZULC$plot, dpi = 300)


##Filtrar por cirugía 
TESIS_C <- TESIS_3N %>%
  filter(Cirugia_de_mama %in% c(1, 2))

##SLE pCR

SLE_p <- Surv(TESIS_C$Tiempo_SLE, TESIS_C$Super_libre_evento) 
T_SLE_p <- survfit(SLE ~ 1, data = TESIS_C, type = "kaplan-meier") 
summary(T_SLE_p)

TESIS_C$pCR <- factor(TESIS_C$pCR, levels = c(0,1),
                      labels = c("Negativo", "Positivo"))

SLE_p <- Surv(TESIS_C$Tiempo_SLE, TESIS_C$Super_libre_evento) 
T_SLE_p <- survfit(SLE_p ~ pCR, data = TESIS_C, type = "kaplan-meier") 
summary(T_SLE_p)



#Gráfica SLE Estratificado por pCR 
ROSA_pcr <- ggsurvplot(fit = T_SLE_p, data = TESIS_C, conf.int = T, palette = c("#ff5aa4", "#2E86C1"), conf.int.fill = c("#BFC9CA"), title = "SLE", 
                   xlab = "meses", ylab = "SLE", legend.title = "Estimación", 
                   legend.labs = c("pCR ausente", "pCR presente")) 


##Guardar imagen 
ROSA_pcr$plot
ggsave("ROSA.tiff", ROSA_pcr$plot, dpi = 300)


##SG Estratificado por pCR 

TESIS_C$pCR <- factor(TESIS_C$pCR, levels = c(0,1),
                           labels = c("Negativo", "Positivo"))

SG_P <- Surv(TESIS_C$Tiempo_SG, TESIS_C$Vivo_muerto) 
T_SG_P <- survfit(SG_P ~ pCR, data = TESIS_C, type = "kaplan-meier") 
summary(T_SG_P)


#Gráfica SLE Estratificado
AZULC <- ggsurvplot(fit = T_SG_P, data = TESIS_C, conf.int = T, palette = c("#ff5aa4", "#2E86C1"), conf.int.fill = c("#BFC9CA"), title = "SG", 
                    xlab = "meses", ylab = "SG", legend.title = "Estimación", 
                    legend.labs = c("pCR ausente", "pCR presente")) 


##Guardar imagen 
AZULC$plot
ggsave("AZULC.tiff", AZULC$plot, dpi = 300)





##SLE pCR

SLE_p <- Surv(TESIS_C$Tiempo_SLE, TESIS_C$Super_libre_evento) 
T_SLE_p <- survfit(SLE ~ 1, data = TESIS_C, type = "kaplan-meier") 
summary(T_SLE_p)

TESIS_C$pCR <- factor(TESIS_C$pCR, levels = c(0,1),
                      labels = c("Negativo", "Positivo"))

SLE_p <- Surv(TESIS_C$Tiempo_SLE, TESIS_C$Super_libre_evento) 
T_SLE_p <- survfit(SLE_p ~ pCR, data = TESIS_C, type = "kaplan-meier") 
summary(T_SLE_p)



#Gráfica SLE Estratificado por pCR 
ROSA_pcr <- ggsurvplot(fit = T_SLE_p, data = TESIS_C, conf.int = T, palette = c("#ff5aa4", "#2E86C1"), conf.int.fill = c("#BFC9CA"), title = "SLE", 
                   xlab = "meses", ylab = "SLE", legend.title = "Estimación", 
                   legend.labs = c("pCR ausente", "pCR presente")) 


##Guardar imagen 
ROSA_pcr$plot
ggsave("ROSA.tiff", ROSA_pcr$plot, dpi = 300)


##SG Estratificado por pCR 

TESIS_C$pCR <- factor(TESIS_C$pCR, levels = c(0,1),
                           labels = c("Negativo", "Positivo"))

SG_P <- Surv(TESIS_C$Tiempo_SG, TESIS_C$Vivo_muerto) 
T_SG_P <- survfit(SG_P ~ pCR, data = TESIS_C, type = "kaplan-meier") 
summary(T_SG_P)


#Gráfica SG Estratificado
AZULC <- ggsurvplot(fit = T_SG_P, data = TESIS_C, conf.int = T, palette = c("#ff5aa4", "#2E86C1"), conf.int.fill = c("#BFC9CA"), title = "SG", 
                    xlab = "meses", ylab = "SG", legend.title = "Estimación", 
                    legend.labs = c("pCR ausente", "pCR presente")) 


##Guardar imagen 
AZULC$plot
ggsave("AZULC.tiff", AZULC$plot, dpi = 300)




##SLE BCR 

SLE_b <- Surv(TESIS_C$Tiempo_SLE, TESIS_C$Super_libre_evento) 
T_SLE_b <- survfit(SLE_b ~ 1, data = TESIS_C, type = "kaplan-meier") 
summary(T_SLE_b)

TESIS_C$RCB_cat <- factor(TESIS_C$RCB_cat, levels = c(0,1),
                      labels = c("2-3", "0-1"))

SLE_b <- Surv(TESIS_C$Tiempo_SLE, TESIS_C$Super_libre_evento) 
T_SLE_b <- survfit(SLE_b ~ RCB_cat, data = TESIS_C, type = "kaplan-meier") 
summary(T_SLE_b)


#Gráfica SLE Estratificado por pCR 
ROSA_rcb <- ggsurvplot(fit = T_SLE_b, data = TESIS_C, conf.int = T, palette = c("#ff5aa4", "#2E86C1"), conf.int.fill = c("#BFC9CA"), title = "SLE", 
                       xlab = "meses", ylab = "SLE", legend.title = "Estimación", 
                       legend.labs = c("RCB 2-3", "RCB 0-1")) 



##Guardar imagen 
ROSA_rcb$plot
ggsave("ROSA.tiff", ROSA_pcr$plot, dpi = 300)



##SG Estratificado por RCB
TESIS_C$RCB_cat <- factor(TESIS_C$RCB_cat, levels = c(0,1),
                          labels = c("2-3", "0-1"))

SG_b <- Surv(TESIS_C$Tiempo_SG, TESIS_C$Vivo_muerto) 
T_SG_b <- survfit(SG_b ~ RCB_cat, data = TESIS_C, type = "kaplan-meier") 
summary(T_SG_b)


#Gráfica SG Estratificado
AZULC <- ggsurvplot(fit = T_SG_b, data = TESIS_C, conf.int = T, palette = c("#ff5aa4", "#2E86C1"), conf.int.fill = c("#BFC9CA"), title = "SG", 
                    xlab = "meses", ylab = "SG", legend.title = "Estimación", 
                    legend.labs = c("RCB 2-3", "RCB 0-1")) 


##Guardar imagen 
AZULC$plot
ggsave("AZULC.tiff", AZULC$plot, dpi = 300)


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
SLE <- Surv(TESIS_3N$Tiempo_SLE, TESIS_3N$Super_libre_evento)  
T_SLE <- survfit(SLE ~ 1, data = TESIS_3N, type = "kaplan-meier") 
summary(T_SLE)


#Gráfica SLE 

SLEp <- ggsurvplot(fit = T_SLE, data = TESIS_3N, fun = "pct", conf.int = F, palette = c("#1F4E79"), size = 1.2, #conf.int.fill = c("#BFC9CA"), 
                   title = "SLE", 
                   break.time.by = 5, xlim = c(0,30), ylim = c(0,100), 
                   xlab = "Meses", ylab = "Supervivencia (%)", legend.title = "Estimación", 
                   ggtheme = theme_classic(base_size = 14, base_family = "Helvetica"), 
                   legend.labs = c("SLE")) 

##ver gráfica
SLEp$plot

#personalización ggplot
SLEp$plot <- SLEp$plot + 
  scale_x_continuous(
    breaks = seq(0, 30, 5),
    limits = c(0, 30),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    limits = c(0, 100),
    expand = c(0, 0)
  ) +
  geom_vline(
    xintercept = 24,
    linetype = "dashed",
    color = "grey30",
    linewidth = 0.8
  ) +
  theme(
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(10, 15, 10, 10)
  )


print(SLEp)


##Guardar imagen 
SLEp$plot
ggsave("SLEP.tiff", SLEp$plot, dpi = 300)

##SLE Estratificado 

TESIS_3N$Estatus <- factor(TESIS_3N$Estatus, levels = c(0,1),
                           labels = c("Negativo", "Positivo"))

SLE <- Surv(TESIS_3N$Tiempo_SLE, TESIS_3N$Super_libre_evento)  
T_SLE_E <- survfit(SLE ~ Estatus, data = TESIS_3N, type = "kaplan-meier") 
summary(T_SLE_E)


#Gráfica SLE Estratificado
SLE2 <- ggsurvplot(fit = T_SLE_E, data = TESIS_3N, fun = "pct", conf.int = F, palette = c("#FC2C03", "#1F4E79"), size = 1.2, #conf.int.fill = c("#BFC9CA"), 
                   title = "SLE", 
                   break.time.by = 5, xlim = c(0,30), ylim = c(0,100), 
                   xlab = "Meses", ylab = "Supervivencia (%)", legend.title = "Estimación", 
                   ggtheme = theme_classic(base_size = 14, base_family = "Helvetica"), 
                   legend.labs = c("irAE presente", "irAE ausente")) 

SLE2$plot


##Guardar imagen 
SLE2$plot


#personalización ggplot
SLE2$plot <- SLE2$plot + 
  scale_x_continuous(
    breaks = seq(0, 30, 5),
    limits = c(0, 30),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    limits = c(0, 100),
    expand = c(0, 0)
  ) +
  geom_vline(
    xintercept = 24,
    linetype = "dashed",
    color = "grey30",
    linewidth = 0.8
  ) +
  theme(
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(10, 15, 10, 10)
  )


print(SLE2)

##GUARDAR
ggsave("SLE2.tiff", SLE2$plot, dpi = 300)


##SG
SG <- Surv(TESIS_3N$Tiempo_SG, TESIS_3N$Vivo_muerto)  
T_SG <- survfit(SG ~ 1, data = TESIS_3N, type = "kaplan-meier") 
summary(T_SG)


#Gráfica SG

SGp <- ggsurvplot(fit = T_SG, data = TESIS_3N, fun = "pct", conf.int = F, palette = c("#1F4E79"), size = 1.2, #conf.int.fill = c("#BFC9CA"), 
                  title = "SG",
                  break.time.by = 5, xlim = c(0,30), ylim = c(0,100),
                   xlab = "Meses", ylab = "Supervivencia (%)", legend.title = "Estimación", 
                  ggtheme = theme_classic(base_size = 14, base_family = "Helvetica"), 
                   legend.labs = c("SG")) 

SGp$plot

#personalización ggplot
SGp$plot <- SGp$plot + 
  scale_x_continuous(
    breaks = seq(0, 30, 5),
    limits = c(0, 30),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    limits = c(0, 100),
    expand = c(0, 0)
  ) +
  geom_vline(
    xintercept = 24,
    linetype = "dashed",
    color = "grey30",
    linewidth = 0.8
  ) +
  theme(
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(10, 15, 10, 10)
  )


print(SGp)


##Guardar imagen 
SGp$plot
ggsave("SGp.tiff", SGp$plot, dpi = 300)


##SG Estratificado 

TESIS_3N$Estatus <- factor(TESIS_3N$Estatus, levels = c(0,1),
                           labels = c("Negativo", "Positivo"))

SG_E <- Surv(TESIS_3N$Tiempo_SG, TESIS_3N$Vivo_muerto) 
T_SG_E <- survfit(SG ~ Estatus, data = TESIS_3N, type = "kaplan-meier") 
summary(T_SG_E)


#Gráfica SG Estratificado
SG2 <- ggsurvplot(fit = T_SG_E, data = TESIS_3N, fun = "pct", conf.int = F, palette = c("#FC2C03", "#1F4E79"),  size = 1.2, #conf.int.fill = c("#BFC9CA"), 
                  title = "SG", 
                    xlab = "Meses", ylab = "Supervivencia (%)", legend.title = "Estimación", 
                  ggtheme = theme_classic(base_size = 14, base_family = "Helvetica"),
                    legend.labs = c("irAE presente", "irAE ausente")) 


SG2$plot

#personalización ggplot
SG2$plot <- SG2$plot + 
  scale_x_continuous(
    breaks = seq(0, 30, 5),
    limits = c(0, 30),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    limits = c(0, 100),
    expand = c(0, 0)
  ) +
  geom_vline(
    xintercept = 24,
    linetype = "dashed",
    color = "grey30",
    linewidth = 0.8
  ) +
  theme(
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(10, 15, 10, 10)
  )


print(SG2)



##Guardar imagen 
SG2$plot
ggsave("SG2.tiff", SG2$plot, dpi = 300)


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
SLE_pcr <- ggsurvplot(fit = T_SLE_p, data = TESIS_C, fun = "pct", conf.int = F, palette = c("#FC2C03", "#1F4E79"), size = 1.2,  #conf.int.fill = c("#BFC9CA"), title = "SLE", 
                       xlab = "Meses", ylab = "Supervivencia (%)", legend.title = "Estimación", 
                       ggtheme = theme_classic(base_size = 14, base_family = "Helvetica"),
                       legend.labs = c("pCR ausente", "pCR presente")) 



SLE_pcr$plot

#personalización ggplot
SLE_pcr$plot <- SLE_pcr$plot + 
  scale_x_continuous(
    breaks = seq(0, 30, 5),
    limits = c(0, 30),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    limits = c(0, 100),
    expand = c(0, 0)
  ) +
  geom_vline(
    xintercept = 24,
    linetype = "dashed",
    color = "grey30",
    linewidth = 0.8
  ) +
  theme(
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(10, 15, 10, 10)
  )


print(SLE_pcr)



##Guardar imagen 
SLE_pcr$plot
ggsave("SLE.tiff", SLE_pcr$plot, dpi = 300)


##SG Estratificado por pCR 

TESIS_C$pCR <- factor(TESIS_C$pCR, levels = c(0,1),
                      labels = c("Negativo", "Positivo"))

SG_P <- Surv(TESIS_C$Tiempo_SG, TESIS_C$Vivo_muerto) 
T_SG_P <- survfit(SG_P ~ pCR, data = TESIS_C, type = "kaplan-meier") 
summary(T_SG_P)


#Gráfica SG pcr 
SG_pcr <- ggsurvplot(fit = T_SG_P, data = TESIS_C, fun = "pct", conf.int = F, palette = c("#FC2C03", "#1F4E79"), size = 1.2,  #conf.int.fill = c("#BFC9CA"), title = "SG", 
                    xlab = "Meses", ylab = "Supervivencia (%)", legend.title = "Estimación", 
                    ggtheme = theme_classic(base_size = 14, base_family = "Helvetica"),
                    legend.labs = c("pCR ausente", "pCR presente")) 


SG_pcr$plot

#personalización ggplot
SG_pcr$plot <- SG_pcr$plot + 
  scale_x_continuous(
    breaks = seq(0, 30, 5),
    limits = c(0, 30),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    limits = c(0, 100),
    expand = c(0, 0)
  ) +
  geom_vline(
    xintercept = 24,
    linetype = "dashed",
    color = "grey30",
    linewidth = 0.8
  ) +
  theme(
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(10, 15, 10, 10)
  )


print(SG_pcr)


##Guardar imagen 
SG_pcr$plot
ggsave("SG_pcr.tiff", SG_pcr$plot, dpi = 300)


##SLE BCR 

SLE_b <- Surv(TESIS_C$Tiempo_SLE, TESIS_C$Super_libre_evento) 
T_SLE_b <- survfit(SLE_b ~ 1, data = TESIS_C, type = "kaplan-meier") 
summary(T_SLE_b)

TESIS_C$RCB_cat <- factor(TESIS_C$RCB_cat, levels = c(0,1),
                          labels = c("2-3", "0-1"))

SLE_b <- Surv(TESIS_C$Tiempo_SLE, TESIS_C$Super_libre_evento) 
T_SLE_b <- survfit(SLE_b ~ RCB_cat, data = TESIS_C, type = "kaplan-meier") 
summary(T_SLE_b)


#Gráfica SLE Estratificado por rcb 
SLE_rcb <- ggsurvplot(fit = T_SLE_b, data = TESIS_C, fun = "pct", conf.int = F, palette = c("#FC2C03", "#1F4E79"), size = 1.2, #conf.int.fill = c("#BFC9CA"), title = "SLE", 
                       xlab = "Meses", ylab = "Supervivencia (%)", legend.title = "Estimación", 
                       ggtheme = theme_classic(base_size = 14, base_family = "Helvetica"),
                       legend.labs = c("RCB 2-3", "RCB 0-1")) 


SLE_rcb$plot

#personalización ggplot
SLE_rcb$plot <- SLE_rcb$plot + 
  scale_x_continuous(
    breaks = seq(0, 30, 5),
    limits = c(0, 30),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    limits = c(0, 100),
    expand = c(0, 0)
  ) +
  geom_vline(
    xintercept = 24,
    linetype = "dashed",
    color = "grey30",
    linewidth = 0.8
  ) +
  theme(
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(10, 15, 10, 10)
  )


print(SLE_rcb)


##Guardar imagen 
SLE_rcb$plot
ggsave("SLE_rcb.tiff", SLE_pcr$plot, dpi = 300)



##SG Estratificado por RCB
TESIS_C$RCB_cat <- factor(TESIS_C$RCB_cat, levels = c(0,1),
                          labels = c("2-3", "0-1"))

SG_b <- Surv(TESIS_C$Tiempo_SG, TESIS_C$Vivo_muerto) 
T_SG_b <- survfit(SG_b ~ RCB_cat, data = TESIS_C, type = "kaplan-meier") 
summary(T_SG_b)


#Gráfica SG Estratificado por rcb
SG_rcb <- ggsurvplot(fit = T_SG_b, data = TESIS_C, fun = "pct", conf.int = F, palette = c("#FC2C03", "#1F4E79"), size = 1.2,#conf.int.fill = c("#BFC9CA"), title = "SG", 
                    xlab = "Meses", ylab = "Supervivencia (%)", legend.title = "Estimación", 
                    ggtheme = theme_classic(base_size = 14, base_family = "Helvetica"),
                    legend.labs = c("RCB 2-3", "RCB 0-1")) 



SG_rcb$plot

#personalización ggplot
SG_rcb$plot <- SG_rcb$plot + 
  scale_x_continuous(
    breaks = seq(0, 30, 5),
    limits = c(0, 30),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    limits = c(0, 100),
    expand = c(0, 0)
  ) +
  geom_vline(
    xintercept = 24,
    linetype = "dashed",
    color = "grey30",
    linewidth = 0.8
  ) +
  theme(
    axis.line = element_line(linewidth = 0.8),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(10, 15, 10, 10)
  )


print(SG_rcb)



##Guardar imagen 
SG_rcb$plot
ggsave("SG_rcb.tiff", SG_rcb$plot, dpi = 300)

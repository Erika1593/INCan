
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(skimr)
library(gridExtra)
library(corrplot)
library(car)
library(scatterplot3d)
library(dplyr)
library(scales)
library(plotrix)
library(survival)
library(survminer)
library(ggeffects)
library(litedown)

##Lectura de base de datos
RESIDUAL <- read.csv("~/Rstudio_jobs/Dr.Sergio/Residual_Sergio.csv", stringsAsFactors = FALSE) ##Mantener el orden de las filas


## Recategorizar por grupo  
DATA <- RESIDUAL %>%
  mutate(
    ALND_categoria = if_else(NUM_GL_RES < 10, "L-ALND", "S-ALND")
  )

## Recategorizar por status de vivo-muerto 
## 1 - vivo 
## 2 - muerto 

DATA <- DATA %>%
  mutate(
    Status_categoria = if_else(SG < 3, 1, 2)
  )


## Recategorizar recurrencia 
DATA <- DATA %>%
  mutate(
    FECHA_REC_AR = if_else(
      AR == 1,
      FECHA_REC,
      as.Date(NA)
    )
  )

write.csv(DATA, "residual_recategorizado.csv", row.names = FALSE)


## CORRECCIÓN DE DATOS PROBLEMA 


## FECHA MAL ESCRITA
DATA[129, c("FECHA_CX", "FECHA_REC", "FECHA_MUER")]

# Corregir como texto
DATA$FECHA_CX[129] <- "27/07/2015"


## FECHA ILÓGICA
# Eliminar paciente 187
DATA <- DATA[-187, ]


##Modificación de formato a fecha para lectura
DATA$FECHA_CX <- as.Date(DATA$FECHA_CX, format="%d/%m/%Y")
DATA$FECHA_REC <- as.Date(DATA$FECHA_REC, format="%d/%m/%Y")
DATA$FECHA_MUER <- as.Date(DATA$FECHA_MUER, format="%d/%m/%Y")

## COMPROBAR QUE LAS FECHAS ESTÉN CORRECTAS 
class(DATA$FECHA_CX)
class(DATA$FECHA_REC)
class(DATA$FECHA_MUER)
class(DATA$FECHA_REC_AR)
## SUPERVIVENCIA LIBRE DE ENFERMEDAD (SLE)


# Fecha del evento:
# - Si existe recaída -> fecha de recaída
# - Si no existe recaída -> fecha de muerte (ÚLTIMO SEGUIMIENTO) ---- ESTO ES LO QUE HAY QUE CORREGIR ****

DATA$Fecha_evento_SLE_AR <- DATA$FECHA_REC_AR

DATA$Fecha_evento_SLE_AR[!is.na(DATA$FECHA_REC_AR)] <-
  DATA$FECHA_REC_AR[!is.na(DATA$FECHA_REC_AR)]


# TIEMPO DE SEGUIMIENTO EN MESES (SLE)

DATA$Tiempo_SLE_AR <- as.numeric(
  DATA$Fecha_evento_SLE_AR - DATA$FECHA_CX
) / 30.44


# 1 = recaída
# 0 = no tuvo recaída y fue censurado en la fecha de muerte

DATA$Super_libre_evento_AR <- ifelse(
  !is.na(DATA$FECHA_REC_AR),
  1,
  0
)


##______________________________
## AJUSTAR DESPUÉS DE HABLAR CON DR. SERGIO 
##______________________________


# Comprobar SLE 
DATA %>%
  select(FECHA_CX, FECHA_REC, FECHA_MUER, Fecha_evento_SLE, Tiempo_SLE, FECHA_REC_AR) %>%
  head(10)


summary(DATA$Tiempo_SLE_AR)
max(DATA$Tiempo_SLE_AR, na.rm = TRUE)
sum(DATA$Tiempo_SLE_AR < 0, na.rm = TRUE)

## CREAR OBJETO DE SUPERVIVENCIA PARA SLE 


SLE_AR <- Surv(
  time = DATA$Tiempo_SLE_AR,
  event = DATA$Super_libre_evento_AR
)

# KAPLAN-MEIER PARA SLE POR ALND


T_SLE_AR <- survfit(
  SLE_AR ~ ALND_categoria,
  data = DATA
)

summary(T_SLE_AR)

#COMPARACIÓN DE LOG RANK PARA SLE 
logrank <- survdiff(
  SLE_AR ~ ALND_categoria,
  data = DATA
)

logrank


##p-value del log rank 
p_logrank <- 1 - pchisq(
  logrank$chisq,
  df = length(logrank$n) - 1
)

p_logrank

## PREPARAR DATOS PARA GRÁFICA SLE 
km <- summary(T_SLE_AR)

km_data <- data.frame(
  tiempo = km$time,
  supervivencia = km$surv,
  estrato = km$strata
)

## GRÁFICA KAPLAN SLE 

ggplot(
  km_data,
  aes(
    x = tiempo,
    y = supervivencia * 100,
    color = estrato
  )
) +
  geom_step(linewidth = 1.2) +
  
  scale_color_manual(
    values = c(
      "ALND_categoria=L-ALND" = "#FF69B4",
      "ALND_categoria=S-ALND" = "#8E44AD"
    ),
    labels = c("L-ALND", "S-ALND")
  ) +
  
  scale_x_continuous(
    breaks = seq(0, 120, 20)
  ) +
  
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20)
  ) +
  
  coord_cartesian(
    xlim = c(0, 120)
  ) +
  
  labs(
    title = "Recurrencia axilar",
    x = "Tiempo (meses)",
    y = "Recurrencia axilar (%)",
    color = ""
  ) +
  
  theme_classic(base_size = 14)



##__________________________________
##__________________________________



## SUPERVIVENCIA GLOBAL 
# TIEMPO - DESDE FECHA_CX HASTA FECHA_MUER

# EVENTO: 1 = MURIÓ, 0 = VIVO AL ÚLTIMO SEGUIMIENTO 


# TIEMPO DE SEGUIMIENTO EN MESES (SG) 
DATA$Tiempo_OS <- as.numeric(
  DATA$FECHA_MUER - DATA$FECHA_CX
) / 30.44


## ESTABLECER AL MOMENTO DE SABER QUIENES SÍ MURIERON 
DATA$Evento_OS <- ifelse(DATA$MUERTE == 1, 1, 0)



#BUSCA EN LA BASE DE DATOS COLUMNAS RELACIONADAS CON STATUS VIVO O MUERTO 
grep(
  "MUER|FALLE|DEFUN|VIVO|ESTADO|STATUS",
  names(DATA),
  value = TRUE,
  ignore.case = TRUE
)




###__________________
## GRAFICA ESTILIZADA

fit_SLE <- survfit(
  Surv(Tiempo_SLE, Super_libre_evento) ~ ALND_categoria,
  data = DATA
)

km_data <- data.frame(
  time = fit_SLE$time,
  surv = fit_SLE$surv,
  lower = fit_SLE$lower,
  upper = fit_SLE$upper,
  strata = fit_SLE$strata
)

# Extraer correctamente el nombre de cada grupo
km_data$grupo <- rep(
  names(fit_SLE$strata),
  fit_SLE$strata
)

# Quitar el prefijo "ALND_categoria="
km_data$grupo <- sub(
  "ALND_categoria=",
  "",
  km_data$grupo
)

# ============================================================
# LOG-RANK
# ============================================================

logrank <- survdiff(
  Surv(Tiempo_SLE, Super_libre_evento) ~ ALND_categoria,
  data = DATA
)

chi2 <- logrank$chisq
gl <- length(logrank$n) - 1

p_logrank <- 1 - pchisq(
  chi2,
  df = gl
)

texto_logrank <- paste0(
  "Log-rank: χ² = ", round(chi2, 2),
  "\np = ",
  format.pval(
    p_logrank,
    digits = 3,
    eps = 0.001
  )
)


# ============================================================
# GRÁFICA KAPLAN-MEIER
# ============================================================

ggplot(
  km_data,
  aes(
    x = time,
    y = surv * 100,
    color = grupo
  )
) +
  
  # Intervalos de confianza
  geom_ribbon(
    aes(
      ymin = lower * 100,
      ymax = upper * 100,
      fill = grupo
    ),
    alpha = 0.12,
    color = NA
  ) +
  
  # Curvas
  geom_step(
    linewidth = 1.3
  ) +
  
  # Colores
  scale_color_manual(
    values = c(
      "L-ALND" = "#FF69B4",
      "S-ALND" = "#8E44AD"
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "L-ALND" = "#FF69B4",
      "S-ALND" = "#8E44AD"
    )
  ) +
  
  # Eje X
  scale_x_continuous(
    breaks = seq(0, 120, 20),
    expand = c(0, 0)
  ) +
  
  # Eje Y
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    limits = c(0, 100),
    labels = function(x) paste0(x, "%")
  ) +
  
  # Mostrar hasta 120 meses
  coord_cartesian(
    xlim = c(0, 120),
    ylim = c(0, 100)
  ) +
  
  labs(
    title = "Supervivencia libre de enfermedad",
    x = "Tiempo (meses)",
    y = "Supervivencia libre de enfermedad",
    color = "",
    fill = ""
  ) +
  
  # Log-rank
  annotate(
    "label",
    x = 72,
    y = 88,
    label = texto_logrank,
    hjust = 0,
    size = 4.2,
    fontface = "bold"
  ) +
  
  theme_classic(
    base_size = 14
  ) +
  
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 17
    ),
    
    axis.title = element_text(
      face = "bold"
    ),
    
    axis.text = element_text(
      color = "black"
    ),
    
    legend.position = c(0.82, 0.72),
    
    legend.background = element_rect(
      fill = "white",
      color = "grey70"
    ),
    
    legend.key.width = unit(
      1.5,
      "cm"
    )
  )

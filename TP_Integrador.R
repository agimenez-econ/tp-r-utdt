#==============================================================================#
# TP INTEGRADOR - MÓDULO R
# Movilidad entre formalidad e informalidad laboral en Argentina
# EPH, 4T2023 - 1T2026
#==============================================================================#

#==============================================================================#
# 0. PAQUETES
#==============================================================================#

library(tidyverse)
library(eph)
library(broom)
library(pROC)

#==============================================================================#
# 1. CARGA Y UNIÓN DE LOS MICRODATOS EPH
#==============================================================================#

#------------------------------------------------------------------------------#
# 1.1. Carga de los microdatos
#------------------------------------------------------------------------------#

# Se descargan los microdatos individuales de la EPH desde 1T2023 hasta 1T2026.
# Se conserva esta ventana temporal para disponer de períodos consecutivos que
# permitan construir posteriormente las transiciones laborales.

eph_lista <- list()

for (año in 2023:2026) {
  
  trimestres <- if (año == 2026) 1 else 1:4
  
  for (trimestre in trimestres) {
    
    nombre <- paste0("eph_", año, "_", trimestre)
    
    eph_lista[[nombre]] <- get_microdata(
      year = año,
      trimester = trimestre,
      type = "individual"
    )
  }
}

# Verificación de las bases cargadas

length(eph_lista)
names(eph_lista)


#------------------------------------------------------------------------------#
# 1.2. Unión de las bases trimestrales
#------------------------------------------------------------------------------#

# En 4T2024, la variable CH05 (fecha de nacimiento) presenta una clase diferente 
# al resto de los trimestres (integer en lugar de character). 
# Se homogeneiza su clase antes de realizar la unión de las bases.

eph_lista$eph_2024_4$CH05 <- as.character(eph_lista$eph_2024_4$CH05)

# Unión de las bases trimestrales

eph_2023_2026 <- bind_rows(eph_lista)

#==============================================================================#
# 2. ANÁLISIS EXPLORATORIO DE DATOS (EDA)
#==============================================================================#

#------------------------------------------------------------------------------#
# 2.1. ESTRUCTURA GENERAL
#------------------------------------------------------------------------------#

# Dimensión del dataset

dim(eph_2023_2026)


# Tipo de cada variable

tabla_tipos <- tibble(
  variable = names(eph_2023_2026),
  tipo = sapply(eph_2023_2026, function(x) class(x)[1])
)

tabla_tipos

tabla_tipos |>
  count(tipo)


# Cobertura temporal

tabla_cobertura_temporal <- eph_2023_2026 |>
  count(ANO4, TRIMESTRE)

tabla_cobertura_temporal


# Cobertura geográfica

tabla_region <- eph_2023_2026 |>
  count(REGION, sort = TRUE)

tabla_region


tabla_aglomerado <- eph_2023_2026 |>
  count(AGLOMERADO, sort = TRUE)

tabla_aglomerado


# Distribución de las observaciones por región

grafico_region <- eph_2023_2026 |>
  count(REGION) |>
  ggplot(aes(x = factor(REGION), y = n)) +
  geom_col(fill =  "#1F77B4" ) +
  labs(
    title = "Distribución de las observaciones por región",
    x = "Región",
    y = "Número de observaciones"
  ) +
  theme_minimal()

grafico_region


#------------------------------------------------------------------------------#
# 2.2. VALORES FALTANTES (NA)
#------------------------------------------------------------------------------#

# Variables relevantes para el análisis

variables_relevantes <- c(
  
  # Identificación y período
  "CODUSU", "NRO_HOGAR", "COMPONENTE", "ANO4", "TRIMESTRE",
  
  # Situación laboral y formalidad
  "ESTADO", "EMPLEO",
  
  # Características individuales
  "CH04", "CH06", "NIVEL_ED",
  
  # Características laborales
  "CAT_OCUP", "PP3E_TOT", "INTENSI", "PP04A", "PP04C",
  
  # Ingresos
  "P21",
  
  # Geografía y ponderación
  "REGION", "AGLOMERADO"
)


# Tabla de cantidad y porcentaje de valores faltantes

tabla_na <- tibble(
  variable = variables_relevantes,
  n_na = sapply(
    eph_2023_2026[variables_relevantes],
    function(x) sum(is.na(x))
  ),
  porcentaje_na = sapply(
    eph_2023_2026[variables_relevantes],
    function(x) mean(is.na(x)) * 100
  )
) |>
  arrange(desc(porcentaje_na))

tabla_na


# Gráfico de valores faltantes

grafico_na <- tabla_na |>
  arrange(porcentaje_na) |>
  mutate(
    variable = factor(variable, levels = variable)
  ) |>
  ggplot(aes(x = variable, y = porcentaje_na)) +
  geom_col(fill =  "#1F77B4") +
  coord_flip() +
  labs(
    title = "Valores faltantes en las variables seleccionadas",
    x = NULL,
    y = "Valores faltantes (%)"
  ) +
  theme_minimal()

grafico_na

# VALORES FALTANTES SEGÚN CONDICIÓN DE ACTIVIDAD

# Las variables laborales presentan valores faltantes estructurales
# entre personas desocupadas e inactivas, dado que estas variables
# corresponden a características del empleo.

caracteristicas_laborales <- c(
  "PP3E_TOT",
  "PP3F_TOT",
  "INTENSI",
  "PP04A",
  "PP04C",
  "CAT_OCUP"
)


tabla_na_estado <- eph_2023_2026 |>
  filter(ESTADO %in% c(1, 2, 3)) |>
  group_by(ESTADO) |>
  summarise(
    across(
      all_of(caracteristicas_laborales),
      ~ mean(is.na(.x)) * 100
    ),
    .groups = "drop"
  ) |>
  mutate(
    estado = factor(
      ESTADO,
      levels = c(1, 2, 3),
      labels = c(
        "Ocupado",
        "Desocupado",
        "Inactivo"
      )
    )
  )

tabla_na_estado


tabla_na_estado_largo <- tabla_na_estado |>
  select(-ESTADO) |>
  pivot_longer(
    cols = all_of(caracteristicas_laborales),
    names_to = "variable",
    values_to = "porcentaje_na"
  )


grafico_na_estado <- tabla_na_estado_largo |>
  ggplot(
    aes(
      x = estado,
      y = porcentaje_na
    )
  ) +
  geom_col(fill =  "#1F77B4") +
  facet_wrap(~ variable) +
  labs(
    title = "Valores faltantes en variables laborales según situación laboral",
    x = "Situación laboral",
    y = "Valores faltantes (%)"
  ) +
  theme_minimal()

grafico_na_estado


# VALORES FALTANTES DE EMPLEO

# EMPLEO es la variable utilizada para identificar formalidad e informalidad.

tabla_na_empleo <- eph_2023_2026 |>
  filter(ESTADO %in% c(1, 2, 3)) |>
  group_by(ESTADO) |>
  summarise(
    n = n(),
    n_na = sum(is.na(EMPLEO)),
    porcentaje_na = mean(is.na(EMPLEO)) * 100,
    .groups = "drop"
  ) |>
  mutate(
    estado = factor(
      ESTADO,
      levels = c(1, 2, 3),
      labels = c(
        "Ocupado",
        "Desocupado",
        "Inactivo"
      )
    )
  )

tabla_na_empleo


grafico_na_empleo <- tabla_na_empleo |>
  ggplot(aes(x = estado, y = porcentaje_na)) +
  geom_col(fill =  "#1F77B4") +
  labs(
    title = "Valores faltantes de la variable de formalidad según situación laboral",
    x = "Situación laboral",
    y = "Valores faltantes (%)"
  ) +
  theme_minimal()

grafico_na_empleo


# DISPONIBILIDAD DE EMPLEO POR PERÍODO

# Se analiza la disponibilidad de EMPLEO entre las personas ocupadas.
# Esto permite identificar desde qué período puede medirse formalidad.

tabla_disponibilidad_empleo <- eph_2023_2026 |>
  filter(ESTADO == 1) |>
  group_by(ANO4, TRIMESTRE) |>
  summarise(
    n_ocupados = n(),
    n_validos = sum(EMPLEO %in% c(1, 2), na.rm = TRUE),
    n_ns_nr = sum(EMPLEO == 9, na.rm = TRUE),
    n_na = sum(is.na(EMPLEO)),
    porcentaje_disponible =
      sum(EMPLEO %in% c(1, 2), na.rm = TRUE) / n() * 100,
    .groups = "drop"
  ) |>
  mutate(
    periodo = paste0(ANO4, "T", TRIMESTRE)
  )

tabla_disponibilidad_empleo


# Gráfico de disponibilidad de EMPLEO

grafico_disponibilidad_empleo <- tabla_disponibilidad_empleo |>
  ggplot(
    aes(
      x = periodo,
      y = porcentaje_disponible,
      group = 1
    )
  ) +
  geom_line(
    color = "#0B3C5D",
    linewidth = 1
  ) +
  geom_point(
    color =  "#1F77B4",
    size = 2
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Disponibilidad de la variable de formalidad por período",
    x = "Período",
    y = "Personas ocupadas con formalidad identificable"
  ) +
  theme_minimal()

grafico_disponibilidad_empleo


# Definición de la ventana temporal para el análisis

# El análisis de los valores faltantes muestra que la variable EMPLEO,
# utilizada para identificar la formalidad laboral, comienza a estar
# disponible a partir de 4T2023.

# Por este motivo, se define una base de análisis que comienza en 4T2023
# y se extiende hasta 1T2026. La base original se conserva sin modificaciones.

base_analisis <- eph_2023_2026 |>
  mutate(
    periodo_id = (ANO4 - 2023) * 4 + TRIMESTRE,
    periodo = paste0(ANO4, "T", TRIMESTRE)
  ) |>
  filter(
    ANO4 > 2023 |
      (ANO4 == 2023 & TRIMESTRE >= 4)
  )

#------------------------------------------------------------------------------#
# 2.3. DISTRIBUCIONES UNIVARIADAS
#------------------------------------------------------------------------------#

# FORMALIDAD LABORAL

# EMPLEO = 1: Formal
# EMPLEO = 2: Informal
# EMPLEO = 9: Ns/Nr

# Se utilizan las categorías 1 y 2 de la variable EMPLEO

tabla_formalidad <- base_analisis |>
  filter(
    ESTADO == 1,
    EMPLEO %in% c(1, 2)
  ) |>
  mutate(
    formalidad = factor(
      EMPLEO,
      levels = c(1, 2),
      labels = c(
        "Formal",
        "Informal"
      )
    )
  ) |>
  count(formalidad) |>
  mutate(
    porcentaje = n / sum(n) * 100
  )

tabla_formalidad


grafico_formalidad <- tabla_formalidad |>
  ggplot(
    aes(
      x = formalidad,
      y = porcentaje,
      fill = formalidad
    )
  ) +
  geom_col() +
  scale_fill_manual(
    values = c(
      "Formal" = "#0B3C5D",
      "Informal" = "#6BAED6"
    )
  ) +
  geom_text(
    aes(
      label = paste0(round(porcentaje, 1), "%")
    ),
    vjust = -0.3
  ) +
  labs(
    title = "Distribución de las personas ocupadas según formalidad laboral",
    x = "Formalidad laboral",
    y = "Porcentaje",
    fill = "Formalidad"
  ) +
  theme_minimal()

grafico_formalidad


# Formalidad por período

tabla_formalidad_periodo <- base_analisis |>
  filter(
    ESTADO == 1,
    EMPLEO %in% c(1, 2)
  ) |>
  mutate(
    formalidad = factor(
      EMPLEO,
      levels = c(1, 2),
      labels = c(
        "Formal",
        "Informal"
      )
    ),
    periodo = paste0(ANO4, "T", TRIMESTRE)
  ) |>
  count(
    periodo,
    formalidad
  ) |>
  group_by(periodo) |>
  mutate(
    porcentaje = n / sum(n) * 100
  ) |>
  ungroup()


grafico_formalidad_periodo <- tabla_formalidad_periodo |>
  ggplot(
    aes(
      x = periodo,
      y = porcentaje,
      fill = formalidad
    )
  ) +
  geom_col() +
  scale_fill_manual(
    values = c(
      "Formal" = "#0B3C5D",
      "Informal" = "#6BAED6"
    )
  ) +
  labs(
    title = "Formalidad laboral por período",
    x = "Período",
    y = "Porcentaje",
    fill = "Formalidad"
  ) +
  theme_minimal()

grafico_formalidad_periodo

# EDAD

# CH06 = 99 corresponde a Ns/Nr. Se excluye de los análisis que utilizan edad.

grafico_edad <- base_analisis |>
  filter(
    ESTADO == 1,
    CH06 != 99
  ) |>
  ggplot(aes(x = CH06)) +
  geom_histogram(
    bins = 30,
    fill =  "#1F77B4"
  ) +
  scale_x_continuous(
    breaks = seq(15, 90, by = 5)
  ) +
  labs(
    title = "Distribución de la edad de las personas ocupadas",
    x = "Edad",
    y = "Número de personas"
  ) +
  theme_minimal()

grafico_edad


# Distribución de la edad por período

grafico_edad_periodo <- base_analisis |>
  filter(
    ESTADO == 1,
    CH06 != 99
  ) |>
  mutate(
    periodo = paste0(ANO4, "T", TRIMESTRE)
  ) |>
  ggplot(
    aes(
      x = periodo,
      y = CH06
    )
  ) +
  geom_boxplot(
    fill = "#6BAED6",
    color = "#0B3C5D"
  ) +
  labs(
    title = "Distribución de la edad de las personas ocupadas por período",
    x = "Período",
    y = "Edad"
  ) +
  theme_minimal()

grafico_edad_periodo


# SEXO

tabla_sexo <- base_analisis |>
  filter(
    ESTADO == 1,
    CH04 %in% c(1, 2)
  ) |>
  mutate(
    sexo = factor(
      CH04,
      levels = c(1, 2),
      labels = c(
        "Varón",
        "Mujer"
      )
    )
  ) |>
  count(sexo) |>
  mutate(
    porcentaje = n / sum(n) * 100
  )

tabla_sexo


grafico_sexo <- tabla_sexo |>
  ggplot(
    aes(
      x = sexo,
      y = porcentaje
    )
  ) +
  geom_col(fill =  "#1F77B4") +
  geom_text(
    aes(
      label = paste0(round(porcentaje, 1), "%")
    ),
    vjust = -0.3
  ) +
  labs(
    title = "Distribución de las personas ocupadas por sexo",
    x = "Sexo",
    y = "Porcentaje"
  ) +
  theme_minimal()

grafico_sexo


# NIVEL EDUCATIVO

tabla_educacion <- base_analisis |>
  filter(
    ESTADO == 1,
    NIVEL_ED %in% 1:7
  ) |>
  mutate(
    nivel_educativo = factor(
      NIVEL_ED,
      levels = c(7, 1, 2, 3, 4, 5, 6),
      labels = c(
        "Sin instrucción",
        "Primario incompleto",
        "Primario completo",
        "Secundario incompleto",
        "Secundario completo",
        "Superior/universitario incompleto",
        "Superior/universitario completo"
      )
    )
  ) |>
  count(nivel_educativo) |>
  mutate(
    porcentaje = n / sum(n) * 100
  )

tabla_educacion


grafico_educacion <- tabla_educacion |>
  ggplot(
    aes(
      x = nivel_educativo,
      y = porcentaje
    )
  ) +
  geom_col(fill =  "#1F77B4") +
  geom_text(
    aes(
      label = paste0(round(porcentaje, 1), "%")
    ),
    vjust = -0.3,
    size = 3
  ) +
  labs(
    title = "Distribución de las personas ocupadas según nivel educativo",
    x = "Nivel educativo",
    y = "Porcentaje"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    )
  )

grafico_educacion


# CATEGORÍA OCUPACIONAL

tabla_categoria_ocupacional <- base_analisis |>
  filter(
    ESTADO == 1,
    CAT_OCUP %in% 1:4
  ) |>
  mutate(
    categoria_ocupacional = factor(
      CAT_OCUP,
      levels = c(1, 2, 3, 4),
      labels = c(
        "Patrón",
        "Cuenta propia",
        "Obrero o empleado",
        "Trabajador familiar sin remuneración"
      )
    )
  ) |>
  count(categoria_ocupacional) |>
  mutate(
    porcentaje = n / sum(n) * 100
  )

tabla_categoria_ocupacional


grafico_categoria_ocupacional <- tabla_categoria_ocupacional |>
  ggplot(
    aes(
      x = categoria_ocupacional,
      y = porcentaje
    )
  ) +
  geom_col(fill =  "#1F77B4") +
  geom_text(
    aes(
      label = paste0(round(porcentaje, 1), "%")
    ),
    vjust = -0.3,
    size = 3
  ) +
  labs(
    title = "Distribución de las personas ocupadas según categoría ocupacional",
    x = "Categoría ocupacional",
    y = "Porcentaje"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 20,
      hjust = 1
    )
  )

grafico_categoria_ocupacional


# HORAS TRABAJADAS

# Según la documentación de la EPH, el rango válido de PP3E_TOT
# es de 1 a 168 horas semanales.

tabla_horas <- base_analisis |>
  filter(
    ESTADO == 1
  ) |>
  summarise(
    minimo = min(PP3E_TOT, na.rm = TRUE),
    maximo = max(PP3E_TOT, na.rm = TRUE),
    media = mean(PP3E_TOT, na.rm = TRUE),
    mediana = median(PP3E_TOT, na.rm = TRUE),
    n_menor_1 = sum(PP3E_TOT < 1, na.rm = TRUE),
    n_mayor_168 = sum(PP3E_TOT > 168, na.rm = TRUE)
  )

tabla_horas


# Identificación de valores fuera del rango válido

base_analisis |>
  filter(
    ESTADO == 1,
    PP3E_TOT < 1
  ) |>
  count(PP3E_TOT)


base_analisis |>
  filter(
    ESTADO == 1,
    PP3E_TOT > 168
  ) |>
  count(
    PP3E_TOT,
    sort = TRUE
  )


# Proporción de observaciones por encima de 120 horas

tabla_horas_120 <- base_analisis |>
  filter(
    ESTADO == 1,
    PP3E_TOT >= 1,
    PP3E_TOT <= 168
  ) |>
  summarise(
    mayores_120hs = sum(PP3E_TOT > 120),
    porcentaje_mayores_120 =
      mayores_120hs / n() * 100
  )

tabla_horas_120


# Boxplot de horas trabajadas

grafico_horas_boxplot <- base_analisis |>
  filter(
    ESTADO == 1,
    PP3E_TOT >= 1,
    PP3E_TOT <= 168
  ) |>
  ggplot(
    aes(
      x = "",
      y = PP3E_TOT
    )
  ) +
  geom_boxplot(
    fill = "#6BAED6",
    color = "#0B3C5D"
  ) +
  labs(
    title = "Distribución de las horas trabajadas en la ocupación principal",
    x = NULL,
    y = "Horas trabajadas por semana"
  ) +
  theme_minimal()

grafico_horas_boxplot


# Histograma de horas trabajadas

grafico_horas <- base_analisis |>
  filter(
    ESTADO == 1,
    PP3E_TOT >= 1,
    PP3E_TOT <= 168
  ) |>
  ggplot(aes(x = PP3E_TOT)) +
  geom_histogram(
    binwidth = 10,
    boundary = 0,
    fill =  "#1F77B4"
  ) +
  scale_x_continuous(
    breaks = seq(0, 168, by = 10)
  ) +
  coord_cartesian(
    xlim = c(0, 120)
  ) +
  labs(
    title = "Distribución de las horas trabajadas en la ocupación principal",
    x = "Horas trabajadas por semana",
    y = "Número de personas",
    caption = paste(
      "La visualización se limita a 120 horas para facilitar la lectura.",
      "Las observaciones restantes dentro del rango válido se conservan."
    )
  ) +
  theme_minimal()

grafico_horas


# INGRESO LABORAL

# P21 corresponde al ingreso de la ocupación principal.
# Se inspeccionan valores negativos, ceros y la cola superior.

tabla_ingreso <- base_analisis |>
  filter(
    ESTADO == 1
  ) |>
  summarise(
    minimo = min(P21, na.rm = TRUE),
    maximo = max(P21, na.rm = TRUE),
    mediana = median(P21, na.rm = TRUE),
    media = mean(P21, na.rm = TRUE),
    p90 = quantile(P21, 0.90, na.rm = TRUE),
    p95 = quantile(P21, 0.95, na.rm = TRUE),
    p99 = quantile(P21, 0.99, na.rm = TRUE)
  )

tabla_ingreso


# Identificación de valores negativos
# Los valores negativos corresponden a códigos especiales de la variable.

tabla_ingreso_negativo <- base_analisis |>
  filter(
    ESTADO == 1,
    P21 < 0
  ) |>
  count(P21)

tabla_ingreso_negativo


# Identificación de valores iguales a cero

tabla_ingreso_cero <- base_analisis |>
  filter(
    ESTADO == 1,
    P21 == 0
  ) |>
  count(P21)

tabla_ingreso_cero


# Análisis de la cola superior

tabla_cola_ingreso <- base_analisis |>
  filter(
    ESTADO == 1,
    P21 > 0
  ) |>
  summarise(
    p90 = quantile(P21, 0.90),
    p95 = quantile(P21, 0.95),
    p99 = quantile(P21, 0.99),
    p99_5 = quantile(P21, 0.995),
    maximo = max(P21)
  )

tabla_cola_ingreso


# Inspección de los casos por encima del percentil 99.5

p995_ingreso <- tabla_cola_ingreso$p99_5


casos_ingreso_extremo <- base_analisis |>
  filter(
    ESTADO == 1,
    P21 > p995_ingreso
  ) |>
  select(
    ANO4,
    TRIMESTRE,
    P21,
    CH04,
    CH06,
    NIVEL_ED,
    CAT_OCUP,
    PP3E_TOT,
    INTENSI,
    PP04A,
    PP04C,
    REGION,
    AGLOMERADO
  ) |>
  arrange(desc(P21))

casos_ingreso_extremo


# Histograma del ingreso laboral en escala lineal

p99_ingreso <- base_analisis |>
  filter(
    ESTADO == 1,
    P21 > 0
  ) |>
  summarise(
    p99 = quantile(P21, 0.99)
  ) |>
  pull(p99)


grafico_ingreso <- base_analisis |>
  filter(
    ESTADO == 1,
    P21 > 0
  ) |>
  ggplot(aes(x = P21)) +
  geom_histogram(
    binwidth = 100000,
    boundary = 0,
    fill =  "#1F77B4"
  ) +
  coord_cartesian(
    xlim = c(0, p99_ingreso)
  ) +
  labs(
    title = "Distribución del ingreso laboral",
    subtitle = "Escala lineal, hasta el percentil 99",
    x = "Ingreso de la ocupación principal",
    y = "Número de personas"
  ) +
  theme_minimal()

grafico_ingreso


# Histograma en escala logarítmica.
# Se utiliza únicamente para visualizar la distribución completa.

grafico_ingreso_log <- base_analisis |>
  filter(
    ESTADO == 1,
    P21 > 0
  ) |>
  mutate(
    log_ingreso = log10(P21)
  ) |>
  ggplot(aes(x = log_ingreso)) +
  geom_histogram(
    binwidth = 0.1,
    fill =  "#1F77B4"
  ) +
  labs(
    title = "Distribución del ingreso laboral",
    subtitle = "Escala logarítmica utilizada únicamente para visualización",
    x = "Logaritmo del ingreso laboral",
    y = "Número de personas"
  ) +
  theme_minimal()

grafico_ingreso_log


#------------------------------------------------------------------------------#
# 2.4 RELACIONES BIVARIADAS CON FORMALIDAD
#------------------------------------------------------------------------------#

# Se analiza la relación entre formalidad laboral y las principales
# características individuales y laborales consideradas para el análisis.

# FORMALIDAD Y SEXO

tabla_formalidad_sexo <- base_analisis |>
  filter(
    ESTADO == 1,
    EMPLEO %in% c(1, 2),
    CH04 %in% c(1, 2)
  ) |>
  mutate(
    sexo = factor(
      CH04,
      levels = c(1, 2),
      labels = c(
        "Varón",
        "Mujer"
      )
    ),
    formalidad = factor(
      EMPLEO,
      levels = c(1, 2),
      labels = c(
        "Formal",
        "Informal"
      )
    )
  ) |>
  count(
    sexo,
    formalidad
  ) |>
  group_by(sexo) |>
  mutate(
    porcentaje = n / sum(n) * 100
  ) |>
  ungroup()


grafico_formalidad_sexo <- tabla_formalidad_sexo |>
  ggplot(
    aes(
      x = sexo,
      y = porcentaje,
      fill = formalidad
    )
  ) +
  geom_col(position = "fill") +
  scale_fill_manual(
    values = c(
      "Formal" = "#0B3C5D",
      "Informal" = "#6BAED6"
    )
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x * 100, "%")
  ) +
  labs(
    title = "Formalidad laboral según sexo",
    x = "Sexo",
    y = "Porcentaje",
    fill = "Formalidad"
  ) +
  theme_minimal()

grafico_formalidad_sexo


# FORMALIDAD Y NIVEL EDUCATIVO

tabla_formalidad_educacion <- base_analisis |>
  filter(
    ESTADO == 1,
    EMPLEO %in% c(1, 2),
    NIVEL_ED %in% 1:7
  ) |>
  mutate(
    nivel_educativo = factor(
      NIVEL_ED,
      levels = c(7, 1, 2, 3, 4, 5, 6),
      labels = c(
        "Sin instrucción",
        "Primario incompleto",
        "Primario completo",
        "Secundario incompleto",
        "Secundario completo",
        "Superior/universitario incompleto",
        "Superior/universitario completo"
      )
    ),
    formalidad = factor(
      EMPLEO,
      levels = c(1, 2),
      labels = c(
        "Formal",
        "Informal"
      )
    )
  ) |>
  count(
    nivel_educativo,
    formalidad
  ) |>
  group_by(nivel_educativo) |>
  mutate(
    porcentaje = n / sum(n) * 100
  ) |>
  ungroup()


grafico_formalidad_educacion <- tabla_formalidad_educacion |>
  ggplot(
    aes(
      x = nivel_educativo,
      y = porcentaje,
      fill = formalidad
    )
  ) +
  geom_col(position = "fill") +
  scale_fill_manual(
    values = c(
      "Formal" = "#0B3C5D",
      "Informal" = "#6BAED6"
    )
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x * 100, "%")
  ) +
  labs(
    title = "Formalidad laboral según nivel educativo",
    x = "Nivel educativo",
    y = "Porcentaje",
    fill = "Formalidad"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    )
  )

grafico_formalidad_educacion


# FORMALIDAD Y EDAD

grafico_formalidad_edad <- base_analisis |>
  filter(
    ESTADO == 1,
    EMPLEO %in% c(1, 2),
    CH06 != 99
  ) |>
  mutate(
    formalidad = factor(
      EMPLEO,
      levels = c(1, 2),
      labels = c(
        "Formal",
        "Informal"
      )
    )
  ) |>
  ggplot(
    aes(
      x = formalidad,
      y = CH06,
      fill = formalidad
    )
  ) +
  geom_boxplot() +
  scale_fill_manual(
    values = c(
      "Formal" = "#0B3C5D",
      "Informal" = "#6BAED6"
    )
  ) +
  labs(
    title = "Distribución de la edad según formalidad laboral",
    x = "Formalidad laboral",
    y = "Edad",
    fill = "Formalidad"
  ) +
  theme_minimal()

grafico_formalidad_edad


# FORMALIDAD Y CATEGORÍA OCUPACIONAL

tabla_formalidad_categoria <- base_analisis |>
  filter(
    ESTADO == 1,
    EMPLEO %in% c(1, 2),
    CAT_OCUP %in% 1:4
  ) |>
  mutate(
    categoria_ocupacional = factor(
      CAT_OCUP,
      levels = c(1, 2, 3, 4),
      labels = c(
        "Patrón",
        "Cuenta propia",
        "Obrero o empleado",
        "Trabajador familiar sin remuneración"
      )
    ),
    formalidad = factor(
      EMPLEO,
      levels = c(1, 2),
      labels = c(
        "Formal",
        "Informal"
      )
    )
  ) |>
  count(
    categoria_ocupacional,
    formalidad
  ) |>
  group_by(categoria_ocupacional) |>
  mutate(
    porcentaje = n / sum(n) * 100
  ) |>
  ungroup()


grafico_formalidad_categoria <- tabla_formalidad_categoria |>
  ggplot(
    aes(
      x = categoria_ocupacional,
      y = porcentaje,
      fill = formalidad
    )
  ) +
  geom_col(position = "fill") +
  scale_fill_manual(
    values = c(
      "Formal" = "#0B3C5D",
      "Informal" = "#6BAED6"
    )
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x * 100, "%")
  ) +
  labs(
    title = "Formalidad laboral según categoría ocupacional",
    x = "Categoría ocupacional",
    y = "Porcentaje",
    fill = "Formalidad"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 20,
      hjust = 1
    )
  )

grafico_formalidad_categoria


# FORMALIDAD E INGRESO LABORAL

grafico_formalidad_ingreso <- eph_2023_2026 |>
  filter(
    ESTADO == 1,
    EMPLEO %in% c(1, 2),
    P21 > 0
  ) |>
  mutate(
    formalidad = factor(
      EMPLEO,
      levels = c(1, 2),
      labels = c(
        "Formal",
        "Informal"
      )
    ),
    ingreso_miles = P21 / 1000
  ) |>
  ggplot(
    aes(
      x = formalidad,
      y = ingreso_miles,
      fill = formalidad
    )
  ) +
  geom_boxplot() +
  scale_fill_manual(
    values = c(
      "Formal" = "#0B3C5D",
      "Informal" = "#6BAED6"
    )
  ) +
  labs(
    title = "Distribución del ingreso laboral según formalidad",
    subtitle = "Ingreso expresado en miles de pesos",
    x = "Formalidad laboral",
    y = "Ingreso laboral (miles de pesos)",
    fill = "Formalidad"
  ) +
  theme_minimal()

grafico_formalidad_ingreso


# FORMALIDAD Y REGIÓN

tabla_formalidad_region <- base_analisis |>
  filter(
    ESTADO == 1,
    EMPLEO %in% c(1, 2)
  ) |>
  mutate(
    formalidad = factor(
      EMPLEO,
      levels = c(1, 2),
      labels = c(
        "Formal",
        "Informal"
      )
    ),
    region = factor(
      REGION,
      levels = c(1, 40, 41, 42, 43, 44),
      labels = c(
        "Gran Buenos Aires",
        "Noroeste",
        "Noreste",
        "Cuyo",
        "Pampeana",
        "Patagonia"
      )
    )
  ) |>
  count(
    region,
    formalidad
  ) |>
  group_by(region) |>
  mutate(
    porcentaje = n / sum(n) * 100
  ) |>
  ungroup()


grafico_formalidad_region <- tabla_formalidad_region |>
  ggplot(
    aes(
      x = region,
      y = porcentaje,
      fill = formalidad
    )
  ) +
  geom_col(position = "fill") +
  scale_fill_manual(
    values = c(
      "Formal" = "#0B3C5D",
      "Informal" = "#6BAED6"
    )
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x * 100, "%")
  ) +
  labs(
    title = "Formalidad laboral según región",
    x = "Región",
    y = "Porcentaje",
    fill = "Formalidad"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 20,
      hjust = 1
    )
  )

grafico_formalidad_region

#==============================================================================#
# 3. CONSTRUCCIÓN DE LA BASE DE MOVILIDAD
#==============================================================================#

#-------------------------------------------------------------------------------
# 3.1. Identificación de personas
#-------------------------------------------------------------------------------

# La identificación longitudinal de las personas se realiza mediante la
# combinación de CODUSU, NRO_HOGAR y COMPONENTE.

# Verificación de unicidad de cada persona dentro de cada período

duplicados_persona <- base_analisis |>
  count(
    CODUSU,
    NRO_HOGAR,
    COMPONENTE,
    periodo_id
  ) |>
  filter(n > 1)

duplicados_persona


#-------------------------------------------------------------------------------
# 3.2. Unión de períodos consecutivos t → t+1
#-------------------------------------------------------------------------------

# Se construye una base de movilidad en la que cada fila representa a una
# persona observada en dos períodos consecutivos.
#
# Las características individuales y laborales corresponden al período t,
# mientras que EMPLEO y ESTADO del período t+1 se utilizan para identificar
# la situación posterior.

base_movilidad <- base_analisis |>
  select(
    CODUSU,
    NRO_HOGAR,
    COMPONENTE,
    ANO4,
    TRIMESTRE,
    periodo_id,
    ESTADO,
    EMPLEO,
    CH04,
    CH06,
    NIVEL_ED,
    CAT_OCUP,
    PP3E_TOT,
    INTENSI,
    PP04A,
    PP04C,
    P21,
    REGION,
    AGLOMERADO
  ) |>
  rename(
    periodo_t = periodo_id,
    estado_t = ESTADO,
    empleo_t = EMPLEO
  ) |>
  inner_join(
    base_analisis |>
      select(
        CODUSU,
        NRO_HOGAR,
        COMPONENTE,
        periodo_id,
        ESTADO,
        EMPLEO
      ) |>
      mutate(
        periodo_t = periodo_id - 1
      ) |>
      rename(
        estado_t1 = ESTADO,
        empleo_t1 = EMPLEO
      ) |>
      select(
        CODUSU,
        NRO_HOGAR,
        COMPONENTE,
        periodo_t,
        estado_t1,
        empleo_t1
      ),
    by = c(
      "CODUSU",
      "NRO_HOGAR",
      "COMPONENTE",
      "periodo_t"
    )
  )


#------------------------------------------------------------------------------#
# 3.3. Verificaciones
#------------------------------------------------------------------------------#

# Dimensiones de la base de movilidad

dim(base_movilidad)


# Verificación de duplicados después de la unión

base_movilidad |>
  count(
    CODUSU,
    NRO_HOGAR,
    COMPONENTE,
    periodo_t
  ) |>
  filter(n > 1)


# Verificación de los períodos de transición

base_movilidad |>
  count(periodo_t) |>
  arrange(periodo_t)

#==============================================================================#
# 4. ANÁLISIS DESCRIPTIVO DE LA MOVILIDAD
#==============================================================================#

#------------------------------------------------------------------------------#
# 4.1. TRANSICIONES DE FORMALIDAD
#------------------------------------------------------------------------------#

# Se consideran únicamente personas ocupadas en ambos períodos y con
# información válida sobre formalidad en t y t+1.

base_transiciones_empleo <- base_movilidad |>
  filter(
    estado_t == 1,
    estado_t1 == 1,
    empleo_t %in% c(1, 2),
    empleo_t1 %in% c(1, 2)
  )

# Clasificación de las transiciones entre períodos consecutivos

base_transiciones_empleo <- base_transiciones_empleo |>
  mutate(
    transicion_empleo = case_when(
      empleo_t == 1 & empleo_t1 == 1 ~ "Formal → Formal",
      empleo_t == 1 & empleo_t1 == 2 ~ "Formal → Informal",
      empleo_t == 2 & empleo_t1 == 1 ~ "Informal → Formal",
      empleo_t == 2 & empleo_t1 == 2 ~ "Informal → Informal"
    )
  )

# Cantidad de observaciones por tipo de transición

base_transiciones_empleo |>
  count(transicion_empleo)

#------------------------------------------------------------------------------#
# 4.2. MATRIZ DE TRANSICIÓN
#------------------------------------------------------------------------------#

matriz_transicion <- base_transiciones_empleo |>
  count(empleo_t, empleo_t1) |>
  group_by(empleo_t) |>
  mutate(
    porcentaje = n / sum(n) * 100
  ) |>
  ungroup()

matriz_transicion

grafico_matriz_transicion <- ggplot(
  matriz_transicion,
  aes(
    x = factor(empleo_t1,
               levels = c(1, 2),
               labels = c("Formal", "Informal")),
    y = factor(empleo_t,
               levels = c(1, 2),
               labels = c("Formal", "Informal")),
    fill = porcentaje
  )
) +
  geom_tile() +
  geom_text(
    aes(label = paste0(round(porcentaje, 1), "%")),
    size = 5
  ) +
  scale_fill_gradient(
    low = "#D9EAF7",
    high = "#0B3C5D"
  ) +
  labs(
    title = "Matriz de transición de la formalidad laboral",
    subtitle = "Porcentaje de personas según situación en t y t+1",
    x = "Formalidad en t+1",
    y = "Formalidad en t",
    fill = "%"
  ) +
  theme_minimal()

#------------------------------------------------------------------------------#
# 4.3. EVOLUCIÓN TEMPORAL DE LAS TRANSICIONES
#------------------------------------------------------------------------------#

evolucion_transiciones <- base_transiciones_empleo |>
  group_by(periodo_t, transicion_empleo) |>
  summarise(
    n = n(),
    .groups = "drop"
  )

evolucion_transiciones <- evolucion_transiciones |>
  mutate(
    periodo = case_when(
      periodo_t == 4  ~ "2023T4",
      periodo_t == 5  ~ "2024T1",
      periodo_t == 6  ~ "2024T2",
      periodo_t == 7  ~ "2024T3",
      periodo_t == 8  ~ "2024T4",
      periodo_t == 9  ~ "2025T1",
      periodo_t == 10 ~ "2025T2",
      periodo_t == 11 ~ "2025T3",
      periodo_t == 12 ~ "2025T4"
    )
  )

tasas_transicion <- base_transiciones_empleo |>
  group_by(periodo_t, empleo_t) |>
  summarise(
    tasa_transicion = mean(empleo_t != empleo_t1) * 100,
    .groups = "drop"
  ) |>
  filter(empleo_t %in% c(1, 2)) |>
  mutate(
    transicion = case_when(
      empleo_t == 1 ~ "Formal → Informal",
      empleo_t == 2 ~ "Informal → Formal"
    ),
    periodo = case_when(
      periodo_t == 4  ~ "2023T4",
      periodo_t == 5  ~ "2024T1",
      periodo_t == 6  ~ "2024T2",
      periodo_t == 7  ~ "2024T3",
      periodo_t == 8  ~ "2024T4",
      periodo_t == 9  ~ "2025T1",
      periodo_t == 10 ~ "2025T2",
      periodo_t == 11 ~ "2025T3",
      periodo_t == 12 ~ "2025T4"
    )
  )

grafico_tasas_transicion <- ggplot(
  tasas_transicion,
  aes(
    x = periodo,
    y = tasa_transicion,
    group = transicion,
    color = transicion
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(
    values = c(
      "Formal → Informal" = "#0B3C5D",
      "Informal → Formal" = "#6BAED6"
    )
  ) +
  labs(
    title = "Evolución de las tasas de transición de formalidad",
    subtitle = "Transiciones entre períodos consecutivos",
    x = "Período",
    y = "Tasa de transición (%)",
    color = "Transición"
  ) +
  theme_minimal()

#==============================================================================#
# 5. MOVILIDAD DE FORMALIDAD Y CARACTERÍSTICAS INDIVIDUALES
#==============================================================================#

# CATEGORÍA OCUPACIONAL

grafico_movilidad_categoria <- base_transiciones_empleo |>
  filter(
    CAT_OCUP %in% c(1, 2, 3, 4),
    empleo_t != empleo_t1
  ) |>
  mutate(
    categoria_ocupacional = factor(
      CAT_OCUP,
      levels = c(1, 2, 3, 4),
      labels = c(
        "Patrón",
        "Cuenta propia",
        "Obrero o\nempleado",
        "Trabajador familiar\nsin remuneración"
      )
    ),
    transicion = factor(
      case_when(
        empleo_t == 1 & empleo_t1 == 2 ~ "Formal → Informal",
        empleo_t == 2 & empleo_t1 == 1 ~ "Informal → Formal"
      ),
      levels = c(
        "Formal → Informal",
        "Informal → Formal"
      )
    )
  ) |>
  count(transicion, categoria_ocupacional) |>
  group_by(transicion) |>
  mutate(
    porcentaje = n / sum(n) * 100
  ) |>
  ungroup() |>
  ggplot(
    aes(
      x = transicion,
      y = porcentaje,
      fill = categoria_ocupacional
    )
  ) +
  geom_col(position = "fill") +
  scale_y_continuous(
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Composición por categoría ocupacional de las transiciones de formalidad",
    subtitle = "Personas que cambiaron de condición de formalidad, 4T2023–1T2026",
    x = "Transición",
    y = "Distribución (%)",
    fill = "Categoría ocupacional"
  ) +
  theme_minimal()

# SEXO

grafico_movilidad_sexo <- base_transiciones_empleo |>
  filter(
    CH04 %in% c(1, 2),
    empleo_t != empleo_t1
  ) |>
  mutate(
    sexo = factor(
      CH04,
      levels = c(1, 2),
      labels = c("Varón", "Mujer")
    ),
    transicion = factor(
      case_when(
        empleo_t == 1 & empleo_t1 == 2 ~ "Formal → Informal",
        empleo_t == 2 & empleo_t1 == 1 ~ "Informal → Formal"
      ),
      levels = c(
        "Formal → Informal",
        "Informal → Formal"
      )
    )
  ) |>
  count(transicion, sexo) |>
  group_by(transicion) |>
  mutate(
    porcentaje = n / sum(n) * 100
  ) |>
  ungroup() |>
  ggplot(
    aes(
      x = transicion,
      y = porcentaje,
      fill = sexo
    )
  ) +
  geom_col(position = "fill") +
  scale_fill_manual(
    values = c(
      "Varón" = "#0B3C5D",
      "Mujer" = "#6BAED6"
    )
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Composición por sexo de las transiciones de formalidad",
    subtitle = "Personas que cambiaron de condición de formalidad, 4T2023–1T2026",
    x = "Transición",
    y = "Distribución (%)",
    fill = "Sexo"
  ) +
  theme_minimal()

# NIVEL EDUCATIVO

grafico_movilidad_educacion <- base_transiciones_empleo |>
  filter(
    NIVEL_ED %in% 1:7,
    empleo_t != empleo_t1
  ) |>
  mutate(
    nivel_educativo = factor(
      NIVEL_ED,
      levels = c(7, 1, 2, 3, 4, 5, 6),
      labels = c(
        "Sin instrucción",
        "Primario incompleto",
        "Primario completo",
        "Secundario incompleto",
        "Secundario completo",
        "Superior y universitario incompleto",
        "Superior y universitario completo"
      )
    ),
    transicion = factor(
      case_when(
        empleo_t == 1 & empleo_t1 == 2 ~ "Formal → Informal",
        empleo_t == 2 & empleo_t1 == 1 ~ "Informal → Formal"
      ),
      levels = c(
        "Formal → Informal",
        "Informal → Formal"
      )
    )
  ) |>
  count(transicion, nivel_educativo) |>
  group_by(transicion) |>
  mutate(
    porcentaje = n / sum(n) * 100
  ) |>
  ungroup() |>
  ggplot(
    aes(
      x = transicion,
      y = porcentaje,
      fill = nivel_educativo
    )
  ) +
  geom_col(position = "fill") +
  scale_y_continuous(
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Composición educativa de las transiciones de formalidad",
    subtitle = "Personas que cambiaron de condición de formalidad, 4T2023–1T2026",
    x = "Transición",
    y = "Distribución (%)",
    fill = "Nivel educativo"
  ) +
  theme_minimal()

# EDAD

grafico_movilidad_edad <- base_transiciones_empleo |>
  filter(
    CH06 != 99
  ) |>
  mutate(
    transicion = case_when(
      empleo_t == 1 & empleo_t1 == 2 ~ "Formal → Informal",
      empleo_t == 2 & empleo_t1 == 1 ~ "Informal → Formal"
    )
  ) |>
  filter(!is.na(transicion)) |>
  mutate(
    transicion = factor(
      transicion,
      levels = c(
        "Formal → Informal",
        "Informal → Formal"
      )
    )
  ) |>
  ggplot(
    aes(
      x = transicion,
      y = CH06,
      fill = transicion
    )
  ) +
  geom_boxplot() +
  scale_fill_manual(
    values = c(
      "Formal → Informal" = "#0B3C5D",
      "Informal → Formal" = "#6BAED6"
    )
  ) +
  labs(
    title = "Edad y movilidad dentro del empleo",
    subtitle = "Personas que cambiaron de condición de formalidad, 4T2023–1T2026",
    x = "Transición",
    y = "Edad (años)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# INGRESO LABORAL

grafico_movilidad_ingreso <- base_transiciones_empleo |>
  filter(
    P21 > 0
  ) |>
  mutate(
    transicion = case_when(
      empleo_t == 1 & empleo_t1 == 2 ~ "Formal → Informal",
      empleo_t == 2 & empleo_t1 == 1 ~ "Informal → Formal"
    )
  ) |>
  filter(!is.na(transicion)) |>
  mutate(
    transicion = factor(
      transicion,
      levels = c(
        "Formal → Informal",
        "Informal → Formal"
      )
    ),
    ingreso_miles = P21 / 1000
  ) |>
  ggplot(
    aes(
      x = transicion,
      y = ingreso_miles,
      fill = transicion
    )
  ) +
  geom_boxplot() +
  scale_fill_manual(
    values = c(
      "Formal → Informal" = "#0B3C5D",
      "Informal → Formal" = "#6BAED6"
    )
  ) +
  labs(
    title = "Ingreso laboral y movilidad dentro del empleo",
    subtitle = "Personas que cambiaron de condición de formalidad, 4T2023–1T2026",
    x = "Transición",
    y = "Ingreso laboral (miles de pesos)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#==============================================================================#
# 6. APLICACIÓN DE LA TÉCNICA
#==============================================================================#

#------------------------------------------------------------------------------#
# 6.1. CONSTRUCCIÓN DE LAS BASES DE MODELIZACIÓN
#------------------------------------------------------------------------------#

# Modelo 1: Formal → Informal

base_formal_informal <- base_transiciones_empleo |>
  filter(
    empleo_t == 1,
    empleo_t1 %in% c(1, 2)
  ) |>
  mutate(
    y = ifelse(empleo_t1 == 2, 1, 0)
  )


# Modelo 2: Informal → Formal

base_informal_formal <- base_transiciones_empleo |>
  filter(
    empleo_t == 2,
    empleo_t1 %in% c(1, 2)
  ) |>
  mutate(
    y = ifelse(empleo_t1 == 1, 1, 0)
  )

#------------------------------------------------------------------------------#
# 6.2. PREPARACIÓN DE LAS VARIABLES
#------------------------------------------------------------------------------#

preparar_base_modelo <- function(base) {
  
  base |>
    filter(
      CH04 %in% c(1, 2),
      CH06 != 99,
      PP3E_TOT >= 1,
      PP3E_TOT <= 168,
      P21 > 0,
      NIVEL_ED %in% 1:7
    ) |>
    mutate(
      
      sexo = factor(
        CH04,
        levels = c(1, 2),
        labels = c("Varón", "Mujer")
      ),
      
      edad = CH06,
      
      nivel_educativo = factor(
        NIVEL_ED,
        levels = 1:7,
        labels = c(
          "Primario incompleto",
          "Primario completo",
          "Secundario incompleto",
          "Secundario completo",
          "Superior/universitario incompleto",
          "Superior/universitario completo",
          "Sin instrucción"
        )
      ),
      
      categoria_ocupacional = factor(CAT_OCUP),
      
      region = factor(REGION),
      
      horas = PP3E_TOT,
      
      ingreso_miles = P21 / 1000
    ) |>
    select(
      y,
      sexo,
      edad,
      nivel_educativo,
      categoria_ocupacional,
      horas,
      ingreso_miles,
      region
    ) |>
    drop_na()
}


base_FI <- preparar_base_modelo(
  base_formal_informal
)

base_IF <- preparar_base_modelo(
  base_informal_formal
)


# Agrupamiento de las categorías educativas con baja frecuencia

base_FI <- base_FI |>
  mutate(
    nivel_educativo = fct_collapse(
      nivel_educativo,
      "Primario incompleto" = c(
        "Primario incompleto",
        "Sin instrucción"
      )
    )
  )

base_IF <- base_IF |>
  mutate(
    nivel_educativo = fct_collapse(
      nivel_educativo,
      "Primario incompleto" = c(
        "Primario incompleto",
        "Sin instrucción"
      )
    )
  )

#------------------------------------------------------------------------------#
# 6.3. DISTRIBUCIÓN DE LA VARIABLE DEPENDIENTE
#------------------------------------------------------------------------------#

table(base_FI$y)
prop.table(table(base_FI$y))

table(base_IF$y)
prop.table(table(base_IF$y))

#------------------------------------------------------------------------------#
# 6.4. DIVISIÓN TRAIN/TEST
#------------------------------------------------------------------------------#

set.seed(2026)

# Formal → Informal

n_FI <- nrow(base_FI)

idx_train_FI <- sample(
  1:n_FI,
  size = 0.7 * n_FI
)

train_FI <- base_FI[idx_train_FI, ]

test_FI <- base_FI[-idx_train_FI, ]


# Informal → Formal

n_IF <- nrow(base_IF)

idx_train_IF <- sample(
  1:n_IF,
  size = 0.7 * n_IF
)

train_IF <- base_IF[idx_train_IF, ]

test_IF <- base_IF[-idx_train_IF, ]

#------------------------------------------------------------------------------#
# 6.5. ESTIMACIÓN DE LOS MODELOS LOGIT
#------------------------------------------------------------------------------#

# Modelo 1: Formal → Informal

modelo_FI <- glm(
  y ~ sexo +
    edad +
    nivel_educativo +
    categoria_ocupacional +
    horas +
    ingreso_miles +
    region,
  data = train_FI,
  family = binomial
)

summary(modelo_FI)


# Modelo 2: Informal → Formal

modelo_IF <- glm(
  y ~ sexo +
    edad +
    nivel_educativo +
    categoria_ocupacional +
    horas +
    ingreso_miles +
    region,
  data = train_IF,
  family = binomial
)

summary(modelo_IF)

#------------------------------------------------------------------------------#
# 6.6. ODDS RATIOS
#------------------------------------------------------------------------------#

# Formal -> Informal

odds_ratios_FI <- exp(coef(modelo_FI))

odds_ratios_FI


# Informal -> Formal

odds_ratios_IF <- exp(coef(modelo_IF))

odds_ratios_IF


# Intervalos de confianza

IC_FI <- exp(confint(modelo_FI))

IC_FI


IC_IF <- exp(confint(modelo_IF))

IC_IF


#------------------------------------------------------------------------------#
# 6.7. PREDICCIÓN DE PROBABILIDADES
#------------------------------------------------------------------------------#

prob_FI <- predict(
  modelo_FI,
  newdata = test_FI,
  type = "response"
)


prob_IF <- predict(
  modelo_IF,
  newdata = test_IF,
  type = "response"
)

# Distribución de probabilidades predichas

df_prob_FI <- tibble(
  probabilidad = prob_FI,
  transicion = "Formal → Informal"
)

df_prob_IF <- tibble(
  probabilidad = prob_IF,
  transicion = "Informal → Formal"
)


df_probabilidades <- bind_rows(
  df_prob_FI,
  df_prob_IF
)


grafico_probabilidades <- ggplot(
  df_probabilidades,
  aes(
    x = probabilidad
  )
) +
  geom_histogram(
    bins = 40,
    fill =  "#1F77B4"
  ) +
  geom_vline(
    xintercept = 0.5,
    linetype = "dashed",
    color = "#0B3C5D"
  ) +
  facet_wrap(
    ~ transicion
  ) +
  labs(
    title = "Distribución de las probabilidades predichas",
    subtitle = "La línea vertical representa el umbral convencional de 0,5",
    x = "Probabilidad predicha",
    y = "Cantidad de observaciones"
  ) +
  theme_minimal()


#------------------------------------------------------------------------------#
# 6.8. CLASIFICACIÓN CON UMBRAL 0.5
#------------------------------------------------------------------------------#

pred_FI_05 <- ifelse(
  prob_FI >= 0.5,
  1,
  0
)


pred_IF_05 <- ifelse(
  prob_IF >= 0.5,
  1,
  0
)


#------------------------------------------------------------------------------#
# 6.9. MATRICES DE CONFUSIÓN
#------------------------------------------------------------------------------#

matriz_FI <- table(
  Predicho = pred_FI_05,
  Real = test_FI$y
)

matriz_FI


matriz_IF <- table(
  Predicho = pred_IF_05,
  Real = test_IF$y
)

matriz_IF


#------------------------------------------------------------------------------#
# 6.10. MÉTRICAS DE CLASIFICACIÓN
#------------------------------------------------------------------------------#

calcular_metricas <- function(real, predicho) {
  
  VP <- sum(real == 1 & predicho == 1)
  FP <- sum(real == 0 & predicho == 1)
  VN <- sum(real == 0 & predicho == 0)
  FN <- sum(real == 1 & predicho == 0)
  
  accuracy <- (VP + VN) / (VP + VN + FP + FN)
  
  precision <- ifelse(
    VP + FP == 0,
    NA,
    VP / (VP + FP)
  )
  
  sensibilidad <- ifelse(
    VP + FN == 0,
    NA,
    VP / (VP + FN)
  )
  
  especificidad <- ifelse(
    VN + FP == 0,
    NA,
    VN / (VN + FP)
  )
  
  F1 <- ifelse(
    precision + sensibilidad == 0,
    NA,
    2 * precision * sensibilidad /
      (precision + sensibilidad)
  )
  
  balanced_accuracy <- (
    sensibilidad + especificidad
  ) / 2
  
  tibble(
    Accuracy = accuracy,
    Precision = precision,
    Sensibilidad = sensibilidad,
    Especificidad = especificidad,
    F1 = F1,
    Balanced_Accuracy = balanced_accuracy
  )
}


metricas_FI_05 <- calcular_metricas(
  test_FI$y,
  pred_FI_05
)

metricas_IF_05 <- calcular_metricas(
  test_IF$y,
  pred_IF_05
)


metricas_FI_05

metricas_IF_05


#------------------------------------------------------------------------------#
# 6.11. CURVAS ROC Y AUC
#------------------------------------------------------------------------------#

roc_FI <- roc(
  test_FI$y,
  prob_FI,
  levels = c(0, 1),
  direction = "<"
)


roc_IF <- roc(
  test_IF$y,
  prob_IF,
  levels = c(0, 1),
  direction = "<"
)


auc_FI <- auc(roc_FI)

auc_IF <- auc(roc_IF)


auc_FI

auc_IF


# Curva ROC: Formal -> Informal

plot(
  roc_FI,
  main = "Curva ROC: Formal → Informal",
  col = "#0B3C5D"
)

abline(
  a = 0,
  b = 1,
  lty = 2,
  col = "#6BAED6"
)


# Curva ROC: Informal -> Formal

plot(
  roc_IF,
  main = "Curva ROC: Informal → Formal",
  col = "#0B3C5D"
)

abline(
  a = 0,
  b = 1,
  lty = 2,
  col = "#6BAED6"
)


#------------------------------------------------------------------------------#
# 6.12. EVALUACIÓN DE DISTINTOS UMBRALES
#------------------------------------------------------------------------------#

umbrales <- seq(
  0.01,
  0.99,
  by = 0.01
)


F1_FI <- sapply(
  umbrales,
  function(umbral) {
    
    pred <- ifelse(
      prob_FI >= umbral,
      1,
      0
    )
    
    calcular_metricas(
      test_FI$y,
      pred
    )$F1
  }
)


F1_IF <- sapply(
  umbrales,
  function(umbral) {
    
    pred <- ifelse(
      prob_IF >= umbral,
      1,
      0
    )
    
    calcular_metricas(
      test_IF$y,
      pred
    )$F1
  }
)


# Umbral que maximiza F1

umbral_optimo_FI <- umbrales[
  which.max(F1_FI)
]


umbral_optimo_IF <- umbrales[
  which.max(F1_IF)
]


umbral_optimo_FI

umbral_optimo_IF


#------------------------------------------------------------------------------#
# 6.13. CLASIFICACIÓN CON UMBRAL ÓPTIMO
#------------------------------------------------------------------------------#

pred_FI_optimo <- ifelse(
  prob_FI >= umbral_optimo_FI,
  1,
  0
)


pred_IF_optimo <- ifelse(
  prob_IF >= umbral_optimo_IF,
  1,
  0
)


# Matrices de confusión

matriz_FI_optimo <- table(
  Predicho = pred_FI_optimo,
  Real = test_FI$y
)


matriz_IF_optimo <- table(
  Predicho = pred_IF_optimo,
  Real = test_IF$y
)


matriz_FI_optimo

matriz_IF_optimo


# Métricas

metricas_FI_optimo <- calcular_metricas(
  test_FI$y,
  pred_FI_optimo
)


metricas_IF_optimo <- calcular_metricas(
  test_IF$y,
  pred_IF_optimo
)


metricas_FI_optimo

metricas_IF_optimo


#------------------------------------------------------------------------------#
# 6.14. GRÁFICO F1 SEGÚN EL UMBRAL
#------------------------------------------------------------------------------#

datos_F1_FI <- tibble(
  umbral = umbrales,
  F1 = F1_FI
)


datos_F1_IF <- tibble(
  umbral = umbrales,
  F1 = F1_IF
)


grafico_F1_FI <- ggplot(
  datos_F1_FI,
  aes(
    x = umbral,
    y = F1
  )
) +
  geom_line(
    color = "#0B3C5D"
  ) +
  geom_vline(
    xintercept = umbral_optimo_FI,
    linetype = "dashed",
    color = "#6BAED6"
  ) +
  labs(
    title = "F1 según el umbral de clasificación",
    subtitle = "Modelo Formal → Informal",
    x = "Umbral",
    y = "F1"
  ) +
  theme_minimal()


grafico_F1_IF <- ggplot(
  datos_F1_IF,
  aes(
    x = umbral,
    y = F1
  )
) +
  geom_line(
    color = "#0B3C5D"
  ) +
  geom_vline(
    xintercept = umbral_optimo_IF,
    linetype = "dashed",
    color = "#6BAED6"
  ) +
  labs(
    title = "F1 según el umbral de clasificación",
    subtitle = "Modelo Informal → Formal",
    x = "Umbral",
    y = "F1"
  ) +
  theme_minimal()


#------------------------------------------------------------------------------#
# 6.15. COMPARACIÓN DE MÉTRICAS
#------------------------------------------------------------------------------#

comparacion_FI <- bind_rows(
  metricas_FI_05 |>
    mutate(Umbral = 0.5),
  
  metricas_FI_optimo |>
    mutate(Umbral = umbral_optimo_FI)
) |>
  select(
    Umbral,
    Accuracy,
    Precision,
    Sensibilidad,
    Especificidad,
    F1,
    Balanced_Accuracy
  )


comparacion_IF <- bind_rows(
  metricas_IF_05 |>
    mutate(Umbral = 0.5),
  
  metricas_IF_optimo |>
    mutate(Umbral = umbral_optimo_IF)
) |>
  select(
    Umbral,
    Accuracy,
    Precision,
    Sensibilidad,
    Especificidad,
    F1,
    Balanced_Accuracy
  )


comparacion_FI

comparacion_IF


#------------------------------------------------------------------------------#
# 6.16. GRÁFICOS DE ODDS RATIOS
#------------------------------------------------------------------------------#

resultados_FI <- tidy(
  modelo_FI,
  exponentiate = TRUE,
  conf.int = TRUE
) |>
  filter(term != "(Intercept)")


resultados_IF <- tidy(
  modelo_IF,
  exponentiate = TRUE,
  conf.int = TRUE
) |>
  filter(term != "(Intercept)")


etiquetas_OR <- c(
  "sexoMujer" = "Mujer",
  "edad" = "Edad",
  "nivel_educativoPrimario completo" = "Primario completo",
  "nivel_educativoSecundario incompleto" = "Secundario incompleto",
  "nivel_educativoSecundario completo" = "Secundario completo",
  "nivel_educativoSuperior/universitario incompleto" = "Superior/universitario incompleto",
  "nivel_educativoSuperior/universitario completo" = "Superior/universitario completo",
  "categoria_ocupacional2" = "Cuenta propia",
  "categoria_ocupacional3" = "Obrero o empleado",
  "horas" = "Horas trabajadas",
  "ingreso_miles" = "Ingreso laboral (+$1.000)",
  "region40" = "Noroeste",
  "region41" = "Noreste",
  "region42" = "Cuyo",
  "region43" = "Pampeana",
  "region44" = "Patagonia"
)


resultados_FI <- resultados_FI |>
  mutate(
    term = recode(term, !!!etiquetas_OR)
  )


resultados_IF <- resultados_IF |>
  mutate(
    term = recode(term, !!!etiquetas_OR)
  )


# Formal -> Informal

grafico_OR_FI <- ggplot(
  resultados_FI,
  aes(
    x = estimate,
    y = reorder(term, estimate)
  )
) +
  geom_point(
    color = "#0B3C5D"
  ) +
  geom_errorbar(
    aes(
      xmin = conf.low,
      xmax = conf.high
    ),
    orientation = "y",
    width = 0.2,
    color = "#0B3C5D"
  ) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "#6BAED6"
  ) +
  scale_x_log10() +
  labs(
    title = "Odds Ratios: Formal → Informal",
    x = "Odds Ratio",
    y = NULL
  ) +
  theme_minimal()


# Informal -> Formal

grafico_OR_IF <- ggplot(
  resultados_IF,
  aes(
    x = estimate,
    y = reorder(term, estimate)
  )
) +
  geom_point(
    color = "#0B3C5D"
  ) +
  geom_errorbar(
    aes(
      xmin = conf.low,
      xmax = conf.high
    ),
    orientation = "y",
    width = 0.2,
    color = "#0B3C5D"
  ) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "#6BAED6"
  ) +
  scale_x_log10() +
  labs(
    title = "Odds Ratios: Informal → Formal",
    x = "Odds Ratio",
    y = NULL
  ) +
  theme_minimal()

#==============================================================================#
# 7. OBJETOS FINALES PARA EL INFORME
#==============================================================================#

#-------------------------------------------------------------------------------
# 7.1. CREACIÓN DE CARPETAS DE RESULTADOS
#-------------------------------------------------------------------------------

dir.create("resultados", showWarnings = FALSE)
dir.create("resultados/figuras", showWarnings = FALSE)
dir.create("resultados/tablas", showWarnings = FALSE)


#-------------------------------------------------------------------------------
# 7.2. GUARDADO DE TODOS LOS GRÁFICOS DEL ANÁLISIS EXPLORATORIO
#-------------------------------------------------------------------------------

objetos_graficos <- ls(pattern = "^grafico_")

for (nombre_grafico in objetos_graficos) {
  
  grafico <- get(nombre_grafico)
  
  if (inherits(grafico, "ggplot")) {
    
    grafico <- grafico +
      theme(
        plot.background = element_rect(
          fill = "white",
          colour = NA
        ),
        panel.background = element_rect(
          fill = "white",
          colour = NA
        ),
        legend.background = element_rect(
          fill = "white",
          colour = NA
        ),
        legend.key = element_rect(
          fill = "white",
          colour = NA
        )
      )
    
    assign(nombre_grafico, grafico)
  }
}

# Los objetos que comienzan con "grafico_" corresponden a las figuras
# generadas durante el análisis exploratorio.

objetos_graficos <- ls(pattern = "^grafico_")

for (nombre_grafico in objetos_graficos) {
  
  grafico <- get(nombre_grafico)
  
  if (inherits(grafico, "ggplot")) {
    
    ggsave(
      filename = file.path(
        "resultados",
        "figuras",
        paste0(nombre_grafico, ".png")
      ),
      plot = grafico,
      width = 9,
      height = 6,
      dpi = 300,
      bg = "white"
    )
    
  }
}


#-------------------------------------------------------------------------------
# 7.3. GUARDADO DE LAS CURVAS ROC
#-------------------------------------------------------------------------------

# Las curvas ROC fueron realizadas con funciones gráficas de pROC,
# por lo que se guardan mediante archivos PNG.

png(
  filename = "resultados/figuras/grafico_ROC_FI.png",
  width = 2400,
  height = 1800,
  res = 300
)

plot(
  roc_FI,
  main = "Curva ROC: Formal → Informal",
  col = "#0B3C5D"
)

abline(
  a = 0,
  b = 1,
  lty = 2,
  col = "#6BAED6"
)

dev.off()


png(
  filename = "resultados/figuras/grafico_ROC_IF.png",
  width = 2400,
  height = 1800,
  res = 300
)

plot(
  roc_IF,
  main = "Curva ROC: Informal → Formal",
  col = "#0B3C5D"
)

abline(
  a = 0,
  b = 1,
  lty = 2,
  col = "#6BAED6"
)

dev.off()


#-------------------------------------------------------------------------------
# 7.4. TABLA DE TRANSICIONES DE FORMALIDAD
#-------------------------------------------------------------------------------

tabla_transiciones <- base_transiciones_empleo |>
  count(transicion_empleo) |>
  mutate(
    porcentaje = n / sum(n) * 100
  )

write_csv(
  tabla_transiciones,
  "resultados/tablas/tabla_transiciones.csv"
)


#-------------------------------------------------------------------------------
# 7.5. MATRIZ DE TRANSICIÓN
#-------------------------------------------------------------------------------

tabla_matriz_transicion <- matriz_transicion |>
  mutate(
    empleo_t = factor(
      empleo_t,
      levels = c(1, 2),
      labels = c("Formal", "Informal")
    ),
    empleo_t1 = factor(
      empleo_t1,
      levels = c(1, 2),
      labels = c("Formal", "Informal")
    )
  ) |>
  select(
    empleo_t,
    empleo_t1,
    n,
    porcentaje
  )

write_csv(
  tabla_matriz_transicion,
  "resultados/tablas/tabla_matriz_transicion.csv"
)


#-------------------------------------------------------------------------------
# 7.6. EVOLUCIÓN TEMPORAL DE LAS TRANSICIONES
#-------------------------------------------------------------------------------

write_csv(
  tasas_transicion,
  "resultados/tablas/tabla_tasas_transicion.csv"
)


#-------------------------------------------------------------------------------
# 7.7. DISTRIBUCIÓN DE LA VARIABLE DEPENDIENTE
#-------------------------------------------------------------------------------

tabla_dependiente_FI <- tibble(
  valor = names(table(base_FI$y)),
  frecuencia = as.vector(table(base_FI$y)),
  porcentaje = as.vector(prop.table(table(base_FI$y))) * 100
)

tabla_dependiente_IF <- tibble(
  valor = names(table(base_IF$y)),
  frecuencia = as.vector(table(base_IF$y)),
  porcentaje = as.vector(prop.table(table(base_IF$y))) * 100
)

write_csv(
  tabla_dependiente_FI,
  "resultados/tablas/tabla_dependiente_FI.csv"
)

write_csv(
  tabla_dependiente_IF,
  "resultados/tablas/tabla_dependiente_IF.csv"
)


#-------------------------------------------------------------------------------
# 7.8. MATRICES DE CONFUSIÓN CON UMBRAL 0.5
#-------------------------------------------------------------------------------

write_csv(
  as.data.frame(matriz_FI),
  "resultados/tablas/matriz_confusion_FI_05.csv"
)

write_csv(
  as.data.frame(matriz_IF),
  "resultados/tablas/matriz_confusion_IF_05.csv"
)


#-------------------------------------------------------------------------------
# 7.9. MÉTRICAS DE CLASIFICACIÓN CON UMBRAL 0.5
#-------------------------------------------------------------------------------

write_csv(
  metricas_FI_05,
  "resultados/tablas/metricas_FI_05.csv"
)

write_csv(
  metricas_IF_05,
  "resultados/tablas/metricas_IF_05.csv"
)


#-------------------------------------------------------------------------------
# 7.10. AUC
#-------------------------------------------------------------------------------

tabla_auc <- tibble(
  modelo = c(
    "Formal → Informal",
    "Informal → Formal"
  ),
  AUC = c(
    as.numeric(auc_FI),
    as.numeric(auc_IF)
  )
)

write_csv(
  tabla_auc,
  "resultados/tablas/tabla_AUC.csv"
)


#-------------------------------------------------------------------------------
# 7.11. UMBRALES ÓPTIMOS
#-------------------------------------------------------------------------------

tabla_umbrales_optimos <- tibble(
  modelo = c(
    "Formal → Informal",
    "Informal → Formal"
  ),
  umbral_optimo = c(
    umbral_optimo_FI,
    umbral_optimo_IF
  )
)

write_csv(
  tabla_umbrales_optimos,
  "resultados/tablas/tabla_umbrales_optimos.csv"
)


#-------------------------------------------------------------------------------
# 7.12. MATRICES DE CONFUSIÓN CON UMBRAL ÓPTIMO
#-------------------------------------------------------------------------------

write_csv(
  as.data.frame(matriz_FI_optimo),
  "resultados/tablas/matriz_confusion_FI_optimo.csv"
)

write_csv(
  as.data.frame(matriz_IF_optimo),
  "resultados/tablas/matriz_confusion_IF_optimo.csv"
)


#-------------------------------------------------------------------------------
# 7.13. MÉTRICAS CON UMBRAL ÓPTIMO
#-------------------------------------------------------------------------------

write_csv(
  metricas_FI_optimo,
  "resultados/tablas/metricas_FI_optimo.csv"
)

write_csv(
  metricas_IF_optimo,
  "resultados/tablas/metricas_IF_optimo.csv"
)


#-------------------------------------------------------------------------------
# 7.14. COMPARACIÓN DE MÉTRICAS
#-------------------------------------------------------------------------------

write_csv(
  comparacion_FI,
  "resultados/tablas/comparacion_metricas_FI.csv"
)

write_csv(
  comparacion_IF,
  "resultados/tablas/comparacion_metricas_IF.csv"
)


#-------------------------------------------------------------------------------
# 7.15. ODDS RATIOS
#-------------------------------------------------------------------------------

tabla_OR <- bind_rows(
  
  resultados_FI |>
    mutate(
      modelo = "Formal → Informal"
    ),
  
  resultados_IF |>
    mutate(
      modelo = "Informal → Formal"
    )
) |>
  select(
    modelo,
    term,
    estimate,
    conf.low,
    conf.high,
    p.value
  )

write_csv(
  tabla_OR,
  "resultados/tablas/tabla_odds_ratios.csv"
)


#-------------------------------------------------------------------------------
# 7.16. DATOS UTILIZADOS PARA LOS GRÁFICOS DE F1
#-------------------------------------------------------------------------------

datos_F1_FI <- datos_F1_FI |>
  mutate(
    modelo = "Formal → Informal"
  )

datos_F1_IF <- datos_F1_IF |>
  mutate(
    modelo = "Informal → Formal"
  )

datos_F1 <- bind_rows(
  datos_F1_FI,
  datos_F1_IF
)

write_csv(
  datos_F1,
  "resultados/tablas/datos_F1.csv"
)


#-------------------------------------------------------------------------------
# 7.17. GUARDADO DE LAS TABLAS EXPLORATORIAS
#-------------------------------------------------------------------------------

# Se guardan automáticamente todas las tablas creadas durante el EDA
# cuyo nombre comienza con "tabla_".

objetos_tablas <- ls(pattern = "^tabla_")

for (nombre_tabla in objetos_tablas) {
  
  tabla <- get(nombre_tabla)
  
  if (is.data.frame(tabla)) {
    
    write_csv(
      tabla,
      file.path(
        "resultados",
        "tablas",
        paste0(nombre_tabla, ".csv")
      )
    )
    
  }
}


#-------------------------------------------------------------------------------
# 7.18. VERIFICACIÓN DE ARCHIVOS GENERADOS
#-------------------------------------------------------------------------------

list.files(
  "resultados/figuras",
  full.names = TRUE
)

list.files(
  "resultados/tablas",
  full.names = TRUE
)
# Movilidad laboral entre formalidad e informalidad en Argentina

Este repositorio contiene los materiales correspondientes al Trabajo Práctico Integrador de la Maestría en Econometría de la Universidad Torcuato Di Tella.

## Datos

El análisis utiliza los microdatos de la **Encuesta Permanente de Hogares (EPH)** para Argentina, correspondientes al período **2023–2026**.

Los datos son provistos por el **Instituto Nacional de Estadística y Censos (INDEC)**. A partir de períodos consecutivos de la encuesta se construye una base longitudinal para analizar las transiciones entre formalidad e informalidad laboral.

## Metodología

Se estudian dos tipos de transición:

- Formalidad → Informalidad
- Informalidad → Formalidad

Para ello, se aplican **modelos de regresión logística (Logit)**, utilizando características individuales y laborales observadas en el período inicial de cada transición.

El análisis incluye además un análisis exploratorio de los datos y la evaluación del desempeño predictivo de los modelos mediante métricas de clasificación y AUC-ROC.

## Cómo ejecutar el proyecto

Para reproducir el análisis:

1. Abrir `TP_Integrador.R` en R o RStudio.
2. Instalar las librerías requeridas por el script, en caso de no tenerlas instaladas.
3. Ejecutar el script completo.

El script contiene el código utilizado para la preparación de los datos, el análisis exploratorio, la construcción de las transiciones y la estimación y evaluación de los modelos.

## Archivos

- `TP_Integrador.R`: script completo utilizado para realizar el análisis.
- `Informe_TP_Integrador.pdf`: informe final con la metodología, resultados y conclusiones.

## Autora

**Agustina Giménez**  
Maestría en Econometría – Universidad Torcuato Di Tella  
2026

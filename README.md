# Regional-Economic-Dynamics-Data-Science-Portfolio
Análisis multiescalar usando Econometría Espacial, Machine Learning y Modelos Basados en Agentes.

Este repositorio integra técnicas de **Econometría Espacial**, **Machine Learning** y **Sistemas Complejos** para analizar el desarrollo regional, trampas de innovación y externalidades cognitivas en México.

## 🛠 stack Tecnológico
* **Python:** Procesamiento de datos (Pandas), Simulación (Agent-Based Modeling).
* **R:** Estadística Espacial (`spdep`, `splm`), Clasificación (`rpart`) y Clustering.

## 🗂 Estructura del Proyecto

### 1. Construcción de Indicadores (Externalidades Cognitivas)
Se desarrollaron índices propios para medir capacidades regionales utilizando:
* `2. Cálculo de coeficientes...`: Metodología para cuantificar flujos de conocimiento.
* `ARCI.ipynb`: Implementación del Análisis de Relaciones.

### 2. Análisis Econométrico Espacial
Evaluación de la dependencia espacial en el crecimiento económico.
* **Paneles Espaciales:** Modelos SAR y SEM para controlar autocorrelación espacial (`spatialpaneldata.R`).
* **Shift-Share Espacial:** Descomposición del crecimiento considerando la vecindad (`shift_share.R`).

### 3. Machine Learning Regional
Segmentación del territorio basada en desempeño económico.
* **Árboles de Decisión:** Clasificación de regiones según su potencial (`árbolclasificación_regional.R`).
* **Conglomerados:** Análisis de clusters multidimensionales (`conglomerados.R`).

### 4. Simulación de Sistemas Complejos (ABM)
Modelado de la "Trampa de Innovación" mediante agentes heterogéneos.
* `abm_regional_trampa_innovacion.ipynb`: Simulación de cómo la interacción local afecta la adopción tecnológica global.

## 📊 Visualizaciones Destacadas

<img width="5369" height="3540" alt="02_trayectorias_temporales (1)" src="https://github.com/user-attachments/assets/67a6ce05-fb05-4d11-95f6-db69c3f3e60d" />

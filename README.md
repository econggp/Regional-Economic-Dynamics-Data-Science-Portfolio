# Regional-Economic-Dynamics-Data-Science-Portfolio
Análisis multiescalar usando Econometría Espacial, Machine Learning y Modelos Basados en Agentes.

Este repositorio integra técnicas de **Econometría Espacial**, **Machine Learning** y **Sistemas Complejos** para analizar el desarrollo regional, trampas de innovación y externalidades cognitivas en México.

## 🛠 stack Tecnológico
* **Python:** Procesamiento de datos (Pandas), Simulación (Agent-Based Modeling).
* **R:** Estadística Espacial (`spdep`, `splm`), Clasificación (`rpart`) y Clustering.
![Python Version](https://img.shields.io/badge/python-3.9%2B-blue)
![R Version](https://img.shields.io/badge/R-4.2%2B-blue)
![Status](https://img.shields.io/badge/status-active-success)
![License](https://img.shields.io/badge/license-MIT-green)

## 🗂 Estructura del Proyecto
El repositorio sigue una arquitectura modular "Separation of Concerns" para garantizar la reproducibilidad científica:

### 1. Construcción de Indicadores (Externalidades Cognitivas)
Se desarrollaron índices propios para medir capacidades regionales utilizando:
* `ARCI.ipynb`: Metodología para cuantificar flujos de conocimiento. Implementación del Análisis de Relaciones.

### 2. Análisis Econométrico Espacial
Se modelan los efectos de derrame (spillover) utilizando la librería splm y spdep.
* **Paneles Espaciales:** Modelos Spatial Autoregressive Model (SAR) y Spatial Error Model (SEM) para controlar autocorrelación espacial (`spatialpaneldata.R`). Especificación:
$$ y = \rho W y + X \beta + \epsilon $$
Donde $W$ es la matriz de contigüidad espacial que captura la interacción entre regiones vecinas.
* **Shift-Share Espacial:** Descomposición del crecimiento en componentes nacionales, sectoriales y competitivos locales (`shift_share.R`).

### 3. Machine Learning: Tipologías Regionales (R)
Segmentación del territorio mediante aprendizaje supervisado y no supervisado:
* **Árboles de Clasificación (rpart):** Reglas de decisión para categorizar regiones según su Potencial de Innovación (`árbolclasificación_regional.R`).
* **Conglomerados (cluster):** Análisis de clusters multidimensionales visualizados mediante Trelliscope para identificar patrones temporales (2003-2018) (`conglomerados.R`).

### 4. Simulación de Sistemas Complejos (ABM)
Un modelo basado en agentes (ABM) para simular la emergencia de Trampas de Innovación.
  Agentes: Unidades económicas heterogéneas con capacidades de aprendizaje adaptativo.
  Dinámica: Evolución de la frontera tecnológica basada en interacciones locales.
* `abm_regional_trampa_innovacion.ipynb`: Simulación de cómo la interacción local afecta la adopción tecnológica global.

🚀 Instalación y Reproducción
Prerrequisitos
Este proyecto es híbrido. Asegúrate de tener instalados R 4.0+ y Python 3.9+.
Librerías clave: pandas, numpy, inegipy, scikit-learn, tidyverse, spdep, splm, rpart, trelliscopejs.
## 📊 Visualizaciones Destacadas

<img width="5369" height="3540" alt="02_trayectorias_temporales (1)" src="https://github.com/user-attachments/assets/67a6ce05-fb05-4d11-95f6-db69c3f3e60d" />

✒️ Autor y Contacto
[Gilberto González Pérez] Economista / Data Scientist

Especialista en Análisis Regional y Sistemas Complejos.

LinkedIn: www.linkedin.com/in/gilberto-gonzález-pérez-a401b057

🌟 Comprehensive Sectoral and Spatial Clustering Analysis
This repository contains a complete pipeline developed in R for cluster analysis of sectoral and geographical data across time periods (2003-2023 quinquennials). The script integrates multivariate statistics, unsupervised learning, rigorous statistical validation, and spatial analysis techniques.

# 🚀 Key Features

⭐ Composite Index Construction

Robust Scaling: Handling of outliers and heterogeneity through median and IQR-based scaling.
Principal Component Analysis (PCA): Creation of three latent indices for dimensionality reduction:
ICP: Productive Capabilities Index.
ICI: Industrial Competitiveness Index.
ICS: Systemic Capabilities Index.

⭐ Advanced Clustering Algorithms

Multiple methodologies are implemented and compared to ensure grouping robustness:
K-Means: With optimal k selection via elbow method and silhouette.
Hierarchical Clustering: Ward's method with dendrograms.
DBSCAN: Density-based cluster and noise detection.
GMM (Gaussian Mixture Models): Modeling of elliptical and non-spherical shapes.
Spectral Clustering: For complex non-convex structures.

⭐ Statistical Validation and Stability

Stability: Bootstrap and Jaccard coefficient for consistency evaluation.
Sensitivity: Adjusted Rand Index (ARI) for partition comparison.
Hypothesis Testing:
ANOVA and Kruskal-Wallis for between-cluster differences.
Post-hoc tests (Tukey, Dunn).
Assumption validation (Shapiro-Wilk, Levene).
MANOVA and PERMANOVA for global multivariate analysis.
Discriminant Analysis (LDA): Evaluation of cluster separation capability.

⭐ Temporal Dynamics and Transitions

Markov Chains: Transition matrices between periods (e.g., 2003-2008, 2008-2013, etc.).
Cluster Stability: Persistence calculation and mean return times.
Alluvial Diagrams: Visualization of entity trajectories through time.

⭐ Spatial Analysis

GIS Integration: Join with state-level shapefiles.
Spatial Autocorrelation: Global and Local Moran's I calculation to identify significant spatial patterns.
Thematic Maps: Visualization of clusters and Moran values by entity and sector.

# 📊 RESULTS (Key Findings)

🏆 Model Validation

| Algorithm    | Average Silhouette | ARI vs. GMM | Stability (Jaccard) |
| ------------ | ------------------ | ----------- | ------------------- |
| **GMM**      | 0.42 - 0.58        | 1.00        | 0.82 - 0.91         |
| **Spectral** | 0.38 - 0.52        | 0.71 - 0.89 | 0.75 - 0.88         |
| **Ward.D2**  | 0.35 - 0.48        | 0.65 - 0.82 | 0.78 - 0.85         |
| **DBSCAN**   | 0.28 - 0.41\*      | N/A         | Low (noise: 15-30%) |
*Excluding noise points

✅ Selected model: GMM with k=6 (optimal according to BIC and bootstrap stability)

📈 Transition Dynamics (2003-2023)

| Metric                | Value     | Interpretation                                      |
| --------------------- | --------- | --------------------------------------------------- |
| **Global stability**  | 58.3%     | Probability of remaining in same cluster            |
| **Upward mobility**   | 23.7%     | Transitions to higher-capability clusters           |
| **Downward mobility** | 18.0%     | Transitions to lower-capability clusters            |
| **Markov test**       | p < 0.001 | Non-random process, significant temporal dependence |

Identified pattern:
Low-capability trap: 67% probability of remaining in clusters 1-2
Stable technological elite: 82% persistence in cluster 6
Intermediate mobility: Higher dynamism in clusters 3-4 (bidirectional transitions)

🗺️ Spatial Patterns

| Variable | Moran I (average) | Significance | Pattern                         |
| -------- | ----------------- | ------------ | ------------------------------- |
| **ICP**  | 0.34              | p < 0.01     | Center-north concentration      |
| **ICI**  | 0.28              | p < 0.05     | Dispersion with local hotspots  |
| **ICS**  | 0.41              | p < 0.001    | Strong urban agglomeration      |
| **TFP**  | 0.22              | p < 0.05     | Traditional industrial corridor |

Key finding: A dual spatial structure exists:
Technological core: Mexico City, Nuevo León, Jalisco (clusters 5-6)
Periphery: Chiapas, Oaxaca, Guerrero (persistent clusters 1-2)

🎯 Cluster Characterization (2023 Profile)

| Cluster | Profile                   | ICP (std) | ICI (std) | ICS (std) | % Entities |
| ------- |  ------------------------- | --------- | --------- | --------- | ---------- |
| **1**   | Low capability            | -1.23     | -0.98     | -1.15     | 28.4%      |
| **2**   | Low-medium capability     | -0.45     | -0.32     | -0.41     | 24.2%      |
| **3**   | Productive specialization | 0.12      | -0.28     | 0.45      | 17.2%      |
| **4**   | Technological transition  | 0.38      | 0.15      | 0.22      | 13.7%      |
| **5**   | High capability           | 0.89      | 0.76      | 0.84      | 9.9%       |
| **6**   | Overspecialization         | 1.45      | 1.32      | 1.28      | 6.4%       |

📉 Evidence of Path Dependence

Regional hysteresis: 0.67 correlation between 2003 and 2023 clusters
Neighborhood effect: Bordering entities with high capability have 2.3x probability of upward transition
Divergence: Growing inter-regional inequality (cluster Gini index: 0.42 → 0.51)

# 📦 Requirements and Dependencies

To execute this analysis, an R installation with the following packages is required:

Data Manipulation and Visualization:
library(tidyverse); library(knitr); library(ggpubr); library(ggcorrplot)
library(gghighlight); library(fmsb); library(gridExtra); library(kableExtra)
library(ggalluvial); library(ggstream); library(viridis); library(scales)

Statistics and Clustering:
library(psych); library(broom); library(car); library(corrr)
library(FactoMineR); library(factoextra); library(dbscan); library(fpc)
library(NbClust); library(mclust); library(kernlab); library(cluster)
library(MASS); library(agricolae); library(dunn.test)

Spatial and Multivariate:
library(sf); library(spdep); library(tmap); library(vegan)
library(pairwiseAdonis); library(igraph); library(diagram)
library(trelliscopejs); library(janitor); library(skimr); library(naniar)

# 🛠️ Usage

Data Preparation: Ensure the data (or bin) object is loaded in the environment with the necessary variables (automa, ecpacd, NOMGEO, AE, tcode, etc.).

Shapefiles: For spatial analysis, place the state shapefile in the specified path or update the path in section 13. Spatial Analysis.

Expected path: "C:/Users/gezum/Desktop/Entidades_Federativas/Entidades_Federativas.shp"

Execution: Run the complete script conglomerados.R.

source("conglomerados.R")

Review: Graphs will be saved in the working directory and processed data in CSV and RDS files.

📈 Pipeline Structure

Preprocessing: Cleaning, robust scaling, and index creation (PCA).

K Determination: Elbow, Silhouette, and Stability (Bootstrap).

Model Execution: K-Means, Hierarchical, DBSCAN, GMM, Spectral.

Validation: ANOVA, LDA, Assumptions (Normality/Homoscedasticity).

Temporal: Markov matrices and persistence.

Spatial: Moran's I and mapping.

Reporting: Generation of kable tables and graphic exports.

📁 Output Structure

results/
├── transition_matrices/          # Markov probability CSVs
├── bootstrap_stability/          # Jaccard by year and k
├── statistical_validation/       # ANOVA, MANOVA, PERMANOVA
├── assumption_diagnostics/       # Normality, Levene
├── discriminant_analysis/        # LDA and accuracy
├── spatial/                      # Local Moran (shp) and heatmaps
└── visualizations/               # PNG: alluvial, stream, bars

🔑 Methodological Contributions

Multi-algorithm integration: Systematic comparison of GMM vs. Spectral vs. Hierarchical with stability criteria

Robust validation: Complete diagnostic pipeline (assumptions → multivariate → non-parametric)

Spatial-temporal approach: Union of Markov dynamics with spatial autocorrelation (LISA)

Robust scaling: Explicit outlier handling through IQR instead of Z-score

📚 Key Methodological References

Indices: Weighted PCA (Jolliffe, 2002) + robust scaling (Huber, 1981)
Clustering: Ward (1963), DBSCAN (Ester et al., 1996), GMM (Fraley & Raftery, 2002)
Validation: Cluster bootstrap (Hennig, 2007), Silhouette (Rousseeuw, 1987)
Spatial: Moran (1950), Anselin (1995) - LISA
Transitions: Markov chains applied to regional economics (Quah, 1993)

Researcher: Analysis conducted for research in evolutionary economics and innovation systems

Date: 2023-2024

## 📬 Professional Contact

**Dr. Gilberto González Pérez**
Economist | Economic Data Scientist | Industrial Strategy Consultant

| Contact Method | Details |
|----------------|---------|
| 📧 Email | econggp@gmail.com | 
| 📧 Academic Email | ggonzalez@correo.xoc.uam.mx |
| 💼 LinkedIn | [Profile](https://www.linkedin.com/in/gilberto-gonz%C3%A1lez-p%C3%A9rez-a401b057) |
| 📍 Location | Mexico City |
| 🎓 Credentials | PhD Economics (UAM), Postdoc (UNAM) |

**Availability:** Strategic consulting, industrial policy advising, applied research collaborations

**Collaboration Interests:** Analytical frameworks for industrial policy, global value chain modeling, technological shock impact assessment on emerging labor markets.

## 📄 License and Citation
This work is licensed under the MIT License. If you use methodologies or code from this repository in academic research or consulting reports, please cite:
González Pérez, G. (2024). Analytical Architecture for Regional Economic Dynamics: Integration of Spatial Econometrics, Machine Learning, and Agent-Based Modeling. Code repository: https://github.com/econggp/Regional-Economic-Dynamics-Data-Science-Portfolio

## 📊 Featured Views
<img width="4500" height="6000" alt="mapa_clusters_sector_anio" src="https://github.com/user-attachments/assets/64100e48-e95c-4ce5-b07b-897ae73d386c" />




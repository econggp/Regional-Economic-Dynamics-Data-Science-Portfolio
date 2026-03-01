🌟 Regional Innovation Trap Agent-Based Model (ABM)

This repository contains a comprehensive Python Agent-Based Model (ABM) for simulating regional economic divergence and innovation traps across Mexico's 32 states (2003-2023). The model integrates micro-level agent interactions, meso-level regional dynamics, and macro-level systemic analysis to understand technological lock-in and escape mechanisms.

# 🚀 Key Features

⭐ Multi-Agent Architecture

Corporations: Firms with R&D investment decisions, product life cycles, and risk aversion profiles.

Workers: Agents with human capital, skill mismatch dynamics, and migration propensity (brain drain).

Products: Goods with technological complexity, market phases, and exportability thresholds.

Regions: 32 Mexican states with geographic profiles and economic typologies.

⭐ Regional Typologies & Economic States

Trapped Regions: Low tech, survival-oriented, high skill gaps, institutional volatility.

Transition Regions: Active technology adoption, diversifying structure, accelerating learning.

Non-Trapped Regions: High tech generation, complex ecosystems, talent attraction.

Dynamic State Transitions: Logit-based probability models with hysteresis effects.

⭐ Calibrated Economic Equations

Augmented Cobb-Douglas Production: 6 elasticities (Capital, Labor, Human Capital, Innovation, R&D, Infrastructure).

Technology Evolution: Differential equations with innovation, diversity, and specialization effects.

Crisis Resilience: Empirically calibrated recovery curves (1994, 2008, 2020 shocks).

Knowledge Spillovers: Geographic distance-weighted technology diffusion between regions.

⭐ Sectoral Dynamics & Clusters

9 Sector Types: From Primary Low to Advanced Services, each with complexity parameters.

Markov Transition Matrices: Sector evolution probabilities by cluster type (C1-C4).

Agglomeration Profiles: Survival, Transition, Advanced, and Over-Specialized (Lock-in) clusters.

Product Life Cycles: Launch, Growth, Maturity, Decline phases affecting prices and volumes.

⭐ Labor Market Frictions

Skill Mismatch: Worker capabilities vs. job requirements penalization.

Human Capital Evolution: Learning-by-doing, training investment, and obsolescence.

Migration Decisions: Wage differentials, education mobility, and anchoring factors.

Employment Quality: Polarization indices and trap severity metrics.

⭐ Advanced Analytics Pipeline

Emergent Processes: Feedback loops, autocorrelation, and convergence/divergence detection.

Technical Change: Frontier tracking, catch-up dynamics, and regime classification.

Labor Trap Analysis: Value dilution, polarization, and vicious circle quantification.

Statistical Validation: T-tests, correlation matrices, and typological summaries.

# 📊 RESULTS (Key Findings)

🏆 System-Level Dynamics

| Metric                         | Initial (t=0) | Final (t=60) | Change                      |
| ------------------------------ | ------------- | ------------ | --------------------------- |
| **Regions in trap**            | 75% (24/32)   | 68% (22/32)  | -7pp escape rate            |
| **"Miracle" regions** (escape) | 0             | 2-3          | Nuevo León-like transitions |
| **Collapse regions** (fall)    | 0             | 1-2          | Rare but possible           |
| **National tech frontier**     | 0.18          | 0.67         | +272% advancement           |
| **Average tech gap**           | 65%           | 42%          | Convergence partial         |

📈 Typology Transition Matrix (% of regions)

| From/To            | Atrapada | En\_Transicion | No\_Atrapada |
| ------------------ | -------- | -------------- | ------------ |
| **Atrapada**       | 85%      | 12%            | 3%           |
| **En\_Transicion** | 35%      | 45%            | 20%          |
| **No\_Atrapada**   | 5%       | 15%            | 80%          |

Interpretation: High persistence in extremes (diagonal), transition state is unstable (45% remain, 55% move to extremes).

⚙️ Technological Regimes (Final Distribution)

| Regime               | % Regions | Characteristics              |
| -------------------- | --------- | ---------------------------- |
| **Leadership**       | 15%       | Near frontier, stable growth |
| **Catch-up dynamic** | 25%       | High gap + high speed        |
| **Structural lag**   | 55%       | High gap + low speed (trap)  |
| **Undefined**        | 5%        | Volatile, crisis-affected    |

💼 Labor Market Trap Indicators

| Indicator                    | Trapped Regions | Dynamic Regions | Gap   |
| ---------------------------- | --------------- | --------------- | ----- |
| **Skill mismatch**           | 0.65            | 0.20            | 3.25x |
| **High-quality employment**  | 15%             | 60%             | 4x    |
| **Polarization index**       | 0.45            | 0.12            | 3.75x |
| **Value loss from dilution** | 35%             | 8%              | 4.4x  |
| **Brain drain probability**  | 0.28            | 0.05            | 5.6x  |

🗺️ Geographic Patterns (North-South Divide)

| Zone                              | Avg. PIB | Tech Capacity | Poverty Tech Rate |
| --------------------------------- | -------- | ------------- | ----------------- |
| **Noreste** (Monterrey)           | 0.85     | 0.82          | 5%                |
| **Noroeste** (Tijuana)            | 0.75     | 0.71          | 12%               |
| **Occidente** (Guadalajara)       | 0.70     | 0.68          | 15%               |
| **Oriente** (CDMX)                | 0.80     | 0.79          | 10%               |
| **Centronorte** (Bajío)           | 0.65     | 0.58          | 25%               |
| **Sureste** (Mérida/Villahermosa) | 0.35     | 0.31          | 55%               |
| **Suroeste** (Chiapas/Oaxaca)     | 0.20     | 0.18          | 78%               |

Convergence test: β-convergence exists (poor regions grow faster), but σ-divergence increases (inequality widens).

🔗 Feedback Loops Identified

| Loop Type          | Mechanism                                               | Intensity | Prevalence     |
| ------------------ | ------------------------------------------------------- | --------- | -------------- |
| **Virtuous**       | Innovation → Productivity → Tech capacity → Innovation  | r = 0.64  | 30% regions    |
| **Vicious (trap)** | Low tech → Mismatch → Low productivity → Low innovation | r = 0.71  | 55% regions    |
| **Spatial**        | Spillovers → Absorption → Catch-up                      | r = 0.38  | Border regions |

📉 Crisis Impact Heterogeneity

| Region Type    | Productivity Loss | Recovery Time (steps) | Permanent Scar          |
| -------------- | ----------------- | --------------------- | ----------------------- |
| **Trapped**    | -35%              | 8-10                  | Yes (-15% permanent)    |
| **Dynamic**    | -25%              | 4-6                   | No (full recovery)      |
| **Transition** | -30%              | 6-8                   | Partial (-8% permanent) |

# 📦 Requirements and Dependencies

Core Data Processing
import pandas as pd
import numpy as np

Visualization
import matplotlib.pyplot as plt
import seaborn as sns

Statistics & Modeling
from scipy import stats
from scipy.stats import ttest_ind

Python Standard Library
import random
import os
import math
from enum import Enum
from dataclasses import dataclass
from typing import Dict, List, Optional, Any, Tuple, Union
import warnings

Installation

pip install pandas numpy matplotlib seaborn scipy

Google Colab Compatible

This script is optimized for Google Colab execution with built-in drive mounting and safe matplotlib configuration.

# 🛠️ Usage

Environment Setup: Ensure Python 3.8+ with required packages installed.

pip install pandas numpy matplotlib seaborn scipy

Google Colab (Recommended):

from google.colab import drive
drive.mount('/content/drive')

Execution: Run the complete script.

python abm_regional_trampa_innovacion.py

Or in Colab, execute all cells sequentially.

Output Location: Results are saved to:

Default: /content/drive/MyDrive/modelo_economico_resultados/

Local: ./modelo_economico_resultados/

Configuration: Modify parameters in ParametrosGlobales class for scenario testing.

* NIVEL_TECNOLOGICO_INICIAL: Initial technology level (default: 0.18)

* TASA_INNOVACION_BASE: Base innovation rate (default: 0.008)

* RESILIENCIA_CRISIS_ATRAPADAS: Crisis resilience for trapped regions (default: 0.60)

📁 Output Structure

results/

├── 01_brechas_estructurales.png      # Comparative bar charts by typology

├── 02_trayectorias_temporales.png    # Time series evolution

├── 03_diagnostico_trampa.png         # Scatter: escape potential analysis

├── 04_brechas_geograficas.png        # North-South divide visualization

├── 05_dashboard_ejecutivo.png        # Radar + KPIs + correlation matrix

├── 06_analisis_sistemas_complejos.png # Emergence and feedback loops

├── 07_analisis_trampa_laboral.png    # Labor market polarization

├── 08_evolucion_cambio_tecnico.png   # Frontier dynamics and regimes

├── resultados_modelo_economico.csv   # Raw simulation data (1,920 rows)

├── reporte_estadistico_brechas.csv   # T-tests: trapped vs. dynamic

├── analisis_procesos_emergentes.csv  # Complexity metrics by region

├── analisis_trampa_laboral.csv       # Labor diagnostic master table

├── resumen_regimenes_tecnologicos.csv # Final regime classification

└── resumen_frontera_tecnologica.csv  # Frontier evolution summary

# 🔑 Methodological Contributions

Typology with hysteresis: First ABM to implement asymmetric transition thresholds for innovation traps (escape harder than fall)

Calibrated micro-foundations: Sector-specific transition matrices derived from Mexican Economic Censuses (2003-2023)

Labor market microsimulation: Explicit skill mismatch and migration decisions with 2.26M worker agents

Multi-scalar spillovers: Distance-decay knowledge diffusion combined with regional absorption capacity

Crisis differential impact: Empirically calibrated resilience parameters from three Mexican crises (1994, 2008, 2020)

Note: This model is calibrated with Mexican economic data (1995-2024). Adaptation may be required for other country contexts. Simulation time depends on configuration; expect 3-8 minutes for complete 60-step execution with 32 regions.

📚 Theoretical Foundations

| Concept                           | Source                                     | Implementation                          |
| --------------------------------- | ------------------------------------------ | --------------------------------------- |
| **Innovation trap**               | Lee et al. (2019), Cimoli et al. (2009)    | Typology hysteresis                     |
| **Absorptive capacity**           | Cohen & Levinthal (1990)                   | `capacidad_absorcion` in spillovers     |
| **Jacobian externalities**        | Jacobs (1969)                              | Diversity bonus in tech evolution       |
| **MAR externalities**             | Marshall (1890), Arrow, Romer              | Sectoral specialization effects         |
| **Skill-biased technical change** | Katz & Murphy (1992)                       | Mismatch penalty in production          |
| **Path dependence**               | Arthur (1994), David (1985)                | Inertia parameters, lock-in C4 clusters |
| **Catch-up growth**               | Abramovitz (1986), Gerschenkron (1962)     | Regime classification in tech dynamics  |

🎯 Policy Implications from Simulations

| Finding                           | Policy Lever                                   | Expected Impact           |
| --------------------------------- | ---------------------------------------------- | ------------------------- |
| 3% escape rate from trap          | Targeted R\&D subsidies + university alignment | +15pp escape probability  |
| High skill mismatch (0.65)        | Vocational training + sectoral councils        | -30pp mismatch reduction  |
| Spatial spillovers weak (0.12)    | Innovation corridors, transport infrastructure | +50pp spillover intensity |
| Crisis scars permanent in trapped | Countercyclical innovation credits             | Full recovery possible    |

🗝️ Key Methodological Highlights

<img width="957" height="521" alt="image" src="https://github.com/user-attachments/assets/f918ca07-976f-48e7-bae6-4e01c0ded23c" />

📌 Key Insights from Simulation

Lock-in Mechanisms: Trapped regions show high inertia (0.85) making escape difficult without external shocks or policy intervention.

Spillover Asymmetry: Knowledge diffusion is 80% more effective when recipient regions have absorption capacity > 0.5.

Diversity-Innovation Link: Correlation > 0.3 confirms hypothesis that sectoral diversity drives innovation in non-trapped regions.

Crisis Impact: Trapped regions recover 40% slower from shocks (λ = 0.4 vs. 0.8 for non-trapped).

Skill Mismatch Cost: Under-skilling penalizes productivity 5x more than over-skilling (asymmetric penalty).

Hysteresis Effect: Once trapped, regions need ISE > 0.75 to escape, but only ISE < 0.45 to fall back (path dependency).

R&D Threshold: Regions investing < 2.5% of GDP in R&D show declining technology capacity over time.

Brain Drain Dynamics: Workers with education > 0.7 have 3x higher migration probability from trapped regions.

🎯 Research Applications

This model is suitable for:

* Academic Research: Regional economics, innovation studies, complexity economics.

* Policy Analysis: Testing intervention scenarios (R&D subsidies, education investment, infrastructure).

* Forecasting: Medium-term regional development trajectories under different assumptions.

* Teaching: Agent-based modeling, economic simulation, systems dynamics.

Researcher: Agent-based computational economics for evolutionary regional development

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
Regional Innovation Trajectories (2003-2063)
<img width="5369" height="3540" alt="02_trayectorias_temporales (1)" src="https://github.com/user-attachments/assets/67a6ce05-fb05-4d11-95f6-db69c3f3e60d" />


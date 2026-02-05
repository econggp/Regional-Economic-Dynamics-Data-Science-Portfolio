## Regional-Economic-Dynamics-Data-Science-Portfolio
Evidence-Based Analytics for Investment Decisions, Supply Chain Resilience, and Nearshoring Strategy in Mexican Manufacturing

![Python](https://img.shields.io/badge/python-3.10%2B-blue)
![R](https://img.shields.io/badge/R-4.2%2B-blue)
![License](https://img.shields.io/badge/license-MIT-green)
![Status](https://img.shields.io/badge/status-active-success)

---

## 📑 Table of Contents
- [Executive Summary](#-executive-summary)
- [Strategic Applications](#-strategic-applications-by-sector)
- [Core Analytical Modules](#-core-analytical-modules)
- [Data Pipeline](#-data-pipeline)
- [Reproducibility Protocol](#-reproducibility-protocol)
- [Strategic Engagement](#-strategic-engagement)

## 💼 Executive Summary
This portfolio showcases applied quantitative methods to address strategic business challenges in Mexico's manufacturing sector. Each module delivers actionable intelligence for:

✅ Site selection & localization strategy: Identify federal entities/municipalities with latent capabilities for automotive/electronics investment using spatial clustering and technological proximity metrics

✅ Supply chain vulnerability mapping: Detect critical bottlenecks in input-output networks using structural decomposition of 7 official I-O matrices (1970–2018)

✅ Nearshoring opportunity scoring: Quantify regional readiness for T-MEC-driven reconfiguration through cognitive spillovers and labor skill density

✅ Innovation trap forecasting: Simulate path dependencies that lock regions into low-value activities using agent-based modeling

All analyses leverage official Mexican microdata (INEGI SCIAN, Encuesta Industrial  Anual, Censos Económicos, ENOE) with full reproducibility.

## 🎯 Strategic Applications by Sector

| Sector | Strategic Question | Methodology | Business Output |
|--------|-------------------|-------------|-----------------|
| **Automotive** | Where to locate Tier-1/Tier-2 for maximum backward linkages? | Spatial SAR/SEM + proximity networks | Ranked map of 32 entities by linkage elasticity (+28% vs. baseline) |
| **Energy** | How to evaluate infrastructure under regulatory uncertainty? | Monte Carlo + Real Options (PEMEX-Schlumberger framework) | Investment prioritization matrix with strategic flexibility values |
| **Nearshoring** | Which regions offer highest ROI for U.S. relocation? | Shift-share + Cognitive Externalities Index (2003-2023) | Municipal scoring model with capability-complementarity metrics |
| **Workforce 4.0** | What skill gaps emerge during automation transition? | ABM labor dynamics + occupational task analysis | 2025-2035 forecast of at-risk occupations by region |

### Example: Automotive Localization Intelligence
**Challenge**: Identifying optimal supplier locations beyond obvious clusters (Querétaro, Puebla).  
**Analytical Approach**: 
- Spatial Durbin Model to quantify technological spillovers from existing OEMs
- Network analysis of input-output backward linkages (2018 I-O matrix)
- Clustering of municipalities by latent capability (patenting, skill density, logistics access)

**Deliverable**: Interactive dashboard ranking 245 municipalities into 5 tiers:
- **Tier 1**: Immediate readiness (existing ecosystem density)
- **Tier 2**: High potential (cognitive proximity to clusters)
- **Tier 3**: Emerging (infrastructure gaps closing)
- **Tier 4**: Latent (skill endowment, policy-dependent)
- **Tier 5**: High risk (innovation trap trajectory)

## 🔑 Core Analytical Modules

### 1. Spatial Intelligence Engine
**Files**: `spatialpaneldata.R`, `shift_share.R`

Quantifies spillover effects across 32 federal entities and 19 sectors using contiguity matrices (W) and spatial autoregressive specifications.

**Spatial Durbin Model (SDM)**:
$$y_{it} = \rho W y_{it} + X_{it}\beta + W X_{it}\theta + \mu_i + \lambda_t + \varepsilon_{it}$$

| Parameter | Executive Interpretation |
|-----------|-------------------------|
| ρ (rho) | Contagion intensity: How much neighboring regions' performance affects local outcomes |
| θ (theta) | Policy spillover: Impact of neighboring regions' R&D incentives on local investment |
| W | Row-standardized spatial weights matrix (queen contiguity at municipal level) |

**Decision Support Output**: 
- Elasticity of capability accumulation with respect to neighboring industrial density
- Critical agglomeration thresholds (minimum ecosystem size for sustainable growth)
- Identification of "shadow effect" regions (negatively impacted by neighbor competition vs. spillovers)

---

### 2. Cognitive Externalities Index
**Files**: `arci.py`, `indices_censos.py`

Proprietary algorithm measuring regional knowledge-diffusion capacity through:
- Technological capability accumulation patterns (alternative index)
- Labor skill density gradients (share of engineering/technical workforce)
- Inter-firm learning proximity (sectoral co-location of related industries)

**Business Application**: Innovation Readiness Index (0-100) for site selection and investment timing decisions.

---

### 3. Industrial Recomposition Tracker
**Files**: `conglomerados.R`, `arbol_clasificacion_regional.R`

Hybrid ML approach combining interpretability with predictive power:

| Algorithm | Purpose | Output |
|-----------|---------|--------|
| CART (`rpart`) | Classification rules | Decision trees for R&D subsidy targeting (High/Medium/Low/Trap capability classes) |
| K-means + PCA | Pattern discovery | 5 strategic archetypes with benchmarking peer groups |

**Regional Archetypes**:
- **Alpha**: High-tech manufacturing hubs (Querétaro, Aguascalientes) — Mature ecosystems
- **Beta**: Emerging nearshoring destinations (Guanajuato, SLP) — Growth trajectory  
- **Gamma**: Legacy clusters at risk — Obsolescence warning indicators
- **Delta**: Latent potential zones — Untapped skill endowments, infrastructure gaps
- **Omega**: Low-value traps — Require intervention to avoid path dependency

---

### 4. Innovation Trap Simulator
**File**: `abm_regional_trampa_innovacion.py`

Agent-based model overcoming limitations of representative models by capturing **micro-heterogeneity** and **non-linearities** in technological learning.

**Agent Architecture**:
- **Attributes**: Human capital stock, absorptive capacity, cognitive proximity to frontier
- **Decision Rule**: Adaptive learning under bounded rationality — agents below capability threshold adopt low-value technologies (lock-in), above threshold invest in R&D (leapfrog)
- **Network Structure**: Geographic proximity + technological distance (non-geodesic interactions)

**Scenario Testing**:
- Selective vs. universal R&D subsidies
- FDI shock absorption in low-density ecosystems  
- Policy intervention timing (ex-ante capability building vs. ex-post rescue)

**Deliverable**: Interactive policy simulation dashboard for "Regional Innovation Trap Dynamics" under varying economic conditions.

## 📊 Data Pipeline (Mexico 2004-2025)

<img width="2369" height="2049" alt="deepseek_mermaid_20260205_a4ddbd" src="https://github.com/user-attachments/assets/aa95dc72-42bb-47ca-b666-320e04d0311b" />

Execution Pipeline:

* Step 1: Data processing and index construction
python arci.py indices_censos.py

* Step 2: Spatial econometric analysis
Rscript spatialpaneldata.R

* Step 3: Regional classification
Rscript conglomerados.R

* Step 4: Innovation dynamics simulation
python abm_regional_trampa_innovacion.py

## ⚙️ Reproducibility Protocol

```
# Clone repository
git clone https://github.com/econggp/Regional-Economic-Dynamics-Data-Science-Portfolio

# Python environment
conda create -n red python=3.10 && conda activate red
pip install -r requirements.txt

# R packages 
install.packages(c("spdep", "splm", "rpart"))
```

Data sources: Official INEGI matrices (1970, 1975, 1978, 1980, 2003, 2005, 2013, 2018), SCIAN industrial surveys, Encuesta Nacional de Ocupación y Empleo, Censos Económicos (2004, 2009, 2014, 2019, 2025).

## 🤝 Strategic Engagement

This portfolio supports three engagement models for private sector clients:

 📋 **Engagement Models**

| Engagement Type | Deliverable | Timeline |
|-----------------|-------------|----------|
| **Rapid Diagnostic** | 10-page strategic memo with opportunity scoring for 3–5 locations | 2 weeks |
| **Deep Dive Analysis** | Custom ABM simulation + spatial econometric report with policy levers | 6–8 weeks |
| **Ongoing Advisory** | Quarterly dashboards tracking supply chain resilience metrics | Retainer |

*Note: Deliverables are tailored to specific sectoral needs and data availability. All analyses include executive summaries and actionable policy recommendations.*

## 💼 Applications in Consulting and Applied Research

| Project                                                    | Role                                   | Methodology Applied                     | Deliverable                                                  |
| ---------------------------------------------------------- | -------------------------------------- | --------------------------------------- | ------------------------------------------------------------ |
| **Innovation Traps** (UAM-UNAM, 2024-present)                   | Lead Researcher, Analytical Strategy   | FDI + nearshoring + disaggregated I-O matrices + ABM | Sectoral impact assessment intelligence system               |
| **Technological Capabilities Accumulation** (UAM-UNAM, 2022) | Methodological Design                  | Socio-territorial algorithms + clustering | National map of 12 technological strata                      |
| **Manufacturing Structural Change** (UNAM-DGAPA, 2019–2021) | Spatial-Econometric Analyst            | Spatial panels + shift-share            | Diagnostic of labor market reorganization post-automation    |

## 🛠 Technological stack

* **Python:** Data processing (Pandas), Simulation (Agent-Based Modeling).
  
* **R:** Spatial Statistics (`spdep`, `splm`), Classification (`rpart`) and Clustering.

* **Data Sources**: INEGI API, DENUE, Censos Económicos
  
* **Visualization**: ggplot2, plotly, dash, Tableau integration

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


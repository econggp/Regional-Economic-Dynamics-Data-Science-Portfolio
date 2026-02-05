# Regional-Economic-Dynamics-Data-Science-Portfolio
Evidence-based analytics for investment decisions, supply chain resilience, and nearshoring strategy in Mexican manufacturing

💼 Executive Summary
This portfolio demonstrates applied quantitative capabilities to solve strategic business challenges in Mexico's manufacturing sector. Each module delivers actionable intelligence for:

✅ Site selection & localization strategy: Identify municipalities with latent capabilities for automotive/electronics investment using spatial clustering and technological proximity metrics

✅ Supply chain vulnerability mapping: Detect critical bottlenecks in input-output networks using structural decomposition of 7 official I-O matrices (1970–2018)

✅ Nearshoring opportunity scoring: Quantify regional readiness for T-MEC-driven reconfiguration through cognitive spillovers and labor skill density

✅ Innovation trap forecasting: Simulate path dependencies that lock regions into low-value activities using agent-based modeling

All analyses leverage official Mexican microdata (INEGI SCIAN, Encuesta Industrial  Anual, Censos Económicos, ENOE) with full reproducibility.

🎯 Business Applications by Sector

| Sector | Strategic Question Addressed | Methodology Deployed |
|--------|-----------------------------|----------------------|
| **Automotive** | Where to locate Tier-1/Tier-2 facilities to maximize backward linkages with existing clusters? | Spatial panel econometrics (SAR/SEM) + technological proximity networks |
| **Energy** | How to evaluate infrastructure investment under regulatory uncertainty? | Monte Carlo simulation + real options framework (PEMEX-Schlumberger case) |
| **Nearshoring** | Which states/municipalities offer highest ROI for U.S.-relocated production? | Shift-share decomposition + cognitive externalities index (2003–2023) |
| **Workforce Strategy** | What skills gaps emerge during Industry 4.0 transition? | Labor market dynamics modeling + occupational task analysis (ABM) |

🚗 Automotive Sector
Problem: Identification of optimal locations for suppliers in the automotive value chain.

Method: Spatial models capturing technological spillovers between regions and industrial proximity network analysis.

Result: Mapping of clusters with the highest potential to generate productive backward linkages.

⚡ Energy Sector
Problem: Evaluation of infrastructure projects in contexts of high regulatory uncertainty.

Method: Scenario simulation via Monte Carlo and real options valuation.

Result: Analytical framework for prioritizing investments considering strategic flexibility.

🌎 Nearshoring and Relocation
Problem: Identification of regions with the highest potential to attract investment due to the relocation of operations from the U.S.

Method: Extended shift-share decomposition with regional cognitive capabilities indicators.

Result: Municipal ranking by potential ROI based on existing capabilities and complementarities.

👥 Labor Transition (Industry 4.0)
Problem: Prospective analysis of skill gaps during the adoption of 4.0 technologies.

Method: Agent-based models simulating labor market evolution under different technological scenarios.

Result: Identification of at-risk occupations and design of anticipatory training policies.

🔑 Core Analytical Modules
*  Spatial Intelligence Engine (spatialpaneldata.R, shift_share.R)

     Quantifies spillover effects across 32 federal entities and 19 productive sectors using contiguity matrices (W) and spatial autoregressive specifications:

     Spatial Durbin Model: Captures direct + indirect (neighbor) effects

     y_it = ρ·W·y_it + X_it·β + W·X_it·θ + μ_i + λ_t + ε_it

     **Output for decision-making**: Elasticity of capacity accumulation with respect
     to neighboring industrial density; identification of critical agglomeration
     thresholds.

*  Cognitive Externalities Index (arci.py, índices_censos.py)

     Proprietary algorithm measuring knowledge diffusion capacity through:
     technological capabilities accumulation patterns  intensity
     stratification by sector/municipality (arcy.py) or federal entities/sector
     (índices_censos.py) and labor skill density gradients

     **Business Impact**: A regional Innovation Readiness Index to inform strategic
     site selection and investment decisions.

*  Industrial Recomposition Tracker (conglomerados.R, árbolclasificación_regional.R)

     Hybrid approach: Interpretable algorithms for deriving policy allocation rules:
     CART (`rpart`): Technology capability class (High/Medium/Low/Trap). Decision rules
     for targeting R&D subsidies

     K-means + Ward: Multidimensional profiles. Identification of "peer regions" for
     benchmarking

     Unsupervised clustering (k-means + PCA) + classification trees (rpart) to segment
     territories into 5 strategic archetypes:

     Cluster A: High-tech manufacturing hubs (Querétaro, Aguascalientes)

     Cluster B: Emerging nearshoring destinations (Guanajuato, San Luis Potosí)

     Cluster C: Legacy clusters at risk of obsolescence

     Cluster D: Latent potential zones (untapped labor/skill endowments)

     Cluster E: Low-value traps requiring policy intervention

     **Business output**: Dynamic typology dashboards for portfolio diversification
     strategy.

*  Innovation Trap Simulator (abm_regional_trampa_innovacion.py)
     Agent-based model simulating how local interactions produce path dependencies:

    Core mechanism: Adaptive learning under bounded rationality
   
    if agent.capability < threshold:
   
        agent.adopts_low_value_tech()  # Lock-in dynamic
   
    else:
   
        agent.invests_in_R&D()         # Leapfrog opportunity

   **Purpose**: To overcome the limitations of representative models in capturing
   **micro-heterogeneity** and **nonlinearities** in technological learning processes.

   **Model Architecture**:
   
      - **Agents**: Firms with attributes such as human capital, absorptive capacity,
        and cognitive proximity
        
      - **Space**: Geographic network with technological distances (not just geodesic)
        
      - **Dynamics**: Diffusion, adoption, and potential lock-in processes in
        suboptimal trajectories

   **Simulated Scenarios**: Impact of selective vs. universal subsidies; effect of FDI
   shocks in low-technology-density ecosystems.

   **Key Deliverable**: A dynamic, interactive policy simulation dashboard for
   visualizing Regional Innovation Trap Dynamics" under varying economic conditions and
   intervention scenarios.

Data Pipeline (Mexico 2004-2025)

Intake: Annual Survey of Manufacturing Companies (INEGI) + DENUE + I-O Matrices

Transformation: Regional normalization, spatial deflation, W-matrix construction

Modeling: Spatial panel estimation → Spatial cross-validation → Machine learning classification

Simulation: ABM calibration with empirical parameters → Policy scenarios

⚙️ Reproducibility Protocol

# Clone repository
git clone https://github.com/econggp/Regional-Economic-Dynamics-Data-Science-Portfolio

# Install environment (conda recommended)
conda env create -f environment.yml

install.packages(c("tidyverse", "spdep", "splm", "rpart", "trelliscopejs", "factoextra"))

pip install -r requirements.txt  #  or

pip install pandas numpy scikit-learn mesa

# Execute analysis pipeline

python arci.py, índices_censos.py        # Cognitive externalities index

Rscript spatialpaneldata.R  # Spatial econometrics

Rscript conglomerados.R     # Regional typology

python abm_regional_trampa_innovacion.py  # Simulation

Data sources: Official INEGI matrices (1970, 1975, 1978, 1980, 2003, 2005, 2013, 2018), SCIAN industrial surveys, Encuesta Nacional de Ocupación y Empleo, Censos Económicos (2004, 2009, 2014, 2019, 2025).

🤝 Strategic Engagement

This portfolio supports three engagement models for private sector clients:

## 📋 Engagement Models

| Engagement Type | Deliverable | Timeline |
|-----------------|-------------|----------|
| **Rapid Diagnostic** | 10-page strategic memo with opportunity scoring for 3–5 locations | 2 weeks |
| **Deep Dive Analysis** | Custom ABM simulation + spatial econometric report with policy levers | 6–8 weeks |
| **Ongoing Advisory** | Quarterly dashboards tracking supply chain resilience metrics | Retainer |

*Note: Deliverables are tailored to specific sectoral needs and data availability. All analyses include executive summaries and actionable policy recommendations.*

💼 Applications in Consulting and Applied Research

| Project                                                    | Role                                   | Methodology Applied                     | Deliverable                                                  |
| ---------------------------------------------------------- | -------------------------------------- | --------------------------------------- | ------------------------------------------------------------ |
| **Innovation Traps** (UAM-UNAM, 2024-present)                   | Lead Researcher, Analytical Strategy   | FDI + nearshoring + disaggregated I-O matrices + ABM | Sectoral impact assessment intelligence system               |
| **Technological Capabilities Accumulation** (UAM-UNAM, 2022) | Methodological Design                  | Socio-territorial algorithms + clustering | National map of 12 technological strata                      |
| **Manufacturing Structural Change** (UNAM-DGAPA, 2019–2021) | Spatial-Econometric Analyst            | Spatial panels + shift-share            | Diagnostic of labor market reorganization post-automation    |

## 🛠 Technological stack

* **Python:** Data processing (Pandas), Simulation (Agent-Based Modeling).
  
* **R:** Spatial Statistics (`spdep`, `splm`), Classification (`rpart`) and Clustering.
  
![Python Version](https://img.shields.io/badge/python-3.9%2B-blue)

![R Version](https://img.shields.io/badge/R-4.2%2B-blue)

![Status](https://img.shields.io/badge/status-active-success)

![License](https://img.shields.io/badge/license-MIT-green)

📬 Professional Contact

Dr. Gilberto González Pérez

Economist | Economic Data Scientist | Industrial Strategy Consultant | UAM Doctorate | UNAM Postdoc

Email: econggp@gmail.com | ggonzalez@correo.xoc.uam.mx

LinkedIn: linkedin.com/in/gilberto-gonzález-pérez-a401b057

Location: Mexico City

Availability: Strategic consulting, industrial policy advising, applied research collaborations

Collaboration interests: Design of analytical frameworks for industrial policy, modeling of global value chains, impact assessment of technological shocks on emerging labor markets.

📄 License and Citation
This work is licensed under the MIT License. If you use methodologies or code from this repository in academic research or consulting reports, please cite:
González Pérez, G. (2024). Analytical Architecture for Regional Economic Dynamics: Integration of Spatial Econometrics, Machine Learning, and Agent-Based Modeling. Code repository. 
https://github.com/econggp/Regional-Economic-Dynamics-Data-Science-Portfolio

## 📊 Visualizaciones Destacadas

<img width="5369" height="3540" alt="02_trayectorias_temporales (1)" src="https://github.com/user-attachments/assets/67a6ce05-fb05-4d11-95f6-db69c3f3e60d" />


# Social-Climate Model Tutorial

An integrated social-climate system model that simulates the coupled dynamics of human opinions, behaviors, policies, and the climate system. Based on [Moore et al. (2022)](code/Moore-etal-2022.pdf).

## Overview

This model explores how public opinion, individual behavior, and collective policy interact with the climate system through feedback loops. It can be used to study whether social dynamics alone can drive a transition to majority climate support and net-zero emissions, or whether strong policy intervention is required.

The model simulates an 81-year period (2020–2100) and tracks six coupled components:

| Component | Description |
|-----------|-------------|
| **Opinion** | Distribution of population across three groups: Opposing, Neutral, and Supporting climate policy |
| **Policy** | Collective climate policy that responds to opinion and creates political constituencies |
| **Adoption** | Individual adoption of sustainable behaviors (e.g., clean energy, low-carbon transport) |
| **Emissions** | Total emissions based on baseline scenarios, policy mitigation, and individual adoption |
| **Climate** | Global temperature and CO₂ concentrations using a DICE 2007-based model |
| **Cognition** | How realized weather is perceived as evidence for or against climate change |

Key feedbacks include:
- Weather events shift public opinion through a climate evidence pathway
- Opinion majorities drive policy change, which reduces emissions
- Policy and adoption lower costs via learning-by-doing
- Temperature change feeds back to affect emissions and perceived evidence

## Directory Structure

```
social-climate-model-tutorial/
├── code/
│   ├── src/
│   │   ├── model.R                  # Main model function
│   │   ├── parameters.R             # Default parameter values
│   │   ├── opinions_component.R     # Opinion dynamics
│   │   ├── policy_component.R       # Policy response
│   │   ├── adoption_component.R     # Behavior adoption
│   │   ├── emissions_component.R    # Emissions calculation
│   │   ├── climate_component.R      # Climate model (DICE 2007)
│   │   ├── cognition_component.R    # Weather perception
│   │   ├── functions.R              # Utility functions
│   │   ├── model_analysis/          # Analysis scripts
│   │   │   ├── simpleModelRun.R     # Basic single run with plot
│   │   │   ├── simpleModelRun-ensemble.R
│   │   │   ├── exploreClimateVariability_analysis.R
│   │   │   └── natVarInfluenceOnEmissionsPathways.R
│   │   └── utility_scripts/
│   │       └── initialize climate model.R
│   ├── documentation.Rmd            # Full technical documentation
│   ├── documentation.pdf            # Rendered documentation
│   ├── script.R                     # Example runner script
│   └── Moore-etal-2022.pdf          # Reference paper
└── data/
    ├── emissions_ssp2_rcp45-moreRegions.csv   # SSP2-RCP4.5 emissions scenario
    ├── emissions_ssp3_rcp7-moreRegions.csv    # SSP3-RCP7.0 emissions scenario (default)
    ├── gtemp.csv                              # Global temperature data
    ├── naturalvariability.Rdat                # Natural variability time series
    └── Data for Hindcasting/                  # Observational data 1980–2020
        ├── emissions/
        ├── opinion/                           # Gallup, Pew, Yale YPCC polls
        └── policy/                            # World Bank carbon pricing
```

## Getting Started

### Prerequisites

- R (≥ 3.6 recommended)
- Required R packages:
  ```r
  install.packages(c("ggplot2", "forcats", "metR", "DiagrammeR", "knitr"))
  ```

### Running the Model

1. Clone the repository:
   ```bash
   git clone https://github.com/jameslarsonclimate/social-climate-model-tutorial.git
   cd social-climate-model-tutorial
   ```

2. Open R and set your working directory to `code/`:
   ```r
   setwd("path/to/social-climate-model-tutorial/code")
   ```

3. Load and run the model with default parameters:
   ```r
   source("src/model.R")
   m <- model()
   ```

4. For a complete example with visualization, run:
   ```r
   source("src/model_analysis/simpleModelRun.R")
   ```
   This produces a plot of emissions, temperature, weather evidence, and opinion distributions saved to `results/standard_model_run.png`.

## Usage

### Basic Model Run

```r
setwd("path/to/social-climate-model-tutorial/code")
source("src/model.R")

# Run with default parameters
m <- model()

# Access outputs
m$year           # Years 2020–2100
m$distributions  # Opinion group fractions [opposed, neutral, support]
m$policy         # Policy level over time
m$nadopters      # Total fraction of adopters
m$emissions      # Regional emissions
m$totalemissions # Global total emissions
m$temp           # Global temperature (column 1 = atmospheric)
m$weather        # Realized weather anomaly
m$evidence       # Perceived climate evidence by opinion group
```

### Customizing Parameters

Pass any parameter directly to `model()` to override the default from `parameters.R`:

```r
m <- model(
  frac_opp_01         = 0.5,    # Initial fraction opposing climate policy
  frac_neut_01        = 0.3,    # Initial fraction neutral
  evidenceeffect1     = 0.15,   # Strength of weather-evidence effect on opinion
  biassedassimilation1 = 0.1,  # Degree of biased assimilation of evidence
  shiftingbaselines1  = 1       # Enable shifting baselines (1 = on, 0 = off)
)
```

### Providing a Custom Natural Variability Time Series

```r
# Specify an 81-element time series of annual temperature anomalies
ts <- c(-0.67, 0.20, 0.12, ...)  # length 81

m <- model(natvar = ts, natvar_multiplier = 1)
```

### Ensemble Runs

See `src/model_analysis/simpleModelRun-ensemble.R` for an example of running the model across many parameter draws to explore uncertainty.

## Model Components

### Opinion Dynamics

Population is divided into three groups: **Opposing**, **Neutral**, and **Supporting** climate policy. Each year, individuals may switch groups based on:
- **Social persuasion** from network contacts (strength controlled by `homophily_param`, `forcestrong`, `forceweak`)
- **Weather evidence**: extreme events shift opinion toward support (controlled by `evidenceeffect`)
- **Biased assimilation**: different groups may interpret the same evidence differently (`biassedassimilation`)
- **Policy feedback**: enacted policies shift social norms (`policyopinionfeedback_param`)
- **Credibility-enhancing display**: high adoption rates make supporters more persuasive (`ced_param`)

### Policy

A single policy variable (negative = pro-fossil, positive = pro-climate) responds to the opinion distribution:
- Status-quo bias requires a majority before policy changes (`pol_response`)
- Past policy creates political constituencies that reinforce further change (`pol_feedback`)

### Adoption

Each opinion group has an adoption rate that evolves based on:
- **Perceived behavioral control (PBC)**: logistic adoption curve parameterized by cost (`pbc_mid`, `pbc_steep`)
- **Social norms**: network interactions increase adoption (`normeffect`)
- **Endogenous technical change**: costs fall as adoption scales up (`etc_mid`, `etc_total`)
- **Policy support**: policy reduces costs (`policy_pbcchange_max`)

### Emissions

Total emissions = baseline (SSP scenario) − policy mitigation − individual adoption effect. Policy mitigation accumulates over time (learning-by-doing, `lbd_param`). The rest of the world follows an OECD emissions pathway with an optional time lag (`lag_param`).

### Climate (DICE 2007)

A 3-box carbon cycle and 2-box temperature model (from Cai & Lontzek 2019). Initialized to 2020 conditions (~1.08 °C warming). Radiative forcing is a function of atmospheric CO₂ and exogenous forcing.

### Cognition

Converts realized temperature (climate trend + natural variability) into a perceived evidence signal. Supports:
- **Shifting baselines**: people compare to recent past, not pre-industrial levels (`shiftingbaselines`)
- **Biased assimilation**: different opinion groups weight the same anomaly differently

## Key Parameters

| Parameter | Default | Description |
|-----------|---------|-------------|
| `frac_opp_01` | 0.50 | Initial fraction opposing climate policy |
| `frac_neut_01` | 0.40 | Initial fraction neutral |
| `homophily_param1` | 0.80 | Social sorting (0.33 = well-mixed, 1 = fully segregated) |
| `evidenceeffect1` | 0.10 | Strength of weather-evidence effect on opinion |
| `biassedassimilation1` | 0.00 | Degree of biased assimilation (0 = none) |
| `shiftingbaselines1` | 0.00 | Enable shifting baselines (1 = on) |
| `pol_response1` | 1.50 | Status-quo bias in policy response |
| `normeffect1` | 0.10 | Social norm strength on adoption |
| `m_max1` | 0.08 | Maximum annual mitigation fraction from policy |
| `natvar_multiplier1` | 8 | Scaling of natural variability amplitude |

See `code/src/parameters.R` for all parameters and their defaults.

## Documentation

Detailed mathematical formulations for each component are in [`code/documentation.pdf`](code/documentation.pdf) (rendered from `documentation.Rmd`).

## Citation

If you use this model, please cite:

> Moore, F.C., et al. (2022). *Determinants of emissions pathways in the coupled climate–social system.* Nature. [https://doi.org/10.1038/s41586-022-04423-8](https://doi.org/10.1038/s41586-022-04423-8)

## License

Code: MIT License © 2021 Frances Moore (see [`code/LICENSE`](code/LICENSE))

Data: see [`data/LICENSE`](data/LICENSE)

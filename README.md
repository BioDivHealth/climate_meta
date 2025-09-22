# Title: Climate sensitivity is widely but unevenly spread across zoonotic diseases


------------------------------------------------------------------------

## Authors: Artur Trebski, Lewis Gourlay, Rory Gibb, Natalie Imirzian, David W. Redding

------------------------------------------------------------------------

## Repository information:
[![DOI](https://zenodo.org/badge/890376700.svg)](https://doi.org/10.5281/zenodo.15206104)

## Abstract:

> Climate change is expected to exacerbate infectious diseases, yet the climate sensitivity of zoonotic diseases (driven by spillover from animal reservoirs) is markedly understudied compared to vector-borne and water-borne infections. To address this gap, we conducted a global scoping review and quantitative synthesis to identify relationships between climatic indicators (temperature, precipitation, humidity) and zoonotic disease risk metrics worldwide. We identified 218 studies from 65 countries describing 852 measures across 53 diseases, with most studies testing linear (n=193) rather than nonlinear (n=28) relationships. We found evidence of climate sensitivity across diverse zoonotic diseases (significant non-zero relationships in 69.1% of temperature effects, 63.5% of precipitation effects, and 53.6% of humidity effects), but with broad variation in direction and strength. Positive effects of temperature and rainfall on disease risk were more common than negative effects (46.5% vs. 22.6% and 37.8% vs. 25.7% of all records, respectively). These studies were predominantly located in areas expected to have substantial increases in annual mean temperature (>1.5ºC in 97% of studies) and rainfall (> 25 mm in 53% of studies) by 2041–2070. Notably, the most consistent relationship was between temperature and vector-borne zoonoses (56% of Positive effects, mean Hedges' g = 0.36). Overall, our analyses provide evidence that climate sensitivity is common across zoonoses, likely leading to substantial yet complex effects of future climate change on zoonotic burden. Finally, we emphasize the need for future studies to utilize biologically relevant models, apply rigorous space-time controls, consider causal perspectives, and address taxonomic and geographic biases to allow a robust consensus of climate-risk relationships to emerge. 


------------------------------------------------------------------------

## Repository navigation:

-   📁[data](https://github.com/BioDivHealth/climate_meta/tree/main/data) included most of the data needed to performed the analyses, and the data collated during data extraction and systematic literatrue search.
    -   [SI File 1](https://github.com/BioDivHealth/climate_meta/blob/main/data/Extended_Data/Dataset_S1.xlsx) includes the full dataset, metadata, studies included in the dataset and log of literature search. The data is in the draft version before the peer-review process.
    -   Climatology data for study locations were downloaded from [CHELSA Website](https://chelsa-climate.org/downloads/)
-   📁[scripts](https://github.com/BioDivHealth/climate_meta/tree/main/scripts) contains all of the R code needed to complete the analyses and produce the figures and tables.
    -   Data Analysis scripts (in `scripts/analysis/`):
        -   [00_Effect_Size_Calculations.Rmd](https://github.com/BioDivHealth/climate_meta/blob/main/scripts/analysis/00_Effect_Size_Calculations.Rmd "00_Effect_Size_Calculations.Rmd") - calculates Hedge's g based on extracted statistics
        -   [01_AD_KS_Multiple_Testing_Correction.R](https://github.com/BioDivHealth/climate_meta/blob/main/scripts/analysis/01_AD_KS_Multiple_Testing_Correction.R "01_AD_KS_Multiple_Testing_Correction.R") - performs Anderson–Darling and Kolmogorov–Smirnov tests with multiple testing correction
        -   [01_AD_KS_tests_v1.R](https://github.com/BioDivHealth/climate_meta/blob/main/scripts/analysis/01_AD_KS_tests_v1.R "01_AD_KS_tests_v1.R")
        -   [01_Direction_ChiSquare_Tests.R](https://github.com/BioDivHealth/climate_meta/blob/main/scripts/analysis/01_Direction_ChiSquare_Tests.R "01_Direction_ChiSquare_Tests.R")
        -   [02_climate_variables.R](https://github.com/BioDivHealth/climate_meta/blob/main/scripts/analysis/02_climate_variables.R "02_climate_variables.R") - handles spatial climatology data (CHELSA)
        -   [03_Hedge_vs_ClimChange_Tests.R](https://github.com/BioDivHealth/climate_meta/blob/main/scripts/analysis/03_Hedge_vs_ClimChange_Tests.R "03_Hedge_vs_ClimChange_Tests.R") - performs statistical tests (Chi-square and Exact Fisher tests) on contingency tables describing distribution of categories of effect sizes across different levels of predicted temperature/precipitation change
        -   [04_Results.Rmd](https://github.com/BioDivHealth/climate_meta/blob/main/scripts/analysis/04_Results.Rmd "04_Results.Rmd") - runs all the analyses and creates all figures
        -   [ClimateCorrelationPrecipitation.R](https://github.com/BioDivHealth/climate_meta/blob/main/scripts/analysis/ClimateCorrelationPrecipitation.R "ClimateCorrelationPrecipitation.R")
        -   [ClimateCorrelationTemperature.R](https://github.com/BioDivHealth/climate_meta/blob/main/scripts/analysis/ClimateCorrelationTemperature.R "ClimateCorrelationTemperature.R")
    -   Manuscript text (results section)
        -   [05_Results_Full_Text.Rmd](https://github.com/BioDivHealth/climate_meta/blob/main/scripts/analysis/05_Results_Full_Text.Rmd "05_Results_Full_Text.Rmd") - creates a full results text for the manuscript based on dynamically generated results
    -   Figures generation scripts (in `scripts/figures/`)
        -   [Figure1.R](https://github.com/BioDivHealth/climate_meta/blob/main/scripts/figures/Figure1.R "Figure1.R")
        -   [Figure2.R](https://github.com/BioDivHealth/climate_meta/blob/main/scripts/figures/Figure2.R "Figure2.R")
        -   [Figure3.R](https://github.com/BioDivHealth/climate_meta/blob/main/scripts/figures/Figure3.R "Figure3.R")
        -   [Figure4.R](https://github.com/BioDivHealth/climate_meta/blob/main/scripts/figures/Figure4.R "Figure4.R")
        -   [FigureS2.R](https://github.com/BioDivHealth/climate_meta/blob/main/scripts/figures/FigureS2.R "FigureS2.R")
        -   [FigureS3.R](https://github.com/BioDivHealth/climate_meta/blob/main/scripts/figures/FigureS3.R "FigureS3.R")
        -   [FigureS4.R](https://github.com/BioDivHealth/climate_meta/blob/main/scripts/figures/FigureS4.R "FigureS4.R")
        -   [FigureS5.R](https://github.com/BioDivHealth/climate_meta/blob/main/scripts/figures/FigureS5.R "FigureS5.R")
-   📁[outputs](https://github.com/BioDivHealth/climate_meta/tree/main/outputs) contains all of the figures and tables resulting from statistical analyses associated with this manuscript.

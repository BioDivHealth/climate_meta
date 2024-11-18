# Title: Uncovering general patterns of climate change sensitivity in zoonotic disease dynamics

------------------------------------------------------------------------

## Authors: Lewis Gourlay, Artur Trebski, Rory Gibb, Natalie Imirzian, David W. Redding

------------------------------------------------------------------------

## Abstract:

> Climate change is expected to exacerbate the burden of many infectious diseases, yet the climate sensitivity of zoonotic diseases (driven primarily by spillover from an animal reservoir) is markedly understudied relative to vector-borne and water-borne infections. To address this gap we conducted a global systematic review and meta-analysis to identify published relationships between climatic indicators (temperature, precipitation, humidity) and risk metrics across zoonotic infections worldwide. We identified 185 studies from 55 countries, describing 547 effect sizes across 51 diseases (25 viruses, 18 bacteria, 8 parasites), with most studies testing linear (n=166) rather than nonlinear (n=23) relationships. We find evidence of climate sensitivity across diverse zoonotic disease systems (significant, non-zero relationships in 64.3% of temperature effects, 49.8% of precipitation effects and 48.9% of humidity effects), but with broad variation in the direction and strength of relationships. Positive effects of temperature and rainfall on disease risk measures were more common than negative effects (39.1 to 25.2% and 30.5 to 19.2% of all records respectively) and these studies were overwhelmingly located in areas expected to have substantial increases in annual mean temperature (100% of studies located in areas predicted a greater than 1ºC increase) and rainfall (46% of studies in areas predicted a greater than 25mm increase in annual rainfall) by 2041-2070. Notably, the most consistent relationship recovered was for zoonoses with invertebrate vectors as part of their transmission cycle. Overall, our analyses provide evidence that climate sensitivity is common across zoonoses, which is likely to lead to substantial although complex effects of future climate change on zoonotic burden. Finally, we highlight the need for future studies to employ more biological appropriate models, ensure rigorous space-time controls, consider causal perspectives and address both taxonomic and geographic biases, to allow a robust consensus of climate-risk  relationships to emerge.

------------------------------------------------------------------------

## Repository navigation:

-   📁[data](https://github.com/BioDivHealth/climate_meta/tree/main/data) included most of the data needed to performed the analyses, and the data collated during data extraction and systematic literatrue search.
    -   Climatology data for study locations were downloaded from [CHELSA Website](https://chelsa-climate.org/downloads/)
-   📁[scripts](https://github.com/BioDivHealth/climate_meta/tree/main/scripts) contains all of the R code needed to complete the analyses and produce the figures and tables.
    -   Data Analysis scripts:
        -   [00_Effect_Size_Calculations.Rmd](https://github.com/BioDivHealth/climate_meta/blob/main/scripts/00_Effect_Size_Calculations.Rmd "00_Effect_Size_Calculations.Rmd") - calculates Hedge's g based on extracted statistics
        -   [01_AD_KS_tests.R](https://github.com/BioDivHealth/climate_meta/blob/main/scripts/01_AD_KS_tests.R "01_AD_KS_tests.R") - performs Anderson-Darling Tests to investigate distribution of effect sizes across groups, also performs Two-sample Kolmogorov-Smirnov test to investigate differences in effect size distributions between vectored and non-vectored diseases
        -   [02_climate_variables.R](https://github.com/BioDivHealth/climate_meta/blob/main/scripts/02_climate_variables.R "02_climate_variables.R") - handles spatial climatology data (CHELSA)
        -   [03_Hedge_vs_ClimChange_Tests.R](https://github.com/BioDivHealth/climate_meta/blob/main/scripts/03_Hedge_vs_ClimChange_Tests.R "03_Hedge_vs_ClimChange_Tests.R") - performs statistical tests (Chi-square and Exact Fisher tests) on contingency tables describing distribution of categories of effect sizes across different levels of predicted temperature/precipitation change
        -   [04_results.Rmd](https://github.com/BioDivHealth/climate_meta/blob/main/scripts/04_results.Rmd "04_results.Rmd") - runs all the analyses and creates all figures
    -   Manuscript text (results section)
        -   [05_Results_Full_Text.Rmd](https://github.com/BioDivHealth/climate_meta/blob/main/scripts/05_Results_Full_Text.Rmd "05_Results_Full_Text.Rmd") - creates a full results text for the manuscript based on dynamically generated results
    -   Figures generation scripts
        -   [Figure1.R](https://github.com/BioDivHealth/climate_meta/blob/main/scripts/Figure1.R "Figure1.R")
        -   [Figure2.R](https://github.com/BioDivHealth/climate_meta/blob/main/scripts/Figure2.R "Figure2.R")
        -   [Figure3.R](https://github.com/BioDivHealth/climate_meta/blob/main/scripts/Figure3.R "Figure3.R")
        -   [Figure4.R](https://github.com/BioDivHealth/climate_meta/blob/main/scripts/Figure4.R "Figure4.R")
        -   [FigureS2.R](https://github.com/BioDivHealth/climate_meta/blob/main/scripts/FigureS2.R "FigureS2.R")
        -   [FigureS3.R](https://github.com/BioDivHealth/climate_meta/blob/main/scripts/FigureS3.R "FigureS3.R")
        -   [FigureS4.R](https://github.com/BioDivHealth/climate_meta/blob/main/scripts/FigureS4.R "FigureS4.R")
        -   [FigureS5.R](https://github.com/BioDivHealth/climate_meta/blob/main/scripts/FigureS5.R "FigureS5.R")
        -   [FigureS6.R](https://github.com/BioDivHealth/climate_meta/blob/main/scripts/FigureS6.R "FigureS6.R")
-   📁[outputs](https://github.com/BioDivHealth/climate_meta/tree/main/outputs) contains all of the figures and tables resulting from statistical analyses associated with this manuscript.

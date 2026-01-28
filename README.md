# Coffee-Sleep study analysis code

This repository contains the analysis code (Bash and R) using in:
> Söderholm S, Ulander M, William Toma V, Kaufmann S, Qiao X, Berglind D, Calling S, Daka B, Grote L, Martinell M, Bergman F, Henriksson P, Östgren CJ, Zhong W, Cantù C, Iredahl F, "Habitual coffee consumption poorly correlates with sleep quality and daytime sleepiness: a cross-sectional study", _PLos One_, 2026

## Contents
- **Soderholm_etal_CoffeeSleep_main_analysis_20260127.R** : _R script containing code for the main analysis and visualization_
- **Soderholm_etal_CoffeeSleep_GWAS_20260127.sh** : _Bash script for performing genome-wide association studies (GWAS)_ 
- **Soderholm_etal_CoffeeSleep_GWASplots_20260127.R** : _R script containing code for processing and visualization of GWAS results_

> [!NOTE]
> - The data underlying the study derives from the Swedish CArdioPulmonary bioImage Study (SCAPIS), a multi-center study conducted at six university hospitals in Sweden, examining 30,154 randomly selected men and women aged 50-64 years. The first public release of data was on March 17, 2021, available to be applied for by researchers based in Sweden or international researchers in collaboration with a researcher based in Sweden. Applicants need to be connected to a research institution in Sweden and need an approval from the Swedish Ethical Review Board (Etikprövningsmyndigheten). Furthermore, regarding international data sharing, SCAPIS data is subject to confidentiality under Chapter 24, Section 8 of the Swedish Public Access to Information and Secrecy Act (OSL), and Section 7 of the Public Access to Information and Secrecy Ordinance.
> - Scripts were last tested on Linux Mint

## Requirements
- R 4.4.2 (used in the main analysis script)
- R 4.0.3 (used i nthe GWAS results plotting script)
- PLINK 2.0

## License
MIT License

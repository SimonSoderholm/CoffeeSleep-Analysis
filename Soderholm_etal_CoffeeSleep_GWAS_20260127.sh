#!/bin/bash

mkdir ./GWAS_results/Coffee_Run4

genotypeFile="Data_merged/SCAPIS_merged_QCed"

phenotypeFile="SCAPIS_data/phenotype_list.txt"

colName="q5question2"

threadnum=8

covariateFile="SCAPIS_data/covariate_list.txt"

covariateColnames="Site, AgeAtVisitOne, GenotypingBatch, PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10"

plink2 --pfile ${genotypeFile} --pheno ${phenotypeFile}	--pheno-name ${colName}	--covar ${covariateFile} --covar-name ${covariateColnames} --glm hide-covar firth-fallback firth-residualize single-prec-cc --threads ${threadnum} --out GWAS_results/Coffee_Run4/Association_coffee_res_Run4 --covar-variance-standardize


#--freq

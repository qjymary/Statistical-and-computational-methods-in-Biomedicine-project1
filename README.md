# Statistical-and-computational-methods-in-Biomedicine-project1
**Disease Prediction on the Crohn's Syndrome Dataset**

To address the high-dimensionality issue (number of variables far exceeding sample size), applied Chi-square tests to obtain p-values for each SNP, followed by Benjamini-Hochberg correction, reducing the SNP set from 248,409 to 65. 

Used single classifier models and ensemble learners to predict disease status, with LightGBM achieving high accuracy (AUC=0.675) and faster runtime after Bayesian tuning (BayesSearchCV). 

Utilized the IGESS package developed by Dai Mingwei et al. to perform feature selection and model prediction based on summary statistics from 7 existing GWAS (p-values) and genotype data from 4,081 individuals, improving prediction accuracy (AUC=0.696).

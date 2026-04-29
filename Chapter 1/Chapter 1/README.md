# Chapter 1 - Predicting Civilian Electoral Contention

This folder contains the replication materials for Chapter 1, "Civilian Electoral Contention".

## Structure
- `scripts/data_cleaning.R`: cleans and merges ECAV with external datasets, creates `data/df_civilians.csv`.
- `scripts/ch1_analysis.ipynb`: chapter analysis notebook for prediction models and exports chapter figures to `figures/`.
- `data/`: chapter input and derived datasets.
- `figures/`: output figures (created automatically when running scripts).

## Required public datasets (not included)
- V-Dem
- ACLED
- NELDA
- QoG
- UCDP GED
- CIRI

Place downloaded source files in `Chapter 1/data/` using the filenames referenced in `scripts/data_cleaning.R`.

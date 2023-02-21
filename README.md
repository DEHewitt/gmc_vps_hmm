# gmc_vps_hmm

This repository contains all code to process and analyse data as presented in Hewitt et al., (2023). Data for this project is _include info about copyright_. This data is hosted on the [Integrated Marine Observing System Animal Tracking Facility (IMOS ATF) database](https://imos.org.au/facilities/animaltracking) under the NSW Department of Primary Industries Coastal and Estuarine Fish Tracking (CEFT) project. However, this data is under embargo until _date_.

Some analysis for this project was carried out using the [High Performance Cluster (HPC) Katana](https://doi.org/10.26190/669x-a286) hosted by UNSW ResTech, while some was performed on my personal laptop. The scripts are integrated, as in output from one flows into the next. However, this makes for a somewhat complicated workflow whereby computationally intensive parts were submitted to Katana and less intensive aspects were done locally. With this in mind, the following briefly describes the workflow and output from each step.

1. Submit `crawl.pbs` to Katana which runs `crawl.R` to estimate temporally-regular locations. 
2. Locally process data using `environmental.R` this adds environmental data. 
3. Submit `mixed-hmm.pbs` to Katana which runs `mixed-hmm.R`. This fits all the mixed hidden Markov models with _K_ = {1, ..., 4}.
4. Locally run `compare-mixtures.R` which compares the mixed models via AIC.
5. Submit `hmm.pbs` to Katana which runs `hmm.R`. This script fits hidden Markov models with covariates on the transition probability matrices using the random-effects structure selected during Step 4. 
6. Locally run `results.R`. This is the final stage of the analysis and it starts by selecting among covariates and is then mostly plotting and producing summaries of the selected model.

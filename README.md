# Riverside Opioid Analysis
Comprehensive analytic pipeline for modeling opioid-related health outcomes in Riverside County, CA by ZIP code.  
This project integrates ZIP-code–level overdose deaths, hospitalizations, ED visits, and Healthy Places Index (HPI) indicators to quantify geographic variation in opioid burden and identify structural determinants of severity using Poisson regression and multivariable linear regression.

---

## Overview

This repository contains all data preparation, modeling, and visualization scripts used to build a composite opioid severity index and evaluate its relationship with social determinants of health. The workflow uses **reproducible R code**, including Poisson regression frameworks, robust standard errors, dispersion checks, and choropleth maps.

This project was developed as part of a multi-quarter honors capstone at the University of California, Riverside.

---

## Objectives

1. **Construct a composite opioid severity score**  
   - Weighted combination of ED visits, hospitalizations, and overdose deaths
   - ED visit = 1, hospitalizations = 2, overdose deaths = 3

2. **Model the association between community-level factors and opioid burden**  
   - Healthy Places Index (HPI) indicators  
   - Composite Opioid Severity Score 

3.**Fit Poisson regression models with robust standard errors**   
   - Robust IRR tables  
   - Dispersion testing 

4. **Map geographic trends across Riverside County ZIP codes**  
   - Visualize severity distribution  
   - Identify hotspots of opioid harm  

---

## Key Methods

### **Modeling Framework**
- Poisson regression  
- Robust standard errors via `sandwich` + `lmtest`  
- IRR computation and 95% CIs  
- Dispersion testing (underdispersion observed)

### **Core Utility Functions**
- `robust_irr_tbl()` – Generates robust IRR tables  
- `dispersion()` – Computes dispersion parameter  
- `safe_nb()` – Automatically fits NB if Poisson is overdispersed
- `save_fig` - Saves figures

### **Mapping**
- Choropleth maps using `tigris`, `sf`, and `ggplot2`  
- ZIP-code level joining and spatial cleaning  

---

## 📁 Repository Structure


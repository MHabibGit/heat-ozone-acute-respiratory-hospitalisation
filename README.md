# Reproducibility of the heat–ozone hospitalisation study

This repository contains the R code used to reproduce the **statistical analyses** and **figures** presented in:

## **Variation in respiratory hospitalisation risk under joint heat–ozone exposure across urban forms**

The study examines short-term respiratory hospitalisation risk associated with compound heat–ozone exposure across different urban forms and sociodemographic groups in Amsterdam, Rotterdam, The Hague, and Utrecht, the Netherlands, during the warm season (May–September) from 2013–2019.

The repository is intended to support transparency and reproducibility of the study. Because the individual-level hospitalisation and sociodemographic data are subject to restricted access through Statistics Netherlands (CBS), the complete analysis cannot be reproduced using the publicly available files alone. Researchers wishing to reproduce the statistical analyses must obtain the appropriate access to the [CBS Microdata environment](https://microdata.cbs.nl/en).

---

# Author and affiliation

**Corresponding author:** Maha Habib

**Contact information:**
[mmoustafahabib@tudelft.nl](mailto:mmoustafahabib@tudelft.nl)

**Affiliation:**
Faculty of Architecture and the Built Environment, Delft University of Technology
Julianalaan 134
2628 BL Delft
The Netherlands

---

# Repository structure

The repository is organised into the following components:

```text
/
├── statistical_analysis/
│   ├── heat_ozone_hospitalisation_risk.R
│   └── adjusting_for_population.R
│
├── figures/
│   ├── Fig2ab.R
│   ├── Fig2cd.R
│   ├── Fig3b.R
│   └── Fig4.R
│
├── figures_manual/
│   ├── Fig1.png
│   └── [Fig3a.png]
│
└── README.md
```

---

# Data availability

The analysis uses two types of data: publicly available environmental data and restricted individual-level population and hospitalisation data.

## Publicly available environmental data

The environmental datasets used in the analysis are available through the **4TU.ResearchData repository**.

These include:

* daily air quality data;
* daily meteorological data; and
* urban form / Local Climate Zone (LCZ) data.

These datasets contain the environmental variables required for the exposure assessment and can be used alongside the analysis code.

[Insert 4TU.ResearchData DOI/link here.]

## Restricted CBS and hospitalisation data

The statistical analysis additionally requires individual-level hospitalisation and sociodemographic data obtained through **Statistics Netherlands (CBS) Microdata**.

These data are **not included in this GitHub repository** and cannot be redistributed because access is subject to the terms and conditions of CBS Microdata Services.

Researchers wishing to reproduce the statistical analysis must independently apply for access to the relevant CBS microdata through CBS and comply with the applicable data-use requirements.

Information on CBS Microdata access, application procedures, conditions of use, and available datasets can be found here:

[Insert CBS Microdata link here.]

All analyses involving the restricted data were conducted within the CBS Microdata environment.

---

# Statistical analysis

The statistical analysis is implemented in two main R scripts.

## 1. `heat_ozone_hospitalisation_risk.R`

This script contains the main data preparation and statistical analysis workflow.

### Hospitalisation and sociodemographic data

The first part of the script prepares the Dutch hospitalisation data and links individual hospitalisation records to relevant sociodemographic information from CBS.

The linked information includes, among other variables:

* sex/gender;
* age;
* household income;
* residential address/location; and
* other variables required for the study stratifications.

Hospitalisation records are subsequently linked to the urban form classification. Residential locations are assigned to the corresponding 100 × 100 m grid cell and Local Climate Zone (LCZ), allowing hospitalisation risks to be examined across urban forms.

### Environmental exposure data

The script then links the individual hospitalisation records to daily meteorological and air quality data.

The environmental data include the variables required to characterise:

* heat exposure;
* ozone exposure;
* solar radiation;
* wind speed;
* humidity;
* precipitation;
* PM₂.₅; 
* NO₂.

Additional filtering is applied to restrict the analysis to the study period and warm season:

**May–September, 2013–2019**

The resulting dataset is used for the statistical analysis of acute respiratory hospitalisation risk associated with compound heat–ozone exposure.

---

## 2. `adjusting_for_population.R`

This script prepares the population information required to account for changes in the population at risk over the study period.

The script compiles the relevant annual population data and generates the population offset required for the statistical models.

Because the population changes over time and differs across the study population and spatial units, these offsets are incorporated into the statistical analysis to account for the changing population at risk.

> **Important:** This script can take a substantial amount of time to run because it processes and compiles the required population information. It should therefore be run before the main statistical analysis if the required population-offset files have not already been generated.

---

# Recommended order of execution

For a complete reproduction of the analysis, the recommended workflow is:

### Step 1 — Obtain CBS Microdata access

Obtain authorised access to the relevant CBS Microdata datasets and the Dutch hospitalisation data required for the study.

All restricted-data processing must be performed within the authorised CBS Microdata environment.

### Step 2 — Make the environmental datasets available

Make the environmental datasets from the 4TU.ResearchData repository available within the analysis environment.

These include the urban form, meteorological, and air quality data required by the statistical analysis.

### Step 3 — Generate the population offset

Run:

```r
adjusting_for_population.R
```

This generates the population information and offset required for the statistical models.

This step may take considerable time.

### Step 4 — Run the main statistical analysis

Run:

```r
heat_ozone_hospitalisation_risk.R
```

This script:

1. filters and prepares the hospitalisation records;
2. links hospitalisation records to CBS sociodemographic information;
3. links residential locations to the LCZ classification;
4. links records to meteorological and air quality exposures;
5. restricts the data to the study period and warm season;
6. prepares the analysis datasets; and
7. runs the statistical models used to estimate heat–ozone hospitalisation risk.

### Step 5 — Reproduce the figures

Once the required analysis outputs have been generated, the figure scripts can be run:

```r
Fig2ab.R
Fig2cd.R
Fig3b.R
Fig4.R
```
These scripts generate the coded figures presented in the manuscript. Please note that further visual refinements were made in Adobe Illustrator to produce the final published figures, so the R outputs may differ slightly in appearance.

Figures that were produced or assembled manually in Adobe Illustrator are provided directly as high resolution JPEG files in the `figures_manual/` folder.

---

# Figure reproduction

The repository contains R scripts for figures generated programmatically.

| Figure    | Script     | Description                        |
| --------- | ---------- | ---------------------------------- |
| Fig. 2a–b | `Fig2ab.R` | [Insert brief description]         |
| Fig. 2c–d | `Fig2cd.R` | [Insert brief description]         |
| Fig. 3b   | `Fig3b.R`  | [Insert brief description]         |
| Fig. 4    | `Fig4.R`   | [Insert brief description]         |

---

# Software requirements

The analysis was conducted in **R**.

**R version 4.4.3** 

The analysis requires the R packages specified in the individual scripts.

To reproduce the analysis as closely as possible to the published results, users should use the same R version and package versions as those used for the original analysis.

---

# File paths and working directory

The scripts use local file paths to access the required datasets and generate analysis outputs.

Before running the scripts, users should check and update the file paths at the beginning of each script to correspond to their working environment.

When reproducing the analysis within the CBS Microdata environment, the relevant restricted datasets must be available under the appropriate CBS project directories.

---

# Data protection and confidentiality

The individual-level hospitalisation and CBS microdata used in this study are restricted and subject to the conditions governing access to CBS Microdata.

These data are therefore **not provided with this repository** and must not be redistributed.

Only researchers with appropriate authorisation should access or process these data. All analyses involving restricted information must be conducted within the authorised CBS Microdata environment and in accordance with CBS requirements.

The publicly available environmental datasets do not provide access to the individual-level hospitalisation records used in the study.

---

# Reproducibility limitations

Complete reproduction of the statistical results requires access to the restricted hospitalisation and CBS microdata used in the original analysis.

Consequently:

* the **R analysis code** is publicly available;
The environmental datasets are publicly available through the associated **4TU.ResearchData repository**:

**DOI:** [10.4121/293c7ef6-05aa-41f3-8fb1-a3738fe66801](https://doi.org/10.4121/293c7ef6-05aa-41f3-8fb1-a3738fe66801)
* the **individual-level hospitalisation and CBS data are restricted** and are not redistributed;
* the **statistical analysis must therefore be reproduced within an authorised CBS Microdata environment**; and
* manually produced figures are provided as PNG files where applicable.

Researchers without access to the restricted microdata can inspect and use the publicly available environmental datasets and reproduce the applicable environmental-data processing and figure components, but cannot independently reconstruct the individual-level hospitalisation analysis from the public files alone.

---

# Citation

If you use the code or datasets in this repository, please cite the associated publication and the corresponding 4TU.ResearchData dataset(s).

### Manuscript

[Full citation will be provided once published]

---

# Contact

For questions regarding the code or reproducibility of the analysis, please contact:

**Maha Habib**
Delft University of Technology
[mmoustafahabib@tudelft.nl](mailto:mmoustafahabib@tudelft.nl)

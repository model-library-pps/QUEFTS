# QUEFTS
This repository contains an R implementation of the Quantitative Evaluation of the Fertility of Tropical Soils (QUEFTS) model.

## Description
QUEFTS was first published by Janssen et al (1990).

QUEFTS can be used for quantitative evaluation of the native fertility of tropical soils. The procedure consists of four successive steps. First the potential supplies of nitrogen, phosphorus and potassium are calculated, applying relationships between chemical properties of the 0-20 cm soil layer and the maximum quantity of those nutrients that can be taken up by the crop, if no other nutrients and no other growth factors are yield-limiting. In the second step, the actual uptake of each nutrient is calculated as a function of the potential supply of that nutrient, taking into account the potential supplies of the other two nutrients. Step 3 comprises the establishment of three yield ranges, as depending on the actual uptakes of nitrogen, phosphorus, and potassium, respectively. Next, these yield ranges are combined in pairs, and the yields estimated for pairs of nutrients are averaged to obtain an ultimate yield estimate (Step 4).

## Example
An example of how to run a QUEFTS simulation can be found [here](QUEFTS/QUEFTS_simulation.R). In this example, nutrient limited yields of soybean are calculated using standard crop-specific QUEFTS parameters ([link](QUEFTS/crop_parameters.csv)).

## Software requirements
QUEFTS requires the R Programming Language to run. The newest version of R can be downloaded here ([here](https://cran.r-project.org/bin/windows/base/)). After R is installed, R scripts can be run within the RStudio Integrated Development Environment (IDE). The newest version of RStudio can be downloaded [here](https://posit.co/download/rstudio-desktop).

# User manual
This manual assumes that QUEFTS is run within RStudio. In order to run the previously mentioned example, follow these steps:
- Download the QUEFTS_R repository in a directory of your choice.
- Open RStudio
- Choose File -> Open Project
- Enter the folder .../QUEFTS
- Click on QUEFTS_R.Rproj and click on the "Open" button. 
- Click on the "Files" tab in the Files / Plot / Packages window.
- Click on QUEFTS_simulation.R
- Click on the "Source" button to run the example simulation.


## References
Janssen B.H., Guiking F.C.T., Van der Eijk D., Smaling E.M.A., Wolf J., Reuler H. A system for quantitative evaluation of the fertility of tropical soils (QUEFTS). Geoderma 46: 299-318. https://doi.org/10.1016/0016-7061(90)90021-Z


## Contact person
Mink Zijlstra (mink.zijlsta@wur.nl)
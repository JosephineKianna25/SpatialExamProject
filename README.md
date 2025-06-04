# Mapping accessibility of Electric Vehicle Charging Services for Trans-European Travel

## Project description 
This repository contains the contents of the final exam project in the course Spatial Analytics at Aarhus University in 2025.
All resources and materials needed in order to replicate our code is to be found in this repository.

In this project, we investigate the feasibility of travelling with an electric vehicle (EV) within a 2000 km radius south of Aarhus, Denmark. 
We selected four destination cities and analyzed which of these were the most accesible, based on time-effiency, hospitality points and EV charging infrastructure along the routes.
easiest, most time-saving and/or consumer-friendly.

## Content and Repository Structure
All necessary files to reproduce the analysis are included in this repository. 

The script `Boundingbox.R` outputs a plot of major cities within a 2000 km reach south of Aarhus. We chose our four destinations based on this plot.

The main script is `EDSI.R` and should be run from start to finish. This outputs various visualizations of the distribution of EV chargers and hospitality points of interest within a buffer along the routes. It also calculates metrics for travel feasibilty based on chosen infrastructure and comfort features.

Data is loaded directly in the script and results are also generated in the script. 

The outputs are saved in the repository in the format of png files for ggplots including the bounding box, Electric driving suitability index bar plot, infrastructure and comfort subscore bar plot, POI distribution and POI density plot. The html files include interactive maps of the POI's and suitable driving range stops, as well as a table for the EDSI metrics.

## Usage and Technical Requirements
To reproduce the results, we advise you to clone the repository.

1. Execute this from the command line: 

``$ git clone https://github.com/JosephineKianna25/SpatialExamProject.git``

2. Open the script Hospitality_map_&_EDSI.R in RStudio and run it from top to bottom. 

You will need R installed with the required libraries. A list of necessary packages is included in the top of the script. 

## Contact details
For any questions or feedback regarding the repository and its contents, feel free to contact us on: 

Louise Østergaard: 202209107@post.au.dk

Josephine K. P. Sørensen: 202206921@post.au.dk


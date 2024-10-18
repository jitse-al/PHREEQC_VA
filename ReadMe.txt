About this code:
This code is used in Alsemgeest et al. (n.d., Hydrothermal alteration at the Vista Alegre impact structure, Brazil), to provide thermochemical constraints for each of the found mineral phases in hydrothermal alteration sets. Code should be used in combination with the package phreeqc.R, and can be adapted to fit other purposes.
A file structure is used to allow incorporation of uncertainties in (folder in brackets):
- Database information (database)
- Types of precipitating phases (Equilibrium Phases), including the possibility to add additional phases (Phases) and solution  species (Solution Species) for each database
- Types of material dissolving (Reaction Types)
- Types of initial solutions (Solutions)
- Quantities of dissolved material (the input R file)
- Temperature range (the input R file)

The main inputfile used by R is either one of the following, and the code should be run from there:
Hydrothermal vein set 1 - input 1.R
Hydrothermal vein set 1 - input 2.R
Hydrothermal vein set 2 - input 1.R
Hydrothermal vein set 2 - input 2.R

Additional input is provided for the following files within the subfolders. Please check the subfolders and adapt, correspondingly, the parameters that you want to adapt.
- Hydrothermal vein set 1 - input 2.R
- Hydrothermal vein set 2 - input 2.R

Please note the following:
- DO NOT EDIT inputfile.pqi! This is the inputfile used by PHREEQC and is loaded and adapted automatically for each single model. Changing this will prevent the model from running properly.
- Working directory in the R inputfile needs to be adapted to work on another computer.
- Code is complex, running it will take a while (~6-24 hours).
- Datasets are available online, and should be referred to following the references within Alsemgeest et al. (n.d., Hydrothermal alteration at the Vista Alegre impact structure, Brazil).
- For information or help on how to use this code, please contact jitse.alsemgeest@gmail.com or j.alsemgeest@vu.nl

Last update: 12/07/2021
This repo contains data, r code, and additional files related to my phd project "Population and Mobility in the Middle Jomon Period Viewed from Architectural and Skeletal Remains".

This has been a learning process for me. I have attempted to make this project as transparent and open as possible. The code included here isn't always pretty, but it's functional, and it's the same code used to conduct the analyses in my dissertation project. 
I have tried to structure this repo to make it as simple as possible to replicate the analyses included in my dissertation. 
For ease of use, once the appropriate packages are installed, clicking the "knit" button on the Project_Code.Rmd file should rerun the analyses used in my dissertation research. 

To ensure package consistency, a renv.lock file is included in the repo in order to match package versions to ones used in the original analyses.
To do this, prior to knitting the Project_Code.Rmd file, the renv command "renv::restore()" should be run.
There have been issues with the associated Bioconductor packages failing to install using the restore() function.
If a problem occurs with loading the Bioconductor packages using renv, you may need to first install a more current version of Bioconductor separately, and then use the renv::restore() function, which should load the appropriate package versions. 

Running all scripts associated with the dissertation within the markdown file itself took a very long time, so the markdown file is setup to refer to the main scripts 01-library-function-data-import.R to 07-extras.R residing in the main "phd" folder of the repo. Additional analyses and code that were not included in the final version of the dissertation are saved in the "other-things" folder.Raw data is saved within the "raw-data" folder inside the main "data" folder. Initial derived data from analyses are saved in the "derived-data" folder, also found in the main "data" folder. Figures are saved in pdf format in the "figures" folder. 

Knitting the Project_Code.Rmd file will run the scripts necesary to populate the "derived-data" and "figures" folders, but as the derived data used in these analyses are the result of random sampling, the figures produced will not be exact duplicates of those included in the dissertation.  

Please don't hesitate to contact me if there are any issues.
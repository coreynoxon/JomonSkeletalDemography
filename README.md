### **A Paleodemographic Approach to the Middle Jomon Boom and Bust Population Pattern: related data and code.** 

------------------------------------------------------------------------

This repo contains data, r code, and additional files related to my paper "A Paleodemographic Approach to the Middle Jomon Boom and Bust Population Pattern".

I have attempted to make this project as transparent and open as possible. The code included isn't always pretty, but it should be functional, and it's the same code used to conduct the analyses in the paper. I have tried to structure this repo to make it as simple as possible to reproduce the results discussed in the paper. For ease of use, once the appropriate packages are installed, clicking the "knit" button on the Project_Code.Rmd file should rerun the analyses used in the paper.

To ensure package consistency, a renv.lock file is included in the repo in order to match package versions to ones used in the original analyses. To do this, prior to knitting the Project_Code.Rmd file, the renv command "renv::restore()" should be run. The project was compiled using an older version of R, so please ensure that the version you use matches the version listed in the renv file.

Running all scripts associated with the paper within the markdown file itself can take a long time, so the markdown file is setup to refer to the main scripts 01-library-function-data-import.R to 09-extras.R residing in the main folder of the repo. The different sensitivity analyses mentioned in the paper are present in scripts 05-sensitivity-analysis-15p5.R, 06-sensitivity-analysis-range.R, and 07-sensitivity-analysis-weight.R. If the files are run directly from markdown pdf versions of each plot will be created and saved to the corresponding folder found in the sensitivity section of the figures folder. Raw data is saved within the "raw-data" folder inside the main "data" folder. Initial derived data from analyses are saved in the "derived-data" folder, also found in the main "data" folder. Figures are saved in pdf format in the "figures" folder.

Knitting the Project_Code.Rmd file will run the scripts necessary to populate the "derived-data" and "figures" folders, but as the derived data used in these analyses are the result of random sampling, if the 03-MC-aoristic-analysis.R script is run again the figures produced will not be exact duplicates of those included in the paper.

Please don't hesitate to contact me if there are any issues.

### License

------------------------------------------------------------------------

CC-BY-3.0

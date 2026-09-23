# Raw data directory

The 2022 Ghana Demographic and Health Survey microdata are not distributed with this repository.

To reproduce the analysis:

1. Register with The DHS Program and obtain authorised access to the 2022 Ghana DHS Individual Recode dataset.
2. Place the authorised Stata Individual Recode file in this directory with the filename:

   `GHIR8CFL.DTA`

3. Run the project from the repository root using `source("99_run_project.R")`.

Do not commit, upload, or redistribute the DHS microdata through this repository. The `.gitignore` file is configured to exclude raw DHS data files.


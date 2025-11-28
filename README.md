# Writing Clean and Efficient Code

This repository contains slides and materials for the PML workshop **“Writing Clean and Efficient Code.”**  
The workshop draws heavily on previous iterations taught by [Adam Kaplan](https://github.com/adamkaplan0) (“Good Coding Practices for Social Scientists”) and [Tomoya Sasaki](https://github.com/tomoya-sasaki) (“Efficient Coding in R”).

The workshop is structured around two main topics. First, we review how to write **clean, well-documented, and reproducible code**, including practical tools and conventions. Second, we introduce basic ideas in **computational complexity** and **memory allocation in R**, and then illustrate the benefits of **vectorization** and **parallel processing**.

## Organization

This repository contains the slides and supporting materials for the workshop. Since the workshop is primarily lecture-style, there are only a few short examples.

- The **analysis/** folder contains:
  - `analysis.R`: a clean, well-documented version of the analysis.
  - `analysis_UGLY.R`: an intentionally messy version for comparison.  
  Both scripts run a linear regression on simulated data and save a regression table and predicted value plots for the report.

- The **data/** folder contains the simulated `.csv` dataset.
- The **report/** folder contains the figures, tables, and the `.Rmd` report file that compiles into the final PDF.
- The **simulation/** folder contains the R script that generates the simulated data used in the analysis.
- The **renv/** folder and related files should not be modified. They are included for reproducibility. To learn more about how `renv` works, see the official [renv](https://rstudio.github.io/renv/articles/renv.html) documentation.
- The **examples/** folder contains the R scripts for the two short practical exercises discussed during the workshop.

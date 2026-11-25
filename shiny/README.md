# Student Sleep Explorer — STA304H5F Group 12

Public app URL: **add the actual Posit Connect Cloud URL after publication**.

## Run locally

Open `app.R` in RStudio with `survey.csv` in the same folder. Install dependencies once:

```r
install.packages(c("shiny", "ggplot2", "dplyr", "rsconnect"))
```

Click Run App, or run `shiny::runApp()` from this directory. Use a current R release (R 4.1 or later). The app reproduces the EDA sampling with seed 30412 and checks the reported means before starting.

## Deployment

1. Review data sharing permission before making the repository public. `survey.csv` contains the 84 cleaned responses without student codenames; row combinations can still carry re-identification risk. Do not upload the original workbook or codebook containing identifiers. If public sharing of row-level data is not permitted, obtain instructor guidance before publishing.
2. In RStudio, run `source("prepare_deployment.R")` from this directory after installing the dependencies. This generates `manifest.json` using the local R package versions. Commit `app.R`, `survey.csv`, `README.md` and generated `manifest.json` to the course GitHub repository.
3. Sign in to https://connect.posit.cloud/ using GitHub. Publish a Shiny app from the repository, choose the branch and `app.R` entry file, and follow the build log.
4. Open the published app in an incognito browser. Test filters, plots, colours, axis ranges, model and t-test, including empty/small subgroups. Paste the verified public URL into this README and submit as instructed.

## Default sample checks (from EDA)

- 50 observations, 25 female and 25 male.
- Mean nightly sleep 7.28 hours; mean weekly study 12.81 hours.
- Female mean 7.20; male mean 7.36; Welch p approximately 0.623.
- Multiple regression R² approximately 0.28; study coefficient approximately -0.05; p approximately 0.012.

## Rubric coverage

- Three reactive plots: histogram, scatterplot, boxplot; selectable variables and groups.
- Axis zoom and selectable colour palettes; three shared demographic filters.
- Tab-based display selection, readable labels, plain-language reactive interpretations.
- Fixed-formula adjusted model and Welch t-test with subgroup safeguards.
- About tab documents sampling, measurements and limitations.

## Validation status — deployment candidate

The previous version was run locally by the group. Screenshots matched the EDA descriptive statistics, regression and Welch test; the group reported that interaction tests passed. The revised version preserves the input CSV, random seed and model formula, and adds explanations, a pooled-SD Cohen's d, explicit reference categories, improved axis controls and table styling. The revised code has received a static delimiter check, but R is not available in the authoring environment, so it still needs local execution and interaction testing. No public URL has been created.

## Final local checks

- Start the app from this folder; do not paste package installation commands into app.R.
- Confirm the default sample values above and Cohen's d approximately -0.14 (direct pooled-SD estimate; the EDA report gives -0.15).
- Verify that the histogram shows both endpoints and that changing axes only zooms the view.
- Switch variables and palettes, test single-gender and small/empty subgroups, then reset filters.
- Check that the adjusted model reference group changes appropriately when a subgroup excludes the original reference. A single living arrangement intentionally disables the original adjusted model.
- Review the expanded interpretation text and public data-sharing permission before publication.

## Appearance

The interface uses U of T Blue (#1E3765). Plot palettes are Bluish Green (default), U of T Blue and Warm Minimal. Boxplots use a uniform fill; histogram bins alternate the first two palette colours.

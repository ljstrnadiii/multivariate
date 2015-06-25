# multivariate

This is the repo for my final exam for a course in Multivariate Data Analysis at Case Western.
The full report is the pdf file: **`Multivariate_Final.pdf`**. Refer to this pdf for the full analysis.


### Part 1: Men and Women's Race times for various distances
* **`parta.R`** - checks for violations of univariate/multivariate normality
* **`partb.R`** - constructs probability ellipses, T^2 regions, Bonferroni Confidence Intervals (Simultaneous)
* **`partc.R`** - analysis of variance: mean comparison between gender
* **`partd.R`** - Looks at the Principal Components of Men

### Part 2: MS Patients and Visual Stimuli Groups:
* **`2parta.R`** - assesses marginal normality, constructs Fisher's linear discriminant (classification), and performs Lachenbruch's holdout procedure (estimates error rate of missclassification). Lachenbruch's holdout essentially hold out a new instance every loop, creates a classification rule on the rest, classifies the one held out, and counts the number of holdouts missclassified. The error is the number of holdouts missclassified over the sum of both populations.



R packages used: Kernsmooth, xtable, and ellipse




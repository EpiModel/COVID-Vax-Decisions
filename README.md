# Modelling the Interplay between Responsive Individual Vaccination Decisions and the Spread of SARS-CoV-2
This repository holds the source to code to reproduce the analysis model of SARS-CoV-2 transmission and vaccination behaviours in the state of Georgia in 2021/2022.

These models are written and executed in the R statistical software language. To run these files, it is necessary to first install our epidemic modeling software, [EpiModel](https://github.com/EpiModel/EpiModel), and our extension package specifically for modeling SARS-CoV-2 transmission dynamics, [EpiModelCOVID](https://github.com/EpiModel/EpiModelCOVID). The branch of the EpiModelCOVID repository associated with this research project is [Vax-Decisions](https://github.com/EpiModel/EpiModelCOVID/tree/Vax-Decisions).

This installation is accomplished with the renv package in R. First install renv (if you do not already have it installed) and run:

```r
renv::init()
```

in your project directory. Select the option to restore the package set set from the `renv.lock` file.

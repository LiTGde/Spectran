
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Spectran <img src="man/figures/logo.png" align="right" height="139"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/LiTGde/Spectran/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/LiTGde/Spectran/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

LiTG Spectran is a tool to analyse light spectra for visual and
non-visual (so-called melanopic) needs, wrapped up in a Shiny App.
Spectran allows for the import of spectra in various csv forms but also
provides a wide range of example spectra and even the creation of own
spectral distributions. The goal of the app is to provide easy access
and a visual overview of the spectral calculations underlying common
parameters. It isthus ideal for educational purposes or the creation of
presentation ready graphs in lighting research and application.

## Installation

You can install the development version of Spectran from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("LiTGde/Spectran")
```

## How to use Spectran

The commands show how to start Spectran:

`library(Spectran)`

`Spectran()`

or simply go to the example hosted on
[shinyapps.io](https://litg.shinyapps.io/Spectran_en/).

You can also use the German version:

`Spectran(lang_setting = "Deutsch")`

or go to it´s example on
[shinyapps.io](https://litg.shinyapps.io/Spectran_de/).

## Example Screenshots

<figure>
<img src="inst/app/www/img/English/06_Nonvisual_Analysis.png"
alt="UI for Melanopic Analysis" />
<figcaption aria-hidden="true">UI for Melanopic Analysis</figcaption>
</figure>

<figure>
<img src="inst/app/www/img/English/07_Age-dependent_Analysis.png"
alt="UI for Analysis of Age-dependent changes" />
<figcaption aria-hidden="true">UI for Analysis of Age-dependent
changes</figcaption>
</figure>

<figure>
<img src="inst/app/www/img/English/10_Table_Export.png"
alt="Export Table for Alpha-opic Quantities" />
<figcaption aria-hidden="true">Export Table for Alpha-opic
Quantities</figcaption>
</figure>

<figure>
<img src="inst/app/www/img/English/09_Graph_Export.png"
alt="Export File for Photometric Quantities" />
<figcaption aria-hidden="true">Export File for Photometric
Quantities</figcaption>
</figure>

<figure>
<img src="inst/app/www/img/English/01_Import_UI.png"
alt="UI for File Import" />
<figcaption aria-hidden="true">UI for File Import</figcaption>
</figure>

<figure>
<img src="inst/app/www/img/English/02_Example_Spectra.png"
alt="UI for Example Spectra Import" />
<figcaption aria-hidden="true">UI for Example Spectra
Import</figcaption>
</figure>

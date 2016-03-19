<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/poissonconsulting/ranmr.svg?branch=master)](https://travis-ci.org/poissonconsulting/ranmr) [![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.45224.svg)](http://dx.doi.org/10.5281/zenodo.45224)

Loch Rannoch Ferox Trout Mark-Recapture Analysis
================================================

`ranmr` is the companion R analysis package for Thorne et al.'s manuscript The Low Abundance and High Catchability of Large Piscivorous Ferox Trout (Salmo trutta) in Loch Rannoch, Scotland.

Installation
------------

To use the `ranmr` package first install [R](http://cran.r-project.org) (version 3.2.2 or greater) and [JAGS](http://mcmc-jags.sourceforge.net) (version 3.4.0 or greater).

Then execute the following code at the R terminal:

    #install.packages("devtools")
    devtools::install_github(paste("poissonconsulting", sep = "/", c(
      "tulip@v0.0.13", "datalist@v0.5.0", "juggler@v0.1.5", "jaggernaut@v2.3.1",
      "ranmrdata", "ranmr")))
    library(ranmr)

Replication
-----------

To quickly replicate the results with *unreliable* model estimates use:

    replicate_results("debug")

This option is useful for testing everything is installed correctly.

To replicate the results with **reliable** model estimates use:

    replicate_results("report")

To replicate the results with the same analysis settings and figure font family as the manuscript:

    extrafont::font_import() # if you haven't done this already on your machine
    replicate_results("paper", parallel = TRUE, base_family = "Arial")

Information
-----------

For more information type `?replicate_results` after loading the package.

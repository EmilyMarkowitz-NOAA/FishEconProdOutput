<!-- README.md is generated from README.Rmd. Please edit that file -->

# FishEconProdOutput <a href={https://emilymarkowitz-noaa.github.io/FishEconProdOutput}><img src="man/figures/logo.png" align="right" width=139 height=139 alt="logo with a plot of economic productivity for fish and inverts." />

### *Measuring Output for U.S. Commercial Fisheries From Theory to Practice*

[![DOI](https://zenodo.org/badge/291852337.svg)](https://zenodo.org/badge/latestdoi/291852337)

[![](https://img.shields.io/badge/devel%20version-0.1.1-blue.svg)](https://github.com/EmilyMarkowitz-NOAA/FishEconProdOutput)
[![](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![](https://img.shields.io/github/last-commit/EmilyMarkowitz-NOAA/FishEconProdOutput.svg)](https://github.com/EmilyMarkowitz-NOAA/FishEconProdOutput/commits/master)

**Emily Markowitz**<sup>1</sup> (Emily.Markowitz AT noaa.gov)

**John Walden**<sup>2</sup> (John.Walden AT noaa.gov)

**Sun Ling Wang**<sup>3</sup> (SunLing.Wang AT usda.gov)

<sup>1</sup>Alaska Fisheries Science Center, National Marine Fisheries
Service, National Oceanic and Atmospheric Administration, Seattle, WA
98195

-   *Work completed as a contractor with ECS in the NOAA Fisheries
    Office of Science and Technology Economics & Social Analysis
    Division.*

<sup>2</sup>Northeast Fisheries Science Center, National Marine
Fisheries Service, National Oceanic and Atmospheric Administration,
Woods Hole, MA

<sup>3</sup>United States Department of Agriculture, Economic Research
Service

-   *Work completed while on detail with the NOAA Fisheries Office of
    Science and Technology Economics & Social Analysis Division.*

------------------------------------------------------------------------

Since many of the original collaborators have changed positions, future
maintenance may be provided by:

**Alex Richardson**<sup>4</sup> (Alex.Richardson AT noaa.gov)

<sup>4</sup>Contractor with ECS Tech in the NOAA Fisheries Office of
Science and Technology Economics & Social Analysis Division, Silver
Spring, MD 20910

*The views expressed are those of the author and should not be
attributed to the NOAA, ECS, or ERS*

# Study Purpose

This package provides guidelines on fishery productivity measurement at
the individual fishery and aggregate sector levels. Attention is given
to the constructions of output and total factor productivity based on
available data and a bottom-up approach. Given that there is no
nation-wide standard cost survey, we recommend starting with measuring
TFP at the fishery level based on a translog gross output production
possibility frontier using index number techniques. Special attention is
given to measuring quality-adjusted physical capital inputs in the
bottom-up approach.

# Download this package

    library(devtools)
    devtools::install_github("EmilyMarkowitz-NOAA/FishEconProdOutput", dependencies = TRUE, build_vignettes = T)
    library(FishEconProdOutput)

# Documentation

**GitHub Repository:**
[https://github.com/emilyhmarkowitz/FishEconProdOutput](https://github.com/EmilyMarkowitz-NOAA/FishEconProdOutput)

**Documentation GitHub Pages:**
<https://emilymarkowitz-noaa.github.io/FishEconProdOutput/>

You can find vignettes here

    browseVignettes("FishEconProdOutput")

Or directly view the vignettes here:

-   [FEUS-tables](https://emilymarkowitz-noaa.github.io/FishEconProdOutput/articles/FEUS-tables.html):
    How to create the tables used in FEUS
-   [NEFSC-Fishery](https://emilymarkowitz-noaa.github.io/FishEconProdOutput/articles/NEFSC-Fishery.html):
    How to use this analysis for a collection of sub fisheries, such as
    in the Northeast US

# Publications

[Wang, Sun Ling., John B. Walden, and Emily H. Markowitz. 2021.
*Measuring Output, Inputs, and Total Factor Productivity for the U.S.
Commercial Fishery: A Proposal*. NOAA Tech. Memo. NMFS-F/SPO-217, 14
p.](https://spo.nmfs.noaa.gov/sites/default/files/TM217.pdf)

# NOAA README

> This repository is a scientific product and is not official
> communication of the National Oceanic and Atmospheric Administration,
> or the United States Department of Commerce. All NOAA GitHub project
> code is provided on an ‘as is’ basis and the user assumes
> responsibility for its use. Any claims against the Department of
> Commerce or Department of Commerce bureaus stemming from the use of
> this GitHub project will be governed by all applicable Federal law.
> Any reference to specific commercial products, processes, or services
> by service mark, trademark, manufacturer, or otherwise, does not
> constitute or imply their endorsement, recommendation or favoring by
> the Department of Commerce. The Department of Commerce seal and logo,
> or the seal and logo of a DOC bureau, shall not be used in any manner
> to imply endorsement of any commercial product or activity by DOC or
> the United States Government.

# License

> Software code created by U.S. Government employees is not subject to
> copyright in the United States (17 U.S.C. §105). The United
> States/Department of Commerce reserve all rights to seek and obtain
> copyright protection in countries other than the United States for
> Software authored in its entirety by the Department of Commerce. To
> this end, the Department of Commerce hereby grants to Recipient a
> royalty-free, nonexclusive license to use, copy, and create derivative
> works of the Software outside of the United States.

<img src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" height="75" alt="NOAA Fisheries">

[U.S. Department of Commerce](https://www.commerce.gov/) | [National
Oceanographic and Atmospheric Administration](https://www.noaa.gov) |
[NOAA Fisheries](https://www.fisheries.noaa.gov/)

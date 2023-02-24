# tame <img src="man/figures/Logo_tame.png" align="right" height="300" />

<!-- badges: start -->
[![CRANstatus](https://www.r-pkg.org/badges/version/EpiForsk)](https://cran.r-project.org/package=tame)
[![R-CMD-check](https://github.com/Laksafoss/tame/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Laksafoss/tame/actions/workflows/R-CMD-check.yaml)
![CRAN Downloads overall](https://cranlogs.r-pkg.org/badges/grand-total/tame)
<!-- badges: end -->


**T**iming, **A**natomical, Therapeutic and Chemical Based **Me**dication Clustering.

## Overview
`tame` is an R package that implements an agglomerative hierarchical clustering with a bespoke distance measure based on medication similarities in the Anatomical Therapeutic Chemical Classification System, medication timing and medication amount or dosage. Tools for summarizing, illustrating and manipulating the cluster objects are also available.



## Installation
```
install.package("tame")
```


## Usage
Use `medic` to cluster medication data with ATC codes and dosage trajectories. 

```
library(tame)

# A simple clustering based only on ATC
clust <- medic(complications, id = id, atc = atc, k = 3)


# A simple clustering with both ATC and timing
clust <- medic(
  complications,
  id = id,
  atc = atc,
  timing = first_trimester:third_trimester,
  k = 3
)
```


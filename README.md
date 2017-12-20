FAOSTAT
=======
[![travis-ci-build-status](https://travis-ci.org/mkao006/FAOSTATpackage.svg?branch=master)](https://travis-ci.org/mkao006/FAOSTATpackage)
[![codecov.io](https://codecov.io/github/mkao006/FAOSTATpackage/coverage.svg?branch=master)](https://codecov.io/github/mkao006/FAOSTATpackage?branch=master)
[![CRAN version](http://www.r-pkg.org/badges/version/FAOSTAT)](http://cran.rstudio.com/web/packages/FAOSTAT/index.html)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/FAOSTAT)](http://cran.r-project.org/web/packages/FAOSTAT/index.html)

This repository contains all the files to build the FAOSTAT package.

# NOTE: This package has been orphaned due to the current
  re-development of the FAOSTAT API and the fact that I no longer work
  for the organisation and has no access to the updates.

==============================================================================

The package can be installed from CRAN:

```r
install.packages("FAOSTAT")
```

or install the develop version via the following link, different
version can be installed by specifying the **ref** argument.

```r
library(devtools)
install_github(repo = "mkao006/FAOSTATpackage", subdir = "FAOSTAT")
```

Vignettes and demos are available and please make use of them:

```r
vignette(topic = "FAOSTAT")
demo(topic = "FAOSTATdemo")
```

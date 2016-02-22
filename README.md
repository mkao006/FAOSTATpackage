FAOSTAT
=======
[![travis-ci-build-status]](https://travis-ci.org/mkao006/FAOSTATpackage.svg?branch=master)(https://travis-ci.org/mkao006/FAOSTATpackage)
[![codecov.io](https://codecov.io/github/mkao006/FAOSTATpackage/coverage.svg?branch=master)](https://codecov.io/github/mkao006/FAOSTATpackage?branch=master)

This repository contains all the files to build the FAOSTAT package.

==============================================================================

The package can be installed from CRAN:

```r
install.packages("FAOSTAT")
```

or install the develop version via the following link, different
version can be installed by specifying the **ref** argument.

```r
library(devtools)
install_github(repo = "FAOSTATpackage", username = "mkao006", subdir = "FAOSTAT")
```

Vignettes and demos are available and please make use of them:

```r
vignette(topic = "FAOSTAT")
demo(topic = "FAOSTATdemo")
```
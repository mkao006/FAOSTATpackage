FAOSTAT
=======


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
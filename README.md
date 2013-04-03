# How to install


Install some dependencies from Bioconductor

```r
source("http://bioconductor.org/biocLite.R")
biocLite("biovizBase", dep = TRUE)
```

Please check svn version of MutableRanges/biovizBase, and install them from
command line by using `R CMD INSTALL`

```bash
svn co https://hedgehog.fhcrc.org/bioconductor/trunk/madman/Rpacks/MutableRanges/
svn co https://hedgehog.fhcrc.org/bioconductor/trunk/madman/Rpacks/biovizBase/
```

Install object* and visnab

```r
if(!require(devtools)) install.packages("devtools")
library(devtools)
install_github("objectWidgets", "ggobi")
install_github("objectWidgetsQt", "tengfei")
install_github("visnab", "tengfei")
```



# CADET (Clustering And Differential Expression Testing)  <img src="./cadet_sticker.png" align="right" width="150px"/>

### What is CADET?

`CADET` is an `R` package for testing for a difference in means for a given feature 
between clusters of observations identified via hierarchical or k-means clustering.

This package, as well as its underlying methodology, is motivated by the practical need to interpret and validate groups of observations obtained via clustering. In this case, a common validation approach involves testing differences in feature means between observations in two estimated clusters. However, classical hypothesis tests lead to an inflated Type I error rate, because the $p$-values are computed using *the same set of data* used for generating the hypothesis. 

To overcome this problem, we propose a new test for the difference in means in a single feature between a pair of clusters obtained using hierarchical or $k$-means clustering. More details can be found in our manuscript (Chen and Gao, 2023+).

### How do I install the package?

To download and load `CADET`, use the code below.
```r
require("devtools")
devtools::install_github("yiqunchen/CADET")
library(CADET)
```

### Tutorials and Use

Visit https://yiqunchen.github.io/CADET/ for tutorials and examples. Please file an [issue](https://github.com/yiqunchen/CADET/issues) if you have a request for a tutorial that is not currently included.


### Citation

If you use `CADET` for your analysis, please cite our manuscript:

Chen YT,  Gao LL. (2023+) Testing for a difference in means of a single feature after clustering. arXiv preprint.

### Bug Reports / Change Requests

If you encounter a bug or would like to make a change request, please file it as an issue [here](https://github.com/yiqunchen/CADET/issues).

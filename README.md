## `DMTL`

[![Build Status](https://www.travis-ci.com/dhruba018/DMTL.svg?branch=main)](https://www.travis-ci.com/dhruba018/DMTL)
[![CRAN version](http://www.r-pkg.org/badges/version/DMTL)](https://CRAN.R-project.org/package=DMTL)
[![R version](https://img.shields.io/badge/R%3E=-3.6.1-blue)](https://cran.r-project.org/)
[![LICENSE](https://img.shields.io/badge/license-GPL--3-blueviolet)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![Repo Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

R package for applying **<ins>d</ins>istribution <ins>m</ins>apping based <ins>t</ins>ransfer <ins>l</ins>earning**. `DMTL` employs the widely renowned concept of histogram matching and extend it to  **distribution matching** by including non-parametric density estimates like kernel density estimates (KDE). The typical use case would be if somebody wants to utilize data from multiple sources for statistical modeling but there exists significant distribution shift between both predictor and response values. In this case, `DMTL` can alleviate this shift by generating a distribution matching based map and transfer the target data to the source domain to utilize the available source data for modeling using a suitable predictive modeling technique.  

![DMTL](DMTL_Model.jpg)


### How to Install
The package is now available on CRAN. So you can install it from R / RStudio as usual - 
		
		install.packages("DMTL")

If you want to install the dev version available in this repo, you will need the [`devtools`](https://CRAN.R-project.org/package=devtools) package. You can install it using the following commands - 
		
		install.packages("devtools")
		devtools::install_github("dhruba018/DMTL")  

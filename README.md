# mojson

<!-- badges: start -->
[![](https://cranlogs.r-pkg.org/badges/last-week/mojson)](https://cran.rstudio.com/web/packages/mojson/index.html)
[![](https://cranlogs.r-pkg.org/badges/grand-total/mojson)](https://cran.rstudio.com/web/packages/mojson/index.html)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version-last-release/mojson)](https://cran.r-project.org/package=mojson)
[![CRAN checks](https://cranchecks.info/badges/worst/mojson)](https://cranchecks.info/pkgs/mojson)
<!-- badges: end -->

> A Serialization-Style Flattening and Description for JSON

Support JSON flattening in a long data frame way, where the nesting keys will be stored in the absolute path. It also provides an easy way to summarize the basic description of a JSON list. The idea of 'mojson' is to transform a JSON object in an absolute serialization way, which means the early key-value pairs will appear in the heading rows of the resultant data frame. 

   
'mojson' also provides an alternative way of comparing two different JSON lists, returning the left/inner/right-join style results.

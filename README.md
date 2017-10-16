## rineq

[![Travis-CI Build Status](https://travis-ci.org/brechtdv/rineq.svg?branch=master)](https://travis-ci.org/brechtdv/rineq)

_Statistical Analysis of Health Inequalities_

The `rineq` package provides functions to  calculate the relative, generalized, and Erreygers corrected concentration index; plot Lorenz curves; and decompose health inequalities using (generalized) linear models, survival models, and regression and binary trees.

#### Available functions

Concentration index
<table>
<tr><td><code>ci</code></td><td>Calculate the relative, generalized, or Erreygers corrected concentration index</td></tr>
<tr><td><code>correct_sign</code></td><td>Correct negative values in the health variable</td></tr>
</table>

Decomposition
<table>
<tr><td><code>contribution</code></td><td>Decompose the Relative Concentration Index into its components</td></tr>
<tr><td><code>rpart_ci</code></td><td>Recursive partitioning and regression trees using the concentration index</td></tr>
<tr><td><code>imp</code></td><td>Evaluate variable importance in <code>rpart_ci()</code> trees</td></tr>
</table>

#### Install

To download and install the latest development version from GitHub:
```r
devtools::install_github("brechtdv/rineq")
```

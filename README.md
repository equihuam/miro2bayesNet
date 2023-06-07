
# miro2bayes

<!-- badges: start -->
<!-- badges: end -->

The goal of miro2bayes is to Interacts with a Miro board that has been
setup to represent a Bayesian network. With this library yyou will be
able to gather the attribtes to layout the DAG Directed Acyclic Graph)
of a network, which has been collaboratively portraid in Miro. With all
basic data in R you cana produces a formal DAG representation which is
suitable to further processing in R and even in other platforms like
Python or Netica.

## Installation

You can install the development version of miro2bayes like so:

``` r
library(devtools)
install_github("equihuam/miro2bayesNet")
```

## Example

This is a basic example which shows you how get Bayesian network data
from Miro:

``` r
library(miro2bayes)
miro_data <- datosMiro(servMiro = "miro", user = "your-miro-token")
miro_data
```

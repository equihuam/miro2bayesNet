
# miro2bayes

<!-- badges: start -->
<!-- badges: end -->

The goal of this library is to Interact with a Miro board that has been
setup to represent a Bayesian network. We assumed that in **Miro**
*sticky notes* represent network’s *nodes* and *connectors* represent
*arcs* sugesting a causal/influence link. sticky notes should have text
describingg the node and a single *tag*, indicating the name of the
corresponding variable in the database. With this library you will be
able to gather the attributes to layout the DAG (Directed Acyclic Graph)
of the network, which has been collaboratively portraid in *Miro*. With
all basic data in *R* you can produce a formal DAG representation which
is suitable to further processing and model-training in R and even in
other platforms like Python or Netica. There are function in thi library
to do so.

## Installation

You can install the development version of `miro2bayes` like so:

``` r
library(devtools)
install_github("equihuam/miro2bayesNet")
```

## Example

This is a basic example which shows you how get Bayesian network data
from *Miro*:

``` r
library(miro2bayes)
miro_data <- datosMiro(servMiro = "miro", user = "your-miro-token")
miro_data
```

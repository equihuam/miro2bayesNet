
# miro2bayes

<!-- badges: start -->
<!-- badges: end -->

The goal of this library is to Interacts with a Miro board that has been
setup to represent a Bayesian network. We assumed that in **Miro**
*sticky notes* represent network’s *Nodes* and *connectors* represent
*arcs* sggesting a causal/influence link. sticky notes should have text
describingg the node and a single *tag*, indicating the name of the
variable in the database holding the available data for the node. With
this library you will be able to gather the attribtes to layout the DAG
Directed Acyclic Graph) of a network, which has been collaboratively
portraid in Miro. With all basic data in R you cana produces a formal
DAG representation which is suitable to further processing and
model-training in R and even in other platforms like Python or Netica.

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

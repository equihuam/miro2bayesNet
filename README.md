
# miro2bayes

<!-- badges: start -->
<!-- badges: end -->

The goal of this library is to Interacts with a Miro board that has been
setup to represent a Bayesian network. We assumed that in **Miro**,
*sticky notes* represent network’s *nodes*, and *connectors* represent
*arcs* sugesting a causal/influence link. sticky notes should have text
describingg the node, and a single *tag*, indicating the name of the
corresponding variable in the database. With this library you will be
able to gather the attributes to layout the DAG (Directed Acyclic Graph)
of the network, which has been collaboratively portraid in *Miro*. With
all the basic data brought into *R*, you can formally process a DAG.
This inicludes the ability to further processing and training the model
in *R*, and even in other platforms like Python or Netica. There are
functions in this library to do so.

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
miro_data <- datosMiro(servMiro = "miro", user = "your-miro-token", boad_id = "your-board-id")
miro_data
```

If you need to find the *board id* of interest you could use function
`miroBoards`. This function queries Miro with the credentials provided,
and returns a tibble with the data describing boards available to that
user.

``` r
miro_boards <- miroBoards(servMiro = "miro", user = "your-miro-token")
miro_boards
```

## Typical Use Cycle

<div style="float: right; margin: 0 10px 10px 30px; width: 200px; height: 300px;
            object-fit: cover;">

![](man/figures/use%20cycle.png)

</div>

A typical cycle of use would be as ilustrated in the FIgure. This way
you shall have a collection of implied conditional independence
relations to discuss the **DAG** structure. By critical analysis of the
**DAG** you should go back to *Miro* to solve inconsistencies or further
develop the intended *causal proposition*.

``` r
miro_data <- datosMiro(servMiro = "miro", user = "your-miro-token")
miro_DAG <-  prepara_DAG(nodos = miro_data$nodos, arcos = miro_data$arcos)
cat(indepCond)
```

Once a satisfactory Bayesian network has been produce the function
**red2DNE** is used to produce a *.DNE* file that can be used in Netica
and other dedicated Bayesian network software for training, analysisis
and prediction.

``` r
miro_data <- datosMiro(servMiro = "miro", user = "your-miro-token")

data_DNE <- red2DNE(datos_marcos = miro_data$marcos, 
                    papelitos = miro_data$nodos, 
                    arcos = miro_data$arcos)
```

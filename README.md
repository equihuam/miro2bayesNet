
# miro2bayes

<!-- badges: start -->
<!-- badges: end -->

The goal of this library is to interact with a **Miro** board that has
been set up to represent a Bayesian network. We assumed that in
**Miro**, *sticky notes* represent the network’s *nodes*, and
*connectors* represent *arcs* suggesting a causal/influence link. Sticky
notes should have text describing the node, and a single *tag*,
indicating the name of the corresponding *variable in the database*.
With this library, you will be able to gather the attributes to lay out
the DAG (Directed Acyclic Graph) of the network, which has been
collaboratively portrayed in **Miro**. With all the basic data brought
into *R*, you can formally process a DAG. This includes the ability to
further process and train the model in *R*, and even in other platforms
like `bnlearn` in *R*, or the various options available in *Python* or
in dedicated applications like *Netica*. There are functions in this
library to do so.

To use interact with **Miro** you need credential than are obtained
following [this Quickstart directions for your *first REST API
call*](https://developers.miro.com/docs/rest-api-build-your-first-hello-world-app?utm_source=your_apps).
The `miro2bayes` library uses `keyring` library to keep your credentials
safe; so, you will need to register your credentials with function
`key_set(service="identifier-you-like", username = "user-name-you-like")`
as [described here](https://rdrr.io/cran/keyring/man/key_get.html). Both
*service*, and *username* are required respectively as *servMiro* and
*user* parameters to get access to **Miro** with `datosMiro`.

## Installation

You can install the development version of `miro2bayes` like so:

``` r
library(devtools)
install_github("equihuam/miro2bayesNet")
```

## Example

This is a basic example which shows you how to get Bayesian network data
from **Miro**:

``` r
miro_data <- datosMiro(servMiro = "miro", user = "your-miro-token", boad_id = "your-board-id")
miro_data
```

If you need to find the *board id* of interest, you could use the
function `miroBoards`. This function queries **Miro** with the
credentials provided and returns a `tibble` with the data describing the
boards available to that user.

``` r
miro_boards <- miroBoards(servMiro = "miro", user = "your-miro-token")
miro_boards[, c("name", "id")]
```

## Typical Use Cycle

<img align="right" height="200px" src="figures/use cycle.png">

A typical cycle of use would be as illustrated in the Figure. This way
you shall have a collection of implied conditional independence
relations to discuss the **DAG** structure. By critical analysis of the
**DAG** you should go back to **Miro** to solve inconsistencies or
further develop the intended *causal proposition*.

``` r
miro_data <- datosMiro(servMiro = "miro", user = "your-miro-token")
miro_DAG <-  prepara_DAG(nods = miro_data$nodos, arcs = miro_data$arcos)
cat(indepCond)
```

A quick check of the network recovered from **Miro** can be produced
with the function `miro_validar`. The check shows whether the network is
indeed a *DAG* (no cycles present!) and a few numbers describing what
was found in **Miro**: number of nodes, identified variable names, the
status of links, and so for.

``` r
check_data <- miro_validar(variables = datos_miro$nodes, arcs = datos_miro$arcs))
t(check_data)
```

Once a satisfactory Bayesian network has been produced, the function
**red2DNE** is used to produce a *.DNE* file that can be used in Netica
and other dedicated Bayesian network software for training, analysis and
prediction.

``` r
miro_data <- datosMiro(servMiro = "miro", user = "your-miro-token")

data_DNE <- red2DNE(frames_data = miro_data$marcos, 
                    variables= miro_data$nodos, 
                    arcs = miro_data$arcos)
```

Another option is to feed **Miro** data into `bnlearn` with the function
`miro2bnlearn`. Which is done as follows.

``` r
netMiro_bn <- miro2bnlearn(nodes = datos_miro$nodes, arcs = datos_miro$arcs)
```

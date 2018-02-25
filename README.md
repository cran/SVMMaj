# SVMMaj R package

## Introduction

The main features of this package are: implementation of the **SVM-Maj** majorization algorithm for SVMs, handling of nonlinearity through splines and kernels, the ability to handle several error functions (among other the classic hinge, quadratic hinge and Huber hinge error).

## How to use this package

The main functions of the package are `svmmaj`, which estimates the SVM, and `svmmajcrossval`, which performs a grid search of *k*-fold cross validations using **SVM-Maj** to find the combination of input values, (such as `lambda` and `degree` in the case of a polynomial kernel) giving the best prediction performance.

The former function requires the `n x k` attribute matrix `X` and the `n x 1` vector `y` with class labels. Apart from the data objects, other parameter input values can be given as input to tune the model: 
- `lambda`, 
- `hinge`, 
- `weights.obs`, 
- `scale`, and 
- parameters for nonlinearities and settings of the algorithm itself. 

For example,

```
svmmaj(X, y, lambda = 2, hinge = "quadratic", scale = "interval")
```

runs the SVM model with `lambda = 2`, using a quadratic hinge and for each attribute, the values are scaled to the interval [0,1].
The function `svmmajcrossval` uses the same parameter input values and additionally the parameters to be used as grid points of the *k*-fold cross validation. These parameters should be given in the list object `search.grid`, e.g.,

```
svmmajcrossval(X, y, search.grid = list(lambda = c(1, 2, 4)))
```

# SlicStan

SlicStan<sup>1</sup> is a Stan-like probabilistic programming language that translates to [Stan](https://mc-stan.org/).
It provides automatic program transformations that allow for a more lightweight syntax and 
inference optimizations. There are three main ways in which SlicStan and Stan differ:
1. SlicStan contains no program blocks, nor any annotations as to what block a variable 
belongs to (other than what the input data to the model is).
2. In SlicStan, there is no need to distinguish between random variables defined 
using `~` (e.g. `x ~ normal(0, 1)`) and those defined using pseudo-random number generators 
(e.g. `x = normal_rng(0, 1)`).
3. SlicStan supports discrete parameters, as long as the number of discrete parameters is 
fixed, known in advance, and their support is finite. 

For example, the following program is a valid program in SlicStan:
```Stan
real phi0 ~ beta(1, 1);
real theta0 ~ beta(1, 1); 

int<2> z1 ~ bernoulli(theta0);
real phi1 = phi0 * z1 + (1 - phi0) * (1 - z1);
data real y1 ~ normal(phi1, 1);

real theta1 = theta0 * z1 + (1 - theta0) * (1 - z1);
int<2> z2 ~ bernoulli(theta1);
```

This will translate to a Stan program with parameters `phi0` and `theta0`, and
generated quantities `z1` and `z2` (with `z1` being automatically marginalized 
out from Stan's target density).

If you are interested in reading more about SlicStan and seeing examples, you 
can refer to either the 2019 POPL paper [1] (which also gives the operational density-based
semantics of SlicStan), or the 2017 MSc thesis [2]. 

> **NOTE**: SlicStan is a research repo, and as such the code is largely experimental, incomplete, 
> and poorly documented. If you are looking for a reliable Bayesian workflow, please 
> consider [Stan](https://mc-stan.org/). If you are interested in contributing SlicStan's or 
> similar ideas to Stan, please have a look at [the Stan3 repo](https://github.com/stan-dev/stanc3).

<sup>1</sup> SlicStan stands for "Slightly Less Intensely Constrained Stan".

------------------
## Papers  
[1] Gorinova, M. I., Gordon, A. D., Sutton, C., & Vákár, M. (2020). 
[Conditional independence by typing](https://arxiv.org/abs/2010.11887). *arXiv preprint arXiv:2010.11887*.


[2] Gorinova, M. I., Gordon, A. D., & Sutton, C. (2019). 
[Probabilistic programming with densities in SlicStan: efficient, flexible, and deterministic](https://doi.org/10.1145/3290348). 
*Proceedings of the ACM on Programming Languages*, 3(POPL), 1-30. 

[3] Gorinova, M. I. (2017). [Probabilistic Programming with SlicStan](http://homepages.inf.ed.ac.uk/s1207807/files/slicstan.pdf). 
*MSc dissertation, University of Edinburgh*.

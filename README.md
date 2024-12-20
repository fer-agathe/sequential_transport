# Replication materials

<div align="center">
This repository contains the replication materials for the article titled 
<i>Sequential Conditional Transport on Probabilistic Graphs for Interpretable Counterfactual Fairness</i>.

[![arXiv](https://img.shields.io/badge/arXiv-2408.03425-b31b1b.svg)](https://arxiv.org/abs/2408.03425)
</div>

📕 A companion html e-book that explains the codes and provides supplementary materials can be accessed at the following url: 
<https://fer-agathe.github.io/sequential_transport>.

📄 The new version will soon be uploaded on arXiv. In the meantime, it can be accessed here: 
https://github.com/fer-agathe/sequential_transport/blob/main/Prep-print-v2.pdf

## Reference of the paper (preprint version 1) (version 2 coming soon)

```
@misc{fernandesmachado_2024_sequential,
      title={Sequential Conditional Transport on Probabilistic Graphs for Interpretable Counterfactual Fairness}, 
      author={Fernandes Machado, Agathe and Charpentier, Arthur and Gallic, Ewen},
      year={2024},
      eprint={2408.03425},
      archivePrefix={arXiv},
      primaryClass={cs.LG},
      url={https://arxiv.org/abs/2408.03425}, 
      doi={10.48550/arXiv.2408.03425}
}
```

## Small Package

We defined some of the functions used in this ebook in a small R package, {seqtransfairness}, which can be downloaded from the [github repository](https://github.com/fer-agathe/sequential_transport) associated with the paper.

To install the package:
```{r install-package, eval=FALSE}
remotes::install_github(
  repo = "fer-agathe/sequential_transport", subdir = "seqtransfairness"
)
```

Then, the package can be loaded as follows:
```{r, message=FALSE, warning=FALSE}
library(seqtransfairness)
```

Alternatively, if you have downloaded the Github repository, you will find a copy of the package in the `seqtransfairness` folder. To load the package, the `load_all()` function from R package {devtools} can be used. This is what is done inside the scripts.


The functions, placed in the `seqtransfairness/R/` folder are documented in the package. Hence, once the package is loaded with the `load_all()` function, the help pages can be accessed (e.g., by typing in the R console: `?seqtransfairness`, or `?seq_trans`).


## How to replicate

To replicate the codes, provided you have installed R and Rstudio on your computer, double click on the following file to open RStudio (so that the correct working directory is set): `./scripts/sequential_transport.Rproj`. Then, you can open the scripts in RStudio.

```
├ ── data
│    └── law_data.csv
├ ── functions
│    └── utils.R
│    └── graphs.R
├ ── seqtransfairness
├ ── scripts
|    └── 01_optimal_transport.R
|    └── 02_gaussian.R
|    └── 03_03_transport-grid.R
|    └── 04_algorithm-5.R
|    └── 05_wrong-causal-assumptions.R
|    └── 05_wrong-causal-assumptions-ot.py
|    └── 06_real-data-lawschool.R
|    └── 06_real-data-lawschool-ot.py
|    └── 07_real-data-adult.R
|    └── 07_real-data-adult-ot.py
|    └── 08_real-data-compas.R
|    └── 08_read-data-compas-ot.py
|    └── sequential_transport.Rproj
├ ── README.md
```

To replicate the codes, provided you have installed R and Rstudio on your computer, and have also installed python (for multivariate optimal transport), double click on the following file to open RStudio (so that the correct working directory is set): `./scripts/sequential_transport.Rproj`. Then, you can open the scripts from RStudio.


## Replication of Figures and Tables

The following Figures from the paper are produced using R:

- Figure 4: 04_Algorithm-5.R
- Figure 6: 02_gaussian.R
- Figure 7: 04_Algorithm-5.R
- Figure 8: 06_real-data-lawschool.R
- Figure 9: 06_real-data-lawschool.R
- Figure 10 (Appendix): 01_optimal_transport.R
- Figure 11 (Appendix): 01_optimal_transport.R
- Figure 12 (Appendix): 02_gaussian.R
- Figure 13 (Appendix): 02_gaussian.R
- Figure 16 (Appendix): 07_real-data-adult.R (left) and 08_real-data-compas.R (right)
- Figure 18 (Appendix): 05_wrong-causal-assumptions.R
- Figure 19 (Appendix): 05_wrong-causal-assumptions.R

The other figures are tikz pictures showing DAGs.

The following Tables are produced using R:

- Table 1: 06_real-data-lawschool.R
- Table 2 (Appendix): 07_real-data-adult.R (top) 08_real-data-compas.R (bottom)
- Table 3 (Appendix): 06_real-data-lawschool.R


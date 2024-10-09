# Replication materials

This repository contains the replication materials for the article titled 
_Sequential Conditional Transport on Probabilistic Graphs for Interpretable Counterfactual Fairness_.

The working paper is on arXiv: <https://arxiv.org/abs/2408.03425>

A companion html e-book that explains the codes and provides supplementary materials can be accessed at the following url: <https://fer-agathe.github.io/sequential_transport>.

## Reference of the paper (preprint)

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

## How to replicate

To replicate the codes, provided you have installed R and Rstudio on your computer, double click on the following file to open RStudio (so that the correct working directory is set): `./scripts/sequential_transport.Rproj`. Then, you can open the scripts in RStudio.

The following structure is adopted.

```
Supplementary-materials
├ ── docs
│    └── index.html
│    └── ...
├ ── data
├ ── functions
│    └── utils.R
│    └── graphs.R
├ ── scripts
|    └── 01_optimal_transport.R
|    └── 02_gaussian.R
|    └── 03_regression.R
|    └── 04_1_law_data.R
|    └── 04_2_law_classifier.R
|    └── 04_3_law_fairadapt.R
|    └── 04_4a_law_optimal_transport.R
|    └── 04_4b_law_optimal_transport.R
|    └── 04_5_law_sequential_transport.R
|    └── 04_6_comparison.R
|    └── sequential_transport.Rproj
```


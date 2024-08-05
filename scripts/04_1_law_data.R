# Law school dataset
# Required packages----
library(tidyverse)

# Graphs
font_main = font_title = 'Times New Roman'
extrafont::loadfonts(quiet = T)
face_text='plain'
face_title='plain'
size_title = 14
size_text = 11
legend_size = 11

global_theme <- function() {
  theme_minimal() %+replace%
    theme(
      text = element_text(family = font_main, size = size_text, face = face_text),
      legend.text = element_text(family = font_main, size = legend_size),
      axis.text = element_text(size = size_text, face = face_text),
      plot.title = element_text(
        family = font_title,
        size = size_title,
        hjust = 0.5
      ),
      plot.subtitle = element_text(hjust = 0.5)
    )
}

# Seed
set.seed(2025)

# Data----

# This law school dataset contains information collected through a survey
# conducted from 1991 through 1997 by the Law School Admission Council across
# 163 law schools in the United States of America (Wightman (1998)).
# In total, 21,790 law students were tracked through law school, graduation,
# and sittings for bar exams.

# We use the formatted data from De Lara et al. (2024), also used by
# Lara et al. (2021)
# (Github associated to the paper: <https://github.com/lucasdelara/PI-Fair.git>)

# Each row from the raw data gives information for a student.
# The following characteristics are available:

# - `race`: Race of the student (character: Amerindian, Asian, Black, Hispanic,
#    Mexican, Other, Puertorican, White).
# - `sex`: Sex of the student (numeric: 1 female, 2 male).
# - `LSAT`: LSAT score received by the student (numeric).
# - `UGPA`: Undergraduate GPA of the student (numeric).
# - `region_first`: region in which the student took their first bar examination
#    (Far West, Great Lakes, Midsouth, Midwest, Mountain West, Northeast,
#    New England, Northwest, South Central, South East) (character)
# - `ZFYA`: standardized first-year law school grades (first year average grade,
#    FYA) (numeric).
# - `sander_index`: Sander index of the student: weighted average of normalized
#    UGPA and LSAT scores (however, no details are given for this specific
#    dataset, see Sander 2OO4, p. 393) (numeric)
# - `first_pf`: Probably a binary variable that indicates whether the student
#    passed on their first trial ? No information is given about this
#    variable... (numeric 0/1).

# References
# - De Lara, Lucas, Alberto González-Sanz, Nicholas Asher, and Jean-Michel
#   Loubes. 2021. “Transport-Based Counterfactual Models.” arXiv 2108.13025.
# - De Lara, Lucas, Alberto González-Sanz, Nicholas Asher, Laurent Risser, and
#   Jean-Michel Loubes. 2024. “Transport-Based Counterfactual Models.”
#   Journal of Machine Learning Research 25 (136): 1–59.
# - Sander, Richard H. 2004. “A Systemic Analysis of Affirmative Action in
#   American Law Schools.” Stan. L. Rev. 57: 367.
# - Wightman, Linda F. 1998. “LSAC National Longitudinal Bar Passage Study.
#   LSAC Research Report Series.”
#   In. https://api.semanticscholar.org/CorpusID:151073942.


# Pre-Processing----
df <- read_csv('../data/law_data.csv')

df <- df |>
  select(
    race, # we can take S = race (white/black)
    sex,  # or S = gender
    LSAT,
    UGPA,
    ZFYA  # Y
  )

# Table for S = race
df_race <- df |>
  select(
    race,
    UGPA,
    LSAT,
    ZFYA
  ) |>
  filter(
    race %in% c("Black", "White")
  ) |>
  rename(
    S = race,
    X1 = UGPA,
    X2 = LSAT,
    Y = ZFYA
  ) |>  # no NA values
  mutate(
    S = as.factor(S)
  )

# Table for S = gender
df_gender <- df |>
  select(
    sex,
    UGPA,
    LSAT,
    ZFYA
  ) |>
  rename(
    S = sex,
    X1 = UGPA,
    X2 = LSAT,
    Y = ZFYA
  ) |>  # no NA values
  mutate(
    S = as.factor(S)
  )

# Distribution of the standardized first-year law school grades among the two
# groups, when S is the race
ggplot(
  data = df_race,
  mapping = aes(x = Y, fill = S)
) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.5
  ) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Race",
    x = "Y",
    y = "Density"
  ) +
  global_theme()


# Distribution of the standardized first-year law school grades among the two
# groups, when S is the gender
ggplot(
  data = df_gender,
  mapping = aes(x = Y, fill = S)) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.5
  ) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Gender",
    x = "Y",
    y = "Density"
  ) +
  global_theme()

# Causal Graph----
variables <- colnames(df_race)
# Adjacency matrix: upper triangular
adj <- matrix(
  c(0, 1, 1, 1,
    0, 0, 1, 1,
    0, 0, 0, 1,
    0, 0, 0, 0),
  ncol = length(variables),
  dimnames = rep(list(variables), 2),
  byrow = TRUE
)

causal_graph <- fairadapt::graphModel(adj)
plot(causal_graph)

# Topological order
top_order <- variables
top_order

# Saving Objects----
save(df_race, file = "../data/df_race.rda")
save(df_gender, file = "../data/df_gender.rda")
save(adj, file = "../data/adj.rda")

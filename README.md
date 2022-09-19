
This repository contains materials for the journal article:

> Lim, S., Jahng, S. (2019). Determining the number of factors using parallel analysis and its recent variants. *Psychological Methods, 24*(4), 452-467. https://doi.org/10.1037/met0000230

This branch `validate_noise_size` contains materials for an updated version of the simulation, where the generation logic for population correlation matrices was updated to satisfy the condition:

- `k` denotes the true number of factors
- `lambda_k` denotes the `k`-th eigenvalue of the main correlation matrix (without raising the diagonals to ones)
- `lambda_e` denotes the first eigenvalue of the weighted noise matrix (without raising the diagonals to ones)
- condition: `lambda_k >= lambda_e` must hold

The main goal of this update was to address one of the arguments presented by Achim (2021):

> Achim, A. (2021). Determining the number of factors using parallel analysis and its recent variants: Comment on Lim and Jahng (2019). *Psychological Methods, 26*(1), 69–73. https://doi.org/10.1037/met0000269

**Please do not cite the contents of this branch.** The updated dataset has not been used in any published article; the updates to the simulation script and results have not been peer reviewed.

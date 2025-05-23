# Garden by Numbers – Designing the Optimal Garden

This project applies a variety of optimisation algorithms to design a garden that is optimal in terms of aesthetics. It also helps determine which plant sizes to purchase in order to maximise immediate visual impact, stay within a specified budget, and minimise ongoing maintenance effort.

## Purpose

This project was created as part of a master's-level optimisation course. It combines simulation, heuristics, and linear programming to solve real-world inspired aesthetic and budget-constrained garden layout problems.

## Technologies & Packages Used

- **Language**: R, LaTeX
- **R Packages**:  
  `dplyr`, `tidyr`, `ggplot2`, `rglpk`, `slam`, `tibble`, `purrr`, `furrr`

## Project Structure

| Path                                   | Description                                                   |
|----------------------------------------|---------------------------------------------------------------|
| `data/`                                | Input plant dataset (Excel format)                              |
| `images/`                              | Images used for illustration in the report                    |
| `plots/`                               | Generated plots and visual outputs                            |
| `results/`                             | CSV result files from algorithm runs                          |
| `Garden_utils.R`                       | Core functions used by the SA and GA models                   |
| `Simulated Annealing.Rmd`             | Simulated Annealing optimisation notebook                     |
| `Genetic Algorithm.Rmd`               | Genetic Algorithm optimisation notebook                       |
| `Mixed Integer Linear Programming.Rmd`| MILP model for mature plant selection with maximal visual impact                        |
| `Multi-objective Linear Programming.Rmd` | Multi-objective LP using Archimedean and Chebychev methods |
| `Optimisation Assignment Final Report.pdf` | Full final written report                                |
| `README.md`                            | Project overview and documentation                            |


## How to Run the Project

1. Download the repository to your local machine.
2. Open the `.Rmd` files in RStudio.
3. It is strongly recommended to run **individual code chunks** rather than knitting the entire document, as some chunks may take up to 2 hours to execute. These are clearly marked in the files.
4. Run the Simulated Annealing (`SA`) and Genetic Algorithm (`GA`) files before the Mixed Integer Linear Programming (`MILP`) and Multi-objective Linear Programming (`MOLP`) files, as the latter depend on the outputs of the former.

## Input Data

The main input dataset is located in the `data/` folder. It includes a list of 50 plants with characteristics such as:
- Height at maturity
- Flower colour
- Flowering time
- Category (e.g., groundcover, shrub, bulb)

## Author

Developed by [Annie0619](https://github.com/Annie0619)

## License

This project does not currently specify a license. Please contact the author if you would like to reuse or adapt the contents.


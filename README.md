
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


packages <- c("mlbench", "viridis", "rmarkdown", "gtools", "MASS")
install.packages(setdiff(packages, rownames(installed.packages())))

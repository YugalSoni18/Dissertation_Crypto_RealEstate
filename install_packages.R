# Install required R packages
pkgs <- c("tidyverse","tidytext","readr","dplyr","stringr","forcats","ggplot2","tidyr")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if(length(to_install)) install.packages(to_install, repos="https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only=TRUE))

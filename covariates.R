# https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/

install.packages("devtools")
library("devtools")
library("roxygen2")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
document()
setwd("..")
install("covariates")

# install from github:
install_github('shenan1/covariates')

# prefix functions with namespace covariates:::

# git add R/* man/*
# git commit
# include message: press i to insert message
#	git push
# git rm fname
# file will also be deleted from the filesystem
# git mv fname newfname

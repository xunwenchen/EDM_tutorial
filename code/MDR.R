# note MDR S-map method is using older version of rEDM package
# so we remove new version and install the old version as follows
detach("package:rEDM", unload = TRUE)
remove.packages("rEDM")
install.packages("rEDM_1.2.3.tar.gz", repos = NULL, type = "source") # install from local file

library(rEDM)
# check version
packageVersion("rEDM")




## ----install from BioConductor, eval=FALSE------------------------------------
#  if (!requireNamespace("BiocManager", quietly = TRUE))
#      install.packages("BiocManager")
#  
#  BiocManager::install("wpm")

## ----launch wpm, eval = FALSE-------------------------------------------------
#  library(wpm)
#  wpm()

## ---- echo=FALSE--------------------------------------------------------------
knitr::kable(data.frame("Sample" = c("s1","s2","s3","s4")))

## ---- echo=FALSE--------------------------------------------------------------
knitr::kable(
    data.frame("Sample" = c("s1","s2","s3","s4"), "Group" = c("A","A","B","C"))
)

## ----SessionInfo--------------------------------------------------------------
sessionInfo()


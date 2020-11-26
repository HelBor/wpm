## ----launch wpm, eval = FALSE-------------------------------------------------
#  library(wpm)
#  wpm()

## ---- echo=FALSE--------------------------------------------------------------
knitr::kable(data.frame("Sample" = c("s1","s2","s3","s4")))

## ---- echo=FALSE--------------------------------------------------------------
knitr::kable(
    data.frame("Sample" = c("s1","s2","s3","s4"), "Type" = c("A","A","B","C"),
               "Treatment" = c("trt1","tr1","Ctrl","Ctrl"))
)

## ----convert CSV file, eval = FALSE-------------------------------------------
#  imported_csv <- wpm::convertCSV("path-to-CSV-file")

## ----create an MSnSet object--------------------------------------------------
sample_names <- c("s1","s2","s3","s4", "s5")
M <- matrix(NA, nrow = 4, ncol = 5)
colnames(M) <- sample_names
rownames(M) <- paste0("id", LETTERS[1:4])
pd <- data.frame(Environment = rep_len(LETTERS[1:3], 5),
                 Category = rep_len(1:2, 5), row.names = sample_names)
rownames(pd) <- colnames(M)
my_MSnSet_object <- MSnbase::MSnSet(exprs = M,pData =  pd)

## ----convert ESet/MSnSet object-----------------------------------------------
df <- wpm::convertESet(my_MSnSet_object, "Environment")

## ----convert SummarizedExperiment object--------------------------------------
nrows <- 200
ncols <- 6
counts <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
colData <- data.frame(Treatment=rep(c("ChIP", "Input"), 3),
                      row.names=LETTERS[1:6])
se <- SummarizedExperiment::SummarizedExperiment(assays=list(counts=counts),
                                                 colData=colData)
df <- wpm::convertSE(se, "Treatment")

## ----run wpm with a CSV file, eval=FALSE--------------------------------------
#  wpm_result <- wpm::wrapperWPM(user_df = imported_csv$df_wpm,
#              plate_dims = list(8,12),
#              nb_plates = 1,
#              forbidden_wells = "A1,A2,A3",
#              fixed_wells = "B1,B2",
#              spatial_constraint = "NS")

## ----run wpm------------------------------------------------------------------
wpm_result <- wpm::wrapperWPM(user_df = df,
            plate_dims = list(8,12),
            nb_plates = 1,
            forbidden_wells = "A1,A2,A3",
            fixed_wells = "B1,B2",
            spatial_constraint = "NS")

## ----visualize plate map------------------------------------------------------
drawned_map <- wpm::drawMap(df = wpm_result,
        sample_gps = length(levels(as.factor(colData$Treatment))),
        gp_levels = gp_lvl <- levels(as.factor(colData$Treatment)),
        plate_lines = 8,
        plate_cols = 12,
        project_title = "my Project Title")

## ----see the map--------------------------------------------------------------
drawned_map

## ----save map plot, eval=FALSE------------------------------------------------
#  ggplot2::ggsave(
#      filename = "my file name",
#      plot = drawned_map,
#      width = 10,
#      height = 7,
#      units = "in"
#  )

## ---- eval = FALSE------------------------------------------------------------
#  numberOfThePlate <- 1
#  drawned_map <- wpm::drawMap(df = wpm_result[[numberOfThePlate]],
#          sample_gps = length(levels(as.factor(pd$Environment))),
#          gp_levels = gp_lvl <- levels(as.factor(pd$Environment)),
#          plate_lines = 8,
#          plate_cols = 12,
#          project_title = "my Project Title")

## ----SessionInfo--------------------------------------------------------------
sessionInfo()


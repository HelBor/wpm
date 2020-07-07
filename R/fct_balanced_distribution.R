##' Makes a balanced distribution of the elements between several plates.
##'
##' @description This function makes it possible to distribute the samples
##' equitably on several plates, taking into account the numbers in the groups
##' (if there are any). This means that, for example, if 2 plates are to be
##' filled, then 50% of each group will be assigned to each plate. More
##' generally, all the plates are assigned the same number of elements. When
##' the numbers do not allow it (in particular when the total number of
##' elements to be allocated is not a multiple of the number of plates),
##' there will be a slight difference in the number of samples on the plates.
##'
##' @param d the user dataframe
##' @param nb_p the number of plates to fill
##' @param df_max_size the maximum number of samples that can be placed on the
##' current plate
##'
##' @return a list of dataframes each corresponding to a plate to fill.
##' @importFrom rlang .data
balancedGrpDistrib <- function(d, nb_p, df_max_size){

    grouped <- dplyr::group_by(d, .data$Group)
    # vector containing the workforce for each group
    workforces <- dplyr::group_size(grouped)
    test <- dplyr::group_split(grouped)
    # staff per group for each plate
    w <- round(workforces/nb_p)

    toReturn <- list()
    missing <- rep(list(0), times = nb_p)

    for (p in seq_len(nb_p) ) {
        df <- data.frame(matrix(ncol = ncol(grouped)))
        names(df) <- names(grouped)

        for (g in seq_along(workforces) ) {
            if ( nrow(test[[g]]) < w[g]) {
                df <- rbind(df, as.data.frame(test[[g]]))
                missing[[p]] <- missing[[p]] + as.numeric(w[g] - nrow(test[[g]]))
                test[[g]] <- data.frame("Sample" = NA, "Group" = NA, "ID" = NA,
                                        "Well" = NA, "Status" = NA, "Row" = NA,
                                        "Column" = NA)
            }else{
                selected <- resample(test[[g]]$ID, size = w[g])
                wg <- as.data.frame(test[[g]][test[[g]]$ID %in% selected, ])
                test[[g]] <- test[[g]][!test[[g]]$ID %in% selected, ]
                df <- rbind(df, wg)
            }


        }
        df <- df[!is.na(df$ID), ]
        df$Group <- as.factor(df$Group)
        df$Status <- as.factor(df$Status)
        toReturn[[p]] <- df
    }

    m <- dplyr::bind_rows(test)
    m <- m[!is.na(m$ID), ]

    ## as long as samples remain unassigned to a plate...
    while (nrow(m) != 0) {

        ## ... they are assigned to the plate with the smallest number of samples
        ## without exceeding the maximum authorized number of samples per plate.
        incomplete_plate <- which.min(
            unlist(lapply(toReturn, function(x) nrow(x))))

        incomplete_size <- nrow(toReturn[[incomplete_plate]])

        if (incomplete_size < df_max_size) {
            if ( (df_max_size - incomplete_size) > nrow(m)) {
                nb_to_pick <- nrow(m)
            }else{
                nb_to_pick <- df_max_size - incomplete_size
            }
            toTake <- resample(m$ID, size = (nb_to_pick))
            totake_df <- m[which(m$ID %in% toTake), ]
            toReturn[[incomplete_plate]] <- rbind(toReturn[[incomplete_plate]],
                                                  totake_df)
            m <- subset(m, !(m$ID %in% toTake))

        }else if (incomplete_size >= df_max_size) {
            maxi <- which.max(unlist(lapply(toReturn, function(x) nrow(x))))

            if (incomplete_size < nrow(toReturn[[maxi]])) {
                toTake <- resample(m$ID, size = (nrow(toReturn[[maxi]]) - incomplete_size))
                totake_df <- m[which(m$ID %in% toTake), ]
                toReturn[[incomplete_plate]] <- rbind(toReturn[[incomplete_plate]], totake_df)
                m <- subset(m, !(m$ID %in% toTake))
            }else if (incomplete_size == nrow(toReturn[[maxi]])) {
                toReturn[[incomplete_plate]] <- rbind(toReturn[[incomplete_plate]], m)
                m <- m[!m$ID, ]
            }
        }
    }
    logging::loginfo("nrow in toReturn: %s ",
                     unlist(lapply(toReturn, function(x) nrow(x))),
                     logger = "balancedGrpDistrib")
    return(toReturn)

}

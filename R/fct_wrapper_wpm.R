##' Check the inputs for the wrapper function
##' 
##' @description Checks if all the inputs given to the function WrapperWPM are 
##' correct and intercompatible.
##' @param user_df expected dataframe, returns adapted message error
##' @param plate_dims expected list of plate dimensions (rows and columns)
##' @param nb_plates expected number of plates
##' @param spatial_constraint expected character for spatial constraint
##' @param max_iteration expected number of iterations
##' @return returns an error message if a problem is found with some parameter.
checkWpmInputs <- function(user_df, plate_dims, nb_plates, spatial_constraint,
                           max_iteration){
    if (!methods::is(user_df, "data.frame")) {
        stop("wrong user_df parameter: Please provide a valid dataframe as 
            obtained with the convertCSv or convertESet function.",
             call. = FALSE)
    }else if (!all(c("Sample", "Group", "ID") %in% colnames(user_df))) {
        stop("wrong the user_df parameter: Please provide a valid dataframe
        as obtained with the convertCSv or convertESet function.", call. = FALSE)
    }
    if (!methods::is(plate_dims, "list")) {
        stop("wrong plate_dims parameter: Please give a list containing 2 
            numbers.", call. = FALSE)
    }else{
        if (!methods::is(plate_dims[[1]], "numeric")
           | !methods::is(plate_dims[[2]], "numeric")) {
            stop("wrong plate_dims parameter: Please give a list containing 2 
            numbers.", call. = FALSE)
        }
    }
    if (!methods::is(nb_plates, "numeric")) {
        stop("wrong nb_plates parameter: invalid argument, please provide 
            a number.", call. = FALSE)
    }
    if (!spatial_constraint %in% c("none","NEWS","NS","WE")) {
        stop("wrong spatial_constraint parameter: wrong mode selected. 
            Please choose a valid mod between 'none', 'NEWS', 'WE' or 'NS' ",
            call. = FALSE)
    }
    if (!methods::is(max_iteration, "numeric")) {
        stop("wrong max_iteration parameter: please give a number.",
            call. = FALSE)
    }
} 


##' Generate plate plans in a single step
##'
##' @description Wrapper function that generates plate plans like the wpm
##' shiny application. This feature allows the user to use the wpm package
##' from the command line rather than going through a web application.
##' @param user_df dataframe containing user data obtained with the
##' `convertCSV()` or `convertESet()` functions.
##' @param plate_dims list, containing 2 values: the first is the number of
##' plate's lines and second is the number of plate's columns.
##' @param nb_plates numeric, corresponds to the number of plates to fill
##' @param forbidden_wells character, the wells that will not be used at all
##' for the experiment. This argument needs to be a character string giving the
##'  wells coordinates of the form "LetterNumber" (eg. "A1" for the well
##'  positionned in the first row/ first column of the plate).
##' @param blank_wells character, the wells that will be used during
##'  experiment but without biological sample in it. Same input structure as for
##'  forbidden_wells parameter.
##' @param QC_wells character, the wells that will be used for Quality Control
##'  samples during the Experiment. Same input structure as for forbidden_wells
##'  parameter.
##' @param spatial_constraint character, is the spatial constraint used to
##' place the samples on the plate. It can also be called neighborhood
##' constraint. Currently, the possible values are "none", "NS" (for
##' North-South), "WE" (for West-East) and "NEWS" (North-South-East-West).
##' @param max_iteration numeric, maximal number of attemps for wpm to find a
##' valid solution.
##' @return a dataframe if wpm finds a solution.
##' @examples
##' # create a MSnSet toy example
##' sample_names <- c("s1","s2","s3","s4", "s5")
##' M <- matrix(NA, nrow = 4, ncol = 5)
##' colnames(M) <- sample_names
##' rownames(M) <- paste0("id", LETTERS[1:4])
##' pd <- data.frame(Environment = rep_len(LETTERS[1:3], 5),
##'                  Category = rep_len(1:2, 5), row.names = sample_names)
##' rownames(pd) <- colnames(M)
##' x <- MSnbase::MSnSet(exprs = M,pData =  pd)
##' # convert it to a valid dataframe for wpm
##' df <- convertESet(x, "Environment")
##' # run wpm on the toy example
##' wrapperWPM(user_df = df, plate_dims = list(8,12), nb_plates = 1,
##'            forbidden_wells = "A1,A2,A3", QC_wells = "B1,B2",
##'            spatial_constraint = "NS")
##' @export
wrapperWPM <- function(user_df, plate_dims, nb_plates, forbidden_wells = NULL,
                       blank_wells = NULL, QC_wells = NULL,
                       spatial_constraint = "none", max_iteration = 20){

    checkWpmInputs(user_df, plate_dims, nb_plates,
                   spatial_constraint, max_iteration)

    user_df$Group <- as.factor(user_df$Group)
    user_df$Well <- as.character(NA)
    user_df$Status <- as.factor("toRandom")
    user_df$Row <- as.numeric(NA)
    user_df$Column <- as.numeric(NA)

    ## Convert special wells into valid dataframes
    if (!is.null(forbidden_wells)) {
        if (!methods::is(forbidden_wells, "character")) {
            stop("wrong forbidden_wells parameter: please provide a character 
                string.", call. = FALSE)
        }
        fw <- convertVector2Df(forbidden_wells, plate_dims[[1]], 
                               plate_dims[[2]],"forbidden")
    }else{
        fw <- NULL
    }
    if (!is.null(blank_wells)) {
        if (!methods::is(blank_wells, "character")) {
            stop("wrong blank_wells parameter: please provide a character 
                string.", call. = FALSE)
        }
        bw <- convertVector2Df(blank_wells, plate_dims[[1]], 
                               plate_dims[[2]],"blank")
    }else{
        bw <- NULL
    }
    if (!is.null(QC_wells)) {
        if (!methods::is(QC_wells, "character")) {
            stop("wrong QC_wells parameter: please provide a character string.",
                call. = FALSE)
        }
        QCw <- convertVector2Df(QC_wells, plate_dims[[1]], 
                                plate_dims[[2]],"notRandom")
    }else{
        QCw <- NULL
    }

    tNbW <- plate_dims[[1]] * plate_dims[[2]] * nb_plates
    ## Balanced Distribution
    special_wells <- joinDataframes(
        forbidden_w = fw, blank_w = bw, notRandom_w = QCw,
        nb_samples = nrow(user_df), totalNbWells = tNbW, nb_p = nb_plates)
    if (methods::is(special_wells, "data.frame")) {
        ## Backtracking part
        print(special_wells)
        logging::loginfo("max_iteration: %s", max_iteration)
        output <- backtracking(
            max_iter = max_iteration, user_data = user_df,
            wells = special_wells, rows = plate_dims[[1]],
            columns = plate_dims[[2]], nb_plates = nb_plates, 
            constraint = spatial_constraint, prog = NULL)
    }else{
        output <- special_wells
    }
    return(output)
}

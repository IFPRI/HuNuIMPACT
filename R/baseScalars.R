#' baseScalars
#'
#' @param baseyr Base year for IMPACT simulation. Currently 2020
#'
#' @return Hunger scalars for pop at risk calculation
#' @export
#'
#' @examples
#'\dontrun{
#'baseScalars()
#'}
baseScalars <- function(baseyr = 2020) {
    ls <- list()
    ls["newOcal"] <- NULL
    ls["sharemin"] <- 1
    ls["sharemax"] <- 100
    ls["SAROHint"] <- 254.54
    ls["x_param"] <- -269.59
    ls["x_sqrd_param"] <- 71.795
    # For controlling quadratic nature of model specification
    ls["x_max"] <- 1.877498
    ls["y_min"] <- 1
    ls[["baseyr"]] <- baseyr
    return(ls)
}

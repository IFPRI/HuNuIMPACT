#' readDrivers
#'
#' @param gdx GDX file from an IMPACT run
#' @param var Which variable to read. Use "pop" or "gdp" or "food"
#' @importFrom DOORMAT readGDX
#' @return Returns population, GDP(pc) or food availability
#' @export
#'
#' @examples
#'\dontrun{
#'readDrivers(gdx, var = "pop")
#'}
readDrivers <- function(gdx, var) {
    if (var == "pop") name <- "popx0"
    if (var == "gdp") name <- "gdpx0"
    if (var == "food") name <- "qfx0"
    df <- readGDX(gdx = gdx, name = name, quick_df = FALSE)$data
    df <- df[, !names(df) %in% "model"]
    return(df)
}

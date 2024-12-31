#' calcAggregate
#'
#' @param gdx GDX file from an IMPACT run
#' @param baseyr Base year for simulation
#' @param mapping Vector of mapping files with their extension. Defaults to
#' "aggreg_standard.csv". You can add multiple mappings with
#'
#' @importFrom janitor get_dupes
#' @importFrom collapse join
#' @import dplyr
#' @return Aggregate indicators for Hunger and Nutirition
#' @export
#'
#' @examples
#' \dontrun{
#' calcAggregate()
#' }
calcAggregate <- function(gdx, baseyr, mapping = c("aggreg_standard.csv")) {
    mapdf <- NULL
    scenario <- gsub(pattern = "\\.gdx", replacement = "", x = basename(gdx))
    dfl <- list()
    for (i in mapping) {
        temp <- read.csv(file = system.file("extdata", i, package = "HuNuIMPACT"),
                          header = TRUE,
                          sep = ";")
        mapdf <- rbind(mapdf, temp)
        # TBD  ----> Excel
    }
    if(nrow(get_dupes(mapdf)) > 0) stop("Duplicates detected in mapping")
    df <- CoreCalc(gdx = gdx, baseyr = baseyr)

    # POPX0 ----
    POPX0 <- df[["population"]]
    cols <- names(POPX0)
    POPX0 <- join(x = POPX0,
                 y = mapdf,
                 on = "cty",
                 multiple = TRUE)
    POPX0 <- POPX0 %>%
        group_by(reg, yrs) %>%
        summarise(value = sum(value, na.rm = TRUE))
    POPX0$scenario <- scenario
    dfl[["POPX0_agg"]] <- POPX0

    # PopulationAtRisk ----
    PopulationAtRisk <- df[["PopulationAtRisk"]]
    PopulationAtRisk <- join(x = PopulationAtRisk,
                             y = mapdf,
                             on = "cty",
                             multiple = TRUE)
    PopulationAtRisk <- PopulationAtRisk %>%
        group_by(reg, yrs) %>%
        summarise(value = sum(value, na.rm = TRUE))
    PopulationAtRisk$scenario <- scenario
    dfl[["PopulationAtRisk_agg"]] <- PopulationAtRisk

    # ShareAtRisk ----
    ShareAtRisk <- join(x = PopulationAtRisk,
                        y = POPX0,
                        on = c("reg", "yrs"))
    cols <- names(PopulationAtRisk)
    ShareAtRisk$value <- 100 * ShareAtRisk$value / ShareAtRisk$value_POPX0
    ShareAtRisk <- ShareAtRisk[,cols]
    dfl[["ShareAtRisk_agg"]] <- ShareAtRisk

    # Hunger Years ----

    ## Country level ----
    HungerYears <- df[["PopulationAtRisk"]]
    HungerYears <- HungerYears %>%
        group_by(cty) %>%
        summarise(value = sum(value, na.rm = TRUE))
    HungerYears$scenario <- scenario
    dfl[["HungerYears"]] <- HungerYears

    ## Aggregate level ----
    HungerYears <- join(x = HungerYears,
                        y = mapdf,
                        on = "cty",
                        multiple = TRUE)
    HungerYears <- HungerYears %>%
        group_by(reg) %>%
        summarise(value = sum(value, na.rm = TRUE))
    HungerYears$scenario <- scenario
    dfl[["HungerYears_agg"]] <- HungerYears

    # FoodAvailability_nutrients ----
    FoodAvailability_nutrients <- df[["FoodAvailability"]]
}

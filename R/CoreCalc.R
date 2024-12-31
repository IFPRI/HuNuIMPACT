#' Core Calculations for this module
#'
#' @param gdx GDX file from an IMPACT run
#' @param baseyr Base year for simulation. Currently 2020.
#'
#' @importFrom collapse join
#' @import dplyr
#' @return Core calculation return
#' @export
#'
#' @examples
#'\dontrun{
#'CoreCalc(gdx)
#'}
CoreCalc <- function(gdx, baseyr = 2020) {
    dfl <- list()

    baseScalars <- baseScalars()

    # FoodAvailability ----
    fa_annual <- readDrivers(gdx = gdx, var = "food")
    pop <- readDrivers(gdx = gdx, var = "pop")
    FoodAvailability <- join(x = fa_annual[, !names(fa_annual) %in% "description"],
                             y = pop[, !names(pop) %in% "description"],
                             on = c("cty", "yrs"),
                             suffix = c(".fa", ".pop")
                             )
    FoodAvailability$value <- FoodAvailability$value.fa / FoodAvailability$value.pop
    FoodAvailability$value <- FoodAvailability$value / 365 #per day
    FoodAvailability <- FoodAvailability[, !names(FoodAvailability) %in% c("value.fa", "value.pop")]
    dfl[["FoodAvailability"]] <- FoodAvailability

    # population ----
    dfl[["population"]] <- pop[, !names(pop) %in% "description"]

    # TotalCalorie ----
    KCALperKG <- readGDX(gdx = system.file("extdata",
                                           "Simple_KCAL.gdx",
                                           package = "HuNuIMPACT"),
                         name = "KCALperKG", quick_df = TRUE)$data
    TotalOtherCalories <- readGDX(gdx = system.file("extdata",
                                                    "Simple_KCAL.gdx",
                                                    package = "HuNuIMPACT"),
                                  name = "OtherKCAL", quick_df = TRUE)$data
    TotalCalorie <- join(x = FoodAvailability,
                         y = KCALperKG,
                         suffix = c(".fa", "kcalpkg"),
                         on = c("c", "cty"))
    TotalCalorie$value <- TotalCalorie$value.fa * TotalCalorie$valuekcalpkg
    TotalCalorie <- TotalCalorie[, names(FoodAvailability)]
    dfl[["TotalCalorie"]] <- TotalCalorie

    # PCCalories ----
    PCCalories <- join(x = TotalCalorie %>%
                           group_by(cty, yrs) %>%
                           summarise(value = sum(value, na.rm = TRUE)),
                       y = TotalOtherCalories,
                       on = c("cty"),
                       suffix = c(".tot", ".oth"))
    PCCalories$value.oth[is.na(PCCalories$value.oth)] <- 0
    PCCalories$value <- PCCalories$value.tot + PCCalories$value.oth
    PCCalories <- PCCalories[, c("cty", "yrs", "value")]
    dfl[["PCCalories"]] <- PCCalories

    # ShareKCAL ----
    ShareKCAL <- join(x = TotalCalorie,
                      y = PCCalories,
                      on = c("cty", "yrs"),
                      suffix = c(".tot", ".pcc"))
    ShareKCAL$value <- ShareKCAL$value.tot / ShareKCAL$value.pcc
    ShareKCAL <- ShareKCAL[!is.na(ShareKCAL$value), ]
    ShareKCAL <- ShareKCAL[, names(TotalCalorie)]
    dfl[["ShareKCAL"]] <- ShareKCAL

    # CheckSumShares ----
    CheckSumShares <- ShareKCAL %>%
        group_by(cty, yrs) %>%
        summarise(value = sum(value, na.rm = TRUE))
    dfl[["CheckSumShares"]] <- CheckSumShares
    if (any(CheckSumShares$value != 1)) {
        message("-----> Kcal shares do not equal 1")
        message("-----> ... see 'CheckSumShares' dataframe from returned list.")
    }

    # PerCapKCAL ----
    PerCapKCAL <- PCCalories
    dfl[["PerCapKCAL"]] <- PerCapKCAL

    # TotalKCal ----
    TotalKCal <- join(x = PCCalories,
                      y = pop[, !names(pop) %in% "description"],
                      on = c("cty", "yrs"),
                      suffix = c(".pccal", ".pop"))
    TotalKCal$value <- TotalKCal$value.pccal * TotalKCal$value.pop
    TotalKCal <- TotalKCal[, names(PCCalories)]
    dfl[["TotalKCal"]] <- TotalKCal

    # PerCapKCAL_com ----
    PerCapKCAL_com <- join(x = PerCapKCAL,
                           y = ShareKCAL,
                           on = c("cty", "yrs"))
    PerCapKCAL_com$value <- PerCapKCAL_com$value * PerCapKCAL_com$value_ShareKCAL
    PerCapKCAL_com <- PerCapKCAL_com[, names(ShareKCAL)]
    dfl[["PerCapKCAL_com"]] <- PerCapKCAL_com

    # This code calculates the "Share at risk of hunger" and the consequent
    # number at risk of hunger according to G. Fischer's IIASA World Food System
    # model used by IIASA and FAO.  See following references:
    # Fischer et al
    # Socio-economic and climate change impacts on agriculture: an integrated assessment, 1990-2080
    # Phil. Trans. R. Soc. B 2005 360, 2067-2083
    # doi: 10.1098/rstb.2005.1744
    # Plus others, just search "Fischer" and "share at risk of hunger" on google scholar.

    # Relative Kcal ----
    MinKCAL <- readGDX(gdx = system.file("extdata",
                                         "PoU_params.gdx",
                                         package = "HuNuIMPACT"),
                       name = "MinKCAL", quick_df = TRUE)$data
    EstimationError <- readGDX(gdx = system.file("extdata",
                                                 "PoU_params.gdx",
                                                 package = "HuNuIMPACT"),
                               name = "EstimationError", quick_df = TRUE)$data
    RelativeKCAL <- join(x = PCCalories, y = MinKCAL, on = "cty")
    RelativeKCAL$value <- RelativeKCAL$value / RelativeKCAL$value_MinKCAL
    RelativeKCAL <- RelativeKCAL[, names(PCCalories)]
    dfl[["RelativeKCAL"]] <- RelativeKCAL

    # Share at risk ---
    ShareAtRisk <- join(x = RelativeKCAL,
                        y = EstimationError,
                        on = "cty",
                        suffix = c(".relKcal", ".Ee"))
    ShareAtRisk$x_param <- baseScalars[["x_param"]]
    ShareAtRisk$SAROHint <- baseScalars[["SAROHint"]]
    ShareAtRisk$x_sqrd_param <- baseScalars[["x_sqrd_param"]]
    ShareAtRisk$value <- ShareAtRisk$SAROHint +
        (ShareAtRisk$value.relKcal * ShareAtRisk$x_param) +
        (ShareAtRisk$x_sqrd_param * (ShareAtRisk$value.relKcal^2)) +
        ShareAtRisk$value.Ee

    # Apply conditionals that will control behavior of calculation if
    # RelativeKCAL(cty) surpasses where the 1st derivative equals zero
    # begins to give increasing ShareAtRisk(cty) due to the quadratic
    # nature of estimated relationship or if ShareAtRisk(cty,yrs) falls
    # below 5% or exceeds 100%

    # this overwrites those cases that exceed x_max
    ShareAtRisk$value[ShareAtRisk$value.relKcal > baseScalars[["x_max"]]] <- baseScalars[["y_min"]]
    # this doesn't allow shares to fall below 5% unless they don't exist
    ShareAtRisk$value[ShareAtRisk$value != 0 & ShareAtRisk$value < baseScalars[["y_min"]]] <- baseScalars[["y_min"]]
    # Do not exceed 100
    ShareAtRisk$value[ShareAtRisk$value > 100] <- 100
    # DO NOT DUMP ShareAtRisk BEFORE CALCULATING POP AT RISK

    # PopulationAtRisk ----
    PopulationAtRisk <- join(x = ShareAtRisk[, names(RelativeKCAL)],
                             y = pop[, !names(pop) %in% "description"],
                             on = c("cty", "yrs"),
                             suffix = c(".shr", ".pop"))
    PopulationAtRisk$value <- (PopulationAtRisk$value.shr / 100) * PopulationAtRisk$value.pop

    # Scale population at risk
    # The scaling is done only on the population at risk numbers and then we are
    # backcalculating the Share at Risk based on this scaling
    POATotalI3 <- readGDX(gdx = system.file("extdata",
                                            "PoU_params.gdx",
                                            package = "HuNuIMPACT"),
                          name = "POATotalI3", quick_df = TRUE)$data
    cty2reg1 <- readGDX(gdx = system.file("extdata",
                                            "PoU_params.gdx",
                                            package = "HuNuIMPACT"),
                          name = "cty2reg1", quick_df = TRUE)$data
    cty2reg1 <- cty2reg1[, -3]

    ctycalc <- intersect(unique(POATotalI3$cty[POATotalI3$value > 0]),
                         unique(ShareAtRisk$cty[ShareAtRisk$value > 0
                                                & ShareAtRisk$yrs == baseyr]))

    POAShareScale <- join(x = POATotalI3,
                          y = PopulationAtRisk[PopulationAtRisk$yrs %in% baseyr, ],
                          on = "cty", suffix = c("", ".popatrisk"))
    POAShareScale <- POAShareScale[POAShareScale$cty %in% ctycalc, ]
    POAShareScale$value <- POAShareScale$value / POAShareScale$value.popatrisk
    POAShareScale <- POAShareScale[, names(POATotalI3)]

    PopulationAtRisk <- join(x = PopulationAtRisk,
                              y = POAShareScale,
                              on = "cty",
                              suffix = c(".popatrisk", ".POAShareScale"))
    PopulationAtRisk$value <- PopulationAtRisk$value.popatrisk * PopulationAtRisk$value.POAShareScale
    PopulationAtRisk$value[is.na(PopulationAtRisk$value)] <-
        PopulationAtRisk$value.popatrisk[is.na(PopulationAtRisk$value)]
    PopulationAtRisk <- PopulationAtRisk[, c("cty", "yrs", "value")]

    popAggReg1 <- join(x = PopulationAtRisk[PopulationAtRisk$yrs %in% baseyr, ],
                   y = cty2reg1,
                   on = "cty")
    popAggReg1 <- popAggReg1 %>%
        group_by(reg1) %>%
        summarise(value = sum(value, na.rm = TRUE))

    POATotalI3Reg1 <- join(x = POATotalI3,
                   y = cty2reg1,
                   on = "cty")
    POATotalI3Reg1 <- POATotalI3Reg1 %>%
        group_by(reg1) %>%
        summarise(value = sum(value, na.rm = TRUE))

    PopulationAtRiskReg <- join(popAggReg1[!is.na(popAggReg1$reg1), ],
                                POATotalI3Reg1[!is.na(POATotalI3Reg1$reg1), ],
                                on = "reg1", suffix = c(".popAggReg1", ".POATotalI3Reg1"))
    PopulationAtRiskReg$value <- PopulationAtRiskReg$value.popAggReg1 - PopulationAtRiskReg$value.POATotalI3Reg1
    PopulationAtRiskReg <- PopulationAtRiskReg[, c("reg1", "value")]
    dfl[["PopulationAtRiskReg"]] <- PopulationAtRiskReg

    # ScaleRegion ----
    RegTotal <- readGDX(gdx = system.file("extdata",
                                          "PoU_params.gdx",
                                          package = "HuNuIMPACT"),
                        name = "RegTotal", quick_df = TRUE)$data
    ScaleRegion <- join(RegTotal,
                        PopulationAtRiskReg[PopulationAtRiskReg$value > 0, ],
                        on = "reg1")
    ScaleRegion <- ScaleRegion[!is.na(ScaleRegion$value_y), ]
    ScaleRegion$value <- ScaleRegion$value / ScaleRegion$value_y
    ScaleRegion <- ScaleRegion[, c("reg1", "value")]
    dfl[["ScaleRegion"]] <- ScaleRegion

    # Missing cty ----
    cty <- read.csv(file = system.file("extdata",
                                       "cty.csv",
                                       package = "HuNuIMPACT"), sep = ";", header = TRUE)
    MissingCty <- setdiff(unique(cty$cty), unique(POATotalI3$cty))

    # Scale PopAtRisk ----
    ScaleRegion2 <- join(x = ScaleRegion, y = cty2reg1, on = "reg1", multiple = TRUE)
    ScaleRegion2 <- ScaleRegion2[, c("cty", "value")]

    PopulationAtRisk <- join(x = PopulationAtRisk,
                  y = ScaleRegion2,
                  on = "cty",
                  suffix = c(".popatrisk", ".scale"))
    PopulationAtRisk$value <- PopulationAtRisk$value.popatrisk
    PopulationAtRisk$value[PopulationAtRisk$cty %in% MissingCty] <-
        (PopulationAtRisk$value.popatrisk[PopulationAtRisk$cty %in% MissingCty] *
             PopulationAtRisk$value.scale[PopulationAtRisk$cty %in% MissingCty])
    PopulationAtRisk$value[PopulationAtRisk$value < 0] <- 0
    # And, even after all that, we're still about 2.9% short of the world total
    # in 2020 from FAO, so this is a quick fix on that
    PopulationAtRisk$value <- PopulationAtRisk$value * 1.029

    PopulationAtRisk <- PopulationAtRisk[, c("cty", "yrs", "value")]

    PopulationAtRisk <- join(x = PopulationAtRisk,
                         y = pop[, -3],
                         on = c("cty", "yrs"),
                         suffix = c(".risk", ".pop"))
    PopulationAtRisk$value <- PopulationAtRisk$value.risk
    PopulationAtRisk$value[is.na(PopulationAtRisk$value)] <- 0
    PopulationAtRisk <- PopulationAtRisk %>%
        group_by(cty, yrs) %>%
        mutate(pop_risk_ratio = value.risk / value.pop)
    PopulationAtRisk$value[PopulationAtRisk$pop_risk_ratio >= 1] <- 1
    PopulationAtRisk <- PopulationAtRisk[, c("cty", "yrs", "value")]
    dfl[["PopulationAtRisk"]] <- PopulationAtRisk

    # Final Share at RISK ----
    ShareAtRisk2 <- join(PopulationAtRisk,
                        pop[, -3],
                        on = c("cty", "yrs"))
    ShareAtRisk2$value <- 100 * ShareAtRisk2$value / ShareAtRisk2$value_y
    dfl[["ShareAtRisk"]] <- ShareAtRisk2[, c("cty", "yrs", "value")]

    return(dfl)
}

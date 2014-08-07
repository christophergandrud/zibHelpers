# ---------------------------------------------------------------------------- #
# Functions for exploring/presenting Zero-inflated Beta Regression Output
# See Liu and Kong (under review) for backgroud
# Christopher Gandrud
# All functions available under the MIT License
# ---------------------------------------------------------------------------- #

#' Function to plot parameter posteriors from Zero-inflated Beta Regressions
#'
#' @param obj model object from \code{zoib} where \code{one.inflation = FALSE}
#' and \code{joint = FALSE}.
#' @param iter integer. The number of iterations.
#' @param variable_names character vector of variable names. Must be in the
#' same order and the same length as variables enterd into the \code{zoib}
#' formula.
#' @param xlab character string x-axis label.
#' @param title character string title for the plot
#'
#' @return a ggplot2 plot
#'
#' @examples
#' \dontrun{
#' Load packages
#' library(zoib)
#' library(zibHelpers)
#'
#' # Run example of clustered zero-inflated beta regression from
#' # Liu and Kong (under review, 12-14)
#'
#' # Load data
#' data("AlcoholUse", package = "zoib")
#' AlcoholUse$Grade <- as.factor(AlcoholUse$Grade)
#'
#' nIter = 50 # Number of iterations, including burn-in
#'
#' # Estimate
#' Out <- zoib(Percentage ~ Grade + Days + Gender|1|Grade + Days + Gender|1,
#'             data = AlcoholUse, random = 1, EUID = AlcoholUse$County,
#'             zero.inflation = TRUE, one.inflation = FALSE, joint = FALSE,
#'             n.iter = nIter)
#'
#' # Plot the posterior summary
#' zibPlot(Out, iter = nIter)
#' }
#'
#' @importFrom reshape2 melt
#' @importFrom dplyr group_by mutate
#' @importFrom magrittr %>%
#' @import ggplot2
#' @export

zibPlot <- function(obj, iter, variable_names = NULL, xlab = '\nCoef. Estimates',
                    title = ''){
    # CRAN stuff
    Var2 <- value <- medians <- lower95 <- upper95 <- part <- lower90 <-
        upper90 <- NULL

    # Extract simulations
    sims_subset <- obj$oripara
    max <- iter/2
    sims_subset <- sims_subset[[2]][1:max,]

    # Melt
    sims_subset_molten <- melt(sims_subset)

    # Find median and CI
    sims_subset_molten <- group_by(sims_subset_molten, Var2)
    Summed <- mutate(sims_subset_molten, medians = median(value))
    Summed <- mutate(Summed, lower95 = quantile(value, 0.025))
    Summed <- mutate(Summed, lower90 = quantile(value, 0.05))
    Summed <- mutate(Summed, upper90 = quantile(value, 0.95))
    Summed <- mutate(Summed, upper95 = quantile(value, 0.975))

    Summed <- Summed[!duplicated(Summed[, 'Var2']), ]

    # Indicator of whether beta or logit part
    Summed$part <- NA
    Summed$part <- ifelse(grepl('b\\[', as.character(Summed$Var2)),
                        'logit(mean of beta)', Summed$part)
    Summed$part <- ifelse(grepl('b0\\[', as.character(Summed$Var2)),
                        'logit(Pr(y = 0))', Summed$part)
    Summed$part <- ifelse(grepl('d', as.character(Summed$Var2)),
                          'shared', Summed$part)
    Summed$part <- ifelse(grepl('sigma', as.character(Summed$Var2)),
                          'shared', Summed$part)
    Summed$part <- factor(Summed$part)

    # Label
    if (!is.null(variable_names)){
        n_model_vars <- (nrow(Summed) - 4)/2
        if (length(variable_names) != n_model_vars){
            stop('variable_names is a different length than expected.\n',
                 .call = FALSE)
        }
        Summed$Labels <- factor(ReLabel(Summed,
                                variable_names = variable_names))

        Summed$Var2 <- 1:nrow(Summed)
        Summed$Var2 <- factor(Summed$Var2, labels = Summed$Labels)
        clevels <- levels(Summed$Var2)
    }
    # Reverse order
    clevels <- levels(Summed$Var2) %>% rev

    # Plot
    pp <- ggplot(Summed, aes(x = medians, y = Var2, xmin = lower95,
                            xmax = upper95, color = part, group = part)) +
        geom_point(size = 3) +
        geom_segment(aes(x = lower95, xend = upper95, yend = Var2),
                     size = 0.5) +
        geom_segment(aes(x = lower90, xend = upper90,
                         yend = Var2), size = 1.5) +
        scale_y_discrete(limits = clevels) +
        geom_vline(xintercept = 0, linetype = 'dotted') +
        scale_color_manual(values = c('#a6bddb', '#2b8cbe', 'gray'),
                            guide = FALSE) +
        xlab(xlab) + ylab('') + ggtitle(title) +
        theme_bw(base_size = 15)

    return(pp)
}

#' Extract Gelman-Rubin diagnostics as data frame
#'
#' @param obj model object from \code{zoib} where \code{one.inflation = FALSE}
#' and \code{joint = FALSE}.
#' @param iter integer. The number of iterations.
#'
#' @return data frame
#'
#' @examples
#' \dontrun{
#' Load packages
#' library(zoib)
#' library(zibHelpers)
#'
#' # Run example of clustered zero-inflated beta regression from
#' # Liu and Kong (under review, 12-14)
#'
#' # Load data
#' data("AlcoholUse", package = "zoib")
#' AlcoholUse$Grade <- as.factor(AlcoholUse$Grade)
#'
#' nIter = 50 # Number of iterations, including burn-in
#'
#' # Estimate
#' Out <- zoib(Percentage ~ Grade + Days + Gender|1|Grade + Days + Gender|1,
#'             data = AlcoholUse, random = 1, EUID = AlcoholUse$County,
#'             zero.inflation = TRUE, one.inflation = FALSE, joint = FALSE,
#'             n.iter = nIter)
#'
#' # Gelman-Rubin diagnostics
#' GelmanDiag(Out, nIter)
#' }
#'
#' @importFrom coda gelman.diag
#' @export


GelmanDiag <- function(obj, iter){
    obj <- GetzibPost(obj, max = iter/2)
    gr <-  gelman.diag(obj)
    grDF <- gr[[1]]
    return(grDF)
}


#' Extract and summarise posterior distribution
#'
#' @param obj model object from \code{zoib} where \code{one.inflation = FALSE}
#' and \code{joint = FALSE}.
#' @param iter integer. The number of iterations.
#'
#' @return data frame
#'
#' @examples
#' \dontrun{
#' Load packages
#' library(zoib)
#' library(zibHelpers)
#'
#' # Run example of clustered zero-inflated beta regression from
#' # Liu and Kong (under review, 12-14)
#'
#' # Load data
#' data("AlcoholUse", package = "zoib")
#' AlcoholUse$Grade <- as.factor(AlcoholUse$Grade)
#'
#' nIter = 50 # Number of iterations, including burn-in
#'
#' # Estimate
#' Out <- zoib(Percentage ~ Grade + Days + Gender|1|Grade + Days + Gender|1,
#'             data = AlcoholUse, random = 1, EUID = AlcoholUse$County,
#'             zero.inflation = TRUE, one.inflation = FALSE, joint = FALSE,
#'             n.iter = nIter)
#'
#' # Summarise the posterior
#' SummaryZib(Out, nIter)
#' }
#'
#' @export

SummaryZib <- function(obj, iter){
    obj <- GetzibPost(obj, max = iter/2)
    Sum <-  summary(obj)
    return(Sum)
}

#' Internal function for relabelling the parameters
#'
#' @param obj Object passed from \code{zibPlot}.
#' @param variable_names character vector of variable names. Must be in the
#' same order and the same length as variables entered into the \code{zoib}
#' formula.
#'
#' @keywords internal
#' @noRd

ReLabel <-function(obj, variable_names){
    obj$nn <- NA
    obj$nn <- ifelse(grepl('b\\[1\\]', obj$Var2), 'Intercept [Beta]', obj$nn)
    obj$nn <- ifelse(grepl('b0\\[1\\]', obj$Var2), 'Intercept [Pr(y = 0)]',
                    obj$nn)
    obj$nn <- ifelse(grepl('d', obj$Var2), 'log(dispersion of beta)', obj$nn)
    obj$nn <- ifelse(grepl('sigma', obj$Var2), 'sigma', obj$nn)
    for (i in 1:length(variable_names)){
        i2 <- i + 1
        id <- paste0('\\[', i2, '\\]')
        obj$nn <- ifelse(grepl(id, obj$Var2) & grepl('b\\[', obj$Var2),
                        paste0(variable_names[i], ' [Beta]'), obj$nn)
        obj$nn <- ifelse(grepl(id, obj$Var2) & grepl('b0\\[', obj$Var2),
                        paste0(variable_names[i], ' [Pr(y = 0)]'), obj$nn)
    }
    return(obj$nn)
}

#' Extract simulations in suitable form for Gelman-Rubin test and summary
#'
#' @param obj model object from \code{zoib} where \code{one.inflation = FALSE}
#' and \code{joint = FALSE}.
#' @param max maximum number of simulations post burn-in
#'
#' @importFrom coda mcmc.list as.mcmc
#' @keywords internal
#' @noRd

GetzibPost <- function(obj, max){
    post.sample <- obj$oripara
    sample.c1<- post.sample[[1]][1:max,]
    sample.c2<- post.sample[[2]][1:max,]
    sample12 <- mcmc.list(as.mcmc(sample.c1),as.mcmc(sample.c2))
    return(sample12)
}

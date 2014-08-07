\name{zibPlot}
\alias{zibPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
zibPlot(obj, max, variable_names = NULL, xlab = "\nCoef. Estimates", title = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
}
  \item{max}{
%%     ~~Describe \code{max} here~~
}
  \item{variable_names}{
%%     ~~Describe \code{variable_names} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (obj, max, variable_names = NULL, xlab = "\nCoef. Estimates", 
    title = "") 
{
    require(reshape2)
    require(dplyr)
    require(ggplot2)
    sims_subset <- obj$oripara
    sims_subset <- sims_subset[[2]][1:max, ]
    sims_subset_molten <- melt(sims_subset)
    sims_subset_molten <- group_by(sims_subset_molten, Var2)
    Summed <- mutate(sims_subset_molten, medians = median(value))
    Summed <- mutate(Summed, lower95 = quantile(value, 0.025))
    Summed <- mutate(Summed, lower90 = quantile(value, 0.05))
    Summed <- mutate(Summed, upper90 = quantile(value, 0.95))
    Summed <- mutate(Summed, upper95 = quantile(value, 0.975))
    Summed <- Summed[!duplicated(Summed[, "Var2"]), ]
    Summed$part <- NA
    Summed$part <- ifelse(grepl("b\\[", as.character(Summed$Var2)), 
        "logit(mean of beta)", Summed$part)
    Summed$part <- ifelse(grepl("b0\\[", as.character(Summed$Var2)), 
        "logit(Pr(y = 0))", Summed$part)
    Summed$part <- ifelse(grepl("d", as.character(Summed$Var2)), 
        "shared", Summed$part)
    Summed$part <- ifelse(grepl("sigma", as.character(Summed$Var2)), 
        "shared", Summed$part)
    Summed$part <- factor(Summed$part)
    if (!is.null(variable_names)) {
        n_model_vars <- (nrow(Summed) - 4)/2
        if (length(variable_names) != n_model_vars) {
            stop("variable_names is a different length than expected.\n", 
                .call = FALSE)
        }
        Summed$Labels <- factor(ReLabel(Summed, variable_names = variable_names))
        Summed$Var2 <- 1:nrow(Summed)
        Summed$Var2 <- factor(Summed$Var2, labels = Summed$Labels)
        clevels <- levels(Summed$Var2)
    }
    clevels <- levels(Summed$Var2) \%>\% rev
    pp <- ggplot(Summed, aes(x = medians, y = Var2, xmin = lower95, 
        xmax = upper95, color = part, group = part)) + geom_point(size = 3) + 
        geom_segment(aes(x = lower95, xend = upper95, yend = Var2), 
            size = 0.5) + geom_segment(aes(x = lower90, xend = upper90, 
        yend = Var2), size = 1.5) + scale_y_discrete(limits = clevels) + 
        geom_vline(xintercept = 0, linetype = "dotted") + scale_color_manual(values = c("#a6bddb", 
        "#2b8cbe", "gray"), guide = FALSE) + xlab(xlab) + ylab("") + 
        ggtitle(title) + theme_bw(base_size = 15)
    return(pp)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

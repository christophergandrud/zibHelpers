\name{ReLabel}
\alias{ReLabel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ReLabel(obj, variable_names)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
}
  \item{variable_names}{
%%     ~~Describe \code{variable_names} here~~
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
function (obj, variable_names) 
{
    obj$nn <- NA
    obj$nn <- ifelse(grepl("b\\[1\\]", obj$Var2), "Intercept [Beta]", 
        obj$nn)
    obj$nn <- ifelse(grepl("b0\\[1\\]", obj$Var2), "Intercept [Pr(y = 0)]", 
        obj$nn)
    obj$nn <- ifelse(grepl("d", obj$Var2), "log(dispersion of beta)", 
        obj$nn)
    obj$nn <- ifelse(grepl("sigma", obj$Var2), "sigma", obj$nn)
    for (i in 1:length(variable_names)) {
        i2 <- i + 1
        id <- paste0("\\[", i2, "\\]")
        obj$nn <- ifelse(grepl(id, obj$Var2) & grepl("b\\[", 
            obj$Var2), paste0(variable_names[i], " [Beta]"), 
            obj$nn)
        obj$nn <- ifelse(grepl(id, obj$Var2) & grepl("b0\\[", 
            obj$Var2), paste0(variable_names[i], " [Pr(y = 0)]"), 
            obj$nn)
    }
    return(obj$nn)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

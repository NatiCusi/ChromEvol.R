\name{specify.model}
\alias{specify.model}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
specify.model(model, filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{filename}{
%%     ~~Describe \code{filename} here~~
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
function (model, filename) 
{
    model <- gsub("fix_", "", model)
    if (model == "cr") 
        write("_gainConstR\n_lossConstR\n_duplConstR\n_demiPloidyR -999\n_gainLinearR -999\n_lossLinearR -999", 
            file = paste(filename, "_params.txt", sep = ""), 
            append = TRUE)
    if (model == "crd") 
        write("_gainConstR\n_lossConstR\n_duplConstR\n_demiPloidyR -2\n_gainLinearR -999\n_lossLinearR -999", 
            file = paste(filename, "_params.txt", sep = ""), 
            append = TRUE)
    if (model == "crde") 
        write("_gainConstR\n_lossConstR\n_duplConstR\n_demiPloidyR\n_gainLinearR -999\n_lossLinearR -999", 
            file = paste(filename, "_params.txt", sep = ""), 
            append = TRUE)
    if (model == "crnd") 
        write("_gainConstR\n_lossConstR\n_duplConstR -999\n_demiPloidyR -999\n_gainLinearR -999\n_lossLinearR -999", 
            file = paste(filename, "_params.txt", sep = ""), 
            append = TRUE)
    if (model == "lr") 
        write("_gainConstR\n_lossConstR\n_gainLinearR\n_lossLinearR\n_duplConstR\n_demiPloidyR -999", 
            file = paste(filename, "_params.txt", sep = ""), 
            append = TRUE)
    if (model == "lrd") 
        write("_gainConstR\n_lossConstR\n_gainLinearR\n_lossLinearR\n_duplConstR\n_demiPloidyR -2", 
            file = paste(filename, "_params.txt", sep = ""), 
            append = TRUE)
    if (model == "lrde") 
        write("_gainConstR\n_lossConstR\n_gainLinearR\n_lossLinearR\n_duplConstR\n_demiPloidyR", 
            file = paste(filename, "_params.txt", sep = ""), 
            append = TRUE)
    if (model == "lrnd") 
        write("_gainConstR\n_lossConstR\n_gainLinearR\n_lossLinearR\n_duplConstR -999\n_demiPloidyR -999", 
            file = paste(filename, "_params.txt", sep = ""), 
            append = TRUE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

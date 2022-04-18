\name{plot.ce}
\alias{plot.ce}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots onyl the tree of an ChromEvol object
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.ce(chromEvol, cex.tips = 0.6, offset = 0.004, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{chromEvol}{
Object of class ChromEvol
}
  \item{cex.tips}{
%%     ~~Describe \code{cex.tips} here~~
}
  \item{offset}{
%%     ~~Describe \code{offset} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (chromEvol, cex.tips = 0.6, offset = 0.004, ...) 
{
    plot(chromEvol[[3]], cex = cex.tips, no.margin = TRUE, label.offset = offset, 
        ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\name{read.results}
\alias{read.results}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.results(..., node = node)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{node}{
%%     ~~Describe \code{node} here~~
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
function (..., node = node) 
{
    models <- list(...)
    models <- unlist(models)
    CE <- vector("list", length(models))
    ML <- vector("list", 1)
    events <- vector("list", 6)
    for (i in 1:length(models)) {
        CE[[i]] <- read.cE(paste(models[[i]], sep = ""))
        se <- sum.Events(CE[[i]])
        events[[1]][[i]] <- as.numeric(CE[[i]][[3]]$node.label[node])
        for (j in 1:4) events[[j + 1]][i] <- se[j, 3]
        events[[6]][i] <- se[5, 3]
    }
    names(events) <- c("CN_ML", "No_GAINS>0.5", "No_LOSS>0.5", 
        "No_DUPL>0.5", "No_DEMI>0.5", "totNoEvents")
    events
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

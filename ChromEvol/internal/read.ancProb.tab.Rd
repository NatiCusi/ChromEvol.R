\name{read.ancProb.tab}
\alias{read.ancProb.tab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.ancProb.tab(filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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
function (filename) 
{
    anc.tab <- read.table(filename, header = TRUE, stringsAsFactors = FALSE)
    anc.tab2 <- as.matrix(anc.tab)
    anc.tab3 <- apply(anc.tab2, 2, as.numeric)
    dimnames(anc.tab3)[[1]] <- anc.tab2[, 1]
    anc.tab3 <- anc.tab3[, -1]
    dimnames(anc.tab3)[[2]] <- gsub("X", "", dimnames(anc.tab3)[[2]])
    ancNodes <- anc.tab3[grep("^N[0-9]", dimnames(anc.tab3)[[1]]), 
        ]
    ancTips <- anc.tab3[-(grep("^N[0-9]", dimnames(anc.tab3)[[1]])), 
        ]
    foo <- function(x) as.numeric(dimnames(ancNodes)[[2]][which(x == 
        max(x))])
    pp <- apply(ancNodes, 1, foo)
    anc <- list(Nodes = ancNodes, Tips = ancTips, Nodes_best_CN = pp)
    anc
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\name{match.table2tip}
\alias{match.table2tip}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
match.table2tip(tree, table, nomatch = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree}{
%%     ~~Describe \code{tree} here~~
}
  \item{table}{
%%     ~~Describe \code{table} here~~
}
  \item{nomatch}{
%%     ~~Describe \code{nomatch} here~~
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
function (tree, table, nomatch = NA) 
{
    if (dim(table)[2] == 1) 
        table <- cbind(dimnames(table)[[1]], table[, 1])
    d <- duplicated(c(tree$tip.label, table[, 1]))
    dd <- d[(length(tree$tip.label) + 1):length(d)]
    table <- table[dd, ]
    if (length(tree$tip.label) > length(table[[1]])) {
        table2 <- NULL
        for (i in 1:length(table[1, ])) table2 <- cbind(table2, 
            table[, i][match(tree$tip.label, table[, 1], nomatch = nomatch)])
    }
    else {
        m <- tree$tip.label[match(table[[1]], tree$tip.label, 
            nomatch = NA)]
        table1 <- data.frame(table[[1]][!is.na(m)])
        for (i in 2:length(table)) table1[i] <- table[[i]][!is.na(m)]
        table2 <- table1
        for (i in 1:length(table1)) table2[i] <- table1[[i]][match(tree$tip.label, 
            table1[[1]], nomatch = NA)]
    }
    names(table2) <- names(table)
    return(table2)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

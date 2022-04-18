\name{read.cE.tab}
\alias{read.cE.tab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reads chromEvol input table in fasta format
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.cE.tab(table, geiger = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table}{
path of the table in fasta format}
  \item{geiger}{
if TRUE table is formatted according to the input necessary for the function fitDiscrete (or more?) in geiger}
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
function (table, geiger = FALSE) 
{
    table <- read.table(table, sep = "\n", stringsAsFactor = FALSE)
    table2 <- data.frame()
    table2 <- table[grep(">", table[, 1]), 1]
    table2 <- gsub(">", "", table2)
    table2 <- cbind(table2, table[grep(">", table[, 1], invert = TRUE), 
        1])
    table2 <- gsub(" ", "", table2)
    table2 <- as.data.frame(table2, stringsAsFactors = FALSE)
    if (geiger) {
        table2[, 2] <- gsub("=.*", "", table2[, 2])
        table3 <- as.factor(table2[, 2])
        names(table3) <- table2[, 1]
        table2 <- table3
    }
    table2
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\name{make.input.table}
\alias{make.input.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates input table
}
\description{
Creates a file with a species names (tip labels) of the tree, to add the chromosome numbers manually, so that it can be used as input table for \code{\link{ce.input}}}
\usage{
make.input.table(phy, file = "Chromosome_numbers.txt", sort = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{phy}{
the phylogeny on which chromosome numbers will be reconstructed
}
  \item{file}{
name of output file
}
  \item{sort}{
specifies if tip labels of the phylogeny will be sorted (default) or not.
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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

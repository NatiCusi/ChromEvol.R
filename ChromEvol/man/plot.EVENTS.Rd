\name{plot.EVENTS}
\alias{plot.EVENTS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots the inferred number of the four types of events on the branches}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.EVENTS(ChromEvol, events = "best", frame = FALSE, info = TRUE, 
cex.tip = 0.6, cex.event = 0.6, cex.node.No = 0.4, cex.legend = 0.8, 
chromNo = "ML", add = FALSE, position = "above", title = NULL, 
x = 0, y = NA, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ChromEvol}{
Object of class ChromEvol
}
  \item{events}{
Specifies te type of events that are plotted on the branches. 
"all": all inferred events (from simulation and heuristic estimation); "best": only those events with an expectation >0.5 (from simulation and heuristic estimation); "SIMbest": only events from simulations with expectation > 0.5
}
  \item{frame}{
specifies if a frame should be plotted around the number of events
}
  \item{info}{
%%     ~~Describe \code{info} here~~
}
  \item{cex.tip}{
a numeric value giving the factor scaling the tip, edge (events), chromosome numbers at nodes or the legend (Character EXpansion).
}
  \item{cex.event}{
see above
}
  \item{cex.node.No}{
see above
}
  \item{cex.legend}{
see above
}
  \item{chromNo}{
indicates if chromosome numbers inferred under maximum likelihood ("ML") or Bayesian reconstruction ("PP") should be plotted at nodes.
}
  \item{add}{
Logical (FALSE). If TRUE, the tree is not plotted again but the specified events are plotted on an existing tree (e.g. by plot.anc)

}
  \item{position}{
specifies the position of the number of events on branches: "above" (default), or "below".
}
  \item{title}{
A title can be specified here
}
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
Specifies the position of legend. If y=NA  the legend will be placed by clicking on the figure (function: locator() ). If only a value for y is given it is plotted at the respective height at the left margin of the plot.
}
  \item{\dots}{
further graphical parameters to be passed to plot.phylo
}
\item{only.legend}{If TRUE only the legend is plotted to an existing plot.
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
 \code{\link{plot.anc}},  \code{\link{plot.phylo}},  \code{\link{read.cE}}
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

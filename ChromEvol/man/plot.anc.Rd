\name{plot.anc}
\alias{plot.anc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots inferred chromosome numbers at nodes
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.anc(ChromEvol, node.pie = TRUE, node.No = "PP", info = FALSE, tip.pie = TRUE, 
inferred.tip = FALSE, col.inftips = "red", chroCOL = NULL, cex.node.pie = 0.8, 
cut.off = 0.1, cex.tip = 0.8, cex.tip.pie = 0.3, cut.tipNo = FALSE, adj.tip.pie = 0.5, 
cex.chrNo = 0.6, cex.legend = 0.8, add = FALSE, label.offset = NA, title = NULL, 
cex.title = 2, frame = "c", x = 0, y = NA, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ChromEvol}{
Object of class ChromEvol
}
  \item{node.pie}{
Logical, default = TRUE: pie charts indicating the probabilities of the inferred chromosome number are plotted at nodes
}
  \item{node.No}{
Logical, default = TRUE: node number with the highest probability is plotted at nodes  (on pies, if node.pie=TRE)}
  \item{info}{
If TRUE a line with information about the plotted parameters is written above the figure
}
  \item{tip.pie}{
Logical, default = TRUE: pie charts indicating the probabilities of the inferred chromosome number at the tips are plotted between the terminal branches of of the tree and the species names

}
  \item{inferred.tip}{
Logical, default = FALSE. Empirical chromosome numbers will be plotted at tips: if tip.pie=TRUE numbers wil be plotted next to the tiplabels; if tip.pie=FALSE numbers will be plotted between the terminal branches of of the tree and the species names
}

  \item{chroCOL}{
The colors of the chromosome numbers can be specified (from the lowest to the highest). 
}
%  \item{cex.node.pie, cex.tip, cex.tip.pie, cex.chrNo, cex.legend, cex.title}{
%Scaling factor of the various arguments
%}
  \item{cut.off}{

}
  \item{cut.tipNo}{
Cuts off empirical tip numbers (of the input table) that are added by default by the ChromEvol program to the tip labels (secies names)
}
  \item{adj.tip.pie}{
Specifies horizontal adjustment of the tip pies. A numeric specifying the justification: 0 (left-justification), 0.5 (centering), or 1 (right-justification). 
}
  \item{add}{
Logical (FALSE). If TRUE, the tree is not plotted again but just the indicated charts, numbers, text is added to an existing tree (e.g. by plot.EVENTS)
}
  \item{label.offset}{
A numeric giving the space between the nodes and the tips of the phylogeny and their corresponding labels.
}
  \item{title}{
A title can be specified here
}
  \item{frame}{
Specifies the frame type of the best inferred chromosome number plotted at nodes.
}
  \item{x}{
  	}
   \item{y}{
Specifies the position of legend. If y=NA  the legend will be placed by clicking on the figure (function: locator() ). If only a value for y is given it is plotted at the respective height at the left margin of the plot.
}
\item{only.legend}{
	If TRUE only the legend is plotted to an existing plot.
}
\item{\dots}{
further graphical parameters to be passed to plot.phylo
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
 \code{\link{plot.EVENTS}},  \code{\link{plot.phylo}},  \code{\link{read.cE}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

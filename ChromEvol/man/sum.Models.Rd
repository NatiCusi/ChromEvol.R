\name{sum.Models}
\alias{sum.Models}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summarizes parameters of several models
}
\description{
Summarizes parameters of several models for comparison, either of only 
the best models (bestAIC ± 2) or all models
}
\usage{
sum.Models(path, best = TRUE, node = 1, write.table = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
specifies the path of the folder that includes the results from a chromEvol analysis 
run under the All_Models option
}
  \item{best}{
if TRUE (default) only the parameters of the best models are summarized, i.e. all models that have an AIC of the bestAIC ± 2
}
  \item{node}{
specifies a node of interest for which the inferred chromosome numbers will be 
summarized, default is 1, i.e. the root node.
}
  \item{write.table}{
if TRUE the summary is written in a file called "/summary_ALL_models" or "/summary_BEST_models", respectively, in the folder specified in \code{path}
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
AIC, Log-likelihoods
	exp:Denote the AIC values of the candidate models by AIC1, AIC2, AIC3, …, AICR. Let AICmin be the minimum of those values. Then exp((AICmin−AICi)/2) can be interpreted as the relative probability that the ith model minimizes the (estimated) information loss
	rates of transitions
	best and second best chromosome number (CN) from Bayesian inference with PP and CN from ML inference at specific node (default: root node (1))
	number of events with PP > 0.5

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

\name{read.expect.tab}
\alias{read.expect.tab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.expect.tab(filename)
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
    exp <- scan(filename, what = "list", sep = "\n")
    text <- c("#Nodes with GAIN events with expectation above 0.5", 
        "#Nodes with LOSS events with expectation above 0.5", 
        "#Nodes with duplication events with expectation above 0.5", 
        "#Nodes with demi-duplication events with expectation above 0.5", 
        "#+++++++++++++++++++++++++++++")
    events <- list()
    EVENTS <- list()
    EVENTS2 <- list()
    for (j in 1:4) {
        events[[j]] <- exp[(which(exp[1:length(exp)] == text[j]) + 
            1):(which(exp[1:length(exp)] == text[5])[j] - 1)]
        events[[j]] <- strsplit(events[[j]], split = ": ")
        ev <- NULL
        for (i in 1:length(events[[j]])) ev <- rbind(ev, events[[j]][[i]])
        EVENTS[[j]] <- ev
    }
    write.table(exp[(which(exp[1:length(exp)] == "#ALL EVENTS EXPECTATIONS PER NODE") + 
        2):(which(exp[1:length(exp)] == "#Expected number of events from root to leaf") - 
        2)], sep = "/t", file = "exp.tab")
    exp.tab <- read.table("exp.tab", stringsAsFactors = FALSE, 
        skip = 1, quote = "", sep = "\t")
    exp.tab[, 1] <- gsub("\"", "", exp.tab[, 1])
    exp.tab[, 5] <- as.numeric(gsub("\"", "", exp.tab[, 5]))
    exp.tab2 <- exp.tab
    foo <- function(x) round(x, digits = 3)
    exp.tab2[2:5] <- lapply(exp.tab[2:5], foo)
    exp.tab2[, 1] <- gsub("[0-9].*/t", "", exp.tab2[, 1])
    he <- exp[(which(exp[1:length(exp)] == "#EVENTS NOT ACCOUNTED FOR IN THE SIMULATIONS: ") + 
        2):(which(exp[1:length(exp)] == "#ALL EVENTS EXPECTATIONS PER NODE") - 
        2)]
    if (length(grep("HEURISTIC ESTIMATION", he)) > 0) {
        he2 <- strsplit(he, split = "\t")
        ev <- c("Gains = ", "Losses = ", "Duplications = ", "Demi-dupl = ")
        HeurEst <- data.frame()
        for (i in grep("Gains", he2)) {
            for (j in 3:6) HeurEst[i, j - 2] <- as.numeric(gsub(ev[j - 
                2], "", he2[[i]][j]))
        }
        HeurEst <- HeurEst[grep("Gains", he2), ]
    }
    else HeurEst <- data.frame(rbind(rep(NA, 4)))
    list(Events_with_high_PP = EVENTS, All_events = exp.tab2, 
        Heuristic_Estimation = HeurEst)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

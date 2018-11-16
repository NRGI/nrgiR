\name{get_eiti_entity}
\alias{get_eiti_entity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get EITI government entity data}
\description{
Downloads EITI summary data file for government entity revenues from resourcedata.org. Returns a single \code{data.table}.
}
\usage{
get_eiti_entity()
}
%- maybe also 'usage' for other objects documented here.
\arguments{

%%     ~~Describe \code{x} here~~
}

\details{
Data sourced from \url{https://www.resourcedata.org/dataset/eiti-complete-summary-table}/
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
dt = get_eiti_entity()
head(dt)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

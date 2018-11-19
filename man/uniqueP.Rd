\name{uniqueP}
\alias{uniqueP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Unique collapse of character vector}
\description{Paste a character vector's unique values into a single string.}
\usage{
uniqueP(x, collapse="; ", sort=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{character vector.}
  \item{collapse}{character string to be pasted between \code{x} elements.}
  \item{sort}{\code{TRUE}/\code{FALSE}. Sorted the elements alphabetically before being collapsed?}
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
vec = c("apple","banana","banana","corn","corn","tomato")

uniqueP(vec)

#Use with \emph{data.table} syntax
dt = data.table(group=c("A","A","A","B","B","B","B"), name=c("Mary","Mary","Jim","Bill","Fred","Bill","Anne"))

dt[, uniqueP(name), by=.(group)]


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

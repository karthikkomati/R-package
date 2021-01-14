\name{plot.brent}
\alias{plot.brent}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot.brent
%%  ~~function to do ... ~~
}
\description{
Plots the inputs of brent's method
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.brent(a)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
  list with the class set to brent containing a function, upper bound and lower bound
%%     ~~Describe \code{x} here~~
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
Karthik Komati
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
> f = function(x){x^2 -1}
> a <- list(f,0,2)
> class(a) <- 'brent'
> plot(a)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

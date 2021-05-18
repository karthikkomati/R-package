\name{brent}
\alias{brent}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
brent
%%  ~~function to do ... ~~
}
\description{
Given a function with a root between the interval [a,b] , brent returns the root.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
brent(f, a, b, tol = 1e-05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{
%%
    function
}
\item{a}{
%%
  upper bound
}
\item{b}{
  lower bound
%%     ~~Describe \code{x} here~~
}
\item{tol}{
  tolerance. The default value is 1e-5
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
returns the root
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
f <- function(x){(x + 3)*(x - 1)^2}

brent(f,-4,4/3)
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

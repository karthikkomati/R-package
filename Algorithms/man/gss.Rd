\name{gss}
\alias{gss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
gss
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Given a function with a single local minimum between the interval [a,b] , gss returns a sub interval less than the tolerance which contains the minimum
}
\usage{
gss(f,a,b,tol)
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

returns an interval less than the tolerance, which contains the minimum
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
f <- function(x){abs(x - 2) + (x - 1)^2}

gss(f, 1, 1.4)
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

\name{newton_optimization_hd}
\alias{newton_optimization_hd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
newton_optimization_hd
}
\description{
Given a twice differentiable equation in three dimensions or higher, this function will return the closest extrema to the starting point
}
\usage{
newton(f,var,values=0,tol=1e-10,iter=1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{
        a twice differentiable equation
}
\item{var}{
        the variables that make up the input function
}
\item{values}{
  initial values
}
\item{tol}{
        tolerance. The default value is 1e-10
}
\item{iter}{
        the maximum number of iterations. Default value is 1000
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
returns the closest extrema to the initial value
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
 f <- function(x,y){x^3+y^3-3}
 l <- list('x','y')
 newton(f,l)
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

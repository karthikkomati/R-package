\name{newton_deriv}
\alias{newton_deriv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
newton_deriv
}
\description{
Given an differentiable equation and its derivative, Newton-Raphson method will find a root of the function if the root exists
}
\usage{
newton(f, f1, x=0,tol=1e-10,iter=1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{
        a differentiable equation
}
\item{f1}{The derivative of f}
\item{x}{
        the initial value. Default value is zero
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
returns the root of the equation
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

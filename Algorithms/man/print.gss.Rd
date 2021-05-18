\name{print.gss}
\alias{print.gss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
print.gss
%%  ~~function to do ... ~~
}
\description{

Prints the outputs of the golden section search
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
print.gss(a)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
   list with the class set to gss containing a function, upper bound and lower bound
%%     ~~Describe \code{a} here~~
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
> f = function(x){x^2}
> a <- list(f,-1,1)
> class(a) <- 'gss'
> print(a)
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

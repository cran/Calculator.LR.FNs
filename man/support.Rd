\name{support}
\alias{support}

\title{
Support of LR fuzzy number
}
\description{
To determining the support of a LR fuzzy number one can use from this function.
In other words, the \code{support} function is able to compute the smallest and biggest values \eqn{x} for which \eqn{\mu(x)>0}.
}

\usage{
support(M, Left.fun = NULL, Right.fun = NULL)
}

\arguments{
  \item{M}{
A LR, RL or L fuzzy number
}
  \item{Left.fun}{
The left-shape function which usually defined before using LRFN.plot (see examples in bellow)
}
  \item{Right.fun}{
The right-shape function which usually defined before using LRFN.plot (see examples in bellow)
}
}

\value{
The "support" function return a interval-valued vector in which the membership function value of LR fuzzy number is bigger than zero.
}


\author{
Abbas Parchami }

\examples{
Left.fun  = function(x)  { (1-x)*(x>=0)}
Right.fun = function(x)  { (exp(-x))*(x>=0)}
T = LR(1, 0.6, 0.2)
support(T)
LRFN.plot( T, xlim=c(-5,20), lwd=2, lty=3, col=4)

N = RL(3, 0.5, 2)
support(N)

Left.fun  = function(x)  { (1-x)*(x>=0)}
M = L(2,4,3)
support(M)

Left.fun  = function(x)  { (1-x^2)*(x>=0)}
Right.fun = function(x)  { (exp(-x))*(x>=0)}
support( LR(17,5,3))


## The function is currently defined as
function (M, Left.fun = NULL, Right.fun = NULL) 
{
    range1 = M[1] - M[2] - M[3] - 100
    range2 = M[1] + M[2] + M[3] + 100
    x = seq(range1, range2, len = 2e+05)
    if (M[4] == 0) {
        y = Left.fun((M[1] - x)/M[2]) * (x <= M[1]) + Right.fun((x - 
            M[1])/M[3]) * (M[1] < x)
    }
    else if (M[4] == 1) {
        y = Right.fun((M[1] - x)/M[2]) * (x <= M[1]) + Left.fun((x - 
            M[1])/M[3]) * (M[1] < x)
    }
    else if (M[4] == 0.5) {
        y = Left.fun((M[1] - x)/M[2]) * (x <= M[1]) + Left.fun((x - 
            M[1])/M[3]) * (M[1] < x)
    }
    supp = c()
    supp[1] = min(x[0 < y & y < 1])
    supp[2] = max(x[0 < y & y < 1])
    if (supp[1] == min(x)) {
        supp[1] = -Inf
    }
    if (supp[2] == max(x)) {
        supp[2] = +Inf
    }
    return(supp)

  }
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Calculator for LR Fuzzy Numbers }
\keyword{ Introducing the form of LR fuzzy number Fuzzy Number }
\keyword{ Introducing the form of RL fuzzy number Fuzzy Number }
\keyword{ Introducing the form of L fuzzy number Fuzzy Number }
\keyword{ Ploting and drawing LR fuzzy numbers }
\keyword{ Sign of LR fuzzy number }
\keyword{ Support of LR fuzzy number }
\keyword{ Scalar multiplication on LR fuzzy numbers }
\keyword{ Summation of two LR fuzzy numbers }
\keyword{ Subtraction of two LR fuzzy numbers }
\keyword{ Product of two LR fuzzy numbers }
\keyword{ Division of two LR fuzzy numbers }

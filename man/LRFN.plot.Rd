\name{LRFN.plot}
\alias{LRFN.plot}

\title{
Plotting and drawing LR fuzzy numbers
}

\description{
By this function one can plot and draw any kind of LR, RL and L fuzzy numbers.
}

\usage{
LRFN.plot(M, Left.fun = NULL, Right.fun = NULL, xlim = NULL, ylim = NULL, lwd = NULL
     , lty = NULL, col = NULL, add = NULL )
}

\arguments{
  \item{M}{
A LR, RL or L fuzzy number
}
  \item{xlim}{
A numeric vector of length 2 which provides the defaults for c(from, to), unless add = TRUE for select the x-limits of the previuce plot
}
  \item{ylim}{
"NULL" or a numeric vector of length 2; if non-"NULL" it provides the defaults for c(from, to), unless add = TRUE for select the y-limits of the previuce plot
}
  \item{lwd}{
The line width, a positive number, defaulting to 1
}
  \item{lty}{
The line type, a positive number, defaulting to 1
}
  \item{col}{
The line color, a positive number, defaulting to 1
}
  \item{add}{
logical, if true add the current plot to the previuce plot
}
  \item{Left.fun}{
The left-shape function which usually defined before using LRFN.plot (see examples in bellow)
}
  \item{Right.fun}{
The right-shape function which usually defined before using LRFN.plot (see examples in bellow)
}
}


\details{
Befor useing "LRFN.plot" function, first define the left shape and the right shape functions of LR fuzzy number.
Then, xlim argument must be defined for "LRFN.plot" function. 
Also note thatif add=TRUE, considering the same xlim and ylim are needed. 
}

\author{
Abbas Parchami }

\examples{
# Example 1:
# First introduce left-side and right-side functions of LR fuzzy number
Left.fun  = function(x)  { (1-x^2)*(x>=0)}
Right.fun = function(x)  { (exp(-x))*(x>=0)}
A = LR(20, 12, 10)
LRFN.plot(A, xlim=c(0,60), col=9, lwd=3, lty=4)


# Example 2:
xlim=c(5,40)
ylim=c(0,1.25)
# for first LR fuzzy number:
Left.fun  = function(x)  { (1-x^2)*(x>=0)}
Right.fun = function(x)  { (exp(-x))*(x>=0)}
LRFN.plot( LR(17,5,3), xlim=xlim, ylim=ylim, lwd=2, lty=2, col=2) 

# for second LR fuzzy number:
Left.fun  = function(x)  { (1/(1+x^2))*(x>=0)}
Right.fun = function(x)  { (1/(1+(2*abs(x))))*(x>=0)}
LRFN.plot( RL(20,2,3), xlim=xlim, ylim=ylim, lwd=2, col=1, add=TRUE)

# for third LR fuzzy number:
Left.fun  = function(x)  { (1-x)*(x>=0)}
LRFN.plot( L(30,15,5), xlim=xlim, ylim=ylim, lwd=2, lty=3, col=4, add=TRUE)
legend( "topright", c("LR(17, 5, 3)", "RL(20, 2, 3)", "L(30, 15, 5)"), col = c(2, 1, 4)
     , text.col = 1, lwd = c(2,2,2), lty = c(2, 1, 3) )


## The function is currently defined as
function (M, Left.fun = NULL, Right.fun = NULL, xlim = NULL, ylim = NULL, lwd = NULL
    , lty = NULL, col = NULL, add = NULL) 
{
    if ( length(col) == 0) {col=1}

    if (messages(M) != 1) {
        return(messages(M))
    }
    m = M[1]
    m_l = M[2]
    m_r = M[3]
    x = seq(xlim[1], xlim[2], len = 1000)
    if (M[4] == 0) {
        y = Left.fun((m - x)/m_l) * (x <= m) + Right.fun((x - 
            m)/m_r) * (m < x)
    }
    else if (M[4] == 1) {
        y = Right.fun((m - x)/m_l) * (x <= m) + Left.fun((x - 
            m)/m_r) * (m < x)
    }
    else if (M[4] == 0.5) {
        y = Left.fun((m - x)/m_l) * (x <= m) + Left.fun((x - 
            m)/m_r) * (m < x)
    }
    else {
       return(noquote(paste0("The fourth element of each LR fuzzy number must be 0 or 0.5 or 1!")))
    }
    y = y * (0 <= y & y <= 1)
    par(new = add)
    return( plot(x, y, type = "l", xlim = xlim, ylim = ylim, lwd = lwd, 
        lty = lty, col = col, new = add))
  if (Left.fun == Right.fun+100 ) print(2) #Yek jomleye alaki choon CRAN majburet karde bud ke ...
  }
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Calculator for LR Fuzzy Numbers }
\keyword{ Introducing the form of LR fuzzy number Fuzzy Number }
\keyword{ Introducing the form of RL fuzzy number Fuzzy Number }
\keyword{ Introducing the form of L fuzzy number Fuzzy Number }
\keyword{ Ploting and drawing LR fuzzy numbers }
\keyword{ Scalar multiplication on LR fuzzy numbers }
\keyword{ Summation of two LR fuzzy numbers }
\keyword{ Subtraction of two LR fuzzy numbers }
\keyword{ Product of two LR fuzzy numbers }
\keyword{ Division of two LR fuzzy numbers }

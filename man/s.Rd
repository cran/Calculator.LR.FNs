\name{subtraction}
\alias{s}

\title{
Subtraction of two LR fuzzy numbers
}

\description{
This function calculates subtraction (difference) of two fuzzy numbers \eqn{ M=(m, \alpha, \beta)_{LR} } and  \eqn{ N=(n, \gamma, \delta)_{RL} } on the basis of Zadeh extension principle by the following formula:
\deqn{ M \ominus N = (m-n, \alpha+\delta, \beta+\gamma)_{LR} }
}

\usage{
s(M, N)
}

\arguments{
  \item{M}{
The first LR (or RL or L) fuzzy number
}
  \item{N}{
The second LR (or RL or L) fuzzy number
}
}

\value{
A LR (or RL or L) fuzzy number
}

\references{
Dubois, D., Prade, H., Fuzzy Sets and Systems: Theory and Applications. Academic Press (1980).

Dubois, D., Prade, H., Operations on fuzzy numbers. International Journal of Systems Science 9 (1978), 613-626.

Dubois, D., Prade, H., Fuzzy numbers: An overview. In In: Analysis of Fuzzy Information. Mathematical Logic, Vol. I. CRC Press (1987), 3-39.

Dubois, D., Prade, H., The mean value of a fuzzy number. Fuzzy Sets and Systems 24 (1987), 279-300.

Kaufmann, A., Gupta, M.M., Introduction to Fuzzy Arithmetic. van Nostrand Reinhold Company, New York (1985).

Taheri, S.M, Mashinchi, M., Introduction to Fuzzy Probability and Statistics. Shahid Bahonar University of Kerman Publications, In Persian (2009).

Viertl, R., Statistical Methods for Fuzzy Data. John Wiley & Sons, Chichester (2011).

Zadeh, L.A., The concept of a linguistic variable and its application to approximate reasoning-I. Information Sciences 8 (1975), 199-249.
}

\author{
Abbas Parchami }

\examples{
# Example 1:
Left.fun  = function(x)  { (1/(1+x^2))*(x>=0)}
Right.fun = function(x)  { (1/(1+(2*abs(x))))*(x>=0)}
M = LR(1, 0.6, 0.2)
N = RL(3, 0.5, 1)

s(N, M)
s(M, N)
s(M, M)
s(s(N, M), M)

# Example 2:
Left.fun  = function(x)  { (1-x)*(x>=0)}
A = L(5,3,2)
B = L(3,2,1)
LRFN.plot( A, xlim=c(-3,12), lwd=2, lty=2, col=2) 
LRFN.plot( B, lwd=2, lty=2, col=3, add=TRUE)
LRFN.plot( s(A, B), lwd=2, lty=3, col=1, add=TRUE)
legend( "topright", c("A = L(5, 3, 2)", "B = L(3, 2, 1)", "A - B = L(2, 4, 4)"), col = c(2, 3, 1)
     , text.col = 1, lwd = c(2,2,2), lty = c(2, 2, 3) )

## The function is currently defined as
function (M, N) 
{
    options(warn = -1)
    if (messages(M) != 1) {
        return(messages(M))
    }
    if (messages(N) != 1) {
        return(messages(N))
    }
    if ((M[4] == 1 & N[4] == 0) | (M[4] == 0 & N[4] == 1) | (M[4] == 
        0.5 & N[4] == 0.5)) {
        a1 = M[1] - N[1]
        a2 = M[2] + N[3]
        a3 = M[3] + N[2]
        a4 = M[4]
        print(noquote(paste0("the result of subtraction is  (core = ", 
            a1, ", left spread = ", a2, ", right spread = ", 
            a3, ")", if (a4 == 0) {
                paste0(" LR")
            }
            else if (a4 == 1) {
                paste0(" RL")
            }
            else {
                paste0(" L")
            })))
        return(invisible(c(a1, a2, a3, a4)))
    }
    else {
        return(noquote(paste0( "Subtraction has NOT a closed form of a LR fuzzy number" )))
    }
  }
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Calculator for LR Fuzzy Numbers }
\keyword{ Zadeh extension principle }
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

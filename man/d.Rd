\name{division}
\alias{d}

\title{
Division of two LR fuzzy numbers
}

\description{
This function calculates the division of two LR fuzzy numbers.
Although on the basis of Zadeh extension principle, the class of LR fuzzy numbers is not closed under the operations multiplication and division, 
but we consider the following approximation for division of fuzzy number \eqn{ M=(m, \alpha, \beta)_{LR} }  by fuzzy number  \eqn{ N=(n, \gamma, \delta)_{RL} }  to work easy in the class of LR fuzzy numbers:
\deqn{  M \oslash N
\simeq 
\left( \frac{m}{n}, \frac{m \delta + n \alpha}{n^2}, \frac{m \gamma + n \beta}{n^2} \right)_{LR} }
}

\usage{
d(M, N)
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
Left.fun  = function(x)  { (1-x)*(x>=0)}
A = L(6, 1, 2)
B = L(3, 2, 3)
xlim=c(-1.5,9)
ylim=c(0,1.22)
LRFN.plot( A, xlim=xlim, ylim=ylim, lwd=2, lty=2, col=2) 
LRFN.plot( B, xlim=xlim, ylim=ylim, lwd=2, lty=2, col=3, add=TRUE)
LRFN.plot( d(A,B), xlim=xlim, ylim=ylim, lwd=2, lty=1, col=1, add=TRUE)
legend( "topright", c("A = L(6, 1, 2)", "B = L(3, 2, 3)", "A / B = L(2, 2.33, 2)")
     , col = c(2, 3, 1), text.col = 1, lwd = c(2,2,2), lty = c(2, 2, 1) )


# Example 2:
Left.fun  = function(x)  { (1-x)*(x>=0)}
Right.fun = function(x)  { (1-x^2)*(x>=0)}
A = LR(8, 0.5, 1)
B = RL(2, 1, 1.5)

d(A,B)
d(LR(8, 0.5, 1), RL(2, 1, 1.5))

d(A,A)

d(d(A,B),B)
d(A,d(B,B))

C = LR(-3, 0.5, 1)
d(A,C)
xlim=c(-3,9.5)
ylim=c(0,1.3)
LRFN.plot( A, xlim=xlim, ylim=ylim, lwd=2, lty=2, col=2) 
LRFN.plot( B, xlim=xlim, ylim=ylim, lwd=2, lty=2, col=3, add=TRUE)
LRFN.plot( d(A,B), xlim=xlim, ylim=ylim, lwd=2, lty=3, col=4, add=TRUE)
LRFN.plot( d(d(A,B),B), xlim=xlim, ylim=ylim, lwd=2, lty=4, col=1, add=TRUE)
legend( "topleft", c("A = LR(8, 0.5, 1)", "B = RL(2, 1, 1.5)", "A / B = LR(4, 3.25, 2.5)"
     , "(A / B) / B = LR(2, 3.125, 2.25)"), col = c(2, 3, 4, 1), text.col = 1, lwd = c(2,2,2)
     , lty = c(2, 2, 3, 4) )


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
        if ((sign(M) == "Positive") & (sign(N) == "Positive")) {
            a1 = M[1]/N[1]
            a2 = ((M[1] * N[3]) + (N[1] * M[2]))/(N[1]^2)
            a3 = ((M[1] * N[2]) + (N[1] * M[3]))/(N[1]^2)
            a4 = M[4]
            print(noquote(paste0("the result of division is approximately  (core = ", 
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
         return(noquote(paste0(
"A regular approximation is not defined for division since at least one of LR FNs is not positive"
                )))
        }
    }
    else {
        return(noquote(paste0("Division has NOT a closed form of a LR fuzzy number")))
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

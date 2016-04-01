\name{multiplication}
\alias{m}

\title{
Product of two LR fuzzy numbers
}

\description{
This function calculates the multiplication (product) of two LR fuzzy numbers.
Although on the basis of Zadeh extension principle, the class of LR fuzzy numbers is not closed under the operations multiplication and division, 
but we consider the following approximation for the product of two LR fuzzy numbers \eqn{ M=(m, \alpha, \beta)_{LR} } and  \eqn{ N=(n, \gamma, \delta)_{LR} } in this package to work easy in the class of LR fuzzy numbers:
\deqn{ M \otimes N 
\simeq \left\{
\begin{array}{lcc}
(mn, m \gamma + n \alpha, m \delta + n \beta)_{LR} &\ \ if & \ \ M \succ 0 \ and \ N \succ 0
\\
(mn, m \gamma - n \beta, m \delta - n \alpha)_{RL} &\ \ if & \ \ M \succ 0 \ and \ N \prec 0
\\
(mn, -n \beta - m \delta , -n \alpha -m \gamma)_{RL} &\ \ if & \ \ M \prec 0 \ and \ N \prec 0
\end{array}
\right. 
 }
}

\usage{
m(M, N)
}
%- maybe also 'usage' for other objects documented here.

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
Right.fun = function(x)  { (1/(1+(2*abs(x))))*(x>=0)}
A = LR(1, 0.6, 0.2)
B = LR(-3, 0.5, 1)
m(A, B)
m(B, A)
xlim = c(-5,4)
ylim=c(0,1.15)
LRFN.plot( A, xlim=xlim, ylim=ylim, lwd=2, lty=2, col=2) 
LRFN.plot( B, xlim=xlim, ylim=ylim, lwd=2, lty=2, col=3, add=TRUE)
legend( "topright", c("A = LR(1, 0.6, 0.2)", "B = LR(-3, 0.5, 1)"), col = c(2, 3)
     , text.col = 1, lwd = c(2,2), lty = c(2, 2) )


# Example 2:
Left.fun  = function(x)  { (1-x)*(x>=0)}
Right.fun = function(x)  { (exp(-x))*(x>=0)}
A = LR(1.5, 1, 2)
B = LR(3, 2, 1)
xlim = c(-3,20)
ylim=c(0,1.15)
LRFN.plot( A, xlim=xlim, ylim=ylim, lwd=2, lty=2, col=2) 
LRFN.plot( B, xlim=xlim, ylim=ylim, lwd=2, lty=2, col=3, add=TRUE)
LRFN.plot( m(A,B), xlim=xlim, ylim=ylim, lwd=2, lty=3, col=1, add=TRUE)
legend( "topright", c("A = LR(1.5, 1, 2)", "B = LR(3, 2, 1)", "A * B = LR(4.5, 6, 7.5)")
     , col = c(2, 3, 1), text.col = 1, lwd = c(2,2,2), lty = c(2, 2, 3) )


# Example 3:
M = LR(1.2, 0.6, 0.2)
N = LR(3, 0.5, 1)

m(M,N)
m( LR(1.2, 0.6, 0.2) , LR(3, 0.5, 1) )

m(N,m(M,M))
m(m(N,M),M)

xlim = c(-2,10)
ylim=c(0,1.4)
LRFN.plot( M, xlim=xlim, ylim=ylim, lwd=2, lty=2, col=2) 
LRFN.plot( N, xlim=xlim, ylim=ylim, lwd=2, lty=2, col=3, add=TRUE)
LRFN.plot( m(M,N), xlim=xlim, ylim=ylim, lwd=2, lty=3, col=4, add=TRUE)
LRFN.plot( m(M,M), xlim=xlim, ylim=ylim, lwd=2, lty=4, col=5, add=TRUE)
LRFN.plot( m(m(N,M),M), xlim=xlim, ylim=ylim, lwd=2, lty=5, col=1, add=TRUE)
legend( "topright", c("M = LR(1.2, 0.6, 0.2)", "N = LR(3, 0.5, 1)", "M * N = LR(3.6, 2.4, 1.8)"
     , "M * M = LR(3.6, 2.4, 1.8)", "(N * M) * M = LR(4.32, 5.04, 2.88)"), col = c(2, 3, 4, 5, 1),
     text.col = 1, lwd = c(2,2,2,2,2), lty = c(2, 2, 3, 4, 5) )


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
    if (M[4] != N[4]) {
        return(noquote(paste0("NOT productive")))
    }
    else if ((sign(M) == "Positive") & (sign(N) == "Positive")) {
        a1 = M[1] * N[1]
        a2 = (M[1] * N[2]) + (N[1] * M[2])
        a3 = (M[1] * N[3]) + (N[1] * M[3])
        a4 = (M[4] + N[4])/2
        print(noquote(paste0("the result of multiplication is approximately  (core = ", 
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
    else if ((sign(M) == "Negative") & (sign(N) == "Negative")) {
        a1 = M[1] * N[1]
        a2 = -(M[1] * N[2]) - (N[1] * M[2])
        a3 = -(M[1] * N[3]) - (N[1] * M[3])
        a4 = abs(M[4] - 1)
        print(noquote(paste0("the result of multiplication is approximately  (core = ", 
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
    else if ((sign(M) == "Positive") & (sign(N) == "Negative")) {
        a1 = M[1] * N[1]
        a2 = (M[1] * N[2]) - (N[1] * M[3])
        a3 = (M[1] * N[3]) - (N[1] * M[2])
        a4 = abs(M[4] - 1)
        print(noquote(paste0("the result of multiplication is approximately  (core = ", 
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
    else if ((sign(M) == "Negative") & (sign(N) == "Positive")) {
        a1 = M[1] * N[1]
        a2 = (N[1] * M[2]) - (M[1] * N[3])
        a3 = (N[1] * M[3]) - (M[1] * N[2])
        a4 = abs(N[4] - 1)
        print(noquote(paste0("the result of multiplication is approximately  (core = ", 
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
"A regular approxi. is not defined for multiplication since at least one of FNs is non + and non -"
              )))
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

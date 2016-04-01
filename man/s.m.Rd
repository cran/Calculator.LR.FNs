\name{scalar multiplication}
\alias{s.m}

\title{
Scalar multiplication on LR fuzzy numbers
}

\description{
This function calculates the scalar multiplication of any non-zero real number to any LR fuzzy number on the basis of Zadeh extension principle 
by the following formula which is for any LR fuzzy number \eqn{M=(m, \alpha, \beta)_{LR}} and real number \eqn{ \lambda \in R-\{0\} }: 
\deqn{ \lambda \odot M
= M \odot \lambda
=\left\{
\begin{array}{lcc}
(\lambda m, \lambda \alpha, \lambda \beta)_{LR} &\ \ if   & \  \  \lambda > 0
\\
(\lambda m, -\lambda \beta, -\lambda \alpha)_{RL} &\ \ if   & \  \  \lambda < 0
\end{array}
\right. }
}

\usage{
s.m(k, N)
}

\arguments{
  \item{k}{
A non-zero real number
}
  \item{N}{
A LR (or RL, or L) fuzzy number
}
}

\details{
This function has commutative property, i.e \eqn{ k \odot M = M \odot k }.
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
Right.fun  = function(x)  { (1-x)*(x>=0)}
k = 2
M = LR(1, 0.6, 0.2)
N = L(3, 0.6, 1)
P = RL(5, 0.1, 0.3)
s.m(k, N)

# commutative property for scalar multiplication on LR fuzzy numbers (Jabejaei)
s.m(k, M)
s.m(M, k)

s.m(k, P)
s.m(-2, LR(4,2,1))

s.m(2, s.m(-2, LR(4,2,1)))

# Example 2:
Left.fun  = function(x)  { (1/(1+x^2))*(x>=0)}
Right.fun = function(x)  { (1/(1+(2*abs(x))))*(x>=0)}
A = RL(3,2,1)
xlim=c(-4,15)
LRFN.plot( A, xlim=xlim, ylim=c(0,1), lwd=2, lty=2, col=2) 
LRFN.plot( s.m(0.5, A), xlim=xlim, ylim=c(0,1), lwd=2, lty=3, col=1, add=TRUE)
LRFN.plot( s.m(2, A), xlim=xlim, ylim=c(0,1), lwd=2, lty=4, col=1, add=TRUE)
legend( "topright", c("A = RL(3, 2, 1)", "0.5 A", "2 A"), col = c(2, 1, 1), text.col = 1
     , lwd = c(2,2,2), lty = c(2, 3, 4))


## The function is currently defined as
function (k, N) 
{
    if (messages(N) != 1) {
        return(messages(N))
    }
    if (messages(k) != 1) {
        return(messages(k))
    }
    if (length(k) == 4 & length(N) == 1) {
        zarf = N
        N[1] = k[1]
        N[2] = k[2]
        N[3] = k[3]
        N[4] = k[4]
        k = zarf
    }
    if (k == 0) {
        return(noquote(paste0(" The scalar multiplication is not defined for zero ")))
    }
    else {
        a1 = k * N[1]
        a2 = k * (N[2] * (k > 0) - N[3] * (k < 0))
        a3 = k * (N[3] * (k > 0) - N[2] * (k < 0))
        a4 = N[4]
        print(noquote(paste0("the result of scalar multiplication is  (core = ", 
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

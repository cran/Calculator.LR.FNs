\name{Calculator.LR.FNs-package}
\alias{Calculator.LR.FNs-package}
\alias{Calculator.LR.FNs}
\docType{package}
\title{ Calculator for LR Fuzzy Numbers }

\description{
Calculator for LR Fuzzy Numbers package, i.e. \code{Calculator.LR.FNs} package, is an open source (LGPL 3) package for R which provides the generalized four arithmetic operations \eqn{+}, \eqn{-}, \eqn{\times } and \eqn{\div } on LR fuzzy numbers. 
Arithmetic operations addition and subtraction are based on Zadeh extension principle. Also the scalar multiplication of a real number into a LR fuzzy number is considered in this package on the basis of Zadeh extension principle.
Although the class of LR fuzzy numbers is not theoretically closed under the operations \eqn{\times } and \eqn{\div }, but we apply from approximation for multiplication and division of LR fuzzy numbers which lead the users to a LR fuzzy numbers.
\code{Calculator.LR.FNs} package make it easier for researchers, students and any other interested people about fuzzy Mathematics to experience this with a simple calculator.
Function \code{LRFN.plot} is designed in \code{Calculator.LR.FNs} package for potting the membership function of any LR fuzzy number.
}

\details{
If the Operation has NOT a closed form or is not defined as a LR fuzzy number, one can continue calculations by  \code{FuzzyNumbers}  package to achive a the figure of membership function of final result using cuts of the final result.
}

\author{
Abbas Parchami

Maintainer: \packageMaintainer{Calculator.LR.FNs}
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


\seealso{
FuzzyNumbers
}


\examples{
# Example 1: mean of LR FNs
Left.fun  = function(x)  { (1-x)*(x>=0)}
A = L(6, 1, 2)
B = L(2, 4, 1)
xlim=c(-3,9)
ylim=c(0,1.2)
LRFN.plot( A, xlim=xlim, ylim=ylim, lwd=2, lty=2, col=2) 
LRFN.plot( B, xlim=xlim, ylim=ylim, lwd=2, lty=2, col=3, add=TRUE)
LRFN.plot(  s.m( 0.5 , s(A,B) ), xlim=xlim, ylim=ylim, lwd=2, lty=3, col=1, add=TRUE)
     # ploting the mean of A and B
legend( "topright", c("A = L(6, 1, 2)", "B = L(2, 4, 1)", "(A + B) / 2 = L(4, 2.5, 1.5)")
     , col = c(2, 3, 1), text.col = 1, lwd = c(2,2,2), lty = c(2, 2, 3) )

# Example 2: Compute and ploting  {0.5(A+B)}*A  where A and B are two LR FNs
xlim=c(-3,41)
LRFN.plot( A, xlim=xlim, ylim=c(0,1), lwd=2, lty=2, col=2) 
LRFN.plot( B, xlim=xlim, ylim=c(0,1), lwd=2, lty=2, col=3, add=TRUE)
LRFN.plot( m( s.m( 0.5 , s(A,B) ) , A ) , xlim=xlim, ylim=c(0,1), lwd=2, lty=3, col=1
     , add=TRUE)  # ploting the mean of A and B
legend( "topright", c("A = L(6, 1, 2)", "B = L(2, 4, 1)", "{(A + B) / 2} * A = L(24, 19, 17)")
     , col = c(2, 3, 1), text.col = 1, lwd = c(2,2,2), lty = c(2, 2, 3) )

# Example 3: The mean of n=10 random LR fuzzy numbers
n = 10
Left.fun  = function(x)  { (1-x)*(x>=0)}
Right.fun = function(x)  { (exp(-x))*(x>=0)}
xlim=c(2, 18)
ylim=c(0, 1.15)
sum_x = c(0,0,0,0)

for (i in 1:n)
 {
  x = rnorm(1,10,3)
  x_l = runif(1,0,3)
  x_r = runif(1,0,2)
  X = c()
  X = LR(x, x_l, x_r)
  LRFN.plot( X, xlim=xlim, ylim=ylim, lwd=1, lty=1, col=1, add = (i != 1) )
  sum_x = a( sum_x , X )
 }

sum_x
X_bar = s.m( (1/n) , sum_x )
LRFN.plot( X_bar , xlim=xlim, ylim=ylim, lwd=2, lty=2, col=2, add = TRUE )
legend( "topright", c("LR FNs", "mean of LR FNs"), col = c(1, 2), text.col = 1
     , lwd = c(1, 2), lty = c(1, 2) )


# Example 4:
Left.fun = function(x)  { (1-x^2)*(x>=0)}
Right.fun = function(x)  { (1-x)*(x>=0)}

A = LR(2, 0.5, 1)
B = LR(1, 0.1, 0.6)
C = RL(3, 0.7, 1.5)
D = LR(3, 0.5, 0.3)

m(A,B)
s.m( 1.2 , m(A,B) )
d( s.m( 1.2 , m(A,B) ) , C)
m( d( s.m( 1.2 , m(A,B) ) , C) , D)

xlim = c(-0.2, 6)
ylim=c(0, 1.75)
LRFN.plot( A, xlim=xlim, ylim=ylim, lwd=2, lty=1, col=1) 
LRFN.plot( B, xlim=xlim, ylim=ylim, lwd=2, lty=1, col=2, add=TRUE)
LRFN.plot( C, xlim=xlim, ylim=ylim, lwd=2, lty=1, col=3, add=TRUE)
LRFN.plot( D, xlim=xlim, ylim=ylim, lwd=2, lty=1, col=4, add=TRUE)

LRFN.plot( m(A,B), xlim=xlim, ylim=ylim, lwd=2, lty=2, col=5, add=TRUE)
LRFN.plot( s.m( 1.2 , m(A,B) ), xlim=xlim, ylim=ylim, lwd=2, lty=3, col=6, add=TRUE)
LRFN.plot( d( s.m( 1.2 , m(A,B) ) , C), xlim=xlim, ylim=ylim, lwd=2, lty=4, col=7, add=TRUE)
LRFN.plot( m( d( s.m( 1.2 , m(A,B) ) , C) , D), xlim=xlim, ylim=ylim, lwd=2, lty=5
     , col=8, add=TRUE)

legend( "topright", c("A = LR(2, 0.5, 1)", "B = LR(1, 0.1, 0.6)", "C = RL(3, 0.7, 1.5)"
     , "D = LR(3, 0.5, 0.3)", "A * B = LR(2, 0.7, 2.2)", "1.2 (A * B) = LR(2.4, 0.84, 2.4)"
     , "{1.2 (A * B)} / C = LR(0.8, 0.68, 1.067)", "[{1.2 (A * B)} / C] * D = LR(2.4, 2.44, 3.44)")
     , col = c(1:8), text.col = 1, lwd = c(2,2,2,2,2,2,2,2), lty = c(1, 1, 1 ,1 , 2, 3, 4, 5) )


# Example 5:
Left.fun = function(x)  { (1-x^3)*(x>=0)}
Right.fun = function(x)  { (1-x)*(x>=0)}

A = LR(5, 0.5, 1)
B = LR(2, 0.3, 0.6)
C = RL(1, 0.7, 1.5)
D = LR(0.5, 0.5, 1)

E = s.m(a(A,B), 1/2) # The mean of A and B
F = s(s.m(a(A,B), 1/2), C)
G = m(F,D)

xlim = c(-1, 6)
ylim = c(0, 1.5)
LRFN.plot( A, xlim=xlim, ylim=ylim, lwd=3, lty=1, col=1) 
LRFN.plot( B, xlim=xlim, ylim=ylim, lwd=3, lty=1, col=2, add=TRUE)
LRFN.plot( C, xlim=xlim, ylim=ylim, lwd=3, lty=1, col=3, add=TRUE)
LRFN.plot( D, xlim=xlim, ylim=ylim, lwd=3, lty=1, col=4, add=TRUE)

LRFN.plot( E, xlim=xlim, ylim=ylim, lwd=3, lty=2, col=5, add=TRUE)
LRFN.plot( F, xlim=xlim, ylim=ylim, lwd=3, lty=3, col=6, add=TRUE)
LRFN.plot( G, xlim=xlim, ylim=ylim, lwd=3, lty=4, col=7, add=TRUE)

legend( "topleft", c("A = LR(5, 0.5, 1)", "B = LR(2, 0.3, 0.6)", "C = RL(1, 0.7, 1.5)",
   "D = LR(0.5, 0.5, 1)", "(A + B)/2 = LR(3.5, 0.4, 0.8)", "[(A + B)/2] - C = LR(2.5, 1.9, 1.5)",
   "{[(A + B)/2] - C} * D  = LR(1.25, 2.2, 3.2)" ),  col = c(1:7), text.col = 1, 
   lwd = c(2,2,2,2,2,2,2), lty = c(1, 1, 1, 1 , 2, 3, 4), bty = "n" )

}

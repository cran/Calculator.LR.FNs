\name{L}
\alias{L}

\title{
Introducing the form of L fuzzy number
}

\description{
Considering the definition of LR fuzzy number in \code{LR}, if the left and the right shape functions of a LR fuzzy number are be equal (i.e., \eqn{ L(.) = R(.) }), then LR fuzzy number is a L fuzzy number which denoted by \eqn{ (n, \alpha, \beta)L }.
Function \code{L}  introduce a total form for L fuzzy number to computer.
}

\usage{
L(m, m_l, m_r)
}

\arguments{
  \item{m}{
The core of L fuzzy number
}
  \item{m_l}{
The left spread of L fuzzy number
}
  \item{m_r}{
The right spread of L fuzzy number
}
}

%% \details{
%%  ~~ If necessary, more details than the description above ~~
%% }

\value{
This function help to users to define any L fuzzy number after introducing the left shape function L. This function consider L fuzzy number L(m, m_l, m_r) as a vector with 4 elements. The first three elements are m, m_l and m_r respectively; and the fourth element is considerd equal to 0.5 for distinguish L fuzzy number from LR and RL fuzzy numbers.
}

\references{
Dubois, D., Prade, H., Fuzzy Sets and Systems: Theory and Applications. Academic Press (1980).

Taheri, S.M, Mashinchi, M., Introduction to Fuzzy Probability and Statistics. Shahid Bahonar University of Kerman Publications, In Persian (2009).
}


\author{
Abbas Parchami }

\examples{
# First introduce the left shape function of L fuzzy number
Left.fun  = function(x)  { (1-x^2)*(x>=0)}
A = L(20, 12, 10)
LRFN.plot(A, xlim=c(0,60), col=2, lwd=2)

## The function is currently defined as
function (m, m_l, m_r) 
{
    c(m, m_l, m_r, 0.5)
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


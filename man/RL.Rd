\name{RL}
\alias{RL}

\title{
Introducing the form of RL fuzzy number
}

\description{
Considering the definition of LR fuzzy number in \code{LR}, it is obvious that \eqn{ (n, \alpha, \beta)RL } will be a RL fuzzy number.
Function \code{RL} introduce a total form for RL fuzzy number to computer. 
}

\usage{
RL(m, m_l, m_r)
}

\arguments{
  \item{m}{
The core of RL fuzzy number
}
  \item{m_l}{
The left spread of RL fuzzy number
}
  \item{m_r}{
The right spread of RL fuzzy number
}
}

%% \details{
%%  ~~ If necessary, more details than the description above ~~
%% }

\value{
This function help to users to define any RL fuzzy number after introducing the left shape and the right shape functions L and R. This function consider RL fuzzy number RL(m, m_l, m_r) as a vector with 4 elements. The first three elements are m, m_l and m_r respectively; and the fourth element is considerd equal to 1 for distinguish RL fuzzy number from LR and L fuzzy numbers.
}


\references{
Dubois, D., Prade, H., Fuzzy Sets and Systems: Theory and Applications. Academic Press (1980).

Taheri, S.M, Mashinchi, M., Introduction to Fuzzy Probability and Statistics. Shahid Bahonar University of Kerman Publications, In Persian (2009).
}


\author{
Abbas Parchami }

\examples{
# First introduce left and right shape functions of RL fuzzy number
Left.fun  = function(x)  { (1-x^2)*(x>=0)}
Right.fun = function(x)  { (exp(-x))*(x>=0)}
A = RL(40, 12, 10)
LRFN.plot(A, xlim=c(0,60), col=1)

## The function is currently defined as
function (m, m_l, m_r) 
{
    c(m, m_l, m_r, 1)
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

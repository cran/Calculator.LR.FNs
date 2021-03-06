\name{LR}
\alias{LR}

\title{
Introducing the form of LR fuzzy number
}


\description{
Function \code{LR} introduce a total form for LR fuzzy number. 
Note that, if the membership function of fuzzy number \eqn{N} is 
 \deqn{ N(x)=\left\{
\begin{array}{lcc}
L \left(  \frac{n-x}{\alpha} \right)   &\ \ if   & \  \  x \leq n
\\
R \left( \frac{x-n}{\beta} \right)   &\ \ if   & \  \  x > n
\end{array}
\right. }
where \eqn{L} and \eqn{R} are two non-increasing functions from \eqn{ R^+ \cup \{0\} } to \eqn{[0,1]} (say left and right shape function) and \eqn{L(0)=R(0)=1} and also \eqn{\alpha,\beta>0}; 
then \eqn{N} is named a LR fuzzy number and we denote it by  \eqn{ N=(n, \alpha, \beta)LR } in which  \eqn{n} is core and  \eqn{\alpha} and  \eqn{\beta} are left and right spreads of \eqn{N}, respectively. 
}


\usage{
LR(m, m_l, m_r)
}

\arguments{
  \item{m}{
The core of LR fuzzy number
}
  \item{m_l}{
The left spread of LR fuzzy number
}
  \item{m_r}{
The right spread of LR fuzzy number
}
}

%% \details{
%%  ~~ If necessary, more details than the description above ~~
%% }

\value{
This function help to users to define any LR fuzzy number after introducing the left shape and the right shape functions L and R. This function consider LR fuzzy number LR(m, m_l, m_r) as a vector with 4 elements. The first three elements are m, m_l and m_r respectively; and the fourth element is considerd equal to 0 for distinguish LR fuzzy number from RL and L fuzzy numbers.
}


\references{
Dubois, D., Prade, H., Fuzzy Sets and Systems: Theory and Applications. Academic Press (1980).

Taheri, S.M, Mashinchi, M., Introduction to Fuzzy Probability and Statistics. Shahid Bahonar University of Kerman Publications, In Persian (2009).
}


\author{
Abbas Parchami }

\examples{
# First introduce left and right shape functions of LR fuzzy number
Left.fun  = function(x)  { (1-x^2)*(x>=0)}
Right.fun = function(x)  { (exp(-x))*(x>=0)}
A = LR(20, 12, 10)
LRFN.plot(A, xlim=c(0,60), col=1)

## The function is currently defined as
function (m, m_l, m_r) 
{
    c(m, m_l, m_r, 0)
  }
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Calculator for LR Fuzzy Numbers }
\keyword{ Zadehs extension principle }
\keyword{ Introducing the form of LR fuzzy number Fuzzy Number }
\keyword{ Introducing the form of RL fuzzy number Fuzzy Number }
\keyword{ Introducing the form of L fuzzy number Fuzzy Number }
\keyword{ Ploting and drawing LR fuzzy numbers }

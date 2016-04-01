\name{sign}
\alias{sign}

\title{
Sign of LR fuzzy number
}

\description{
To distinguish and determining the sign of a LR fuzzy number one can use from this function. 
In other words, the function \code{sign} is able to categorize the class of all LR fuzzy numbers into three kinds positive, negative and non of them (non-positive and non negative).
}

\usage{
sign(M)
}

\arguments{
  \item{M}{
A LR, RL or L fuzzy number
}
}

\value{
The "sign" function only return three charactical values: "Positive", "Negative" or "non-positive and non negative".
}

\author{
Abbas Parchami }

\examples{
Left.fun  = function(x)  { (1-x)*(x>=0)}

M = L(2,4,3)
support(M)
sign(M)

sign( L(5,4,3) )

( sign( L(5,4,3) ) == "Positive" )

## The function is currently defined as
function (M) 
{
    supp = support(M)
    if (supp[1] > 0) {
        return(noquote(paste0("Positive")))
    }
    else {
        if (supp[2] < 0) {
            return(noquote(paste0("Negative")))
        }
        else {
            return(noquote(paste0("non-positive and non negative")))
        }
    }
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
\keyword{ Product of two LR fuzzy numbers }
\keyword{ Division of two LR fuzzy numbers }

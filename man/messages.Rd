\name{messages}
\alias{messages}

\title{
messages}

\description{
The purpose of this function is supporting the functions of this package (by introducing some nested "if-else" conditions) from all possible messages which are defined in functions of this package.
The "messages" function is used in most of functions of this package.
}

\usage{
messages(M)
}

\arguments{
  \item{M}{
A L, LR or RL fuzzy number
}
}

\value{
Some special messages like: "NOT additive", "NOT productive", .... If any message is not necessary for this function, then the value 1 will be return by this function which is used in the text and the body of other functions.
}

\author{
Abbas Parchami }

\note{
This function has not any applications for users of package and it considered only for shortening the length of programming.
}


\examples{
messages("NOT additive")
messages( LR(3,1,1) )

## The function is currently defined as
function (M) 
{
    options(warn = -1)
    if (M == "Addition has NOT a closed form of a LR fuzzy number") {
        return(noquote(paste0("Addition has NOT a closed form of a LR fuzzy number")))
    }
    else if (M == "Subtraction has NOT a closed form of a LR fuzzy number") {
        return(noquote(paste0("Subtraction has NOT a closed form of a LR fuzzy number")))
    }
    else if (M == "Production has NOT a closed form of a LR fuzzy number") {
        return(noquote(paste0("Production has NOT a closed form of a LR fuzzy number")))
    }
    else if (M == "Division has NOT a closed form of a LR fuzzy number") {
        return(noquote(paste0("Division has NOT a closed form of a LR fuzzy number")))
    }
    else if (M == " The fourth element of each LR fuzzy number must be 0 or 0.5 or 1! ") {
     return(noquote(paste0(" The fourth element of each LR fuzzy number must be 0 or 0.5 or 1! ")))
    }
    else if (M == " The scalar multiplication is not defined for zero ") {
        return(noquote(paste0(" The scalar multiplication is not defined for zero ")))
    }
    else if (M == 
"A regular approxi. is not defined for multiplication since at least one of FNs is non + and non -"
         ) {
        return(noquote(paste0(
"A regular approxi. is not defined for multiplication since at least one of FNs is non + and non -"
               )))
    }
    else if (M == 
"A regular approximation is not defined for division since at least one of LR FNs is not positive"
         ) {
        return(noquote(paste0(
"A regular approximation is not defined for division since at least one of LR FNs is not positive"
               )))
    }
    else {
        return(1)
    }
  }
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Calculator for LR Fuzzy Numbers }

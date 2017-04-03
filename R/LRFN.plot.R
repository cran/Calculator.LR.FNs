LRFN.plot <-
function(M, Left.fun = NULL, Right.fun = NULL, ...)
{
if ( messages(M) != 1 )  { return( messages(M) ) }

m = M[1]
m_l = M[2]
m_r = M[3]

x <- NULL

if ( M[4] == 0 ) { y = function(x) Left.fun((m-x)/m_l) * (x<=m) + Right.fun((x-m)/m_r) * (m<x) }
  else if ( M[4] == 1 ) { y = function(x) Right.fun((m-x)/m_l) * (x<=m) + Left.fun((x-m)/m_r) * (m<x) }
  else if ( M[4] == 0.5 ) { y = function(x) Left.fun((m-x)/m_l) * (x<=m) + Left.fun((x-m)/m_r) * (m<x) }
  else { return( noquote( paste0("The fourth element of each LR fuzzy number must be 0 or 0.5 or 1!" ) ) ) }		

return(curve(y(x) * (0<=y(x) & y(x)<=1), ...) )
}

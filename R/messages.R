messages <-
function (M)  {

options(warn = -1) 

if  ( M == "NOT additive" ) 
  {
   return( noquote( paste0("NOT additive" ) ) )
  } 
 else if  ( M == "NOT subtractive" ) 
  {
   return( noquote( paste0("NOT subtractive" ) ) )
  } 
 else if  ( M == "NOT productive" ) 
  {
   return( noquote( paste0("NOT productive" ) ) )
  } 
 else if  ( M == "NOT dividable" ) 
  {
   return( noquote( paste0("NOT dividable" ) ) )
  } 

 else if  ( M == " The fourth element of each LR fuzzy number must be 0 or 0.5 or 1! " ) 
  {
   return( noquote( paste0(" The fourth element of each LR fuzzy number must be 0 or 0.5 or 1! " ) ) )
  } 
 else if  ( M == " The scalar multiplication is not defined for zero " ) 
  {
   return( noquote( paste0(" The scalar multiplication is not defined for zero " ) ) )
  } 
 else if  ( M == "A regular approximation is not defined for multiplication since at least one of LR fuzzy numbers is non-positive and non-negative fuzzy number" ) 
  {
   return( noquote( paste0("A regular approximation is not defined for multiplication since at least one of LR fuzzy numbers is non-positive and non-negative fuzzy number" ) ) )
  } 
 else if  ( M == "A regular approximation is not defined for division since at least one of LR fuzzy numbers is not positive" ) 
  {
   return( noquote( paste0("A regular approximation is not defined for division since at least one of LR fuzzy numbers is not positive" ) ) )
  } 
 else
  {
   return( 1 )
  } 
}

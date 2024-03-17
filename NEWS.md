
## Version 2.0.0
  * Changing the internal functions to match exactly
  with the equations presented in Strona & Veech (2015).
  This only affects the special case of min(N1,N2) == 0,
  i.e., the situation where one node has no (potential)
  neighbours.

  * Adding unit tests (ensuring R results match python
  package results)
  
  * Changing code to match new CRAN standards
  
  * Adding license
  
  * Changing continuous integration from Travis to Circle CI

## Version 1.2.0

  * Changing code to match new CRAN standards

## Version 1.1.0

  * Corrected a bug regarding the use of matrices as input data
  * Added a new function enabling the conversion of frequency
    interaction matrices to edge lists
  * Updated the help files
  
## Version 1.0.0

  * Initial version

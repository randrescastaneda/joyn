## Resubmission
This is a resubmission. In this version I have:

* updated the documentation of argument `verbose` in merge.R.

## Test environments
* Local Windows 10, R 4.0.4
* Github Actions checked for windows-latest (release), macOS-latest (release), ubuntu-20.04 (release), ubuntu-20.04 (devel)

## R CMD check results

* When run devtools::check() locally, I got
0 errors | 1 warning | 0 notes 

Twarning was:
checking data for ASCII and uncompressed saves ... OK
   WARNING
  'qpdf' is needed for checks on size reduction of PDFs

* When run check_rhub() locally, I got
0 errors | 0 warning | 1 notes 

The note reads: 
* Maintainer: 'R.Andres Castaneda <acastanedaa@worldbank.org>'
  
  New submission
  Possibly mis-spelled words in DESCRIPTION:
  
    Stata (11:29)
    dplyr (10:39)

Explanation: My Name is "R.Andres". Also, I need to mention both Stata and the 
R package dplyr to make it clear what my package does.

* When run in Github Actions, I got, 
0 errors | 0 warning | 0 notes 
  

## Resubmission
This is a resubmission. In this version I have:

* removed 'LazyData' from DESCRIPTION

* The note reads: 
  Possibly mis-spelled words in DESCRIPTION:
  
    Stata (11:29)
    dplyr (10:39)

Explanation: I need to mention both Stata and the 
R package dplyr to make clear what my package does.

## version 0.1.3
* changes and additions are available in NEWS.md


## Test environments
* Local Windows 10, R 4.0.5
* Github Actions checked for windows-latest (release), macOS-latest (release), ubuntu-20.04 (release), ubuntu-20.04 (devel)
0 errors | 0 warning | 0 notes 

## R CMD check results

* When run devtools::check() locally, I got
0 errors | 1 warning | 0 notes 

Warning was:
checking data for ASCII and uncompressed saves ... OK
   WARNING
  'qpdf' is needed for checks on size reduction of PDFs

* When run check_rhub() locally, I got
0 errors √ | 0 warnings √ | 1 note x

The note reads: 
* 'LazyData' is specified without a 'data' directory

Explanation: I modified the status of data in previous version from external to internal.
However, I created rd files for the data in order to make it available for the 
examples and the vignettes. 

* When run in Github Actions, I got, 
0 errors | 0 warning | 0 notes 

* Also, I got the following note. Error: Bioconductor version '3.13' requires R version '4.1'; R version is too new; see https://bioconductor.org/install
  Should I downgrade my version of R?

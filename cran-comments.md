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


* When run in Github Actions, I got no error, no warnings, no notes. 
  

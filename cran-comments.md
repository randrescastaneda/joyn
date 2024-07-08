# Version 0.2.2

0 errors | 0 warnings | 0 notes

# version 0.2.0

0 errors | 0 warnings | 0 notes

## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

## Resubmission
This is a resubmission. In this version We have fixed the following notes:
Found the following HTML validation problems:
full_join.html:303:1 (full_join.Rd:179): Warning: trimming empty <dt>
inner_join.html:303:1 (inner_join.Rd:179): Warning: trimming empty <dt>
left_join.html:303:1 (left_join.Rd:179): Warning: trimming empty <dt>
right_join.html:303:1 (right_join.Rd:179): Warning: trimming empty <dt>

These notes were created because there was a typo in our roxigen comments. They has been fixed. All check are still passing:

0 errors ✔ | 0 warnings ✔ | 0 notes ✔


# version 0.1.4
* changes and additions are available in NEWS.md


## Test environments
* Local Windows 10, R 4.1.1
* Github Actions checked for windows-latest (release), macOS-latest (release), ubuntu-20.04 (release), ubuntu-20.04 (devel)
0 errors | 0 warning | 0 notes 

## R CMD check results

### When run devtools::check() locally, I got
0 errors | 1 warning | 0 notes 

Warning was:
checking data for ASCII and uncompressed saves ... OK
   WARNING
  'qpdf' is needed for checks on size reduction of PDFs

### When run check_rhub() locally, I got
0 errors √ | 0 warnings √ | 1 note x

The note reads: 

> checking CRAN incoming feasibility ... NOTE
  
URL: https://rdatatable.gitlab.io/data.table/reference/merge.html
  From: man/merge.Rd
Found the following (possibly) invalid URLs:
  Status: Error
  Message: libcurl error code 35:
  	schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed).


* Explanation: I am referring to the original documentation of data.table. I read
online that it could be ignored. Right?


### When run in Github Actions, I got, 
0 errors | 0 warning | 0 notes 


## Resubmission
This is a resubmission. In this version I have:

* Added trailing slashes to the URLs that I think are failing. I apologize, but 
I don't understand the problem with the URL. According to the CRAN email, there 
is a problem with the codecov url in one of the badges of the readme file. I 
am using the very same url structure of very-well known packages like dplyr or
tidyr. In case it fails again, could you please expand on the details of 
the problem? Thanks. 


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


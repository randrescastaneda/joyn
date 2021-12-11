## Resubmission
This is a resubmission. In this version I have:

* Added trailing slashes to the URLs that I think are failing. The CRAN email
refers to a url that is not available in my pkg, so I think it is generated 
somewhere else.  I ran all the checks and they all passed. 
The email reads as follows

> Found the following (possibly) invalid URLs:
> URL: https://nam11.safelinks.protection.outlook.com/?url=https%3A%2F%2Fcodecov.io%2Fgh%2Frandrescastaneda%2Fjoyn&amp;data=04%7C01%7Cacastanedaa%40worldbank.org%7Ccd44d020f3c34ff3075208d9bbfc344f%7C31a2fec0266b4c67b56e2796d8f59c36%7C0%7C0%7C637747514149872957%7CUnknown%7CTWFpbGZsb3d8eyJWIjoiMC4wLjAwMDAiLCJQIjoiV2luMzIiLCJBTiI6Ik1haWwiLCJXVCI6Mn0%3D%7C3000&amp;sdata=cH0egYEgJMnZubuwAFal7mhm2HNoR9BZvfcDj6xXYv0%3D&amp;reserved=0 (moved to https://nam11.safelinks.protection.outlook.com/?url=https%3A%2F%2Fapp.codecov.io%2Fgh%2Frandrescastaneda%2Fjoyn&amp;data=04%7C01%7Cacastanedaa%40worldbank.org%7Ccd44d020f3c34ff3075208d9bbfc344f%7C31a2fec0266b4c67b56e2796d8f59c36%7C0%7C0%7C637747514149872957%7CUnknown%7CTWFpbGZsb3d8eyJWIjoiMC4wLjAwMDAiLCJQIjoiV2luMzIiLCJBTiI6Ik1haWwiLCJXVCI6Mn0%3D%7C3000&amp;sdata=ZerXT%2F%2B8RMcjlnIWHNUI1eF1LwDkd8d9Lei3dGq4kHo%3D&amp;reserved=0)
> From: README.md
> Status: 301
> Message: Moved Permanently

> Please change http --> https, add trailing slashes, or follow moved
> content as appropriate.



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


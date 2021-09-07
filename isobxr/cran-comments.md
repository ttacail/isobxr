# isobxr v1.0.1

This is a new patch version fixing bugs. In this version I have made the following changes:

* Prevents any file writing outside tempdir in debian systems 
  for the examples/vignettes/tests
  (removing any pdf outputs in tempdir)

* Proper addressing of tempdir, 
  ensuring proper post-run clearing of tempdir including in windows systems

## Test environments
* OS X install (10.13.6), R 4.0.3 (local)
* Ubuntu Linux 20.04.1 LTS, R-devel, GCC (r-hub)
* Fedora Linux, R-devel, clang, gfortran  (r-hub)
* Debian Linux, R-devel, clang  (r-hub)
* Debian Linux, R-devel, GCC  (r-hub)
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit (r-hub)


## R CMD check results

* 0 errors | 0 warnings | 0 notes 

## Downstream dependencies


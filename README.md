# VACOLS R Scripts

Scripts for the analysis of the Department of Veterans Affairs' VACOLS database

## Installing ROracle

1) [Download from Oracle](http://www.oracle.com/technetwork/topics/intel-macsoft-096467.html) two packages: `instantclient-basiclite-macos.x64-12.1.0.2.0.zip` and `instantclient-sdk-macos.x64-12.1.0.2.0.zip`

2) Move both packages to the `~/Library/Caches/Homebrew/` directory, leaving them zipped, and run:

```sh
brew tap InstantClientTap/instantclient
brew install instantclient-basiclite instantclient-sdk
```

3) Download [ROracle](https://cran.r-project.org/package=ROracle), and from the directory containing the gzipped package, run:

```sh
export LD_LIBRARY_PATH=/usr/local/Cellar/instantclient-basiclite/12.1.0.2.0/lib:$LD_LIBRARY_PATH`
R CMD INSTALL --configure-args='--with-oci-lib=/usr/local/Cellar/instantclient-basiclite/12.1.0.2.0/lib --with-oci-inc=/usr/local/Cellar/instantclient-sdk/12.1.0.2.0/lib/sdk/include' ROracle_1.2-2.tar.gz
```

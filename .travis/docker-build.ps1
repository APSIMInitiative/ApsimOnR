$env:PATH = "C:\Program Files\R\R-4.0.2\bin\x64;C:\rtools40\usr\bin;C:\rtools40\mingw64\bin;" + $env:PATH

# R is by default a shortcut/alias for `run`. We need to override this to run R.exe
Set-Alias -Name 'R' -Value 'R.exe' -Option AllScope

cd apsimonr
if (!$?) { exit 1 }
echo Initialising packrat 
R -e "0" --args --bootstrap-packrat
if (!$?) { exit 1 }

echo Restoring packrat deps
R -e "packrat::restore(restart = FALSE)"
if (!$?) { exit 1 }

echo Building package
R CMD build .
if (!$?) { exit 1 }

mkdir packrat/bundles
echo Creating packrat bundle
R -e "packrat::bundle(file = 'packrat/bundles/ApsimOnR-$env:TRAVIS_OS_NAME-bin_$env:PKGVER.$env:TRAVIS_BUILD_NUMBER.tar.gz', include.lib = T, omit.cran.src = T, include.src = F, include.bundles = F)"
if (!$?) { exit 1 }


# respectables R package

`respectables` is an R package to ...

# Installation

`respectables` is still in development and not published on CRAN. You can install the latest state with:

```r
remotes::install_github("Roche/respectables")
```


## R Package Dependencies

We use `renv` to control for external dependencies, please run:

```
renv::restore()
```

If you would like to test against the latest packages on CRAN run:

```
renv::update()
```

## System Dependencies

## Ubuntu 20.04

In order to run `renv::restore()` you need certain system dependencies installed, run:

```
sudo apt-get install -y libxml2-dev libfontconfig1-dev libharfbuzz-dev \
                        libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev \
                        libjpeg-dev
```

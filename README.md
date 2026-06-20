ggscalebars
================

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge//lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

<!-- badges: end -->

Scalebars are commonly used as alternatives to classical axes. This
package provides scalebars for ggplot.

### Package Installation

``` r
remotes::install_github("NMIephys/ggscalebars")
```

``` r
# read and plot HEKA patch-clamp file
library(ephys4Patchmaster)
```

    ## Lade nötiges Paket: ephys4R

    ## Lade nötiges Paket: scalebars

    ## 
    ## Attache Paket: 'scalebars'

    ## Die folgenden Objekte sind maskiert von 'package:ggscalebars':
    ## 
    ##     coord_scalebars, scalebars, theme_scalebar_h, theme_scalebar_v,
    ##     theme_scalebars

``` r
herg.plot <- (
  read_PATCHMASTER(ephysdata::examplefile("herg"), exp = 1, ser = 1) %>% 
             ggsweeps() )

herg.plot 
```

![](man/figures/readme_plot-1.png)<!-- -->

![](man/figures/drc-1.png)<!-- -->

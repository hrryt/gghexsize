
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gghexsize

<!-- badges: start -->
<!-- badges: end -->

The goal of gghexsize is to …

## Installation

You can install the development version of gghexsize from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("hrryt/gghexsize")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ggplot2)
library(gghexsize)

p <- ggplot(diamonds, aes(carat, depth, z = price)) +
  scale_x_continuous(
    name = "Carat",
    limits = c(0, 3)
  ) +
  scale_y_continuous(
    name = "Total depth percentage",
    label = ~sprintf("%i%%", .x),
    limits = c(50, 75)
  ) +
  scale_fill_viridis_c(
    name = "Median price / $1000",
    label = ~.x / 1000
  )

p +
  geom_hextile(fun = "median", na.rm = TRUE) +
  scale_size_tile(name = "Number of diamonds", limits = c(0, 100))
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r

p +
  geom_hextile(
    aes(weight = price, size = after_stat(ndensity)),
    fun = "median", na.rm = TRUE
  ) +
  scale_size_tile(name = "Price-weighted density", limits = c(0, 0.1))
```

<img src="man/figures/README-example-2.png" width="100%" />

``` r

ggplot(diamonds, aes(carat, depth, fill = NULL)) +
    geom_hextile(colour = "black") +
    scale_size_tile(limits = c(0, 100))
```

<img src="man/figures/README-example-3.png" width="100%" />

# Hex Color pallets for Spectran

Contains named vectors of Hex color codes for Spectran's color pallets.

## Usage

``` r
ColorP
```

## Format

`ColorP` A list containing 5 elements, each a named vector of colors

- Lang:

  401 colors along the spectrum that make a nice, yet nonaccurate
  representation of the visible spectrum. Created from a picture
  provided by `Dieter Lang`.

- Lang_bright:

  same as `Lang` but with a slightly brightened version of the picture.
  Strikes a nice balance between "accuracy" and "nice colors". Is the
  default when starting
  [`Spectran()`](https://litgde.github.io/Spectran/reference/Spectran.md)

- Dan_Bruton:

  401 colors along the spectrum that were made with the
  `cooltools::wavelength2col()` function and are based on Fortran Code
  by Dan_Bruton. Probably most accurate, but black at both spectral
  ends, which does not represent the colors themselves but also human
  sensitivity to them.

- Rainbow:

  401 colors along the spectrum that make a vibrant and pleasing, yet
  nonaccurate representation of the visible spectrum. Created by the
  [`grDevices::rainbow()`](https://rdrr.io/r/grDevices/palettes.html)
  function.

- Color_Rendering:

  14 colors that represent the reference colors used to determine the
  color rendering index.

## Source

[`grDevices::rainbow()`](https://rdrr.io/r/grDevices/palettes.html)

[cooltools](https://github.com/obreschkow/cooltools)

<https://github.com/LiTGde/Spectran/issues/14>

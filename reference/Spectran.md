# Unlock the Spectrum: Easy, Educational, and Engaging Analysis of Light Spectra

Unlock the Spectrum: Easy, Educational, and Engaging Analysis of Light
Spectra

## Usage

``` r
Spectran(
  lang_setting = "English",
  lang_link = FALSE,
  color_palette = "Lang",
  ...
)
```

## Arguments

- lang_setting:

  A language for the application. Currently **Deutsch** for German and
  **English** (default) are implemented. Expects a *character*.

- lang_link:

  Show a header link to the other language version hosted on Posit
  Connect. Expects a *logical* (default FALSE).

- color_palette:

  A color palette for the application. Currently `**Lang**` (default),
  `**Lang_bright**`, `**Dan_Bruton**`, and `**Rainbow**` are
  implemented. Expects a `*character*`. In terms of `color accuracy`,
  the decending order is likely `**Dan_Bruton**`, `**Lang**`,
  `**Lang_bright**`, and `**Rainbow**`. However, all of them are wrong
  in the sense, that monochromatic light can not well be recreated with
  RGB colors. Look at the documentation for
  [ColorP](https://litgde.github.io/Spectran/reference/ColorP.md) for
  more information about these palettes.

- ...:

  Any other settings that get passed to shinyApp

## Value

Open a viewer with the shiny app

## Examples

``` r
if(interactive()) {
Spectran()}

#try another language
if(interactive()) {
Spectran(lang_setting = "Deutsch")}

#or try another color palette
if(interactive()) {
Spectran(color_palette = "Dan_Bruton")}
```


<!-- README.md is generated from README.Rmd. Please edit that file -->

# timesaveR <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/LukasWallrich/timesaveR/workflows/R-CMD-check/badge.svg)](https://github.com/LukasWallrich/timesaveR/actions)
[![Codecov test
coverage](https://codecov.io/gh/LukasWallrich/timesaveR/branch/master/graph/badge.svg)](https://app.codecov.io/gh/LukasWallrich/timesaveR?branch=master)
<!-- badges: end -->

This package aims to speed up research projects in social psychology
(and related fields). For that, it primarily includes some functions
that help lay the groundwork and others that facilitate the reporting of
results.

Among others, the package can help with the following:

- **Creating scales**, including reverse-coding and reporting their
  internal consistency,
- **Creating correlation tables** in APA style, including descriptive
  statistics and confidence intervals, and an option to use survey
  weights, multiple imputation or full-information maximum-likelihood
  estimation
- **Creating regression tables** comparing standardised and
  non-standardised regression coefficients, and comparing the F-change
  between two nested models
- **Formatting numbers for statistical reporting**, including rounding
  with trailing zeros, or displaying numbers as *p*-values or as
  confidence intervals
- **Data entry and transfer**, for quick interactive use. This includes
  splitting clipboard content into vectors, converting tibbles to
  tribble code, or obtaining nicely formatted code and results to paste
  into another application

## Why use this package?

There are many packages that support data analysis and reporting. For
instance, the `psych` package offers functions to create scales, while
the `modelsummary` package offers options to create customisable tables
in a wide variety of output format. They power many of the functions
offered here ‘under the hood.’

`apa` and `papaja` are two packages that directly support the reporting
of results in APA style - they can complement this package well.
However, none of the existing offered quite what we needed. This package

- takes an end-to-end view of the data analysis process, streamlining
  time-consuming steps at various stages
- offers analysis templates that make it easy to get started,
  particularly for R novices,
- prioritises publication-readiness and good reporting practices over
  customisability in creating tables and charts
- integrates with the `tidyverse` by supporting tidy evaluation and
  returning tibbles where possible

## Status and scope

timesaveR grew out of our own research and teaching workflows, and that
is what it is maintained for: it is a personal/lab toolkit, distributed
via GitHub rather than CRAN (a deliberate choice, given the package’s
broad dependency surface and workflow-specific scope). It is tested and
actively used, but expect an opinionated toolkit rather than a
general-purpose product.

Its most distinctive parts are the pieces that are hard to find
elsewhere in one place: **scale construction with reliability and
reverse-coding** (including for survey-weighted and multiply-imputed
data), **survey-weighted and multiple-imputation descriptives,
correlation tables and t-tests**, and **mediation/moderated-mediation
models with publication-ready, annotated path diagrams**. For general
model tables, effect sizes, or full APA manuscripts, packages such as
`modelsummary`, `gtsummary`, `easystats`, `psych` and `papaja` are
excellent - timesaveR aims to complement them, not replace them.

# Installation

You can install timesaveR from GitHub with the command below. If you do
not have the `remotes`-package installed, run
`install.packages("remotes")` first.

``` r
remotes::install_github('lukaswallrich/timesaveR')
```

(timesaveR is not on CRAN, and this is currently not planned - see
above.)

# Get started

There are many functions in the package, and we will create vignettes
detailing various use cases. However, the following can give you an
initial flavor. The examples use data from the European Social Survey
Wave 7 (2014). Here, I ignore survey weights. However, the package
offers similar functions for analysing weighted survey data, which are
explained in the [survey data vignette](doc/survey_functions.html).

## Load the package

(I also load `dplyr` since that is the recommended usage - of course,
there are base R alternatives for all of the steps.)

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(timesaveR)
#> Note re timesaveR: Many functions in this package are alpha-versions - please treat results with care and report bugs and desired features.
```

## Create scales

Let’s create scales for health behaviours and depressive symptoms, each
including some reverse coding.

``` r
scales <- list(
  depression = c("fltdpr", "flteeff", "slprl", "wrhpp", "fltlnl", 
                 "enjlf", "fltsd", "cldgng"),
  healthy_eating = c("etfruit", "eatveg")
  )
  
scales_reverse <- list(
  depression = c("wrhpp", "enjlf"),
  healthy_eating = c("etfruit", "eatveg")
)

scales <- make_scales(ess_health, items = scales, reversed = scales_reverse)
#> The following scales will be calculated with specified reverse coding:
#> depression, healthy_eating
```

<img src="man/figures/README-create-scales-1.png" alt="" width="100%" /><img src="man/figures/README-create-scales-2.png" alt="" width="100%" />

``` r

#Check descriptives, including reliability
scales$descriptives
#> # A tibble: 2 × 10
#>   Scale      n_items reliability reliability_method  mean    SD reversed rev_min
#>   <chr>        <int>       <dbl> <chr>              <dbl> <dbl> <chr>      <dbl>
#> 1 depression       8       0.802 cronbachs_alpha     1.67 0.484 wrhpp e…       1
#> 2 healthy_e…       2       0.658 spearman_brown      4.97 1.11  etfruit…       1
#> # ℹ 2 more variables: rev_max <dbl>, text <chr>

#Add scale scores to dataset
ess_health <- bind_cols(ess_health, scales$scores)
```

## Report correlations and descriptive statistics

Next, we are often interested in descriptive statistics, variable
distributions and correlations.

``` r
ess_health %>% select(agea, health, depression, healthy_eating) %>% 
    cor_matrix() %>% report_cor_table()
```

<div id="urujongvsf" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#urujongvsf table {
  font-family: times, system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#urujongvsf thead, #urujongvsf tbody, #urujongvsf tfoot, #urujongvsf tr, #urujongvsf td, #urujongvsf th {
  border-style: none;
}
&#10;#urujongvsf p {
  margin: 0;
  padding: 0;
}
&#10;#urujongvsf .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 12px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: none;
  border-top-width: 2px;
  border-top-color: #FFFFFF;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#urujongvsf .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#urujongvsf .gt_title {
  color: #333333;
  font-size: 12px;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#urujongvsf .gt_subtitle {
  color: #333333;
  font-size: 12px;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#urujongvsf .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#urujongvsf .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #000000;
}
&#10;#urujongvsf .gt_col_headings {
  border-top-style: none;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 1px;
  border-bottom-color: #000000;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#urujongvsf .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#urujongvsf .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#urujongvsf .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#urujongvsf .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#urujongvsf .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 1px;
  border-bottom-color: #000000;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#urujongvsf .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#urujongvsf .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: none;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#urujongvsf .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: none;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#urujongvsf .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#urujongvsf .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#urujongvsf .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: none;
  border-top-width: 1px;
  border-top-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#urujongvsf .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#urujongvsf .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#urujongvsf .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#urujongvsf .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#urujongvsf .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#urujongvsf .gt_first_summary_row {
  border-top-style: none;
  border-top-color: #D3D3D3;
}
&#10;#urujongvsf .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#urujongvsf .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#urujongvsf .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#urujongvsf .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: none;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#urujongvsf .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: none;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#urujongvsf .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#urujongvsf .gt_table_body {
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #000000;
  border-bottom-style: solid;
  border-bottom-width: 1px;
  border-bottom-color: #000000;
}
&#10;#urujongvsf .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#urujongvsf .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#urujongvsf .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#urujongvsf .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#urujongvsf .gt_left {
  text-align: left;
}
&#10;#urujongvsf .gt_center {
  text-align: center;
}
&#10;#urujongvsf .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#urujongvsf .gt_font_normal {
  font-weight: normal;
}
&#10;#urujongvsf .gt_font_bold {
  font-weight: bold;
}
&#10;#urujongvsf .gt_font_italic {
  font-style: italic;
}
&#10;#urujongvsf .gt_super {
  font-size: 65%;
}
&#10;#urujongvsf .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#urujongvsf .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#urujongvsf .gt_indent_1 {
  text-indent: 5px;
}
&#10;#urujongvsf .gt_indent_2 {
  text-indent: 10px;
}
&#10;#urujongvsf .gt_indent_3 {
  text-indent: 15px;
}
&#10;#urujongvsf .gt_indent_4 {
  text-indent: 20px;
}
&#10;#urujongvsf .gt_indent_5 {
  text-indent: 25px;
}
&#10;#urujongvsf .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#urujongvsf div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Variable"><span class='gt_from_md'>Variable</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="desc"><span class='gt_from_md'><em>M (SD)</em></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="a1"><span class='gt_from_md'>1</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="a2"><span class='gt_from_md'>2</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="a3"><span class='gt_from_md'>3</span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Variable" class="gt_row gt_left"><span class='gt_from_md'><ol>
<li>agea</li>
</ol>
</span></td>
<td headers="desc" class="gt_row gt_right"><span class='gt_from_md'>50.61 (18.51)</span></td>
<td headers="1" class="gt_row gt_left"><span class='gt_from_md'> <br /> </span></td>
<td headers="2" class="gt_row gt_left"><span class='gt_from_md'> <br /> </span></td>
<td headers="3" class="gt_row gt_left"><span class='gt_from_md'> <br /> </span></td></tr>
    <tr><td headers="Variable" class="gt_row gt_left"><span class='gt_from_md'><ol start="2">
<li>health</li>
</ol>
</span></td>
<td headers="desc" class="gt_row gt_right"><span class='gt_from_md'>2.26 (0.92)</span></td>
<td headers="1" class="gt_row gt_left"><span class='gt_from_md'>.28 ***<br /><span style="font-size:80%">[0.25, 0.30]</span></span></td>
<td headers="2" class="gt_row gt_left"><span class='gt_from_md'> <br /> </span></td>
<td headers="3" class="gt_row gt_left"><span class='gt_from_md'> <br /> </span></td></tr>
    <tr><td headers="Variable" class="gt_row gt_left"><span class='gt_from_md'><ol start="3">
<li>depression</li>
</ol>
</span></td>
<td headers="desc" class="gt_row gt_right"><span class='gt_from_md'>1.67 (0.48)</span></td>
<td headers="1" class="gt_row gt_left"><span class='gt_from_md'>.03 *<br /><span style="font-size:80%">[0.00, 0.05]</span></span></td>
<td headers="2" class="gt_row gt_left"><span class='gt_from_md'>.42 ***<br /><span style="font-size:80%">[0.40, 0.44]</span></span></td>
<td headers="3" class="gt_row gt_left"><span class='gt_from_md'> <br /> </span></td></tr>
    <tr><td headers="Variable" class="gt_row gt_left"><span class='gt_from_md'><ol start="4">
<li>healthy_eating</li>
</ol>
</span></td>
<td headers="desc" class="gt_row gt_right"><span class='gt_from_md'>4.97 (1.11)</span></td>
<td headers="1" class="gt_row gt_left"><span class='gt_from_md'>.17 ***<br /><span style="font-size:80%">[0.15, 0.19]</span></span></td>
<td headers="2" class="gt_row gt_left"><span class='gt_from_md'>-.09 ***<br /><span style="font-size:80%">[-0.11, -0.07]</span></span></td>
<td headers="3" class="gt_row gt_left"><span class='gt_from_md'>-.13 ***<br /><span style="font-size:80%">[-0.15, -0.10]</span></span></td></tr>
  </tbody>
  <tfoot>
    <tr class="gt_sourcenotes">
      <td class="gt_sourcenote" colspan="5"><span class='gt_from_md'><em>M</em> and <em>SD</em> are used to represent mean and standard deviation, respectively.</span></td>
    </tr>
    <tr class="gt_sourcenotes">
      <td class="gt_sourcenote" colspan="5"><span class='gt_from_md'>Values in square brackets indicate the confidence interval for each correlation.</span></td>
    </tr>
    <tr class="gt_sourcenotes">
      <td class="gt_sourcenote" colspan="5"><span class='gt_from_md'>† <em>p</em> &lt; .1, * <em>p</em> &lt; .05, ** <em>p</em> &lt; .01, *** <em>p</em> &lt; .001</span></td>
    </tr>
  </tfoot>
</table>
</div>

``` r

#It is often helpful to rename variables in this step
#Use get_rename_tribbles(ess_health) to get most of this code
var_renames <- tibble::tribble(
    ~old,     ~new,     
    "agea",   "Age",  
    "health",  "Poor health",
    "depression",  "Depression",
    "healthy_eating",  "Healthy eating",
)

#A rename tibble or vector automatically only selects the variables included into it
ess_health %>% cor_matrix(var_names = var_renames) %>% report_cor_table()
```

<div id="ialahilteb" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ialahilteb table {
  font-family: times, system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#ialahilteb thead, #ialahilteb tbody, #ialahilteb tfoot, #ialahilteb tr, #ialahilteb td, #ialahilteb th {
  border-style: none;
}
&#10;#ialahilteb p {
  margin: 0;
  padding: 0;
}
&#10;#ialahilteb .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 12px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: none;
  border-top-width: 2px;
  border-top-color: #FFFFFF;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#ialahilteb .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#ialahilteb .gt_title {
  color: #333333;
  font-size: 12px;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#ialahilteb .gt_subtitle {
  color: #333333;
  font-size: 12px;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#ialahilteb .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ialahilteb .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #000000;
}
&#10;#ialahilteb .gt_col_headings {
  border-top-style: none;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 1px;
  border-bottom-color: #000000;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ialahilteb .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#ialahilteb .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#ialahilteb .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#ialahilteb .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#ialahilteb .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 1px;
  border-bottom-color: #000000;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#ialahilteb .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#ialahilteb .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: none;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#ialahilteb .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: none;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#ialahilteb .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#ialahilteb .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#ialahilteb .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: none;
  border-top-width: 1px;
  border-top-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#ialahilteb .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ialahilteb .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#ialahilteb .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#ialahilteb .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#ialahilteb .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ialahilteb .gt_first_summary_row {
  border-top-style: none;
  border-top-color: #D3D3D3;
}
&#10;#ialahilteb .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#ialahilteb .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ialahilteb .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ialahilteb .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: none;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#ialahilteb .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: none;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#ialahilteb .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#ialahilteb .gt_table_body {
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #000000;
  border-bottom-style: solid;
  border-bottom-width: 1px;
  border-bottom-color: #000000;
}
&#10;#ialahilteb .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ialahilteb .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ialahilteb .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ialahilteb .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ialahilteb .gt_left {
  text-align: left;
}
&#10;#ialahilteb .gt_center {
  text-align: center;
}
&#10;#ialahilteb .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#ialahilteb .gt_font_normal {
  font-weight: normal;
}
&#10;#ialahilteb .gt_font_bold {
  font-weight: bold;
}
&#10;#ialahilteb .gt_font_italic {
  font-style: italic;
}
&#10;#ialahilteb .gt_super {
  font-size: 65%;
}
&#10;#ialahilteb .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#ialahilteb .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#ialahilteb .gt_indent_1 {
  text-indent: 5px;
}
&#10;#ialahilteb .gt_indent_2 {
  text-indent: 10px;
}
&#10;#ialahilteb .gt_indent_3 {
  text-indent: 15px;
}
&#10;#ialahilteb .gt_indent_4 {
  text-indent: 20px;
}
&#10;#ialahilteb .gt_indent_5 {
  text-indent: 25px;
}
&#10;#ialahilteb .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#ialahilteb div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Variable"><span class='gt_from_md'>Variable</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="desc"><span class='gt_from_md'><em>M (SD)</em></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="a1"><span class='gt_from_md'>1</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="a2"><span class='gt_from_md'>2</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="a3"><span class='gt_from_md'>3</span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Variable" class="gt_row gt_left"><span class='gt_from_md'><ol>
<li>Age</li>
</ol>
</span></td>
<td headers="desc" class="gt_row gt_right"><span class='gt_from_md'>50.61 (18.51)</span></td>
<td headers="1" class="gt_row gt_left"><span class='gt_from_md'> <br /> </span></td>
<td headers="2" class="gt_row gt_left"><span class='gt_from_md'> <br /> </span></td>
<td headers="3" class="gt_row gt_left"><span class='gt_from_md'> <br /> </span></td></tr>
    <tr><td headers="Variable" class="gt_row gt_left"><span class='gt_from_md'><ol start="2">
<li>Poor health</li>
</ol>
</span></td>
<td headers="desc" class="gt_row gt_right"><span class='gt_from_md'>2.26 (0.92)</span></td>
<td headers="1" class="gt_row gt_left"><span class='gt_from_md'>.28 ***<br /><span style="font-size:80%">[0.25, 0.30]</span></span></td>
<td headers="2" class="gt_row gt_left"><span class='gt_from_md'> <br /> </span></td>
<td headers="3" class="gt_row gt_left"><span class='gt_from_md'> <br /> </span></td></tr>
    <tr><td headers="Variable" class="gt_row gt_left"><span class='gt_from_md'><ol start="3">
<li>Depression</li>
</ol>
</span></td>
<td headers="desc" class="gt_row gt_right"><span class='gt_from_md'>1.67 (0.48)</span></td>
<td headers="1" class="gt_row gt_left"><span class='gt_from_md'>.03 *<br /><span style="font-size:80%">[0.00, 0.05]</span></span></td>
<td headers="2" class="gt_row gt_left"><span class='gt_from_md'>.42 ***<br /><span style="font-size:80%">[0.40, 0.44]</span></span></td>
<td headers="3" class="gt_row gt_left"><span class='gt_from_md'> <br /> </span></td></tr>
    <tr><td headers="Variable" class="gt_row gt_left"><span class='gt_from_md'><ol start="4">
<li>Healthy eating</li>
</ol>
</span></td>
<td headers="desc" class="gt_row gt_right"><span class='gt_from_md'>4.97 (1.11)</span></td>
<td headers="1" class="gt_row gt_left"><span class='gt_from_md'>.17 ***<br /><span style="font-size:80%">[0.15, 0.19]</span></span></td>
<td headers="2" class="gt_row gt_left"><span class='gt_from_md'>-.09 ***<br /><span style="font-size:80%">[-0.11, -0.07]</span></span></td>
<td headers="3" class="gt_row gt_left"><span class='gt_from_md'>-.13 ***<br /><span style="font-size:80%">[-0.15, -0.10]</span></span></td></tr>
  </tbody>
  <tfoot>
    <tr class="gt_sourcenotes">
      <td class="gt_sourcenote" colspan="5"><span class='gt_from_md'><em>M</em> and <em>SD</em> are used to represent mean and standard deviation, respectively.</span></td>
    </tr>
    <tr class="gt_sourcenotes">
      <td class="gt_sourcenote" colspan="5"><span class='gt_from_md'>Values in square brackets indicate the confidence interval for each correlation.</span></td>
    </tr>
    <tr class="gt_sourcenotes">
      <td class="gt_sourcenote" colspan="5"><span class='gt_from_md'>† <em>p</em> &lt; .1, * <em>p</em> &lt; .05, ** <em>p</em> &lt; .01, *** <em>p</em> &lt; .001</span></td>
    </tr>
  </tfoot>
</table>
</div>

``` r

#Often, it is also interesting to include variable distributions
ess_health %>% cor_matrix(var_names = var_renames) %>%
    report_cor_table(add_distributions = TRUE, data = ess_health)
```

<div id="ignfvttmdo" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ignfvttmdo table {
  font-family: times, system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#ignfvttmdo thead, #ignfvttmdo tbody, #ignfvttmdo tfoot, #ignfvttmdo tr, #ignfvttmdo td, #ignfvttmdo th {
  border-style: none;
}
&#10;#ignfvttmdo p {
  margin: 0;
  padding: 0;
}
&#10;#ignfvttmdo .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 12px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: none;
  border-top-width: 2px;
  border-top-color: #FFFFFF;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#ignfvttmdo .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#ignfvttmdo .gt_title {
  color: #333333;
  font-size: 12px;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#ignfvttmdo .gt_subtitle {
  color: #333333;
  font-size: 12px;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#ignfvttmdo .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ignfvttmdo .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #000000;
}
&#10;#ignfvttmdo .gt_col_headings {
  border-top-style: none;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 1px;
  border-bottom-color: #000000;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ignfvttmdo .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#ignfvttmdo .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#ignfvttmdo .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#ignfvttmdo .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#ignfvttmdo .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 1px;
  border-bottom-color: #000000;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#ignfvttmdo .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#ignfvttmdo .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: none;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#ignfvttmdo .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: none;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#ignfvttmdo .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#ignfvttmdo .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#ignfvttmdo .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: none;
  border-top-width: 1px;
  border-top-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#ignfvttmdo .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ignfvttmdo .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#ignfvttmdo .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#ignfvttmdo .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#ignfvttmdo .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ignfvttmdo .gt_first_summary_row {
  border-top-style: none;
  border-top-color: #D3D3D3;
}
&#10;#ignfvttmdo .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#ignfvttmdo .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ignfvttmdo .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ignfvttmdo .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: none;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#ignfvttmdo .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: none;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#ignfvttmdo .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#ignfvttmdo .gt_table_body {
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #000000;
  border-bottom-style: solid;
  border-bottom-width: 1px;
  border-bottom-color: #000000;
}
&#10;#ignfvttmdo .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ignfvttmdo .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ignfvttmdo .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ignfvttmdo .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ignfvttmdo .gt_left {
  text-align: left;
}
&#10;#ignfvttmdo .gt_center {
  text-align: center;
}
&#10;#ignfvttmdo .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#ignfvttmdo .gt_font_normal {
  font-weight: normal;
}
&#10;#ignfvttmdo .gt_font_bold {
  font-weight: bold;
}
&#10;#ignfvttmdo .gt_font_italic {
  font-style: italic;
}
&#10;#ignfvttmdo .gt_super {
  font-size: 65%;
}
&#10;#ignfvttmdo .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#ignfvttmdo .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#ignfvttmdo .gt_indent_1 {
  text-indent: 5px;
}
&#10;#ignfvttmdo .gt_indent_2 {
  text-indent: 10px;
}
&#10;#ignfvttmdo .gt_indent_3 {
  text-indent: 15px;
}
&#10;#ignfvttmdo .gt_indent_4 {
  text-indent: 20px;
}
&#10;#ignfvttmdo .gt_indent_5 {
  text-indent: 25px;
}
&#10;#ignfvttmdo .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#ignfvttmdo div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Variable"><span class='gt_from_md'>Variable</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="desc"><span class='gt_from_md'><em>M (SD)</em></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Distributions"><span class='gt_from_md'>Distributions</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="a1"><span class='gt_from_md'>1</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="a2"><span class='gt_from_md'>2</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="a3"><span class='gt_from_md'>3</span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Variable" class="gt_row gt_left"><span class='gt_from_md'><ol>
<li>Age</li>
</ol>
</span></td>
<td headers="desc" class="gt_row gt_right"><span class='gt_from_md'>50.61 (18.51)</span></td>
<td headers="Distributions" class="gt_row gt_right"><img src="data:image/png;base64,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o0ZisdjT09PNzc3e3p7P/7/jbG/evDl69OiGDRvi4+Nbt249ffr0WrVqEfpkUI7o6OjQ0NA//vhj1KhRpLMAF6DcmSI/P3/y5Mlbt26tV6/e8OHDPTw8VJpamUKheP369f379x88eBATE/P5WK2JiYm9vb21tbWhoWFeXl5SUtKbN28oimratGn//v07dOjA4/G093mgghQKxcSJE4uKihISEvT09EjHAdZDuTNCampqly5d7t+/P3DgwEGDBlXoDEWFQvHq1auEhISkpKR37959+vRJKpUKBAJra2sHB4e2bduqc+gGmODmzZtz5sw5cOBAnz59SGcB1kO5k5ecnCwSid69e/fzzz/jgLguUygUI0aMMDc3v3//Pn7NgirCqZCEpaamikSi9+/fr1y5Es2u43g8Xr9+/aKjo8+fP086C7Ae9txJysvL8/DwePr06cqVK/GIO6AoSiqVDho0qGnTppcvXyadBdgNe+7EKBSKoUOHxsbGzps3D80On+nr6/fp0+fKlSu3b98mnQXYDeVOzG+//Xb48OGxY8e6uLiQzgIMEhQUZG5ujvsAQxWh3Mm4e/fujBkzfHx8evToQToLMItAIOjevfvRo0efPHlCOguwGMqdgLy8vAEDBtSsWTMsLAwnRUBJ3bt3r1at2vLly0kHARZDuRMwc+bMpKSkWbNmmZiYkM4CTFS9evXOnTvv3r3782VoAJWActe2v//+e/369b17927evDnpLMBcvXv3pigKD/GASsOpkFpVVFTUsmXL/Pz8LVu24P5QULZly5Zdu3YtOTnZwsKCdBZgH+y5a9Xy5cufPn06ZcoUNDuUq2/fvvn5+Rs2bCAdBFgJe+7a8+LFCycnJ3d39zlz5pDOAuzw008/JSQkvH79ulq1aqSzAMtgz117QkND+Xz+2LFjSQcB1ujfv396evr27dtJBwH2QblryYULF44dOzZ48GBLS0vSWYA1nJycWrRosWLFCqlUSjoLsAzKXRukUunkyZNtbW1xyRJUVL9+/V68eBEZGUk6CLAMyl0btmzZ8vjx47Fjx1boRu0AFEW5ubk1aNBg6dKl+HoMKgTlTrvs7OyffvqpTZs2bm5upLMA+/B4vD59+jx48ODSpUukswCboNxpt3Tp0o8fP37//fe40wBUjq+vb82aNZctW0Y6CLAJyp1eycnJv/32W0BAQMOG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" style="height:50px;"></td>
<td headers="1" class="gt_row gt_left"><span class='gt_from_md'> <br /> </span></td>
<td headers="2" class="gt_row gt_left"><span class='gt_from_md'> <br /> </span></td>
<td headers="3" class="gt_row gt_left"><span class='gt_from_md'> <br /> </span></td></tr>
    <tr><td headers="Variable" class="gt_row gt_left"><span class='gt_from_md'><ol start="2">
<li>Poor health</li>
</ol>
</span></td>
<td headers="desc" class="gt_row gt_right"><span class='gt_from_md'>2.26 (0.92)</span></td>
<td headers="Distributions" class="gt_row gt_right"><img src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAfQAAAH0CAIAAABEtEjdAAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAAA9hAAAPYQGoP6dpAAAWgklEQVR4nO3dbWxVhf3AcaBQnQVEITAdMlQYSwGDE/ZgoMCGEVFc5gs3xp4g27IFdGaTzKeFLYxsWYAYSYbRaGQEZGHJeMYoD1YmKAQBmWyMBy2CrBHCw6At7e3t/wX535xcEIu293B//Xxe9ZzeQ3+3tN97e86557ZvampqB0AsHdIeAICWJ+4AAYk7QEDiDhCQuAMEJO4AAYk7QEDiDhCQuAMEJO4AAYk7QEDiDhCQuAMEJO4AAYk7QEDiDhCQuAMEJO4AAYk7QEDiDhCQuAMEJO4AAYk7QEDiDhCQuAMEJO4AAYk7QEDiDhCQuAMEJO4AAXVMe4C2Zdu2bcePH097igLp169f3759054C2ihxL6iVK1fu2bMn7SkK5P777xd3SIvdMgABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC401pKSkrSHgHaLnGntXzhC19IewRou8QdICBxBwhI3AECEneAgMQdICBxBwhI3AEC6pj2ALRFx44dO336dNpTFEjnzp27d++e9hS0OeJOCpYsWfLaa6+lPUWBVFRU/OxnP0t7Ctocu2UAAhJ3gIDEHSAgcQcISNwBAhJ3gIDEHSAgcQcISNwBAhJ3gIDEHSAgcQcISNwBAhJ3gIDEHSAgcQcISNwBAhJ3gIDEHSAgcQcISNwBAhJ3gIDEHSAgcQcISNwBAhJ3gIDEHSAgcQcISNwBAhJ3gIDEHSAgcQcISNwBAhJ3gIDEHSAgcQcISNwBAhJ3gIDEHSAgcQcISNwBAhJ3gIDEHSAgcQcISNwBAhJ3gIDEHSAgcQcISNwBAhJ3gIDEHSAgcQcISNwBAhJ3gIDEHSAgcQcISNwBAhJ3gIDEHSAgcQcISNwBAhJ3gIDEHSCgjoX5Mvv27ctkMoX5Wqnr169fx44F+sYCXFCBGjR79uyTJ08W5mulbu7cud27d097CqBNs1sGICBxBwhI3AECEneAgMQdICBxBwhI3AECEneAgMQdICBxBwhI3AECEneAgMQdICBxBwhI3AECEneAgMQdICBxBwhI3AECEneAgMQdICBxBwhI3AECEneAgMQdICBxBwhI3AECEneAgMQdICBxBwhI3AECEneAgMQdICBxBwhI3AECEneAgMQdICBxBwhI3AECEneAgMQdICBxBwhI3AECEneAgMQdICBxBwhI3AECEneAgMQdICBxBwhI3AECEneAgMQdICBxBwhI3AECEneAgMQdICBxBwhI3AECEneAgMQdICBxBwhI3AECEneAgMQdICBxBwhI3AECEneAgMQdICBxBwhI3AECEneAgMQdICBxBwhI3AECEneAgMQdICBxBwhI3AECEneAgMQdICBxBwhI3AECEneAgMQdICBxBwhI3AECEneAgMQdICBxBwhI3AECEneAgMQdICBxBwhI3AECEneAgMQdICBxBwhI3AECEneAgMQdICBxBwhI3AECEneAgMQdICBxh9bVvXv3tEegLRJ3aF09evRIewTaInEHCEjcAQISd4CAxB0gIHEHCEjcAQISd4CAOqY9ALRdb7/99ltvvZX2FAXSs2fPcePGpT1FGyLukJoDBw68/PLLaU9RIAMGDBD3QrJbBiAgcQcISNwBAhJ3gIDEHSAgcQcISNwBAhJ3gIDEHSAgcQcISNwBAhJ3gIDEHSAgcQcISNwBAhJ3gIDEHSAgcQcISNwBAhJ3gIDEHSAgcQcISNwBAhJ3gIDEHSAgcQcISNwBAhJ3gIDEHSAgcQcISNwBAhJ3gIDEHSAgcQcISNwBAhJ3gIDEHSAgcQcISNyBQrjiiivSHqFtEXegEHr16pX2CG2LuAMEJO4AAYk7QEDiDhCQuAMEJO4AAYk7QEAd0x4AaOuOHTv2n//8J+0pCqRTp05Dhw4twBcSdyBle/funTt3btpTFMjVV19dmLjbLQMQkLgDBCTuAAGJO0BA7ZuamgrwZbZu3VpfX1+AL3Q5GDp06Edd3XTXrl2nTp0q8DxpGTx4cNeuXS/4qX379lVXVxd4nrT079+/Z8+eF/zUoUOHqqqqCjxPWq677rqbbrrpgp86evTonj17CjxPWkpLS4cNG1aAL1SguANQSHbLAAQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABiTtAQOIOEJC4AwQk7gABdUx7AFpedXX1+vXrX3311Z07d+7fv//UqVNlZWXXX3/9rbfeOnTo0NGjRw8aNKhDh/iP60ePHn3llVfWr1+/ffv2/fv319TUlJWV9e/f/5ZbbhkyZMgdd9zRv3//9u3bpz0mtI6muJYvX97p/61cuTLtcQph+/btt99++8f+p/fo0WPBggWNjY1pz9taDh48OH78+I/9PnTr1m3+/PkNDQ1pz5uat99+u1PC2bNn056o5XXr1q3TJ1XUPxuR437vvffmfo1XrFiR9jitK5PJPPDAA5f0uH7bbbcdP3487cFb3l/+8pdL+j5cd911e/bsSXvqFNTU1Hz2s59Nfivixb22tvaSfhjyFHXcw/5tfvLkyeXLl6c9RYE0Njbee++9c+fOvaSttm3bVl5efvLkyVaaKhVPP/30D37wg0va5MiRI+Xl5bt3726lkS5bkydP/u9//5v2FK3rww8/THuE1ISN+4wZM9IeoXAefvjh1atXJ9fceOONCxcuPHz4cH19fTabbWhoOHz48Pz583v37p282ZEjR+66665sNlvYeVvLjh07fv7znyfXlJWVzZ49e8+ePTU1NdlstrGx8ejRo8uWLSsvL0/erLGxcfTo0TU1NYWdN01/+9vfFi9enPYUre79999Pe4T0pP2nQ6uorKzMu5uBd8ts2bIl784+9thjmUzmgjeur69/8MEH827/4osvFnjm1pDJZPr06ZO8X+PHjz9z5swFb5zNZufNm5f3fXj88ccLPHNaPvjgg5KSkvNrEG+3THIfXdv5/z0nWtxPnDgxZ86c839qA8f91ltvTd7TadOmfewm5z+9ra2tLcCorWrlypXJO3X77bd/1CNcTl7fS0tL49XtfJlMZsiQIef/joSM+69//evcvVu4cGHa4xRU0ce9pqamqqqqsrLyT3/600VOFIka9/feey95N3v37t2cQ0B1dXXXXHNNcsOlS5cWYNpW9bWvfS15jw4dOvSxm2Sz2X79+iW32rZtWwFGTdcTTzzxUb8m8eJ+55135u7dG2+8kfY4BVXccZ88efJH/Zi2kbjPmjUreTcXLVrUzA1nz56d3HDEiBGtOmdryzssPGbMmGZu+MwzzyQ3fOGFF1p1ztSdvxMvdty7d++eu3fvv/9+2uMUVNgDqm3EsmXLkovJ5ykXl3ca+MaNG4v6sGreuS6TJk1q5oZ5O7XeeeedFpvp8nP69OnkT8ikSZPGjBmT4jytraGh4dixY7nFHj16pDhM4Yl7Ectmsxs3bswtdu/e/dprr23mtp/73Ofy1hT1SWP/+te/kot5yb6Izp07JxfPnDnTYjNdZpqamiZOnHj8+PFzi7179543b94FD6uGcfTo0dzHV1111ZVXXpniMIUn7kXs9OnTycWKiormb3v+D3pRn/BeVVWVXOzVq1czNzxx4kRysfmPjkVnwYIFyVd+rF279oorrkhxngI4fPhw7uOPOoYcWHHH/bnnnvuo/U0PPfRQ2tO1utyzsHPyjg1eXGNjY96ajh2L+EJDyb++2533fPwi3nzzzeRi3vnvYVRVVf3whz/MLT711FMDBgxIcZ7C2L9/f+5jcY+jLVwYK++Ze/OfrrZr1+7IkSN5a4p6j+TcuXOTD+2lpaXN2arpvLMhm3NlnqKTyWSSu9pHjRo1derUFOcpmH//+9+5j6M+bF9EET9ZY+DAgU1NTZ9s27xXtHbp0qVr164tMVQxWbRo0Z49e3KLAwcO/PznP5/iPK1k2rRpubtZVlb297//vY1cC3Pnzp25j/v375/iJKkQ97bo7Nmzv/nNb5JrJk6cmNYwafnrX//6ve99L7nmUq84VhQqKyuffPLJ3OLy5cu7deuW4jyFtHXr1tzH5x62a2trX3vttddff/3NN9986623Tp8+XVZWNnDgwOHDh48YMWL06NGhjkO08qmWqfnlL3+ZvJtRz3P/ZM6/AsHu3bvTHqoQMpnMhx9+uHr16hEjRuR9B5588sm0p2t5J06c6NKlS+4+Tp06Ne8GeefORjrPPZPJJO/a/v37Z86cefG9tR06dJg6derJkyfTnr1liHubk3wed86dd96Z9lCt6NixY+euzf1Rv9hdunTZsGFD2mO2vGw2+41vfCN3N/v161dfX593m8Bxzzu1N/kgd3GlpaUxLlQQ/6gjOQ0NDT/96U/zziPq1KnTokWL0hqpMBoaGhoaGi74Kq0rr7xyzZo1o0aNKvhQrW7evHnr1q3LLb7yyiudOnVKcZ4C++CDD5KL//vf/5q5YX19/cSJE6dMmdL0SQ9oXSbEva2oqqoqLy9/9tln89avXbs28MndH6uurm748OFf//rX81pQ7Pbu3TtlypTc4nPPPde3b9/0xknBu+++e/7KioqKFStW5C6FnclkqqurV61adf5rRP785z9PmzatIJO2FnGPL5PJ/PGPf+zbt+++ffuS6zt16vTqq69e0kufotqwYcMNN9yQfLlvUauvr7/jjjtyi+PGjWv+9RjCyHvRcs+ePbds2VJZWXnPPfdcf/31nTp1at++fUlJSc+ePceNG1dZWVlZWZl3Nb3Zs2e/9NJLhZ26JYl7cBs3buzbt++jjz6at75///579+4dOXJkKlMV0rXXXntuF2Q2m62trT1y5Mi6deseeOCBvFfeZ7PZioqKGO/HNGXKlNxLdrt167Z48eI2cu5j0q5du3Ifjxkz5r333hs2bNhFbl9RUbFv3768y3JMmDChoaGhtUZsbanu8W9FDqhWV1d/1JtEP/HEE0X95pAtoqam5rHHHsv7zvTp06fY3zd8zZo1yXu0adOmi9w48AHVT+b8K8ctWbIk7aE+Ic/cA8pkMrNmzerVq9eKFSvyPvXFL35x165dM2bMKOqLDbSIz3zmMzNnzsx749mDBw++/PLLaY306R07duxb3/pWbvGRRx7Ju8w9F1deXv79738/uebpp59Oa5hPSdyj2blz580333z+saDS0tLnn3/+nXfeGTRoUCqDXZ6mTp365S9/Obkm7wrvRSSbzY4fP76uru7c4uDBg3//+9+nO1Ix+sUvfpFcPHeabFrDfBriHkdTU9OcOXOGDBly8ODBvE89+OCDx44dmzRpUlu45M6l+t3vfpdcXLZsWZH+Mi9evHjz5s3nPi4pKVmzZk3sK/q2krxLqmWz2bxLhxaLtv63eRiZTOa+++47fz/MiBEj5s+ff+ONN6YyVVHIu/h7Nps9c+ZM868reflIns3Z2NjYnP/0vKOFV111VfLh/9vf/vaCBQtacMKi0Llz55KSkuRlU2tra/NOpCkKnsdFkMlk7r777ryyl5WVrVq1qrKyMnzZH3/88dKEf/zjH5e0efKd2M45e/Zsy02XmoZmyNuksbEx+dm8V/C3HXnvdlCkfwCJewSTJ0/OOww4duzYw4cPjxs3ri2cA9etW7dkki746pWLOP9Xt029kpM85/50S64p0kutiXvRW7p0ad7fztOnT1+9evXVV1+d1kgFlnep7uSFXpujvr4+b00x7pOhpeRdlKZnz55FeqlIcS9udXV1eVfrnT59+m9/+9u28IQ9J+8tqJJvJtccecefy8vLi/Sw88MPP3ypp0L/6Ec/Sv4LdXV1yc+++OKLKd2VT+XkyZPJ3XTNf0Pdc9auXZtcHDt2bItOVzhF+UNMzsKFC2tqanKL3/zmN6dPn57iPKno169fcifp3r1789517+IWLlyYXLz//vtbbLLLXt7DWIznBF27di0tLc3tptuxY0fzT3dpamqaMWNGcs1PfvKTVpixEMS9uM2cOTP3cWlp6fz582P8fl6SkpKSH//4x8k1zz//fDO3PXr06B/+8Ifkmu9+97stNhlpaN++/YQJE5Jr5s+f38xtly5dmnxzrrKysuJ9FZi4F7F33303efDw0UcfbTv72fPkvf3II4880pzDqvX19XfffXdyn/uoUaPa4PuxxZO3r/Khhx46cODAx2515MiRvIf2Z555pkhPlWkn7kVt/fr1ycXJkyenNUnq+vfvn3zVeDabHTJkSPLSUeerqqr6yle+smXLluTKF154oZUmpJBGjhyZdyRm8ODBr7/++kU22blzZ3l5ee71ve3atSsvL//Od77TWiO2PnEvYsuWLUsu3nTTTaWfQm1tbVp3pEU89dRTZWVlucVTp07dcsstEyZM2LRp0/Hjx8+9U0dTU9OJEyc2b948adKkvn377tixI/kvLFiwIOQbZLdB7du3z/vtqKmpGT58+D333LNu3brq6urcz8Px48c3b9583333DRkyJLlr/txLfIv00Po5XqFaxPKeuTc2NiZfVtfWdOvWbcOGDXkXilm8ePHixYubs/msWbPy3i+bolZeXr5mzZq77roruXLVqlWrVq362G07dOiwcePGPn36tNp0hVDEj0ttXE1NTd5LLRg2bNj27dsv9ZXiXbp0Wb9+/a9+9atWmoq0jB079p///Oel/jXWu3fv3bt3F+9x1BxxL1bHjx9Pe4TL0ZAhQw4dOjR9+vTmvMq0S5cuc+bMqa6uHj16dAFmo/AGDhy4b9++JUuW9O7d+2Nv3LNnz2efffbAgQN51w4rUu2L9AJ4cHH19fVbt25du3btG2+8sWfPnsOHD2cyma5du958882DBw++7bbbKioqBg0aVNQ7VWm+pqamAwcOvPTSS5s2bdq2bVtVVVVdXd0111xzww03fOlLX/rqV786cuTIAQMGRDqTWNwBAvK0BSAgcQcISNwBAhJ3gIDEHSAgcQcISNwBAhJ3gIDEHSAgcQcISNwBAhJ3gIDEHSAgcQcISNwBAhJ3gIDEHSAgcQcI6P8AQmSjZOFnWAYAAAAASUVORK5CYII=" style="height:50px;"></td>
<td headers="1" class="gt_row gt_left"><span class='gt_from_md'>.28 ***<br /><span style="font-size:80%">[0.25, 0.30]</span></span></td>
<td headers="2" class="gt_row gt_left"><span class='gt_from_md'> <br /> </span></td>
<td headers="3" class="gt_row gt_left"><span class='gt_from_md'> <br /> </span></td></tr>
    <tr><td headers="Variable" class="gt_row gt_left"><span class='gt_from_md'><ol start="3">
<li>Depression</li>
</ol>
</span></td>
<td headers="desc" class="gt_row gt_right"><span class='gt_from_md'>1.67 (0.48)</span></td>
<td headers="Distributions" class="gt_row gt_right"><img src="data:image/png;base64,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a06cONG6eezYMUtLS7ZPissy8BD5+fmhoaFtmp2iqOPHj2O6NF9otdqYmJjhw4ffvXuXdBZRy87Onjt3buvmxo0blUqlGc6Lcof/j06n+/LLL5VK5a1bt4z3W1panj59ulO3PgEXnDp1ysfHx/jmZDCnpqamkSNHtm4+88wzHV9AoptQ7vBfiYmJSqVy8eLFbfar1ers7OzY2FgiqeBRnJ2dDZPf9Xp9Q0NDcXHxiRMn3nrrrTY3suv1+qFDh+J5TETMnTu39R5juVweHx9vtgeXo9yBoijq3r17EyZMGDp0aFFRUZuXPv7448zMTMyS5jKapm1sbNzd3YcPH7569era2toPP/ywzXvwFC3zO3z48E8//dS6efDgQZlMZrazo9zFTqfTff31125ubvv27WvzUkhISEZGxqefforFBvjF1tb2s88+a/OY3IKCgqNHj5KKJELl5eWTJk1q3fzggw/arMvPNpS7qKWnpwcGBr7//vtt9ltZWf3888/Xrl0LDw8nEgy6b968eQMGDDDes379elJhxEav148fP16r1Ro2IyIiWF26/aFQ7iLFMMyqVauioqIKCgravDR//vzy8vJZs2ZhjSC+W758ufHmnj17urwEBXRKfHx86yNWLCwsDh06ZPIVfduFf26LkU6ne+655x68DjNkyJBNmzb5+/sTSQUm12bxd71eX19f3/F1JaHLjKeftrS0dOTvVJtbFuzs7IxHVy+88MLmzZs7lQFDM9HR6XRjx45t0+z29vYHDhw4c+YMmp1TPvroIysjZ8+e7dThCoWizZ7GxkbTpYOOau6ANoe0tLQYv6rT6Tp7UpS76MyePbvNF2ujR48uKip65plnzDZJCzpILpcb/w3Pzc3t1OEPXgoww42RwBEod3HZvXt3m3/cLV269ODBg05OTqQiwWO0ecJGenp6pw5vampqswfXZMQD5S4iWq22zWq9S5cuXbZsGQbsnNXmgVnGz2briDbfloeGhuJLcvNYsGDBox5U9yivvPKK8SdotVrjV7dt29bZDPhJi8jWrVs1Gk3r5sSJE5cuXUowD7RLpVIZP8A2Ozu7zVP3Hm/r1q3Gm1OnTjVZMjC1Nr93uz/kQrmLyGeffdb6v62srDZt2oQxO8dZWFi89tprxnt+/vnnDh57//79L774wnjP9OnTTZYMOA/lLha5ubnGX8ctXrwY19l5oc2jUT744IOOfK3a1NQ0duxY42vuw4YNU6vVps8HXIVyF4uTJ08ab86ePZtUEugUtVr9t7/9rXVTr9dHRUVlZGQ85pD8/PyBAwcmJycb7/zll19YSgjchHIXiz179hhvBgQEWHVDQ0MDqf8QEVq9erW9vX3rZk1NTWRk5LRp086dO1dZWWlYDoxhmKqqqvPnz8+aNUupVKalpRl/wubNm7H0m9jgDlWxaDNyb2lpaWlpIRUGOkUul586darNQjHx8fHx8fEdOfzrr79u87xsEAOM3EVBo9HU19eTTgFd179//8uXL/fo0aNTR8lkspMnT7733nsspQIuQ7mLQmVlJekI0F1RUVGFhYVLly7tyF2mMpls1apVpaWlcXFxZsgGHERjlTgAfmlqarp48eLx48eTkpKysrKKiop0Op2jo2NgYGBERES/fv2GDh0aHh6O+5VEDuUOACBA+N0OACBAKHcAAAFCuQMACBDKHQBAgFDuAAAChHIHABAglDsAgACh3AEABAjlDgAgQCh3AAABQrkDAAgQyh0AQIBQ7gAAAoRyBwAQIJQ7AIAAodwBAAQI5Q4AIEAodwAAAfp/cu7dX8kOX40AAAAASUVORK5CYII=" style="height:50px;"></td>
<td headers="1" class="gt_row gt_left"><span class='gt_from_md'>.03 *<br /><span style="font-size:80%">[0.00, 0.05]</span></span></td>
<td headers="2" class="gt_row gt_left"><span class='gt_from_md'>.42 ***<br /><span style="font-size:80%">[0.40, 0.44]</span></span></td>
<td headers="3" class="gt_row gt_left"><span class='gt_from_md'> <br /> </span></td></tr>
    <tr><td headers="Variable" class="gt_row gt_left"><span class='gt_from_md'><ol start="4">
<li>Healthy eating</li>
</ol>
</span></td>
<td headers="desc" class="gt_row gt_right"><span class='gt_from_md'>4.97 (1.11)</span></td>
<td headers="Distributions" class="gt_row gt_right"><img src="data:image/png;base64,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Ossb6QvCHchf//rXI0eOrF69esaMGcwRHx+fbdu2DRky5He/+11ubq5Jrf34449yuXzEiBFWqBSAfxRFTZgw4ejRo+3t7ea1QNP0F198MXz4cAcHh4iICLPbMQ7hbu+KiorefPPNadOmzZkzR/+4TCbbsmWLg4PDww8/3NnZybI1jUbz008/TZw4EZMgQcQmT57c2tp68uRJM67t6OhYvHjxihUrHBwc/P398/Pz79y5w3mFBOEOa9eulUgkK1eu7LlDqbe39/r162/cuLF582aWrZ08ebK+vj4hIYHrMgEEJDY21t3dff/+/aZeqNPpHnnkkR9//HHVqlUffvjh448/bo3yGAh3u3bjxo3vvvvukUceUSqVvZ4QHx9///33v/feezdu3GDT4N69ez08PDAJEsRNKpVOmDBh//79pu7dsWXLlgMHDrzyyisLFiyw9obvCHe7tmXLFg8Pj8WLFxs557nnnpPL5cuXL+/3levW1tZ9+/YlJCRIpVJOywQQnOnTp9fV1Z06dYr9JefOndu8efPcuXPnzZtnvcK6IdztV25u7v79+xcuXGh8Gzw3N7eVK1dev379448/Nt7gt99+29TUdN9993FaJoAQjRs3zt3d/b///S/L81taWpYsWRIQELBy5UqrFtYN4W6/PvroI0dHxwULFvR75tSpU8ePH79hw4by8vK+zqFp+uOPPw4LC4uOjua0TAAhkkql06ZN++6771paWtic/9ZbbxUVFa1bt27QoEHWro2BcLdTzc3N//73v2fMmOHp6dnvyRRFvfzyyxqN5oUXXqBputdzzp8/f+XKlYULF1p7JBFAIObMmdPc3PzDDz/0e+b169f/9re/LViwYCCnCCPc7dTevXubm5vnz5/P8nw/P7/ly5cfPHhwz549vZ6wdetWhUJxzz33cFcjgKBFR0eHhob2O1yp1WqfffZZpVL5zDPPDExhDIS7nfr8888jIiKioqLYX7Jw4cLRo0f/v//3//Lz8w2+SkxM/OWXXx5//HEnJydOywQQLoqiHnzwwYsXL16/ft3IaTt27EhOTl65cqXxh1ucQ7jbo8zMzCtXrtx3330mDaFQFLVhwwYHB4cHH3xQ/7WLpqamFStWhISEsP89AEAc7rnnHnd39z//+c99nVBQUPDGG29Mnjx5ypQpA1kYQbjbp127dkml0pkzZ5p6oY+Pz6ZNm3Jzc2fOnFlYWEgIaWhoWLRo0e3bt9etW4cZkGBvZDLZwoUL9+3bd/PmzZ7farXap59+2sHB4ZVXXhn4Z1EId7uj1Wp37949ceJEDw8PMy6PjY3dsmVLbm7u0KFDx48fHxoaevLkyfXr12OSDNinxYsXe3h4vPLKKz3nGmzZsuXs2bMvv/xyXy8JWhXC3e6cOnWqoqLCkief48aN27lz58MPP0wImTJlyqeffjpr1izuCgSwJe7u7s8888yJEyc+//xz/ePffPPNH//4x/vvv797Pb4Bht+j7c5XX33l4eFh4XrrSqVy+fLlXJUEYNPuv//+CxcurFy50tnZecmSJTRNf/bZZytXroyLi1u1ahVfVaHnbl+YFQKmTZuG8XEArlAUtXHjxpEjRz799NNhYWHBwcEvvPBCfHz81q1bHR0d+aoKf8Pty88//9zS0oJRFABuyWSybdu2nThx4vLlyw4ODs8++2xCQgK/L/Qh3O3Lnj17/Pz8YmJi+C4EQGwkEsns2bNnz57NdyH/g2EZO6JWq48cOXL33XdjhQAA0UO425Hvv/++q6vLjOntAGBzEO52ZM+ePWFhYeHh4XwXAgBWh3C3F7dv3z59+jQepQLYCYS7vfj6668JIXy9TwEAAwzhbhdomt61a1dsbKyvry/ftQDAQEC424WUlJSsrCzhTNICAGtDuNuFXbt2OTk5TZs2je9CAGCAINzFr7Oz86uvvpo6daqrqyvftQDAAEG4i9/BgwfVavWcOXP4LgQABg7CXfw++eSTgICAuLg4vgsBgIGDcBe5vLy8EydOzJ07F0sOANgVhLvI/eMf/3B0dJw7dy7fhQDAgEK4i1lTU9POnTunT58ul8v5rgUABhTCXcw+++yzxsbGhx56iO9CAGCgIdxFq62tbfv27ePGjYuMjOS7FgAYaAh30fr4448rKyufeuopvgsBAB4g3MWprq5uy5YtEyZMGD58ON+1AAAPEO7i9MYbbzQ1NT3//PN8FwIA/EC4i9CJEyc+/fTThx9+OCQkhO9aAIAf2CBb0Lq6ujIzM8vKygghISEh0dHRDg4Oxi8pKyt77LHHwsLCMNoOYM8Q7gJ18+bN999//4cffmhubu4+6ObmNmfOnEcfffSBBx5wdnbueVVZWdns2bNbWlq2bdvm5OQ0gPUCgLAg3AWnpaVl/fr1//jHP1xdXe++++4xY8b4+/vTNF1WVpaenn769Ol9+/YpFIonnnji0UcfHT9+PNOX12g033zzzauvvtrS0vLuu+9iQAbAzlE0TfNdA/yfzMzMhQsX5uXlLV68+Mknn3RzczM4gabpGzduHDly5OzZsx0dHe7u7jExMc7OzmlpaWq1Ojo6et26dUh2AJtw+PDh7du3V1RU+Pn5cd44eu4CkpiYuHjxYmdn57/+9a8jR47s9RyKouLi4uLi4tra2q5du5aamnr79u22traJEydOmTIlPj4eC4QBAEG4C8e33377xBNPhIWFvfPOO0qlst/zXVxcEhISEhISBqA2ALA5CHcTdHZ2nj17Njk5uaamxtXVNSYmZsaMGQqFwvKWd+3atWzZstjY2C1btshkMssbBAA7h3BnpbGx8f333//nP/+pVqsJITKZrL29XafTSaXSBQsWrFmzJj4+3uzGP//88+eeey4+Pv7tt9/udQ4MAICpEO79O3LkyLJly6qrq6dOnTpnzpyYmBiZTKbRaPLz88+cOXPkyJF9+/YtXrz4gw8+MONJ5t///veXXnpp0qRJmzZtcnR0tEb9AGCH8IaqMTqdbuPGjXPnznV3d//kk0/eeuut+Ph4ZthEKpVGRUWtWLHi66+/Xr58+eHDh6Ojoz/66COtVsuycZqmt2zZ8tJLL02bNu3tt99GsgMAhxDufero6Pj973//zjvvzJ8//+9//3tERESvpw0aNOixxx778ssvR48e/fLLLyckJGRnZ/fbeGdn54oVK9566625c+e++eabUil+hQIALiHce9fW1jZ//vzvvvvuxRdffOWVV/rtVqtUqi1btmzcuDErKys2Nvatt95qaWnp6+Rbt25Nnjz5X//619NPP/2HP/xBIsGfAgBwDLHSCybZjx8/vn79+sWLF7O8iqKoGTNm7Ny5c8aMGVu2bBkyZMj27dvr6ur0z6mqqtqwYcOoUaNyc3PfeeedJUuWYFo6AFgD3lA11NHRsXDhwl9++WXDhg0zZ840r5Hs7Owvv/zy6tWrUql0/PjxERERNE1nZ2dfu3aNpul777332Wefxb6mAHbOqm+oItx/Q6PRPPzww/v371+3bt29995rYWvFxcUnT55MS0urqamRSCQqlWrUqFGzZs0KCAjgpFoAsGlYfmCAaLXap556av/+/atXr7Y82QkhoaGhS5cutbwd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" style="height:50px;"></td>
<td headers="1" class="gt_row gt_left"><span class='gt_from_md'>.17 ***<br /><span style="font-size:80%">[0.15, 0.19]</span></span></td>
<td headers="2" class="gt_row gt_left"><span class='gt_from_md'>-.09 ***<br /><span style="font-size:80%">[-0.11, -0.07]</span></span></td>
<td headers="3" class="gt_row gt_left"><span class='gt_from_md'>-.13 ***<br /><span style="font-size:80%">[-0.15, -0.10]</span></span></td></tr>
  </tbody>
  <tfoot>
    <tr class="gt_sourcenotes">
      <td class="gt_sourcenote" colspan="6"><span class='gt_from_md'><em>M</em> and <em>SD</em> are used to represent mean and standard deviation, respectively.</span></td>
    </tr>
    <tr class="gt_sourcenotes">
      <td class="gt_sourcenote" colspan="6"><span class='gt_from_md'>Values in square brackets indicate the confidence interval for each correlation.</span></td>
    </tr>
    <tr class="gt_sourcenotes">
      <td class="gt_sourcenote" colspan="6"><span class='gt_from_md'>† <em>p</em> &lt; .1, * <em>p</em> &lt; .05, ** <em>p</em> &lt; .01, *** <em>p</em> &lt; .001</span></td>
    </tr>
  </tfoot>
</table>
</div>

## Describe categorical variables and their relation with an outcome

Often, we are also interested in how the means of an outcome variable
differ between different groups. It can be fiddly to get these tables
and the pairwise significance tests done, but this function does it in a
breeze.

``` r
# Start with this in the console - that gets you 80% of the tribbles below.
# get_rename_tribbles(ess_health, gndr, cntry)

var_renames <- tribble(
    ~old,     ~new,     
    "gndr",   "Gender",  
    "cntry",  "Country"
)

level_renames <- tribble(
    ~var,     ~level_old, ~level_new, 
    "gndr",   "1",        "male",       
    "gndr",   "2",        "female",       
    "cntry",  "DE",       "Germany",      
    "cntry",  "FR",       "France",      
    "cntry",  "GB",       "UK"
)

report_cat_vars(ess_health, health, gndr, cntry, var_names = var_renames, 
              level_names = level_renames)
```

<html lang="en">

<head>

<meta charset="utf-8"/>

<style>body{background-color:white;}</style>

</head>

<body>

<div id="rwzrtkemfm"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>\#rwzrtkemfm table { font-family: times, system-ui, ‘Segoe UI’,
Roboto, Helvetica, Arial, sans-serif, ‘Apple Color Emoji’, ‘Segoe UI
Emoji’, ‘Segoe UI Symbol’, ‘Noto Color Emoji’; -webkit-font-smoothing:
antialiased; -moz-osx-font-smoothing: grayscale; }

\#rwzrtkemfm thead, \#rwzrtkemfm tbody, \#rwzrtkemfm tfoot, \#rwzrtkemfm
tr, \#rwzrtkemfm td, \#rwzrtkemfm th { border-style: none; }

\#rwzrtkemfm p { margin: 0; padding: 0; }

\#rwzrtkemfm .gt_table { display: table; border-collapse: collapse;
line-height: normal; margin-left: auto; margin-right: auto; color:
\#333333; font-size: 12px; font-weight: normal; font-style: normal;
background-color: \#FFFFFF; width: auto; border-top-style: none;
border-top-width: 2px; border-top-color: \#FFFFFF; border-right-style:
none; border-right-width: 2px; border-right-color: \#D3D3D3;
border-bottom-style: none; border-bottom-width: 2px;
border-bottom-color: \#A8A8A8; border-left-style: none;
border-left-width: 2px; border-left-color: \#D3D3D3; }

\#rwzrtkemfm .gt_caption { padding-top: 4px; padding-bottom: 4px; }

\#rwzrtkemfm .gt_title { color: \#333333; font-size: 12px; font-weight:
initial; padding-top: 4px; padding-bottom: 4px; padding-left: 5px;
padding-right: 5px; border-bottom-color: \#FFFFFF; border-bottom-width:
0; }

\#rwzrtkemfm .gt_subtitle { color: \#333333; font-size: 12px;
font-weight: initial; padding-top: 3px; padding-bottom: 5px;
padding-left: 5px; padding-right: 5px; border-top-color: \#FFFFFF;
border-top-width: 0; }

\#rwzrtkemfm .gt_heading { background-color: \#FFFFFF; text-align:
center; border-bottom-color: \#FFFFFF; border-left-style: none;
border-left-width: 1px; border-left-color: \#D3D3D3; border-right-style:
none; border-right-width: 1px; border-right-color: \#D3D3D3; }

\#rwzrtkemfm .gt_bottom_border { border-bottom-style: solid;
border-bottom-width: 2px; border-bottom-color: \#000000; }

\#rwzrtkemfm .gt_col_headings { border-top-style: none;
border-top-width: 2px; border-top-color: \#D3D3D3; border-bottom-style:
solid; border-bottom-width: 1px; border-bottom-color: \#000000;
border-left-style: none; border-left-width: 1px; border-left-color:
\#D3D3D3; border-right-style: none; border-right-width: 1px;
border-right-color: \#D3D3D3; }

\#rwzrtkemfm .gt_col_heading { color: \#333333; background-color:
\#FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit;
border-left-style: none; border-left-width: 1px; border-left-color:
\#D3D3D3; border-right-style: none; border-right-width: 1px;
border-right-color: \#D3D3D3; vertical-align: bottom; padding-top: 5px;
padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x:
hidden; }

\#rwzrtkemfm .gt_column_spanner_outer { color: \#333333;
background-color: \#FFFFFF; font-size: 100%; font-weight: normal;
text-transform: inherit; padding-top: 0; padding-bottom: 0;
padding-left: 4px; padding-right: 4px; }

\#rwzrtkemfm .gt_column_spanner_outer:first-child { padding-left: 0; }

\#rwzrtkemfm .gt_column_spanner_outer:last-child { padding-right: 0; }

\#rwzrtkemfm .gt_column_spanner { border-bottom-style: solid;
border-bottom-width: 1px; border-bottom-color: \#000000; vertical-align:
bottom; padding-top: 5px; padding-bottom: 5px; overflow-x: hidden;
display: inline-block; width: 100%; }

\#rwzrtkemfm .gt_spanner_row { border-bottom-style: hidden; }

\#rwzrtkemfm .gt_group_heading { padding-top: 8px; padding-bottom: 8px;
padding-left: 5px; padding-right: 5px; color: \#333333;
background-color: \#FFFFFF; font-size: 100%; font-weight: initial;
text-transform: inherit; border-top-style: none; border-top-width: 2px;
border-top-color: \#D3D3D3; border-bottom-style: none;
border-bottom-width: 2px; border-bottom-color: \#D3D3D3;
border-left-style: none; border-left-width: 1px; border-left-color:
\#D3D3D3; border-right-style: none; border-right-width: 1px;
border-right-color: \#D3D3D3; vertical-align: middle; text-align: left;
}

\#rwzrtkemfm .gt_empty_group_heading { padding: 0.5px; color: \#333333;
background-color: \#FFFFFF; font-size: 100%; font-weight: initial;
border-top-style: none; border-top-width: 2px; border-top-color:
\#D3D3D3; border-bottom-style: none; border-bottom-width: 2px;
border-bottom-color: \#D3D3D3; vertical-align: middle; }

\#rwzrtkemfm .gt_from_md \> :first-child { margin-top: 0; }

\#rwzrtkemfm .gt_from_md \> :last-child { margin-bottom: 0; }

\#rwzrtkemfm .gt_row { padding-top: 8px; padding-bottom: 8px;
padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style:
none; border-top-width: 1px; border-top-color: \#FFFFFF;
border-left-style: none; border-left-width: 1px; border-left-color:
\#D3D3D3; border-right-style: none; border-right-width: 1px;
border-right-color: \#D3D3D3; vertical-align: middle; overflow-x:
hidden; }

\#rwzrtkemfm .gt_stub { color: \#333333; background-color: \#FFFFFF;
font-size: 100%; font-weight: initial; text-transform: inherit;
border-right-style: none; border-right-width: 2px; border-right-color:
\#D3D3D3; padding-left: 5px; padding-right: 5px; }

\#rwzrtkemfm .gt_stub_row_group { color: \#333333; background-color:
\#FFFFFF; font-size: 100%; font-weight: initial; text-transform:
inherit; border-right-style: none; border-right-width: 2px;
border-right-color: \#D3D3D3; padding-left: 5px; padding-right: 5px;
vertical-align: top; }

\#rwzrtkemfm .gt_row_group_first td { border-top-width: 2px; }

\#rwzrtkemfm .gt_row_group_first th { border-top-width: 2px; }

\#rwzrtkemfm .gt_summary_row { color: \#333333; background-color:
\#FFFFFF; text-transform: inherit; padding-top: 8px; padding-bottom:
8px; padding-left: 5px; padding-right: 5px; }

\#rwzrtkemfm .gt_first_summary_row { border-top-style: none;
border-top-color: \#D3D3D3; }

\#rwzrtkemfm .gt_first_summary_row.thick { border-top-width: 2px; }

\#rwzrtkemfm .gt_last_summary_row { padding-top: 8px; padding-bottom:
8px; padding-left: 5px; padding-right: 5px; border-bottom-style: none;
border-bottom-width: 2px; border-bottom-color: \#D3D3D3; }

\#rwzrtkemfm .gt_grand_summary_row { color: \#333333; background-color:
\#FFFFFF; text-transform: inherit; padding-top: 8px; padding-bottom:
8px; padding-left: 5px; padding-right: 5px; }

\#rwzrtkemfm .gt_first_grand_summary_row { padding-top: 8px;
padding-bottom: 8px; padding-left: 5px; padding-right: 5px;
border-top-style: none; border-top-width: 6px; border-top-color:
\#D3D3D3; }

\#rwzrtkemfm .gt_last_grand_summary_row_top { padding-top: 8px;
padding-bottom: 8px; padding-left: 5px; padding-right: 5px;
border-bottom-style: none; border-bottom-width: 6px;
border-bottom-color: \#D3D3D3; }

\#rwzrtkemfm .gt_striped { background-color: rgba(128, 128, 128, 0.05);
}

\#rwzrtkemfm .gt_table_body { border-top-style: solid; border-top-width:
1px; border-top-color: \#000000; border-bottom-style: solid;
border-bottom-width: 1px; border-bottom-color: \#000000; }

\#rwzrtkemfm .gt_footnotes { color: \#333333; background-color:
\#FFFFFF; border-bottom-style: none; border-bottom-width: 2px;
border-bottom-color: \#D3D3D3; border-left-style: none;
border-left-width: 2px; border-left-color: \#D3D3D3; border-right-style:
none; border-right-width: 2px; border-right-color: \#D3D3D3; }

\#rwzrtkemfm .gt_footnote { margin: 0px; font-size: 90%; padding-top:
4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; }

\#rwzrtkemfm .gt_sourcenotes { color: \#333333; background-color:
\#FFFFFF; border-bottom-style: none; border-bottom-width: 2px;
border-bottom-color: \#D3D3D3; border-left-style: none;
border-left-width: 2px; border-left-color: \#D3D3D3; border-right-style:
none; border-right-width: 2px; border-right-color: \#D3D3D3; }

\#rwzrtkemfm .gt_sourcenote { font-size: 90%; padding-top: 4px;
padding-bottom: 4px; padding-left: 5px; padding-right: 5px; }

\#rwzrtkemfm .gt_left { text-align: left; }

\#rwzrtkemfm .gt_center { text-align: center; }

\#rwzrtkemfm .gt_right { text-align: right; font-variant-numeric:
tabular-nums; }

\#rwzrtkemfm .gt_font_normal { font-weight: normal; }

\#rwzrtkemfm .gt_font_bold { font-weight: bold; }

\#rwzrtkemfm .gt_font_italic { font-style: italic; }

\#rwzrtkemfm .gt_super { font-size: 65%; }

\#rwzrtkemfm .gt_footnote_marks { font-size: 75%; vertical-align: 0.4em;
position: initial; }

\#rwzrtkemfm .gt_asterisk { font-size: 100%; vertical-align: 0; }

\#rwzrtkemfm .gt_indent_1 { text-indent: 5px; }

\#rwzrtkemfm .gt_indent_2 { text-indent: 10px; }

\#rwzrtkemfm .gt_indent_3 { text-indent: 15px; }

\#rwzrtkemfm .gt_indent_4 { text-indent: 20px; }

\#rwzrtkemfm .gt_indent_5 { text-indent: 25px; }

\#rwzrtkemfm .katex-display { display: inline-flex !important;
margin-bottom: 0.75em !important; }

\#rwzrtkemfm div.Reactable \> div.rt-table \> div.rt-thead \>
div.rt-tr.rt-tr-group-header \> div.rt-th-group:after { height: 0px
!important; } </style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">

<thead>

<tr class="gt_col_headings">

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="a::stub">

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="N">

<span class="gt_from_md">N</span>
</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Share">

<span class="gt_from_md">Share</span>
</th>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="a*M-(SD)*">

<span class="gt_from_md"><em>M (SD)</em></span>
</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr class="gt_group_heading_row">

<th colspan="4" class="gt_group_heading" style="font-weight: bold; background-color: #E0E0E0;" scope="colgroup" id="Gender">

Gender
</th>

</tr>

<tr class="gt_row_group_first">

<th id="stub_1_1" scope="row" class="gt_row gt_left gt_stub">

<span class="gt_from_md">male</span>
</th>

<td headers="Gender stub_1_1 N" class="gt_row gt_right">

<span class="gt_from_md">3482</span>
</td>

<td headers="Gender stub_1_1 Share" class="gt_row gt_right">

48.2%
</td>

<td headers="Gender stub_1_1 *M (SD)*" class="gt_row gt_left">

<span class="gt_from_md">2.23 (0.90) <sup>b</sup></span>
</td>

</tr>

<tr>

<th id="stub_1_2" scope="row" class="gt_row gt_left gt_stub">

<span class="gt_from_md">female</span>
</th>

<td headers="Gender stub_1_2 N" class="gt_row gt_right">

<span class="gt_from_md">3744</span>
</td>

<td headers="Gender stub_1_2 Share" class="gt_row gt_right">

51.8%
</td>

<td headers="Gender stub_1_2 *M (SD)*" class="gt_row gt_left">

<span class="gt_from_md">2.30 (0.93) <sup>a</sup></span>
</td>

</tr>

<tr class="gt_group_heading_row">

<th colspan="4" class="gt_group_heading" style="font-weight: bold; background-color: #E0E0E0;" scope="colgroup" id="Country">

Country
</th>

</tr>

<tr class="gt_row_group_first">

<th id="stub_1_3" scope="row" class="gt_row gt_left gt_stub">

<span class="gt_from_md">Germany</span>
</th>

<td headers="Country stub_1_3 N" class="gt_row gt_right">

<span class="gt_from_md">3045</span>
</td>

<td headers="Country stub_1_3 Share" class="gt_row gt_right">

42.1%
</td>

<td headers="Country stub_1_3 *M (SD)*" class="gt_row gt_left">

<span class="gt_from_md">2.34 (0.88) <sup>a</sup></span>
</td>

</tr>

<tr>

<th id="stub_1_4" scope="row" class="gt_row gt_left gt_stub">

<span class="gt_from_md">France</span>
</th>

<td headers="Country stub_1_4 N" class="gt_row gt_right">

<span class="gt_from_md">1917</span>
</td>

<td headers="Country stub_1_4 Share" class="gt_row gt_right">

26.5%
</td>

<td headers="Country stub_1_4 *M (SD)*" class="gt_row gt_left">

<span class="gt_from_md">2.29 (0.89) <sup>a</sup></span>
</td>

</tr>

<tr>

<th id="stub_1_5" scope="row" class="gt_row gt_left gt_stub">

<span class="gt_from_md">UK</span>
</th>

<td headers="Country stub_1_5 N" class="gt_row gt_right">

<span class="gt_from_md">2264</span>
</td>

<td headers="Country stub_1_5 Share" class="gt_row gt_right">

31.3%
</td>

<td headers="Country stub_1_5 *M (SD)*" class="gt_row gt_left">

<span class="gt_from_md">2.14 (0.97) <sup>b</sup></span>
</td>

</tr>

</tbody>

<tfoot>

<tr class="gt_sourcenotes">

<td class="gt_sourcenote" colspan="4">

<span class="gt_from_md"><em>M</em> and <em>SD</em> are used to
represent mean and standard deviation for health for that group,
respectively.<br></span>
</td>

</tr>

<tr class="gt_sourcenotes">

<td class="gt_sourcenote" colspan="4">

<span class="gt_from_md">Within each variable, the means of groups with
different superscripts differ with <em>p</em> \< .05 <br>
(<em>p</em>-values were adjusted using the Holm-method.)</span>
</td>

</tr>

</tfoot>

</table>

</div>

</body>

## Report regression models with standardized coefficients

In psychology, it is often expected that regression models are reported
with both unstandardised (B) and standardized (beta) coefficients. This
can be fiddly as separate tables will contain too much redundant
information. The functions below easily run a model with standardised
variables and create a publication-ready table.

``` r
ess_health$gndr <- factor(ess_health$gndr)

#Standard lm model
mod1 <- lm(depression ~ agea + gndr + health + cntry, ess_health)

#Model with standardised coefficients

mod2 <- lm_std(depression ~ agea + gndr + health + cntry, ess_health)

report_lm_with_std(mod1, mod2)
#> Warning: The `tidy()` method for objects of class `lm_std` is not maintained by the broom team, and is only supported through the `lm` tidier method. Please be cautious in interpreting and reporting broom output.
#> 
#> This warning is displayed once per session.
```

<html lang="en">

<head>

<meta charset="utf-8"/>

<style>body{background-color:white;}</style>

</head>

<body>

<div id="ocdxhaxnys"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>\#ocdxhaxnys table { font-family: times, system-ui, ‘Segoe UI’,
Roboto, Helvetica, Arial, sans-serif, ‘Apple Color Emoji’, ‘Segoe UI
Emoji’, ‘Segoe UI Symbol’, ‘Noto Color Emoji’; -webkit-font-smoothing:
antialiased; -moz-osx-font-smoothing: grayscale; }

\#ocdxhaxnys thead, \#ocdxhaxnys tbody, \#ocdxhaxnys tfoot, \#ocdxhaxnys
tr, \#ocdxhaxnys td, \#ocdxhaxnys th { border-style: none; }

\#ocdxhaxnys p { margin: 0; padding: 0; }

\#ocdxhaxnys .gt_table { display: table; border-collapse: collapse;
line-height: normal; margin-left: auto; margin-right: auto; color:
\#333333; font-size: 12px; font-weight: normal; font-style: normal;
background-color: \#FFFFFF; width: auto; border-top-style: none;
border-top-width: 2px; border-top-color: \#FFFFFF; border-right-style:
none; border-right-width: 2px; border-right-color: \#D3D3D3;
border-bottom-style: none; border-bottom-width: 2px;
border-bottom-color: \#A8A8A8; border-left-style: none;
border-left-width: 2px; border-left-color: \#D3D3D3; }

\#ocdxhaxnys .gt_caption { padding-top: 4px; padding-bottom: 4px; }

\#ocdxhaxnys .gt_title { color: \#333333; font-size: 12px; font-weight:
initial; padding-top: 4px; padding-bottom: 4px; padding-left: 5px;
padding-right: 5px; border-bottom-color: \#FFFFFF; border-bottom-width:
0; }

\#ocdxhaxnys .gt_subtitle { color: \#333333; font-size: 12px;
font-weight: initial; padding-top: 3px; padding-bottom: 5px;
padding-left: 5px; padding-right: 5px; border-top-color: \#FFFFFF;
border-top-width: 0; }

\#ocdxhaxnys .gt_heading { background-color: \#FFFFFF; text-align:
center; border-bottom-color: \#FFFFFF; border-left-style: none;
border-left-width: 1px; border-left-color: \#D3D3D3; border-right-style:
none; border-right-width: 1px; border-right-color: \#D3D3D3; }

\#ocdxhaxnys .gt_bottom_border { border-bottom-style: solid;
border-bottom-width: 2px; border-bottom-color: \#000000; }

\#ocdxhaxnys .gt_col_headings { border-top-style: none;
border-top-width: 2px; border-top-color: \#D3D3D3; border-bottom-style:
solid; border-bottom-width: 1px; border-bottom-color: \#000000;
border-left-style: none; border-left-width: 1px; border-left-color:
\#D3D3D3; border-right-style: none; border-right-width: 1px;
border-right-color: \#D3D3D3; }

\#ocdxhaxnys .gt_col_heading { color: \#333333; background-color:
\#FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit;
border-left-style: none; border-left-width: 1px; border-left-color:
\#D3D3D3; border-right-style: none; border-right-width: 1px;
border-right-color: \#D3D3D3; vertical-align: bottom; padding-top: 5px;
padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x:
hidden; }

\#ocdxhaxnys .gt_column_spanner_outer { color: \#333333;
background-color: \#FFFFFF; font-size: 100%; font-weight: normal;
text-transform: inherit; padding-top: 0; padding-bottom: 0;
padding-left: 4px; padding-right: 4px; }

\#ocdxhaxnys .gt_column_spanner_outer:first-child { padding-left: 0; }

\#ocdxhaxnys .gt_column_spanner_outer:last-child { padding-right: 0; }

\#ocdxhaxnys .gt_column_spanner { border-bottom-style: solid;
border-bottom-width: 1px; border-bottom-color: \#000000; vertical-align:
bottom; padding-top: 5px; padding-bottom: 5px; overflow-x: hidden;
display: inline-block; width: 100%; }

\#ocdxhaxnys .gt_spanner_row { border-bottom-style: hidden; }

\#ocdxhaxnys .gt_group_heading { padding-top: 8px; padding-bottom: 8px;
padding-left: 5px; padding-right: 5px; color: \#333333;
background-color: \#FFFFFF; font-size: 100%; font-weight: initial;
text-transform: inherit; border-top-style: none; border-top-width: 2px;
border-top-color: \#D3D3D3; border-bottom-style: none;
border-bottom-width: 2px; border-bottom-color: \#D3D3D3;
border-left-style: none; border-left-width: 1px; border-left-color:
\#D3D3D3; border-right-style: none; border-right-width: 1px;
border-right-color: \#D3D3D3; vertical-align: middle; text-align: left;
}

\#ocdxhaxnys .gt_empty_group_heading { padding: 0.5px; color: \#333333;
background-color: \#FFFFFF; font-size: 100%; font-weight: initial;
border-top-style: none; border-top-width: 2px; border-top-color:
\#D3D3D3; border-bottom-style: none; border-bottom-width: 2px;
border-bottom-color: \#D3D3D3; vertical-align: middle; }

\#ocdxhaxnys .gt_from_md \> :first-child { margin-top: 0; }

\#ocdxhaxnys .gt_from_md \> :last-child { margin-bottom: 0; }

\#ocdxhaxnys .gt_row { padding-top: 8px; padding-bottom: 8px;
padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style:
none; border-top-width: 1px; border-top-color: \#FFFFFF;
border-left-style: none; border-left-width: 1px; border-left-color:
\#D3D3D3; border-right-style: none; border-right-width: 1px;
border-right-color: \#D3D3D3; vertical-align: middle; overflow-x:
hidden; }

\#ocdxhaxnys .gt_stub { color: \#333333; background-color: \#FFFFFF;
font-size: 100%; font-weight: initial; text-transform: inherit;
border-right-style: none; border-right-width: 2px; border-right-color:
\#D3D3D3; padding-left: 5px; padding-right: 5px; }

\#ocdxhaxnys .gt_stub_row_group { color: \#333333; background-color:
\#FFFFFF; font-size: 100%; font-weight: initial; text-transform:
inherit; border-right-style: none; border-right-width: 2px;
border-right-color: \#D3D3D3; padding-left: 5px; padding-right: 5px;
vertical-align: top; }

\#ocdxhaxnys .gt_row_group_first td { border-top-width: 2px; }

\#ocdxhaxnys .gt_row_group_first th { border-top-width: 2px; }

\#ocdxhaxnys .gt_summary_row { color: \#333333; background-color:
\#FFFFFF; text-transform: inherit; padding-top: 8px; padding-bottom:
8px; padding-left: 5px; padding-right: 5px; }

\#ocdxhaxnys .gt_first_summary_row { border-top-style: none;
border-top-color: \#D3D3D3; }

\#ocdxhaxnys .gt_first_summary_row.thick { border-top-width: 2px; }

\#ocdxhaxnys .gt_last_summary_row { padding-top: 8px; padding-bottom:
8px; padding-left: 5px; padding-right: 5px; border-bottom-style: none;
border-bottom-width: 2px; border-bottom-color: \#D3D3D3; }

\#ocdxhaxnys .gt_grand_summary_row { color: \#333333; background-color:
\#FFFFFF; text-transform: inherit; padding-top: 8px; padding-bottom:
8px; padding-left: 5px; padding-right: 5px; }

\#ocdxhaxnys .gt_first_grand_summary_row { padding-top: 8px;
padding-bottom: 8px; padding-left: 5px; padding-right: 5px;
border-top-style: none; border-top-width: 6px; border-top-color:
\#D3D3D3; }

\#ocdxhaxnys .gt_last_grand_summary_row_top { padding-top: 8px;
padding-bottom: 8px; padding-left: 5px; padding-right: 5px;
border-bottom-style: none; border-bottom-width: 6px;
border-bottom-color: \#D3D3D3; }

\#ocdxhaxnys .gt_striped { background-color: rgba(128, 128, 128, 0.05);
}

\#ocdxhaxnys .gt_table_body { border-top-style: solid; border-top-width:
1px; border-top-color: \#000000; border-bottom-style: solid;
border-bottom-width: 1px; border-bottom-color: \#000000; }

\#ocdxhaxnys .gt_footnotes { color: \#333333; background-color:
\#FFFFFF; border-bottom-style: none; border-bottom-width: 2px;
border-bottom-color: \#D3D3D3; border-left-style: none;
border-left-width: 2px; border-left-color: \#D3D3D3; border-right-style:
none; border-right-width: 2px; border-right-color: \#D3D3D3; }

\#ocdxhaxnys .gt_footnote { margin: 0px; font-size: 90%; padding-top:
4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; }

\#ocdxhaxnys .gt_sourcenotes { color: \#333333; background-color:
\#FFFFFF; border-bottom-style: none; border-bottom-width: 2px;
border-bottom-color: \#D3D3D3; border-left-style: none;
border-left-width: 2px; border-left-color: \#D3D3D3; border-right-style:
none; border-right-width: 2px; border-right-color: \#D3D3D3; }

\#ocdxhaxnys .gt_sourcenote { font-size: 90%; padding-top: 4px;
padding-bottom: 4px; padding-left: 5px; padding-right: 5px; }

\#ocdxhaxnys .gt_left { text-align: left; }

\#ocdxhaxnys .gt_center { text-align: center; }

\#ocdxhaxnys .gt_right { text-align: right; font-variant-numeric:
tabular-nums; }

\#ocdxhaxnys .gt_font_normal { font-weight: normal; }

\#ocdxhaxnys .gt_font_bold { font-weight: bold; }

\#ocdxhaxnys .gt_font_italic { font-style: italic; }

\#ocdxhaxnys .gt_super { font-size: 65%; }

\#ocdxhaxnys .gt_footnote_marks { font-size: 75%; vertical-align: 0.4em;
position: initial; }

\#ocdxhaxnys .gt_asterisk { font-size: 100%; vertical-align: 0; }

\#ocdxhaxnys .gt_indent_1 { text-indent: 5px; }

\#ocdxhaxnys .gt_indent_2 { text-indent: 10px; }

\#ocdxhaxnys .gt_indent_3 { text-indent: 15px; }

\#ocdxhaxnys .gt_indent_4 { text-indent: 20px; }

\#ocdxhaxnys .gt_indent_5 { text-indent: 25px; }

\#ocdxhaxnys .katex-display { display: inline-flex !important;
margin-bottom: 0.75em !important; }

\#ocdxhaxnys div.Reactable \> div.rt-table \> div.rt-thead \>
div.rt-tr.rt-tr-group-header \> div.rt-th-group:after { height: 0px
!important; } </style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">

<thead>

<tr class="gt_col_headings">

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="a-">

<span class="gt_from_md"></span>
</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Model1">

<span class='gt_from_md'><em>
<center>

B (SE)
</center>

</em></span>
</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Model2">

<span class='gt_from_md'><em>
<center>

β \[95% CI\]
</center>

</em></span>
</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td headers=" " class="gt_row gt_left">

<span class="gt_from_md">(Intercept)</span>
</td>

<td headers="Model1" class="gt_row gt_right">

<span class='gt_from_md'>1.20 (0.02)\*\*\*</span>
</td>

<td headers="Model2" class="gt_row gt_right">

<span class="gt_from_md">-0.14 \[-0.18, -0.10\]</span>
</td>

</tr>

<tr>

<td headers=" " class="gt_row gt_left">

<span class="gt_from_md">agea</span>
</td>

<td headers="Model1" class="gt_row gt_right">

<span class='gt_from_md'>-0.00 (0.00)\*\*\*</span>
</td>

<td headers="Model2" class="gt_row gt_right">

<span class="gt_from_md">-0.10 \[-0.12, -0.08\]</span>
</td>

</tr>

<tr>

<td headers=" " class="gt_row gt_left">

<span class="gt_from_md">gndr2</span>
</td>

<td headers="Model1" class="gt_row gt_right">

<span class='gt_from_md'>0.12 (0.01)\*\*\*</span>
</td>

<td headers="Model2" class="gt_row gt_right">

<span class="gt_from_md">0.24 \[0.20, 0.28\]</span>
</td>

</tr>

<tr>

<td headers=" " class="gt_row gt_left">

<span class="gt_from_md">health</span>
</td>

<td headers="Model1" class="gt_row gt_right">

<span class='gt_from_md'>0.23 (0.01)\*\*\*</span>
</td>

<td headers="Model2" class="gt_row gt_right">

<span class="gt_from_md">0.44 \[0.42, 0.47\]</span>
</td>

</tr>

<tr>

<td headers=" " class="gt_row gt_left">

<span class="gt_from_md">cntryFR</span>
</td>

<td headers="Model1" class="gt_row gt_right">

<span class="gt_from_md">-0.01 (0.01)   </span>
</td>

<td headers="Model2" class="gt_row gt_right">

<span class="gt_from_md">-0.02 \[-0.08, 0.03\]</span>
</td>

</tr>

<tr>

<td headers=" " class="gt_row gt_left">

<span class="gt_from_md">cntryGB</span>
</td>

<td headers="Model1" class="gt_row gt_right">

<span class='gt_from_md'>0.04 (0.01)\*\* </span>
</td>

<td headers="Model2" class="gt_row gt_right">

<span class="gt_from_md">0.08 \[0.03, 0.13\]</span>
</td>

</tr>

<tr style="border-top-style: solid; border-top-width: 2px;">

<td class="gt_row gt_left" rowspan="1" colspan="1">

<em>N
</td>

<td class="gt_row gt_center" rowspan="1" colspan="2">

7171
</td>

</tr>

<tr>

<td class="gt_row gt_left" rowspan="1" colspan="1">

<em>R<sup>2</sup>
</td>

<td class="gt_row gt_center" rowspan="1" colspan="2">

.20
</td>

</tr>

<tr>

<td class="gt_row gt_left" rowspan="1" colspan="1">

<em>F</em>-tests
</td>

<td class="gt_row gt_center" rowspan="1" colspan="2">

<p>

<em>F</em>(5, 7165) = 358.25, <em>p</em> \< .001
</p>

</td>

</tr>

</tbody>

<tfoot>

<tr class="gt_sourcenotes">

<td class="gt_sourcenote" colspan="3">

<span class="gt_from_md">† <em>p</em> \< .1, \* <em>p</em> \< .05, \*\*
<em>p</em> \< .01, \*\*\* <em>p</em> \< .001</span>
</td>

</tr>

</tfoot>

</table>

</div>

</body>

``` r

#Often the coefficients should be renamed - get_coef_rename_tribble(mod1) 
#is the starting point. In that, markdown formatting can be used.

coef_names <- tribble(
  ~old,           ~new,           
   "(Intercept)",  "*(Intercept)*", 
   "agea",         "Age",        
   "gndr2",        "Gender *(female)*",       
   "health",       "Poor health",      
   "cntryFR",      "France *(vs DE)*",     
   "cntryGB",      "UK *(vs DE)*"
)

report_lm_with_std(mod1, mod2, coef_renames = coef_names)
```

<html lang="en">

<head>

<meta charset="utf-8"/>

<style>body{background-color:white;}</style>

</head>

<body>

<div id="ikedvyvnib"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>\#ikedvyvnib table { font-family: times, system-ui, ‘Segoe UI’,
Roboto, Helvetica, Arial, sans-serif, ‘Apple Color Emoji’, ‘Segoe UI
Emoji’, ‘Segoe UI Symbol’, ‘Noto Color Emoji’; -webkit-font-smoothing:
antialiased; -moz-osx-font-smoothing: grayscale; }

\#ikedvyvnib thead, \#ikedvyvnib tbody, \#ikedvyvnib tfoot, \#ikedvyvnib
tr, \#ikedvyvnib td, \#ikedvyvnib th { border-style: none; }

\#ikedvyvnib p { margin: 0; padding: 0; }

\#ikedvyvnib .gt_table { display: table; border-collapse: collapse;
line-height: normal; margin-left: auto; margin-right: auto; color:
\#333333; font-size: 12px; font-weight: normal; font-style: normal;
background-color: \#FFFFFF; width: auto; border-top-style: none;
border-top-width: 2px; border-top-color: \#FFFFFF; border-right-style:
none; border-right-width: 2px; border-right-color: \#D3D3D3;
border-bottom-style: none; border-bottom-width: 2px;
border-bottom-color: \#A8A8A8; border-left-style: none;
border-left-width: 2px; border-left-color: \#D3D3D3; }

\#ikedvyvnib .gt_caption { padding-top: 4px; padding-bottom: 4px; }

\#ikedvyvnib .gt_title { color: \#333333; font-size: 12px; font-weight:
initial; padding-top: 4px; padding-bottom: 4px; padding-left: 5px;
padding-right: 5px; border-bottom-color: \#FFFFFF; border-bottom-width:
0; }

\#ikedvyvnib .gt_subtitle { color: \#333333; font-size: 12px;
font-weight: initial; padding-top: 3px; padding-bottom: 5px;
padding-left: 5px; padding-right: 5px; border-top-color: \#FFFFFF;
border-top-width: 0; }

\#ikedvyvnib .gt_heading { background-color: \#FFFFFF; text-align:
center; border-bottom-color: \#FFFFFF; border-left-style: none;
border-left-width: 1px; border-left-color: \#D3D3D3; border-right-style:
none; border-right-width: 1px; border-right-color: \#D3D3D3; }

\#ikedvyvnib .gt_bottom_border { border-bottom-style: solid;
border-bottom-width: 2px; border-bottom-color: \#000000; }

\#ikedvyvnib .gt_col_headings { border-top-style: none;
border-top-width: 2px; border-top-color: \#D3D3D3; border-bottom-style:
solid; border-bottom-width: 1px; border-bottom-color: \#000000;
border-left-style: none; border-left-width: 1px; border-left-color:
\#D3D3D3; border-right-style: none; border-right-width: 1px;
border-right-color: \#D3D3D3; }

\#ikedvyvnib .gt_col_heading { color: \#333333; background-color:
\#FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit;
border-left-style: none; border-left-width: 1px; border-left-color:
\#D3D3D3; border-right-style: none; border-right-width: 1px;
border-right-color: \#D3D3D3; vertical-align: bottom; padding-top: 5px;
padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x:
hidden; }

\#ikedvyvnib .gt_column_spanner_outer { color: \#333333;
background-color: \#FFFFFF; font-size: 100%; font-weight: normal;
text-transform: inherit; padding-top: 0; padding-bottom: 0;
padding-left: 4px; padding-right: 4px; }

\#ikedvyvnib .gt_column_spanner_outer:first-child { padding-left: 0; }

\#ikedvyvnib .gt_column_spanner_outer:last-child { padding-right: 0; }

\#ikedvyvnib .gt_column_spanner { border-bottom-style: solid;
border-bottom-width: 1px; border-bottom-color: \#000000; vertical-align:
bottom; padding-top: 5px; padding-bottom: 5px; overflow-x: hidden;
display: inline-block; width: 100%; }

\#ikedvyvnib .gt_spanner_row { border-bottom-style: hidden; }

\#ikedvyvnib .gt_group_heading { padding-top: 8px; padding-bottom: 8px;
padding-left: 5px; padding-right: 5px; color: \#333333;
background-color: \#FFFFFF; font-size: 100%; font-weight: initial;
text-transform: inherit; border-top-style: none; border-top-width: 2px;
border-top-color: \#D3D3D3; border-bottom-style: none;
border-bottom-width: 2px; border-bottom-color: \#D3D3D3;
border-left-style: none; border-left-width: 1px; border-left-color:
\#D3D3D3; border-right-style: none; border-right-width: 1px;
border-right-color: \#D3D3D3; vertical-align: middle; text-align: left;
}

\#ikedvyvnib .gt_empty_group_heading { padding: 0.5px; color: \#333333;
background-color: \#FFFFFF; font-size: 100%; font-weight: initial;
border-top-style: none; border-top-width: 2px; border-top-color:
\#D3D3D3; border-bottom-style: none; border-bottom-width: 2px;
border-bottom-color: \#D3D3D3; vertical-align: middle; }

\#ikedvyvnib .gt_from_md \> :first-child { margin-top: 0; }

\#ikedvyvnib .gt_from_md \> :last-child { margin-bottom: 0; }

\#ikedvyvnib .gt_row { padding-top: 8px; padding-bottom: 8px;
padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style:
none; border-top-width: 1px; border-top-color: \#FFFFFF;
border-left-style: none; border-left-width: 1px; border-left-color:
\#D3D3D3; border-right-style: none; border-right-width: 1px;
border-right-color: \#D3D3D3; vertical-align: middle; overflow-x:
hidden; }

\#ikedvyvnib .gt_stub { color: \#333333; background-color: \#FFFFFF;
font-size: 100%; font-weight: initial; text-transform: inherit;
border-right-style: none; border-right-width: 2px; border-right-color:
\#D3D3D3; padding-left: 5px; padding-right: 5px; }

\#ikedvyvnib .gt_stub_row_group { color: \#333333; background-color:
\#FFFFFF; font-size: 100%; font-weight: initial; text-transform:
inherit; border-right-style: none; border-right-width: 2px;
border-right-color: \#D3D3D3; padding-left: 5px; padding-right: 5px;
vertical-align: top; }

\#ikedvyvnib .gt_row_group_first td { border-top-width: 2px; }

\#ikedvyvnib .gt_row_group_first th { border-top-width: 2px; }

\#ikedvyvnib .gt_summary_row { color: \#333333; background-color:
\#FFFFFF; text-transform: inherit; padding-top: 8px; padding-bottom:
8px; padding-left: 5px; padding-right: 5px; }

\#ikedvyvnib .gt_first_summary_row { border-top-style: none;
border-top-color: \#D3D3D3; }

\#ikedvyvnib .gt_first_summary_row.thick { border-top-width: 2px; }

\#ikedvyvnib .gt_last_summary_row { padding-top: 8px; padding-bottom:
8px; padding-left: 5px; padding-right: 5px; border-bottom-style: none;
border-bottom-width: 2px; border-bottom-color: \#D3D3D3; }

\#ikedvyvnib .gt_grand_summary_row { color: \#333333; background-color:
\#FFFFFF; text-transform: inherit; padding-top: 8px; padding-bottom:
8px; padding-left: 5px; padding-right: 5px; }

\#ikedvyvnib .gt_first_grand_summary_row { padding-top: 8px;
padding-bottom: 8px; padding-left: 5px; padding-right: 5px;
border-top-style: none; border-top-width: 6px; border-top-color:
\#D3D3D3; }

\#ikedvyvnib .gt_last_grand_summary_row_top { padding-top: 8px;
padding-bottom: 8px; padding-left: 5px; padding-right: 5px;
border-bottom-style: none; border-bottom-width: 6px;
border-bottom-color: \#D3D3D3; }

\#ikedvyvnib .gt_striped { background-color: rgba(128, 128, 128, 0.05);
}

\#ikedvyvnib .gt_table_body { border-top-style: solid; border-top-width:
1px; border-top-color: \#000000; border-bottom-style: solid;
border-bottom-width: 1px; border-bottom-color: \#000000; }

\#ikedvyvnib .gt_footnotes { color: \#333333; background-color:
\#FFFFFF; border-bottom-style: none; border-bottom-width: 2px;
border-bottom-color: \#D3D3D3; border-left-style: none;
border-left-width: 2px; border-left-color: \#D3D3D3; border-right-style:
none; border-right-width: 2px; border-right-color: \#D3D3D3; }

\#ikedvyvnib .gt_footnote { margin: 0px; font-size: 90%; padding-top:
4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; }

\#ikedvyvnib .gt_sourcenotes { color: \#333333; background-color:
\#FFFFFF; border-bottom-style: none; border-bottom-width: 2px;
border-bottom-color: \#D3D3D3; border-left-style: none;
border-left-width: 2px; border-left-color: \#D3D3D3; border-right-style:
none; border-right-width: 2px; border-right-color: \#D3D3D3; }

\#ikedvyvnib .gt_sourcenote { font-size: 90%; padding-top: 4px;
padding-bottom: 4px; padding-left: 5px; padding-right: 5px; }

\#ikedvyvnib .gt_left { text-align: left; }

\#ikedvyvnib .gt_center { text-align: center; }

\#ikedvyvnib .gt_right { text-align: right; font-variant-numeric:
tabular-nums; }

\#ikedvyvnib .gt_font_normal { font-weight: normal; }

\#ikedvyvnib .gt_font_bold { font-weight: bold; }

\#ikedvyvnib .gt_font_italic { font-style: italic; }

\#ikedvyvnib .gt_super { font-size: 65%; }

\#ikedvyvnib .gt_footnote_marks { font-size: 75%; vertical-align: 0.4em;
position: initial; }

\#ikedvyvnib .gt_asterisk { font-size: 100%; vertical-align: 0; }

\#ikedvyvnib .gt_indent_1 { text-indent: 5px; }

\#ikedvyvnib .gt_indent_2 { text-indent: 10px; }

\#ikedvyvnib .gt_indent_3 { text-indent: 15px; }

\#ikedvyvnib .gt_indent_4 { text-indent: 20px; }

\#ikedvyvnib .gt_indent_5 { text-indent: 25px; }

\#ikedvyvnib .katex-display { display: inline-flex !important;
margin-bottom: 0.75em !important; }

\#ikedvyvnib div.Reactable \> div.rt-table \> div.rt-thead \>
div.rt-tr.rt-tr-group-header \> div.rt-th-group:after { height: 0px
!important; } </style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">

<thead>

<tr class="gt_col_headings">

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="a-">

<span class="gt_from_md"></span>
</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Model1">

<span class='gt_from_md'><em>
<center>

B (SE)
</center>

</em></span>
</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Model2">

<span class='gt_from_md'><em>
<center>

β \[95% CI\]
</center>

</em></span>
</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td headers=" " class="gt_row gt_left">

<span class="gt_from_md"><em>(Intercept)</em></span>
</td>

<td headers="Model1" class="gt_row gt_right">

<span class='gt_from_md'>1.20 (0.02)\*\*\*</span>
</td>

<td headers="Model2" class="gt_row gt_right">

<span class="gt_from_md">-0.14 \[-0.18, -0.10\]</span>
</td>

</tr>

<tr>

<td headers=" " class="gt_row gt_left">

<span class="gt_from_md">Age</span>
</td>

<td headers="Model1" class="gt_row gt_right">

<span class='gt_from_md'>-0.00 (0.00)\*\*\*</span>
</td>

<td headers="Model2" class="gt_row gt_right">

<span class="gt_from_md">-0.10 \[-0.12, -0.08\]</span>
</td>

</tr>

<tr>

<td headers=" " class="gt_row gt_left">

<span class="gt_from_md">Gender <em>(female)</em></span>
</td>

<td headers="Model1" class="gt_row gt_right">

<span class='gt_from_md'>0.12 (0.01)\*\*\*</span>
</td>

<td headers="Model2" class="gt_row gt_right">

<span class="gt_from_md">0.24 \[0.20, 0.28\]</span>
</td>

</tr>

<tr>

<td headers=" " class="gt_row gt_left">

<span class="gt_from_md">Poor health</span>
</td>

<td headers="Model1" class="gt_row gt_right">

<span class='gt_from_md'>0.23 (0.01)\*\*\*</span>
</td>

<td headers="Model2" class="gt_row gt_right">

<span class="gt_from_md">0.44 \[0.42, 0.47\]</span>
</td>

</tr>

<tr>

<td headers=" " class="gt_row gt_left">

<span class="gt_from_md">France <em>(vs DE)</em></span>
</td>

<td headers="Model1" class="gt_row gt_right">

<span class="gt_from_md">-0.01 (0.01)   </span>
</td>

<td headers="Model2" class="gt_row gt_right">

<span class="gt_from_md">-0.02 \[-0.08, 0.03\]</span>
</td>

</tr>

<tr>

<td headers=" " class="gt_row gt_left">

<span class="gt_from_md">UK <em>(vs DE)</em></span>
</td>

<td headers="Model1" class="gt_row gt_right">

<span class='gt_from_md'>0.04 (0.01)\*\* </span>
</td>

<td headers="Model2" class="gt_row gt_right">

<span class="gt_from_md">0.08 \[0.03, 0.13\]</span>
</td>

</tr>

<tr style="border-top-style: solid; border-top-width: 2px;">

<td class="gt_row gt_left" rowspan="1" colspan="1">

<em>N
</td>

<td class="gt_row gt_center" rowspan="1" colspan="2">

7171
</td>

</tr>

<tr>

<td class="gt_row gt_left" rowspan="1" colspan="1">

<em>R<sup>2</sup>
</td>

<td class="gt_row gt_center" rowspan="1" colspan="2">

.20
</td>

</tr>

<tr>

<td class="gt_row gt_left" rowspan="1" colspan="1">

<em>F</em>-tests
</td>

<td class="gt_row gt_center" rowspan="1" colspan="2">

<p>

<em>F</em>(5, 7165) = 358.25, <em>p</em> \< .001
</p>

</td>

</tr>

</tbody>

<tfoot>

<tr class="gt_sourcenotes">

<td class="gt_sourcenote" colspan="3">

<span class="gt_from_md">† <em>p</em> \< .1, \* <em>p</em> \< .05, \*\*
<em>p</em> \< .01, \*\*\* <em>p</em> \< .001</span>
</td>

</tr>

</tfoot>

</table>

</div>

</body>

``` r

#You can also easily display multiple nested models side-by-side and get the
#F-change significance test. For that, all models need to be fit on the same
#dataset, so that I will drop all missing data.

mod1 <- lm(depression ~ agea + gndr + health + cntry, tidyr::drop_na(ess_health))
mod2 <- lm_std(depression ~ agea + gndr + health + cntry, tidyr::drop_na(ess_health))


mod3 <- lm(depression ~ agea * gndr + eisced + health + cntry, tidyr::drop_na(ess_health))
mod4 <- lm_std(depression ~ agea * gndr + eisced + health + cntry, tidyr::drop_na(ess_health))

coef_names <- tribble(
  ~old,           ~new,           
   "(Intercept)",  "*(Intercept)*", 
   "agea",         "Age",        
   "gndr2",        "Gender *(female)*",       
   "health",       "Poor health",      
   "cntryFR",      "France *(vs DE)*",     
   "cntryGB",      "UK *(vs DE)*",
   "eisced",       "Education",
   "agea:gndr2",   "Age x Female",
)

report_lm_with_std(mod = list(mod1, mod3), mod_std = list(mod2, mod4),
                   coef_renames = coef_names, R2_change = TRUE)
```

<html lang="en">

<head>

<meta charset="utf-8"/>

<style>body{background-color:white;}</style>

</head>

<body>

<div id="hnldanaxjp"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>\#hnldanaxjp table { font-family: times, system-ui, ‘Segoe UI’,
Roboto, Helvetica, Arial, sans-serif, ‘Apple Color Emoji’, ‘Segoe UI
Emoji’, ‘Segoe UI Symbol’, ‘Noto Color Emoji’; -webkit-font-smoothing:
antialiased; -moz-osx-font-smoothing: grayscale; }

\#hnldanaxjp thead, \#hnldanaxjp tbody, \#hnldanaxjp tfoot, \#hnldanaxjp
tr, \#hnldanaxjp td, \#hnldanaxjp th { border-style: none; }

\#hnldanaxjp p { margin: 0; padding: 0; }

\#hnldanaxjp .gt_table { display: table; border-collapse: collapse;
line-height: normal; margin-left: auto; margin-right: auto; color:
\#333333; font-size: 12px; font-weight: normal; font-style: normal;
background-color: \#FFFFFF; width: auto; border-top-style: none;
border-top-width: 2px; border-top-color: \#FFFFFF; border-right-style:
none; border-right-width: 2px; border-right-color: \#D3D3D3;
border-bottom-style: none; border-bottom-width: 2px;
border-bottom-color: \#A8A8A8; border-left-style: none;
border-left-width: 2px; border-left-color: \#D3D3D3; }

\#hnldanaxjp .gt_caption { padding-top: 4px; padding-bottom: 4px; }

\#hnldanaxjp .gt_title { color: \#333333; font-size: 12px; font-weight:
initial; padding-top: 4px; padding-bottom: 4px; padding-left: 5px;
padding-right: 5px; border-bottom-color: \#FFFFFF; border-bottom-width:
0; }

\#hnldanaxjp .gt_subtitle { color: \#333333; font-size: 12px;
font-weight: initial; padding-top: 3px; padding-bottom: 5px;
padding-left: 5px; padding-right: 5px; border-top-color: \#FFFFFF;
border-top-width: 0; }

\#hnldanaxjp .gt_heading { background-color: \#FFFFFF; text-align:
center; border-bottom-color: \#FFFFFF; border-left-style: none;
border-left-width: 1px; border-left-color: \#D3D3D3; border-right-style:
none; border-right-width: 1px; border-right-color: \#D3D3D3; }

\#hnldanaxjp .gt_bottom_border { border-bottom-style: solid;
border-bottom-width: 2px; border-bottom-color: \#000000; }

\#hnldanaxjp .gt_col_headings { border-top-style: none;
border-top-width: 2px; border-top-color: \#D3D3D3; border-bottom-style:
solid; border-bottom-width: 1px; border-bottom-color: \#000000;
border-left-style: none; border-left-width: 1px; border-left-color:
\#D3D3D3; border-right-style: none; border-right-width: 1px;
border-right-color: \#D3D3D3; }

\#hnldanaxjp .gt_col_heading { color: \#333333; background-color:
\#FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit;
border-left-style: none; border-left-width: 1px; border-left-color:
\#D3D3D3; border-right-style: none; border-right-width: 1px;
border-right-color: \#D3D3D3; vertical-align: bottom; padding-top: 5px;
padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x:
hidden; }

\#hnldanaxjp .gt_column_spanner_outer { color: \#333333;
background-color: \#FFFFFF; font-size: 100%; font-weight: normal;
text-transform: inherit; padding-top: 0; padding-bottom: 0;
padding-left: 4px; padding-right: 4px; }

\#hnldanaxjp .gt_column_spanner_outer:first-child { padding-left: 0; }

\#hnldanaxjp .gt_column_spanner_outer:last-child { padding-right: 0; }

\#hnldanaxjp .gt_column_spanner { border-bottom-style: solid;
border-bottom-width: 1px; border-bottom-color: \#000000; vertical-align:
bottom; padding-top: 5px; padding-bottom: 5px; overflow-x: hidden;
display: inline-block; width: 100%; }

\#hnldanaxjp .gt_spanner_row { border-bottom-style: hidden; }

\#hnldanaxjp .gt_group_heading { padding-top: 8px; padding-bottom: 8px;
padding-left: 5px; padding-right: 5px; color: \#333333;
background-color: \#FFFFFF; font-size: 100%; font-weight: initial;
text-transform: inherit; border-top-style: none; border-top-width: 2px;
border-top-color: \#D3D3D3; border-bottom-style: none;
border-bottom-width: 2px; border-bottom-color: \#D3D3D3;
border-left-style: none; border-left-width: 1px; border-left-color:
\#D3D3D3; border-right-style: none; border-right-width: 1px;
border-right-color: \#D3D3D3; vertical-align: middle; text-align: left;
}

\#hnldanaxjp .gt_empty_group_heading { padding: 0.5px; color: \#333333;
background-color: \#FFFFFF; font-size: 100%; font-weight: initial;
border-top-style: none; border-top-width: 2px; border-top-color:
\#D3D3D3; border-bottom-style: none; border-bottom-width: 2px;
border-bottom-color: \#D3D3D3; vertical-align: middle; }

\#hnldanaxjp .gt_from_md \> :first-child { margin-top: 0; }

\#hnldanaxjp .gt_from_md \> :last-child { margin-bottom: 0; }

\#hnldanaxjp .gt_row { padding-top: 8px; padding-bottom: 8px;
padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style:
none; border-top-width: 1px; border-top-color: \#FFFFFF;
border-left-style: none; border-left-width: 1px; border-left-color:
\#D3D3D3; border-right-style: none; border-right-width: 1px;
border-right-color: \#D3D3D3; vertical-align: middle; overflow-x:
hidden; }

\#hnldanaxjp .gt_stub { color: \#333333; background-color: \#FFFFFF;
font-size: 100%; font-weight: initial; text-transform: inherit;
border-right-style: none; border-right-width: 2px; border-right-color:
\#D3D3D3; padding-left: 5px; padding-right: 5px; }

\#hnldanaxjp .gt_stub_row_group { color: \#333333; background-color:
\#FFFFFF; font-size: 100%; font-weight: initial; text-transform:
inherit; border-right-style: none; border-right-width: 2px;
border-right-color: \#D3D3D3; padding-left: 5px; padding-right: 5px;
vertical-align: top; }

\#hnldanaxjp .gt_row_group_first td { border-top-width: 2px; }

\#hnldanaxjp .gt_row_group_first th { border-top-width: 2px; }

\#hnldanaxjp .gt_summary_row { color: \#333333; background-color:
\#FFFFFF; text-transform: inherit; padding-top: 8px; padding-bottom:
8px; padding-left: 5px; padding-right: 5px; }

\#hnldanaxjp .gt_first_summary_row { border-top-style: none;
border-top-color: \#D3D3D3; }

\#hnldanaxjp .gt_first_summary_row.thick { border-top-width: 2px; }

\#hnldanaxjp .gt_last_summary_row { padding-top: 8px; padding-bottom:
8px; padding-left: 5px; padding-right: 5px; border-bottom-style: none;
border-bottom-width: 2px; border-bottom-color: \#D3D3D3; }

\#hnldanaxjp .gt_grand_summary_row { color: \#333333; background-color:
\#FFFFFF; text-transform: inherit; padding-top: 8px; padding-bottom:
8px; padding-left: 5px; padding-right: 5px; }

\#hnldanaxjp .gt_first_grand_summary_row { padding-top: 8px;
padding-bottom: 8px; padding-left: 5px; padding-right: 5px;
border-top-style: none; border-top-width: 6px; border-top-color:
\#D3D3D3; }

\#hnldanaxjp .gt_last_grand_summary_row_top { padding-top: 8px;
padding-bottom: 8px; padding-left: 5px; padding-right: 5px;
border-bottom-style: none; border-bottom-width: 6px;
border-bottom-color: \#D3D3D3; }

\#hnldanaxjp .gt_striped { background-color: rgba(128, 128, 128, 0.05);
}

\#hnldanaxjp .gt_table_body { border-top-style: solid; border-top-width:
1px; border-top-color: \#000000; border-bottom-style: solid;
border-bottom-width: 1px; border-bottom-color: \#000000; }

\#hnldanaxjp .gt_footnotes { color: \#333333; background-color:
\#FFFFFF; border-bottom-style: none; border-bottom-width: 2px;
border-bottom-color: \#D3D3D3; border-left-style: none;
border-left-width: 2px; border-left-color: \#D3D3D3; border-right-style:
none; border-right-width: 2px; border-right-color: \#D3D3D3; }

\#hnldanaxjp .gt_footnote { margin: 0px; font-size: 90%; padding-top:
4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; }

\#hnldanaxjp .gt_sourcenotes { color: \#333333; background-color:
\#FFFFFF; border-bottom-style: none; border-bottom-width: 2px;
border-bottom-color: \#D3D3D3; border-left-style: none;
border-left-width: 2px; border-left-color: \#D3D3D3; border-right-style:
none; border-right-width: 2px; border-right-color: \#D3D3D3; }

\#hnldanaxjp .gt_sourcenote { font-size: 90%; padding-top: 4px;
padding-bottom: 4px; padding-left: 5px; padding-right: 5px; }

\#hnldanaxjp .gt_left { text-align: left; }

\#hnldanaxjp .gt_center { text-align: center; }

\#hnldanaxjp .gt_right { text-align: right; font-variant-numeric:
tabular-nums; }

\#hnldanaxjp .gt_font_normal { font-weight: normal; }

\#hnldanaxjp .gt_font_bold { font-weight: bold; }

\#hnldanaxjp .gt_font_italic { font-style: italic; }

\#hnldanaxjp .gt_super { font-size: 65%; }

\#hnldanaxjp .gt_footnote_marks { font-size: 75%; vertical-align: 0.4em;
position: initial; }

\#hnldanaxjp .gt_asterisk { font-size: 100%; vertical-align: 0; }

\#hnldanaxjp .gt_indent_1 { text-indent: 5px; }

\#hnldanaxjp .gt_indent_2 { text-indent: 10px; }

\#hnldanaxjp .gt_indent_3 { text-indent: 15px; }

\#hnldanaxjp .gt_indent_4 { text-indent: 20px; }

\#hnldanaxjp .gt_indent_5 { text-indent: 25px; }

\#hnldanaxjp .katex-display { display: inline-flex !important;
margin-bottom: 0.75em !important; }

\#hnldanaxjp div.Reactable \> div.rt-table \> div.rt-thead \>
div.rt-tr.rt-tr-group-header \> div.rt-th-group:after { height: 0px
!important; } </style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">

<thead>

<tr class="gt_col_headings gt_spanner_row">

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="a-">

<span class="gt_from_md"></span>
</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**Model 1**">

<div class="gt_column_spanner">

<span class="gt_from_md"><strong>Model 1</strong></span>

</div>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**Model 2**">

<div class="gt_column_spanner">

<span class="gt_from_md"><strong>Model 2</strong></span>

</div>

</th>

</tr>

<tr class="gt_col_headings">

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Model1">

<span class='gt_from_md'><em>
<center>

B (SE)
</center>

</em></span>
</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Model2">

<span class='gt_from_md'><em>
<center>

β \[95% CI\]
</center>

</em></span>
</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Model3">

<span class='gt_from_md'><em>
<center>

B (SE)
</center>

</em></span>
</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Model4">

<span class='gt_from_md'><em>
<center>

β \[95% CI\]
</center>

</em></span>
</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td headers=" " class="gt_row gt_left">

<span class="gt_from_md"><em>(Intercept)</em></span>
</td>

<td headers="Model1" class="gt_row gt_right">

<span class='gt_from_md'>1.21 (0.02)\*\*\*</span>
</td>

<td headers="Model2" class="gt_row gt_right">

<span class="gt_from_md">-0.14 \[-0.18, -0.10\]</span>
</td>

<td headers="Model3" class="gt_row gt_right">

<span class='gt_from_md'>1.27 (0.02)\*\*\*</span>
</td>

<td headers="Model4" class="gt_row gt_right">

<span class="gt_from_md">-0.14 \[-0.18, -0.10\]</span>
</td>

</tr>

<tr>

<td headers=" " class="gt_row gt_left">

<span class="gt_from_md">Age</span>
</td>

<td headers="Model1" class="gt_row gt_right">

<span class='gt_from_md'>-0.00 (0.00)\*\*\*</span>
</td>

<td headers="Model2" class="gt_row gt_right">

<span class="gt_from_md">-0.10 \[-0.12, -0.08\]</span>
</td>

<td headers="Model3" class="gt_row gt_right">

<span class='gt_from_md'>-0.00 (0.00)\*\*\*</span>
</td>

<td headers="Model4" class="gt_row gt_right">

<span class="gt_from_md">-0.14 \[-0.17, -0.10\]</span>
</td>

</tr>

<tr>

<td headers=" " class="gt_row gt_left">

<span class="gt_from_md">Gender <em>(female)</em></span>
</td>

<td headers="Model1" class="gt_row gt_right">

<span class='gt_from_md'>0.11 (0.01)\*\*\*</span>
</td>

<td headers="Model2" class="gt_row gt_right">

<span class="gt_from_md">0.24 \[0.20, 0.28\]</span>
</td>

<td headers="Model3" class="gt_row gt_right">

<span class="gt_from_md">0.03 (0.03)   </span>
</td>

<td headers="Model4" class="gt_row gt_right">

<span class="gt_from_md">0.24 \[0.20, 0.28\]</span>
</td>

</tr>

<tr>

<td headers=" " class="gt_row gt_left">

<span class="gt_from_md">Poor health</span>
</td>

<td headers="Model1" class="gt_row gt_right">

<span class='gt_from_md'>0.23 (0.01)\*\*\*</span>
</td>

<td headers="Model2" class="gt_row gt_right">

<span class="gt_from_md">0.44 \[0.42, 0.46\]</span>
</td>

<td headers="Model3" class="gt_row gt_right">

<span class='gt_from_md'>0.23 (0.01)\*\*\*</span>
</td>

<td headers="Model4" class="gt_row gt_right">

<span class="gt_from_md">0.44 \[0.42, 0.46\]</span>
</td>

</tr>

<tr>

<td headers=" " class="gt_row gt_left">

<span class="gt_from_md">France <em>(vs DE)</em></span>
</td>

<td headers="Model1" class="gt_row gt_right">

<span class="gt_from_md">-0.01 (0.01)   </span>
</td>

<td headers="Model2" class="gt_row gt_right">

<span class="gt_from_md">-0.03 \[-0.08, 0.02\]</span>
</td>

<td headers="Model3" class="gt_row gt_right">

<span class="gt_from_md">-0.01 (0.01)   </span>
</td>

<td headers="Model4" class="gt_row gt_right">

<span class="gt_from_md">-0.03 \[-0.08, 0.02\]</span>
</td>

</tr>

<tr>

<td headers=" " class="gt_row gt_left">

<span class="gt_from_md">UK <em>(vs DE)</em></span>
</td>

<td headers="Model1" class="gt_row gt_right">

<span class='gt_from_md'>0.04 (0.01)\*\* </span>
</td>

<td headers="Model2" class="gt_row gt_right">

<span class="gt_from_md">0.08 \[0.03, 0.13\]</span>
</td>

<td headers="Model3" class="gt_row gt_right">

<span class='gt_from_md'>0.04 (0.01)\*\* </span>
</td>

<td headers="Model4" class="gt_row gt_right">

<span class="gt_from_md">0.08 \[0.03, 0.13\]</span>
</td>

</tr>

<tr>

<td headers=" " class="gt_row gt_left">

<span class="gt_from_md">Education</span>
</td>

<td headers="Model1" class="gt_row gt_right">

<span class="gt_from_md"></span>
</td>

<td headers="Model2" class="gt_row gt_right">

<span class="gt_from_md"></span>
</td>

<td headers="Model3" class="gt_row gt_right">

<span class="gt_from_md">-0.00 (0.00)†  </span>
</td>

<td headers="Model4" class="gt_row gt_right">

<span class="gt_from_md">-0.02 \[-0.04, 0.00\]</span>
</td>

</tr>

<tr>

<td headers=" " class="gt_row gt_left">

<span class="gt_from_md">Age x Female</span>
</td>

<td headers="Model1" class="gt_row gt_right">

<span class="gt_from_md"></span>
</td>

<td headers="Model2" class="gt_row gt_right">

<span class="gt_from_md"></span>
</td>

<td headers="Model3" class="gt_row gt_right">

<span class='gt_from_md'>0.00 (0.00)\*\* </span>
</td>

<td headers="Model4" class="gt_row gt_right">

<span class="gt_from_md">0.07 \[0.02, 0.11\]</span>
</td>

</tr>

<tr style="border-top-style: solid; border-top-width: 2px;">

<td class="gt_row gt_left" rowspan="1" colspan="1">

<em>N
</td>

<td class="gt_row gt_center" rowspan="1" colspan="2">

6852
</td>

<td class="gt_row gt_center" rowspan="1" colspan="2">

6852
</td>

</tr>

<tr>

<td class="gt_row gt_left" rowspan="1" colspan="1">

<em>R<sup>2</sup>
</td>

<td class="gt_row gt_center" rowspan="1" colspan="2">

.20
</td>

<td class="gt_row gt_center" rowspan="1" colspan="2">

.20
</td>

</tr>

<tr>

<td class="gt_row gt_left" rowspan="1" colspan="1">

<em>F</em>-tests
</td>

<td class="gt_row gt_center" rowspan="1" colspan="2">

<p>

<em>F</em>(5, 6846) = 337.06, <em>p</em> \< .001
</p>

</td>

<td class="gt_row gt_center" rowspan="1" colspan="2">

<p>

<em>F</em>(7, 6844) = 243.10, <em>p</em> \< .001
</p>

</td>

</tr>

<tr>

<td class="gt_row gt_left" rowspan="1" colspan="1">

<em>Change</em>
</td>

<td class="gt_row gt_center" rowspan="1" colspan="4">

Δ<em>R</em><sup>2</sup> = .00, <em>F</em>(2, 6844) = 6.78, <em>p</em> =
.001
</td>

</tr>

</tbody>

<tfoot>

<tr class="gt_sourcenotes">

<td class="gt_sourcenote" colspan="5">

<span class="gt_from_md">† <em>p</em> \< .1, \* <em>p</em> \< .05, \*\*
<em>p</em> \< .01, \*\*\* <em>p</em> \< .001</span>
</td>

</tr>

</tfoot>

</table>

</div>

</body>

# Related/alternative packages

- [`modelsummary`](https://modelsummary.com/) allows you to create
  highly customisable tables with data summaries or the output of
  statistical models that can be saved in a wide range of formats.
- [`apa`](https://CRAN.R-project.org/package=apa) mostly offers
  functions that turn the output of statistical tests (e.g., t-tests)
  into text, in line with APA guidelines.
- [`papaja`](https://github.com/crsh/papaja) offers the opportunity to
  create full APA-style journal manuscripts in R. It’s `apa_table`
  function is a generic alternative to the table functions in this
  package, which support many more types of models, but includes fewer
  details.

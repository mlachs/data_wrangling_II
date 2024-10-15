Strings and Factors
================
Miriam Lachs
2024-10-15

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(p8105.datasets)
```

## Let’s do strings

``` r
string_vec = c("my", "name", "is", "miriam")

str_detect(string_vec, "miriam")
```

    ## [1] FALSE FALSE FALSE  TRUE

``` r
str_replace(string_vec, "miriam", "Miriam")
```

    ## [1] "my"     "name"   "is"     "Miriam"

``` r
string_vec = c(
  "i think we all rule for participating",
  "i think i have been caught",
  "i think this will be quite fun actually",
  "it will be fun, i think"
  )

str_detect(string_vec, "^i think")
```

    ## [1]  TRUE  TRUE  TRUE FALSE

``` r
str_detect(string_vec, "i think$")
```

    ## [1] FALSE FALSE FALSE  TRUE

``` r
string_vec = c(
  "Time for a Pumpkin Spice Latte!",
  "went to the #pumpkinpatch last weekend",
  "Pumpkin Pie is obviously the best pie",
  "SMASHING PUMPKINS -- LIVE IN CONCERT!!"
  )

str_detect(string_vec,"[Pp]umpkin")
```

    ## [1]  TRUE  TRUE  TRUE FALSE

``` r
string_vec = c(
  '7th inning stretch',
  '1st half soon to begin. Texas won the toss.',
  'she is 5 feet 4 inches tall',
  '3AM - cant sleep :('
  )

str_detect(string_vec, "^[0-9][a-zA-Z]")
```

    ## [1]  TRUE  TRUE FALSE  TRUE

``` r
string_vec = c(
  'Its 7:11 in the evening',
  'want to go to 7-11?',
  'my flight is AA711',
  'NetBios: scanning ip 203.167.114.66'
  )

str_detect(string_vec, "7.11")
```

    ## [1]  TRUE  TRUE FALSE  TRUE

How things start to get strange

``` r
string_vec = c(
  'The CI is [2, 5]',
  ':-]',
  ':-[',
  'I found the answer on pages [6-7]'
  )

str_detect(string_vec, "\\[")
```

    ## [1]  TRUE FALSE  TRUE  TRUE

## Factors

``` r
sex_vec = factor(c('male','male','female','female'))

as.numeric(sex_vec)
```

    ## [1] 2 2 1 1

do some releveling

``` r
sex_vec=fct_relevel(sex_vec,'male')

as.numeric(sex_vec)
```

    ## [1] 1 1 2 2

## Revisit examples

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

table_marj = 
  read_html(nsduh_url) |> 
  html_table() |> 
  first() |>
  slice(-1) %>% 
  select(-contains("P Value")) %>% 
  pivot_longer(cols = -State,
               names_to = "age_year",
               values_to = 'percent') %>% 
  separate(age_year, into = c('age','year'),sep = '\\(') %>% 
  mutate(
    year = str_replace(year,'\\)',''),
    percent=str_remove(percent,"[a-c]$"),
    percent=as.numeric(percent)
  )
```

``` r
table_marj %>% 
  filter(age=='12-17') %>% 
  mutate(
    State=fct_reorder(State,percent)
  ) %>% 
  ggplot(aes(x=State,y=percent, colour = year))+
  geom_point()+ 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](strings_and_factors_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

## NYC RRestuarnt Inspections

``` r
data('rest_inspec')
```

``` r
rest_inspec %>% 
  count(boro,grade) %>% 
  pivot_wider(
    names_from = grade,
    values_from = n
  )
```

    ## # A tibble: 6 × 8
    ##   boro              A     B     C `Not Yet Graded`     P     Z  `NA`
    ##   <chr>         <int> <int> <int>            <int> <int> <int> <int>
    ## 1 BRONX         13688  2801   701              200   163   351 16833
    ## 2 BROOKLYN      37449  6651  1684              702   416   977 51930
    ## 3 MANHATTAN     61608 10532  2689              765   508  1237 80615
    ## 4 Missing           4    NA    NA               NA    NA    NA    13
    ## 5 QUEENS        35952  6492  1593              604   331   913 45816
    ## 6 STATEN ISLAND  5215   933   207               85    47   149  6730

``` r
rest_inspec =
  rest_inspec %>% 
  filter(
    str_detect(grade,"[A-C]"),
    !(boro=='Missing')
  )
```

``` r
rest_inspec %>% 
  mutate(dba=str_to_sentence(dba)) %>% 
  filter(str_detect(dba,'Pizza'))
```

    ## # A tibble: 775 × 18
    ##    action          boro  building  camis critical_flag cuisine_description dba  
    ##    <chr>           <chr> <chr>     <int> <chr>         <chr>               <chr>
    ##  1 Violations wer… MANH… 151      5.00e7 Not Critical  Pizza               Pizz…
    ##  2 Violations wer… MANH… 151      5.00e7 Critical      Pizza               Pizz…
    ##  3 Violations wer… MANH… 151      5.00e7 Critical      Pizza               Pizz…
    ##  4 Violations wer… MANH… 15       5.01e7 Critical      Pizza               & Pi…
    ##  5 Violations wer… MANH… 151      5.00e7 Critical      Pizza               Pizz…
    ##  6 Violations wer… MANH… 151      5.00e7 Not Critical  Pizza               Pizz…
    ##  7 Violations wer… MANH… 15       5.01e7 Critical      Pizza               & Pi…
    ##  8 Violations wer… MANH… 151      5.00e7 Critical      Pizza               Pizz…
    ##  9 Violations wer… MANH… 84       5.00e7 Not Critical  Pizza               Pizza
    ## 10 Violations wer… MANH… 525      5.01e7 Not Critical  Pizza               Pizz…
    ## # ℹ 765 more rows
    ## # ℹ 11 more variables: inspection_date <dttm>, inspection_type <chr>,
    ## #   phone <chr>, record_date <dttm>, score <int>, street <chr>,
    ## #   violation_code <chr>, violation_description <chr>, zipcode <int>,
    ## #   grade <chr>, grade_date <dttm>

``` r
rest_inspec %>% 
  mutate(dba=str_to_sentence(dba)) %>% 
  filter(str_detect(dba,'Pizza')) %>% 
  mutate(boro=fct_infreq(boro)) %>% 
  ggplot(aes(x=boro))+geom_bar()
```

![](strings_and_factors_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
rest_inspec %>% 
  mutate(dba=str_to_sentence(dba)) %>% 
  filter(str_detect(dba,'Pizza')) %>% 
  mutate(boro=fct_infreq(boro),
         boro=fct_recode(boro, "THE CITY"="MANHATTAN")) %>% 
  ggplot(aes(x=boro))+geom_bar()
```

![](strings_and_factors_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

One last thing on factors

``` r
rest_inspec %>% 
   mutate(dba=str_to_sentence(dba)) %>% 
  filter(str_detect(dba,'Pizza')) %>% 
  lm(zipcode~boro,data = .)
```

    ## 
    ## Call:
    ## lm(formula = zipcode ~ boro, data = .)
    ## 
    ## Coefficients:
    ##       (Intercept)       boroBROOKLYN      boroMANHATTAN         boroQUEENS  
    ##           10461.2              761.2             -435.7              909.1  
    ## boroSTATEN ISLAND  
    ##            -150.9

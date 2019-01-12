Day003-1
================

``` r
country <- c("Taiwan", "China", "Japan", "Korea", "Singapore")
population <- sample(1e8:1e9, 5)
(df <- data.frame(Country = country, Population = population))
```

    ##     Country Population
    ## 1    Taiwan  201844359
    ## 2     China  786785057
    ## 3     Japan  823296938
    ## 4     Korea  895762740
    ## 5 Singapore  996912893

``` r
index <- which.max(population)
df[index,]$Country
```

    ## [1] Singapore
    ## Levels: China Japan Korea Singapore Taiwan

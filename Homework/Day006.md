Day006
================

``` r
library(magrittr)
library(tidyverse)
```

``` r
app_train <- read.csv("data/application_train.csv")
sapply(app_train, class) %>% as.factor() %>% summary()
```

    ##  factor integer numeric 
    ##      16      41      65

``` r
col_type <- sapply(app_train, class) %>% as.factor()
num_col <- which(!col_type == "factor") # 先篩選數值型的欄位
num_dum_idx <- app_train[,num_col] %>%
  sapply(unique) %>%
  sapply(length) %>%
  {. == 2} 
num_col <- app_train[,num_col][,!num_dum_idx] # 再把只有 2 值 (通常是 0,1) 的欄位去掉
print(paste("Numbers of remain columns:", ncol(num_col)))
```

    ## [1] "Numbers of remain columns: 73"

``` r
# 檢視這些欄位的數值範圍
for(i in 1:ncol(num_col)) {
  boxplot(num_col[,i], main=paste(colnames(num_col)[i], "boxplot"))
}
```

![](Day006_files/figure-markdown_github/unnamed-chunk-4-1.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-2.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-3.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-4.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-5.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-6.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-7.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-8.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-9.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-10.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-11.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-12.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-13.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-14.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-15.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-16.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-17.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-18.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-19.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-20.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-21.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-22.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-23.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-24.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-25.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-26.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-27.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-28.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-29.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-30.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-31.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-32.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-33.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-34.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-35.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-36.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-37.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-38.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-39.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-40.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-41.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-42.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-43.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-44.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-45.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-46.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-47.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-48.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-49.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-50.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-51.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-52.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-53.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-54.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-55.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-56.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-57.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-58.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-59.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-60.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-61.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-62.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-63.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-64.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-65.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-66.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-67.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-68.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-69.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-70.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-71.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-72.png)![](Day006_files/figure-markdown_github/unnamed-chunk-4-73.png)

``` r
# AMT_INCOME_TOTAL
# REGION_POPULATION_RELATIVE
# OBS_60_CNT_SOCIAL_CIRCLE
```

``` r
num_col$AMT_INCOME_TOTAL %>% summary
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ##     25650    112500    147150    168798    202500 117000000

``` r
# 最大值離平均與中位數很遠
```

``` r
ggplot(num_col, aes(AMT_INCOME_TOTAL)) + 
  stat_ecdf(geom = "step") +
  labs(title="ECDF of AMT_INCOME_TOTAL",
       x='Value',
       y='ECDF')
```

![](Day006_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
# 改變 y 軸的 Scale, 讓我們可以正常檢視 ECDF
ggplot(num_col, aes(log(AMT_INCOME_TOTAL))) + 
  stat_ecdf(geom = "step") +
  labs(title="ECDF of AMT_INCOME_TOTAL",
       x='Value (log-scale)',
       y='ECDF')
```

![](Day006_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
num_col$REGION_POPULATION_RELATIVE %>% summary
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## 0.00029 0.01001 0.01885 0.02087 0.02866 0.07251

``` r
ggplot(num_col, aes(REGION_POPULATION_RELATIVE)) + 
  stat_ecdf(geom = "step") +
  labs(title="ECDF of REGION_POPULATION_RELATIVE",
       x='Value',
       y='ECDF')
```

![](Day006_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
ggplot(num_col, aes(REGION_POPULATION_RELATIVE)) + 
  geom_histogram() +
  labs(title="Histogram of REGION_POPULATION_RELATIVE")
```

![](Day006_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
# 就以這個欄位來說，雖然有資料掉在分布以外，也不算異常，僅代表這間公司在稍微熱鬧的地區有的據點較少，
# 導致 region population relative 在少的部分較為密集，但在大的部分較為疏漏
```

``` r
num_col$OBS_60_CNT_SOCIAL_CIRCLE %>% summary
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   0.000   0.000   0.000   1.405   2.000 344.000    1021

``` r
ggplot(num_col, aes(OBS_60_CNT_SOCIAL_CIRCLE)) + 
  stat_ecdf(geom = "step") +
  labs(title="ECDF of OBS_60_CNT_SOCIAL_CIRCLE",
       x='Value',
       y='ECDF')
```

![](Day006_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
ggplot(num_col, aes(OBS_60_CNT_SOCIAL_CIRCLE)) + 
  geom_histogram() +
  labs(title="Histogram of OBS_60_CNT_SOCIAL_CIRCLE")
```

![](Day006_files/figure-markdown_github/unnamed-chunk-13-1.png)

``` r
# 注意：當 histogram 畫出上面這種圖 (只出現一條，但是 x 軸延伸很長導致右邊有一大片空白時，代表右邊有值但是數量稀少。
```

``` r
num_col$OBS_60_CNT_SOCIAL_CIRCLE %>% 
  unique %>% sort(decreasing = TRUE)
```

    ##  [1] 344  47  30  29  28  27  26  25  24  23  22  21  20  19  18  17  16
    ## [18]  15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0

``` r
num_col %>% 
  filter(OBS_60_CNT_SOCIAL_CIRCLE < 20) %>% 
  ggplot(aes(OBS_60_CNT_SOCIAL_CIRCLE)) + 
  geom_histogram() +
  labs(title="Histogram of OBS_60_CNT_SOCIAL_CIRCLE(<20)")
```

![](Day006_files/figure-markdown_github/unnamed-chunk-15-1.png)

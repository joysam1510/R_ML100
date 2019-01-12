Day004
================

``` r
library(magrittr)
library(tidyverse)
library(caret)
```

Label Encoding
--------------

把每個類別 mapping 到某個整數，不會增加新欄位

``` r
# encoding all character features into ids with sort by name
count <- 0
for (f in feature.names) {
    if (class(train[[f]]) == 'factor') {
        levels <- sort(unique(c(train[[f]], test[[f]])))
        train[[f]] <- as.integer(factor(train[[f]], levels = levels))
        test[[f]] <- as.integer(factor(test[[f]], levels = levels))
        count <- count + 1
    }
}

print(paste(count,"labels were label encoded."))
```

Label encoding 的表示方式會讓同一個欄位底下的類別之間有大小關係 (0&lt;1&lt;2&lt;...)，所以在這裡我們只對有類別數量小於等於 2 的類別型欄位示範使用 Label encoding，但不表示這樣處理是最好的，一切取決於欄位本身的意義適合哪一種表示方法

``` r
count <- 0
for (f in feature.names) {
    if (nlevels(train[[f]]) = 1) {
        levels <- sort(unique(c(train[[f]], test[[f]])))
        train[[f]] <- as.integer(factor(train[[f]], levels = levels))
        test[[f]] <- as.integer(factor(test[[f]], levels = levels))
        count <- count + 1
    }
}

print(paste(count,"labels were label encoded."))
```

One Hot Encoding
----------------

``` r
library(caret)
dmy <- dummyVars(" ~ .", data = data)
dummy_df <- data.frame(predict(dmy, newdata = data))
```

HW
--

檢視資料中各個欄位類型的數量

``` r
app_train <- read.csv("data/application_train.csv")
sapply(app_train,class) %>% as.factor() %>% summary()
```

    ##  factor integer numeric 
    ##      16      41      65

檢視資料中類別型欄位各自類別的數量

``` r
n_level <- sapply(app_train,nlevels) 
n_level[n_level>0]
```

    ##         NAME_CONTRACT_TYPE                CODE_GENDER 
    ##                          2                          3 
    ##               FLAG_OWN_CAR            FLAG_OWN_REALTY 
    ##                          2                          2 
    ##            NAME_TYPE_SUITE           NAME_INCOME_TYPE 
    ##                          8                          8 
    ##        NAME_EDUCATION_TYPE         NAME_FAMILY_STATUS 
    ##                          5                          6 
    ##          NAME_HOUSING_TYPE            OCCUPATION_TYPE 
    ##                          6                         19 
    ## WEEKDAY_APPR_PROCESS_START          ORGANIZATION_TYPE 
    ##                          7                         58 
    ##         FONDKAPREMONT_MODE             HOUSETYPE_MODE 
    ##                          5                          4 
    ##         WALLSMATERIAL_MODE        EMERGENCYSTATE_MODE 
    ##                          8                          3

``` r
sub_train <- app_train$WEEKDAY_APPR_PROCESS_START %>% as.data.frame()

dmy <- dummyVars(" ~ .", data = sub_train)
df <- data.frame(predict(dmy, newdata = sub_train))
df %>% set_colnames(sort(unique(sub_train[,1]))) %>% head()
```

    ##   FRIDAY MONDAY SATURDAY SUNDAY THURSDAY TUESDAY WEDNESDAY
    ## 1      0      0        0      0        0       0         1
    ## 2      0      1        0      0        0       0         0
    ## 3      0      1        0      0        0       0         0
    ## 4      0      0        0      0        0       0         1
    ## 5      0      0        0      0        1       0         0
    ## 6      0      0        0      0        0       0         1

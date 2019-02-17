Day038
================

``` r
library(mlbench)
library(tidyverse)
library(caret)
library(pROC)
library(DataExplorer)
```

練習時間
--------

試著使用 sklearn datasets 的其他資料集 (wine, boston, ...)，來訓練自己的線性迴歸模型。

Wine dataset

``` r
wine <- read.delim("data/wine.data", header=F, sep=",")
colnames(wine) <- c("Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium"," Total phenols","Flavanoids",
                    "Nonflavanoid phenols","Proanthocyanins","Color intensity","Hue","OD280/OD315 of diluted wines","Proline")
head(wine)
```

    ##   Alcohol Malic acid  Ash Alcalinity of ash Magnesium  Total phenols
    ## 1       1      14.23 1.71              2.43      15.6            127
    ## 2       1      13.20 1.78              2.14      11.2            100
    ## 3       1      13.16 2.36              2.67      18.6            101
    ## 4       1      14.37 1.95              2.50      16.8            113
    ## 5       1      13.24 2.59              2.87      21.0            118
    ## 6       1      14.20 1.76              2.45      15.2            112
    ##   Flavanoids Nonflavanoid phenols Proanthocyanins Color intensity  Hue
    ## 1       2.80                 3.06            0.28            2.29 5.64
    ## 2       2.65                 2.76            0.26            1.28 4.38
    ## 3       2.80                 3.24            0.30            2.81 5.68
    ## 4       3.85                 3.49            0.24            2.18 7.80
    ## 5       2.80                 2.69            0.39            1.82 4.32
    ## 6       3.27                 3.39            0.34            1.97 6.75
    ##   OD280/OD315 of diluted wines Proline   NA
    ## 1                         1.04    3.92 1065
    ## 2                         1.05    3.40 1050
    ## 3                         1.03    3.17 1185
    ## 4                         0.86    3.45 1480
    ## 5                         1.04    2.93  735
    ## 6                         1.05    2.85 1450

``` r
unique(wine$Alcohol)
```

    ## [1] 1 2 3

BostonHousing dataset

``` r
data(BostonHousing)
str(BostonHousing)
```

    ## 'data.frame':    506 obs. of  14 variables:
    ##  $ crim   : num  0.00632 0.02731 0.02729 0.03237 0.06905 ...
    ##  $ zn     : num  18 0 0 0 0 0 12.5 12.5 12.5 12.5 ...
    ##  $ indus  : num  2.31 7.07 7.07 2.18 2.18 2.18 7.87 7.87 7.87 7.87 ...
    ##  $ chas   : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ nox    : num  0.538 0.469 0.469 0.458 0.458 0.458 0.524 0.524 0.524 0.524 ...
    ##  $ rm     : num  6.58 6.42 7.18 7 7.15 ...
    ##  $ age    : num  65.2 78.9 61.1 45.8 54.2 58.7 66.6 96.1 100 85.9 ...
    ##  $ dis    : num  4.09 4.97 4.97 6.06 6.06 ...
    ##  $ rad    : num  1 2 2 3 3 3 5 5 5 5 ...
    ##  $ tax    : num  296 242 242 222 222 222 311 311 311 311 ...
    ##  $ ptratio: num  15.3 17.8 17.8 18.7 18.7 18.7 15.2 15.2 15.2 15.2 ...
    ##  $ b      : num  397 397 393 395 397 ...
    ##  $ lstat  : num  4.98 9.14 4.03 2.94 5.33 ...
    ##  $ medv   : num  24 21.6 34.7 33.4 36.2 28.7 22.9 27.1 16.5 18.9 ...

``` r
plot_missing(BostonHousing) ## Are there missing values, and what is the missing data profile?
```

![](Day038_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
plot_bar(BostonHousing) ## How does the categorical frequency for each discrete variable look like?
```

![](Day038_files/figure-markdown_github/unnamed-chunk-5-2.png)

``` r
plot_histogram(BostonHousing) ## What is the distribution of each continuous variable?
```

![](Day038_files/figure-markdown_github/unnamed-chunk-5-3.png)

``` r
plot_boxplot(BostonHousing, by = "medv") 
```

![](Day038_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
plot_scatterplot(subset(BostonHousing, select = -c(crim, zn, indus, b)), by = "medv", geom_point_args = list(size=.5))
```

![](Day038_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
plot_correlation(BostonHousing)
```

![](Day038_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
fit <- lm(medv~., data=BostonHousing)
summary(fit)
```

    ## 
    ## Call:
    ## lm(formula = medv ~ ., data = BostonHousing)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -15.595  -2.730  -0.518   1.777  26.199 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.646e+01  5.103e+00   7.144 3.28e-12 ***
    ## crim        -1.080e-01  3.286e-02  -3.287 0.001087 ** 
    ## zn           4.642e-02  1.373e-02   3.382 0.000778 ***
    ## indus        2.056e-02  6.150e-02   0.334 0.738288    
    ## chas1        2.687e+00  8.616e-01   3.118 0.001925 ** 
    ## nox         -1.777e+01  3.820e+00  -4.651 4.25e-06 ***
    ## rm           3.810e+00  4.179e-01   9.116  < 2e-16 ***
    ## age          6.922e-04  1.321e-02   0.052 0.958229    
    ## dis         -1.476e+00  1.995e-01  -7.398 6.01e-13 ***
    ## rad          3.060e-01  6.635e-02   4.613 5.07e-06 ***
    ## tax         -1.233e-02  3.760e-03  -3.280 0.001112 ** 
    ## ptratio     -9.527e-01  1.308e-01  -7.283 1.31e-12 ***
    ## b            9.312e-03  2.686e-03   3.467 0.000573 ***
    ## lstat       -5.248e-01  5.072e-02 -10.347  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.745 on 492 degrees of freedom
    ## Multiple R-squared:  0.7406, Adjusted R-squared:  0.7338 
    ## F-statistic: 108.1 on 13 and 492 DF,  p-value: < 2.2e-16

BreastCancer dataset

``` r
data(BreastCancer)
str(BreastCancer)
```

    ## 'data.frame':    699 obs. of  11 variables:
    ##  $ Id             : chr  "1000025" "1002945" "1015425" "1016277" ...
    ##  $ Cl.thickness   : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 5 5 3 6 4 8 1 2 2 4 ...
    ##  $ Cell.size      : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 1 4 1 8 1 10 1 1 1 2 ...
    ##  $ Cell.shape     : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 1 4 1 8 1 10 1 2 1 1 ...
    ##  $ Marg.adhesion  : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 1 5 1 1 3 8 1 1 1 1 ...
    ##  $ Epith.c.size   : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 2 7 2 3 2 7 2 2 2 2 ...
    ##  $ Bare.nuclei    : Factor w/ 10 levels "1","2","3","4",..: 1 10 2 4 1 10 10 1 1 1 ...
    ##  $ Bl.cromatin    : Factor w/ 10 levels "1","2","3","4",..: 3 3 3 3 3 9 3 3 1 2 ...
    ##  $ Normal.nucleoli: Factor w/ 10 levels "1","2","3","4",..: 1 2 1 7 1 7 1 1 1 1 ...
    ##  $ Mitoses        : Factor w/ 9 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 5 1 ...
    ##  $ Class          : Factor w/ 2 levels "benign","malignant": 1 1 1 1 1 2 1 1 1 1 ...

``` r
plot_missing(BreastCancer) ## Are there missing values, and what is the missing data profile?
```

![](Day038_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
BreastCancer <- BreastCancer %>% select(-Id)
intrain <- sample(nrow(BreastCancer), nrow(BreastCancer)*.8)
train <- BreastCancer[intrain,]; test <- BreastCancer[-intrain,]


control <- trainControl(method="cv", number=5, classProbs = TRUE, summaryFunction=twoClassSummary)
glm_model <- train(Class~., data=train, method="glm", family="binomial", metric="ROC", trControl=control, na.action=na.pass)
glm_model
```

    ## Generalized Linear Model 
    ## 
    ## 559 samples
    ##   9 predictor
    ##   2 classes: 'benign', 'malignant' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 448, 447, 447, 447, 447 
    ## Resampling results:
    ## 
    ##   ROC        Sens       Spec     
    ##   0.9795831  0.9888064  0.8880512

``` r
glm_pred_prob <- predict(glm_model, test, type = 'prob',na.action=na.pass)
glm.ROC <- roc(response = test$Class,
               predictor = glm_pred_prob$benign,
               levels = levels(test$Class))
plot(glm.ROC, type="S", col="red"); text(x=0, y=.25, labels=paste("AUC =", round(glm.ROC$auc, 4)))
```

![](Day038_files/figure-markdown_github/unnamed-chunk-12-1.png)

Day016
================

<img src="data/log_reg_picture.png" alt="A caption" width="100%" />
<p class="caption">
A caption
</p>

Packages loading

``` r
library(purrr)
library(plyr)
library(tidyverse)
library(caret)
```

Data loading

``` r
app_train <- read.csv("data/application_train.csv")
app_test <- read.csv("data/application_test.csv")
```

Data cleaning

``` r
# Create an anomalous flag column
app_train$DAYS_EMPLOYED_ANOM <- app_train$DAYS_EMPLOYED == 365243
app_train$DAYS_EMPLOYED[which(app_train$DAYS_EMPLOYED == 365243)] <- NA

# also apply to testing dataset
app_test$DAYS_EMPLOYED_ANOM <- app_test$DAYS_EMPLOYED == 365243
app_test$DAYS_EMPLOYED[which(app_test$DAYS_EMPLOYED == 365243)] <- NA

# absolute the value of DAYS_BIRTH
app_train$DAYS_BIRTH <- abs(app_train$DAYS_BIRTH)
app_test$DAYS_BIRTH <- abs(app_test$DAYS_BIRTH)

# absolute the value of DAYS_EMPLOYED
app_train$DAYS_EMPLOYED <- abs(app_train$DAYS_EMPLOYED)
app_test$DAYS_EMPLOYED <- abs(app_test$DAYS_EMPLOYED)
```

Make sure training and testing data have the same levels in each feature

``` r
train_labels <- app_train$TARGET
train <- app_train %>% select(-c("TARGET", "DAYS_EMPLOYED_ANOM"))
test <- app_test %>% select(-c("DAYS_EMPLOYED_ANOM"))

for(attr in colnames(train)) {
  if (is.factor(train[[attr]])) {
    new.levels <- setdiff(levels(train[[attr]]), levels(test[[attr]]))
    if ( length(new.levels) == 0 )
    { print(paste(attr, '- no new levels')) }
    else
    {
      print(c(paste(attr, length(new.levels), 'of new levels, e.g.'), head(new.levels, 2)))
      levels(test[[attr]]) <- union(levels(test[[attr]]), levels(train[[attr]]))
    }
  }
}
```

    ## [1] "NAME_CONTRACT_TYPE - no new levels"
    ## [1] "CODE_GENDER 1 of new levels, e.g." "XNA"                              
    ## [1] "FLAG_OWN_CAR - no new levels"
    ## [1] "FLAG_OWN_REALTY - no new levels"
    ## [1] "NAME_TYPE_SUITE - no new levels"
    ## [1] "NAME_INCOME_TYPE 1 of new levels, e.g."
    ## [2] "Maternity leave"                       
    ## [1] "NAME_EDUCATION_TYPE - no new levels"
    ## [1] "NAME_FAMILY_STATUS 1 of new levels, e.g."
    ## [2] "Unknown"                                 
    ## [1] "NAME_HOUSING_TYPE - no new levels"
    ## [1] "OCCUPATION_TYPE - no new levels"
    ## [1] "WEEKDAY_APPR_PROCESS_START - no new levels"
    ## [1] "ORGANIZATION_TYPE - no new levels"
    ## [1] "FONDKAPREMONT_MODE - no new levels"
    ## [1] "HOUSETYPE_MODE - no new levels"
    ## [1] "WALLSMATERIAL_MODE - no new levels"
    ## [1] "EMERGENCYSTATE_MODE - no new levels"

Label encoding

``` r
feature.names <- colnames(train)
count <- 0

# Iterate through the columns
for (f in feature.names) {
    if (class(train[[f]]) == 'factor') {
        if (nlevels(train[[f]]) <= 2) {
        levels <- list(train[[f]], test[[f]]) %>% unlist() %>% levels()
        train[[f]] <- mapvalues(train[[f]], from=levels, to=c(1,2)) %>% as.integer()
        test[[f]] <- mapvalues(test[[f]], from=levels, to=c(1,2)) %>% as.integer()
        count <- count + 1
        }
    }
}

print(paste(count,"labels were label encoded."))
```

    ## [1] "3 labels were label encoded."

``` r
for (f in feature.names) {
  if (class(train[[f]]) == 'factor') {
    print(f)
  }
}
```

    ## [1] "CODE_GENDER"
    ## [1] "NAME_TYPE_SUITE"
    ## [1] "NAME_INCOME_TYPE"
    ## [1] "NAME_EDUCATION_TYPE"
    ## [1] "NAME_FAMILY_STATUS"
    ## [1] "NAME_HOUSING_TYPE"
    ## [1] "OCCUPATION_TYPE"
    ## [1] "WEEKDAY_APPR_PROCESS_START"
    ## [1] "ORGANIZATION_TYPE"
    ## [1] "FONDKAPREMONT_MODE"
    ## [1] "HOUSETYPE_MODE"
    ## [1] "WALLSMATERIAL_MODE"
    ## [1] "EMERGENCYSTATE_MODE"

One-hot-encoding

``` r
train_dmy <- dummyVars("~.", data = train)
train <- data.frame(predict(train_dmy, newdata = train))
test_dmy <- dummyVars("~.", data = test)
test <- data.frame(predict(test_dmy, newdata = test))

head(train)
```

    ##   SK_ID_CURR NAME_CONTRACT_TYPE CODE_GENDER.F CODE_GENDER.M
    ## 1     100002                  1             0             1
    ## 2     100003                  1             1             0
    ## 3     100004                  2             0             1
    ## 4     100006                  1             1             0
    ## 5     100007                  1             0             1
    ## 6     100008                  1             0             1
    ##   CODE_GENDER.XNA FLAG_OWN_CAR FLAG_OWN_REALTY CNT_CHILDREN
    ## 1               0            1               2            0
    ## 2               0            1               1            0
    ## 3               0            2               2            0
    ## 4               0            1               2            0
    ## 5               0            1               2            0
    ## 6               0            1               2            0
    ##   AMT_INCOME_TOTAL AMT_CREDIT AMT_ANNUITY AMT_GOODS_PRICE NAME_TYPE_SUITE.
    ## 1           202500   406597.5     24700.5          351000                0
    ## 2           270000  1293502.5     35698.5         1129500                0
    ## 3            67500   135000.0      6750.0          135000                0
    ## 4           135000   312682.5     29686.5          297000                0
    ## 5           121500   513000.0     21865.5          513000                0
    ## 6            99000   490495.5     27517.5          454500                0
    ##   NAME_TYPE_SUITE.Children NAME_TYPE_SUITE.Family
    ## 1                        0                      0
    ## 2                        0                      1
    ## 3                        0                      0
    ## 4                        0                      0
    ## 5                        0                      0
    ## 6                        0                      0
    ##   NAME_TYPE_SUITE.Group.of.people NAME_TYPE_SUITE.Other_A
    ## 1                               0                       0
    ## 2                               0                       0
    ## 3                               0                       0
    ## 4                               0                       0
    ## 5                               0                       0
    ## 6                               0                       0
    ##   NAME_TYPE_SUITE.Other_B NAME_TYPE_SUITE.Spouse..partner
    ## 1                       0                               0
    ## 2                       0                               0
    ## 3                       0                               0
    ## 4                       0                               0
    ## 5                       0                               0
    ## 6                       0                               1
    ##   NAME_TYPE_SUITE.Unaccompanied NAME_INCOME_TYPE.Businessman
    ## 1                             1                            0
    ## 2                             0                            0
    ## 3                             1                            0
    ## 4                             1                            0
    ## 5                             1                            0
    ## 6                             0                            0
    ##   NAME_INCOME_TYPE.Commercial.associate NAME_INCOME_TYPE.Maternity.leave
    ## 1                                     0                                0
    ## 2                                     0                                0
    ## 3                                     0                                0
    ## 4                                     0                                0
    ## 5                                     0                                0
    ## 6                                     0                                0
    ##   NAME_INCOME_TYPE.Pensioner NAME_INCOME_TYPE.State.servant
    ## 1                          0                              0
    ## 2                          0                              1
    ## 3                          0                              0
    ## 4                          0                              0
    ## 5                          0                              0
    ## 6                          0                              1
    ##   NAME_INCOME_TYPE.Student NAME_INCOME_TYPE.Unemployed
    ## 1                        0                           0
    ## 2                        0                           0
    ## 3                        0                           0
    ## 4                        0                           0
    ## 5                        0                           0
    ## 6                        0                           0
    ##   NAME_INCOME_TYPE.Working NAME_EDUCATION_TYPE.Academic.degree
    ## 1                        1                                   0
    ## 2                        0                                   0
    ## 3                        1                                   0
    ## 4                        1                                   0
    ## 5                        1                                   0
    ## 6                        0                                   0
    ##   NAME_EDUCATION_TYPE.Higher.education
    ## 1                                    0
    ## 2                                    1
    ## 3                                    0
    ## 4                                    0
    ## 5                                    0
    ## 6                                    0
    ##   NAME_EDUCATION_TYPE.Incomplete.higher
    ## 1                                     0
    ## 2                                     0
    ## 3                                     0
    ## 4                                     0
    ## 5                                     0
    ## 6                                     0
    ##   NAME_EDUCATION_TYPE.Lower.secondary
    ## 1                                   0
    ## 2                                   0
    ## 3                                   0
    ## 4                                   0
    ## 5                                   0
    ## 6                                   0
    ##   NAME_EDUCATION_TYPE.Secondary...secondary.special
    ## 1                                                 1
    ## 2                                                 0
    ## 3                                                 1
    ## 4                                                 1
    ## 5                                                 1
    ## 6                                                 1
    ##   NAME_FAMILY_STATUS.Civil.marriage NAME_FAMILY_STATUS.Married
    ## 1                                 0                          0
    ## 2                                 0                          1
    ## 3                                 0                          0
    ## 4                                 1                          0
    ## 5                                 0                          0
    ## 6                                 0                          1
    ##   NAME_FAMILY_STATUS.Separated NAME_FAMILY_STATUS.Single...not.married
    ## 1                            0                                       1
    ## 2                            0                                       0
    ## 3                            0                                       1
    ## 4                            0                                       0
    ## 5                            0                                       1
    ## 6                            0                                       0
    ##   NAME_FAMILY_STATUS.Unknown NAME_FAMILY_STATUS.Widow
    ## 1                          0                        0
    ## 2                          0                        0
    ## 3                          0                        0
    ## 4                          0                        0
    ## 5                          0                        0
    ## 6                          0                        0
    ##   NAME_HOUSING_TYPE.Co.op.apartment NAME_HOUSING_TYPE.House...apartment
    ## 1                                 0                                   1
    ## 2                                 0                                   1
    ## 3                                 0                                   1
    ## 4                                 0                                   1
    ## 5                                 0                                   1
    ## 6                                 0                                   1
    ##   NAME_HOUSING_TYPE.Municipal.apartment NAME_HOUSING_TYPE.Office.apartment
    ## 1                                     0                                  0
    ## 2                                     0                                  0
    ## 3                                     0                                  0
    ## 4                                     0                                  0
    ## 5                                     0                                  0
    ## 6                                     0                                  0
    ##   NAME_HOUSING_TYPE.Rented.apartment NAME_HOUSING_TYPE.With.parents
    ## 1                                  0                              0
    ## 2                                  0                              0
    ## 3                                  0                              0
    ## 4                                  0                              0
    ## 5                                  0                              0
    ## 6                                  0                              0
    ##   REGION_POPULATION_RELATIVE DAYS_BIRTH DAYS_EMPLOYED DAYS_REGISTRATION
    ## 1                   0.018801       9461           637             -3648
    ## 2                   0.003541      16765          1188             -1186
    ## 3                   0.010032      19046           225             -4260
    ## 4                   0.008019      19005          3039             -9833
    ## 5                   0.028663      19932          3038             -4311
    ## 6                   0.035792      16941          1588             -4970
    ##   DAYS_ID_PUBLISH OWN_CAR_AGE FLAG_MOBIL FLAG_EMP_PHONE FLAG_WORK_PHONE
    ## 1           -2120          NA          1              1               0
    ## 2            -291          NA          1              1               0
    ## 3           -2531          26          1              1               1
    ## 4           -2437          NA          1              1               0
    ## 5           -3458          NA          1              1               0
    ## 6            -477          NA          1              1               1
    ##   FLAG_CONT_MOBILE FLAG_PHONE FLAG_EMAIL OCCUPATION_TYPE.
    ## 1                1          1          0                0
    ## 2                1          1          0                0
    ## 3                1          1          0                0
    ## 4                1          0          0                0
    ## 5                1          0          0                0
    ## 6                1          1          0                0
    ##   OCCUPATION_TYPE.Accountants OCCUPATION_TYPE.Cleaning.staff
    ## 1                           0                              0
    ## 2                           0                              0
    ## 3                           0                              0
    ## 4                           0                              0
    ## 5                           0                              0
    ## 6                           0                              0
    ##   OCCUPATION_TYPE.Cooking.staff OCCUPATION_TYPE.Core.staff
    ## 1                             0                          0
    ## 2                             0                          1
    ## 3                             0                          0
    ## 4                             0                          0
    ## 5                             0                          1
    ## 6                             0                          0
    ##   OCCUPATION_TYPE.Drivers OCCUPATION_TYPE.High.skill.tech.staff
    ## 1                       0                                     0
    ## 2                       0                                     0
    ## 3                       0                                     0
    ## 4                       0                                     0
    ## 5                       0                                     0
    ## 6                       0                                     0
    ##   OCCUPATION_TYPE.HR.staff OCCUPATION_TYPE.IT.staff
    ## 1                        0                        0
    ## 2                        0                        0
    ## 3                        0                        0
    ## 4                        0                        0
    ## 5                        0                        0
    ## 6                        0                        0
    ##   OCCUPATION_TYPE.Laborers OCCUPATION_TYPE.Low.skill.Laborers
    ## 1                        1                                  0
    ## 2                        0                                  0
    ## 3                        1                                  0
    ## 4                        1                                  0
    ## 5                        0                                  0
    ## 6                        1                                  0
    ##   OCCUPATION_TYPE.Managers OCCUPATION_TYPE.Medicine.staff
    ## 1                        0                              0
    ## 2                        0                              0
    ## 3                        0                              0
    ## 4                        0                              0
    ## 5                        0                              0
    ## 6                        0                              0
    ##   OCCUPATION_TYPE.Private.service.staff OCCUPATION_TYPE.Realty.agents
    ## 1                                     0                             0
    ## 2                                     0                             0
    ## 3                                     0                             0
    ## 4                                     0                             0
    ## 5                                     0                             0
    ## 6                                     0                             0
    ##   OCCUPATION_TYPE.Sales.staff OCCUPATION_TYPE.Secretaries
    ## 1                           0                           0
    ## 2                           0                           0
    ## 3                           0                           0
    ## 4                           0                           0
    ## 5                           0                           0
    ## 6                           0                           0
    ##   OCCUPATION_TYPE.Security.staff OCCUPATION_TYPE.Waiters.barmen.staff
    ## 1                              0                                    0
    ## 2                              0                                    0
    ## 3                              0                                    0
    ## 4                              0                                    0
    ## 5                              0                                    0
    ## 6                              0                                    0
    ##   CNT_FAM_MEMBERS REGION_RATING_CLIENT REGION_RATING_CLIENT_W_CITY
    ## 1               1                    2                           2
    ## 2               2                    1                           1
    ## 3               1                    2                           2
    ## 4               2                    2                           2
    ## 5               1                    2                           2
    ## 6               2                    2                           2
    ##   WEEKDAY_APPR_PROCESS_START.FRIDAY WEEKDAY_APPR_PROCESS_START.MONDAY
    ## 1                                 0                                 0
    ## 2                                 0                                 1
    ## 3                                 0                                 1
    ## 4                                 0                                 0
    ## 5                                 0                                 0
    ## 6                                 0                                 0
    ##   WEEKDAY_APPR_PROCESS_START.SATURDAY WEEKDAY_APPR_PROCESS_START.SUNDAY
    ## 1                                   0                                 0
    ## 2                                   0                                 0
    ## 3                                   0                                 0
    ## 4                                   0                                 0
    ## 5                                   0                                 0
    ## 6                                   0                                 0
    ##   WEEKDAY_APPR_PROCESS_START.THURSDAY WEEKDAY_APPR_PROCESS_START.TUESDAY
    ## 1                                   0                                  0
    ## 2                                   0                                  0
    ## 3                                   0                                  0
    ## 4                                   0                                  0
    ## 5                                   1                                  0
    ## 6                                   0                                  0
    ##   WEEKDAY_APPR_PROCESS_START.WEDNESDAY HOUR_APPR_PROCESS_START
    ## 1                                    1                      10
    ## 2                                    0                      11
    ## 3                                    0                       9
    ## 4                                    1                      17
    ## 5                                    0                      11
    ## 6                                    1                      16
    ##   REG_REGION_NOT_LIVE_REGION REG_REGION_NOT_WORK_REGION
    ## 1                          0                          0
    ## 2                          0                          0
    ## 3                          0                          0
    ## 4                          0                          0
    ## 5                          0                          0
    ## 6                          0                          0
    ##   LIVE_REGION_NOT_WORK_REGION REG_CITY_NOT_LIVE_CITY
    ## 1                           0                      0
    ## 2                           0                      0
    ## 3                           0                      0
    ## 4                           0                      0
    ## 5                           0                      0
    ## 6                           0                      0
    ##   REG_CITY_NOT_WORK_CITY LIVE_CITY_NOT_WORK_CITY
    ## 1                      0                       0
    ## 2                      0                       0
    ## 3                      0                       0
    ## 4                      0                       0
    ## 5                      1                       1
    ## 6                      0                       0
    ##   ORGANIZATION_TYPE.Advertising ORGANIZATION_TYPE.Agriculture
    ## 1                             0                             0
    ## 2                             0                             0
    ## 3                             0                             0
    ## 4                             0                             0
    ## 5                             0                             0
    ## 6                             0                             0
    ##   ORGANIZATION_TYPE.Bank ORGANIZATION_TYPE.Business.Entity.Type.1
    ## 1                      0                                        0
    ## 2                      0                                        0
    ## 3                      0                                        0
    ## 4                      0                                        0
    ## 5                      0                                        0
    ## 6                      0                                        0
    ##   ORGANIZATION_TYPE.Business.Entity.Type.2
    ## 1                                        0
    ## 2                                        0
    ## 3                                        0
    ## 4                                        0
    ## 5                                        0
    ## 6                                        0
    ##   ORGANIZATION_TYPE.Business.Entity.Type.3 ORGANIZATION_TYPE.Cleaning
    ## 1                                        1                          0
    ## 2                                        0                          0
    ## 3                                        0                          0
    ## 4                                        1                          0
    ## 5                                        0                          0
    ## 6                                        0                          0
    ##   ORGANIZATION_TYPE.Construction ORGANIZATION_TYPE.Culture
    ## 1                              0                         0
    ## 2                              0                         0
    ## 3                              0                         0
    ## 4                              0                         0
    ## 5                              0                         0
    ## 6                              0                         0
    ##   ORGANIZATION_TYPE.Electricity ORGANIZATION_TYPE.Emergency
    ## 1                             0                           0
    ## 2                             0                           0
    ## 3                             0                           0
    ## 4                             0                           0
    ## 5                             0                           0
    ## 6                             0                           0
    ##   ORGANIZATION_TYPE.Government ORGANIZATION_TYPE.Hotel
    ## 1                            0                       0
    ## 2                            0                       0
    ## 3                            1                       0
    ## 4                            0                       0
    ## 5                            0                       0
    ## 6                            0                       0
    ##   ORGANIZATION_TYPE.Housing ORGANIZATION_TYPE.Industry..type.1
    ## 1                         0                                  0
    ## 2                         0                                  0
    ## 3                         0                                  0
    ## 4                         0                                  0
    ## 5                         0                                  0
    ## 6                         0                                  0
    ##   ORGANIZATION_TYPE.Industry..type.10 ORGANIZATION_TYPE.Industry..type.11
    ## 1                                   0                                   0
    ## 2                                   0                                   0
    ## 3                                   0                                   0
    ## 4                                   0                                   0
    ## 5                                   0                                   0
    ## 6                                   0                                   0
    ##   ORGANIZATION_TYPE.Industry..type.12 ORGANIZATION_TYPE.Industry..type.13
    ## 1                                   0                                   0
    ## 2                                   0                                   0
    ## 3                                   0                                   0
    ## 4                                   0                                   0
    ## 5                                   0                                   0
    ## 6                                   0                                   0
    ##   ORGANIZATION_TYPE.Industry..type.2 ORGANIZATION_TYPE.Industry..type.3
    ## 1                                  0                                  0
    ## 2                                  0                                  0
    ## 3                                  0                                  0
    ## 4                                  0                                  0
    ## 5                                  0                                  0
    ## 6                                  0                                  0
    ##   ORGANIZATION_TYPE.Industry..type.4 ORGANIZATION_TYPE.Industry..type.5
    ## 1                                  0                                  0
    ## 2                                  0                                  0
    ## 3                                  0                                  0
    ## 4                                  0                                  0
    ## 5                                  0                                  0
    ## 6                                  0                                  0
    ##   ORGANIZATION_TYPE.Industry..type.6 ORGANIZATION_TYPE.Industry..type.7
    ## 1                                  0                                  0
    ## 2                                  0                                  0
    ## 3                                  0                                  0
    ## 4                                  0                                  0
    ## 5                                  0                                  0
    ## 6                                  0                                  0
    ##   ORGANIZATION_TYPE.Industry..type.8 ORGANIZATION_TYPE.Industry..type.9
    ## 1                                  0                                  0
    ## 2                                  0                                  0
    ## 3                                  0                                  0
    ## 4                                  0                                  0
    ## 5                                  0                                  0
    ## 6                                  0                                  0
    ##   ORGANIZATION_TYPE.Insurance ORGANIZATION_TYPE.Kindergarten
    ## 1                           0                              0
    ## 2                           0                              0
    ## 3                           0                              0
    ## 4                           0                              0
    ## 5                           0                              0
    ## 6                           0                              0
    ##   ORGANIZATION_TYPE.Legal.Services ORGANIZATION_TYPE.Medicine
    ## 1                                0                          0
    ## 2                                0                          0
    ## 3                                0                          0
    ## 4                                0                          0
    ## 5                                0                          0
    ## 6                                0                          0
    ##   ORGANIZATION_TYPE.Military ORGANIZATION_TYPE.Mobile
    ## 1                          0                        0
    ## 2                          0                        0
    ## 3                          0                        0
    ## 4                          0                        0
    ## 5                          0                        0
    ## 6                          0                        0
    ##   ORGANIZATION_TYPE.Other ORGANIZATION_TYPE.Police
    ## 1                       0                        0
    ## 2                       0                        0
    ## 3                       0                        0
    ## 4                       0                        0
    ## 5                       0                        0
    ## 6                       1                        0
    ##   ORGANIZATION_TYPE.Postal ORGANIZATION_TYPE.Realtor
    ## 1                        0                         0
    ## 2                        0                         0
    ## 3                        0                         0
    ## 4                        0                         0
    ## 5                        0                         0
    ## 6                        0                         0
    ##   ORGANIZATION_TYPE.Religion ORGANIZATION_TYPE.Restaurant
    ## 1                          0                            0
    ## 2                          0                            0
    ## 3                          0                            0
    ## 4                          0                            0
    ## 5                          1                            0
    ## 6                          0                            0
    ##   ORGANIZATION_TYPE.School ORGANIZATION_TYPE.Security
    ## 1                        0                          0
    ## 2                        1                          0
    ## 3                        0                          0
    ## 4                        0                          0
    ## 5                        0                          0
    ## 6                        0                          0
    ##   ORGANIZATION_TYPE.Security.Ministries ORGANIZATION_TYPE.Self.employed
    ## 1                                     0                               0
    ## 2                                     0                               0
    ## 3                                     0                               0
    ## 4                                     0                               0
    ## 5                                     0                               0
    ## 6                                     0                               0
    ##   ORGANIZATION_TYPE.Services ORGANIZATION_TYPE.Telecom
    ## 1                          0                         0
    ## 2                          0                         0
    ## 3                          0                         0
    ## 4                          0                         0
    ## 5                          0                         0
    ## 6                          0                         0
    ##   ORGANIZATION_TYPE.Trade..type.1 ORGANIZATION_TYPE.Trade..type.2
    ## 1                               0                               0
    ## 2                               0                               0
    ## 3                               0                               0
    ## 4                               0                               0
    ## 5                               0                               0
    ## 6                               0                               0
    ##   ORGANIZATION_TYPE.Trade..type.3 ORGANIZATION_TYPE.Trade..type.4
    ## 1                               0                               0
    ## 2                               0                               0
    ## 3                               0                               0
    ## 4                               0                               0
    ## 5                               0                               0
    ## 6                               0                               0
    ##   ORGANIZATION_TYPE.Trade..type.5 ORGANIZATION_TYPE.Trade..type.6
    ## 1                               0                               0
    ## 2                               0                               0
    ## 3                               0                               0
    ## 4                               0                               0
    ## 5                               0                               0
    ## 6                               0                               0
    ##   ORGANIZATION_TYPE.Trade..type.7 ORGANIZATION_TYPE.Transport..type.1
    ## 1                               0                                   0
    ## 2                               0                                   0
    ## 3                               0                                   0
    ## 4                               0                                   0
    ## 5                               0                                   0
    ## 6                               0                                   0
    ##   ORGANIZATION_TYPE.Transport..type.2 ORGANIZATION_TYPE.Transport..type.3
    ## 1                                   0                                   0
    ## 2                                   0                                   0
    ## 3                                   0                                   0
    ## 4                                   0                                   0
    ## 5                                   0                                   0
    ## 6                                   0                                   0
    ##   ORGANIZATION_TYPE.Transport..type.4 ORGANIZATION_TYPE.University
    ## 1                                   0                            0
    ## 2                                   0                            0
    ## 3                                   0                            0
    ## 4                                   0                            0
    ## 5                                   0                            0
    ## 6                                   0                            0
    ##   ORGANIZATION_TYPE.XNA EXT_SOURCE_1 EXT_SOURCE_2 EXT_SOURCE_3
    ## 1                     0   0.08303697    0.2629486    0.1393758
    ## 2                     0   0.31126731    0.6222458           NA
    ## 3                     0           NA    0.5559121    0.7295667
    ## 4                     0           NA    0.6504417           NA
    ## 5                     0           NA    0.3227383           NA
    ## 6                     0           NA    0.3542247    0.6212263
    ##   APARTMENTS_AVG BASEMENTAREA_AVG YEARS_BEGINEXPLUATATION_AVG
    ## 1         0.0247           0.0369                      0.9722
    ## 2         0.0959           0.0529                      0.9851
    ## 3             NA               NA                          NA
    ## 4             NA               NA                          NA
    ## 5             NA               NA                          NA
    ## 6             NA               NA                          NA
    ##   YEARS_BUILD_AVG COMMONAREA_AVG ELEVATORS_AVG ENTRANCES_AVG FLOORSMAX_AVG
    ## 1          0.6192         0.0143          0.00        0.0690        0.0833
    ## 2          0.7960         0.0605          0.08        0.0345        0.2917
    ## 3              NA             NA            NA            NA            NA
    ## 4              NA             NA            NA            NA            NA
    ## 5              NA             NA            NA            NA            NA
    ## 6              NA             NA            NA            NA            NA
    ##   FLOORSMIN_AVG LANDAREA_AVG LIVINGAPARTMENTS_AVG LIVINGAREA_AVG
    ## 1        0.1250       0.0369               0.0202         0.0190
    ## 2        0.3333       0.0130               0.0773         0.0549
    ## 3            NA           NA                   NA             NA
    ## 4            NA           NA                   NA             NA
    ## 5            NA           NA                   NA             NA
    ## 6            NA           NA                   NA             NA
    ##   NONLIVINGAPARTMENTS_AVG NONLIVINGAREA_AVG APARTMENTS_MODE
    ## 1                  0.0000            0.0000          0.0252
    ## 2                  0.0039            0.0098          0.0924
    ## 3                      NA                NA              NA
    ## 4                      NA                NA              NA
    ## 5                      NA                NA              NA
    ## 6                      NA                NA              NA
    ##   BASEMENTAREA_MODE YEARS_BEGINEXPLUATATION_MODE YEARS_BUILD_MODE
    ## 1            0.0383                       0.9722           0.6341
    ## 2            0.0538                       0.9851           0.8040
    ## 3                NA                           NA               NA
    ## 4                NA                           NA               NA
    ## 5                NA                           NA               NA
    ## 6                NA                           NA               NA
    ##   COMMONAREA_MODE ELEVATORS_MODE ENTRANCES_MODE FLOORSMAX_MODE
    ## 1          0.0144         0.0000         0.0690         0.0833
    ## 2          0.0497         0.0806         0.0345         0.2917
    ## 3              NA             NA             NA             NA
    ## 4              NA             NA             NA             NA
    ## 5              NA             NA             NA             NA
    ## 6              NA             NA             NA             NA
    ##   FLOORSMIN_MODE LANDAREA_MODE LIVINGAPARTMENTS_MODE LIVINGAREA_MODE
    ## 1         0.1250        0.0377                 0.022          0.0198
    ## 2         0.3333        0.0128                 0.079          0.0554
    ## 3             NA            NA                    NA              NA
    ## 4             NA            NA                    NA              NA
    ## 5             NA            NA                    NA              NA
    ## 6             NA            NA                    NA              NA
    ##   NONLIVINGAPARTMENTS_MODE NONLIVINGAREA_MODE APARTMENTS_MEDI
    ## 1                        0                  0          0.0250
    ## 2                        0                  0          0.0968
    ## 3                       NA                 NA              NA
    ## 4                       NA                 NA              NA
    ## 5                       NA                 NA              NA
    ## 6                       NA                 NA              NA
    ##   BASEMENTAREA_MEDI YEARS_BEGINEXPLUATATION_MEDI YEARS_BUILD_MEDI
    ## 1            0.0369                       0.9722           0.6243
    ## 2            0.0529                       0.9851           0.7987
    ## 3                NA                           NA               NA
    ## 4                NA                           NA               NA
    ## 5                NA                           NA               NA
    ## 6                NA                           NA               NA
    ##   COMMONAREA_MEDI ELEVATORS_MEDI ENTRANCES_MEDI FLOORSMAX_MEDI
    ## 1          0.0144           0.00         0.0690         0.0833
    ## 2          0.0608           0.08         0.0345         0.2917
    ## 3              NA             NA             NA             NA
    ## 4              NA             NA             NA             NA
    ## 5              NA             NA             NA             NA
    ## 6              NA             NA             NA             NA
    ##   FLOORSMIN_MEDI LANDAREA_MEDI LIVINGAPARTMENTS_MEDI LIVINGAREA_MEDI
    ## 1         0.1250        0.0375                0.0205          0.0193
    ## 2         0.3333        0.0132                0.0787          0.0558
    ## 3             NA            NA                    NA              NA
    ## 4             NA            NA                    NA              NA
    ## 5             NA            NA                    NA              NA
    ## 6             NA            NA                    NA              NA
    ##   NONLIVINGAPARTMENTS_MEDI NONLIVINGAREA_MEDI FONDKAPREMONT_MODE.
    ## 1                   0.0000               0.00                   0
    ## 2                   0.0039               0.01                   0
    ## 3                       NA                 NA                   1
    ## 4                       NA                 NA                   1
    ## 5                       NA                 NA                   1
    ## 6                       NA                 NA                   1
    ##   FONDKAPREMONT_MODE.not.specified FONDKAPREMONT_MODE.org.spec.account
    ## 1                                0                                   0
    ## 2                                0                                   0
    ## 3                                0                                   0
    ## 4                                0                                   0
    ## 5                                0                                   0
    ## 6                                0                                   0
    ##   FONDKAPREMONT_MODE.reg.oper.account
    ## 1                                   1
    ## 2                                   1
    ## 3                                   0
    ## 4                                   0
    ## 5                                   0
    ## 6                                   0
    ##   FONDKAPREMONT_MODE.reg.oper.spec.account HOUSETYPE_MODE.
    ## 1                                        0               0
    ## 2                                        0               0
    ## 3                                        0               1
    ## 4                                        0               1
    ## 5                                        0               1
    ## 6                                        0               1
    ##   HOUSETYPE_MODE.block.of.flats HOUSETYPE_MODE.specific.housing
    ## 1                             1                               0
    ## 2                             1                               0
    ## 3                             0                               0
    ## 4                             0                               0
    ## 5                             0                               0
    ## 6                             0                               0
    ##   HOUSETYPE_MODE.terraced.house TOTALAREA_MODE WALLSMATERIAL_MODE.
    ## 1                             0         0.0149                   0
    ## 2                             0         0.0714                   0
    ## 3                             0             NA                   1
    ## 4                             0             NA                   1
    ## 5                             0             NA                   1
    ## 6                             0             NA                   1
    ##   WALLSMATERIAL_MODE.Block WALLSMATERIAL_MODE.Mixed
    ## 1                        0                        0
    ## 2                        1                        0
    ## 3                        0                        0
    ## 4                        0                        0
    ## 5                        0                        0
    ## 6                        0                        0
    ##   WALLSMATERIAL_MODE.Monolithic WALLSMATERIAL_MODE.Others
    ## 1                             0                         0
    ## 2                             0                         0
    ## 3                             0                         0
    ## 4                             0                         0
    ## 5                             0                         0
    ## 6                             0                         0
    ##   WALLSMATERIAL_MODE.Panel WALLSMATERIAL_MODE.Stone..brick
    ## 1                        0                               1
    ## 2                        0                               0
    ## 3                        0                               0
    ## 4                        0                               0
    ## 5                        0                               0
    ## 6                        0                               0
    ##   WALLSMATERIAL_MODE.Wooden EMERGENCYSTATE_MODE. EMERGENCYSTATE_MODE.No
    ## 1                         0                    0                      1
    ## 2                         0                    0                      1
    ## 3                         0                    1                      0
    ## 4                         0                    1                      0
    ## 5                         0                    1                      0
    ## 6                         0                    1                      0
    ##   EMERGENCYSTATE_MODE.Yes OBS_30_CNT_SOCIAL_CIRCLE
    ## 1                       0                        2
    ## 2                       0                        1
    ## 3                       0                        0
    ## 4                       0                        2
    ## 5                       0                        0
    ## 6                       0                        0
    ##   DEF_30_CNT_SOCIAL_CIRCLE OBS_60_CNT_SOCIAL_CIRCLE
    ## 1                        2                        2
    ## 2                        0                        1
    ## 3                        0                        0
    ## 4                        0                        2
    ## 5                        0                        0
    ## 6                        0                        0
    ##   DEF_60_CNT_SOCIAL_CIRCLE DAYS_LAST_PHONE_CHANGE FLAG_DOCUMENT_2
    ## 1                        2                  -1134               0
    ## 2                        0                   -828               0
    ## 3                        0                   -815               0
    ## 4                        0                   -617               0
    ## 5                        0                  -1106               0
    ## 6                        0                  -2536               0
    ##   FLAG_DOCUMENT_3 FLAG_DOCUMENT_4 FLAG_DOCUMENT_5 FLAG_DOCUMENT_6
    ## 1               1               0               0               0
    ## 2               1               0               0               0
    ## 3               0               0               0               0
    ## 4               1               0               0               0
    ## 5               0               0               0               0
    ## 6               1               0               0               0
    ##   FLAG_DOCUMENT_7 FLAG_DOCUMENT_8 FLAG_DOCUMENT_9 FLAG_DOCUMENT_10
    ## 1               0               0               0                0
    ## 2               0               0               0                0
    ## 3               0               0               0                0
    ## 4               0               0               0                0
    ## 5               0               1               0                0
    ## 6               0               0               0                0
    ##   FLAG_DOCUMENT_11 FLAG_DOCUMENT_12 FLAG_DOCUMENT_13 FLAG_DOCUMENT_14
    ## 1                0                0                0                0
    ## 2                0                0                0                0
    ## 3                0                0                0                0
    ## 4                0                0                0                0
    ## 5                0                0                0                0
    ## 6                0                0                0                0
    ##   FLAG_DOCUMENT_15 FLAG_DOCUMENT_16 FLAG_DOCUMENT_17 FLAG_DOCUMENT_18
    ## 1                0                0                0                0
    ## 2                0                0                0                0
    ## 3                0                0                0                0
    ## 4                0                0                0                0
    ## 5                0                0                0                0
    ## 6                0                0                0                0
    ##   FLAG_DOCUMENT_19 FLAG_DOCUMENT_20 FLAG_DOCUMENT_21
    ## 1                0                0                0
    ## 2                0                0                0
    ## 3                0                0                0
    ## 4                0                0                0
    ## 5                0                0                0
    ## 6                0                0                0
    ##   AMT_REQ_CREDIT_BUREAU_HOUR AMT_REQ_CREDIT_BUREAU_DAY
    ## 1                          0                         0
    ## 2                          0                         0
    ## 3                          0                         0
    ## 4                         NA                        NA
    ## 5                          0                         0
    ## 6                          0                         0
    ##   AMT_REQ_CREDIT_BUREAU_WEEK AMT_REQ_CREDIT_BUREAU_MON
    ## 1                          0                         0
    ## 2                          0                         0
    ## 3                          0                         0
    ## 4                         NA                        NA
    ## 5                          0                         0
    ## 6                          0                         0
    ##   AMT_REQ_CREDIT_BUREAU_QRT AMT_REQ_CREDIT_BUREAU_YEAR
    ## 1                         0                          1
    ## 2                         0                          0
    ## 3                         0                          0
    ## 4                        NA                         NA
    ## 5                         0                          0
    ## 6                         1                          1

``` r
#Median imputation of missing values
for (i in which(map(train, anyNA) == TRUE)) {
    if(class(train[[i]]) == "numeric") {
      train[[i]] <- mapvalues(train[[i]], from = NA, to = median(train[[i]], na.rm = TRUE))
    }
}
for (i in which(map(test, anyNA) == TRUE)) {
    if(class(test[[i]]) == "numeric") {
      test[[i]] <- mapvalues(test[[i]], from = NA, to = median(test[[i]], na.rm = TRUE))
    }
}

train$TARGET <- train_labels
```

Model Fitting

``` r
model <- glm(TARGET~., family = binomial(link = "logit"), data = train)
summary(model)
```

    ## 
    ## Call:
    ## glm(formula = TARGET ~ ., family = binomial(link = "logit"), 
    ##     data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.2668  -0.4323  -0.3122  -0.2217   3.4181  
    ## 
    ## Coefficients: (13 not defined because of singularities)
    ##                                                     Estimate Std. Error
    ## (Intercept)                                        2.196e+11  4.890e+11
    ## SK_ID_CURR                                        -5.065e-08  6.727e-08
    ## NAME_CONTRACT_TYPE                                 4.473e-03  6.065e-02
    ## CODE_GENDER.F                                     -2.196e+11  4.890e+11
    ## CODE_GENDER.M                                     -2.196e+11  4.890e+11
    ## CODE_GENDER.XNA                                   -2.196e+11  4.890e+11
    ## FLAG_OWN_CAR                                      -2.840e-01  1.718e-02
    ## FLAG_OWN_REALTY                                    3.177e-02  1.594e-02
    ## CNT_CHILDREN                                       1.983e-02  1.009e-02
    ## AMT_INCOME_TOTAL                                   2.016e-08  2.511e-08
    ## AMT_CREDIT                                         2.380e-06  1.131e-07
    ## AMT_ANNUITY                                        9.268e-06  8.418e-07
    ## AMT_GOODS_PRICE                                   -2.846e-06  1.289e-07
    ## NAME_TYPE_SUITE.                                  -1.952e-01  1.287e-01
    ## NAME_TYPE_SUITE.Children                           3.841e-02  7.005e-02
    ## NAME_TYPE_SUITE.Family                            -2.938e-02  2.134e-02
    ## NAME_TYPE_SUITE.Group.of.people                   -2.809e-02  2.275e-01
    ## NAME_TYPE_SUITE.Other_A                           -9.429e-02  1.260e-01
    ## NAME_TYPE_SUITE.Other_B                            9.725e-02  8.417e-02
    ## NAME_TYPE_SUITE.Spouse..partner                   -6.723e-02  3.745e-02
    ## NAME_TYPE_SUITE.Unaccompanied                             NA         NA
    ## NAME_INCOME_TYPE.Businessman                      -2.275e+01  1.000e+05
    ## NAME_INCOME_TYPE.Commercial.associate             -8.849e-02  1.798e-02
    ## NAME_INCOME_TYPE.Maternity.leave                   3.300e+00  9.439e-01
    ## NAME_INCOME_TYPE.Pensioner                        -2.381e+01  1.118e+05
    ## NAME_INCOME_TYPE.State.servant                    -1.113e-01  3.525e-02
    ## NAME_INCOME_TYPE.Student                          -2.454e+01  7.977e+04
    ## NAME_INCOME_TYPE.Unemployed                       -2.103e+01  1.118e+05
    ## NAME_INCOME_TYPE.Working                                  NA         NA
    ## NAME_EDUCATION_TYPE.Academic.degree               -1.598e+00  5.945e-01
    ## NAME_EDUCATION_TYPE.Higher.education              -2.788e-01  2.030e-02
    ## NAME_EDUCATION_TYPE.Incomplete.higher             -1.924e-01  3.860e-02
    ## NAME_EDUCATION_TYPE.Lower.secondary                9.896e-02  5.515e-02
    ## NAME_EDUCATION_TYPE.Secondary...secondary.special         NA         NA
    ## NAME_FAMILY_STATUS.Civil.marriage                  1.498e-01  4.184e-02
    ## NAME_FAMILY_STATUS.Married                         3.976e-03  3.755e-02
    ## NAME_FAMILY_STATUS.Separated                       1.505e-01  4.488e-02
    ## NAME_FAMILY_STATUS.Single...not.married            1.039e-01  4.086e-02
    ## NAME_FAMILY_STATUS.Unknown                        -2.367e+01  2.570e+05
    ## NAME_FAMILY_STATUS.Widow                                  NA         NA
    ## NAME_HOUSING_TYPE.Co.op.apartment                 -7.523e-02  1.191e-01
    ## NAME_HOUSING_TYPE.House...apartment               -1.237e-02  2.951e-02
    ## NAME_HOUSING_TYPE.Municipal.apartment              1.164e-01  4.556e-02
    ## NAME_HOUSING_TYPE.Office.apartment                -2.197e-01  8.697e-02
    ## NAME_HOUSING_TYPE.Rented.apartment                 4.451e-02  5.391e-02
    ## NAME_HOUSING_TYPE.With.parents                            NA         NA
    ## REGION_POPULATION_RELATIVE                         1.264e+00  6.663e-01
    ## DAYS_BIRTH                                         3.550e-06  2.565e-06
    ## DAYS_EMPLOYED                                     -6.606e-05  4.391e-06
    ## DAYS_REGISTRATION                                  9.897e-06  2.234e-06
    ## DAYS_ID_PUBLISH                                    4.381e-05  4.905e-06
    ## OWN_CAR_AGE                                        4.652e-03  9.746e-04
    ## FLAG_MOBIL                                         2.271e+01  2.682e+05
    ## FLAG_EMP_PHONE                                    -6.528e-01  1.072e+00
    ## FLAG_WORK_PHONE                                    1.830e-01  1.850e-02
    ## FLAG_CONT_MOBILE                                  -2.132e-01  1.680e-01
    ## FLAG_PHONE                                        -6.661e-02  1.722e-02
    ## FLAG_EMAIL                                        -7.989e-02  3.077e-02
    ## OCCUPATION_TYPE.                                  -8.210e-02  9.371e-02
    ## OCCUPATION_TYPE.Accountants                       -2.389e-01  1.040e-01
    ## OCCUPATION_TYPE.Cleaning.staff                     1.171e-02  1.057e-01
    ## OCCUPATION_TYPE.Cooking.staff                     -1.903e-02  1.010e-01
    ## OCCUPATION_TYPE.Core.staff                        -1.511e-01  9.631e-02
    ## OCCUPATION_TYPE.Drivers                            6.271e-02  9.628e-02
    ## OCCUPATION_TYPE.High.skill.tech.staff             -1.751e-01  1.006e-01
    ## OCCUPATION_TYPE.HR.staff                          -9.781e-04  2.013e-01
    ## OCCUPATION_TYPE.IT.staff                          -1.712e-01  2.079e-01
    ## OCCUPATION_TYPE.Laborers                          -6.738e-05  9.337e-02
    ## OCCUPATION_TYPE.Low.skill.Laborers                 1.585e-01  1.110e-01
    ## OCCUPATION_TYPE.Managers                          -8.783e-02  9.701e-02
    ## OCCUPATION_TYPE.Medicine.staff                    -1.228e-01  1.071e-01
    ## OCCUPATION_TYPE.Private.service.staff             -2.072e-01  1.253e-01
    ## OCCUPATION_TYPE.Realty.agents                     -1.433e-01  1.725e-01
    ## OCCUPATION_TYPE.Sales.staff                       -5.775e-02  9.415e-02
    ## OCCUPATION_TYPE.Secretaries                        4.611e-03  1.454e-01
    ## OCCUPATION_TYPE.Security.staff                     6.099e-02  1.046e-01
    ## OCCUPATION_TYPE.Waiters.barmen.staff                      NA         NA
    ## CNT_FAM_MEMBERS                                           NA         NA
    ## REGION_RATING_CLIENT                              -9.697e-02  4.725e-02
    ## REGION_RATING_CLIENT_W_CITY                        2.432e-01  4.745e-02
    ## WEEKDAY_APPR_PROCESS_START.FRIDAY                 -5.614e-03  2.388e-02
    ## WEEKDAY_APPR_PROCESS_START.MONDAY                 -6.428e-02  2.409e-02
    ## WEEKDAY_APPR_PROCESS_START.SATURDAY               -6.520e-02  2.698e-02
    ## WEEKDAY_APPR_PROCESS_START.SUNDAY                 -8.860e-02  3.479e-02
    ## WEEKDAY_APPR_PROCESS_START.THURSDAY               -2.089e-02  2.387e-02
    ## WEEKDAY_APPR_PROCESS_START.TUESDAY                 1.595e-02  2.335e-02
    ## WEEKDAY_APPR_PROCESS_START.WEDNESDAY                      NA         NA
    ## HOUR_APPR_PROCESS_START                           -2.484e-03  2.257e-03
    ## REG_REGION_NOT_LIVE_REGION                        -2.177e-01  9.722e-02
    ## REG_REGION_NOT_WORK_REGION                         8.242e-02  1.034e-01
    ## LIVE_REGION_NOT_WORK_REGION                       -1.529e-01  1.028e-01
    ## REG_CITY_NOT_LIVE_CITY                             1.965e-01  3.545e-02
    ## REG_CITY_NOT_WORK_CITY                            -9.142e-02  3.957e-02
    ## LIVE_CITY_NOT_WORK_CITY                            7.442e-02  3.833e-02
    ## ORGANIZATION_TYPE.Advertising                     -2.277e+01  1.118e+05
    ## ORGANIZATION_TYPE.Agriculture                     -2.284e+01  1.118e+05
    ## ORGANIZATION_TYPE.Bank                            -2.321e+01  1.118e+05
    ## ORGANIZATION_TYPE.Business.Entity.Type.1          -2.297e+01  1.118e+05
    ## ORGANIZATION_TYPE.Business.Entity.Type.2          -2.291e+01  1.118e+05
    ## ORGANIZATION_TYPE.Business.Entity.Type.3          -2.286e+01  1.118e+05
    ## ORGANIZATION_TYPE.Cleaning                        -2.273e+01  1.118e+05
    ## ORGANIZATION_TYPE.Construction                    -2.271e+01  1.118e+05
    ## ORGANIZATION_TYPE.Culture                         -2.295e+01  1.118e+05
    ## ORGANIZATION_TYPE.Electricity                     -2.307e+01  1.118e+05
    ## ORGANIZATION_TYPE.Emergency                       -2.308e+01  1.118e+05
    ## ORGANIZATION_TYPE.Government                      -2.298e+01  1.118e+05
    ## ORGANIZATION_TYPE.Hotel                           -2.316e+01  1.118e+05
    ## ORGANIZATION_TYPE.Housing                         -2.294e+01  1.118e+05
    ## ORGANIZATION_TYPE.Industry..type.1                -2.283e+01  1.118e+05
    ## ORGANIZATION_TYPE.Industry..type.10               -2.327e+01  1.118e+05
    ## ORGANIZATION_TYPE.Industry..type.11               -2.292e+01  1.118e+05
    ## ORGANIZATION_TYPE.Industry..type.12               -2.357e+01  1.118e+05
    ## ORGANIZATION_TYPE.Industry..type.13               -2.292e+01  1.118e+05
    ## ORGANIZATION_TYPE.Industry..type.2                -2.322e+01  1.118e+05
    ## ORGANIZATION_TYPE.Industry..type.3                -2.282e+01  1.118e+05
    ## ORGANIZATION_TYPE.Industry..type.4                -2.295e+01  1.118e+05
    ## ORGANIZATION_TYPE.Industry..type.5                -2.317e+01  1.118e+05
    ## ORGANIZATION_TYPE.Industry..type.6                -2.305e+01  1.118e+05
    ## ORGANIZATION_TYPE.Industry..type.7                -2.299e+01  1.118e+05
    ## ORGANIZATION_TYPE.Industry..type.8                -2.256e+01  1.118e+05
    ## ORGANIZATION_TYPE.Industry..type.9                -2.322e+01  1.118e+05
    ## ORGANIZATION_TYPE.Insurance                       -2.293e+01  1.118e+05
    ## ORGANIZATION_TYPE.Kindergarten                    -2.299e+01  1.118e+05
    ## ORGANIZATION_TYPE.Legal.Services                  -2.241e+01  1.118e+05
    ## ORGANIZATION_TYPE.Medicine                        -2.296e+01  1.118e+05
    ## ORGANIZATION_TYPE.Military                        -2.340e+01  1.118e+05
    ## ORGANIZATION_TYPE.Mobile                          -2.286e+01  1.118e+05
    ## ORGANIZATION_TYPE.Other                           -2.293e+01  1.118e+05
    ## ORGANIZATION_TYPE.Police                          -2.324e+01  1.118e+05
    ## ORGANIZATION_TYPE.Postal                          -2.283e+01  1.118e+05
    ## ORGANIZATION_TYPE.Realtor                         -2.229e+01  1.118e+05
    ## ORGANIZATION_TYPE.Religion                        -2.303e+01  1.118e+05
    ## ORGANIZATION_TYPE.Restaurant                      -2.281e+01  1.118e+05
    ## ORGANIZATION_TYPE.School                          -2.307e+01  1.118e+05
    ## ORGANIZATION_TYPE.Security                        -2.301e+01  1.118e+05
    ## ORGANIZATION_TYPE.Security.Ministries             -2.330e+01  1.118e+05
    ## ORGANIZATION_TYPE.Self.employed                   -2.278e+01  1.118e+05
    ## ORGANIZATION_TYPE.Services                        -2.294e+01  1.118e+05
    ## ORGANIZATION_TYPE.Telecom                         -2.282e+01  1.118e+05
    ## ORGANIZATION_TYPE.Trade..type.1                   -2.299e+01  1.118e+05
    ## ORGANIZATION_TYPE.Trade..type.2                   -2.318e+01  1.118e+05
    ## ORGANIZATION_TYPE.Trade..type.3                   -2.278e+01  1.118e+05
    ## ORGANIZATION_TYPE.Trade..type.4                   -2.404e+01  1.118e+05
    ## ORGANIZATION_TYPE.Trade..type.5                   -2.317e+01  1.118e+05
    ## ORGANIZATION_TYPE.Trade..type.6                   -2.327e+01  1.118e+05
    ## ORGANIZATION_TYPE.Trade..type.7                   -2.283e+01  1.118e+05
    ## ORGANIZATION_TYPE.Transport..type.1               -2.352e+01  1.118e+05
    ## ORGANIZATION_TYPE.Transport..type.2               -2.302e+01  1.118e+05
    ## ORGANIZATION_TYPE.Transport..type.3               -2.242e+01  1.118e+05
    ## ORGANIZATION_TYPE.Transport..type.4               -2.289e+01  1.118e+05
    ## ORGANIZATION_TYPE.University                      -2.308e+01  1.118e+05
    ## ORGANIZATION_TYPE.XNA                                     NA         NA
    ## EXT_SOURCE_1                                      -1.131e+00  5.448e-02
    ## EXT_SOURCE_2                                      -2.030e+00  3.597e-02
    ## EXT_SOURCE_3                                      -2.660e+00  3.861e-02
    ## APARTMENTS_AVG                                     5.465e-01  1.693e+00
    ## BASEMENTAREA_AVG                                  -5.300e+00  2.720e+00
    ## YEARS_BEGINEXPLUATATION_AVG                        8.209e-01  1.605e+00
    ## YEARS_BUILD_AVG                                    5.096e+00  2.457e+00
    ## COMMONAREA_AVG                                     2.390e+00  1.920e+00
    ## ELEVATORS_AVG                                     -2.104e-01  1.283e+00
    ## ENTRANCES_AVG                                      1.969e+00  1.756e+00
    ## FLOORSMAX_AVG                                     -1.470e-01  1.283e+00
    ## FLOORSMIN_AVG                                     -3.790e-01  1.454e+00
    ## LANDAREA_AVG                                       1.056e+00  8.350e-01
    ## LIVINGAPARTMENTS_AVG                              -1.677e+00  2.559e+00
    ## LIVINGAREA_AVG                                     2.127e-01  1.319e+00
    ## NONLIVINGAPARTMENTS_AVG                           -1.070e+00  3.763e+00
    ## NONLIVINGAREA_AVG                                  1.710e+00  1.140e+00
    ## APARTMENTS_MODE                                   -4.961e-02  9.265e-01
    ## BASEMENTAREA_MODE                                  7.270e-01  1.018e+00
    ## YEARS_BEGINEXPLUATATION_MODE                       7.249e-02  7.062e-01
    ## YEARS_BUILD_MODE                                  -8.415e-01  9.111e-01
    ## COMMONAREA_MODE                                    1.843e+00  1.522e+00
    ## ELEVATORS_MODE                                     9.986e-01  8.269e-01
    ## ENTRANCES_MODE                                    -3.874e-01  7.983e-01
    ## FLOORSMAX_MODE                                    -3.771e-01  6.464e-01
    ## FLOORSMIN_MODE                                    -2.416e-01  7.106e-01
    ## LANDAREA_MODE                                     -3.819e-02  9.612e-01
    ## LIVINGAPARTMENTS_MODE                             -6.929e-01  7.580e-01
    ## LIVINGAREA_MODE                                   -1.093e-02  7.807e-01
    ## NONLIVINGAPARTMENTS_MODE                           3.107e+00  2.459e+00
    ## NONLIVINGAREA_MODE                                -1.538e+00  8.254e-01
    ## APARTMENTS_MEDI                                   -3.876e-01  1.777e+00
    ## BASEMENTAREA_MEDI                                  4.447e+00  2.679e+00
    ## YEARS_BEGINEXPLUATATION_MEDI                      -1.192e+00  1.446e+00
    ## YEARS_BUILD_MEDI                                  -4.531e+00  2.442e+00
    ## COMMONAREA_MEDI                                   -4.378e+00  2.369e+00
    ## ELEVATORS_MEDI                                    -7.762e-01  1.490e+00
    ## ENTRANCES_MEDI                                    -2.019e+00  1.867e+00
    ## FLOORSMAX_MEDI                                     1.096e-01  1.383e+00
    ## FLOORSMIN_MEDI                                     6.407e-01  1.574e+00
    ## LANDAREA_MEDI                                     -8.504e-01  1.235e+00
    ## LIVINGAPARTMENTS_MEDI                              2.620e+00  2.564e+00
    ## LIVINGAREA_MEDI                                   -5.097e-02  1.443e+00
    ## NONLIVINGAPARTMENTS_MEDI                          -1.706e+00  4.457e+00
    ## NONLIVINGAREA_MEDI                                -3.404e-01  1.365e+00
    ## FONDKAPREMONT_MODE.                                1.123e-01  4.275e-02
    ## FONDKAPREMONT_MODE.not.specified                   1.055e-01  6.586e-02
    ## FONDKAPREMONT_MODE.org.spec.account               -3.825e-03  7.063e-02
    ## FONDKAPREMONT_MODE.reg.oper.account                8.862e-02  4.124e-02
    ## FONDKAPREMONT_MODE.reg.oper.spec.account                  NA         NA
    ## HOUSETYPE_MODE.                                   -1.041e-02  1.231e-01
    ## HOUSETYPE_MODE.block.of.flats                      2.211e-02  1.100e-01
    ## HOUSETYPE_MODE.specific.housing                    1.343e-01  1.436e-01
    ## HOUSETYPE_MODE.terraced.house                             NA         NA
    ## TOTALAREA_MODE                                     5.485e-02  3.017e-01
    ## WALLSMATERIAL_MODE.                               -8.782e-03  7.372e-02
    ## WALLSMATERIAL_MODE.Block                          -3.033e-02  6.829e-02
    ## WALLSMATERIAL_MODE.Mixed                           1.057e-01  9.787e-02
    ## WALLSMATERIAL_MODE.Monolithic                     -8.589e-02  1.331e-01
    ## WALLSMATERIAL_MODE.Others                          1.718e-01  1.067e-01
    ## WALLSMATERIAL_MODE.Panel                          -2.806e-02  5.905e-02
    ## WALLSMATERIAL_MODE.Stone..brick                    6.171e-02  5.588e-02
    ## WALLSMATERIAL_MODE.Wooden                                 NA         NA
    ## EMERGENCYSTATE_MODE.                               8.198e-02  8.976e-02
    ## EMERGENCYSTATE_MODE.No                            -2.281e-02  7.891e-02
    ## EMERGENCYSTATE_MODE.Yes                                   NA         NA
    ## OBS_30_CNT_SOCIAL_CIRCLE                           7.026e-02  5.051e-02
    ## DEF_30_CNT_SOCIAL_CIRCLE                           1.282e-01  2.854e-02
    ## OBS_60_CNT_SOCIAL_CIRCLE                          -7.228e-02  5.102e-02
    ## DEF_60_CNT_SOCIAL_CIRCLE                           4.663e-02  3.378e-02
    ## DAYS_LAST_PHONE_CHANGE                             7.131e-05  9.507e-06
    ## FLAG_DOCUMENT_2                                    2.762e+00  6.173e-01
    ## FLAG_DOCUMENT_3                                    3.623e-01  5.957e-02
    ## FLAG_DOCUMENT_4                                   -2.322e+01  7.091e+04
    ## FLAG_DOCUMENT_5                                    3.303e-01  7.698e-02
    ## FLAG_DOCUMENT_6                                    2.993e-01  6.886e-02
    ## FLAG_DOCUMENT_7                                    9.755e-02  6.313e-01
    ## FLAG_DOCUMENT_8                                    1.637e-01  6.455e-02
    ## FLAG_DOCUMENT_9                                    1.721e-01  1.364e-01
    ## FLAG_DOCUMENT_10                                  -2.396e+01  1.355e+05
    ## FLAG_DOCUMENT_11                                  -1.446e-01  1.322e-01
    ## FLAG_DOCUMENT_12                                  -2.200e+01  2.630e+05
    ## FLAG_DOCUMENT_13                                  -7.694e-01  1.904e-01
    ## FLAG_DOCUMENT_14                                  -6.598e-01  1.911e-01
    ## FLAG_DOCUMENT_15                                  -7.838e-01  3.138e-01
    ## FLAG_DOCUMENT_16                                  -5.177e-01  8.785e-02
    ## FLAG_DOCUMENT_17                                  -1.012e+00  7.285e-01
    ## FLAG_DOCUMENT_18                                  -5.421e-01  9.185e-02
    ## FLAG_DOCUMENT_19                                  -3.473e-01  3.133e-01
    ## FLAG_DOCUMENT_20                                   3.668e-01  3.088e-01
    ## FLAG_DOCUMENT_21                                   1.748e-01  3.011e-01
    ## AMT_REQ_CREDIT_BUREAU_HOUR                        -6.190e-02  9.138e-02
    ## AMT_REQ_CREDIT_BUREAU_DAY                          1.016e-01  6.946e-02
    ## AMT_REQ_CREDIT_BUREAU_WEEK                        -9.553e-02  3.847e-02
    ## AMT_REQ_CREDIT_BUREAU_MON                         -3.392e-02  1.017e-02
    ## AMT_REQ_CREDIT_BUREAU_QRT                         -7.894e-02  1.242e-02
    ## AMT_REQ_CREDIT_BUREAU_YEAR                         1.705e-03  3.969e-03
    ##                                                   z value Pr(>|z|)    
    ## (Intercept)                                         0.449 0.653409    
    ## SK_ID_CURR                                         -0.753 0.451431    
    ## NAME_CONTRACT_TYPE                                  0.074 0.941207    
    ## CODE_GENDER.F                                      -0.449 0.653409    
    ## CODE_GENDER.M                                      -0.449 0.653409    
    ## CODE_GENDER.XNA                                    -0.449 0.653409    
    ## FLAG_OWN_CAR                                      -16.528  < 2e-16 ***
    ## FLAG_OWN_REALTY                                     1.993 0.046301 *  
    ## CNT_CHILDREN                                        1.966 0.049318 *  
    ## AMT_INCOME_TOTAL                                    0.803 0.422030    
    ## AMT_CREDIT                                         21.045  < 2e-16 ***
    ## AMT_ANNUITY                                        11.010  < 2e-16 ***
    ## AMT_GOODS_PRICE                                   -22.070  < 2e-16 ***
    ## NAME_TYPE_SUITE.                                   -1.517 0.129173    
    ## NAME_TYPE_SUITE.Children                            0.548 0.583493    
    ## NAME_TYPE_SUITE.Family                             -1.377 0.168638    
    ## NAME_TYPE_SUITE.Group.of.people                    -0.123 0.901724    
    ## NAME_TYPE_SUITE.Other_A                            -0.748 0.454322    
    ## NAME_TYPE_SUITE.Other_B                             1.155 0.247904    
    ## NAME_TYPE_SUITE.Spouse..partner                    -1.795 0.072608 .  
    ## NAME_TYPE_SUITE.Unaccompanied                          NA       NA    
    ## NAME_INCOME_TYPE.Businessman                        0.000 0.999818    
    ## NAME_INCOME_TYPE.Commercial.associate              -4.921 8.60e-07 ***
    ## NAME_INCOME_TYPE.Maternity.leave                    3.496 0.000473 ***
    ## NAME_INCOME_TYPE.Pensioner                          0.000 0.999830    
    ## NAME_INCOME_TYPE.State.servant                     -3.157 0.001592 ** 
    ## NAME_INCOME_TYPE.Student                            0.000 0.999755    
    ## NAME_INCOME_TYPE.Unemployed                         0.000 0.999850    
    ## NAME_INCOME_TYPE.Working                               NA       NA    
    ## NAME_EDUCATION_TYPE.Academic.degree                -2.688 0.007180 ** 
    ## NAME_EDUCATION_TYPE.Higher.education              -13.739  < 2e-16 ***
    ## NAME_EDUCATION_TYPE.Incomplete.higher              -4.984 6.23e-07 ***
    ## NAME_EDUCATION_TYPE.Lower.secondary                 1.795 0.072730 .  
    ## NAME_EDUCATION_TYPE.Secondary...secondary.special      NA       NA    
    ## NAME_FAMILY_STATUS.Civil.marriage                   3.579 0.000345 ***
    ## NAME_FAMILY_STATUS.Married                          0.106 0.915673    
    ## NAME_FAMILY_STATUS.Separated                        3.353 0.000799 ***
    ## NAME_FAMILY_STATUS.Single...not.married             2.543 0.010985 *  
    ## NAME_FAMILY_STATUS.Unknown                          0.000 0.999927    
    ## NAME_FAMILY_STATUS.Widow                               NA       NA    
    ## NAME_HOUSING_TYPE.Co.op.apartment                  -0.632 0.527604    
    ## NAME_HOUSING_TYPE.House...apartment                -0.419 0.675083    
    ## NAME_HOUSING_TYPE.Municipal.apartment               2.556 0.010603 *  
    ## NAME_HOUSING_TYPE.Office.apartment                 -2.526 0.011546 *  
    ## NAME_HOUSING_TYPE.Rented.apartment                  0.826 0.408983    
    ## NAME_HOUSING_TYPE.With.parents                         NA       NA    
    ## REGION_POPULATION_RELATIVE                          1.896 0.057937 .  
    ## DAYS_BIRTH                                          1.384 0.166409    
    ## DAYS_EMPLOYED                                     -15.043  < 2e-16 ***
    ## DAYS_REGISTRATION                                   4.430 9.41e-06 ***
    ## DAYS_ID_PUBLISH                                     8.931  < 2e-16 ***
    ## OWN_CAR_AGE                                         4.774 1.81e-06 ***
    ## FLAG_MOBIL                                          0.000 0.999932    
    ## FLAG_EMP_PHONE                                     -0.609 0.542703    
    ## FLAG_WORK_PHONE                                     9.891  < 2e-16 ***
    ## FLAG_CONT_MOBILE                                   -1.269 0.204395    
    ## FLAG_PHONE                                         -3.868 0.000110 ***
    ## FLAG_EMAIL                                         -2.596 0.009422 ** 
    ## OCCUPATION_TYPE.                                   -0.876 0.380989    
    ## OCCUPATION_TYPE.Accountants                        -2.297 0.021645 *  
    ## OCCUPATION_TYPE.Cleaning.staff                      0.111 0.911799    
    ## OCCUPATION_TYPE.Cooking.staff                      -0.188 0.850515    
    ## OCCUPATION_TYPE.Core.staff                         -1.569 0.116746    
    ## OCCUPATION_TYPE.Drivers                             0.651 0.514845    
    ## OCCUPATION_TYPE.High.skill.tech.staff              -1.742 0.081533 .  
    ## OCCUPATION_TYPE.HR.staff                           -0.005 0.996124    
    ## OCCUPATION_TYPE.IT.staff                           -0.823 0.410315    
    ## OCCUPATION_TYPE.Laborers                           -0.001 0.999424    
    ## OCCUPATION_TYPE.Low.skill.Laborers                  1.428 0.153175    
    ## OCCUPATION_TYPE.Managers                           -0.905 0.365268    
    ## OCCUPATION_TYPE.Medicine.staff                     -1.146 0.251685    
    ## OCCUPATION_TYPE.Private.service.staff              -1.655 0.098007 .  
    ## OCCUPATION_TYPE.Realty.agents                      -0.830 0.406344    
    ## OCCUPATION_TYPE.Sales.staff                        -0.613 0.539660    
    ## OCCUPATION_TYPE.Secretaries                         0.032 0.974699    
    ## OCCUPATION_TYPE.Security.staff                      0.583 0.559681    
    ## OCCUPATION_TYPE.Waiters.barmen.staff                   NA       NA    
    ## CNT_FAM_MEMBERS                                        NA       NA    
    ## REGION_RATING_CLIENT                               -2.052 0.040159 *  
    ## REGION_RATING_CLIENT_W_CITY                         5.125 2.98e-07 ***
    ## WEEKDAY_APPR_PROCESS_START.FRIDAY                  -0.235 0.814124    
    ## WEEKDAY_APPR_PROCESS_START.MONDAY                  -2.669 0.007611 ** 
    ## WEEKDAY_APPR_PROCESS_START.SATURDAY                -2.416 0.015678 *  
    ## WEEKDAY_APPR_PROCESS_START.SUNDAY                  -2.547 0.010877 *  
    ## WEEKDAY_APPR_PROCESS_START.THURSDAY                -0.875 0.381639    
    ## WEEKDAY_APPR_PROCESS_START.TUESDAY                  0.683 0.494569    
    ## WEEKDAY_APPR_PROCESS_START.WEDNESDAY                   NA       NA    
    ## HOUR_APPR_PROCESS_START                            -1.101 0.271113    
    ## REG_REGION_NOT_LIVE_REGION                         -2.239 0.025152 *  
    ## REG_REGION_NOT_WORK_REGION                          0.797 0.425249    
    ## LIVE_REGION_NOT_WORK_REGION                        -1.487 0.136889    
    ## REG_CITY_NOT_LIVE_CITY                              5.543 2.98e-08 ***
    ## REG_CITY_NOT_WORK_CITY                             -2.310 0.020862 *  
    ## LIVE_CITY_NOT_WORK_CITY                             1.942 0.052186 .  
    ## ORGANIZATION_TYPE.Advertising                       0.000 0.999837    
    ## ORGANIZATION_TYPE.Agriculture                       0.000 0.999837    
    ## ORGANIZATION_TYPE.Bank                              0.000 0.999834    
    ## ORGANIZATION_TYPE.Business.Entity.Type.1            0.000 0.999836    
    ## ORGANIZATION_TYPE.Business.Entity.Type.2            0.000 0.999836    
    ## ORGANIZATION_TYPE.Business.Entity.Type.3            0.000 0.999837    
    ## ORGANIZATION_TYPE.Cleaning                          0.000 0.999838    
    ## ORGANIZATION_TYPE.Construction                      0.000 0.999838    
    ## ORGANIZATION_TYPE.Culture                           0.000 0.999836    
    ## ORGANIZATION_TYPE.Electricity                       0.000 0.999835    
    ## ORGANIZATION_TYPE.Emergency                         0.000 0.999835    
    ## ORGANIZATION_TYPE.Government                        0.000 0.999836    
    ## ORGANIZATION_TYPE.Hotel                             0.000 0.999835    
    ## ORGANIZATION_TYPE.Housing                           0.000 0.999836    
    ## ORGANIZATION_TYPE.Industry..type.1                  0.000 0.999837    
    ## ORGANIZATION_TYPE.Industry..type.10                 0.000 0.999834    
    ## ORGANIZATION_TYPE.Industry..type.11                 0.000 0.999836    
    ## ORGANIZATION_TYPE.Industry..type.12                 0.000 0.999832    
    ## ORGANIZATION_TYPE.Industry..type.13                 0.000 0.999836    
    ## ORGANIZATION_TYPE.Industry..type.2                  0.000 0.999834    
    ## ORGANIZATION_TYPE.Industry..type.3                  0.000 0.999837    
    ## ORGANIZATION_TYPE.Industry..type.4                  0.000 0.999836    
    ## ORGANIZATION_TYPE.Industry..type.5                  0.000 0.999835    
    ## ORGANIZATION_TYPE.Industry..type.6                  0.000 0.999835    
    ## ORGANIZATION_TYPE.Industry..type.7                  0.000 0.999836    
    ## ORGANIZATION_TYPE.Industry..type.8                  0.000 0.999839    
    ## ORGANIZATION_TYPE.Industry..type.9                  0.000 0.999834    
    ## ORGANIZATION_TYPE.Insurance                         0.000 0.999836    
    ## ORGANIZATION_TYPE.Kindergarten                      0.000 0.999836    
    ## ORGANIZATION_TYPE.Legal.Services                    0.000 0.999840    
    ## ORGANIZATION_TYPE.Medicine                          0.000 0.999836    
    ## ORGANIZATION_TYPE.Military                          0.000 0.999833    
    ## ORGANIZATION_TYPE.Mobile                            0.000 0.999837    
    ## ORGANIZATION_TYPE.Other                             0.000 0.999836    
    ## ORGANIZATION_TYPE.Police                            0.000 0.999834    
    ## ORGANIZATION_TYPE.Postal                            0.000 0.999837    
    ## ORGANIZATION_TYPE.Realtor                           0.000 0.999841    
    ## ORGANIZATION_TYPE.Religion                          0.000 0.999836    
    ## ORGANIZATION_TYPE.Restaurant                        0.000 0.999837    
    ## ORGANIZATION_TYPE.School                            0.000 0.999835    
    ## ORGANIZATION_TYPE.Security                          0.000 0.999836    
    ## ORGANIZATION_TYPE.Security.Ministries               0.000 0.999834    
    ## ORGANIZATION_TYPE.Self.employed                     0.000 0.999837    
    ## ORGANIZATION_TYPE.Services                          0.000 0.999836    
    ## ORGANIZATION_TYPE.Telecom                           0.000 0.999837    
    ## ORGANIZATION_TYPE.Trade..type.1                     0.000 0.999836    
    ## ORGANIZATION_TYPE.Trade..type.2                     0.000 0.999835    
    ## ORGANIZATION_TYPE.Trade..type.3                     0.000 0.999837    
    ## ORGANIZATION_TYPE.Trade..type.4                     0.000 0.999828    
    ## ORGANIZATION_TYPE.Trade..type.5                     0.000 0.999835    
    ## ORGANIZATION_TYPE.Trade..type.6                     0.000 0.999834    
    ## ORGANIZATION_TYPE.Trade..type.7                     0.000 0.999837    
    ## ORGANIZATION_TYPE.Transport..type.1                 0.000 0.999832    
    ## ORGANIZATION_TYPE.Transport..type.2                 0.000 0.999836    
    ## ORGANIZATION_TYPE.Transport..type.3                 0.000 0.999840    
    ## ORGANIZATION_TYPE.Transport..type.4                 0.000 0.999837    
    ## ORGANIZATION_TYPE.University                        0.000 0.999835    
    ## ORGANIZATION_TYPE.XNA                                  NA       NA    
    ## EXT_SOURCE_1                                      -20.754  < 2e-16 ***
    ## EXT_SOURCE_2                                      -56.450  < 2e-16 ***
    ## EXT_SOURCE_3                                      -68.894  < 2e-16 ***
    ## APARTMENTS_AVG                                      0.323 0.746894    
    ## BASEMENTAREA_AVG                                   -1.948 0.051380 .  
    ## YEARS_BEGINEXPLUATATION_AVG                         0.511 0.609077    
    ## YEARS_BUILD_AVG                                     2.074 0.038117 *  
    ## COMMONAREA_AVG                                      1.245 0.213216    
    ## ELEVATORS_AVG                                      -0.164 0.869683    
    ## ENTRANCES_AVG                                       1.121 0.262173    
    ## FLOORSMAX_AVG                                      -0.115 0.908762    
    ## FLOORSMIN_AVG                                      -0.261 0.794374    
    ## LANDAREA_AVG                                        1.265 0.205834    
    ## LIVINGAPARTMENTS_AVG                               -0.655 0.512360    
    ## LIVINGAREA_AVG                                      0.161 0.871883    
    ## NONLIVINGAPARTMENTS_AVG                            -0.284 0.776222    
    ## NONLIVINGAREA_AVG                                   1.499 0.133812    
    ## APARTMENTS_MODE                                    -0.054 0.957295    
    ## BASEMENTAREA_MODE                                   0.714 0.475135    
    ## YEARS_BEGINEXPLUATATION_MODE                        0.103 0.918240    
    ## YEARS_BUILD_MODE                                   -0.924 0.355699    
    ## COMMONAREA_MODE                                     1.211 0.225984    
    ## ELEVATORS_MODE                                      1.208 0.227197    
    ## ENTRANCES_MODE                                     -0.485 0.627479    
    ## FLOORSMAX_MODE                                     -0.583 0.559667    
    ## FLOORSMIN_MODE                                     -0.340 0.733871    
    ## LANDAREA_MODE                                      -0.040 0.968309    
    ## LIVINGAPARTMENTS_MODE                              -0.914 0.360680    
    ## LIVINGAREA_MODE                                    -0.014 0.988827    
    ## NONLIVINGAPARTMENTS_MODE                            1.264 0.206395    
    ## NONLIVINGAREA_MODE                                 -1.863 0.062433 .  
    ## APARTMENTS_MEDI                                    -0.218 0.827289    
    ## BASEMENTAREA_MEDI                                   1.660 0.096994 .  
    ## YEARS_BEGINEXPLUATATION_MEDI                       -0.825 0.409515    
    ## YEARS_BUILD_MEDI                                   -1.856 0.063477 .  
    ## COMMONAREA_MEDI                                    -1.848 0.064533 .  
    ## ELEVATORS_MEDI                                     -0.521 0.602437    
    ## ENTRANCES_MEDI                                     -1.082 0.279428    
    ## FLOORSMAX_MEDI                                      0.079 0.936817    
    ## FLOORSMIN_MEDI                                      0.407 0.684050    
    ## LANDAREA_MEDI                                      -0.688 0.491140    
    ## LIVINGAPARTMENTS_MEDI                               1.022 0.306854    
    ## LIVINGAREA_MEDI                                    -0.035 0.971819    
    ## NONLIVINGAPARTMENTS_MEDI                           -0.383 0.701843    
    ## NONLIVINGAREA_MEDI                                 -0.249 0.803102    
    ## FONDKAPREMONT_MODE.                                 2.628 0.008596 ** 
    ## FONDKAPREMONT_MODE.not.specified                    1.602 0.109199    
    ## FONDKAPREMONT_MODE.org.spec.account                -0.054 0.956816    
    ## FONDKAPREMONT_MODE.reg.oper.account                 2.149 0.031623 *  
    ## FONDKAPREMONT_MODE.reg.oper.spec.account               NA       NA    
    ## HOUSETYPE_MODE.                                    -0.085 0.932585    
    ## HOUSETYPE_MODE.block.of.flats                       0.201 0.840666    
    ## HOUSETYPE_MODE.specific.housing                     0.935 0.349655    
    ## HOUSETYPE_MODE.terraced.house                          NA       NA    
    ## TOTALAREA_MODE                                      0.182 0.855727    
    ## WALLSMATERIAL_MODE.                                -0.119 0.905174    
    ## WALLSMATERIAL_MODE.Block                           -0.444 0.656934    
    ## WALLSMATERIAL_MODE.Mixed                            1.080 0.280176    
    ## WALLSMATERIAL_MODE.Monolithic                      -0.645 0.518716    
    ## WALLSMATERIAL_MODE.Others                           1.611 0.107268    
    ## WALLSMATERIAL_MODE.Panel                           -0.475 0.634648    
    ## WALLSMATERIAL_MODE.Stone..brick                     1.104 0.269424    
    ## WALLSMATERIAL_MODE.Wooden                              NA       NA    
    ## EMERGENCYSTATE_MODE.                                0.913 0.361047    
    ## EMERGENCYSTATE_MODE.No                             -0.289 0.772501    
    ## EMERGENCYSTATE_MODE.Yes                                NA       NA    
    ## OBS_30_CNT_SOCIAL_CIRCLE                            1.391 0.164199    
    ## DEF_30_CNT_SOCIAL_CIRCLE                            4.493 7.01e-06 ***
    ## OBS_60_CNT_SOCIAL_CIRCLE                           -1.417 0.156585    
    ## DEF_60_CNT_SOCIAL_CIRCLE                            1.381 0.167411    
    ## DAYS_LAST_PHONE_CHANGE                              7.500 6.37e-14 ***
    ## FLAG_DOCUMENT_2                                     4.474 7.67e-06 ***
    ## FLAG_DOCUMENT_3                                     6.082 1.19e-09 ***
    ## FLAG_DOCUMENT_4                                     0.000 0.999739    
    ## FLAG_DOCUMENT_5                                     4.291 1.78e-05 ***
    ## FLAG_DOCUMENT_6                                     4.346 1.38e-05 ***
    ## FLAG_DOCUMENT_7                                     0.155 0.877186    
    ## FLAG_DOCUMENT_8                                     2.537 0.011186 *  
    ## FLAG_DOCUMENT_9                                     1.262 0.206939    
    ## FLAG_DOCUMENT_10                                    0.000 0.999859    
    ## FLAG_DOCUMENT_11                                   -1.093 0.274294    
    ## FLAG_DOCUMENT_12                                    0.000 0.999933    
    ## FLAG_DOCUMENT_13                                   -4.040 5.33e-05 ***
    ## FLAG_DOCUMENT_14                                   -3.452 0.000556 ***
    ## FLAG_DOCUMENT_15                                   -2.498 0.012503 *  
    ## FLAG_DOCUMENT_16                                   -5.893 3.79e-09 ***
    ## FLAG_DOCUMENT_17                                   -1.389 0.164951    
    ## FLAG_DOCUMENT_18                                   -5.902 3.60e-09 ***
    ## FLAG_DOCUMENT_19                                   -1.109 0.267550    
    ## FLAG_DOCUMENT_20                                    1.188 0.234867    
    ## FLAG_DOCUMENT_21                                    0.581 0.561555    
    ## AMT_REQ_CREDIT_BUREAU_HOUR                         -0.677 0.498125    
    ## AMT_REQ_CREDIT_BUREAU_DAY                           1.463 0.143499    
    ## AMT_REQ_CREDIT_BUREAU_WEEK                         -2.483 0.013012 *  
    ## AMT_REQ_CREDIT_BUREAU_MON                          -3.335 0.000852 ***
    ## AMT_REQ_CREDIT_BUREAU_QRT                          -6.355 2.08e-10 ***
    ## AMT_REQ_CREDIT_BUREAU_YEAR                          0.430 0.667499    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 172542  on 307510  degrees of freedom
    ## Residual deviance: 153168  on 307275  degrees of freedom
    ## AIC: 153640
    ## 
    ## Number of Fisher Scoring iterations: 25

``` r
fitted.results <- predict(model, newdata= test, type='response')
submit <- data.frame(SK_ID_CURR = as.integer(test$SK_ID_CURR),
                     TARGET = fitted.results)
head(submit)
```

    ##   SK_ID_CURR     TARGET
    ## 1     100001 0.06168021
    ## 2     100005 0.21883656
    ## 3     100013 0.03924733
    ## 4     100028 0.03032419
    ## 5     100038 0.12236619
    ## 6     100042 0.02210948

``` r
#write.csv(submit, file = "data/log_reg_baseline.csv",row.names = FALSE)
```

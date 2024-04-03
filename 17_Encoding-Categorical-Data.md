---
output: 
  html_document: 
    keep_md: yes
---

# 17 Encoding Categorical Data


```r
library(tidyverse)
```

```
## Warning: package 'ggplot2' was built under R version 4.3.3
```

```
## Warning: package 'tidyr' was built under R version 4.3.3
```

```
## Warning: package 'readr' was built under R version 4.3.3
```

```
## Warning: package 'dplyr' was built under R version 4.3.3
```

```
## Warning: package 'stringr' was built under R version 4.3.3
```

```
## Warning: package 'lubridate' was built under R version 4.3.3
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.4     ✔ readr     2.1.5
## ✔ forcats   1.0.0     ✔ stringr   1.5.1
## ✔ ggplot2   3.5.0     ✔ tibble    3.2.1
## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
## ✔ purrr     1.0.2     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

```r
library(tidymodels)
```

```
## Warning: package 'tidymodels' was built under R version 4.3.3
```

```
## ── Attaching packages ────────────────────────────────────── tidymodels 1.2.0 ──
## ✔ broom        1.0.5      ✔ rsample      1.2.1 
## ✔ dials        1.2.1      ✔ tune         1.2.0 
## ✔ infer        1.0.7      ✔ workflows    1.1.4 
## ✔ modeldata    1.3.0      ✔ workflowsets 1.1.0 
## ✔ parsnip      1.2.1      ✔ yardstick    1.3.1 
## ✔ recipes      1.0.10
```

```
## Warning: package 'dials' was built under R version 4.3.3
```

```
## Warning: package 'scales' was built under R version 4.3.3
```

```
## Warning: package 'infer' was built under R version 4.3.3
```

```
## Warning: package 'modeldata' was built under R version 4.3.3
```

```
## Warning: package 'parsnip' was built under R version 4.3.3
```

```
## Warning: package 'recipes' was built under R version 4.3.3
```

```
## Warning: package 'rsample' was built under R version 4.3.3
```

```
## Warning: package 'tune' was built under R version 4.3.3
```

```
## Warning: package 'workflows' was built under R version 4.3.3
```

```
## Warning: package 'workflowsets' was built under R version 4.3.3
```

```
## Warning: package 'yardstick' was built under R version 4.3.3
```

```
## ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
## ✖ scales::discard() masks purrr::discard()
## ✖ dplyr::filter()   masks stats::filter()
## ✖ recipes::fixed()  masks stringr::fixed()
## ✖ dplyr::lag()      masks stats::lag()
## ✖ yardstick::spec() masks readr::spec()
## ✖ recipes::step()   masks stats::step()
## • Dig deeper into tidy modeling with R at https://www.tmwr.org
```

```r
data(ames)
ames <- mutate(ames, Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)
```

## 17.1 IS AN ENCODING NECESSARY?
## 17.2 ENCODING ORDINAL PREDICTORS
## 17.3 USING THE OUTCOME FOR ENCODING PREDICTORS


```r
ames_train %>%
  group_by(Neighborhood) %>%
  summarize(mean = mean(Sale_Price),
            std_err = sd(Sale_Price) / sqrt(length(Sale_Price))) %>% 
  ggplot(aes(y = reorder(Neighborhood, mean), x = mean)) + 
  geom_point() +
  geom_errorbar(aes(xmin = mean - 1.64 * std_err, xmax = mean + 1.64 * std_err)) +
  labs(y = NULL, x = "Price (mean, log scale)")
```

![](17_Encoding-Categorical-Data_files/figure-html/unnamed-chunk-2-1.png)<!-- -->


```r
library(embed)
```

```
## Warning: package 'embed' was built under R version 4.3.3
```

```r
ames_glm <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_lencode_glm(Neighborhood, outcome = vars(Sale_Price)) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20)

ames_glm
```

```
## 
```

```
## ── Recipe ──────────────────────────────────────────────────────────────────────
```

```
## 
```

```
## ── Inputs
```

```
## Number of variables by role
```

```
## outcome:   1
## predictor: 6
```

```
## 
```

```
## ── Operations
```

```
## • Log transformation on: Gr_Liv_Area
```

```
## • Linear embedding for factors via GLM for: Neighborhood
```

```
## • Dummy variables from: all_nominal_predictors()
```

```
## • Interactions with: Gr_Liv_Area:starts_with("Bldg_Type_")
```

```
## • Natural splines on: Latitude and Longitude
```


```r
glm_estimates <-
  prep(ames_glm) %>%
  tidy(number = 2)

glm_estimates
```

```
## # A tibble: 29 × 4
##    level              value terms        id               
##    <chr>              <dbl> <chr>        <chr>            
##  1 North_Ames          5.15 Neighborhood lencode_glm_yj20u
##  2 College_Creek       5.29 Neighborhood lencode_glm_yj20u
##  3 Old_Town            5.07 Neighborhood lencode_glm_yj20u
##  4 Edwards             5.09 Neighborhood lencode_glm_yj20u
##  5 Somerset            5.35 Neighborhood lencode_glm_yj20u
##  6 Northridge_Heights  5.49 Neighborhood lencode_glm_yj20u
##  7 Gilbert             5.28 Neighborhood lencode_glm_yj20u
##  8 Sawyer              5.13 Neighborhood lencode_glm_yj20u
##  9 Northwest_Ames      5.27 Neighborhood lencode_glm_yj20u
## 10 Sawyer_West         5.24 Neighborhood lencode_glm_yj20u
## # ℹ 19 more rows
```


```r
glm_estimates %>%
  filter(level == "..new")
```

```
## # A tibble: 1 × 4
##   level value terms        id               
##   <chr> <dbl> <chr>        <chr>            
## 1 ..new  5.23 Neighborhood lencode_glm_yj20u
```

### 17.3.1 EFFECT ENCODINGS WITH PARTIAL POOLING


```r
ames_mixed <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_lencode_mixed(Neighborhood, outcome = vars(Sale_Price)) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20)

ames_mixed
```

```
## 
```

```
## ── Recipe ──────────────────────────────────────────────────────────────────────
```

```
## 
```

```
## ── Inputs
```

```
## Number of variables by role
```

```
## outcome:   1
## predictor: 6
```

```
## 
```

```
## ── Operations
```

```
## • Log transformation on: Gr_Liv_Area
```

```
## • Linear embedding for factors via mixed effects for: Neighborhood
```

```
## • Dummy variables from: all_nominal_predictors()
```

```
## • Interactions with: Gr_Liv_Area:starts_with("Bldg_Type_")
```

```
## • Natural splines on: Latitude and Longitude
```


```r
mixed_estimates <-
  prep(ames_mixed) %>%
  tidy(number = 2)

mixed_estimates
```

```
## # A tibble: 29 × 4
##    level              value terms        id                 
##    <chr>              <dbl> <chr>        <chr>              
##  1 North_Ames          5.15 Neighborhood lencode_mixed_AmBz1
##  2 College_Creek       5.29 Neighborhood lencode_mixed_AmBz1
##  3 Old_Town            5.07 Neighborhood lencode_mixed_AmBz1
##  4 Edwards             5.10 Neighborhood lencode_mixed_AmBz1
##  5 Somerset            5.35 Neighborhood lencode_mixed_AmBz1
##  6 Northridge_Heights  5.49 Neighborhood lencode_mixed_AmBz1
##  7 Gilbert             5.28 Neighborhood lencode_mixed_AmBz1
##  8 Sawyer              5.13 Neighborhood lencode_mixed_AmBz1
##  9 Northwest_Ames      5.27 Neighborhood lencode_mixed_AmBz1
## 10 Sawyer_West         5.24 Neighborhood lencode_mixed_AmBz1
## # ℹ 19 more rows
```


```r
mixed_estimates %>%
  filter(level == "..new")
```

```
## # A tibble: 1 × 4
##   level value terms        id                 
##   <chr> <dbl> <chr>        <chr>              
## 1 ..new  5.23 Neighborhood lencode_mixed_AmBz1
```


```r
glm_estimates %>%
  rename(`no pooling` = value) %>%
  left_join(
    mixed_estimates %>%
      rename(`partial pooling` = value), by = "level"
  ) %>%
  left_join(
    ames_train %>% 
      count(Neighborhood) %>% 
      mutate(level = as.character(Neighborhood))
  ) %>%
  ggplot(aes(`no pooling`, `partial pooling`, size = sqrt(n))) +
  geom_abline(color = "gray50", lty = 2) +
  geom_point(alpha = 0.7) +
  coord_fixed()
```

```
## Joining with `by = join_by(level)`
```

```
## Warning: Removed 1 row containing missing values or values outside the scale range
## (`geom_point()`).
```

![](17_Encoding-Categorical-Data_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

## 17.4 FEATURE HASHING


```r
library(rlang)
```

```
## Warning: package 'rlang' was built under R version 4.3.3
```

```
## 
## Attaching package: 'rlang'
```

```
## The following objects are masked from 'package:purrr':
## 
##     %@%, flatten, flatten_chr, flatten_dbl, flatten_int, flatten_lgl,
##     flatten_raw, invoke, splice
```

```r
ames_hashed <-
  ames_train %>%
  mutate(Hash = map_chr(Neighborhood, hash))

ames_hashed %>%
  select(Neighborhood, Hash)
```

```
## # A tibble: 2,342 × 2
##    Neighborhood    Hash                            
##    <fct>           <chr>                           
##  1 North_Ames      076543f71313e522efe157944169d919
##  2 North_Ames      076543f71313e522efe157944169d919
##  3 Briardale       b598bec306983e3e68a3118952df8cf0
##  4 Briardale       b598bec306983e3e68a3118952df8cf0
##  5 Northpark_Villa 6af95b5db968bf393e78188a81e0e1e4
##  6 Northpark_Villa 6af95b5db968bf393e78188a81e0e1e4
##  7 Sawyer_West     5b8ac3fb32a9671d55da7b13c6e5ef49
##  8 Sawyer_West     5b8ac3fb32a9671d55da7b13c6e5ef49
##  9 Sawyer          2491e7eceb561d5a5f19abea49145c68
## 10 Sawyer          2491e7eceb561d5a5f19abea49145c68
## # ℹ 2,332 more rows
```


```r
ames_hashed %>%
  ## first make a smaller hash for integers that R can handle
  mutate(Hash = strtoi(substr(Hash, 26, 32), base = 16L),  
         ## now take the modulo
         Hash = Hash %% 16) %>%
  select(Neighborhood, Hash)
```

```
## # A tibble: 2,342 × 2
##    Neighborhood     Hash
##    <fct>           <dbl>
##  1 North_Ames          9
##  2 North_Ames          9
##  3 Briardale           0
##  4 Briardale           0
##  5 Northpark_Villa     4
##  6 Northpark_Villa     4
##  7 Sawyer_West         9
##  8 Sawyer_West         9
##  9 Sawyer              8
## 10 Sawyer              8
## # ℹ 2,332 more rows
```


```r
library(textrecipes)
```

```
## Warning: package 'textrecipes' was built under R version 4.3.3
```

```r
ames_hash <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_dummy_hash(Neighborhood, signed = FALSE, num_terms = 16L) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20)
```

```
## 1 package (text2vec) is needed for this step but is not installed.
```

```
## To install run: `install.packages("text2vec")`
```

```r
ames_hash
```

```
## 
```

```
## ── Recipe ──────────────────────────────────────────────────────────────────────
```

```
## 
```

```
## ── Inputs
```

```
## Number of variables by role
```

```
## outcome:   1
## predictor: 6
```

```
## 
```

```
## ── Operations
```

```
## • Log transformation on: Gr_Liv_Area
```

```
## • Feature hashing with: Neighborhood
```

```
## • Dummy variables from: all_nominal_predictors()
```

```
## • Interactions with: Gr_Liv_Area:starts_with("Bldg_Type_")
```

```
## • Natural splines on: Latitude and Longitude
```

## 17.5 MORE ENCODING OPTIONS

## 17.6 CHAPTER SUMMARY

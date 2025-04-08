Lab 11 - Grading the professor, Pt. 2
================
Hannah Crawley
4/7/2025

## Load packages and data

``` r
library(tidyverse) 
library(tidymodels)
```

    ## Warning: package 'tidymodels' was built under R version 4.4.3

    ## Warning: package 'dials' was built under R version 4.4.3

    ## Warning: package 'infer' was built under R version 4.4.3

    ## Warning: package 'modeldata' was built under R version 4.4.3

    ## Warning: package 'parsnip' was built under R version 4.4.3

    ## Warning: package 'recipes' was built under R version 4.4.3

    ## Warning: package 'rsample' was built under R version 4.4.3

    ## Warning: package 'tune' was built under R version 4.4.3

    ## Warning: package 'workflows' was built under R version 4.4.3

    ## Warning: package 'workflowsets' was built under R version 4.4.3

    ## Warning: package 'yardstick' was built under R version 4.4.3

``` r
library(openintro)
```

    ## Warning: package 'openintro' was built under R version 4.4.3

    ## Warning: package 'airports' was built under R version 4.4.3

    ## Warning: package 'cherryblossom' was built under R version 4.4.3

    ## Warning: package 'usdata' was built under R version 4.4.3

# Part 1

## Exercise 1

``` r
m_bty <- linear_reg() %>%
  set_engine("lm") %>%
  fit(score ~ bty_avg, data = evals)
```

# Part 2

## Exercise 2

> Fit a linear model (one you have fit before): m_bty_gen, predicting
> average professor evaluation score based on average beauty rating
> (bty_avg) and gender. Write the linear model, and note the R2 and the
> adjusted R2.

``` r
m_bty_gen <- linear_reg() %>%
  set_engine("lm") %>%
  fit(score ~ gender + bty_avg, data = evals)
```

## Exercise 3

> Interpret the slope and intercept of m_bty_gen in context of the data.

``` r
summary(m_bty_gen$fit)
```

    ## 
    ## Call:
    ## stats::lm(formula = score ~ gender + bty_avg, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8305 -0.3625  0.1055  0.4213  0.9314 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.74734    0.08466  44.266  < 2e-16 ***
    ## gendermale   0.17239    0.05022   3.433 0.000652 ***
    ## bty_avg      0.07416    0.01625   4.563 6.48e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5287 on 460 degrees of freedom
    ## Multiple R-squared:  0.05912,    Adjusted R-squared:  0.05503 
    ## F-statistic: 14.45 on 2 and 460 DF,  p-value: 8.177e-07

Slope: 0.172 – on average male professors score .172 points higher than
females when beauty is controlled for Slope: 0.074 – for every one point
increase in beauty score increases by .074 when gender is controlled for

## Exercise 4

> What percent of the variability in score is explained by the model
> m_bty_gen.

``` r
summary(m_bty_gen$fit)$r.squared
```

    ## [1] 0.05912279

R^2 = 0.0591 – 5.91% of variability in score is explained by gender and
beauty

## Exercise 5

> What is the equation of the line corresponding to just male
> professors?

Male professors: y = 3.747 + .172 = 3.919

## Exercise 6

> For two professors who received the same beauty rating, which gender
> tends to have the higher course evaluation score?

Male professors tend to have a higher course eval

## Exercise 7

> How does the relationship between beauty and evaluation score vary
> between male and female professors?

On average males tend to score .172 more on average when beauty is
controlled for

## Exercise 8

> How do the adjusted R2 values of m_bty_gen and m_bty compare? What
> does this tell us about how useful gender is in explaining the
> variability in evaluation scores when we already have information on
> the beauty score of the professor.

``` r
summary(m_bty$fit)$r.squared
```

    ## [1] 0.03502226

``` r
summary(m_bty_gen$fit)$r.squared
```

    ## [1] 0.05912279

R^2 = 0.0591 – 5.91% of variability in score is explained by gender and
beauty R^2 = 0.035 – 3.5% of variability in score explained by beauty

Gender contributes additional explained variance beyond beauty alone

## Exercise 9

> Compare the slopes of bty_avg under the two models (m_bty and
> m_bty_gen). Has the addition of gender to the model changed the
> parameter estimate (slope) for bty_avg?

``` r
summary(m_bty$fit)
```

    ## 
    ## Call:
    ## stats::lm(formula = score ~ bty_avg, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9246 -0.3690  0.1420  0.3977  0.9309 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.88034    0.07614   50.96  < 2e-16 ***
    ## bty_avg      0.06664    0.01629    4.09 5.08e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5348 on 461 degrees of freedom
    ## Multiple R-squared:  0.03502,    Adjusted R-squared:  0.03293 
    ## F-statistic: 16.73 on 1 and 461 DF,  p-value: 5.083e-05

``` r
summary(m_bty_gen$fit)
```

    ## 
    ## Call:
    ## stats::lm(formula = score ~ gender + bty_avg, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8305 -0.3625  0.1055  0.4213  0.9314 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.74734    0.08466  44.266  < 2e-16 ***
    ## gendermale   0.17239    0.05022   3.433 0.000652 ***
    ## bty_avg      0.07416    0.01625   4.563 6.48e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5287 on 460 degrees of freedom
    ## Multiple R-squared:  0.05912,    Adjusted R-squared:  0.05503 
    ## F-statistic: 14.45 on 2 and 460 DF,  p-value: 8.177e-07

Beauty model bty slope: 0.067 Beauty and gender model bty slope: 0.074

Accounting for gender the slope of bty_avg increases - 1 point increase
in beauty is a .074 point increase in score

## Exercise 10

> Create a new model called m_bty_rank with gender removed and rank
> added in. Write the equation of the linear model and interpret the
> slopes and intercept in context of the data.

``` r
m_bty_rank <- linear_reg() %>%
  set_engine("lm") %>%
  fit(score ~ rank + bty_avg, data = evals)
summary(m_bty_rank$fit)
```

    ## 
    ## Call:
    ## stats::lm(formula = score ~ rank + bty_avg, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8713 -0.3642  0.1489  0.4103  0.9525 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       3.98155    0.09078  43.860  < 2e-16 ***
    ## ranktenure track -0.16070    0.07395  -2.173   0.0303 *  
    ## ranktenured      -0.12623    0.06266  -2.014   0.0445 *  
    ## bty_avg           0.06783    0.01655   4.098 4.92e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5328 on 459 degrees of freedom
    ## Multiple R-squared:  0.04652,    Adjusted R-squared:  0.04029 
    ## F-statistic: 7.465 on 3 and 459 DF,  p-value: 6.88e-05

Tenure track slope = -0.161 – tenure track professors score lower than
non-tenure track holding beauty constant

Tenured slope = -0.126 – tenured prof score lower than non-tenure track
holding beauty constant

Bty_avg slope = 0.068 – controlling for rank there is a .068 increase in
score for every 1 point increase in beauty

# Part 3: The search for the best model

Going forward, only consider the following variables as potential
predictors: rank, ethnicity, gender, language, age, cls_perc_eval,
cls_did_eval, cls_students, cls_level, cls_profs, cls_credits, bty_avg.

## Exercise 11

> Which variable, on its own, would you expect to be the worst predictor
> of evaluation scores? Why? Hint: Think about which variable would you
> expect to not have any association with the professor’s score.

I would expect that number of credits may not have a strong input on
score

## Exercise 12

> Check your suspicions from the previous exercise. Include the model
> output for that variable in your response.

``` r
m_credits <- linear_reg() %>%
  set_engine("lm") %>%
  fit(score ~ cls_credits, data = evals)
summary(m_credits$fit)$r.squared
```

    ## [1] 0.04201514

``` r
summary(m_credits$fit)
```

    ## 
    ## Call:
    ## stats::lm(formula = score ~ cls_credits, data = data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.84702 -0.34702  0.05298  0.35298  0.85298 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            4.14702    0.02552 162.494  < 2e-16 ***
    ## cls_creditsone credit  0.47520    0.10568   4.496 8.75e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5329 on 461 degrees of freedom
    ## Multiple R-squared:  0.04202,    Adjusted R-squared:  0.03994 
    ## F-statistic: 20.22 on 1 and 461 DF,  p-value: 8.751e-06

Class credit has a large impact on score as classes that are 1 credit
recieve a score of .475 higher on evals and predicts 4.2% of variance in
score

## Exercise 13

> Suppose you wanted to fit a full model with the variables listed
> above. If you are already going to include cls_perc_eval and
> cls_students, which variable should you not include as an additional
> predictor? Why?

I would exclude the number of students in the class as the percentage of
evals completed is in part dependent on the number of students in the
class.

## Exercise 14

> Fit a full model with all predictors listed above (except for the one
> you decided to exclude) in the previous question.

``` r
m_fullmodel <- linear_reg() %>%
  set_engine("lm") %>%
  fit(score ~ rank + ethnicity + gender + language + age + cls_perc_eval + cls_did_eval + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(m_fullmodel$fit)
```

    ## 
    ## Call:
    ## stats::lm(formula = score ~ rank + ethnicity + gender + language + 
    ##     age + cls_perc_eval + cls_did_eval + cls_students + cls_level + 
    ##     cls_profs + cls_credits + bty_avg, data = data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.83665 -0.31377  0.08559  0.35655  1.08091 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.6035360  0.2615008  13.780  < 2e-16 ***
    ## ranktenure track      -0.1022542  0.0823357  -1.242 0.214915    
    ## ranktenured           -0.0444115  0.0652594  -0.681 0.496514    
    ## ethnicitynot minority  0.1838073  0.0776989   2.366 0.018423 *  
    ## gendermale             0.1813064  0.0516980   3.507 0.000499 ***
    ## languagenon-english   -0.1297849  0.1081723  -1.200 0.230850    
    ## age                   -0.0065680  0.0030868  -2.128 0.033900 *  
    ## cls_perc_eval          0.0046764  0.0021063   2.220 0.026904 *  
    ## cls_did_eval           0.0022369  0.0031124   0.719 0.472698    
    ## cls_students          -0.0009486  0.0019726  -0.481 0.630832    
    ## cls_levelupper         0.0103812  0.0568080   0.183 0.855084    
    ## cls_profssingle       -0.0050013  0.0516204  -0.097 0.922860    
    ## cls_creditsone credit  0.5063406  0.1171236   4.323  1.9e-05 ***
    ## bty_avg                0.0609228  0.0166912   3.650 0.000293 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5043 on 449 degrees of freedom
    ## Multiple R-squared:  0.1645, Adjusted R-squared:  0.1403 
    ## F-statistic: 6.799 on 13 and 449 DF,  p-value: 5.372e-12

## Exercise 15

> Using backward-selection with adjusted R-squared as the selection
> criterion, determine the best model. You do not need to show all steps
> in your answer, just the output for the final model. Also, write out
> the linear model for predicting score based on the final model you
> settle on.

``` r
# Build full model using base R
full_model <- stats::lm(score ~ rank + ethnicity + gender + language + age +
                          cls_perc_eval + cls_did_eval + cls_students +
                          cls_level + cls_profs + cls_credits + bty_avg,
                        data = evals)

# Run backward selection using base R's step function
final_model <- stats::step(full_model, direction = "backward")
```

    ## Start:  AIC=-620.19
    ## score ~ rank + ethnicity + gender + language + age + cls_perc_eval + 
    ##     cls_did_eval + cls_students + cls_level + cls_profs + cls_credits + 
    ##     bty_avg
    ## 
    ##                 Df Sum of Sq    RSS     AIC
    ## - rank           2    0.3928 114.57 -622.60
    ## - cls_profs      1    0.0024 114.18 -622.18
    ## - cls_level      1    0.0085 114.19 -622.16
    ## - cls_students   1    0.0588 114.24 -621.95
    ## - cls_did_eval   1    0.1314 114.31 -621.66
    ## - language       1    0.3661 114.54 -620.71
    ## <none>                       114.18 -620.19
    ## - age            1    1.1513 115.33 -617.54
    ## - cls_perc_eval  1    1.2535 115.43 -617.13
    ## - ethnicity      1    1.4231 115.60 -616.46
    ## - gender         1    3.1276 117.30 -609.68
    ## - bty_avg        1    3.3878 117.56 -608.65
    ## - cls_credits    1    4.7526 118.93 -603.31
    ## 
    ## Step:  AIC=-622.6
    ## score ~ ethnicity + gender + language + age + cls_perc_eval + 
    ##     cls_did_eval + cls_students + cls_level + cls_profs + cls_credits + 
    ##     bty_avg
    ## 
    ##                 Df Sum of Sq    RSS     AIC
    ## - cls_level      1    0.0020 114.57 -624.59
    ## - cls_profs      1    0.0037 114.57 -624.59
    ## - cls_students   1    0.0890 114.66 -624.24
    ## - cls_did_eval   1    0.1710 114.74 -623.91
    ## <none>                       114.57 -622.60
    ## - language       1    0.5680 115.14 -622.31
    ## - age            1    0.8990 115.47 -620.98
    ## - cls_perc_eval  1    1.2154 115.78 -619.71
    ## - ethnicity      1    1.6286 116.20 -618.06
    ## - gender         1    3.2321 117.80 -611.72
    ## - bty_avg        1    3.5172 118.09 -610.60
    ## - cls_credits    1    5.6074 120.18 -602.48
    ## 
    ## Step:  AIC=-624.59
    ## score ~ ethnicity + gender + language + age + cls_perc_eval + 
    ##     cls_did_eval + cls_students + cls_profs + cls_credits + bty_avg
    ## 
    ##                 Df Sum of Sq    RSS     AIC
    ## - cls_profs      1    0.0034 114.58 -626.58
    ## - cls_students   1    0.1012 114.67 -626.18
    ## - cls_did_eval   1    0.1862 114.76 -625.84
    ## <none>                       114.57 -624.59
    ## - language       1    0.5710 115.14 -624.29
    ## - age            1    0.8977 115.47 -622.98
    ## - cls_perc_eval  1    1.2223 115.79 -621.68
    ## - ethnicity      1    1.6628 116.23 -619.92
    ## - gender         1    3.2311 117.80 -613.72
    ## - bty_avg        1    3.5152 118.09 -612.60
    ## - cls_credits    1    6.1340 120.71 -602.44
    ## 
    ## Step:  AIC=-626.58
    ## score ~ ethnicity + gender + language + age + cls_perc_eval + 
    ##     cls_did_eval + cls_students + cls_credits + bty_avg
    ## 
    ##                 Df Sum of Sq    RSS     AIC
    ## - cls_students   1    0.1045 114.68 -628.16
    ## - cls_did_eval   1    0.1921 114.77 -627.80
    ## <none>                       114.58 -626.58
    ## - language       1    0.5710 115.15 -626.28
    ## - age            1    0.9013 115.48 -624.95
    ## - cls_perc_eval  1    1.2242 115.80 -623.66
    ## - ethnicity      1    1.7458 116.32 -621.58
    ## - gender         1    3.2310 117.81 -615.70
    ## - bty_avg        1    3.5121 118.09 -614.60
    ## - cls_credits    1    6.3135 120.89 -603.74
    ## 
    ## Step:  AIC=-628.16
    ## score ~ ethnicity + gender + language + age + cls_perc_eval + 
    ##     cls_did_eval + cls_credits + bty_avg
    ## 
    ##                 Df Sum of Sq    RSS     AIC
    ## - cls_did_eval   1    0.4446 115.12 -628.36
    ## <none>                       114.68 -628.16
    ## - language       1    0.5418 115.22 -627.97
    ## - age            1    0.8836 115.56 -626.60
    ## - ethnicity      1    1.8576 116.54 -622.72
    ## - gender         1    3.1442 117.82 -617.63
    ## - bty_avg        1    3.5015 118.18 -616.23
    ## - cls_perc_eval  1    3.5358 118.22 -616.10
    ## - cls_credits    1    6.2995 120.98 -605.40
    ## 
    ## Step:  AIC=-628.36
    ## score ~ ethnicity + gender + language + age + cls_perc_eval + 
    ##     cls_credits + bty_avg
    ## 
    ##                 Df Sum of Sq    RSS     AIC
    ## <none>                       115.12 -628.36
    ## - language       1    0.6192 115.74 -627.88
    ## - age            1    0.9342 116.06 -626.62
    ## - ethnicity      1    1.8997 117.02 -622.79
    ## - cls_perc_eval  1    3.1769 118.30 -617.76
    ## - gender         1    3.4709 118.59 -616.61
    ## - bty_avg        1    4.0096 119.13 -614.51
    ## - cls_credits    1    6.1046 121.23 -606.44

``` r
# View the summary of the final model
summary(final_model)
```

    ## 
    ## Call:
    ## stats::lm(formula = score ~ ethnicity + gender + language + age + 
    ##     cls_perc_eval + cls_credits + bty_avg, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9067 -0.3103  0.0849  0.3712  1.0611 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.446967   0.203191  16.964  < 2e-16 ***
    ## ethnicitynot minority  0.204710   0.074710   2.740 0.006384 ** 
    ## gendermale             0.184780   0.049889   3.704 0.000238 ***
    ## languagenon-english   -0.161463   0.103213  -1.564 0.118427    
    ## age                   -0.005008   0.002606  -1.922 0.055289 .  
    ## cls_perc_eval          0.005094   0.001438   3.543 0.000436 ***
    ## cls_creditsone credit  0.515065   0.104860   4.912 1.26e-06 ***
    ## bty_avg                0.064996   0.016327   3.981 7.99e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.503 on 455 degrees of freedom
    ## Multiple R-squared:  0.1576, Adjusted R-squared:  0.1446 
    ## F-statistic: 12.16 on 7 and 455 DF,  p-value: 2.879e-14

Score = 3.447 + 0.205(ethnicity-NM) + 0.185(gender-M) -
0.161(langauge-NE) - 0.005(age) + 0.005(cls_perc_eval) +
.51(cls_credits-1) + 0.065(bty_avg)

## Exercise 16

> Interpret the slopes of one numerical and one categorical predictor
> based on your final model.

Numerical – Age slope = .005 for every one year increase in age there is
a .005 decrease in score

Categorical – language slope = -.161 prof who do not speak English
receive a score .161 lower than those who speak English on average

## Exercise 17

> Based on your final model, describe the characteristics of a professor
> and course at University of Texas at Austin that would be associated
> with a high evaluation score.

High eval scores are associated with prof who are:

- non-minorities
- males
- English speaking
- younger
- have a greater number of evals completed
- teaching one class credits
- attractive

## Exercise 18

> Would you be comfortable generalizing your conclusions to apply to
> professors generally (at any university)? Why or why not?

I feel that many of these characteristics are likely to apply to other
schools, however, generalizability is limited

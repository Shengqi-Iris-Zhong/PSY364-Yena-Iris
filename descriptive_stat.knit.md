---
title: "Descriptive stats"
author: "Iris Zhong"
date: "3/31/2021"
output:
  pdf_document: default
  html_document: default
---




```r
library(tidyverse)
```

```
## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --
```

```
## v ggplot2 3.3.2     v purrr   0.3.4
## v tibble  3.0.4     v dplyr   1.0.2
## v tidyr   1.1.2     v stringr 1.4.0
## v readr   1.4.0     v forcats 0.5.0
```

```
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(tidyr)
library(mosaic)
```

```
## Warning: package 'mosaic' was built under R version 4.0.4
```

```
## Registered S3 method overwritten by 'mosaic':
##   method                           from   
##   fortify.SpatialPolygonsDataFrame ggplot2
```

```
## 
## The 'mosaic' package masks several functions from core packages in order to add 
## additional features.  The original behavior of these functions should not be affected by this.
```

```
## 
## Attaching package: 'mosaic'
```

```
## The following object is masked from 'package:Matrix':
## 
##     mean
```

```
## The following objects are masked from 'package:dplyr':
## 
##     count, do, tally
```

```
## The following object is masked from 'package:purrr':
## 
##     cross
```

```
## The following object is masked from 'package:ggplot2':
## 
##     stat
```

```
## The following objects are masked from 'package:stats':
## 
##     binom.test, cor, cor.test, cov, fivenum, IQR, median, prop.test,
##     quantile, sd, t.test, var
```

```
## The following objects are masked from 'package:base':
## 
##     max, mean, min, prod, range, sample, sum
```

```r
library(ggplot2)
library(here)
```

```
## Warning: package 'here' was built under R version 4.0.4
```

```
## here() starts at C:/Users/iris_/OneDrive/Desktop/Smith/Spring 2021/PSY 364/Proj/PSY364-Yena-Iris
```

```r
library(stargazer)
```

```
## 
## Please cite as:
```

```
##  Hlavac, Marek (2018). stargazer: Well-Formatted Regression and Summary Statistics Tables.
```

```
##  R package version 5.2.2. https://CRAN.R-project.org/package=stargazer
```

```r
library(psychTools)
```

```
## Warning: package 'psychTools' was built under R version 4.0.4
```

```
## 
## Attaching package: 'psychTools'
```

```
## The following object is masked from 'package:mosaic':
## 
##     read.file
```

```r
library(furniture)
```

```
## Warning: package 'furniture' was built under R version 4.0.4
```

\begin{table}[ ht ] 
\centering 
\caption{}\label{}
\begin{tabular}{ l c c c }
\toprule
 &   &  \multicolumn{ 2 }{c}{ telework }\\ 
  & Total & 0 & 1 \\ 
 & n = 364 & n = 129 & n = 235 \\ 
 \midrule
gender &   &   &  \\ 
\hspace{6pt}    0 & 179 (49.2\%) & 69 (53.5\%) & 110 (46.8\%)\\ 
\hspace{6pt}    1 & 185 (50.8\%) & 60 (46.5\%) & 125 (53.2\%)\\ 
\hspace{6pt}    \emph{missing} & 0 (0\%) & 0 (0\%) & 0 (0\%)\\ 
age &   &   &  \\ 
\hspace{6pt}   & 45.8 (8.4) & 46.7 (8.1) & 45.3 (8.5)\\ 
r\_years &   &   &  \\ 
\hspace{6pt}   & 18.4 (10.2) & 18.3 (11.5) & 18.5 (9.4)\\ 
race &   &   &  \\ 
\hspace{6pt}    Asian or Asian American & 37 (10.2\%) & 11 (8.5\%) & 26 (11.1\%)\\ 
\hspace{6pt}    Black or African American & 24 (6.6\%) & 11 (8.5\%) & 13 (5.5\%)\\ 
\hspace{6pt}    Latinx or Hispanic & 21 (5.8\%) & 5 (3.9\%) & 16 (6.8\%)\\ 
\hspace{6pt}    Middle Eastern & 4 (1.1\%) & 2 (1.6\%) & 2 (0.9\%)\\ 
\hspace{6pt}    Other & 1 (0.3\%) & 0 (0\%) & 1 (0.4\%)\\ 
\hspace{6pt}    Prefer not to answer & 4 (1.1\%) & 1 (0.8\%) & 3 (1.3\%)\\ 
\hspace{6pt}    White or European American & 269 (73.9\%) & 97 (75.2\%) & 172 (73.2\%)\\ 
\hspace{6pt}    White or European American,Latinx or Hispanic & 4 (1.1\%) & 2 (1.6\%) & 2 (0.9\%)\\ 
\hspace{6pt}    \emph{missing} & 0 (0\%) & 0 (0\%) & 0 (0\%)\\ 
childnum &   &   &  \\ 
\hspace{6pt}   & 0.8 (1.3) & 0.7 (1.0) & 0.9 (1.5)\\ 
income &   &   &  \\ 
\hspace{6pt}   & 76,618.8 (105,704.1) & 62,494.7 (66,422.0) & 81,760.8 (116,478.4)\\ 
\bottomrule

\end{tabular}
\end{table}

```r
# import premeasures
prem <- read.csv("TeleCom_Dyad_premeasures.csv",fileEncoding = 'UTF-8-BOM')
# import daily diary
diary <- read.csv("TeleCom_Dyad_dailydiary.csv")
```





```r
prem$r_length <- as.Date(as.character(prem$RecordedDate), format="%m/%d/%y")-
                  as.Date(as.character(prem$relation_length), format="%m/%d/%y")

prem$r_years <- as.numeric(prem$r_length) 
prem <- prem %>%
  mutate(r_years = r_years/365)
```



```r
prem_select <- prem %>%
  dplyr::select(partID = ResponseId, dyadID, gender, telework = Q139, age = birthday, r_years, race, childnum = people_5_TEXT, income = indinc)
```


```r
prem_select <- prem_select %>%
  mutate(telework = case_when(
    telework == "Yes, I am teleworking but my partner is not" ~ 1,
    telework == "Yes, my partner is teleworking but I am not" ~ 0,
    telework == "We are both teleworking" ~ 1),
    childnum = ifelse(is.na(childnum) == T, 0, childnum),
    gender = case_when(gender == "Woman" ~ 1,
                       gender == "Woman,Cis gendered" ~ 1,
                       gender == "Man" ~ 0,
                       gender == "Man,Cis gendered" ~ 0))
```


```r
prem_select %>%
  group_by(dyadID) %>%
  mutate(gender_sum = sum(gender)) %>%
  filter(gender_sum != 1)
```

```
## # A tibble: 18 x 10
## # Groups:   dyadID [9]
##    partID dyadID gender telework   age r_years race  childnum  income gender_sum
##    <chr>   <int>  <dbl>    <dbl> <int>   <dbl> <chr>    <dbl>   <int>      <dbl>
##  1 R_1Fr~ 2.90e7      1        1    35    15.6 Whit~        1   80000          2
##  2 R_cZs~ 2.90e7      1        1    35    15.6 Whit~        0      NA          2
##  3 R_56W~ 2.91e7      0        1    59    26.0 Whit~        0   62000          0
##  4 R_3JC~ 2.91e7      0        0    54    26.0 Whit~        0      NA          0
##  5 R_29s~ 2.94e7      1        1    50    26.9 Asia~        0  145000          2
##  6 R_VPd~ 2.94e7      1        1    50    26.9 Asia~        0  145000          2
##  7 R_1pX~ 2.95e7      0        1    55    43.0 Whit~        0   65000          0
##  8 R_pQS~ 2.95e7      0        1    55    43.2 Whit~        0   50000          0
##  9 R_1LF~ 3.29e7      1        1    55    37.4 Blac~        0      NA          2
## 10 R_2eP~ 3.29e7      1        0    55    38.2 Blac~        2   76000          2
## 11 R_1DA~ 3.34e7      1        0    37    17.5 Midd~        4      NA          2
## 12 R_2uP~ 3.34e7      1        0    37    17.5 Midd~        4      NA          2
## 13 R_3rY~ 3.36e7      0        1    40    19.5 Whit~        1  175000          0
## 14 R_3Lj~ 3.36e7      0        1    40    19.5 Whit~        1  175000          0
## 15 R_OIl~ 3.37e7      1        1    40    20.6 Lati~        0 1490000          2
## 16 R_3fA~ 3.37e7      1        1    40    20.6 Lati~        0  149000          2
## 17 R_11h~ 3.37e7      1        0    36    11.1 Whit~        2      NA          2
## 18 R_aWc~ 3.37e7      1        0    36    11.1 Whit~        2      NA          2
```


```r
prem_select <- prem_select %>%
  mutate(telework = as.factor(telework),
         gender = as.factor(gender))
```














\begin{table}[!htbp] \centering 
  \caption{} 
  \label{} 
\begin{tabular}{@{\extracolsep{5pt}}lccccccc} 
\\[-1.8ex]\hline 
\hline \\[-1.8ex] 
Statistic & \multicolumn{1}{c}{N} & \multicolumn{1}{c}{Mean} & \multicolumn{1}{c}{St. Dev.} & \multicolumn{1}{c}{Min} & \multicolumn{1}{c}{Pctl(25)} & \multicolumn{1}{c}{Pctl(75)} & \multicolumn{1}{c}{Max} \\ 
\hline \\[-1.8ex] 
dyadID & 364 & 26,018,185.000 & 9,455,389.000 & 377,033 & 28,947,773 & 31,423,023 & 33,735,733 \\ 
age & 364 & 45.802 & 8.350 & 26 & 39 & 52 & 74 \\ 
r\_years & 360 & 18.448 & 10.192 & $-$48.074 & 11.813 & 23.205 & 43.186 \\ 
childnum & 364 & 0.830 & 1.346 & 0 & 0 & 1 & 12 \\ 
income & 296 & 76,618.800 & 105,704.100 & 0.000 & 37,500.000 & 92,000.000 & 1,490,000.000 \\ 
\hline \\[-1.8ex] 
\end{tabular} 
\end{table} 
Show in New WindowClear OutputExpand/Collapse Output
\begin{table}[ ht ] 
\centering 
\caption{}\label{}
\begin{tabular}{ l c c }
\toprule
 &   \multicolumn{ 2 }{c}{ telework }\\ 
  & 0 & 1 \\ 
 & n = 79 & n = 216 \\ 
 \midrule
gender &   &  \\ 
\hspace{6pt}    0 & 52 (65.8\%) & 101 (46.8\%)\\ 
\hspace{6pt}    1 & 27 (34.2\%) & 115 (53.2\%)\\ 
age &   &  \\ 
\hspace{6pt}   & 46.7 (8.0) & 45.2 (8.5)\\ 
r\_years &   &  \\ 
\hspace{6pt}   & 17.6 (12.5) & 18.5 (9.4)\\ 
race &   &  \\ 
\hspace{6pt}    Asian or Asian American & 6 (7.6\%) & 25 (11.6\%)\\ 
\hspace{6pt}    Black or African American & 7 (8.9\%) & 12 (5.6\%)\\ 
\hspace{6pt}    Latinx or Hispanic & 4 (5.1\%) & 14 (6.5\%)\\ 
\hspace{6pt}    Middle Eastern & 0 (0\%) & 2 (0.9\%)\\ 
\hspace{6pt}    Other & 0 (0\%) & 1 (0.5\%)\\ 
\hspace{6pt}    Prefer not to answer & 0 (0\%) & 2 (0.9\%)\\ 
\hspace{6pt}    White or European American & 61 (77.2\%) & 159 (73.6\%)\\ 
\hspace{6pt}    White or European American,Latinx or Hispanic & 1 (1.3\%) & 1 (0.5\%)\\ 
childnum &   &  \\ 
\hspace{6pt}   & 0.7 (0.9) & 0.9 (1.5)\\ 
income &   &  \\ 
\hspace{6pt}   & 62,494.7 (66,422.0) & 81,768.9 (116,748.9)\\ 
\bottomrule

\end{tabular}
\end{table}





```r
#df2latex(summary_tbl)
```

\begin{table}[ ht ] 
\centering 
\caption{}\label{}
\begin{tabular}{ l c c }
\toprule
 &   \multicolumn{ 2 }{c}{ gender_chr }\\ 
  & M & W \\ 
 & n = 81 & n = 81 \\ 
 \midrule
gender\_chr &   &  \\ 
\hspace{6pt}    M & 81 (100\%) & 0 (0\%)\\ 
\hspace{6pt}    W & 0 (0\%) & 81 (100\%)\\ 
\hspace{6pt}    \emph{missing} & 0 (0\%) & 0 (0\%)\\ 
age &   &  \\ 
\hspace{6pt}   & 46.6 (8.5) & 44.5 (7.6)\\ 
telework &   &  \\ 
\hspace{6pt}    0 & 37 (45.7\%) & 23 (28.4\%)\\ 
\hspace{6pt}    1 & 44 (54.3\%) & 58 (71.6\%)\\ 
\hspace{6pt}    \emph{missing} & 0 (0\%) & 0 (0\%)\\ 
r\_years &   &  \\ 
\hspace{6pt}   & 18.2 (9.9) & 18.2 (9.9)\\ 
childnum &   &  \\ 
\hspace{6pt}   & 0.8 (0.9) & 0.8 (0.9)\\ 
income &   &  \\ 
\hspace{6pt}   & 70,698.6 (41,079.3) & 64,933.8 (91,925.6)\\ 
grbs\_ss &   &  \\ 
\hspace{6pt}   & 2.9 (0.6) & 2.6 (0.7)\\ 
race &   &  \\ 
\hspace{6pt}    Asian or Asian American & 6 (7.4\%) & 8 (9.9\%)\\ 
\hspace{6pt}    Black or African American & 6 (7.4\%) & 6 (7.4\%)\\ 
\hspace{6pt}    Latinx or Hispanic & 5 (6.2\%) & 6 (7.4\%)\\ 
\hspace{6pt}    Prefer not to answer & 2 (2.5\%) & 0 (0\%)\\ 
\hspace{6pt}    White or European American & 62 (76.5\%) & 61 (75.3\%)\\ 
\hspace{6pt}    \emph{missing} & 0 (0\%) & 0 (0\%)\\ 

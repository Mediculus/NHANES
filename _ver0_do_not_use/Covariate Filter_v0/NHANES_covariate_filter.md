Directed EDA for rnhanes Package
================
Kevin S.W. — UNI: ksw2137
10/24/2020

# Filtering Covariate Data of Interest

Since we’ve explored almost all possible datasets that can be found in
the `rnhanesdata` package, we can now make a more targeted approach to
which dataset we want to work on as well as obtaining particular raw
data information. First though, we need to load the package as always.

``` r
library(rnhanesdata)
```

## Picking which NHANES Data

The original package has 2 waves, C and D, that corresponds to surveys
sent out between 2003-2004 and 2005-2006, respectively. Working with
wave D has advantage because not only is the data more recent, it also
has more “raw” observations and thus potentially more datapoints we
could work with.

``` r
covar_data_d <- as_tibble(
  process_covar(
    waves = "D", extractAll = TRUE
    )[[1]] # process_covar outputs a list of 1 containing the tibble, this extracts the "content" and 
  ) %>%    # turn it into tibble
  janitor::clean_names()

covar_names <- names(covar_data_d)
```

We can then filter out these observations to only include those that may
be useful for accelerometer and flag data (in case we ever want to
explore relationships between these variables).

First, we load up the flag data, `Flags_D` to obtain the unique ID of
each participants (`seqn`). The reason why we used flag data is because
we only wanted participants who have information on their wearable
status (whether it is “reliable” or not at a given minute/time).

``` r
identifier <- Flags_D %>% 
  janitor::clean_names() %>% 
  distinct(seqn) %>% 
  mutate(
    seqn = as.integer(seqn)
  )

covar_data_d <- left_join(identifier, covar_data_d, by = 'seqn')
```

## Filtering our Covariates Data

In another document, we’ve succesfully created a .csv file that
contained all the variables of interest with respect to variables that
might be associated with cardiovascular health. We can use that .csv
file to filter our `covar_data_d`, which will then allow us to proceed
in analyzing the data.

``` r
variable_list <- read_csv("./Datasets/variable_list.csv")

covar_d_clean <- select_if(covar_data_d, 
                           names(covar_data_d) %in% pull(variable_list, var_name))
```

## Exploring Data Frame

We would like to explore our clean data now and firstly check the amount
of `NA` in each column.

``` r
na_check_df <- covar_d_clean %>% 
  map_df(~sum(is.na(.))) %>% 
  pivot_longer(2:103,
               names_to = "var_name",
               values_to = "na_count") %>% 
  arrange(desc(na_count)) 
```

Upon arranging the resulting `NA` count into a descending order,
something peculiar showed, which is that the variables `padtimes`,
`padactiv`, and `padlevel` from the package appear to have more `NA`
compared to the number of `seqn` (respondents). Investigating it further
leads to the finding that this particular variable is saved as a vector
variable, containing many more observations. Thankfully, this is likely
to be redundant variable when we consider including our activity level
data from the package. Thus decision was made to exclude these.

``` r
na_check_df <- na_check_df %>% 
  filter(!(var_name %in% c("padtimes", "padactiv", "padlevel")))
```

## Re-selecting Viable Variables for Data Analysis

Now that we’ve set a level of acceptable responses, we could revisit our
variables and be more selective.

``` r
clean_variable_list <- left_join(na_check_df, variable_list, by = "var_name")
```

Revising the variables, these are the ones we decide to NOT include:

  - mcq010: asthma dx
  - mcq025: age of 1st asthma
  - mcq035: still have asthma currently
  - mcq300a: close relative had MI?
  - mcq300b: close relative had asthma?
  - mcq300c: close relative had diabetes?
  - bmxsub: subscapular skinfold (mm)
  - dmdeduc3: education level - children 6-19
  - dmdfmsiz: total \# of people in the family
  - sdmvpsu: masked variance pseudo-psu
  - sdmvstra: masked variance pseuda-stratum

<!-- end list -->

``` r
filter_remove_list <- 
  c("mcq010", "mcq025", "mcq035", "mcq300a", "mcq300b", "mcq300c", "bmxsub", "dmdfmsiz", "sdmvpsu", "sdmvstra")

clean_variable_list <- clean_variable_list %>% 
  filter(!(str_detect(var_name, 
                      paste(filter_remove_list,            
                            collapse = "|"))))
```

We then further re-clean our `covar_d_clean` dataframe

``` r
covar_d_clean <- select_if(covar_data_d, 
                           names(covar_data_d) %in% c("seqn", pull(clean_variable_list, var_name)))
```

We now check for any distributions, min-max and so on using `skimr`

``` r
skimr::skim(covar_d_clean)
```

|                                                  |                 |
| :----------------------------------------------- | :-------------- |
| Name                                             | covar\_d\_clean |
| Number of rows                                   | 7455            |
| Number of columns                                | 90              |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                 |
| Column type frequency:                           |                 |
| numeric                                          | 90              |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                 |
| Group variables                                  | None            |

Data summary

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |     mean |       sd |       p0 |      p25 |      p50 |      p75 |      p100 | hist  |
| :------------- | ---------: | -------------: | -------: | -------: | -------: | -------: | -------: | -------: | --------: | :---- |
| seqn           |          0 |           1.00 | 36297.92 |  2989.74 | 31128.00 | 33721.50 | 36296.00 | 38877.50 |  41474.00 | ▇▇▇▇▇ |
| alq120q        |       3953 |           0.47 |     5.64 |    44.90 |     0.00 |     1.00 |     2.00 |     4.00 |    999.00 | ▇▁▁▁▁ |
| alq120u        |       4820 |           0.35 |     1.91 |     0.86 |     1.00 |     1.00 |     2.00 |     3.00 |      3.00 | ▇▁▅▁▆ |
| bmxwt          |         17 |           1.00 |    70.82 |    25.62 |    16.20 |    54.70 |    70.10 |    85.50 |    371.00 | ▇▂▁▁▁ |
| bmxht          |         18 |           1.00 |   162.31 |    15.37 |   105.20 |   155.50 |   164.10 |   172.50 |    204.10 | ▁▂▇▇▁ |
| bmxbmi         |         30 |           1.00 |    26.17 |     7.34 |    11.98 |    20.98 |    25.35 |    30.21 |    130.21 | ▇▁▁▁▁ |
| bmxcalf        |        542 |           0.93 |    37.25 |     5.15 |    20.20 |    34.00 |    37.10 |    40.30 |     75.60 | ▁▇▂▁▁ |
| bmxarmc        |        154 |           0.98 |    30.24 |     6.34 |    14.40 |    26.00 |    30.20 |    34.30 |     62.40 | ▂▇▃▁▁ |
| bmxwaist       |        192 |           0.97 |    89.37 |    19.46 |    45.40 |    75.00 |    88.90 |   102.50 |    175.00 | ▃▇▅▁▁ |
| bmxthicr       |        609 |           0.92 |    51.30 |     8.24 |    28.30 |    46.00 |    50.80 |    56.00 |     93.90 | ▂▇▃▁▁ |
| bmxtri         |        680 |           0.91 |    17.35 |     8.30 |     3.00 |    10.60 |    16.00 |    23.00 |     45.00 | ▇▇▅▂▁ |
| peascst1       |          0 |           1.00 |     1.06 |     0.34 |     1.00 |     1.00 |     1.00 |     1.00 |      3.00 | ▇▁▁▁▁ |
| bpxchr         |       7117 |           0.05 |    88.74 |    14.02 |    58.00 |    82.00 |    88.00 |    96.00 |    240.00 | ▇▃▁▁▁ |
| bpxpls         |        547 |           0.93 |    75.40 |    13.04 |    40.00 |    66.00 |    74.00 |    84.00 |    220.00 | ▇▆▁▁▁ |
| bpxpuls        |        225 |           0.97 |     1.03 |     0.16 |     1.00 |     1.00 |     1.00 |     1.00 |      2.00 | ▇▁▁▁▁ |
| bpxpty         |        541 |           0.93 |     1.00 |     0.03 |     1.00 |     1.00 |     1.00 |     1.00 |      2.00 | ▇▁▁▁▁ |
| bpxsy1         |       1260 |           0.83 |   118.99 |    18.75 |    74.00 |   106.00 |   116.00 |   128.00 |    270.00 | ▆▇▁▁▁ |
| bpxdi1         |       1260 |           0.83 |    64.97 |    14.54 |     0.00 |    56.00 |    66.00 |    74.00 |    124.00 | ▁▁▇▂▁ |
| bpxsy2         |       1633 |           0.78 |   118.01 |    18.13 |    72.00 |   106.00 |   114.00 |   126.00 |    232.00 | ▃▇▂▁▁ |
| bpxdi2         |       1633 |           0.78 |    65.30 |    13.91 |     0.00 |    58.00 |    66.00 |    74.00 |    120.00 | ▁▁▇▃▁ |
| bpxsy3         |       1787 |           0.76 |   117.39 |    17.54 |    78.00 |   106.00 |   114.00 |   126.00 |    224.00 | ▃▇▂▁▁ |
| bpxdi3         |       1787 |           0.76 |    65.22 |    13.63 |     0.00 |    58.00 |    66.00 |    74.00 |    118.00 | ▁▁▇▅▁ |
| bpxsy4         |       6351 |           0.15 |   119.40 |    18.04 |    76.00 |   108.00 |   116.00 |   128.00 |    222.00 | ▂▇▂▁▁ |
| bpxdi4         |       6351 |           0.15 |    65.77 |    15.47 |     0.00 |    56.00 |    66.00 |    76.00 |    124.00 | ▁▂▇▃▁ |
| sddsrvyr       |          0 |           1.00 |     4.00 |     0.00 |     4.00 |     4.00 |     4.00 |     4.00 |      4.00 | ▁▁▇▁▁ |
| ridstatr       |          0 |           1.00 |     2.00 |     0.00 |     2.00 |     2.00 |     2.00 |     2.00 |      2.00 | ▁▁▇▁▁ |
| riagendr       |          0 |           1.00 |     1.52 |     0.50 |     1.00 |     1.00 |     2.00 |     2.00 |      2.00 | ▇▁▁▁▇ |
| ridageyr       |          0 |           1.00 |    33.72 |    22.61 |     6.00 |    15.00 |    27.00 |    51.00 |     85.00 | ▇▃▃▂▂ |
| ridreth1       |          0 |           1.00 |     2.83 |     1.21 |     1.00 |     1.00 |     3.00 |     4.00 |      5.00 | ▅▁▇▆▁ |
| dmdeduc3       |       4373 |           0.41 |     7.46 |     7.35 |     0.00 |     4.00 |     7.00 |    10.00 |     99.00 | ▇▁▁▁▁ |
| dmdeduc2       |       3083 |           0.59 |     3.29 |     1.29 |     1.00 |     2.00 |     3.00 |     4.00 |      9.00 | ▅▇▃▁▁ |
| indfminc       |         59 |           0.99 |     8.48 |    11.32 |     1.00 |     5.00 |     7.00 |    10.25 |     99.00 | ▇▁▁▁▁ |
| indfmpir       |        318 |           0.96 |     2.48 |     1.59 |     0.00 |     1.09 |     2.12 |     3.85 |      5.00 | ▇▇▅▅▇ |
| wtint2yr       |          0 |           1.00 | 32018.74 | 28660.37 |  1339.05 |  7966.76 | 23310.39 | 44966.45 | 152162.42 | ▇▃▂▁▁ |
| wtmec2yr       |          0 |           1.00 | 33202.40 | 29607.84 |  1363.17 |  8194.87 | 24099.64 | 46564.73 | 156152.18 | ▇▃▂▁▁ |
| diq010         |          0 |           1.00 |     1.96 |     0.32 |     1.00 |     2.00 |     2.00 |     2.00 |      9.00 | ▇▁▁▁▁ |
| did040         |       6995 |           0.06 |    59.16 |    94.04 |     3.00 |    40.00 |    50.00 |    60.25 |    999.00 | ▇▁▁▁▁ |
| diq220         |       7352 |           0.01 |     3.97 |     1.45 |     1.00 |     3.00 |     5.00 |     5.00 |      5.00 | ▂▁▁▂▇ |
| diq160         |       1594 |           0.79 |     1.99 |     0.41 |     1.00 |     2.00 |     2.00 |     2.00 |      9.00 | ▇▁▁▁▁ |
| diq170         |       1502 |           0.80 |     1.90 |     0.53 |     1.00 |     2.00 |     2.00 |     2.00 |      9.00 | ▇▁▁▁▁ |
| diq180         |       1502 |           0.80 |     1.94 |     1.49 |     1.00 |     1.00 |     2.00 |     2.00 |      9.00 | ▇▁▁▁▁ |
| diq190a        |       1045 |           0.86 |     1.81 |     0.45 |     1.00 |     2.00 |     2.00 |     2.00 |      9.00 | ▇▁▁▁▁ |
| diq190b        |       1045 |           0.86 |     1.75 |     0.47 |     1.00 |     1.00 |     2.00 |     2.00 |      9.00 | ▇▁▁▁▁ |
| diq190c        |       1045 |           0.86 |     1.78 |     0.50 |     1.00 |     2.00 |     2.00 |     2.00 |      9.00 | ▇▁▁▁▁ |
| diq200a        |       1045 |           0.86 |     1.60 |     0.53 |     1.00 |     1.00 |     2.00 |     2.00 |      9.00 | ▇▁▁▁▁ |
| diq200b        |       1045 |           0.86 |     1.60 |     0.53 |     1.00 |     1.00 |     2.00 |     2.00 |      9.00 | ▇▁▁▁▁ |
| diq200c        |       1045 |           0.86 |     1.61 |     0.53 |     1.00 |     1.00 |     2.00 |     2.00 |      9.00 | ▇▁▁▁▁ |
| diq050         |          0 |           1.00 |     1.98 |     0.14 |     1.00 |     2.00 |     2.00 |     2.00 |      2.00 | ▁▁▁▁▇ |
| did060         |       7314 |           0.02 |    33.43 |   140.12 |     1.00 |     3.00 |     9.00 |    15.00 |    999.00 | ▇▁▁▁▁ |
| diq060u        |       7318 |           0.02 |     1.91 |     0.29 |     1.00 |     2.00 |     2.00 |     2.00 |      2.00 | ▁▁▁▁▇ |
| did070         |       6731 |           0.10 |     1.57 |     0.69 |     1.00 |     1.00 |     2.00 |     2.00 |      9.00 | ▇▁▁▁▁ |
| diq230         |       6995 |           0.06 |     2.91 |     1.78 |     1.00 |     1.00 |     3.00 |     5.00 |      9.00 | ▇▃▅▁▁ |
| diq240         |       6995 |           0.06 |     1.19 |     0.39 |     1.00 |     1.00 |     1.00 |     1.00 |      2.00 | ▇▁▁▁▂ |
| did250         |       7081 |           0.05 |     5.03 |     8.26 |     0.00 |     2.00 |     4.00 |     5.00 |    104.00 | ▇▁▁▁▁ |
| did260         |       6995 |           0.06 |    10.15 |    86.05 |     0.00 |     1.00 |     2.00 |     3.00 |    999.00 | ▇▁▁▁▁ |
| diq260u        |       7060 |           0.05 |     1.51 |     0.82 |     1.00 |     1.00 |     1.00 |     2.00 |      4.00 | ▇▂▁▁▁ |
| did270         |       6999 |           0.06 |   301.03 |   403.91 |     0.00 |     2.00 |     4.00 |   666.00 |    999.00 | ▇▁▁▂▂ |
| diq280         |       7125 |           0.04 |   647.40 |   475.13 |     5.00 |     7.70 |   999.00 |   999.00 |    999.00 | ▅▁▁▁▇ |
| diq290         |       7125 |           0.04 |    37.97 |    46.24 |     1.00 |     2.00 |     6.00 |    99.00 |     99.00 | ▇▁▁▁▅ |
| diq300s        |       6998 |           0.06 |  4277.70 |  4875.29 |    78.00 |   129.00 |   150.00 |  9999.00 |   9999.00 | ▇▁▁▁▆ |
| diq300d        |       6998 |           0.06 |  4483.90 |  4935.84 |     0.00 |    73.00 |    91.00 |  9999.00 |   9999.00 | ▇▁▁▁▆ |
| did310s        |       6998 |           0.06 |  5372.10 |  3867.52 |    70.00 |   130.00 |  6666.00 |  6666.00 |   9999.00 | ▆▁▁▇▅ |
| did310d        |       6998 |           0.06 |  5486.17 |  3876.67 |     2.00 |    80.00 |  6666.00 |  9999.00 |   9999.00 | ▆▁▁▇▅ |
| did320         |       6998 |           0.06 |  7502.04 |  3743.07 |    25.00 |  5555.00 |  9999.00 |  9999.00 |   9999.00 | ▂▁▂▁▇ |
| did330         |       7083 |           0.05 |  7659.85 |  3238.73 |    50.00 |  6666.00 |  9999.00 |  9999.00 |   9999.00 | ▂▁▁▅▇ |
| did340         |       7034 |           0.06 |    27.07 |   487.23 |     0.00 |     0.00 |     1.00 |     4.00 |   9999.00 | ▇▁▁▁▁ |
| did350         |       7034 |           0.06 |    25.91 |   487.54 |     0.00 |     1.00 |     1.00 |     1.00 |   9999.00 | ▇▁▁▁▁ |
| diq350u        |       7098 |           0.05 |     1.46 |     0.76 |     1.00 |     1.00 |     1.00 |     2.00 |      4.00 | ▇▂▁▁▁ |
| diq360         |       6998 |           0.06 |     2.75 |     1.41 |     1.00 |     2.00 |     2.00 |     4.00 |      9.00 | ▇▃▁▁▁ |
| diq080         |       6998 |           0.06 |     1.82 |     0.80 |     1.00 |     2.00 |     2.00 |     2.00 |      9.00 | ▇▁▁▁▁ |
| mcq080         |       2070 |           0.72 |     1.72 |     0.50 |     1.00 |     1.00 |     2.00 |     2.00 |      9.00 | ▇▁▁▁▁ |
| mcq160b        |       3083 |           0.59 |     1.98 |     0.38 |     1.00 |     2.00 |     2.00 |     2.00 |      9.00 | ▇▁▁▁▁ |
| mcq160c        |       3083 |           0.59 |     1.99 |     0.51 |     1.00 |     2.00 |     2.00 |     2.00 |      9.00 | ▇▁▁▁▁ |
| mcq160d        |       3083 |           0.59 |     1.99 |     0.43 |     1.00 |     2.00 |     2.00 |     2.00 |      9.00 | ▇▁▁▁▁ |
| mcq160e        |       3083 |           0.59 |     1.97 |     0.35 |     1.00 |     2.00 |     2.00 |     2.00 |      9.00 | ▇▁▁▁▁ |
| mcq160f        |       3083 |           0.59 |     1.97 |     0.30 |     1.00 |     2.00 |     2.00 |     2.00 |      9.00 | ▇▁▁▁▁ |
| mcq160g        |       3083 |           0.59 |     1.99 |     0.29 |     1.00 |     2.00 |     2.00 |     2.00 |      9.00 | ▇▁▁▁▁ |
| mcq180b        |       7303 |           0.02 |  1372.92 | 11426.02 |     1.00 |    49.75 |    60.00 |    70.00 |  99999.00 | ▇▁▁▁▁ |
| mcq180c        |       7283 |           0.02 |  1801.77 | 13121.49 |     3.00 |    50.00 |    60.00 |    70.25 |  99999.00 | ▇▁▁▁▁ |
| mcq180d        |       7317 |           0.02 |   777.84 |  8507.94 |     7.00 |    41.00 |    55.50 |    66.00 |  99999.00 | ▇▁▁▁▁ |
| mcq180e        |       7266 |           0.03 |  2701.70 | 16081.59 |     2.00 |    46.00 |    58.00 |    71.00 |  99999.00 | ▇▁▁▁▁ |
| mcq180f        |       7301 |           0.02 |  3953.19 | 19401.62 |     4.00 |    49.00 |    63.00 |    75.00 |  99999.00 | ▇▁▁▁▁ |
| mcq180g        |       7369 |           0.01 |  3543.34 | 18445.45 |     4.00 |    46.50 |    60.50 |    70.00 |  99999.00 | ▇▁▁▁▁ |
| pfq020         |       4373 |           0.41 |     1.95 |     0.26 |     1.00 |     2.00 |     2.00 |     2.00 |      9.00 | ▇▁▁▁▁ |
| pfq030         |       7287 |           0.02 |     1.46 |     1.39 |     1.00 |     1.00 |     1.00 |     2.00 |      9.00 | ▇▁▁▁▁ |
| smq020         |       3083 |           0.59 |     1.53 |     0.53 |     1.00 |     1.00 |     2.00 |     2.00 |      9.00 | ▇▁▁▁▁ |
| smd030         |       5386 |           0.28 |    21.08 |    64.99 |     0.00 |    15.00 |    17.00 |    19.00 |    999.00 | ▇▁▁▁▁ |
| smq040         |       5386 |           0.28 |     2.16 |     0.95 |     1.00 |     1.00 |     3.00 |     3.00 |      3.00 | ▆▁▁▁▇ |
| smq050q        |       6339 |           0.15 |   287.23 |  5179.12 |     1.00 |     6.00 |    15.00 |    29.00 |  99999.00 | ▇▁▁▁▁ |
| smq050u        |       6342 |           0.15 |     3.84 |     0.48 |     1.00 |     4.00 |     4.00 |     4.00 |      4.00 | ▁▁▁▁▇ |

Once we check distributions and satisfied with results, we should also
make this into another dataset so that we don’t have to keep re-running
the scripts.

``` r
# covar_d_clean %>% write_csv("./Datasets/covariates.csv")
```

# Combining Covariates and Activity Data

**MOVE THIS PART TO A NEW RMD**

Now that we have covariates we can work with, it’s time to merge these
with our activity data. But first, we need to clean our activity records
using the flags dataset.

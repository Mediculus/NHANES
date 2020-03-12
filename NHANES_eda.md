NHANES EDA
================
Kevin S.W. — UNI: ksw2137
03/12/2020

# NHANES Dataset EDA

Purpose of this project is to explore what the dataset contains and if
it is workable. Further exploration will be decided based on these
possibilities.

## Loading Dataset

``` r
# devtools::install_github("andrew-leroux/rnhanesdata")
library(rnhanesdata)
```

Based on the vignette? provided by the author, it seems that there are 2
main “dataset-packs” contained in the package categorized below. Note
that all of these datasets are separated by each “waves” where `_C`
indicates wave 3 (2003-2004) while `_D` indicates wave 4 (2005-2006).

  - Processed:
      - [`PAXINTEN_C` & `PAXINTEN_D`](#accelerometer-data):
        accelerometry data for 2003-2004 and 2005-2006 respectively.
      - [`Flags_C` & `Flags_D`](#flags-data): Wear/non-wear flags for
        2003-2004 and 2005-2006 respectively.
      - [`Covariate_C` & `Covariate_D`](#covariate-data): Additional
        NHANES dataset containing other covariates recorded in the raw
        data for 2003-2004 and 2005-2006 respectively.
      - `Mortality_2011_C` & `Mortality_2011_D`: mortality info for
        2003-2004 and 2005-2006 respectively from NHANES database that
        were processed; released in 2011.
  - Raw:
      - `ALQ_C` & `ALQ_D`: alcohol consumption dataset from 2 “waves” of
        datasets (exists in Covar)
      - `BMX_C` & `BMX_D`: body measurement datasets from 2 waves.
      - `BPX_C` & `BPX_D`: blood pressure data from the 2 waves.
      - `DEMO_C` & `DEMO_D`: demographic data
      - `DIQ_C` & `DIQ_D`: diabetes questionnaire data
      - `MCQ_C` & `MCQ_D`: medical conditions questionnaire
      - `PFQ_C` & `PFQ_D`: physical function questionnaire data
      - `SMQ_C` & `SMQ_D`: smoking status questionnaire
      - `NHANES_2003-2004_MORT_2011_PUBLIC`
      - `NHANES_2005-2006_MORT_2011_PUBLIC`

We are going to explore these processed dataset and see what can be done
with it. Unfortunately, the raw data were inaccessible without using the
package’s functions, `process_covar`. As we’ll see later, we could use
this function to extract all possible covariates, meaning all the raw
.XPT files can be extracted except for the NHANES mortality data.

# Processed Data Exploration

## [Accelerometer Data](#loading-dataset)

``` r
# Data import for accelerometry data from wave C (2003-2004)
nested_accelC <- PAXINTEN_C %>% 
  janitor::clean_names() %>% 
#  pivot_longer(cols = starts_with("MIN"), 
#               names_to = "min",
#               names_prefix = "min",
#               values_to = "activ_count") %>% 
#  mutate(
#    min = as.numeric(min)
#  ) %>% 
  mutate_at(
    .vars = vars("seqn", "paxcal", "paxstat", "weekday", "sddsrvyr"),
    .funs = funs(factor)
  ) %>% 
  group_by(seqn) %>% 
  nest(accel_mins = min1:min1440) %>% 
  nest(wave_C = paxcal:accel_mins)
```

    ## Warning: funs() is soft deprecated as of dplyr 0.8.0
    ## Please use a list of either functions or lambdas: 
    ## 
    ##   # Simple named list: 
    ##   list(mean = mean, median = median)
    ## 
    ##   # Auto named with `tibble::lst()`: 
    ##   tibble::lst(mean, median)
    ## 
    ##   # Using lambdas
    ##   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## This warning is displayed once per session.

``` r
# Data import for accelerometry data from wave D (2005-2006)
nested_accelD <- PAXINTEN_D %>% 
  janitor::clean_names() %>% 
#  pivot_longer(cols = starts_with("MIN"), 
#               names_to = "min",
#               names_prefix = "min",
#               values_to = "activ_count") %>% 
#  mutate(
#    min = as.numeric(min)
#  ) %>% 
  mutate_at(
    .vars = vars("seqn", "paxcal", "paxstat", "weekday", "sddsrvyr"),
    .funs = funs(factor)
  ) %>% 
  group_by(seqn) %>% 
  nest(accel_mins = min1:min1440) %>% 
  nest(wave_D = paxcal:accel_mins)
```

Both of these datasets contain similar variables. Of primary focus:

  - `SEQN`: Unique ID
  - `PAXCAL`: Device calibration with 1 = yes, 2 = no, 9 = unknown
  - `PAXSTAT`: Data reliability status with 1 = reliable, 2 =
    questionable
  - `SDDSRVYR`: Indicates which “wave” the data comes from. 3 means from
    2003-2004, 4 = 2005-2006
  - `WEEKDAY`: Day of the week with 1 as Sunday, 7 as Saturday.
  - `MIN`: Activity count at each minute of the day.

Taking wave C’s data and checking each variable using skimr reveals that
paxcal has 3 unique values and paxstat has 2 values which suggests that
some of these values are either “uncalibrated”, “questionable”, or
“unknown”. This might require further filtering.

``` r
accelC %>% 
  select(-min) %>% 
  skimr::skim_without_charts()
```

We also merged wave C and D to see if all these data are continuously
recorded for the same people. Will utilize nesting as well to minimize
load on PC.

``` r
# combining 2 datasets based on seqn, the unique ID
joined_accel <- full_join(nested_accelC, nested_accelD, by = "seqn")
```

    ## Warning: Column `seqn` joining factors with different levels, coercing to
    ## character vector

``` r
# counts distinct seqn's based on the unique ID
joined_accel %>% 
  ungroup() %>% 
  distinct(seqn) %>% 
  count()
```

# A tibble: 1 x 1

``` 
  n
```

<int> 1 14631

Per our “nested” findings, it was surprising to see that the `seqn` are
all unique from wave C to D. In other words, we have a total of 14631
unique observations, which are all from different people. If we were to
just aggregate these, we could likely ignore the wave ID if time were
not considered.

**NOTE: Working with a single wave dataset may take up to 5GB of RAM**

The function `process_accel` seems to be able to interact with CDC’s
data directly and therefore capable of obtaining more recent data if
needed.

## [Flags Data](#loading-dataset)

``` r
flagC <- Flags_C %>% 
  janitor::clean_names() %>% 
#  pivot_longer(cols = starts_with("MIN"), 
#               names_to = "min",
#               names_prefix = "min",
#               values_to = "flag_indicator") %>% 
#  mutate(
#    min = as.numeric(min)
#  ) %>% 
  mutate_at(
    .vars = vars("seqn", "paxcal", "paxstat", "weekday", "sddsrvyr"),
    .funs = funs(factor)
  ) %>% 
  group_by(seqn) %>% 
  nest(flag_mins = min1:min1440) %>% 
  nest(wave_C = paxcal:flag_mins)


flagD <- Flags_D %>% 
  janitor::clean_names() %>% 
#  pivot_longer(cols = starts_with("MIN"), 
#               names_to = "min",
#               names_prefix = "min",
#               values_to = "flag_indicator") %>% 
#  mutate(
#    min = as.numeric(min)
#  ) %>% 
  mutate_at(
    .vars = vars("seqn", "paxcal", "paxstat", "weekday", "sddsrvyr"),
    .funs = funs(factor)
  ) %>% 
  group_by(seqn) %>% 
  nest(flag_mins = min1:min1440) %>% 
  nest(wave_D = paxcal:flag_mins)
```

The flags data appear to be a very detailed, minute-by-minute indicator
of whether or not the device was worn by that particular person as
identified by their `seqn`. These numbers are equivalent to the number
of data points observed in accelerometer. **Besides the minute tracking
of wear flags, other variables are the same as those found in
accelerometer data.**

We can also nest these data since “flag\_dataset” is capable of being
its own dataset. Since flag and accelerometer data are related, we could
probably join these datasets based on the unique `seqn` identifier and
have everything in 1 “table”

**NOTE: working with accelerometer AND flag data consumes a large amount
of RAM (~10 GB)**

The function `process_flags` from the package allows the “raw” data
obtained from `process_accel` and isolate just the wear variables.

## [Covariate Data](#loading-dataset)

Based on the vignette? it appears that the raw NHANES data beyond
accelerometry are collated within the “covariates data”. This will be
explored further below.

``` r
covarC <- Covariate_C %>% 
  janitor::clean_names() %>% 
  nest(wave_C_covar = sddsrvyr:smoke_cigs)
  

covarD <- Covariate_D %>% 
  janitor::clean_names() %>% 
  nest(wave_D_covar = sddsrvyr:smoke_cigs)


joined_covar <- full_join(covarC, covarD, by = 'seqn')
```

Unlike flag and accelerometer data, these covar data are not structured
similarly to the other two and therefore might benefit from being a
separate dataset. Unique identifier still exist and thus can be joined
if necessary. This will be easily possible if we can maintain nested
dataset of both flag and accelerometer.

Additionally, the amount of `seqn` observations in the covariates are
more than `seqn` within flag and accelerometer. With both wave C and D
combined, we have 20470 observations. We should left join this if we
plan to merge the datasets.

Furthermore, the covariates inside were not the complete covariates from
the NHANES raw data. In these “cleaned” dataset, covariates are:

  - `SDDSRVYR`: Wave indicator as in other datasets
  - `SDMVPSU`: Masked variance pseudo probability sampling units. Used
    for variance estimation??
  - `SDMVSTRA`: Masked variance pseudo stratum. Used for variance
    estimation??
  - `WTINT2YR`: Full sample interviewed weight (likely reported weight)
  - `WTMEC2YR`: Full sample examination weight (likely observed weight)
  - `RIDAGEMN`: Age in months at screening date for those \< 85 years.
    \>= 85 is coded as NA
  - `RIDAGEEX`: Age in months at examination date for those \< 85 years.
    \>= 85 is coded as NA
  - `RIDAGEYR`: Age in years at date of screening
  - `BMI`: BMI in kg/m^2. A copy of BMXBMI variable.
  - `BMI_cat`: Category for the BMI values. underweight (\<= 18.5),
    normal (18.5 \< x \<= 25), overweight (25 \< x \<= 30), obese (\>
    30)
  - Self-reported survey answers:
      - `Race`: ethnicity
      - `Gender`: gender for M/F
      - `Diabetes`: diagnosed diabetes, categorized to Yes, No,
        Borderline, Refused, Don’t know.
      - `CHF`: diagnosed CHF, categorized as yes, no, refused, don’t
        know (variable MCQ160B in raw)
      - `CHD`: diagnosed CHD, categorized as yes, no refused, don’t know
        (variable MCQ160C)
      - `Cancer`: diagnosed history of any cancer. Categorized as yes,
        no, refused, don’t know (MCQ220)
      - `Stroke`: diagnosed history of stroke, categorized as yes, no,
        refused, or don’t know. (MCQ160F)
      - `Education_adult`: Present only in wave D
      - `Mobility_Problem`: mobility issue categorized into any vs. no
        difficulty. Derived from responses of (PFQ-049, -054, -057,
        -059, -061B, -61C). Details in vignette.
      - `Drink_Status`: current alcohol consumption categorized as non-,
        moderate-, or heavy-drinker. Details in vignette
      - `Drinks_Per_Week`: number of drinks per week. Details in
        vignette
      - `Smoke_Cigs`: cigarette smoking status categorized as never,
        former, or current. Details in vignette.

## Mortality Data

Processed mortality data for NHANES, released in 2011 for waves C and D.

*Pending exploration*

# Raw Data Exploration

Based on the package, it appears that the raw NHANES data primarily
contributes to the covariate data. Using a function from within the
`rnhanesdata` package, `process_covar()`, we’re able to interact with
the raw NHANES data within the package and obtain all available
covariates.

``` r
# extract raw NHANES data and convert to tibble
raw_covar_data_C <- as_tibble(
  process_covar(
    waves = "C", extractAll = TRUE
    )[[1]] # process_covar outputs a list of 1 containing the tibble, this extracts the "content" and 
  ) %>%    # turn it into tibble
  janitor::clean_names()
```

| | | 0% Variables with repeated observations per subject found for the
following variables: PADACTIV,PADLEVEL,PADTIMES,PADDURAT,PADMETS,PAAQUEX
Note that these variables will be stored with class AsIs() objects in
resulting data frames. See ?I for details on AsIs class.

    ## 

|
|======================================================================|
100%

``` r
raw_covar_data_D <- as_tibble(
  process_covar(
    waves = "D", extractAll = TRUE
    )[[1]] # process_covar outputs a list of 1 containing the tibble, this extracts the "content" and 
  ) %>%    # turn it into tibble
  janitor::clean_names()
```

| | | 0% Variables with repeated observations per subject found for the
following variables: PADACTIV,PADLEVEL,PADTIMES,PADDURAT,PADMETS,PAAQUEX
Note that these variables will be stored with class AsIs() objects in
resulting data frames. See ?I for details on AsIs class.

    ## 

|
|======================================================================|
100%

By obtaining all possible covariates from the NHANES .XPT contained in
the package, we notice that we have 10122 unique observations with
corresponding 383 variables including the unique `seqn` ID for wave C.
Similarly, for wave D, we have 10348 unique observations with
corresponding 325 variables including the unique `seqn` ID.

Playing around with the function from the package unfortunately only
reveals that the dataset included only ranges from wave C (2003-2004) &
D (2005-2006). However, it appears that if we could obtain .XPT files
from more recent datasets, we could process it similarly.

Beyond that, there appears to be a lot of `NA` variables within these
covariates data. Furthermore, given the massive variables recorded and
their unique naming, I unfortunately have not been able to find a good
resource to decipher the meaning behind the variables barring the
obvious ones.

*Self-question: Why does, given a list of tibble, using tibble vs
data.frame differs in the results? The covar\_data\_C is a list
structure where it it spits out a list of 1 that contains a list of 383
variables that each contains a “list” of the observations*

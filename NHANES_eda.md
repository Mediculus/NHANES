NHANES EDA
================
Kevin S.W. — UNI: ksw2137
03/07/2020

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
package’s functions and I wasn’t too familiar yet with this. Although it
seems that data

# Processed Data Exploration

## [Accelerometer Data](#loading-dataset)

``` r
# Data import for accelerometry data from wave C (2003-2004)
accel1 <- PAXINTEN_C %>% 
  janitor::clean_names() %>% 
  pivot_longer(cols = starts_with("MIN"), 
               names_to = "min",
               names_prefix = "min",
               values_to = "activ_count") %>% 
  mutate(
    min = as.numeric(min)
  ) %>% 
  mutate_at(
    .vars = vars("seqn", "paxcal", "paxstat", "weekday", "sddsrvyr"),
    .funs = funs(factor)
  )

# Data import for accelerometry data from wave D (2005-2006)
accel2 <- PAXINTEN_D %>% 
  janitor::clean_names() %>% 
  pivot_longer(cols = starts_with("MIN"), 
               names_to = "min",
               names_prefix = "min",
               values_to = "activ_count") %>% 
  mutate(
    min = as.numeric(min)
  ) %>% 
  mutate_at(
    .vars = vars("seqn", "paxcal", "paxstat", "weekday", "sddsrvyr"),
    .funs = funs(factor)
  )
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

Taking wave C’s data and checking each variable using skimr reveals:

``` r
accel1 %>% 
  select(-min) %>% 
  skimr::skim_without_charts()
```

From the resutls, we can see that paxcal has 3 unique values and paxstat
has 2 values which suggests that some of these values are either
“uncalibrated”, “questionable”, or “unknown”. This might require
further filtering.

We also merged wave C and D to see if all these data are continuously
recorded for the same people. Will utilize nesting as well to minimize
load on PC.

``` r
# nesting variables other than seqn to minimize pc load
nested_accel1 <- accel1 %>% 
  group_by(seqn) %>% 
  nest(wave_C = paxcal:activ_count)

# similar nesting procedure to minimize pc load
nested_accel2 <- accel2 %>% 
  group_by(seqn) %>% 
  nest(wave_D = paxcal:activ_count)



# combining 2 datasets based on seqn, the unique ID
joined_accel <- full_join(nested_accel1, nested_accel2, by = "seqn")

# counts distinct seqn's based on the unique ID
joined_accel %>% 
  ungroup() %>% 
  distinct(seqn) %>% 
  count()
```

Per our “nested” findings, it was surprising to see that the `seqn` are
all unique from wave C to D. In other words, we have a total of
`nrow(joined_accel)` unique observations, which are all from different
people. If we were to just aggregate these, we could likely ignore the
wave ID if time were not considered.

## [Flags Data](#loading-dataset)

``` r
flag1 <- Flags_C %>% 
  janitor::clean_names()

flag2 <- Flags_D %>% 
  janitor::clean_names()
```

The flags data appear to be a very detailed, minute-by-minute indicator
of whether or not the device was worn by that particular person as
identified by their `seqn`.

## [Covariate Data](#loading-dataset)

Based on the vignette? it appears that the raw NHANES data beyond
accelerometry are collated within the “covariates data”. This will be
explored further below.

``` r
covar1 <- Covariate_C %>% 
  janitor::clean_names()


covar2 <- Covariate_D %>% 
  janitor::clean_names()
```

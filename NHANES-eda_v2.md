Directed EDA for rnhanes Package
================
Kevin S.W. — UNI: ksw2137
05/13/2020

# Re-fresh start for NHANES dataset

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
```

    ##   |                                                                              |                                                                      |   0%
    ##  Variables with repeated observations per subject found for the following variables: PADACTIV,PADLEVEL,PADTIMES,PADDURAT,PADMETS,PAAQUEX Note that these variables will be stored with class AsIs() objects in resulting data frames.  See ?I for details on AsIs class. 
    ##   |                                                                              |======================================================================| 100%

We can then filter out these observations to only include those that may
be useful for accelerometer and flag data (in case we ever want to
explore relationships between these variables).

First, we load up the flag data:

``` r
flag_d <- Flags_D %>% 
  janitor::clean_names() %>% 
#  pivot_longer(cols = starts_with("MIN"),               # code-line for cases where we want to switch to
#               names_to = "min",                        # long format
#               names_prefix = "min",
#               values_to = "flag_indicator") %>% 
#  mutate(
#    min = as.numeric(min)
#  ) %>% 
  mutate_at(                                             # convert listed variables to factors
    .vars = vars("paxcal", "paxstat", "weekday", "sddsrvyr"),
    .funs = funs(factor)
  ) %>% 
  group_by(seqn) %>%                                     # grouping by the unique ID
  nest(flag_mins = min1:min1440) %>%                     # nesting of the "minutes" for flag
  nest(wave_D = paxcal:flag_mins)                        # nested the "extra variables". 
```

The purpose of the code above is to prepare a condensed dataset that
ready-to-use whenever we need to start investigation on variables of
interest. For now though, we will only use `seqn` as an identifier for
merging other datasets. Below, we will use `seqn` to essentially filter
out all the covariate data to only include those that are in the
`flag_d` dataset.

``` r
identifier <- flag_d %>% 
  select(seqn) %>% 
  ungroup() %>% 
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
variable_list <- read_csv("variable_list.csv")

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

We then evaluate from the remaining 99 variables and felt that an `NA`
amount that is \>86.5861838% would not be a good representative of the
variable. Therefore, we filtered all those that have values greater than
6455.

``` r
na_check_df <- na_check_df %>% 
  filter( na_count < 6455)
```

## Re-selecting Viable Variables for Data Analysis

Now that we’ve set a level of acceptable responses, we could revisit our
variables and be more
selective.

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

We then further re-clean our covar dataframe

``` r
covar_d_clean <- select_if(covar_data_d, 
                           names(covar_data_d) %in% c("seqn", pull(clean_variable_list, var_name)))
```

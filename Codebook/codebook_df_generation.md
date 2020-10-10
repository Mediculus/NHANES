NHANES Codebook Filtering
================
Kevin S.W. — UNI: ksw2137
05/13/2020

# CDC’s NHANES Codebook Filtering

Purpose of this document is to obtain descriptions on the variables that
is collected for NHANES and to be able to create a .csv file of
variables of interest so we can use it to `rnhanesdata` package.

For my project in particular, we are interested in variables that might
be associated with cardiovascular indicators or diseases. While we won’t
select all of the possible variables, we will try to select those that
are “most commonly” known to affect or be related to cardiovascular
physiology.

## Obtaining Variable Descriptions

By consulting CDC’s [NHANES
website](https://wwwn.cdc.gov/nchs/nhanes/Search/DataPage.aspx?Component=Demographics&CycleBeginYear=2005),
we could check 1-by-1 which variables are actually relevant to our
interest. As tedious as it was, unfortunately this is the only method
that can be done without further assistance or access to their API.

Given CDC’s structure of data organization, we will give label to these
structure so that the process can be followed along. CDC’s data
organization is as follows:

> Category/Group Variable/Variable

In other words, take the `examination` **category**, within it, we will
have **group variables** such as “Audiometry”, which will contain
various **variables** that is associated with “Audiometry”.

Notes on removed variables:

  - `All Years`, `VID_D`: different link
  - `DSBI`, `DSII`, `DSPI`, `RXQ_DRUG`: data from 1999-2000? (likely
    aggregate data and too difficult to process)

To be able to understand our dataset, first we need a function to grab
the descriptions for all of the variable-levels from the CDC web.

``` r
# page-reader function
read_page <- function(url, tag) {
  
  h = read_html(url)               # reads url input
  
  data_name = h %>%
    html_nodes(tag) %>%            # pulls the specific html tag (for titles)
    html_text()
  
  data_frame(data_name)            # turns scraped data into a dataframe
}
```

Now that we have a page-reader function, first we will scrape all the
available “survey” documents’ names. To do this, we create a tibble
containing the links to each of this categories (demographics, dietary,
examination, laboratory, questionnaire, and limited data “surveys”).

The code below creates our first dataframe that scrapes the group
variables as well as the .doc filenames associated with it.

``` r
# obtaining survey type/and its topic-contents
survey_df <- 
  tibble(
    survey_link = c(
      "https://wwwn.cdc.gov/nchs/nhanes/Search/DataPage.aspx?Component=Demographics&CycleBeginYear=2005",  # demographics
      "https://wwwn.cdc.gov/nchs/nhanes/Search/DataPage.aspx?Component=Dietary&CycleBeginYear=2005",       # dietary  
      "https://wwwn.cdc.gov/nchs/nhanes/Search/DataPage.aspx?Component=Examination&CycleBeginYear=2005",   # examination
      "https://wwwn.cdc.gov/nchs/nhanes/Search/DataPage.aspx?Component=Laboratory&CycleBeginYear=2005",    # laboratory
      "https://wwwn.cdc.gov/nchs/nhanes/Search/DataPage.aspx?Component=Questionnaire&CycleBeginYear=2005", # questionnaire
      "https://wwwn.cdc.gov/nchs/nhanes/Search/DataPage.aspx?Component=Non-Public&CycleBeginYear=2005"     # limited data
      ),
    # categorizing based on CDC's classification of survey
    category = c("demographics", "dietary", "examination", "laboratory", "questionnaire", "limited data")
  ) %>% 
  mutate(
    category = factor(category),
    group_var = map2(.x = survey_link,                   # Obtaining the group variable descriptors
                     .y = "td.text-left", 
                     ~read_page(url = .x, tag = .y)),
    group_filename = map2(.x = survey_link,              # Obtaining the filenames for each group variables
                          .y = ".text-center:nth-child(2) a", 
                          ~read_page(url = .x, tag = .y))
    ) %>% 
  unnest() %>%   
  rename(
    group_var = data_name,                               # Rename to proper descriptors (Group variable name)
    group_filename = data_name1                          # Rename to proper descriptors (filename for group variables)
  ) %>% 
  mutate(
    group_filename = gsub(" Doc", "", group_filename)    # remove the "Doc" text at the end of every filename in group_filename
    ) %>% 
  filter(!(group_filename %in%                           # remove the odd-one-out survey links
             c("All Years", "VID_D", "DSBI", "DSII", "DSPI", "RXQ_DRUG"))) %>% 
  # creating url for each group variables; prep for scraping the individual variables. 
  mutate(
    var_url = case_when(category != "limited data" ~ 
                          str_c("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/", group_filename, ".htm"),
                        category == "limited data" ~ 
                          str_c("https://wwwn.cdc.gov/Nchs/Nhanes/limited_access/", group_filename, ".htm"))
  ) %>% 
  mutate(                                                # mapping our scraper to obtain the individual variables,
    var_content = map2(.x = var_url,                     # nested by group variable.
                       .y = "#CodebookLinks a", 
                       ~read_page(url = .x, tag = .y))
  ) %>% 
  select(-survey_link, -var_url)                                       # remove unneeded column. 
```

Now that we have the “list of descriptions”, we could scan which
variables/topics are of interest to us. We are particularly interested
in variables potentially related to cardiovascular function.

Note however that `nrow =` 151 has 3 more variables when compared to
`distinct(survey_df, group_filename) =` 148, suggesting that there are
some duplicates (see below).

``` r
janitor::get_dupes(survey_df, group_filename)
```

    ## # A tibble: 6 x 5
    ##   group_filename dupe_count category   group_var                    var_content 
    ##   <chr>               <int> <fct>      <chr>                        <list>      
    ## 1 HPVSER_D                2 laboratory "Human Papillomavirus (HPV)… <tibble [5 …
    ## 2 HPVSER_D                2 laboratory "Human Papillomavirus (HPV)… <tibble [5 …
    ## 3 HPVSWR_D                2 laboratory "Human Papillomavirus (HPV)… <tibble [43…
    ## 4 HPVSWR_D                2 laboratory "Human Papillomavirus (HPV)… <tibble [43…
    ## 5 SWR_D_R                 2 limited d… "Human Papillomavirus (HPV)… <tibble [43…
    ## 6 SWR_D_R                 2 limited d… "Human Papillomavirus (HPV)… <tibble [43…

As we can see, the duplicates are likely from slightly different
descriptions of the HPV tests. Thankfully we aren’t interested in these
variables so we can “remove the duplicates” later.

## Obtaining Detailed Version of Descriptions

Another step we need to do is the add clarifying descriptions as some of
these variables are still unclear.

``` r
detailed_var_desc <- survey_df %>% 
  select(category) %>% 
  distinct(category, .keep_all = TRUE) %>%       # obtaining the survey type variables
  # adding url for each survey type
  mutate(
    detailed_var_url = 
      case_when(category != "limited data" ~ 
                  str_c("https://wwwn.cdc.gov/nchs/nhanes/Search/variablelist.aspx?Component=", 
                        category, "&CycleBeginYear=2005"),
                category == "limited data" ~
                  "https://wwwn.cdc.gov/nchs/nhanes/Search/variablelist.aspx?CycleBeginYear=2005&Component=Non-Public")
    ) %>% 
  # obtaining variable ID and the detailed descriptions
  mutate(
    var_name = map2(.x = detailed_var_url, .y = "td:nth-child(1)", ~read_page(url = .x, tag = .y)),
    detailed_var_content = map2(.x = detailed_var_url, .y = "td:nth-child(2)", ~read_page(url = .x, tag = .y)),
    group_filename = map2(.x = detailed_var_url, .y = "td:nth-child(3)", ~read_page(url = .x, tag = .y))
  ) %>% 
  select(-detailed_var_url) %>% 
  unnest() %>% 
  rename("var_name" = data_name,
         "det_var_desc" = data_name1,
         "group_filename" = data_name2)
```

We will keep this dataframe to be used for later, once we remove
variables that we are NOT interested in.

## Selecting Variables of Interest

Unfortunately, this is one of the more time-consuming portion and
tedious as we need to evaluate each variable by eye to determine which
variables are of specific interest.

### Removing Irrelevant Group Variables

Thankfully, our `group_var` variable can provide some insight on which
group variables we should look into. List of removed group variables
below, based on “irrelevance” to cardiovascular issues:

  - Demographic:
      - None
  - Dietary:
      - None
  - Exam Survey:
      - Audiometry -containing descriptions
      - Dual Energy X-ray -containing descriptions
      - Ophthalmology -containing descriptions
      - Oral Health
      - Vision
  - Lab Survey:
      - Acrylamide & Glycidamide
      - Allergen -containing descriptions
      - Arsenics
      - Brominated Flame Retardants
      - Cadmium, Lead, & Total Mercury - Blood
      - Chlamydia & Gonorrhea - Urine
      - Cotinine - Serum
      - Environmental Phenols & Parabens
      - Epstein-Barr Virus
      - Erythrocyte Protoporphyrin
      - Fasting Questionnaire
      - Ferritin
      - Folate
      - HepA Ab
      - HepB Surface Ab
      - HepB Core Ab
      - HepC Confirmed Ab
      - HSV Type-1 & Type-2
      - HIV Ab Test, CD4+ T Lymphocytes & CD8+ T Cells
      - HPV -containing descriptions
      - Iodine - Urine
      - Iron, TIBC
      - Mercury -containing descriptions
      - Metals - Urine
      - Non-dioxin-like Polychlorinated…
      - Organophosphate Insecticides…
      - PTH
      - Perchlorate -containing descriptions
      - Pesticides -containing descriptions
      - Phthalates - Urine
      - Phytoestrogens
      - Polychlorinated dibenzo-p-dioxins
      - Polyfluoroalkyl Chemicals
      - Pooled-Sample Technical Support File
      - Pregnancy Test - Urine
      - PSA
      - Salmonella & Campylobacter Ab (Surplus)
      - Transferrin Receptor
      - Volatile Organic Compounds -containing descriptions.
  - Questionnaire:
      - Acculturation
      - Allergy
      - Audiometry
      - Dermatology
      - HepC Follow Up
      - Kidney Conditions - Urology
      - Mental Health - Depression Screener
      - Oral Health
      - Osteoporosis
      - Pesticide Use
      - Prostate Conditions
      - PSA Follow-up
      - Reproductive Health
      - Sexual Behavior
      - Sleep Disorders
      - Vision
  - Limited Data:
      - Chlamydie & Gonorrhea - Urine
      - HSV Type-2 Youth
      - HPV -containing descriptions
      - Los Angeles County, California - Demographic Variables
      - Mental Health - Depression Screener
      - Sexual Behavior - Youth

We then create a list for these criteria and filter them out.

``` r
# list of group variable to be removed
groupvar_remove_list <- 
  c("Audio", "X-ray", "Ophthalmol", "Oral", "Vision", "Acrylamide", "Allerg", 
    "Arsenics", "Brominated", "Cadmium", "Chlamydia", "Cotinine", "Environmental", "Epstein-Barr", 
    "Erythrocyte Protoporphyrin", "Fasting Questionnaire", "Ferritin", "Folate", "Hepatitis",
    "Herpes Simplex Virus", "HIV", "Human Papillomavirus", "Iodine", "Iron", "Mercury", "Metals", "Non-dioxin-like",
    "Organophosphate", "Parathyroid", "Perchlorate", "Pesticide", "Phthalates", "Phytoestrogens", "Polychlorinated",
    "Polyfluoroalkyl", "Pooled-Sample", "Pregnancy Test", "Prostate", "Salmonella", "Transferrin",
    "Volatile Organic Compounds", "Acculturation", "Dermatology", "Kidney Conditions", "Mental Health", 
    "Oral Health", "Osteoporosis", "Reproductive Health", "Sleep Disorders", "Los Angeles County", "Sexual")


# curated group_filename that removes all those in the remove_list
survey_df_filtered <- survey_df %>% 
  filter(!(str_detect(group_var, 
                      paste(groupvar_remove_list,      # remove using a combination of !, str_detect, paste funcs
                            collapse = "|")))) %>%     # collapse "merges" the list with the specified char as the "space"
  unnest() %>%                                         # reveal variables (to cross ref with rnhanesdata)
  separate(data_name,                                  # separate the variable into the "variable name" and its decription
           into = c("var_name", "var_desc"),
           sep = " - ",
           extra = "merge")                            # used to separate only by the "first seen" separator.
```

Now that we have filtered out unnecessary group variables, we can check
for duplicate variables using `janitor::get_dupes` function.

``` r
janitor::get_dupes(survey_df_filtered, var_name)
```

    ## # A tibble: 141 x 6
    ##    var_name dupe_count category  group_var          group_filename var_desc     
    ##    <chr>         <int> <fct>     <chr>              <chr>          <chr>        
    ##  1 CBSA              2 limited … Geocoded Data, NH… GEO_2000       Core Based S…
    ##  2 CBSA              2 limited … Geocoded Data, NH… GEO_2010       Core Based S…
    ##  3 DMDBORN           2 demograp… Demographic Varia… DEMO_D         Country of B…
    ##  4 DMDBORN           2 limited … California - Demo… CDEMO_AD       Country of b…
    ##  5 DMDCITZN          2 demograp… Demographic Varia… DEMO_D         Citizenship …
    ##  6 DMDCITZN          2 limited … California - Demo… CDEMO_AD       Citizenship …
    ##  7 DMDEDUC2          2 demograp… Demographic Varia… DEMO_D         Education Le…
    ##  8 DMDEDUC2          2 limited … California - Demo… CDEMO_AD       Education le…
    ##  9 DMDEDUC3          2 demograp… Demographic Varia… DEMO_D         Education Le…
    ## 10 DMDEDUC3          2 limited … California - Demo… CDEMO_AD       Education le…
    ## # … with 131 more rows

Unfortunately, as seen by the function, there were multiple duplicate
variables that are subset by different group-variables and thus we
cannot efficiently remove this with `distinct()`. To make this more
manageable, we should subset this by each survey category. Before doing
so however, we should merge our `detailed_var_desc` dataset to our
current dataset, given it’s currently smaller.

``` r
survey_df_filtered <- left_join(survey_df_filtered, 
                               detailed_var_desc,
                               by = c("category", "group_filename", "var_name"))
```

After obtaining a more comprehensive description dataset, we always
check for duplicates.

``` r
janitor::get_dupes(survey_df_filtered, group_filename, var_name)
```

    ## # A tibble: 52 x 7
    ##    group_filename var_name dupe_count category  group_var var_desc det_var_desc 
    ##    <chr>          <chr>         <int> <fct>     <chr>     <chr>    <chr>        
    ##  1 GEO_2000       BG2K              2 limited … Geocoded… Census … Census 2000 …
    ##  2 GEO_2000       BG2K              2 limited … Geocoded… Census … Census 2000 …
    ##  3 GEO_2000       BLOCK2K           2 limited … Geocoded… Census … Census 2000 …
    ##  4 GEO_2000       BLOCK2K           2 limited … Geocoded… Census … Census 2000 …
    ##  5 GEO_2000       CBSA              2 limited … Geocoded… Core Ba… CBSA Lowest …
    ##  6 GEO_2000       CBSA              2 limited … Geocoded… Core Ba… Core Based S…
    ##  7 GEO_2000       CNTY2K            2 limited … Geocoded… Census … Census 2000 …
    ##  8 GEO_2000       CNTY2K            2 limited … Geocoded… Census … Census 2000 …
    ##  9 GEO_2000       LAT               2 limited … Geocoded… Latitud… Latitude in …
    ## 10 GEO_2000       LAT               2 limited … Geocoded… Latitud… Latitude (De…
    ## # … with 42 more rows

As we see above, there were 52 rows of duplicates. Having cross-checked
with CDC’s descriptions that doesn’t appear to show any particular
difference, we could safely use `distinct()` to filter these out.
However, to ensure there’s no “missing information”, we decided to
remove duplicates with exact matches in three columns, `group_filename`,
`var_name`, and `det_var_desc`.

``` r
survey_df_filtered <- distinct(survey_df_filtered, 
                              group_filename, var_name, det_var_desc, 
                              .keep_all = TRUE)
```

These are the first step in filtering our incredibly large dataset. The
next step is then to further filter out the items contained within the
remaining group variables.

### Keeping Variables of Interest under Demographic Category

``` r
demo_survey <- survey_df_filtered %>% 
  filter(category == "demographics") %>% 
  select(-category, -group_var, -group_filename)
```

Under demographic survey data, we see that variables of interest are:

  - SEQN: Sequence number
  - SDDSRVYR: Data release number (likely “wave” ID)
  - RIDSTATR: Interview (1) or interview + exam (2), NA otherwise
  - RIAGENDR: Gender
  - RIDAGEYR: Age at screening in years
  - RIDRETH1: Ethnicity
  - DMDEDUC3: Education level (aged 6-19)
  - DMDEDUC2: Education level (aged 20+)
  - DMDFMSIZ: Number of people in Family
  - INDFMINC: Annual family income
  - INDFMPIR: Family Poverty Income Ratio (PIR; family income:poverty
    threshold)
  - WTINT2YR: Full sample 2-year interview weight
  - WTMEC2YR: Full sample 2-year MEC exam weight
  - SDMVPSU: Masked variance pseudo-PSU
  - SDMVSTRA: Masked variance pseudo-stratum

From these, we could build a “keep-list” for our demographic data and
obtain a filtered variable list for our demographics category.

``` r
demo_keep_list <- 
  c("SEQN", "SDDSRVYR", "RIDSTATR", "RIAGENDR", 
    "RIDAGEYR", "RIDRETH1", "DMDEDUC3", "DMDEDUC2",
    "DMDFMSIZ", "INDFMINC", "INDFMPIR", "WTINT2YR", 
    "WTMEC2YR", "SDMVPSU", "SDMVSTRA")

demo_survey <- demo_survey %>% 
  filter((str_detect(var_name, 
                      paste(demo_keep_list,            
                            collapse = "|"))))
```

### Keeping Variables of Interest under Dietary Category

We can do similar steps with our dietary data.

``` r
diet_survey <- survey_df_filtered %>% 
  filter(category == "dietary") %>% 
  select(-category, -group_filename)

# used to check "unique" data descriptions to quickly filter out which categories might be of interest to explore
#distinct(diet_survey, var_name)
```

Of interest:

  - SEQN
  - DR1ILINE: Another key variable for dietary data; “food/individual”
    component number
  - Dietary Interview - Total Nutrient Intakes, First Day:
      - DBD100: How often adding salt to food
      - DRQSDIET: On special kinds of diet to lose weight/other
        health-related reason (followed by options if yes)
          - DRQSDT1: weight loss (lowcal/carb/hiprot diet)
          - DRQSDT2: lowfat/lowcholes diet
          - DRQSDT3: Low salt diet
          - DRQSDT4: Sugar-free/low sugar diet
          - DRQSDT5: Low-fiber diet
          - DRQSDT6: high-fiber diet
          - DRQSDT7: diabetic diet
          - DRQSDT8: weight-gain/muscle building diet
          - DRQSDT91: other special diet
      - DR1TKCAL: energy (kcal)
      - DR1TPROT: protein (gm)
      - DR1TCARB: carbs (gm)
      - DR1TSUGR: total sugars (gm)
      - DR1TFIBE: dietary fibers (gm)
      - DR1TTFAT: total fat (gm)
      - DR1TSFAT: total sat. FA (gm)
      - DR1TMFAT: total monounsat. FA (gm)
      - DR1TPFAT: total polyunsat. FA (gm)
      - DR1TCHOL: cholesterol (mg)
      - DR1\_320Z: total plain water drank yesterday (gm)
      - DR1\_330Z: total tap water drank yesterday (gm)
      - DR1BWATZ: total bottled water drank yesterday (gm)
  - Dietary Interview - Total Nutrient Intakes, Second Day:
      - DR2TKCAL: energy (kcal)
      - DR2TPROT: protein (gm)
      - DR2TCARB: carbs (gm)
      - DR2TSUGR: total sugars (gm)
      - DR2TFIBE: dietary fibers (gm)
      - DR2TTFAT: total fat (gm)
      - DR2TSFAT: total sat. FA (gm)
      - DR2TMFAT: total monounsat. FA (gm)
      - DR2TPFAT: total polyunsat. FA (gm)
      - DR2TCHOL: cholesterol (mg)
      - DR2\_320Z: total plain water drank yesterday (gm)
      - DR2\_330Z: total tap water drank yesterday (gm)
      - DR2BWATZ: total bottled water drank yesterday (gm)

<!-- end list -->

``` r
diet_keep_list <- 
  c("SEQN", "DR1ILINE", "DBD100", "DRQSDIET", 
    "DRQSDT1", "DRQSDT2", "DRQSDT3", "DRQSDT4", 
    "DRQSDT5", "DRQSDT6", "DRQSDT7", "DRQSDT8", 
    "DRQSDT91", "TKCAL", "TPROT", "CARB", "TSUGR", 
    "TFIBE", "TTFAT", "TSFAT", "TMFAT", "TPFAT", 
    "TCHOL", "320Z", "330Z", "BWATZ")

diet_survey <- diet_survey %>% 
  filter(!(str_detect(group_var, "Individual|Questionnaire|Supplement")), 
         (str_detect(var_name, 
                      paste(diet_keep_list,            
                            collapse = "|"))))
```

### Keeping Variables of Interest under Examination Category

``` r
exam_survey <- survey_df_filtered %>% 
  filter(category == "examination") %>% 
  select(-category, -group_filename)

# used to check "unique" data descriptions to quickly filter out which categories might be of interest to explore
#janitor::get_dupes(exam_survey, var_name)

#exam_survey %>% 
#  distinct(var_name)
```

Variables of Interest:

  - Blood Pressure:
      - SEQN
      - PEASCST1: BP status
      - BPXCHR: 60sec HR (30s x 2)
      - BPXPLS: 60sec pulse (30s x2); duplicate of BPXCHR?
      - BPXPULS: pulse reg/irreg
      - BPXPTY: pulse type
      - BPXSY\*: systolic BP readings:
          - 1: 1st reading
          - 2: 2nd reading
          - 3: 3rd reading
          - 4: 4th reading
      - BPXDI\*: diastolic BP readings:
          - 1: 1st reading
          - 2: 2nd reading
          - 3: 3rd reading
          - 4: 4th reading
  - Body Measures:
      - SEQN
      - BMXWT: weight (kg)
      - BMXHT: standing height (cm)
      - BMXBMI: BMI (kg/m^2)
      - BMXCALF: max calf circumference (cm)
      - BMXARMC: arm circumference (cm)
      - BMXWAIST: waist circumference (cm)
      - BMXTHICR: thigh circumference (cm)
      - BMXTRI: triceps skinfold (mm)
      - BMXSUB: subscapular skinfold (mm)
  - Physical Activity Monitor
      - None from raw (provided in `rnhanesdata` package)

<!-- end list -->

``` r
exam_keep_list <- 
  c("SEQN", "PEASCST1", "BPXCHR", "BPXPLS", "BPXPULS", 
    "BPXPTY", "BPXSY", "BPXDI", "BMXWT", "BMXHT", "BMXBMI", 
    "BMXCALF", "BMXARMC", "BMXWAIST", "BMXTHICR", "BMXTRI", 
    "BMXSUB")

exam_survey <- exam_survey %>% 
  filter(str_detect(var_name, 
                      paste(exam_keep_list,            
                            collapse = "|")))
```

### Keeping Variables of Interest under Laboratory Category

``` r
lab_survey <- survey_df_filtered %>% 
  filter(category == "laboratory") %>% 
  select(-category, -group_filename)

# used to check "unique" data descriptions to quickly filter out which categories might be of interest to explore
#janitor::get_dupes(lab_survey, var_name)
```

Thankfully, the lab survey has initially been filtered by the kind of
tests that were undertaken by each subjects. As such, all components
from those `data_desc` under `laboratory` will be of interest. To recap,
the lab tests that were kept are:

  - Urine Albumin/Creatinine
  - Lipid panel
  - CBC w/ diff
  - CRP
  - HbA1C (glycohemoglobin)
  - Hemocysteine
  - Plasma fasting glucose & insulin
  - Urine Polyaromatic Hydrocarbons (PAH)
  - CMP (standard biochem profile)
  - Vit A, E & carotenoids
  - Vit B12
  - Vit B6
  - Vit C

### Keeping Variables of Interest under Questionnaire Category

``` r
que_survey <- survey_df_filtered %>% 
  filter(category == "questionnaire") %>% 
  select(-category, -group_filename)

# used to check "unique" data descriptions to quickly filter out which categories might be of interest to explore
#janitor::get_dupes(que_survey, var_name)
```

Questionnaires of interest:

  - Alcohol Use:
      - ALQ120Q: how often drinking etOH past 12 months.
      - ALQ120U: unit of measure (\# days of drinking etOH /wk, month,
        or yr)
  - Blood Pressure & Cholesterol
      - BPQ040A: taking rx for HTN
      - BPQ070: when blood cholesterol last checked
      - BPQ080: told by md, HLD
      - BPQ090\*: recommendations for dealing with HLD
          - A: told to eat less fat
          - B: told to reduce weight
          - C: told to exercise more
          - D: told to take rx
  - Cardiovascular Health
      - All
  - Current Health Status
      - HSD010: general health condition
  - Diabetes
      - All
  - Diet Behavior & Nutrition
      - None
  - Drug use
      - None
  - Early Childhood
      - None
  - Food Security
      - FSD032:
          - A: worried about running out of food
          - B: food didn’t last
          - C: couldn’t afford balanced meal
          - D:
          - E:
          - F:
      - FSD041: adults cut size/skip meals
      - FSD052: How often adults cut size/skip meals
      - FSD061: eating less than they should in the past 12 months
      - FSD071: hungry but didn’t eat in the past 12 months
      - FSD081: lost weight because no money for food in the past 12
        months
      - FSD092: any adults in the household not eat for whole day b/c no
        money
      - FSD102: how often do these adults not eat for a whole day?
  - Health Insurance
      - None
  - Hospital Utilization & Access to Care
      - None
  - Housing Characteristics
      - None
  - Immunization
      - None
  - Medical Conditions
      - MCQ010: Asthma Dx
      - MCQ025: Age 1st asthma
      - MCQ035: Still with asthma
      - MCQ080: Overweight told by MD.
      - MCQ160B: CHF dx
      - MCQ160C: CAD dx
      - MCQ160D: dx angina/angina pectoris
      - MCQ160E: MI dx
      - MCQ160F: Stroke dx
      - MCQ160G: dx emphysema
      - MCQ180B: Age of CHF dx
      - MCQ180C: Age of CAD dx
      - MCQ180D: Age of angina/angina pectoris dx
      - MCQ180E: Age of MI dx
      - MCQ180F: Age of stroke Dx
      - MCQ180G: Age of emphysema dx
      - MCQ300A: close relative who had MI
      - MCQ300B: close relative who had asthma
      - MCQ300C: close relative who had diabetes
  - Occupation
      - None
  - Physical Activity (Describes subsections of activities, likely not
    needed)
      - None
  - Physical Activity - Individual Activities
      - PADACTIV: over the past 30 days, what vigorous/moderate
        activities did you do?
      - PADLEVEL: reported intensity level of activity
      - PADTIMES: how often did you do activities over the past 30 days?
      - PADDRAT: average duration of the activities (minutes)
  - Physical Functioning
      - PFQ020: Do you have impairment or health problem that limits
        your ability to crawl/walk/…?
      - PFQ030: Is the impairment chronic or acute (\>= 12 months)?
  - Prescription Medications
      - None
  - Respiratory Health
      - None
  - Smoking - Cigarette Use
      - SMQ020: smoked at least 100 cigs in life
      - SMD030: age started smoking regularly
      - SMQ040: do you now smoke cigs still?
      - SMQ050Q: if not smoking, how long ago did you stop?
      - SMQ050U: unit of days/week/months/year
  - Smoking - Household Smokers
      - None
  - Smoking - Recent Tobacco Use
      - None
  - Social Support
      - None
  - Weight History
      - WHD010: current self-reported height (inches)
      - WHD020: current self-reported weight (lbs)
      - WHQ030: how do subject feel about their weight
      - WHQ040: like to weigh more/less/same
      - WHQ060: is weight change intentional?
  - Weight History -
Youth
      - None

<!-- end list -->

``` r
que_remove_list <- c("Bowel Health", "Diet Behavior", "Drug", "Early Childhood", "Health Insurance", "Hospital Utilization", 
                     "Housing Characteristic", "Immunization", "Occupation", "Prescription Medication", "Respiratory Health",
                     "Household Smokers", "Recent Tobacco Use", "Social Support", "Weight History - Youth")

que_keep_list <- c("SEQN", "ALQ120Q", "ALQ120U", "BPQ040A", "BPQ070", "BPQ080", "BPQ090", "CDQ", "DIQ", "HSD010", "FSD032", "FSD041", 
                   "FSD052", "FSD061", "FSD071", "FSD081", "FSD092", "FSD102", "MCQ010", "MCQ025", "MCQ035", "MCQ080", 
                   "MCQ160B", "MCQ160C", "MCQ160D", "MCQ160E", "MCQ160F", "MCQ160G", "MCQ180B", "MCQ180C", "MCQ180D", 
                   "MCQ180E", "MCQ180F", "MCQ180G", "MCQ300A", "MCQ300B", "MCQ300C", "PADACTIV", "PADLEVEL", "PADTIMES", 
                   "PADDRAT", "PFQ020", "PFQ030", "SMQ020", "SMD030", "SMQ040", "SMQ050Q", "SMQ050U", "WHD010", "WHD020", 
                   "WHQ030", "WHQ040", "WHQ060", "DID")

que_survey <- que_survey %>% 
  filter(!(str_detect(group_var, paste(que_remove_list, collapse = "|"))),
         str_detect(var_name, paste(que_keep_list, collapse = "|")))
```

### Keeping Variables of Interest under Limited Data Category

``` r
lim_survey <- survey_df_filtered %>% 
  filter(category == "limited data") %>% 
  select(-category, -group_filename)

# used to check "unique" data descriptions to quickly filter out which categories might be of interest to explore
#janitor::get_dupes(lim_survey, var_name)

#lim_survey %>% 
#  distinct(data_desc)
```

Variables of interest:

  - Geocoded Data, NHANES 1999-2016, Census 2010

<!-- end list -->

``` r
lim_survey <- lim_survey %>% 
  filter(str_detect(group_var, "Census 2010"))
```

## Combining Filtered Variables within Categories into a Single List

Now that we’ve isolated the variables, we can merge them together to
create a csv file that we can use for when we actually process the
covariates from `rnhanesdata`
package.

``` r
final_df <- bind_rows(demo_survey, diet_survey, exam_survey, lab_survey, que_survey, lim_survey)

final_df <- final_df %>% 
  distinct(var_name, var_desc)

final_df <- final_df %>% 
  mutate(var_name = str_to_lower(var_name)) 
#%>% write_csv("variable_list.csv")
```

## utilities
fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}

ordered_fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels, ordered = T)
}

process_2015_recs <- function(input_dir){
  #note: might be able to add more granular heating fuel, identify non-heated homes, add whether we have a stove, and number of bedrooms
  
  raw_recs <- read_csv(here::here(input_dir, "recs2015_public_v4.csv")) 
  
  process_recs <- raw_recs %>% 
    mutate(recs_house_type = fct_case_when(TYPEHUQ == 1 ~ "mobile",
                                           TYPEHUQ == 2 ~ "sf_detached",
                                           TYPEHUQ == 3 ~ "sf_attached",
                                           TYPEHUQ == 4 ~ "apt2_4",
                                           TYPEHUQ == 5 ~ "apt5+"),
           # add new variables below recs_house_type so select statement works properly
           homeowner = ifelse(KOWNRENT == 1, 1, 0), 
           housing_age = fct_case_when(YEARMADERANGE == 1 ~ "Before 1950",
                                       YEARMADERANGE == 2 ~ "1950 to 1959",
                                       YEARMADERANGE == 3 ~ "1960 to 1969",
                                       YEARMADERANGE == 4 ~ "1970 to 1979",
                                       YEARMADERANGE == 5 ~ "1980 to 1989",
                                       YEARMADERANGE == 6 ~ "1990 to 1999",
                                       YEARMADERANGE %in% c(7, 8) ~ "after 2000"),
           num_rooms = TOTROOMS,
           recs_heating_fuel = fct_case_when(FUELHEAT == 1 ~ "natural gas",
                                         FUELHEAT == 2 ~ "propane",
                                         FUELHEAT == 3 ~ "fuel oil",
                                         FUELHEAT == 5 ~ "electricity",
                                         FUELHEAT == 7 ~ "wood",
                                         FUELHEAT == 21 ~ "other",
                                         FUELHEAT == -2 ~ "none"),
           heating = HEATHOME,
           electric_in_rent = if_else(ELPAY == 2, 1, 0),
           gas_in_rent = if_else(NGPAY == 2, 1, 0), 
           heat_in_rent = if_else(LPGPAY == 2 | FOPAY == 2, 1, 0),
           sex_hhldr = as.factor(case_when(HHSEX == 1 ~ "female",
                                           HHSEX == 2 ~ "male")),
           age_hhldr = HHAGE, 
           rac_hhldr = as.factor(case_when(HOUSEHOLDER_RACE =="1" ~ "white",
                                           HOUSEHOLDER_RACE =="2" ~ "black",
                                           HOUSEHOLDER_RACE =="4" ~ "asian",
                                           TRUE ~ "other/multi")),
           educ_cat_hhldr = factor(case_when(EDUCATION == 1 ~ "no_hs",
                                             EDUCATION == 2 ~ "hs",
                                             EDUCATION == 3 ~ "some_college",
                                             EDUCATION %in% c(4,5) ~ "college(+)")),
           people = NHSLDMEM,
           adults = NUMADULT, 
           children = NUMCHILD, 
           income_recs = fct_case_when(MONEYPY == 1 ~ "Less than 20,000", 
                                       MONEYPY == 2 ~ "20,000 - 39,999", 
                                       MONEYPY == 3 ~ "40,000 - 59,999", 
                                       MONEYPY == 4 ~ "60,000 to 79,999", 
                                       MONEYPY == 5 ~ "80,000 to 99,999", 
                                       MONEYPY == 6 ~ "100,000 to 119,999", 
                                       MONEYPY == 7 ~ "120,000 to 139,999", 
                                       MONEYPY == 8 ~ "140,000 or more"),
           electricity = KWH,
           naturalgas = CUFEETNG, 
           gasbottledtank = GALLONLP, 
           fueloil = GALLONFO,
           hdd = HDD65/12,
           cdd = CDD65/12,
           census_div = fct_case_when(DIVISION == 1 ~ "New England",
                                      DIVISION == 2 ~ "Middle Atlantic",
                                      DIVISION == 3 ~ "East North Central",
                                      DIVISION == 4 ~ "West North Central",
                                      DIVISION == 5 ~ "South Atlantic",
                                      DIVISION == 6 ~ "East South Central",
                                      DIVISION == 7 ~ "West South Central",
                                      DIVISION == 8 ~ "Mountain North",
                                      DIVISION == 9 ~ "Mountain South",
                                      DIVISION == 10 ~ "Pacific"),
           # add additional definitions above weight so select statement works properly
           wgt = NWEIGHT) |> 
    select(climate_region = CLIMATE_REGION_PUB,
           iecc_climate = IECC_CLIMATE_PUB,
           metro_micro = METROMICRO,
           urban = UATYP10,
           recs_house_type:wgt)
}

library(lme4) # for timeseries regression per country (used for education).
#' Class DLE.dimension.education
#'
#' @description subclass for education dimension (inheriting DLE.dimension)

DLE.dimension.education <-
  setRefClass("DLE.dimension.education",
    contains = "DLE.dimension", # inherit the DLE.dimension class

    fields = list(
      school_population = "data.frame",
      threshold_prim_completion = "numeric",
      threshold_ls_completion = "numeric",
      education_exp_threshold_prim = "numeric",
      education_exp_threshold_ls = "numeric",
      education_expenditure_prim = "data.frame",
      education_expenditure_ls = "data.frame",
      completion_rate = "data.frame" # added to more easily retrieve completion rate data
    ),
    methods = list(
      initialize = function(...) { # df.input = data.frame(expenditure=0, HDD=0, hh_size=0)) {
        print("initialize: education")
        callSuper(...)

        # health dimension requires additional information to derive global DLS thresholds.
        DF.DLS <<- data.frame(DF.DLS) # , df.input)
      },

      #' @description
      #' Derive DLS thresholds from data if not pre-determined
      DeriveThreshold = function(method = "regression") {
        print("DeriveThreshold: education")

        threshold_prim_completion <<- 95 # in %
        threshold_ls_completion <<- 90 # in %

        edu.years.data.list <- c(seq(2008, 2022))

        fname_prim <- "API_SE.PRM.CMPT.ZS_DS2_en_csv_v2_5735276.csv" # World Bank
        fname_ls <- "API_SE.SEC.CMPT.LO.ZS_DS2_en_csv_v2_5735329.csv" # World Bank

        fname_exp_prim <- "NATMON_DS_07082023163656149.csv" # UNESCO UIS, retrieved 07/08/2023
        fname_exp_ls <- "NATMON_DS_07082023163741891.csv" # UNESCO UIS, retrieved 07/08/2023
        fname_enrolment_prim <- "NATMON_DS_07082023180321605.csv" # UNESCO UIS, retrieved 07/08/2023
        fname_enrolment_ls <- "NATMON_DS_07082023180405177.csv" # UNESCO UIS, retrieved 07/08/2023

        # school population estimate
        f_name_school_population <- "/NATMON_DS_11112020112739815.csv" # school age population (per level), from Unesco UIS statistics, retrieved 11/11/2020
        f_name_school_population_prim <- "NATMON_DS_07082023164327107.csv" # UNESCO UIS, retrieved 07/08/2023
        f_name_school_population_ls <- "NATMON_DS_07082023164427423.csv" # UNESCO UIS, retrieved 07/08/2023

        school_population_prim <- vroom(here("data-raw", "dls_data", "education_school_population", "primary", f_name_school_population_prim)) %>%
          select(LOCATION, Time, Value, Indicator) %>%
          rename(iso = LOCATION, year = Time, school.pop = Value) %>%
          filter(year == year.base) %>%
          left_join(pop %>% filter(year == year.base) %>% select(iso, population)) %>%
          mutate(percentage_school_prim = school.pop / population) %>%
          select(iso, percentage_school_prim)
        school_population_ls <- vroom(here("data-raw", "dls_data", "education_school_population", "secondary_lower", f_name_school_population_ls)) %>%
          select(LOCATION, Time, Value, Indicator) %>%
          rename(iso = LOCATION, year = Time, school.pop = Value) %>%
          filter(year == year.base) %>%
          left_join(pop %>% filter(year == year.base) %>% select(iso, population)) %>%
          mutate(percentage_school_ls = school.pop / population) %>%
          select(iso, percentage_school_ls)
        school_population <<- message.R11 %>%
          left_join(school_population_prim) %>%
          left_join(school_population_ls) %>%
          group_by(R11.region) %>%
          mutate(
            percentage_school_prim.r11 = mean(percentage_school_prim, na.rm=T),
            percentage_school_ls.r11 = mean(percentage_school_ls, na.rm=T)
          ) %>%
          ungroup() %>%
          mutate(
            percentage_school_prim = ifelse(is.na(percentage_school_prim), percentage_school_prim.r11, percentage_school_prim),
            percentage_school_ls = ifelse(is.na(percentage_school_ls), percentage_school_ls.r11, percentage_school_ls)
          ) %>%
          select(iso, percentage_school_prim, percentage_school_ls)



        ## read in completion rate files.
        education_completion_prim <- vroom(here("data-raw", "dls_data", "education_completion_rate", "primary", fname_prim),
                                           skip = 4) %>% select(where(~(is.numeric(.)|is.character(.)))) %>%
          rename(
            iso = `Country Code`
          ) %>%
          mutate(grp = "primary") %>%
          select(-c(`Indicator Code`, `Indicator Name`, `Country Name`))
        education_completion_ls <- vroom(here("data-raw", "dls_data", "education_completion_rate", "secondary_lower", fname_ls),
                                         skip = 4) %>% select(where(~(is.numeric(.)|is.character(.)))) %>%
          rename(
            iso = `Country Code`
          ) %>%
          mutate(grp = "lower_secondary") %>%
          select(-c(`Indicator Code`, `Indicator Name`, `Country Name`))
        ## read in expenditure file
        # education_exp <- read_csv(paste0(data.path, "/Education", fname_exp)) %>%
        #   rename(
        #     iso = COUNTRYCODE,
        #     year = Time,
        #     exp = Value
        #   ) %>%
        #   select(-c(EDULIT_IND, TIME)) %>%
        #   spread(year, exp) # temporarily convert to wide such that we can use `cleanandlong()` function
        education_expenditure_prim <- vroom(here("data-raw", "dls_data", "education_expenditure", "primary", fname_exp_prim)) %>%
          select(where(~(is.numeric(.)|is.character(.)))) %>%
          mutate(grp = "primary") %>% select(LOCATION,grp,Time,Value) %>%
          rename(iso=LOCATION, year=Time, expenditure=Value) %>%
          left_join(
            vroom(here("data-raw", "dls_data", "education_enrolment", "primary", fname_enrolment_prim)) %>%
              select(where(~(is.numeric(.)|is.character(.)))) %>%
              mutate(grp = "primary") %>% select(LOCATION,grp,Time,Value) %>%
              rename(iso=LOCATION, year=Time, enrolment=Value)
          ) %>%
          mutate(expenditure=expenditure/enrolment*1e6) %>% select(-enrolment)
        education_expenditure_ls <- vroom(here("data-raw", "dls_data", "education_expenditure", "secondary_lower", fname_exp_ls)) %>%
          select(where(~(is.numeric(.)|is.character(.)))) %>%
          mutate(grp = "lower_secondary") %>% select(LOCATION,grp,Time,Value) %>%
          rename(iso=LOCATION, year=Time, expenditure=Value) %>%
          left_join(
            vroom(here("data-raw", "dls_data", "education_enrolment", "secondary_lower", fname_enrolment_ls)) %>%
              select(where(~(is.numeric(.)|is.character(.)))) %>%
              mutate(grp = "lower_secondary") %>% select(LOCATION,grp,Time,Value) %>%
              rename(iso=LOCATION, year=Time, enrolment=Value)
          ) %>%
          mutate(expenditure=expenditure/enrolment*1e6) %>% select(-enrolment)


        ### drop rows if not containing enough information and convert to long format

        cleanandlong <- function(df_toclean, newvar = "completionrate", years = edu.years.data.list) {
          # drop countries for which there is no data for 2011-2015
          df_toclean <- df_toclean[rowSums(is.na(select(df_toclean, as.character(years)))) != ncol(select(df_toclean, as.character(years))), ] # drop all rows where all NA
          # drop countries for which there is less than 2 datapoints in the expenditure dataset
          IndexMat <- sapply(select(df_toclean, -c(iso)), is.na)
          df_toclean <- df_toclean %>% subset(rowSums(!IndexMat) > 2)
          # pivot to long
          # convert to long format (for regression)
          df_clean <- df_toclean %>%
            pivot_longer(-c(iso, grp), names_to = "year", values_to = newvar)
          df_clean$year <- as.numeric(df_clean$year)
          return(df_clean)
        }
        addNAs_in_long_iso_grp <- function(df, years){
          # Create a full sequence of years from 2000 to 2020
          full_years <- tibble(year = years)
          # Expand the data to include all combinations of iso, grp, and year
          expanded_data <- tibble(expand.grid(iso = unique(df$iso), grp = unique(df$grp), year = full_years$year))
          # Merge the expanded data with the original data
          merged_data <- expanded_data %>%
            left_join(df, by = c("iso", "grp", "year")) %>%
            arrange(iso,grp,year)

          return(merged_data)
        }

        education_completion_prim <- cleanandlong(education_completion_prim,
          years = edu.years.data.list
        )
        education_completion_ls <- cleanandlong(education_completion_ls,
          years = edu.years.data.list
        )

        education_expenditure_prim <- education_expenditure_prim %>%
          filter(year%in%edu.years.data.list) %>%
          addNAs_in_long_iso_grp(years = edu.years.data.list)
        education_expenditure_ls <- education_expenditure_ls %>%
          filter(year%in%edu.years.data.list) %>%
          addNAs_in_long_iso_grp(years = edu.years.data.list)


        ### predict values and return baseyear
        predictbaseyear <- function(df_tofill, method = "linear", var) {
          if (var == "expenditure") {
            # do regressions per country
            if (method == "linear") {
              # find intercept and linear regression coefficient
              regressions <- lmList(expenditure ~ year | iso, df_tofill)
            }
            if (method == "log") {
              # find intercept and logarithmic regression coefficient
              regressions <- lmList(expenditure ~ log(year) | iso, df_tofill)
            }
            if (method == "average") {
              # fill missing years with mean of period
              df_filled <- df_tofill %>%
                group_by(iso, grp) %>%
                mutate(expenditure = ifelse(is.na(expenditure), mean(expenditure, na.rm = T), expenditure)) %>%
                rename(exp = expenditure)
            }

            if (method == "linear" | method == "log") {
              # predict values for the full dataset
              df <- df_tofill %>% select(-expenditure)
              df$expenditurepred <- predict(regressions, newdata = df)
              # merge the predicted values only if NA in original, keep the reported values
              df_filled <- left_join(df_tofill, df) %>%
                mutate(exp = coalesce(expenditure, expenditurepred)) %>%
                select(-c(expenditure, expenditurepred))
            }
          }

          if (var == "completionrate") {
            # do regressions per country
            if (method == "linear") {
              # find intercept and linear regression coefficient
              regressions <- lmList(completionrate ~ year | iso, df_tofill)
            }
            if (method == "log") {
              # find intercept and logarithmic regression coefficient
              regressions <- lmList(completionrate ~ log(year) | iso, df_tofill)
            }
            if (method == "average") {
              # fill missing years with mean of period
              df_filled <- df_tofill %>%
                group_by(iso, grp) %>%
                mutate(completionrate = ifelse(is.na(completionrate), mean(completionrate, na.rm = T), completionrate)) %>%
                rename(completion_rate = completionrate)
            }

            if (method == "linear" | method == "log") {
              # predict values for the full dataset
              df <- df_tofill %>% select(-completionrate)
              df$completionratepred <- predict(regressions, newdata = df)
              # merge the predicted values only if NA in original, keep the reported values
              df_filled <- left_join(df_tofill, df) %>%
                mutate(completion_rate = coalesce(completionrate, completionratepred)) %>%
                select(-c(completionrate, completionratepred))
            }
          }


          return(filter(df_filled, year == year.base))
        }



        if (method == "regression") {
          education_completion_prim <- predictbaseyear(education_completion_prim, var = "completionrate") %>% drop_na()
          education_completion_ls <- predictbaseyear(education_completion_ls, var = "completionrate") %>% drop_na()
          education_expenditure_prim <- predictbaseyear(education_expenditure_prim, var = "expenditure") %>% drop_na()
          education_expenditure_ls <- predictbaseyear(education_expenditure_ls, var = "expenditure") %>% drop_na()
        }
        if (method == "average") {
          education_completion_prim <- education_completion_prim %>%
            group_by(iso, grp) %>%
            mutate(completion_rate = mean(na.omit(completionrate))) %>%
            filter(year == year.base) %>% drop_na()
          education_completion_ls <- education_completion_ls %>%
            group_by(iso, grp) %>%
            mutate(completion_rate = mean(na.omit(completionrate))) %>%
            filter(year == year.base) %>% drop_na()

          education_expenditure_prim <- education_expenditure_prim %>%
            group_by(iso, grp) %>%
            mutate(exp = mean(na.omit(expenditure))) %>%
            filter(year == year.base) %>% drop_na()
          education_expenditure_ls <- education_expenditure_ls %>%
            group_by(iso, grp) %>%
            mutate(exp = mean(na.omit(expenditure))) %>%
            filter(year == year.base) %>% drop_na()
        }
        if (method == "interpolation") {
          education_completion_prim <- education_completion_prim %>%
            group_by(iso, grp) %>%
            mutate(completion_rate = na.approx(completionrate, rule = 2)) %>%
            filter(year == year.base) %>% drop_na()
          education_completion_ls <- education_completion_ls %>%
            group_by(iso, grp) %>%
            mutate(completion_rate = na.approx(completionrate, rule = 2)) %>%
            filter(year == year.base) %>% drop_na()

          education_expenditure_prim <- education_expenditure_prim %>%
            group_by(iso, grp) %>%
            mutate(exp = na.approx(expenditure, rule = 2)) %>%
            filter(year == year.base) %>% drop_na()
          education_expenditure_ls <- education_expenditure_ls %>%
            group_by(iso, grp) %>%
            mutate(exp = na.approx(expenditure, rule = 2)) %>%
            filter(year == year.base) %>% drop_na()
        }

        completion_rate <<- education_completion_prim %>%
          bind_rows(education_completion_ls) %>%
          pivot_wider(names_from = grp, values_from = completion_rate) %>%
          mutate(
            headcount_below100_prim = pmax(0, 100-primary)/100,
            headcount_below100_ls = pmax(0, 100-lower_secondary)/100,
            deprivation_rate = pmax(0, 100-pmin(lower_secondary, primary))/100 # generally pick lower secondary; but go with primary if that is higher [note: some uncertainty in general here, due to 'modelled population' assumption underlying completion_rate calculations].
          )

        # write to object field as input for IdentifyGap
        education_expenditure_prim <<- education_expenditure_prim
        education_expenditure_ls <<- education_expenditure_ls




        ### derive thresholds
        # # filter countries meeting the separate thresholds
        # education_completion_prim_countries_meet_threshold <- education_completion_prim %>% filter(completion_rate>threshold_prim_completion) %>% mutate(meet = "yes")
        # education_completion_ls_countries_meet_threshold <- education_completion_ls %>% filter(completion_rate>threshold_ls_completion) %>% mutate(meet = "yes")
        # keep the most efficient half (expenditure) that meets DLS standards
        education_expenditure_prim_meet_thres <- education_expenditure_prim %>%
          left_join(education_completion_prim) %>%
          filter(completion_rate > threshold_prim_completion)
        education_expenditure_prim_meet_thres_efficient <- education_expenditure_prim_meet_thres %>%
          filter(exp < median(median(education_expenditure_prim_meet_thres$exp)))
        education_expenditure_ls_meet_thres <- education_expenditure_ls %>%
          left_join(education_completion_ls) %>%
          filter(completion_rate > threshold_ls_completion)
        education_expenditure_ls_meet_thres_efficient <- education_expenditure_ls_meet_thres %>%
          filter(exp < median(education_expenditure_ls_meet_thres$exp))



        # define threhold
        education_exp_threshold_prim <<- median(education_expenditure_prim_meet_thres_efficient$exp)
        education_exp_threshold_ls <<- median(education_expenditure_ls_meet_thres_efficient$exp)




        # set threshold for both groups.

        DF.DLS <<- DF.DLS %>% mutate(thres = ifelse(grp == "primary",
          education_exp_threshold_prim,
          education_exp_threshold_ls
        )) %>% # Threshold: government expenditure on education per year (\$ per cap, PPP) -- globally uniform

          # Multiply exp per student by school-population to get the gap per capita -- differing per country dependent on demographics
          left_join(school_population) %>%
          mutate(thres = ifelse(grp == "primary",
            thres * percentage_school_prim,
            thres * percentage_school_ls
          )) %>%
          left_join(completion_rate) %>%
          mutate(deprivation.headcount=ifelse(grp == "lower_secondary", deprivation_rate, # only report lower secondary for the headcount; primary only is not sufficient for DLS.
                                              NA)) %>%
          mutate(deprivation.headcount=ifelse((is.na(deprivation.headcount)&(grp == "lower_secondary")),
            mean(deprivation.headcount, .by = c("R11.region")),
            deprivation.headcount
          ))

        # add threshold where not assigned by taking the average of the message region
        DF.DLS <<- DF.DLS %>%
          # take mean of R11 MESSAGE region if there's no data available
          left_join(message.R11 %>% select(-country_name)) %>% # add message.R11 regions to do easy and transparent filling of threshold for missing regions
          group_by(R11.region, grp) %>%
          mutate(
            thres = ifelse(is.na(thres), mean(thres, .by=c("grp", "R11.region"), na.rm = T), thres), # ... if still no estimate (after regression etc.)
            deprivation.headcount = ifelse(is.na(deprivation.headcount), mean(deprivation.headcount, .by=c("R11.region"), na.rm = T), deprivation.headcount) # ..then assume regional mean
          ) %>%

          # add some data on the countries/regions not reporting data to UNESCO (UIS)
          # Pacific OECD region:
          mutate(
            deprivation.headcount = ifelse(((iso%in%c("AUS"))&is.na(deprivation.headcount)&(grp=="lower_secondary")), 0.02, # see under SDG indicator set for 4.1.2 variable "Completion rate, lower secondary education, both sexed (%)" for the year 2015 (ref: http://sdg4-data.uis.unesco.org/)
                                           ifelse(((iso%in%c("JPN"))&is.na(deprivation.headcount)&(grp=="lower_secondary")), 0, # since it has been around zero already since the early 80s (ref: https://data.worldbank.org/indicator/SE.SEC.CMPT.LO.ZS?locations=JP)
                                                  ifelse(((iso%in%c("NZL"))&is.na(deprivation.headcount)&(grp=="lower_secondary")), 0,
                                                         deprivation.headcount))), # New Zealand does not report data, and has a non-standard education system. However, with schooling being compulsory for 6-16 year olds (ref https://www.education.govt.nz/our-work/our-role-and-our-people/education-in-nz/#primary), and high development, the gap can be assumed to be zero

          ) %>%

          ungroup()
      },

      #' @description
      #' Overwrite the same-named function in the superclass to be specific to this dimension
      IdentifyGap = function() {
        print("IdentifyGap: Education")

        # read in expenditure data (produced in DeriveThreshold, above)
        fname_exp_prim <- "Education_prim_expenditure_countries.csv"
        fname_exp_ls <- "Education_ls_expenditure_countries.csv"

        education_exp_prim <- education_expenditure_prim
        education_exp_ls <- education_expenditure_ls
        education_exp <- bind_rows(education_exp_prim, education_exp_ls) # before scaling



        # scale using HALE (options: per education level, per region?)
        fname_hale <- "/HALE.csv" # processed by hand - original data on the WHO website (https://apps.who.int/gho/data/node.main.HALE?lang=en) seems to be down at 23/04/2020...
        health_hale <- vroom(paste0(data.path, "/Health", fname_hale))
        class(health_hale[["2015"]]) <- "numeric"

        # Join data: primary
        edu_reg_prim <- select(message.R11, -country_name) %>% # merge data: education expenditure and HALE
          left_join(select(health_hale, c(`2015`, "Country Code")), by = c(iso = "Country Code")) %>%
          left_join(education_exp_prim, by = "iso") %>%
          rename(hale = "2015")

        print("Regression model: fitting primary education")
        lm_edu_prim <- lm(log(exp) ~ hale, data = edu_reg_prim)
        # print(summary(lm_edu_prim))
        # Extrapolate results
        print("Start results extrapolation")
        edu_extr_prim <- select(edu_reg_prim, -c(grp, year)) # %>% rename(edu_extr_prim=exp) #initialize

        edu_extr_prim <- edu_extr_prim %>%
          mutate(edu_pred_prim = predict(lm_edu_prim, edu_extr_prim)) %>% # predicted results
          mutate(edu_extr_prim = exp(edu_pred_prim)) %>% # copy predicted results into a new column for extrapolated results
          mutate(edu_extr_prim = ifelse(!is.na(exp), exp, edu_extr_prim)) %>% # copy original data, where available
          select(iso, R11.region, edu_extr_prim, hale) %>% # Keep only iso and extrapolation results
          rename(exp = edu_extr_prim)

        edu_reg_prim <- select(message.R11, -country_name) %>% # merge data: education expenditure and HALE
          left_join(select(health_hale, c(`2015`, "Country Code")), by = c(iso = "Country Code")) %>%
          left_join(education_exp_prim, by = "iso") %>%
          rename(hale = "2015") %>%
          select(-c(hale, R11.region))


        # Join data: lower secondary
        edu_reg_ls <- select(message.R11, -country_name) %>% # merge data: education expenditure and HALE
          left_join(select(health_hale, c(`2015`, "Country Code")), by = c(iso = "Country Code")) %>%
          left_join(education_exp_ls, by = "iso") %>%
          rename(hale = "2015")

        print("Regression model: fitting lower secondary education")
        lm_edu_ls <- lm(log(exp) ~ hale, data = edu_reg_ls)
        # print(summary(lm_edu_ls))
        # Extrapolate results
        print("Start results extrapolation")
        edu_extr_ls <- select(edu_reg_ls, -c(grp, year)) # %>% rename(edu_extr_ls=exp) #initialize

        edu_extr_ls <- edu_extr_ls %>%
          mutate(edu_pred_ls = predict(lm_edu_ls, edu_extr_ls)) %>% # predicted results
          mutate(edu_extr_ls = exp(edu_pred_ls)) %>% # copy predicted results into a new column for extrapolated results
          mutate(edu_extr_ls = ifelse(!is.na(exp), exp, edu_extr_ls)) %>% # copy original data, where available
          select(iso, R11.region, edu_extr_ls, hale) %>% # Keep only iso and extrapolation results
          rename(exp = edu_extr_ls)

        edu_reg_ls <- select(message.R11, -country_name) %>% # merge data: education expenditure and HALE
          left_join(select(health_hale, c(`2015`, "Country Code")), by = c(iso = "Country Code")) %>%
          left_join(education_exp_ls, by = "iso") %>%
          rename(hale = "2015") %>%
          select(-c(hale, R11.region))



        # bind primary and lower secondary
        edu_reg_prim <- select(message.R11, -country_name) %>%
          left_join(edu_reg_prim) %>%
          mutate(grp = "primary") # specify grp for all countries, also NA
        edu_reg_ls <- select(message.R11, -country_name) %>%
          left_join(edu_reg_ls) %>%
          mutate(grp = "lower_secondary") # specify grp for all countries, also NA
        education_exp <- bind_rows(edu_reg_prim, edu_reg_ls) %>% select(-year) # after scaling.

        # ggplot(
        #   education_exp %>% left_join(health_hale %>% select(c(`2015`, "Country Code")) %>% rename(iso = "Country Code", HALE = `2015`)) %>%
        #     filter(),
        #   aes(y = exp, x = HALE, colour = grp)
        # ) +
        #   geom_point()
        # min(education_exp$exp, na.rm = T)
        # mean(education_exp$exp, na.rm = T)

        # assign average expenditure of R11.region if NA
        education_exp_filled <- education_exp %>%
          group_by(R11.region, grp) %>%
          mutate(exp = ifelse(is.na(exp),
            mean(na.omit(exp)),
            exp
          )) %>% ungroup()




        # Join data and calculate gap (i.e., [expenditure.needed.for.schoolgoing.population]-[estimated.expenditure] )
        DF.DLS <<- DF.DLS %>%
          left_join(select(education_exp_filled, iso, grp, exp), by = c("iso", "grp")) %>% # Join data: primary education expenditure
          mutate(exp = ifelse(grp == "primary",
            exp * percentage_school_prim,
            exp * percentage_school_ls
          )) %>%
          # add expenditure where not assigned by taking the average of the message region
          left_join(message.R11 %>% select(-country_name)) %>% # add message.R11 regions to do easy and transparent adjustment for colder regions
          group_by(R11.region, grp) %>%
          mutate(exp = ifelse(is.na(exp), mean(exp, na.rm = T), exp)) %>%
          ungroup() %>%
          mutate(gap = thres - exp) %>% # Calculate gap: total $ that should be spent extra on Education
          mutate(gap = ifelse(gap > thres, thres, gap)) %>% # to-do: ensure better way that regression produced values are not negative or otherwise unreasonably low - constrain by lowest observed value in source dataset.
          mutate(gap = ifelse(gap < 0, 0, gap)) %>% # if negative gap, set to zero
          select(-exp) %>%
          select(-c(percentage_school_prim, percentage_school_ls, year)) # N.B.: this currently assumes a simplification, namely it does not account for demographic changes in the future!
        # note; for improvement, add construction gap coming from completion rate, not from current expenditure
      },


      #' @description
      DeriveEnergyIntensity = function() {
        print("DeriveEnergyIntensity: Education")

        # We classify  the emobied energy intensity of education expenditure as operation energy,
        # and thus do not specify any additional construction energy.
        # Return MJ/USD MER (year.base)
        edu.exio <- ImportEXIOFinalEnergy("Education", mode = "Intensity") %>%
          select(iso, type, elec, unit, e.int = val)

        df.tei.prim <- DF.tei %>%
          filter(grp == "primary") %>%
          select(-e.int) %>%
          left_join(edu.exio)
        df.tei.ls <- DF.tei %>%
          filter(grp == "lower_secondary") %>%
          select(-e.int) %>%
          left_join(edu.exio)
        df.tei <- bind_rows(df.tei.prim, df.tei.ls)



        # translate from MER to PPP
        # Need to convert the price to USD MER base year / kg
        df.price.conv <- calculate_price_index_change()

        # I don't aggregate to R11 to use this for deriving R11 aggregate intensity directly.
        # print(head(df.tei))
        df.tei <- df.tei %>%
          left_join(df.price.conv) %>%
          mutate(e.int = e.int * conv) %>%
          select(-conv)
        # print(head(df.tei))


        # # option 1: take regionally different values and apply them directly:
        #
        # DF.tei <<- DF.tei %>% select(-e.int) %>% left_join(df.tei, by=c("iso", "grp", "type", "elec")) %>%
        #   mutate(e.int = ifelse(type=="CON.new" | type=="CON.rep", 0, e.int))
        #
        # DF.tei <<- AggregateRegion(DF.tei, e.int, population)


        # option 2: take median value of most efficient half and apply these globally (as the threshold is also global) --- take the ratio elec and non-elec to split out total.

        df.tei.global.total <- median((df.tei %>% na.omit() %>% filter(type == "OP", elec == "total") %>% filter(e.int <= median(e.int)))$e.int)
        df.tei.global.elec <- median((df.tei %>% na.omit() %>% filter(type == "OP", elec == "elec") %>% filter(e.int <= median(e.int)))$e.int)
        df.tei.global.non.elec <- median((df.tei %>% na.omit() %>% filter(type == "OP", elec == "non.elec") %>% filter(e.int <= median(e.int)))$e.int)
        e.ratio <- df.tei.global.elec / df.tei.global.total
        ne.ratio <- df.tei.global.non.elec / df.tei.global.total
        multiplier <- 1 / (e.ratio + ne.ratio)
        df.tei.global.elec <- multiplier * df.tei.global.elec
        df.tei.global.non.elec <- multiplier * df.tei.global.non.elec

        # print(DF.tei)

        DF.tei <<- DF.tei %>%
          select(-e.int) %>%
          mutate(e.int = ifelse(type == "OP", ifelse(elec == "total", df.tei.global.total,
            ifelse(elec == "elec", df.tei.global.elec,
              ifelse(elec == "non.elec", df.tei.global.non.elec, NA)
            )
          ), 0))



        # to-do: check (and update) aggregation methods, if needed.
      },

      #' @description
      #' Construct rollout scenario based on identified gap and other assumptions.
      #' This can be coded differently for different scenarios.
      ConstructRolloutScenario = function(scen, yr.tgt = year.target, yr.st.ro=year.base) {
        if (scen == "Income") {
          print("ConstructRolloutScenario:  Education - Income")

          callSuper(scen, yr.tgt = yr.tgt, only.OP = TRUE)
        } else if (scen == "Income.regression") {
          print("ConstructRolloutScenario:  Education - Income Regression")

          callSuper(scen, yr.tgt = yr.tgt, only.OP = TRUE)
        } else if (scen == "ACCEL") {
          print("ConstructRolloutScenario:  Education - ACCEL")

          callSuper(scen, yr.tgt = yr.tgt, only.OP = TRUE, year.start.rollout=yr.st.ro)
        } else {
        }
      }
    )
  )

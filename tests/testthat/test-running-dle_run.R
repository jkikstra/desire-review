#' This tests the scripts that are supposed to be used to run modules/models of to do a DLE run (used in a similar fashion for the Kikstra et al. 2021 paper in ERL)
#'
#'
#'
#' Currently passes locally.

library(testthat)
TEST <<- TRUE

test_that("dle_run.R runs without errors",{
  expect_no_error(source(here::here('R','dle_run.R')))
  })


# No NAs in DLE data
dle.output <- read_csv(
  here(DATA.LOCATION,
       "SSP2_2040_lctFALSE_country.csv"), # note: this needs to change if we change anything in the wrapper setup
  lazy = FALSE # cannot use vroom because we need to delete the files again later, and vroom may lock such large files due to defaulting to lazy loading, see: https://stackoverflow.com/questions/69288188/permission-denied-when-using-file-remove-in-r-after-updating-to-4-1-1
)

test_that("No NAs in data where they should not be ", {

  expect_true(
    sum(is.na(dle.output))==0
  )

  if(!(sum(is.na(dle.output))==0)){
    print("NAs in the following dimensions:\n")
    print(
      dle.output %>% filter(is.na(DLE)) %>%
        distinct(dimension,type)
    )
    print("NAs for the years:\n")
    print(
      dle.output %>% filter(is.na(DLE)) %>%
        distinct(year)
    )
  }

})

test_that("CON.new sum (CON.new = CON.new.pop + CON.new.dls)", {
  # test that CON.new is the same as (CON.new.pop + CON.new.dls)
  for (e in c("total", "elec", "non.elec")){
    expect_equal(
      dle.output %>% filter(type=="CON.new", elec==e) %>% pull(DLE) %>% sum(na.rm = T),
      dle.output %>% filter(type%in%c("CON.new.pop","CON.new.dls"), elec==e) %>% pull(DLE) %>% sum(na.rm = T)
    )
  }

}
)

test_that("elec+non.elec = total",{
  for (t in c("OP", "CON.rep", "CON.new", "CON.new.pop", "CON.new.dls")){
    tot <- dle.output %>% filter(type==t, elec=="total") %>% pull(DLE) %>% sum(na.rm = T)
    sumagg <- dle.output %>% filter(type==t, elec%in%c("elec","non.elec")) %>% pull(DLE) %>% sum(na.rm = T)

    expect_equal(
      tot,
      sumagg,
      label = paste("Failure for type:", t, "\n"),
      tolerance = 1e-8
    )

    if(
      abs(tot-sumagg)>1e-8
    ){
      print("total!=elec+non.elec in the following dimensions:\n")
      dle.agg.issues <- dle.output %>% select(-DLE.pcap,-unit.DLE.pcap) %>%
        pivot_wider(names_from = elec, values_from = DLE) %>%
        mutate(perc.diff = (total-(elec+non.elec)) / total ) %>%
        filter(
          abs(perc.diff)>1e-6
        )
      print(
        dle.agg.issues %>%
          distinct(dimension,type)
      )

    }

  }
})




# End the test file by removing all test-data
TEST <<- FALSE
set_data_location(test=TEST)
remove_test_data_output()

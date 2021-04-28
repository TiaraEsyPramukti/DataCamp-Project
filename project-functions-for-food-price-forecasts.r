
# Load the readr and dplyr packages
library(readr)
library(dplyr)

# Import the potatoes dataset
potato_prices <- read_csv("datasets/Potatoes (Irish).csv")

# Take a glimpse at the contents
glimpse(potato_prices)

library(testthat) 
library(IRkernel.testthat)

soln_potato_prices <- readr::read_csv("datasets/Potatoes (Irish).csv")
run_tests(
    test_that("the answer is correct", {
        expect_s3_class(potato_prices, "data.frame")
        expect_identical(
            colnames(potato_prices), 
            colnames(soln_potato_prices),
            info = "`potato_prices` has the wrong columns. Did you import a food price file?"
        )    
        expect_equal(
            potato_prices,
            soln_potato_prices,
            info = "`potato_prices` contains the wrong values. Did you import the Irish potatoes CSV file?"
        )
    })
)

# Import again, only reading specific columns
potato_prices <- read_csv("datasets/Potatoes (Irish).csv", 
                          col_types = cols_only(adm1_name = col_character(),
                                           mkt_name = col_character(),
                                           cm_name = col_character(), 
                                           mp_month = col_double(), 
                                           mp_year = col_double(), 
                                           mp_price = col_double()))

# Rename the columns to be more informative
potato_prices_renamed <- potato_prices %>%
rename(region = adm1_name,
       market = mkt_name,
       commodity_kg = cm_name,
       month = mp_month,
       year = mp_year,
       price_rwf = mp_price)


# Check the result
potato_prices_renamed

library(testthat) 
library(IRkernel.testthat)
library(readr)
# Don't check potato_prices, in order to allow students to read everything and drop columns with select().
soln_potato_prices_renamed <- read_csv("datasets/Potatoes (Irish).csv",
  col_types = cols_only(
    adm1_name = col_character(),
    mkt_name = col_character(),
    cm_name = col_character(),
    mp_month = col_integer(),
    mp_year = col_integer(),
    mp_price = col_double()
  )
) %>% 
  dplyr::rename(
    region = adm1_name, 
    market = mkt_name,
    commodity_kg = cm_name,
    month = mp_month,
    year = mp_year,
    price_rwf = mp_price
  )
run_tests(
    test_that("the answer is correct", {
        expect_s3_class(potato_prices_renamed, "data.frame")
        expect_identical(
            colnames(potato_prices_renamed), 
            colnames(soln_potato_prices_renamed),
            info = "`potato_prices_renamed` has the wrong columns. Did you rename them as specified in the instructions?"
        )    
        # all.equal() inexplicably fails on the tibble; need to convert to data.frame
        expect_equivalent(
            as.data.frame(potato_prices_renamed),
            as.data.frame(soln_potato_prices_renamed),
            info = "`potato_prices_renamed` contains the wrong values. Did you import the Irish potatoes CSV file and drop/rename columns as specified in the instructions?"
        )
    })
)

# Load lubridate

library(lubridate)

# Convert year and month to Date
potato_prices_cleaned <- potato_prices_renamed %>%
mutate(date = make_date(year, month, "01")) %>%
select(-month,-year)
# .... YOUR CODE FOR TASK 3 ....

# See the result
potato_prices_cleaned

library(testthat) 
library(IRkernel.testthat)
soln_potato_prices_cleaned <- readr::read_csv(
    "datasets/Potatoes (Irish).csv",
    col_types = cols_only(
      adm1_name = col_character(),
      mkt_name = col_character(),
      cm_name = col_character(),
      mp_month = col_integer(),
      mp_year = col_integer(),
      mp_price = col_double()
    )) %>% 
  dplyr::rename(
    region = adm1_name, 
    market = mkt_name,
    commodity_kg = cm_name,
    month = mp_month,
    year = mp_year,
    price_rwf = mp_price
  ) %>% 
  dplyr::mutate(
    date = lubridate::ymd(paste(year, month, "01"))
  ) %>% 
  dplyr::select(-month, -year)
run_tests(
    test_that("the answer is correct", {
        expect_s3_class(potato_prices_cleaned, "data.frame")
        expect_identical(
            colnames(potato_prices_cleaned), 
            colnames(soln_potato_prices_cleaned),
            info = "`potato_prices_cleaned` has the wrong columns. Did you create a `date` column then drop `year` and `month`?"
        )    
        expect_equal(
            as.data.frame(potato_prices_cleaned),
            as.data.frame(soln_potato_prices_cleaned),
            info = "`potato_prices_cleaned` contains the wrong values. Did you create a `date` column then drop `year` and `month`?"
        )
    })
)

# Wrap this code into a function
read_price_data <- function(commodity) {
    
potato_prices <- read_csv(paste("datasets/",commodity,".csv", sep = ""),
  col_types = cols_only(
    adm1_name = col_character(),
    mkt_name = col_character(),
    cm_name = col_character(),
    mp_month = col_integer(),
    mp_year = col_integer(),
    mp_price = col_double()
  )
)

potato_prices_renamed <- potato_prices %>% 
  rename(
    region = adm1_name, 
    market = mkt_name,
    commodity_kg = cm_name,
    month = mp_month,
    year = mp_year,
    price_rwf = mp_price
  )

potato_prices_cleaned <- potato_prices_renamed %>% 
  mutate(
    date = ymd(paste(year, month, "01"))
  ) %>% 
  select(-month, -year)

potato_prices_cleaned
    }

# Test it
pea_prices <- read_price_data("Peas (fresh)")
glimpse(pea_prices)

library(testthat) 
library(IRkernel.testthat)
library(readr)
library(dplyr)
library(lubridate)
soln_read_price_data <- function(commodity) {
  data_file <- paste0("datasets/", commodity, ".csv")
  prices <- read_csv(
    data_file,
    col_types = cols_only(
      adm1_name = col_character(),
      mkt_name = col_character(),
      cm_name = col_character(),
      mp_month = col_integer(),
      mp_year = col_integer(),
      mp_price = col_double()
    )
  )
  
  prices_renamed <- prices %>% 
    rename(
      region = adm1_name, 
      market = mkt_name,
      commodity_kg = cm_name,
      month = mp_month,
      year = mp_year,
      price_rwf = mp_price
    )
    
  prices_renamed %>% 
    mutate(
      date = ymd(paste(year, month, "01"))
    ) %>% 
    select(-month, -year)
}
run_tests({
    test_that("read_price_data() works on the pea dataset", {
        expect_is(read_price_data, "function")
        expect_s3_class(pea_prices, "data.frame")    
        expect_equal(
            pea_prices,
            soln_read_price_data("Peas (fresh)"),
            info = "`pea_prices` contains the wrong values. Does your function wrap the three previous tasks?"
        )
    })    
    test_that("read_price_data() work on another dataset", {   
        expect_equal(
            as.data.frame(read_price_data("Tomatoes")),
            as.data.frame(soln_read_price_data("Tomatoes")),
            info = "Your function gives the wrong answer when applied to other food price datasets."
        )
    })
})

# Load ggplot2
library(ggplot2)

# Draw a line plot of price vs. date grouped by market 
ggplot(potato_prices_cleaned, aes(x = date, y = price_rwf, group = market)) +
geom_line(alpha = 0.2) +
labs(title = "Potato price over time")

library(testthat) 
library(IRkernel.testthat)
library(ggplot2)
stud_plot <- last_plot()
soln_plot <- potato_prices_cleaned %>% 
  ggplot(aes(date, price_rwf, group = market)) +
  geom_line(alpha = 0.2) +
  ggtitle("Potato price over time")

run_tests({
    test_that("plot is drawn correctly", {
        expect_s3_class(stud_plot, "ggplot") 
        expect_identical(
            stud_plot$data,
            soln_plot$data,
            info = 'The plot data is incorrect. Did you use `potato_prices_cleaned`?'
        )      
        expect_identical(
            deparse(stud_plot$mapping$x),
            deparse(soln_plot$mapping$x),
            info = 'The `x` aesthetic is incorrect. Did you map it to `date`?'
        )      
        expect_identical(
            deparse(stud_plot$mapping$y),
            deparse(soln_plot$mapping$y),
            info = 'The `y` aesthetic is incorrect. Did you map it to `price_rwf`?'
        )      
        expect_identical(
            deparse(stud_plot$mapping$group),
            deparse(soln_plot$mapping$group),
            info = 'The `group` aesthetic is incorrect. Did you map it to `market`?'
        )      
        expect_identical(
            class(stud_plot$layers[[1]]$geom)[1],
            class(soln_plot$layers[[1]]$geom)[1],
            info = 'There is no line layer. Did you call `geom_line()`?'
        )     
        expect_identical(
            stud_plot$layers[[1]]$aes_params$alpha,
            soln_plot$layers[[1]]$aes_params$alpha,
            info = 'The line transparency is incorrect. Did you set `alpha` to `0.2`?'
        )    
        expect_identical(
            stud_plot$labels$title,
            soln_plot$labels$title,
            info = 'The plot title is incorrect. Did you `paste()` the commodity name and `"price over time"`?'
        )
    })
})

# Wrap this code into a function
plot_price_vs_time <- function(prices, commodity) {

prices %>% 
  ggplot(aes(date, price_rwf, group = market)) +
  geom_line(alpha = 0.2) +
  ggtitle(paste(commodity," price over time", sep = ""))

    }
# Try the function on the pea data
plot_price_vs_time(pea_prices, "Pea")

library(testthat) 
library(IRkernel.testthat)
library(ggplot2)
soln_plot_price_vs_time <- function(price_data, commodity) {
  price_data %>% 
  ggplot(aes(date, price_rwf, group = market)) +
  geom_line(alpha = 0.2) +
  ggtitle(paste(commodity, "price over time"))
}
soln_plot <- soln_plot_price_vs_time(pea_prices, "Pea")
stud_plot <- plot_price_vs_time(pea_prices, "Pea")
dates <- seq(as.Date("2010-01-01"), as.Date("2015-01-01"), by = "1 month")
n_dates <- length(dates)
n_markets <- 3
alt_data <- data_frame(
  date = rep(dates, 3),
  price_rwf = rnorm(n_dates * n_markets),
  market = gl(n_markets, n_dates)
)
run_tests({
    test_that("plot_price_vs_time() works on the pea dataset", {
        expect_is(plot_price_vs_time, "function")
        expect_s3_class(stud_plot, "ggplot") 
        expect_identical(
            stud_plot$data,
            soln_plot$data,
            info = 'The plot data is incorrect. Did you use `potato_prices_cleaned`?'
        )      
        expect_identical(
            deparse(stud_plot$mapping$x),
            deparse(soln_plot$mapping$x),
            info = 'The `x` aesthetic is incorrect. Did you map it to `date`?'
        )      
        expect_identical(
            deparse(stud_plot$mapping$y),
            deparse(soln_plot$mapping$y),
            info = 'The `y` aesthetic is incorrect. Did you map it to `price_rwf`?'
        )      
        expect_identical(
            deparse(stud_plot$mapping$group),
            deparse(soln_plot$mapping$group),
            info = 'The `group` aesthetic is incorrect. Did you map it to `market`?'
        )      
        expect_identical(
            class(stud_plot$layers[[1]]$geom)[1],
            class(soln_plot$layers[[1]]$geom)[1],
            info = 'There is no line layer. Did you call `geom_line()`?'
        )     
        expect_identical(
            stud_plot$layers[[1]]$aes_params$alpha,
            soln_plot$layers[[1]]$aes_params$alpha,
            info = 'The line transparency is incorrect. Did you set `alpha` to `0.2`?'
        )    
        expect_identical(
            stud_plot$labels$title,
            soln_plot$labels$title,
            info = 'The plot title is incorrect. Did you `paste()` the commodity name and `"price over time"`?'
        )  
    })    
    test_that("plot_price_vs_time() work on another dataset", {   
        expect_equal(
            plot_price_vs_time(alt_data, "Stuff"),
            soln_plot_price_vs_time(alt_data, "Stuff"),
            info = "Your function gives the wrong answer when applied to other datasets."
        )
    })
})

# Group by date, and calculate the median price
potato_prices_summarized <- potato_prices_cleaned %>%
group_by(date) %>%
summarize(median_price_rwf = median(price_rwf))

# See the result
potato_prices_summarized

library(testthat) 
library(IRkernel.testthat)
soln_potato_prices_summarized <- readr::read_csv("datasets/Potatoes (Irish).csv",
  col_types = cols_only(
    adm1_name = col_character(),
    mkt_name = col_character(),
    cm_name = col_character(),
    mp_month = col_integer(),
    mp_year = col_integer(),
    mp_price = col_double()
  )
) %>% 
  dplyr::rename(
    region = adm1_name, 
    market = mkt_name,
    commodity_kg = cm_name,
    month = mp_month,
    year = mp_year,
    price_rwf = mp_price
  ) %>% 
  dplyr::mutate(
    date = lubridate::ymd(paste(year, month, "01"))
  ) %>%
  dplyr::group_by(date) %>% 
  dplyr::summarize(median_price_rwf = median(price_rwf))
run_tests(
    test_that("the answer is correct", {
        expect_s3_class(potato_prices_summarized, "data.frame")
        expect_identical(
            colnames(potato_prices_summarized), 
            colnames(soln_potato_prices_summarized),
            info = "`potato_prices_summarized` has the wrong columns. Did you group by `date` and create a summary column, `median_price_rwf`?"
        )    
        expect_equal(
            as.data.frame(potato_prices_summarized),
            as.data.frame(soln_potato_prices_summarized),
            info = "`potato_prices_summarized` contains the wrong values. Did you import the Irish potatoes CSV file and drop/rename columns as specified in the instructions?"
        )
    })
)

# Load magrittr
library(magrittr)

# Extract a time series
potato_time_series <- potato_prices_summarized %$%
ts(median_price_rwf,
   start = c(year(min(date)), month(min(date))),
   end = c(year(max(date)), month(max(date))),
   frequency = 12)

# See the result
potato_time_series

library(testthat) 
library(IRkernel.testthat)
library(magrittr)
soln_potato_time_series <- readr::read_csv("datasets/Potatoes (Irish).csv",
  col_types = cols_only(
    adm1_name = col_character(),
    mkt_name = col_character(),
    cm_name = col_character(),
    mp_month = col_integer(),
    mp_year = col_integer(),
    mp_price = col_double()
  )
) %>% 
  dplyr::rename(
    region = adm1_name, 
    market = mkt_name,
    commodity_kg = cm_name,
    month = mp_month,
    year = mp_year,
    price_rwf = mp_price
  ) %>% 
  dplyr::mutate(
    date = lubridate::ymd(paste(year, month, "01"))
  ) %>%
  dplyr::group_by(date) %>% 
  dplyr::summarize(median_price_rwf = median(price_rwf)) %$% 
  ts(
    median_price_rwf, 
    start = c(year(min(date)), month(min(date))), 
    end   = c(year(max(date)), month(max(date))), 
    frequency = 12
  )
run_tests(
    test_that("the answer is correct", {
        expect_s3_class(potato_time_series, "ts")
        expect_identical(
            start(potato_time_series), 
            start(soln_potato_time_series),
            info = "`potato_time_series` has the wrong start date. Did you give it the year and month of the first date?"
        )    
        expect_identical(
            end(potato_time_series), 
            end(soln_potato_time_series),
            info = "`potato_time_series` has the wrong end date. Did you give it the year and month of the last date?"
        )    
        expect_identical(
            frequency(potato_time_series), 
            frequency(soln_potato_time_series),
            info = "`potato_time_series` has the wrong frequency. Did you give it the number of months in a year?"
        )    
        expect_equal(
            potato_time_series,
            soln_potato_time_series,
            info = "`potato_time_series` contains the wrong values. Did you construct it from the median prices of the summarized potato data?"
        )
    })
)

# Wrap this code into a function
create_price_time_series <- function(prices) {
    
potato_prices_summarized <- prices %>%
  group_by(date) %>% 
  summarize(median_price_rwf = median(price_rwf))

potato_time_series <- potato_prices_summarized %$% 
  ts(
    median_price_rwf, 
    start = c(year(min(date)), month(min(date))), 
    end   = c(year(max(date)), month(max(date))), 
    frequency = 12
  )
}

# Try the function on the pea data
pea_time_series <- create_price_time_series(pea_prices)
pea_time_series

library(testthat) 
library(IRkernel.testthat)
library(dplyr)
soln_create_price_time_series <- function(prices) {
  prices_summarized <- prices %>%
    group_by(date) %>% 
    summarize(median_price_rwf = median(price_rwf))
  
  prices_summarized %$% 
    ts(
      median_price_rwf, 
      start = c(year(min(date)), month(min(date))), 
      end   = c(year(max(date)), month(max(date))), 
      frequency = 12
    )
}
soln_pea_time_series <- soln_create_price_time_series(pea_prices)
dates <- seq(as.Date("2010-01-01"), as.Date("2015-01-01"), by = "1 month")
n_dates <- length(dates)
n_markets <- 3
alt_data <- data_frame(
  date = rep(dates, 3),
  price_rwf = rnorm(n_dates * n_markets),
  market = gl(n_markets, n_dates)
)
run_tests({
    test_that("create_price_time_series() works on the pea dataset", {
        expect_is(create_price_time_series, "function")
        expect_s3_class(pea_time_series, "ts")
        expect_identical(
            start(pea_time_series), 
            start(soln_pea_time_series),
            info = "`pea_time_series` has the wrong start date. Did you give it the year and month of the first date?"
        )    
        expect_identical(
            end(pea_time_series), 
            end(soln_pea_time_series),
            info = "`pea_time_series` has the wrong end date. Did you give it the year and month of the last date?"
        )    
        expect_identical(
            frequency(pea_time_series), 
            frequency(soln_pea_time_series),
            info = "`pea_time_series` has the wrong frequency. Did you give it the number of months in a year?"
        )    
        expect_equal(
            pea_time_series,
            soln_pea_time_series,
            info = "`pea_time_series` contains the wrong values. Did you construct it from the median prices of the summarized potato data?"
        )
    })    
    test_that("create_price_time_series() work on another dataset", {   
        expect_equal(
            create_price_time_series(alt_data),
            soln_create_price_time_series(alt_data),
            info = "Your function gives the wrong answer when applied to other datasets."
        )
    })
})

# Load forecast
library(forecast)

# Forecast the potato time series
potato_price_forecast <- forecast(potato_time_series)

# View it
potato_price_forecast

# Plot the forecast
autoplot(potato_price_forecast,
        main = "Potato price forecast")

library(testthat) 
library(IRkernel.testthat)
library(magrittr)
library(readr)
stud_plot <- last_plot()
soln_potato_time_series <- read_csv("datasets/Potatoes (Irish).csv",
  col_types = cols_only(
    adm1_name = col_character(),
    mkt_name = col_character(),
    cm_name = col_character(),
    mp_month = col_integer(),
    mp_year = col_integer(),
    mp_price = col_number()
  )
) %>% 
  dplyr::rename(
    region = adm1_name, 
    market = mkt_name,
    commodity_kg = cm_name,
    month = mp_month,
    year = mp_year,
    price_rwf = mp_price
  ) %>% 
  dplyr::mutate(
    date = lubridate::ymd(paste(year, month, "01"))
  ) %>%
  dplyr::group_by(date) %>% 
  dplyr::summarize(median_price_rwf = median(price_rwf)) %$% 
  ts(
    median_price_rwf, 
    start = c(year(min(date)), month(min(date))), 
    end   = c(year(max(date)), month(max(date))), 
    frequency = 12
  ) 
(soln_potato_price_forecast <- forecast::forecast(soln_potato_time_series))
soln_plot <- autoplot(soln_potato_price_forecast, main = "Potato price forecast")
run_tests(
    test_that("the answer is correct", {
        expect_s3_class(potato_price_forecast, "forecast") 
        # Can't test whole object because it contains the time series variable name
        expect_equal(
            potato_price_forecast$lower,
            soln_potato_price_forecast$lower,
            info = "`potato_price_forecast` contains the wrong values. Did you call `forecast()` on the potato time series?"
        )
        expect_identical(
            vapply(stud_plot$layers, function(l) class(l$geom)[1], character(1)),
            vapply(soln_plot$layers, function(l) class(l$geom)[1], character(1)),
            info = "The plot has unexpected contents. Did you call `autoplot()` on the forecast?"
        )
        expect_identical(
            stud_plot$labels$title,
            soln_plot$labels$title,
            info = 'The plot title is incorrect. Did you use `"Potato price forecast"`?'
        )  
    })
)

# Wrap the code into a function
plot_price_forecast <- function(time_series, commodity) {
    
potato_price_forecast <- forecast(time_series)
autoplot(potato_price_forecast, main = paste(commodity, " price forecast", sep = ""))

}

# Try the function on the pea data
plot_price_forecast(pea_time_series, "Pea")

library(testthat) 
library(IRkernel.testthat)
library(forecast)
soln_plot_price_forecast <- function(time_series, commodity) {
  price_forecast <- forecast(time_series)
  autoplot(price_forecast, main = paste(commodity, "price forecast")) 
}
soln_plot <- soln_plot_price_forecast(pea_time_series, "Pea")
stud_plot <- plot_price_forecast(pea_time_series, "Pea")
# expect_equal() does't work with environment element; remove it
stud_plot$plot_env <- NULL 
soln_plot$plot_env <- NULL
alt_time_series <- ts(1:60, frequency = 12)
alt_stud_plot <- plot_price_forecast(alt_time_series, "Chicken")
alt_soln_plot <- soln_plot_price_forecast(alt_time_series, "Chicken")
alt_stud_plot$plot_env <- NULL 
alt_soln_plot$plot_env <- NULL
run_tests({
    test_that("plot_price_forecast() works on the pea dataset", {
        expect_is(plot_price_forecast, "function")
        expect_s3_class(stud_plot, "ggplot") 
        expect_identical(
            stud_plot$labels$title,
            soln_plot$labels$title,
            info = 'The plot title is incorrect. Did you `paste()` the commodity name and `"price forecast"`?'
        )   
        expect_equal(
            stud_plot,
            soln_plot,
            info = "The plot object has an unexpected structure. Did you wrap the code provided?"
        )
    })    
    test_that("plot_price_forecast() work on another dataset", {  
        expect_equal(
            alt_stud_plot,
            alt_soln_plot,
            info = "Your function gives the wrong answer when applied to other datasets."
        )
    })
})

# Choose dry beans as the commodity
commodity <- "Beans (dry)"

# Read the price data
bean_prices <- read_price_data(commodity)

# Plot price vs. time
plot_price_vs_time(bean_prices, commodity)

# Create a price time series
bean_time_series <- create_price_time_series(bean_prices)

# Plot the price forecast
plot_price_forecast(bean_time_series, commodity)

library(testthat) 
library(IRkernel.testthat)
soln_commodity <- "Beans (dry)"
soln_bean_prices <- read_price_data(soln_commodity)
soln_bean_time_series <- create_price_time_series(soln_bean_prices)
run_tests(
    test_that("the answer is correct", {
        expect_s3_class(bean_prices, "data.frame")   
        expect_equal(
            bean_prices,
            soln_bean_prices,
            info = "`bean_prices` contains the wrong values. Did you pass the commodity to `read_price_data()`?"
        )
        expect_s3_class(bean_time_series, "ts")   
        expect_equal(
            bean_time_series,
            soln_bean_time_series,
            info = "`bean_time_series` contains the wrong values. Did you pass the price data to `create_price_time_series()`?"
        )
    })
)

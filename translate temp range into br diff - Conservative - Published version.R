#### CODE FOR CALCULATING THE DIFFERENCE IN BIOLOGICAL RATES FOR EACH TEMPORAL WINDOW ####

# This is the conservative version in the sensitivity test.

# For each temporal window, the median and 90th percentile were calculated only 
# if the temperature records did not contain more than the following percentage 
# of missing values over the period of the temporal window: i) Daily - 10%, 
# ii) Weekly - 30%, iii) Biweekly - 30%, iv) Monthly - 30%, and v) Annual - 30%. 
# In addition, for weekly, biweekly, monthly and annual, there had to be more 
# than 5, 11, 27 and 330 days in the temporal window respectively.

# Briefly, the code extracts the time series from the MySQL database,
# obtains the maximum and minimum temperatures in the temporal windows, 
# computes the difference in biological rates according to the formula given
# in the main paper, and exports the results directly to another table in MySQL.

rm(list = ls())

setwd("C:/Users/Owner/Documents/HOTS Database")
library(RMySQL)

mydb = dbConnect(MySQL(), user='root', password='password', dbname='hots', host='localhost')

#This is needed to avoid error "loading local data is disabled this must be enabled on both the client and server sides "
dbSendQuery(mydb, "SET GLOBAL local_infile = true;")

metadata = dbSendQuery(mydb, "select * from metadata")
metadata = fetch(metadata, n=-1)

#Had a different name for this dataset in MySQL database
metadata$tablee[which(metadata$tablee == "india")] = "indian_ocean"

K = 0.00008617

#We chose 2 different E values to check if our results are robust
E = 0.43259
#E = 0.63
BR = function(x,K) {exp(10.38)*exp(1)^(-E/(K*(x+273.15)))}

start = 1
finish = 971
t1 = Sys.time()

#### daily ####
for (j in start:finish) {
  plot_id = metadata$plot_id[j]
  tablee = metadata$tablee[j]
  freq = metadata$measurement_freq_in_mins[j]
  no_of_records_per_day = 24*60/freq
  if (substr(plot_id, nchar(plot_id), nchar(plot_id)) == "'") {
    data = dbSendQuery(mydb, paste0("SELECT plot_id, measurement_date, oxy_conc, ph, salinity, temp 
                                FROM ", tablee, " WHERE plot_id = '", plot_id, "'';"))
  } else {
    data = dbSendQuery(mydb, paste0("SELECT plot_id, measurement_date, oxy_conc, ph, salinity, temp 
                                FROM ", tablee, " WHERE plot_id = '", plot_id, "';"))
  }
  
  data = fetch(data, n=-1)
  if (length(data$plot_id) > 0) {
    #daily
    max = -99
    min = 99
    count = 0
    DAILY_DATE = vector()
    DAILY_BR_DIFF_COL = vector()
    currentDay = substr(data$measurement_date[1], 9, 10)
    for (i in 1:length(data$plot_id)) {
      
      if (substr(data$measurement_date[i], 9, 10) == currentDay) {
        if (!(is.na(data$temp[i]))) {
          if (data$temp[i] > max) {
            max = data$temp[i]
          }
          if (data$temp[i] < min) {
            min = data$temp[i]
          }
          count = count + 1
        }
      } else {
        if (count > 0.9*no_of_records_per_day) {
          DAILY_DATE = append(DAILY_DATE, substr(data$measurement_date[i-1], 1, 10))
          DAILY_BR_DIFF_COL = append(DAILY_BR_DIFF_COL, BR(max, K)-BR(min, K))
          max = -99
          min = 99
          count = 0
        } else {
          DAILY_DATE = append(DAILY_DATE, substr(data$measurement_date[i-1], 1, 10))
          DAILY_BR_DIFF_COL = append(DAILY_BR_DIFF_COL, NA)
        }
        currentDay = substr(data$measurement_date[i], 9, 10)
        if (!(is.na(data$temp[i]))) {
          if (data$temp[i] > max) {
            max = data$temp[i]
          }
          if (data$temp[i] < min) {
            min = data$temp[i]
          }
          count = count + 1
        }
      }
    }
    #last row of data
    if (count > 0.9*no_of_records_per_day) {
      DAILY_DATE = append(DAILY_DATE, substr(data$measurement_date[i-1], 1, 10))
      DAILY_BR_DIFF_COL = append(DAILY_BR_DIFF_COL, BR(max, K)-BR(min, K))
    } else {
      DAILY_DATE = append(DAILY_DATE, substr(data$measurement_date[i-1], 1, 10))
      DAILY_BR_DIFF_COL = append(DAILY_BR_DIFF_COL, NA)
    }
    
    results = data.frame(plot_id, DAILY_DATE, DAILY_BR_DIFF_COL)
    daily_date_check = which(is.na(results$DAILY_DATE))
    dbWriteTable(mydb, "daily_br_diff_conservative_e063", results, append = TRUE, header = TRUE,
                 row.names = FALSE, nrows = length(results$plot_id), sep = ",", eol = "\n",
                 skip = 0, quote = "\"")
  }
}
t2 = Sys.time()
t2-t1
#### end ####

#### weekly ####
t1 = Sys.time()
for (j in start:finish) {
  plot_id = metadata$plot_id[j]
  tablee = metadata$tablee[j]
  if (substr(plot_id, nchar(plot_id), nchar(plot_id)) == "'") {
    data = dbSendQuery(mydb, paste0("SELECT plot_id, measurement_date, oxy_conc, ph, salinity, temp 
                                FROM ", tablee, " WHERE plot_id = '", plot_id, "'';"))
  } else {
    data = dbSendQuery(mydb, paste0("SELECT plot_id, measurement_date, oxy_conc, ph, salinity, temp 
                                FROM ", tablee, " WHERE plot_id = '", plot_id, "';"))
  }
  
  data = fetch(data, n=-1)
  if (length(data$plot_id) > 0) {
    #weekly
    max = -99
    min = 99
    count = 0
    countNAs = 0
    WEEKLY_DATE = vector()
    WEEKLY_BR_DIFF_COL = vector()
    currentDay = substr(data$measurement_date[1], 9, 10)
    first_day_of_week = substr(data$measurement_date[1], 1, 10)
    dayCount = 1
    for (i in 1:length(data$plot_id)) {
      
      if (substr(data$measurement_date[i], 9, 10) != currentDay) {
        currentDay = substr(data$measurement_date[i], 9, 10)
        dayCount = dayCount + 1
      }
      
      if (dayCount != 8) {
        if (!(is.na(data$temp[i]))) {
          if (data$temp[i] > max) {
            max = data$temp[i]
          }
          if (data$temp[i] < min) {
            min = data$temp[i]
          }
          count = count + 1
        } else {
          countNAs = countNAs + 1
        }
      } else {
        if ((countNAs/(countNAs+count)) < 0.3 & dayCount > 5) {
          WEEKLY_DATE = append(WEEKLY_DATE, first_day_of_week)
          WEEKLY_BR_DIFF_COL = append(WEEKLY_BR_DIFF_COL, BR(max, K)-BR(min, K))
          max = -99
          min = 99
          count = 0
          countNAs = 0
        } else {
          WEEKLY_DATE = append(WEEKLY_DATE, first_day_of_week)
          WEEKLY_BR_DIFF_COL = append(WEEKLY_BR_DIFF_COL, NA)
        }
        first_day_of_week = substr(data$measurement_date[i], 1, 10)
        dayCount = 1
        if (!(is.na(data$temp[i]))) {
          if (data$temp[i] > max) {
            max = data$temp[i]
          }
          if (data$temp[i] < min) {
            min = data$temp[i]
          }
          count = count + 1
        } else {
          countNAs = countNAs + 1
        }
      }
    }
    #last row of data
    if ((countNAs/(countNAs+count)) < 0.3 & dayCount > 5) {
      WEEKLY_DATE = append(WEEKLY_DATE, substr(data$measurement_date[i-1], 1, 10))
      WEEKLY_BR_DIFF_COL = append(WEEKLY_BR_DIFF_COL, BR(max, K)-BR(min, K))
    } else {
      WEEKLY_DATE = append(WEEKLY_DATE, substr(data$measurement_date[i-1], 1, 10))
      WEEKLY_BR_DIFF_COL = append(WEEKLY_BR_DIFF_COL, NA)
    }
    
    results = data.frame(plot_id, WEEKLY_DATE, WEEKLY_BR_DIFF_COL)
    weekly_date_check = which(is.na(results$WEEKLY_DATE))
    dbWriteTable(mydb, "weekly_br_diff_conservative_e063", results, append = TRUE, header = TRUE,
                 row.names = FALSE, nrows = length(results$plot_id), sep = ",", eol = "\n",
                 skip = 0, quote = "\"")
  }
}
t2 = Sys.time()
t2-t1
#### end ####

#### biweekly ####
t1 = Sys.time()
for (j in start:finish) {
  plot_id = metadata$plot_id[j]
  tablee = metadata$tablee[j]
  if (substr(plot_id, nchar(plot_id), nchar(plot_id)) == "'") {
    data = dbSendQuery(mydb, paste0("SELECT plot_id, measurement_date, oxy_conc, ph, salinity, temp 
                                FROM ", tablee, " WHERE plot_id = '", plot_id, "'';"))
  } else {
    data = dbSendQuery(mydb, paste0("SELECT plot_id, measurement_date, oxy_conc, ph, salinity, temp 
                                FROM ", tablee, " WHERE plot_id = '", plot_id, "';"))
  }
  
  data = fetch(data, n=-1)
  if (length(data$plot_id) > 0) {
    #biweekly
    max = -99
    min = 99
    count = 0
    countNAs = 0
    BIWEEKLY_DATE = vector()
    BIWEEKLY_BR_DIFF_COL = vector()
    currentDay = substr(data$measurement_date[1], 9, 10)
    first_day_of_week = substr(data$measurement_date[1], 1, 10)
    dayCount = 1
    for (i in 1:length(data$plot_id)) {
      
      if (substr(data$measurement_date[i], 9, 10) != currentDay) {
        currentDay = substr(data$measurement_date[i], 9, 10)
        dayCount = dayCount + 1
      }
      
      if (dayCount != 15) {
        if (!(is.na(data$temp[i]))) {
          if (data$temp[i] > max) {
            max = data$temp[i]
          }
          if (data$temp[i] < min) {
            min = data$temp[i]
          }
          count = count + 1
        } else {
          countNAs = countNAs + 1
        }
      } else {
        if ((countNAs/(countNAs+count)) < 0.3 & dayCount > 11) {
          BIWEEKLY_DATE = append(BIWEEKLY_DATE, first_day_of_week)
          BIWEEKLY_BR_DIFF_COL = append(BIWEEKLY_BR_DIFF_COL, BR(max, K)-BR(min, K))
          max = -99
          min = 99
          count = 0
          countNAs = 0
        } else {
          BIWEEKLY_DATE = append(BIWEEKLY_DATE, first_day_of_week)
          BIWEEKLY_BR_DIFF_COL = append(BIWEEKLY_BR_DIFF_COL, NA)
        }
        first_day_of_week = substr(data$measurement_date[i], 1, 10)
        dayCount = 1
        if (!(is.na(data$temp[i]))) {
          if (data$temp[i] > max) {
            max = data$temp[i]
          }
          if (data$temp[i] < min) {
            min = data$temp[i]
          }
          count = count + 1
        } else {
          countNAs = countNAs + 1
        }
      }
    }
    #last row of data
    if ((countNAs/(countNAs+count)) < 0.3 & dayCount > 11) {
      BIWEEKLY_DATE = append(BIWEEKLY_DATE, substr(data$measurement_date[i-1], 1, 10))
      BIWEEKLY_BR_DIFF_COL = append(BIWEEKLY_BR_DIFF_COL, BR(max, K)-BR(min, K))
    } else {
      BIWEEKLY_DATE = append(BIWEEKLY_DATE, substr(data$measurement_date[i-1], 1, 10))
      BIWEEKLY_BR_DIFF_COL = append(BIWEEKLY_BR_DIFF_COL, NA)
    }
    
    results = data.frame(plot_id, BIWEEKLY_DATE, BIWEEKLY_BR_DIFF_COL)
    biweekly_date_check = which(is.na(results$BIWEEKLY_DATE))
    dbWriteTable(mydb, "biweekly_br_diff_conservative_e063", results, append = TRUE, header = TRUE,
                 row.names = FALSE, nrows = length(results$plot_id), sep = ",", eol = "\n",
                 skip = 0, quote = "\"")
  }
}
t2 = Sys.time()
t2-t1
#### end ####

#### monthly ####
t1 = Sys.time()
for (j in start:finish) {
  plot_id = metadata$plot_id[j]
  tablee = metadata$tablee[j]
  if (substr(plot_id, nchar(plot_id), nchar(plot_id)) == "'") {
    data = dbSendQuery(mydb, paste0("SELECT plot_id, measurement_date, oxy_conc, ph, salinity, temp 
                                FROM ", tablee, " WHERE plot_id = '", plot_id, "'';"))
  } else {
    data = dbSendQuery(mydb, paste0("SELECT plot_id, measurement_date, oxy_conc, ph, salinity, temp 
                                FROM ", tablee, " WHERE plot_id = '", plot_id, "';"))
  }
  
  data = fetch(data, n=-1)
  if (length(data$plot_id) > 0) {
    #monthly
    max = -99
    min = 99
    count = 0
    countNAs = 0
    MONTHLY_DATE = vector()
    MONTHLY_BR_DIFF_COL = vector()
    currentDay = substr(data$measurement_date[1], 9, 10)
    first_day_of_month = substr(data$measurement_date[1], 1, 10)
    dayCount = 1
    for (i in 1:length(data$plot_id)) {
      
      if (substr(data$measurement_date[i], 9, 10) != currentDay) {
        currentDay = substr(data$measurement_date[i], 9, 10)
        dayCount = dayCount + 1
      }
      
      if (dayCount != 31) {
        if (!(is.na(data$temp[i]))) {
          if (data$temp[i] > max) {
            max = data$temp[i]
          }
          if (data$temp[i] < min) {
            min = data$temp[i]
          }
          count = count + 1
        } else {
          countNAs = countNAs + 1
        }
      } else {
        if ((countNAs/(countNAs+count)) < 0.3 & dayCount > 27) {
          MONTHLY_DATE = append(MONTHLY_DATE, first_day_of_month)
          MONTHLY_BR_DIFF_COL = append(MONTHLY_BR_DIFF_COL, BR(max, K)-BR(min, K))
          max = -99
          min = 99
          count = 0
          countNAs = 0
        } else {
          MONTHLY_DATE = append(MONTHLY_DATE, first_day_of_month)
          MONTHLY_BR_DIFF_COL = append(MONTHLY_BR_DIFF_COL, NA)
        }
        first_day_of_month = substr(data$measurement_date[i], 1, 10)
        dayCount = 1
        if (!(is.na(data$temp[i]))) {
          if (data$temp[i] > max) {
            max = data$temp[i]
          }
          if (data$temp[i] < min) {
            min = data$temp[i]
          }
          count = count + 1
        } else {
          countNAs = countNAs + 1
        }
      }
    }
    #last row of data
    if ((countNAs/(countNAs+count)) < 0.3 & dayCount > 27) {
      MONTHLY_DATE = append(MONTHLY_DATE, substr(data$measurement_date[i-1], 1, 10))
      MONTHLY_BR_DIFF_COL = append(MONTHLY_BR_DIFF_COL, BR(max, K)-BR(min, K))
    } else {
      MONTHLY_DATE = append(MONTHLY_DATE, substr(data$measurement_date[i-1], 1, 10))
      MONTHLY_BR_DIFF_COL = append(MONTHLY_BR_DIFF_COL, NA)
    }
    
    results = data.frame(plot_id, MONTHLY_DATE, MONTHLY_BR_DIFF_COL)
    monthly_date_check = which(is.na(results$MONTHLY_DATE))
    dbWriteTable(mydb, "monthly_br_diff_conservative_e063", results, append = TRUE, header = TRUE,
                 row.names = FALSE, nrows = length(results$plot_id), sep = ",", eol = "\n",
                 skip = 0, quote = "\"")
  }
}
t2 = Sys.time()
t2-t1
#### end ####

#### annual ####
t1 = Sys.time()
for (j in start:finish) {
  plot_id = metadata$plot_id[j]
  tablee = metadata$tablee[j]
  if (substr(plot_id, nchar(plot_id), nchar(plot_id)) == "'") {
    data = dbSendQuery(mydb, paste0("SELECT plot_id, measurement_date, oxy_conc, ph, salinity, temp 
                                FROM ", tablee, " WHERE plot_id = '", plot_id, "'';"))
  } else {
    data = dbSendQuery(mydb, paste0("SELECT plot_id, measurement_date, oxy_conc, ph, salinity, temp 
                                FROM ", tablee, " WHERE plot_id = '", plot_id, "';"))
  }
  
  data = fetch(data, n=-1)
  if (length(data$plot_id) > 0) {
    #annual
    max = -99
    min = 99
    count = 0
    countNAs = 0
    ANNUAL_DATE = vector()
    ANNUAL_BR_DIFF_COL = vector()
    currentDay = substr(data$measurement_date[1], 9, 10)
    first_day_of_year = substr(data$measurement_date[1], 1, 10)
    dayCount = 1
    for (i in 1:length(data$plot_id)) {
      
      if (substr(data$measurement_date[i], 9, 10) != currentDay) {
        currentDay = substr(data$measurement_date[i], 9, 10)
        dayCount = dayCount + 1
      }
      
      if (dayCount != 366) {
        if (!(is.na(data$temp[i]))) {
          if (data$temp[i] > max) {
            max = data$temp[i]
          }
          if (data$temp[i] < min) {
            min = data$temp[i]
          }
          count = count + 1
        } else {
          countNAs = countNAs + 1
        }
      } else {
        if ((countNAs/(countNAs+count)) < 0.3 & dayCount > 330) {
          ANNUAL_DATE = append(ANNUAL_DATE, first_day_of_year)
          ANNUAL_BR_DIFF_COL = append(ANNUAL_BR_DIFF_COL, BR(max, K)-BR(min, K))
          max = -99
          min = 99
          count = 0
          countNAs = 0
        } else {
          ANNUAL_DATE = append(ANNUAL_DATE, first_day_of_year)
          ANNUAL_BR_DIFF_COL = append(ANNUAL_BR_DIFF_COL, NA)
        }
        first_day_of_year = substr(data$measurement_date[i], 1, 10)
        dayCount = 1
        if (!(is.na(data$temp[i]))) {
          if (data$temp[i] > max) {
            max = data$temp[i]
          }
          if (data$temp[i] < min) {
            min = data$temp[i]
          }
          count = count + 1
        } else {
          countNAs = countNAs + 1
        }
      }
    }
    #last row of data
    if ((countNAs/(countNAs+count)) < 0.3 & dayCount > 330) {
      ANNUAL_DATE = append(ANNUAL_DATE, substr(data$measurement_date[i-1], 1, 10))
      ANNUAL_BR_DIFF_COL = append(ANNUAL_BR_DIFF_COL, BR(max, K)-BR(min, K))
    } else {
      ANNUAL_DATE = append(ANNUAL_DATE, substr(data$measurement_date[i-1], 1, 10))
      ANNUAL_BR_DIFF_COL = append(ANNUAL_BR_DIFF_COL, NA)
    }
    
    results = data.frame(plot_id, ANNUAL_DATE, ANNUAL_BR_DIFF_COL)
    annual_date_check = which(is.na(results$ANNUAL_DATE))
    dbWriteTable(mydb, "annual_br_diff_conservative_e063", results, append = TRUE, header = TRUE,
                 row.names = FALSE, nrows = length(results$plot_id), sep = ",", eol = "\n",
                 skip = 0, quote = "\"")
  }
}
#### end ####

t2 = Sys.time()
t2-t1

dbDisconnect(mydb)

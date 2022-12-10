#### CODE FOR CALCULATING THE TEMPERATURE RANGE FOR EACH TEMPORAL WINDOW ####

# This is the conservative version in the sensitivity test.

# For each temporal window, the median and 90th percentile were calculated only 
# if the temperature records did not contain more than the following percentage 
# of missing values over the period of the temporal window: i) Daily - 10%, 
# ii) Weekly - 30%, iii) Biweekly - 30%, iv) Monthly - 30%, and v) Annual - 30%. 
# In addition, for weekly, biweekly, monthly and annual, there had to be more 
# than 5, 11, 27 and 330 days in the temporal window respectively.

# Briefly, the code extracts the time series from the MySQL database,
# calculates the temperature range, and exports the results directly to 
# another table in MySQL. 

rm(list = ls())

t1 = Sys.time()
library(RMySQL)

setwd("C:/Users/Owner/Documents/HOTS Database")
mydb = dbConnect(MySQL(), user='root', password='password', dbname='hots', host='localhost')

#This is needed to avoid error "loading local data is disabled this must be enabled on both the client and server sides "
dbSendQuery(mydb, "SET GLOBAL local_infile = true;") 

metadata = read.csv("metadata.csv")

#Had a different name for this dataset in MySQL database
metadata$tablee[which(metadata$tablee == "india")] = "indian_ocean"

start = 1
finish = length(metadata$plot_id)

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
    total = 0
    count = 0
    DAILY_DATE = vector()
    DAILY_MAX_CHANGE = vector()
    DAILY_MEAN = vector()
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
          total = total + data$temp[i]
        }
      } else {
        if (count > 0.9*no_of_records_per_day) {
          DAILY_DATE = append(DAILY_DATE, substr(data$measurement_date[i-1], 1, 10))
          DAILY_MAX_CHANGE = append(DAILY_MAX_CHANGE, (max - min))
          DAILY_MEAN = append(DAILY_MEAN, (total/count))
          max = -99
          min = 99
          total = 0
          count = 0
        } else {
          DAILY_DATE = append(DAILY_DATE, substr(data$measurement_date[i-1], 1, 10))
          DAILY_MAX_CHANGE = append(DAILY_MAX_CHANGE, NA)
          DAILY_MEAN = append(DAILY_MEAN, NA)
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
          total = total + data$temp[i]
        }
      }
    }
    #last row of data
    if (count > 0.9*no_of_records_per_day) {
      DAILY_DATE = append(DAILY_DATE, substr(data$measurement_date[i-1], 1, 10))
      DAILY_MAX_CHANGE = append(DAILY_MAX_CHANGE, (max - min))
      DAILY_MEAN = append(DAILY_MEAN, (total/count))
    } else {
      DAILY_DATE = append(DAILY_DATE, substr(data$measurement_date[i-1], 1, 10))
      DAILY_MAX_CHANGE = append(DAILY_MAX_CHANGE, NA)
      DAILY_MEAN = append(DAILY_MEAN, NA)
    }
    
    results = data.frame(plot_id, DAILY_DATE, DAILY_MAX_CHANGE, DAILY_MEAN)
    daily_date_check = which(is.na(results$DAILY_DATE))
    dbWriteTable(mydb, "daily_temp_conservative", results, append = TRUE, header = TRUE,
                 row.names = FALSE, nrows = length(results$plot_id), sep = ",", eol = "\n",
                 skip = 0, quote = "\"")
  }
}


#### end ####

#### weekly ####
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
    total = 0
    count = 0
    countNAs = 0
    WEEKLY_DATE = vector()
    WEEKLY_MAX_CHANGE = vector()
    WEEKLY_MEAN = vector()
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
          total = total + data$temp[i]
        } else {
          countNAs = countNAs + 1
        }
      } else {
        if ((countNAs/(countNAs+count)) < 0.3) {
          WEEKLY_DATE = append(WEEKLY_DATE, first_day_of_week)
          WEEKLY_MAX_CHANGE = append(WEEKLY_MAX_CHANGE, (max - min))
          WEEKLY_MEAN = append(WEEKLY_MEAN, (total/count))
          max = -99
          min = 99
          total = 0
          count = 0
          countNAs = 0
        } else {
          WEEKLY_DATE = append(WEEKLY_DATE, first_day_of_week)
          WEEKLY_MAX_CHANGE = append(WEEKLY_MAX_CHANGE, NA)
          WEEKLY_MEAN = append(WEEKLY_MEAN, NA)
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
          total = total + data$temp[i]
        } else {
          countNAs = countNAs + 1
        }
      }
    }
    #last row of data
    if ((countNAs/(countNAs+count)) < 0.3 & dayCount > 5) {
      # dayCount should be at least 6
      # if too little data, not reflective of the temporal window
      # if too little data just by a small margin, can still reflect temporal window
      WEEKLY_DATE = append(WEEKLY_DATE, first_day_of_week)
      WEEKLY_MAX_CHANGE = append(WEEKLY_MAX_CHANGE, (max - min))
      WEEKLY_MEAN = append(WEEKLY_MEAN, (total/count))
    } else {
      WEEKLY_DATE = append(WEEKLY_DATE, first_day_of_week)
      WEEKLY_MAX_CHANGE = append(WEEKLY_MAX_CHANGE, NA)
      WEEKLY_MEAN = append(WEEKLY_MEAN, NA)
    }
    
    results = data.frame(plot_id, WEEKLY_DATE, WEEKLY_MAX_CHANGE, WEEKLY_MEAN)
    weekly_date_check = which(is.na(results$WEEKLY_DATE))
    dbWriteTable(mydb, "weekly_temp_conservative", results, append = TRUE, header = TRUE,
                 row.names = FALSE, nrows = length(results$plot_id), sep = ",", eol = "\n",
                 skip = 0, quote = "\"")
  }
}

#### end ####

#### biweekly ####
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
    total = 0
    count = 0
    countNAs = 0
    BIWEEKLY_DATE = vector()
    BIWEEKLY_MAX_CHANGE = vector()
    BIWEEKLY_MEAN = vector()
    currentDay = substr(data$measurement_date[1], 9, 10)
    first_day_of_biweek = substr(data$measurement_date[1], 1, 10)
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
          total = total + data$temp[i]
        } else {
          countNAs = countNAs + 1
        }
      } else {
        if ((countNAs/(countNAs+count)) < 0.3) {
          BIWEEKLY_DATE = append(BIWEEKLY_DATE, first_day_of_biweek)
          BIWEEKLY_MAX_CHANGE = append(BIWEEKLY_MAX_CHANGE, (max - min))
          BIWEEKLY_MEAN = append(BIWEEKLY_MEAN, (total/count))
          max = -99
          min = 99
          total = 0
          count = 0
          countNAs = 0
        } else {
          BIWEEKLY_DATE = append(BIWEEKLY_DATE, first_day_of_biweek)
          BIWEEKLY_MAX_CHANGE = append(BIWEEKLY_MAX_CHANGE, NA)
          BIWEEKLY_MEAN = append(BIWEEKLY_MEAN, NA)
        }
        first_day_of_biweek = substr(data$measurement_date[i], 1, 10)
        dayCount = 1
        if (!(is.na(data$temp[i]))) {
          if (data$temp[i] > max) {
            max = data$temp[i]
          }
          if (data$temp[i] < min) {
            min = data$temp[i]
          }
          count = count + 1
          total = total + data$temp[i]
        } else {
          countNAs = countNAs + 1
        }
      }
    }
    #last row of data
    if ((countNAs/(countNAs+count)) < 0.3 & dayCount > 11) {
      # dayCount should be at least 12
      # if too little data, not reflective of the temporal window
      # if too little data just by a small margin, can still reflect temporal window
      BIWEEKLY_DATE = append(BIWEEKLY_DATE, first_day_of_biweek)
      BIWEEKLY_MAX_CHANGE = append(BIWEEKLY_MAX_CHANGE, (max - min))
      BIWEEKLY_MEAN = append(BIWEEKLY_MEAN, (total/count))
    } else {
      BIWEEKLY_DATE = append(BIWEEKLY_DATE, first_day_of_biweek)
      BIWEEKLY_MAX_CHANGE = append(BIWEEKLY_MAX_CHANGE, NA)
      BIWEEKLY_MEAN = append(BIWEEKLY_MEAN, NA)
    }
    
    results = data.frame(plot_id, BIWEEKLY_DATE, BIWEEKLY_MAX_CHANGE, BIWEEKLY_MEAN)
    biweekly_date_check = which(is.na(results$BIWEEKLY_DATE))
    dbWriteTable(mydb, "biweekly_temp_conservative", results, append = TRUE, header = TRUE,
                 row.names = FALSE, nrows = length(results$plot_id), sep = ",", eol = "\n",
                 skip = 0, quote = "\"")
  }
}

#### end ####

#### monthly ####
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
    total = 0
    count = 0
    countNAs = 0
    MONTHLY_DATE = vector()
    MONTHLY_MAX_CHANGE = vector()
    MONTHLY_MEAN = vector()
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
          total = total + data$temp[i]
        } else {
          countNAs = countNAs + 1
        }
      } else {
        if ((countNAs/(countNAs+count)) < 0.3) {
          MONTHLY_DATE = append(MONTHLY_DATE, first_day_of_month)
          MONTHLY_MAX_CHANGE = append(MONTHLY_MAX_CHANGE, (max - min))
          MONTHLY_MEAN = append(MONTHLY_MEAN, (total/count))
          max = -99
          min = 99
          total = 0
          count = 0
          countNAs = 0
        } else {
          MONTHLY_DATE = append(MONTHLY_DATE, first_day_of_month)
          MONTHLY_MAX_CHANGE = append(MONTHLY_MAX_CHANGE, NA)
          MONTHLY_MEAN = append(MONTHLY_MEAN, NA)
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
          total = total + data$temp[i]
        } else {
          countNAs = countNAs + 1
        }
      }
    }
    #last row of data
    if ((countNAs/(countNAs+count)) < 0.3 & dayCount > 27) {
      # dayCount should be at least 28
      # if too little data, not reflective of the temporal window
      # if too little data just by a small margin, can still reflect temporal window
      MONTHLY_DATE = append(MONTHLY_DATE, first_day_of_month)
      MONTHLY_MAX_CHANGE = append(MONTHLY_MAX_CHANGE, (max - min))
      MONTHLY_MEAN = append(MONTHLY_MEAN, (total/count))
    } else {
      MONTHLY_DATE = append(MONTHLY_DATE, first_day_of_month)
      MONTHLY_MAX_CHANGE = append(MONTHLY_MAX_CHANGE, NA)
      MONTHLY_MEAN = append(MONTHLY_MEAN, NA)
    }
    
    results = data.frame(plot_id, MONTHLY_DATE, MONTHLY_MAX_CHANGE, MONTHLY_MEAN)
    monthly_date_check = which(is.na(results$MONTHLY_DATE))
    dbWriteTable(mydb, "monthly_temp_conservative", results, append = TRUE, header = TRUE,
                 row.names = FALSE, nrows = length(results$plot_id), sep = ",", eol = "\n",
                 skip = 0, quote = "\"")
  }
}

#### end ####

#### annual ####
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
    total = 0
    count = 0
    countNAs = 0
    ANNUAL_DATE = vector()
    ANNUAL_MAX_CHANGE = vector()
    ANNUAL_MEAN = vector()
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
          total = total + data$temp[i]
        } else {
          countNAs = countNAs + 1
        }
      } else {
        if ((countNAs/(countNAs+count)) < 0.3) {
          ANNUAL_DATE = append(ANNUAL_DATE, first_day_of_year)
          ANNUAL_MAX_CHANGE = append(ANNUAL_MAX_CHANGE, (max - min))
          ANNUAL_MEAN = append(ANNUAL_MEAN, (total/count))
          max = -99
          min = 99
          total = 0
          count = 0
          countNAs = 0
        } else {
          ANNUAL_DATE = append(ANNUAL_DATE, first_day_of_year)
          ANNUAL_MAX_CHANGE = append(ANNUAL_MAX_CHANGE, NA)
          ANNUAL_MEAN = append(ANNUAL_MEAN, NA)
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
          total = total + data$temp[i]
        } else {
          countNAs = countNAs + 1
        }
      }
    }
    #last row of data
    if ((countNAs/(countNAs+count)) < 0.3 & dayCount > 330) {
      # dayCount should be at least 331
      # if too little data, not reflective of the temporal window
      # if too little data just by a small margin, can still reflect temporal window
      ANNUAL_DATE = append(ANNUAL_DATE, first_day_of_year)
      ANNUAL_MAX_CHANGE = append(ANNUAL_MAX_CHANGE, (max - min))
      ANNUAL_MEAN = append(ANNUAL_MEAN, (total/count))
    } else {
      ANNUAL_DATE = append(ANNUAL_DATE, first_day_of_year)
      ANNUAL_MAX_CHANGE = append(ANNUAL_MAX_CHANGE, NA)
      ANNUAL_MEAN = append(ANNUAL_MEAN, NA)
    }
    
    results = data.frame(plot_id, ANNUAL_DATE, ANNUAL_MAX_CHANGE, ANNUAL_MEAN)
    annual_date_check = which(is.na(results$ANNUAL_DATE))
    dbWriteTable(mydb, "annual_temp_conservative", results, append = TRUE, header = TRUE,
                 row.names = FALSE, nrows = length(results$plot_id), sep = ",", eol = "\n",
                 skip = 0, quote = "\"")
  }
}
#### end ####

dbDisconnect(mydb)

daily_date_check
weekly_date_check
biweekly_date_check
monthly_date_check
annual_date_check

t2 = Sys.time()
t2 - t1

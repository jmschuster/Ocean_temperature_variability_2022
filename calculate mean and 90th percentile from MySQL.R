# NOTE: Change table names accordingly (e.g. this instance is for tables of 
# br_diff with an E value of 0.433. Repeat for the other tables: temp_ranges,
# conservative versions, etc.)

rm(list = ls())

setwd("C:/Users/Owner/Documents/HOTS Database/Final Paper")

library(RMySQL)

gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(which(x > 0)))
}

#IMPORTANT: change password to password -> ALTER USER 'username'@'localhost'
#IDENTIFIED WITH mysql_native_password BY 'password';
mydb = dbConnect(MySQL(), user='root', password='password', dbname='hots', host='localhost')

#To retrieve data from the database we need to save a results set object.
metadata = dbSendQuery(mydb, "select * from metadata")

#This saves the results of the query as a data frame object. 
#The n in the function specifies the number of records to retrieve, using n=-1 retrieves all pending records.
metadata = fetch(metadata, n=-1)

size_font = 15

#filter by depth
metadata = metadata[which(metadata$depth_in_m <= 12.5), ]
metadata = metadata[which(metadata$depth_in_m != -999.0), ]
metadata = metadata[which(metadata$climate_classification != "Polar"), ]

#reclassify climate classification
metadata <- metadata %>% 
  mutate(climate_classification = ifelse(abs(metadata$lat_in_degrees) < 20, "Tropical", 
                                         ifelse(abs(metadata$lat_in_degrees) > 35, "Temperate", "Subtropical")))

#### daily_br_diff ####
daily_br_diff = dbSendQuery(mydb, "select * from daily_br_diff")
daily_br_diff = fetch(daily_br_diff, n=-1)

daily_br_diff = daily_br_diff[-which(!(
  daily_br_diff$plot_id %in% metadata$plot_id)
),]

daily_br_diff$climate_classification = NA
for (i in unique(daily_br_diff$plot_id)) {
  daily_br_diff$climate_classification[which(daily_br_diff$plot_id == i)] = 
    metadata$climate_classification[which(metadata$plot_id == i)]
}
daily_br_diff$depth = NA
for (i in unique(daily_br_diff$plot_id)) {
  daily_br_diff$depth[which(daily_br_diff$plot_id == i)] = 
    metadata$depth_in_m[which(metadata$plot_id == i)]
}

daily_br_diff = daily_br_diff[
  order(match(daily_br_diff$plot_id, metadata$plot_id)),]

#group plot ids extending multiple time periods
daily_br_diff <- daily_br_diff %>% 
  mutate(plot_id = gsub( " .*$", "", daily_br_diff$plot_id))

#summarize data for each plot id
a_mean_df <- aggregate(daily_br_diff_col ~ plot_id , 
                       daily_br_diff, function(i) mean(i))
gm_mean_df <- aggregate(daily_br_diff_col ~ plot_id , 
                        daily_br_diff, function(i) gm_mean(i))
median_df <- aggregate(daily_br_diff_col ~ plot_id , 
                       daily_br_diff, function(i) median(i))
ninetieth_percentile_df <- aggregate(daily_br_diff_col ~ plot_id , 
                                     daily_br_diff, function(i) quantile(i, 0.9))
results_summary = data.frame(a_mean_df$plot_id, a_mean_df$daily_br_diff_col,
                             gm_mean_df$daily_br_diff_col, median_df$daily_br_diff_col,
                             ninetieth_percentile_df$daily_br_diff_col)
colnames(results_summary) = c("plot_id", "daily_a_mean", "daily_gm_mean",
                              "daily_median", "daily_ninetieth_perc")

metadata$plot_id = gsub( " .*$", "", metadata$plot_id)
metadata = metadata[match(unique(metadata$plot_id), metadata$plot_id),]
results_summary = metadata[,c(2:11,20:21,26)] %>% full_join(results_summary, by = "plot_id")

#### end ####

#### weekly_br_diff ####
metadata = dbSendQuery(mydb, "select * from metadata")
metadata = fetch(metadata, n=-1)
metadata = metadata[which(metadata$depth_in_m <= 12.5), ]
metadata = metadata[which(metadata$depth_in_m != -999.0), ]
metadata = metadata[which(metadata$climate_classification != "Polar"), ]
metadata <- metadata %>% 
  mutate(climate_classification = ifelse(abs(metadata$lat_in_degrees) < 20, "Tropical", 
                                         ifelse(abs(metadata$lat_in_degrees) > 35, "Temperate", "Subtropical")))


weekly_br_diff = dbSendQuery(mydb, "select * from weekly_br_diff")
weekly_br_diff = fetch(weekly_br_diff, n=-1)

weekly_br_diff = weekly_br_diff[-which(!(
  weekly_br_diff$plot_id %in% metadata$plot_id)
),]

weekly_br_diff$climate_classification = NA
for (i in unique(weekly_br_diff$plot_id)) {
  weekly_br_diff$climate_classification[which(weekly_br_diff$plot_id == i)] = 
    metadata$climate_classification[which(metadata$plot_id == i)]
}
weekly_br_diff$depth = NA
for (i in unique(weekly_br_diff$plot_id)) {
  weekly_br_diff$depth[which(weekly_br_diff$plot_id == i)] = 
    metadata$depth_in_m[which(metadata$plot_id == i)]
}

#reorder plot id by order in metadata 
weekly_br_diff = weekly_br_diff[order(match(weekly_br_diff$plot_id, metadata$plot_id)),]
#end

#group plot ids extending multiple time periods
weekly_br_diff <- weekly_br_diff %>% 
  mutate(plot_id = gsub( " .*$", "", weekly_br_diff$plot_id))
#

#summarize data for each plot id 
a_mean_df <- aggregate(weekly_br_diff_col ~ plot_id , 
                       weekly_br_diff, function(i) mean(i))
gm_mean_df <- aggregate(weekly_br_diff_col ~ plot_id , 
                        weekly_br_diff, function(i) gm_mean(i))
median_df <- aggregate(weekly_br_diff_col ~ plot_id , 
                       weekly_br_diff, function(i) median(i))
ninetieth_percentile_df <- aggregate(weekly_br_diff_col ~ plot_id , 
                                     weekly_br_diff, function(i) quantile(i, 0.9))
temp_results_summary = data.frame(a_mean_df$plot_id, a_mean_df$weekly_br_diff_col,
                             gm_mean_df$weekly_br_diff_col, median_df$weekly_br_diff_col,
                             ninetieth_percentile_df$weekly_br_diff_col)
colnames(temp_results_summary) = c("plot_id", "weekly_a_mean", "weekly_gm_mean",
                              "weekly_median", "weekly_ninetieth_perc")
results_summary = results_summary %>% full_join(temp_results_summary, by = "plot_id")

#### end ####


#### biweekly_br_diff ####
biweekly_br_diff = dbSendQuery(mydb, "select * from biweekly_br_diff")
biweekly_br_diff = fetch(biweekly_br_diff, n=-1)

biweekly_br_diff = biweekly_br_diff[-which(!(
  biweekly_br_diff$plot_id %in% metadata$plot_id)
),]

biweekly_br_diff$climate_classification = NA
for (i in unique(biweekly_br_diff$plot_id)) {
  biweekly_br_diff$climate_classification[which(biweekly_br_diff$plot_id == i)] = 
    metadata$climate_classification[which(metadata$plot_id == i)]
}
biweekly_br_diff$depth = NA
for (i in unique(biweekly_br_diff$plot_id)) {
  biweekly_br_diff$depth[which(biweekly_br_diff$plot_id == i)] = 
    metadata$depth_in_m[which(metadata$plot_id == i)]
}

#reorder plot id by order in metadata 
biweekly_br_diff = biweekly_br_diff[order(match(biweekly_br_diff$plot_id, metadata$plot_id)),]
#end

#group plot ids extending multiple time periods
biweekly_br_diff <- biweekly_br_diff %>% 
  mutate(plot_id = gsub( " .*$", "", biweekly_br_diff$plot_id))
#

#summarize data for each plot id
a_mean_df <- aggregate(biweekly_br_diff_col ~ plot_id , 
                       biweekly_br_diff, function(i) mean(i))
gm_mean_df <- aggregate(biweekly_br_diff_col ~ plot_id , 
                        biweekly_br_diff, function(i) gm_mean(i))
median_df <- aggregate(biweekly_br_diff_col ~ plot_id , 
                       biweekly_br_diff, function(i) median(i))
ninetieth_percentile_df <- aggregate(biweekly_br_diff_col ~ plot_id , 
                                     biweekly_br_diff, function(i) quantile(i, 0.9))
temp_results_summary = data.frame(a_mean_df$plot_id, a_mean_df$biweekly_br_diff_col,
                             gm_mean_df$biweekly_br_diff_col, median_df$biweekly_br_diff_col,
                             ninetieth_percentile_df$biweekly_br_diff_col)
colnames(temp_results_summary) = c("plot_id", "biweekly_a_mean", "biweekly_gm_mean",
                              "biweekly_median", "biweekly_ninetieth_perc")
results_summary = results_summary %>% full_join(temp_results_summary, by = "plot_id")
#

#### monthly_br_diff ####
monthly_br_diff = dbSendQuery(mydb, "select * from monthly_br_diff")
monthly_br_diff = fetch(monthly_br_diff, n=-1)

monthly_br_diff = monthly_br_diff[-which(!(
  monthly_br_diff$plot_id %in% metadata$plot_id)
),]

monthly_br_diff$climate_classification = NA
for (i in unique(monthly_br_diff$plot_id)) {
  monthly_br_diff$climate_classification[which(monthly_br_diff$plot_id == i)] = 
    metadata$climate_classification[which(metadata$plot_id == i)]
}
monthly_br_diff$depth = NA
for (i in unique(monthly_br_diff$plot_id)) {
  monthly_br_diff$depth[which(monthly_br_diff$plot_id == i)] = 
    metadata$depth_in_m[which(metadata$plot_id == i)]
}

#reorder plot id by order in metadata 
monthly_br_diff = monthly_br_diff[order(match(monthly_br_diff$plot_id, metadata$plot_id)),]
#end

#group plot ids extending multiple time periods
monthly_br_diff <- monthly_br_diff %>% 
  mutate(plot_id = gsub( " .*$", "", monthly_br_diff$plot_id))
#

#summarize data for each plot id
a_mean_df <- aggregate(monthly_br_diff_col ~ plot_id , 
                       monthly_br_diff, function(i) mean(i))
gm_mean_df <- aggregate(monthly_br_diff_col ~ plot_id , 
                        monthly_br_diff, function(i) gm_mean(i))
median_df <- aggregate(monthly_br_diff_col ~ plot_id , 
                       monthly_br_diff, function(i) median(i))
ninetieth_percentile_df <- aggregate(monthly_br_diff_col ~ plot_id , 
                                     monthly_br_diff, function(i) quantile(i, 0.9))
temp_results_summary = data.frame(a_mean_df$plot_id, a_mean_df$monthly_br_diff_col,
                             gm_mean_df$monthly_br_diff_col, median_df$monthly_br_diff_col,
                             ninetieth_percentile_df$monthly_br_diff_col)
colnames(temp_results_summary) = c("plot_id", "monthly_a_mean", "monthly_gm_mean",
                              "monthly_median", "monthly_ninetieth_perc")
results_summary = results_summary %>% full_join(temp_results_summary, by = "plot_id")
#

#### annual_br_diff ####
annual_br_diff = dbSendQuery(mydb, "select * from annual_br_diff")
annual_br_diff = fetch(annual_br_diff, n=-1)

annual_br_diff = annual_br_diff[-which(!(
  annual_br_diff$plot_id %in% metadata$plot_id)
),]

annual_br_diff$climate_classification = NA
for (i in unique(annual_br_diff$plot_id)) {
  annual_br_diff$climate_classification[which(annual_br_diff$plot_id == i)] = 
    metadata$climate_classification[which(metadata$plot_id == i)]
}
annual_br_diff$depth = NA
for (i in unique(annual_br_diff$plot_id)) {
  annual_br_diff$depth[which(annual_br_diff$plot_id == i)] = 
    metadata$depth_in_m[which(metadata$plot_id == i)]
}

#reorder plot id by order in metadata 
annual_br_diff = annual_br_diff[order(match(annual_br_diff$plot_id, metadata$plot_id)),]
#end

#group plot ids extending multiple time periods
annual_br_diff <- annual_br_diff %>% 
  mutate(plot_id = gsub( " .*$", "", annual_br_diff$plot_id))
#

#summarize data for each plot id
a_mean_df <- aggregate(annual_br_diff_col ~ plot_id , 
                       annual_br_diff, function(i) mean(i))
gm_mean_df <- aggregate(annual_br_diff_col ~ plot_id , 
                        annual_br_diff, function(i) gm_mean(i))
median_df <- aggregate(annual_br_diff_col ~ plot_id , 
                       annual_br_diff, function(i) median(i))
ninetieth_percentile_df <- aggregate(annual_br_diff_col ~ plot_id , 
                                     annual_br_diff, function(i) quantile(i, 0.9))
temp_results_summary = data.frame(a_mean_df$plot_id, a_mean_df$annual_br_diff_col,
                             gm_mean_df$annual_br_diff_col, median_df$annual_br_diff_col,
                             ninetieth_percentile_df$annual_br_diff_col)
colnames(temp_results_summary) = c("plot_id", "annual_a_mean", "annual_gm_mean",
                              "annual_median", "annual_ninetieth_perc")
results_summary = results_summary %>% full_join(temp_results_summary, by = "plot_id")
#

list.files()
metadata$plot_id = gsub( " .*$", "", metadata$plot_id)
for (i in 1:length(results_summary$plot_id)) {
  plot_ids_of_interest = metadata[which(metadata$plot_id == results_summary$plot_id[i]),]
  results_summary$start_date[i] = plot_ids_of_interest$start_date[1]
  results_summary$end_date[i] = plot_ids_of_interest$end_date[length(plot_ids_of_interest$plot_id)]
  results_summary$temp_degC_percentage_missing_values[i] = 
    round(sum(plot_ids_of_interest$temp_degC_percentage_missing_values/100*plot_ids_of_interest$no_of_data))/
    sum(plot_ids_of_interest$no_of_data)*100
  results_summary$no_of_data[i] = sum(plot_ids_of_interest$no_of_data)
}
colnames(results_summary)[13] = "net_no_of_data"
colnames(results_summary)[9] = "climate_classification"
results_summary$climate_classification = results_summary$net_no_of_data
results_summary$net_no_of_data = round((100-results_summary$temp_degC_percentage_missing_values)/100*
                                    results_summary$no_of_data)
results_summary = results_summary[,-c(7,8)]
write.csv(results_summary, "metadata_with_variables - br diff e0433 - 8th Dec.csv")

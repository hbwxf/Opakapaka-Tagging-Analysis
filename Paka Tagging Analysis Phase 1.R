#### TODO: 
####  plotting index as part of analysis (unfilled circles for uncertain detections etc...)
####  create gifs??? (use jitter for residency)
####  plot receivers detected by # of detections
####  % of days detected / days since tagging
####  Total days detected /days at liberty = fidelity index
####  Track length vs. Size? Good tracks vs. size
####  Residence behavior vs. Size?
#### Fix get_graph() function

#### Bottomfish Movement Analysis
#### Written: January 2016 by Stephen R. Scherrer

#### Code for the analysis of Acoustic Tags and VR2W 
#### Data applied to Opakapaka tagged in Oahu, Hawaii

##### Clearning Workspace and setting directories ------------------------------------
rm(list=ls()) # Clear workspace
setwd('/Users/stephenscherrer/Documents/Work/UH/Projects/dissertation work/Opakapaka Tagging/Opakapaka Tagging Analysis/')
output_directory = ('/Users/stephenscherrer/Documents/Work/UH/Projects/dissertation work/Opakapaka Tagging/Opakapaka Tagging Analysis/Output')
savehistory(file="/Output/command_history")
 # load("/Users/stephenscherrer/Documents/Work/UH/Projects/dissertation work/Opakapaka Tagging/Opakapaka Tagging Analysis/Output/workspace_image")
##### Importing principle dependencies----------------------------------------------
#   install.packages('wesanderson') # color palett for plotting
#   install.packages('matlab')
#   install.packages('maps')
#   install.packages('mapdata')
#   install.packages('maptools')
#   install.packages('scales')
#   install.packages('ggmap')
#   install.packages('doParallel') # foreach(), detectCores(), registerDoParallel()
#   install.packages('marmap')
#   install.packages('lubridate') # floor_date()
#   install.packages('beepr') # beep()
#   install.packages('dplyr') # filter()
#   install.packages('wesanderson') # wes_palette
#   install.packages('useful') # compare.list()
#   install.packages('RAtmosphere')
#   install.packages('geosphere') # areaPolygon()
#   install.packages('igraph') # graph.adjacency()
# library("matlab", lib.loc) # ceil()
# library('maps')
# library('mapdata')
#library('maptools')  # for shapefiles
#library('scales')  # for transparency
#library('ggmap')
#source('/Users/stephenscherrer/Documents/Programming/R/utility_functions.R')
#library('reshape') # merge_all
library('marmap') # bathymetry()
# source('/Users/stephenscherrer/Documents/Work/UH/Projects/dissertation work/Spacial Ecology/Bottomfish Analysis Feb 2015/R Code/bf_analysis_functions.R')
# library('plotly')
library('lubridate') # floor_date(), ceil_date()
library('doParallel') # foreach()
library('beepr') # beep
library('dplyr') # filter()
library('wesanderson') #wes_palette
library('useful') # compare.list()
library('animation') # NOTE: creating animations requires installation of ImageMagick. See http://www.imagemagick.org for details
library('RAtmosphere')
library('geosphere') # areaPolygon()
library('igraph') # graph.adjacency()

##### Functions --------------------------------------------

#### Utility functions ----------

### Fixes station names, latitude, and longitude, for vue datafile
clean_vue_data = function(vue_data, receiver_data){
  for(i in 1:length(receiver_data$station_name)){
    vue_data$station[which(vue_data$receiver == receiver_data$vr2w_serial[i] & 
                             vue_data$datetime >= receiver_data$deployment_date[i] &
                             vue_data$datetime <= receiver_data$recovery_date[i])] = 
      as.character(receiver_data$station_name[i])
    vue_data$lat[which(vue_data$receiver == receiver_data$vr2w_serial[i] & 
                         vue_data$datetime >= receiver_data$deployment_date[i] &
                         vue_data$datetime <= receiver_data$recovery_date[i])] = 
      as.numeric(receiver_data$lat[i])
    vue_data$lon[which(vue_data$receiver == receiver_data$vr2w_serial[i] & 
                         vue_data$datetime >= receiver_data$deployment_date[i] &
                         vue_data$datetime <= receiver_data$recovery_date[i])] = 
      as.numeric(receiver_data$lon[i])
  }
  vue_data$lat = as.numeric(vue_data$lat)
  vue_data$lon = as.numeric(vue_data$lon)
  return(vue_data)
}

subset_time = function(vue_data, start = min(vue_data$datetime), end = max(vue_data$datetime)){
  new_vue_df = vue_data[which(vue_data$datetime >= as.POSIXct(start) & vue_data$datetime < as.POSIXct(end)), ]
  return (new_vue_df)
}

create_save_directory = function(new_directory){
  if(new_directory != FALSE){
#    main_directory = getwd() # stores old working directory. 
    if(new_directory == TRUE){
      dir.name = paste(getwd(), "/", Sys.time(), sep = "") # if Output Results is TRUE, creates a new directory for storing output files with system time
    }else{
      dir.name = paste(getwd(), "/", new_directory, sep = "") # if Output Results is not FALSE but not TRUE, creates a new directory for storing output files with contents of output_results argument
    }
    dir.create(dir.name) # creates new output directory
    # setwd(dir.name) # changes to newly created output directory
  }
  return(dir.name) # for setting working directory with
}

load_zissou_palette = function(){
  ### Plotting Colors -
  zissou = list()
  zissou$teal   = wes_palette("Zissou", 5)[1]
  zissou$blue   = wes_palette("Zissou", 5)[2]
  zissou$gold   = wes_palette("Zissou", 5)[3]
  zissou$yellow = wes_palette("Zissou", 5)[4]
  zissou$red    = wes_palette("Zissou", 5)[5]
  return(zissou)
}

load_vemco = function(filename, filepath = FALSE, format = '%Y-%m-%d %H:%M:%S'){
  proj_dir = getwd()
  if (isTRUE(filepath != FALSE)) {setwd(filepath)}
  vue_data_raw = read.csv(filename)
  vue_data_cleaned = vue_col_names(vue_data_raw)
  vue_data_cleaned$datetime = strptime(vue_data_cleaned$datetime, 
                                       format = format,
                                       tz = "GMT")
  vue_data_cleaned$datetime = convert_tz(vue_data_cleaned$datetime, new.tz = 'HST')
  vue_data_cleaned$tag_id = clean_tag_id(vue_data_cleaned$tag_id)
  vue_data_cleaned$receiver = clean_receiver(vue_data_cleaned$receiver)
  
  setwd(proj_dir)
  return (vue_data_cleaned)
}

convert_tz = function(datetime, new.tz = 'HST'){
  #Function to convert GMT/UTC times to HST time
  datetime.new.tz = strptime(datetime, format = '%Y-%m-%d %H:%M:%S', tz = new.tz)
  dateoffset = datetime-datetime.new.tz
  datetime.new.tz = datetime.new.tz + dateoffset
  return(datetime.new.tz)
}

vue_col_names = function(vue_data_raw){
  colnames(vue_data_raw)[1]  <- 'datetime'
  colnames(vue_data_raw)[2]  <- 'receiver'
  colnames(vue_data_raw)[3]  <- 'tag_id'
  colnames(vue_data_raw)[4]  <- 'name'
  colnames(vue_data_raw)[5]  <- 'tag_serial'
  colnames(vue_data_raw)[6]  <- 'sensor_value'
  colnames(vue_data_raw)[7]  <- 'sensor.unit'
  colnames(vue_data_raw)[8]  <- 'station'
  colnames(vue_data_raw)[9]  <- 'lat'
  colnames(vue_data_raw)[10] <- 'lon'
  return (vue_data_raw)
}

clean_tag_id = function(tag_id){
  ## Returns tag ID number as a factor, removing the 'A69-####-' prefix
  cleaned_id = as.factor(substring(tag_id, 10, ))
  return (cleaned_id)
}

clean_receiver = function(receiver){
  ## Returns receiver number as a factor, remvoing the 'VR2W-' Prefix
  cleaned_receiver = as.factor(substring(receiver, 6))
  return (cleaned_receiver)
}

load_receiver_data = function(filename, filepath = FALSE){
  proj_dir = getwd()
  if (filepath != FALSE){
    setwd(filepath)
  }
  receiver_dates = receiver_col_names(read.csv(filename))
  receiver_dates$deployment_date = strptime(receiver_dates$deployment_date, format = '%m/%d/%y %H:%M', tz = 'HST')
  receiver_dates$recovery_date = strptime(receiver_dates$recovery_date, format = '%m/%d/%y %H:%M', tz = 'HST')
  receiver_dates$lat = convert_lat_lon(receiver_dates$lat_deg, receiver_dates$lat_min)
  receiver_dates$lon = convert_lat_lon(receiver_dates$lon_deg, receiver_dates$lon_min)
  setwd(proj_dir)
  return (receiver_dates)
}

receiver_col_names = function(receiver_file_raw){
  receiver_file = receiver_file_raw
  colnames(receiver_file)[1] = 'serviced'
  colnames(receiver_file)[2] = 'station_name'
  colnames(receiver_file)[3] = 'consecutive_deployment_number'
  colnames(receiver_file)[4] = 'deployment_date'
  colnames(receiver_file)[5] = 'recovery_date'
  colnames(receiver_file)[6] = 'recovered'
  colnames(receiver_file)[7] = 'in_data_set'
  colnames(receiver_file)[8] = 'lat_deg'
  colnames(receiver_file)[9] = 'lat_min'
  colnames(receiver_file)[10] = 'lon_deg'
  colnames(receiver_file)[11] = 'lon_min'
  colnames(receiver_file)[12] = 'depth'
  colnames(receiver_file)[13] = 'vr2w_serial'
  colnames(receiver_file)[14] = 'acoustic_release_serial'
  colnames(receiver_file)[15] = 'acoustic_release_battery_life'
  colnames(receiver_file)[16] = 'acoustic_release_voltage_at_deployment'
  colnames(receiver_file)[17] = 'acoustic_release_serial_code'
  colnames(receiver_file)[18] = 'temperature_logger_serial'
  colnames(receiver_file)[19] = 'location_code'
  colnames(receiver_file)[20] = 'deployed_by'
  colnames(receiver_file)[21] = 'recovered_by'
  colnames(receiver_file)[22] = 'comments_deployment'
  colnames(receiver_file)[23] = 'comments_recovery'
  return (receiver_file)
}

load_tagging_data = function(filename, filepath = FALSE){
  proj_dir = getwd()
  if (isTRUE(filepath != FALSE)){
    setwd(filepath)
  }
  tagging_data_raw = read.csv(filename)
  tagging_meta_data = meta_data_col_names(tagging_data_raw)
  tagging_meta_data$datetime = strptime(tagging_meta_data$datetime, format = '%m/%d/%y %H:%M', tz = 'HST')
  tagging_meta_data$vem_tag_id = as.factor(tagging_meta_data$vem_tag_id)
  tagging_meta_data$lat = convert_lat_lon(tagging_meta_data$lat_deg, tagging_meta_data$lat_min)
  tagging_meta_data$lon = convert_lat_lon(tagging_meta_data$lon_deg, tagging_meta_data$lon_min)
  setwd(proj_dir)
  return (tagging_meta_data)
}

meta_data_col_names = function(data_frame){
  colnames(data_frame)[1]  <- 'unique_id'
  colnames(data_frame)[2]  <- 'datetime'
  colnames(data_frame)[3]  <- 'species'
  colnames(data_frame)[4]  <- 'conventional_tag_id'
  colnames(data_frame)[5]  <- 'vem_tag_type'
  colnames(data_frame)[6]  <- 'vem_tag_serial'
  colnames(data_frame)[7]  <- 'vem_tag_id'
  colnames(data_frame)[8]  <- 'fork_length(cm)'
  colnames(data_frame)[9]  <- 'precaudal_length(cm)'
  colnames(data_frame)[10] <- 'cohort'
  colnames(data_frame)[11] <- 'area_of_capture'
  colnames(data_frame)[12] <- 'depth_of_capture'
  colnames(data_frame)[13] <- 'lat_deg'
  colnames(data_frame)[14] <- 'lat_min'
  colnames(data_frame)[15] <- 'lon_deg'
  colnames(data_frame)[16] <- 'lon_min'
  colnames(data_frame)[17] <- 'lat'
  colnames(data_frame)[18] <- 'lon'
  colnames(data_frame)[19] <- 'stomach_everted'
  colnames(data_frame)[20] <- 'eyes_popped'
  colnames(data_frame)[21] <- 'bladder_vented'
  colnames(data_frame)[22] <- 'point_of_incision'
  colnames(data_frame)[23] <- 'dna_clip'
  colnames(data_frame)[24] <- 'cannulation'
  colnames(data_frame)[25] <- 'sex'
  colnames(data_frame)[26] <- 'video'
  colnames(data_frame)[27] <- 'photo'
  colnames(data_frame)[28] <- 'photo_name'
  colnames(data_frame)[29] <- 'audio_log_file'
  colnames(data_frame)[30] <- 'dropshot'
  colnames(data_frame)[31] <- 'tissue_sample'
  colnames(data_frame)[32] <- 'gut_sample'
  colnames(data_frame)[33] <- 'tagger'
  colnames(data_frame)[34] <- 'notes'
  colnames(data_frame)[35] <- 'recaptured'
  colnames(data_frame)[36] <- 'detections'
  colnames(data_frame)[37] <- 'comments'
  return(data_frame)
}

clean_vue_lat_lon = function(vue_data_df, receiver_data_df){
  station = rep(NA, times = length(vue_data_df$datetime))
  for (i in 1:length(receiver_data_df$station_name)){
    receiver_subset_index = which(vue_data_df$receiver == receiver_data_df$vr2w_serial[i])
    deploy_subset_index = which(vue_data_df$datetime >= receiver_data_df$deployment_date[i])
    recover_subset_index = which(vue_data_df$datetime < na.omit(receiver_data_df$recovery_date[i]))
    ind = Reduce(intersect, list(receiver_subset_index, deploy_subset_index, recover_subset_index))
    vue_data_df$lat[ind] = receiver_data_df$lat[i]
    vue_data_df$lon[ind] = receiver_data_df$lon[i]
    station[ind] = as.character(receiver_data_df$station_name[i])
  }
  vue_data_df$station = as.factor(station)
  return(vue_data_df)
}

### remove_location -> Removing station location from vue dataframe
remove_location = function(vue_data, location_to_remove = FALSE){
  if (location_to_remove == FALSE){
    return (vue_data)
  }else{
    keep_data = vue_data[vue_data$station != location_to_remove, ]
    return (keep_data)}
}

clean_vue = function(vue_data, tag_ids = FALSE, exclude = FALSE){
  ##Function for removing all tags from a vue DB not explicitly kept by the tag_ids input.
  if (tag_ids[1] == FALSE){
    keep_data = vue_data
  }else{
    if (exclude == FALSE){
      keep_data = vue_data[vue_data$tag_id %in% tag_ids, ]
    } else if (exclude == TRUE){
      keep_data = vue_data[!(vue_data$tag_id %in% tag_ids), ]
    }
    keep_data$tag_id = as.factor(as.character(keep_data$tag_id))
  }
  return(keep_data)
}

clean_tags = function(tag_ids, vue_data){
  ## function to remove tags from tag id list that are not in vue database and report which tags those are
  keep_tags = tag_ids[tag_ids %in% vue_data$tag_id]
  print (paste('The Following Tags Were Not In the VUE Data:', as.character(tag_ids[!(tag_ids %in% vue_data$tag_id)]),sep = ' '))
  return(keep_tags)
}

convert_lat_lon = function(ll_deg, ll_min = FALSE){
  ## Converts latitude and longitude between ll minutes and ll decimal degrees
  # 2 usages:
  # Convert decimal degrees to degree minutes
  # 1 argument
  # ll_pref is a single argument of latitude or longitude in decimal degrees
  # Returns a prefix and decimal for that argument
  # Convert degree minutes to decimal degrees
  # 2 arguments
  # ll_pref is the latitude or longitude's degree
  # ll_min is the degree minutes
  # returns a single float of ll in decimal degrees
  if (ll_min[1] == FALSE){ #then we are going from one number to two
    ll_deg = as.numeric(as.character(ll_deg))
    ll_bin = matrix(0, length(ll_deg), 2)
    for (r in 1:length(ll_deg)){
      if (isTRUE(ll_deg[r] >= 0)){
        ll_dec = ll_deg[r] - floor(ll_deg[r])
        ll_bin[r, ] = c(floor(ll_deg[r]), (ll_dec)*60)
      } else {
        ll_dec = (ll_deg[r] - ceiling(ll_deg[r]))*-1
        ll_bin[r, ] = c(ceiling(ll_deg[r]), (ll_dec)*60)
      }
    }
  }else{ #if we are converting from two numbers to one
    ll_deg = as.numeric(as.character(ll_deg))
    ll_min = as.numeric(as.character(ll_min))
    ll_bin = matrix(0, length(ll_deg), 1)
    for (r in 1:length(ll_deg)){
      ll_dec_deg = abs(ll_deg[r]) + (abs(ll_min[r])/60)
      if (isTRUE(ll_deg[r] < 0)){
        ll_dec_deg = ll_dec_deg*(-1)
      }
      ll_bin[r] = ll_dec_deg
    }
  }
  return (ll_bin)
}

get_recovery_rate = function(vue_data, 
                             start_date, 
                             end_date, 
                             tag_id, 
                             ping_interval){
  ## Function to determine recovery rate from a vue dataset
  vd_time = vue_data[vue_data$study_date > start_date & 
                       vue_data$study_date < end_date, ]
  vd_tag  = vd_time[as.numeric(as.character(vd_time$tag_id)) == tag_id, ]
  # subtract extra 1 because not including start or end dates. 
  # See Critereons for Analysis
  elapsed_days = (end_date - start_date) - 1 
  theoretical_pings_sent = (elapsed_days * 24 * 60 * 60) / ping_interval
  ind_recovery_rate = length(vd_tag$study_date) / theoretical_pings_sent
  recovered = c(ind_recovery_rate, theoretical_pings_sent, length(vd_tag$study_date))
  names(recovered) = c('recovery_rate', 'pings_sent', 'pings_recovered')
  return(recovered)
}

remove_detections_before_tagging = function(vue_data, tagging_data, ncores = 8){
  ## function to remove all tags not in tagging data as well as removing all detections of tags
  ## occurring before the tag is deployed. 
  ## This might occur when a tag was previously deployed for range testing purposes.
  tagging_data$vem_tag_id = as.numeric(as.character(tagging_data$vem_tag_id))
  vue_data$tag_id = as.numeric(as.character(vue_data$tag_id))
  vue_data.filtered = matrix()
  registerDoParallel(cores = ncores)
  vue_data.filtered = foreach(i = 1:length(which(is.na(tagging_data$vem_tag_id) == FALSE)), .combine = rbind) %dopar%{
    return(filter(vue_data, tag_id == tagging_data$vem_tag_id[is.na(tagging_data$vem_tag_id) == FALSE][i], datetime >= tagging_data$datetime[is.na(tagging_data$vem_tag_id) == FALSE][i]))
  }
  return(vue_data.filtered)
}

#### Usage Functions -----------------


get_graph = function(vue_df, 
                     receiver_data,
                     tag_id = FALSE, 
                     time_period = FALSE,
                     igraph=TRUE,
                     binary=FALSE,
                     removeLoops=FALSE,
                     start=FALSE,
                     end=FALSE,
                     numstations = FALSE,
                     remove_zeros = TRUE){
  ### A function to create an adjacency matrix from a vue dataset
  ### Arguments: 
  ### vue_df = a dataframe containing a vue export with 
  ### columns for datetime, station, and tag_id. Imported
  ### with load_vemco function from 'R_Utility_Functions.R' 
  ### script
  ### tag_id = a tag id (or multiple ids) corrosponding to the
  ### unique transmitter number for a vemco tag (or tags)
  ### time_period = a list containing two POSIXct objects 
  ### corrosponding to the begining of the time for analysis
  ### and the up to but not including the end time for analysis
  
  ### TO DO: Add functionality to determine which stations had 
  ### receivers during a given time_period. Replace vue_df$station
  ### with these stations. 
  if (tag_id == FALSE){ # if no tag ids specified, all tag ids used
    tag_id = unique(vue_df$tag_id)
  }
  if (time_period[1] == FALSE){ # if no dates specified, all dates used
    time_period = c(min(vue_df$datetime), max(vue_df$datetime))
  }else {
    if (start == FALSE) {
      start = min(vue_df$datetime)
    }
    if (end == FALSE) {
      end = max(vue_Df$datetime)
    }
  }
  vue_df = vue_df[which(vue_df$datetime >= start & vue_df$datetime <= end), ]
  numstations = length(unique(receiver_data$station_name))
  
  ## Assigning a station number
  vue_df$station_number = 0
  for(i in 1:length(unique(receiver_data$station_name))){
    vue_df$station_number[vue_df$station == as.character(unique(receiver_data$station_name))[i]] = i
  }
  
  # Pulling out detections for a specific tag
  vue_df = clean_vue(vue_data, tag_id, exclude = FALSE)
  # Order vue_df by tag id
  vue_df = vue_df[order(vue_df$tag_id, vue_df$datetime), ]
  
  # To Do: pull out receivers present during a specific time period
  
  # Build adjacency matrix, with receivers as nodes and fish 
  # movements as edges
  # Rows indicate movement from a receiver
  # Columns indicate movement to a receiver
  adj_matrix = matrix(0, numstations,numstations)
  # If station changes, increase adjacency matrix value by one

  for (i in 2:length(vue_df$station_number)){
    if(vue_df$tag_id[i] == vue_df$tag_id[i-1]){
      prevLoc = as.numeric(vue_df$station_number[i-1])
      newLoc = as.numeric(vue_df$station_number[i])
      if(binary) {
        adj_matrix[prevLoc, newLoc] = 1
      } else {
        adj_matrix[prevLoc, newLoc] =  adj_matrix[prevLoc, newLoc] + 1
      }
    }
  }
  
  if (removeLoops) {
    for (i in 1:numstations) {
      adj_matrix[i,i]=0
    }
  }
  #station_map = station_ids_map(vue_df)
  colnames(adj_matrix) = unique(receiver_data$station_name)
  rownames(adj_matrix) = unique(receiver_data$station_name)
  #colnames(adj_matrix) = station_map$'Station Name'
  #rownames(adj_matrix) = station_map$'Station Name'

if(remove_zeros == TRUE){
    adj_matrix = adj_matrix[which(rowSums(adj_matrix) != 0), ]
    adj_matrix = adj_matrix[ ,which(colSums(adj_matrix) != 0)]
  }

  if(igraph) {
    # Convert adjacency matrix into a graph object
    vemco_graph = graph.adjacency(adj_matrix, mode = 'directed', 
                                  weighted = TRUE)
    return (vemco_graph)
  }
  # Return graph object
  return(adj_matrix)   
} 


### Building a matrix of tags by receiver where numbers corrospond to the number 
### of detections for each tag at a given station
build_detection_matrix = function(vue_df){
  ## Outputs a matrix where each row corrosponds to a unique tag in the vue database 
  ## and each column corrosponds to a unique receiver. Values of each index are the number
  ## of detections for each tag at a particular location.
  detection_matrix = matrix(nrow = length(unique(as.numeric(as.character(vue_df$tag_id)))), 
                            ncol = length(unique(vue_df$station)))
  # dim(detection_matrix)
  for(i in 1:dim(detection_matrix)[1]){
    for(j in 1:dim(detection_matrix)[2]){
      detection_matrix[i,j] = dim(vue_df[which(vue_df$tag_id == unique(vue_df$tag_id)[i] & 
                                                 vue_df$station == unique(vue_df$station)[j]), ])[1]
    }
  }
  rownames(detection_matrix) = unique(as.character(vue_df$tag_id))
  colnames(detection_matrix) = unique(as.character(vue_df$station))
  return(detection_matrix)
}

## Determining days at liberty for each tag
calculate_time_at_liberty = function(vue_df){
  time_at_liberty_matrix =  matrix(ncol = 1, nrow = length(unique(vue_df$tag_id)))
  for(i in 1:length(unique(vue_df$tag_id))){
    indv_data = vue_df[vue_df$tag_id == unique(vue_df$tag_id)[i], ]
    time_at_liberty_matrix[i] = round(difftime(indv_data$datetime[length(indv_data$datetime)], indv_data$datetime[1], units = "days"), digits = 2)
  }
  return(time_at_liberty_matrix)
}

## Determining days between tagging and first apearance on array
calculate_days_before_detection = function(vue_df){
  days_before_detection =  c()
  for(i in 1:length(unique(vue_df$tag_id))){
    indv_data = vue_df[vue_df$tag_id == unique(vue_df$tag_id)[i], ]
    days_before_detection = c(days_before_detection, round(difftime(time1 = min(indv_data$datetime[which(indv_data$station != 'Tagging Location')]), time2 = indv_data$datetime[which(indv_data$station == 'Tagging Location')], units = "days"), digits = 2))
  }
  return(days_before_detection)
}

## Determing unique days detected
calculate_days_detected = function(vue_df){
  days_detected = matrix(ncol = 1, nrow = length(unique(vue_df$tag_id)))
  for(i in 1:length(unique(vue_df$tag_id))){
    indv_data = vue_df[vue_df$tag_id == unique(vue_df$tag_id)[i], ]
    days_detected[i] = length(unique(floor_date(indv_data$datetime, unit = 'day')))
  }
  return(days_detected)
}

#### Appending position and date record for each tagging
generate_tagging_detection = function(tagging_data, vue_data){
  ### generate_tagging_detection -> for each tag in dataset
  ###   adds a detection corrosponding to the tagging location data
  tagging_data$vem_tag_id = as.numeric(as.character(tagging_data$vem_tag_id))
  vue_data$tag_id = as.numeric(as.character(vue_data$tag_id))
  tag_ids = na.exclude(tagging_data$vem_tag_id[tagging_data$vem_tag_id %in% vue_data$tag_id])
  for (i in 1:length(tag_ids)){
    false_record     = vue_data[1, ]
    false_record[1]  = as.POSIXct(as.character(tagging_data$datetime[which(tagging_data$vem_tag_id == tag_ids[i])]))
    false_record[2]  = 0
    false_record[3]  = tag_ids[i]
    false_record[4]  = NA
    false_record[5]  = NA
    false_record[6]  = NA
    false_record[7]  = 1
    false_record[8]  = 'Tagging Location'
    false_record[9]  = convert_lat_lon(tagging_data$lat_deg[which(tagging_data$vem_tag_id == tag_ids[i])], tagging_data$lat_min[which(tagging_data$vem_tag_id == tag_ids[i])])
    false_record[10] = convert_lat_lon(tagging_data$lon_deg[which(tagging_data$vem_tag_id == tag_ids[i])], tagging_data$lon_min[which(tagging_data$vem_tag_id == tag_ids[i])])
    vue_data = rbind(false_record, vue_data)
  }
  # Ordering data by date, then tag, then receiver
  vue_data = vue_data[order(vue_data$datetime, vue_data$tag_id, vue_data$receiver), ]
  return (vue_data)
}

#### Assigning Study Date
generate_study_date = function(vue_df, start_date = FALSE){
  if(start_date == FALSE){
    start_date = floor_date(min(vue_df$datetime), unit = "day")
  }
  vue_df$study_date = as.numeric(difftime(vue_df$datetime, start_date, units = 'days'))
  return(vue_df)
}

#### Listing all stations a tag was detected on
stations_detected = function(vue_data){
  detected_list = list()
  detected_list$unique = list()
  detected_list$all = list()
  for(i in 1:length(unique(vue_data$tag_id))){
    indv_data = vue_data[vue_data$tag_id == unique(vue_data$tag_id)[i], ]
    detected_list$unique[i] = list(unique(indv_data$station))
    detected_char_vec = c(indv_data$station[1])
    for(n in 2:length(indv_data$station)){
      if(indv_data$station[n-1] != indv_data$station[n]){
        detected_char_vec = c(detected_char_vec, indv_data$station[n])
      }
      detected_list$all[i] = list(detected_char_vec)
    }
  }
  return(detected_list)
}

#### Removing false detections
## based on Pincock 2012
remove_false_detections = function(vue_data, transmitter_interval = 60, remove_detections = TRUE){
  script_timer = proc.time()
  tf_detection_index = c()
  registerDoParallel(cores = 8)
  tf_detection_index = foreach(i = c(1:length(unique(vue_data$datetime))), .combine = c) %dopar%{
    return(any(which(vue_data$tag_id[i] == vue_data$tag_id     & 
                       vue_data$receiver[i] == vue_data$receiver &
                       (abs(difftime(time1 = rep(vue_data$datetime[i], length(vue_data$datetime)),  
                                     time2 = vue_data$datetime, units = "secs")) <= (30 * transmitter_interval)) == TRUE) != i))
  }
  print(proc.time() - script_timer) # Stop the clock
  if(remove_detections == TRUE){
    vue_data = vue_data[tf_detection_index == TRUE, ]
    return(vue_data)
  }else{
    return(tf_detection_index)
  }
  beep(2)
}

#### Creating a histogram of detections per day
tag_detection_histogram = function(vue_data, collate = FALSE, print = TRUE){
  if(collate == TRUE){
    if(print == TRUE){
      png('All Tags Daily Detections Histogram.png', height = 800, width = 1000)
    }
    par(mfrow = c(5, 5), mar=c(1,3,3,1), oma=c(0,0,2,0)) # setting up plotting grid
    hist(floor(vue_data$study_date), right = FALSE, breaks = 0:ceiling(max(vue_data$study_date)),
         xlab = 'Study Date', 
         ylab = 'Detections',
         main = 'All Tags')
    for(i in 1:length(unique(vue_data$tag_id))){
      indv_data = vue_data[vue_data$tag_id == unique(vue_data$tag_id)[i], ]
      hist(floor(indv_data$study_date), right = FALSE, breaks = 0:ceiling(max(vue_data$study_date)),
           main = unique(vue_data$tag_id)[i], # individual plot label
           xlab = 'Study Date', # x axis label
           ylab = 'Detections') # y axis label
      abline(v = indv_data$study_date[indv_data$station == 'Tagging Location'], col = 'red')
    }
    title("Daily Detections of Each Tag", outer=TRUE)
    dev.off()
  }else{
    if(print == TRUE){
      png('All Tags - Daily Histogram.png', height = 800, width = 1000)
    }
    par(mfrow = c(1, 1), mar=c(3,3,3,3), oma=c(2,2,2,2)) # setting up plotting grid
    hist(floor(vue_data$study_date), right = FALSE, breaks = 0:ceiling(max(vue_data$study_date)),
         xlab = 'Study Date', 
         ylab = 'Detections',
         main = 'All Tags')
    dev.off()
    for(i in 1:length(unique(vue_data$tag_id))){
      if(print == TRUE){
        png(paste(unique(vue_data$tag_id)[i], 'Daily Histogram.png'), height = 800, width = 1000)
      }
      par(mfrow = c(1, 1), mar=c(3,3,3,3), oma=c(2,2,2,2)) # setting up plotting grid
      indv_data = vue_data[vue_data$tag_id == unique(vue_data$tag_id)[i], ]
      hist(floor(indv_data$study_date), right = TRUE, breaks = 0:ceiling(max(vue_data$study_date)),
           main = unique(vue_data$tag_id)[i], # individual plot label
           xlab = 'Study Date', # x axis label
           ylab = 'Detections', # y axis label
           col = 'black') # fills in histogram bars
      abline(v = floor(indv_data$study_date[indv_data$station == 'Tagging Location'])-1, col = 'red')
      dev.off()
    }
  }
}

#### Plotting Map of Receivers in Study
plot_receiver_map = function(receiver_data, filename = FALSE, region = 'Makapuu', rec_col = FALSE){
  ### Plots map of receiver coordinates
  ## Use rec_col = 'station' to map stations to colors
  ## Available regions include: 'Makapuu" (default), and "Oahu"
  if(exists('filename') == TRUE){ 
    filename = paste(filename, '.png', sep = "")
    png(filename)}
  if(exists('bathymetry') == FALSE){
    if(region == 'Makapuu'){
      bathymetry = getNOAA.bathy(lon1 = -158, 
                                 lon2 = -157.5, 
                                 lat1 = 21.2, 
                                 lat2 = 21.6,
                                 resolution = 1)
    }else if(region == 'Oahu'){
      bathymetry = getNOAA.bathy(lon1 = -158.5, 
                                 lon2 = -157.5, 
                                 lat1 = 21.18, 
                                 lat2 = 21.82,
                                 resolution = 1)
    }else if(region == 'Oahu and Penguin Banks'){
      bathymetry = getNOAA.bathy(lon1 = -158.5, 
                                 lon2 = -157, 
                                 lat1 = 20.75, 
                                 lat2 = 21.82,
                                 resolution = 1)
    }
  }
  ## Importing Zissou Color palette
  zissou = load_zissou_palette()
  ## Plotting basemap
  plot.bathy(bathymetry, land = TRUE, image=TRUE, bpal = list(c(-400, -1, zissou$blue)), deepest.isobath = c(-10000), shallowest.isobath = c(0), step = c(100), drawlabels = TRUE)
  ## Adding scale legend
  scaleBathy(bathymetry, deg = .48, cex = .5)
  #scaleBathy(bathymetry, deg = .48, cex = .5)
  ## Adding receiver locations
  if(exists('rec_col') == FALSE){
    rec_col = 'red'
  }
  receiver_plot_colors = rec_col
  if(class(rec_col) == 'list'){
    color_palette = rec_col
    receiver_plot_colors = rep('black', length(receiver_data$station_name))
    for(i in 1:length(receiver_data$lat)){
      if(receiver_data$station_name[i] %in% color_palette$station){
        receiver_plot_colors[i] = color_palette$colors[which(color_palette$station == receiver_data$station_name[i])]
      }
    }
  }
  points(lat~lon, data = receiver_data, pch = 19, col = receiver_plot_colors,cex = 1)
  ## Adding BRFA boundaries
  brfa_e = as.data.frame(rbind(c(-157.68333333, 21.28333333), 
                               c(-157.53333333, 21.28333333), 
                               c(-157.53333333, 21.4166666), 
                               c(-157.68333333, 21.4166666)))
  colnames(brfa_e) = c('lon', 'lat')
  
  brfa_f = as.data.frame(rbind(c(-157.5666667, 20.9666667), 
                               c(-157.5666667, 21.0333333333),
                               c(-157.3666667, 21.0333333333),
                               c(-157.3666667, 20.9666667),
                               c(-157.5666667, 20.9666667)))
  colnames(brfa_f) = c('lon', 'lat')
  
  lines(lat~lon, data = brfa_e, pch = 19, col = 'purple', lwd = 3, cex = .6)
  lines(lat~lon, data = brfa_f, pch = 19, col = 'purple', cex = .6)
  dev.off()
  return(bathymetry)
}

plot_depths = function(vue_data, individual_tags = FALSE, print = TRUE){
  depth_data = vue_data[is.na(vue_data$sensor_value) == FALSE, ]
  if(dim(depth_data)[1]>0){
  if(individual_tags == TRUE){
    for(i in 1:length(unique(depth_data$tag_id))){
      indv_data = depth_data[depth_data$tag_id == unique(depth_data$tag_id)[i], ]
      if(print == TRUE){
        png(paste(unique(depth_data$tag_id)[i], 'Depth History.png', sep = ' '))
      }
      par(mfrow = c(2, 1), mar=c(3,3,3,3), oma=c(2,2,2,2)) # setting up plotting grid
      plot(indv_data$sensor_value ~ indv_data$study_date, type = 'l',
           ylim = c(300, 0),
           xlim = c(min(indv_data$study_date), max(indv_data$study_date)),
           xlab = 'Study Date',
           ylab = 'Depth (m)',
           main = 'Depth History Across Study')
      points(indv_data$sensor_value ~ indv_data$study_date, pch = 19)
      plot(indv_data$sensor_value ~ hour(indv_data$datetime),
           ylim = c(300, 0),
           xlim = c(0, 23),
           xlab = 'Hour',
           ylab = 'Depth (m)',
           main = 'Hourly Depth Distribution')
      title(paste(unique(depth_data$tag_id)[i], 'Depth History'), outer = TRUE)
      dev.off()
    } 
  }else if(individual_tags == FALSE){
    if(print == TRUE){
      png('All Tags Depth History.png')
    }
    par(mfrow = c(2, 1),  mar=c(3,3,3,3), oma=c(2,2,2,2)) # setting up plotting grid
    plot(depth_data$sensor_value ~ depth_data$study_date, type = 'l',
         ylim = c(300, 0),
         xlim = c(min(vue_data$study_date), max(vue_data$study_date)),
         xlab = 'Study Date',
         ylab = 'Depth (m)',
         main = 'Depth History Across Study')
    points(depth_data$sensor_value ~ depth_data$study_date, pch = 19)
    plot(depth_data$sensor_value ~ hour(depth_data$datetime),
         ylim = c(300, 0),
         xlim = c(0, 23),
         xlab = 'Hour',
         ylab = 'Depth (m)',
         main = 'Hourly Depth Distribution')
    title('All Tags Depth History', outer = TRUE)
    dev.off()
  }
}
}

#### Plotting Movement Maps for Each Fish
plot_movements = function(vue_data, receiver_data, region = 'Makapuu', tag_ids = FALSE, rec_col = FALSE, plot_title = FALSE){
  if (tag_ids[1] == FALSE){
    tag_ids = as.numeric(as.character(unique(vue_data$tag_id)))
  }
  for (id in tag_ids){
    indv_data = vue_data[vue_data$tag_id == id, ]
    
    ### Removing receivers not present while transmitter was active
    receiver_data_to_plot = receiver_data[which(receiver_data$deployment_date <= min(indv_data$datetime) &
                                                  (receiver_data$recovery_date >= min(indv_data$datetime) |
                                                     is.na(receiver_data$recovery_date) == TRUE)), ]
    receiver_data_to_plot$station_name = as.character(receiver_data_to_plot$station_name)
    
    if(plot_title == FALSE){
      plot_title = paste(id, 'Movement Map.png')
    }
    png(plot_title)
    
    if(exists('bathymetry') == FALSE){
      if(region == 'Makapuu'){
        bathymetry = getNOAA.bathy(lon1 = -157.8, 
                                   lon2 = -157.5, 
                                   lat1 = 21.2, 
                                   lat2 = 21.5,
                                   resolution = .75)
      }else if(region == 'Oahu'){
        bathymetry = getNOAA.bathy(lon1 = -158.5, 
                                   lon2 = -157.5, 
                                   lat1 = 21.18, 
                                   lat2 = 21.82,
                                   resolution = 1)
      }else if(region == 'Oahu and Penguin Banks'){
        bathymetry = getNOAA.bathy(lon1 = -158.5, 
                                   lon2 = -157, 
                                   lat1 = 20.75, 
                                   lat2 = 21.82,
                                   resolution = 1)
      }
    }
    
    ## Importing Zissou Color palette
    zissou = load_zissou_palette()
    ## Plotting basemap
    plot.bathy(bathymetry, land = TRUE, image=TRUE, bpal = gray.colors(10), deepest.isobath = c(-500), shallowest.isobath = c(0), step = c(100), drawlabels = TRUE)
    ## Adding scale legend
    scaleBathy(bathymetry, deg = .1, cex = .5)
    #scaleBathy(bathymetry, deg = .48, cex = .5)
    
    ## Adding receiver locations
    if(exists('rec_col') == FALSE){
      rec_col = 'red'
    }
    receiver_plot_colors = rec_col
    if(class(rec_col) == 'list'){
      color_palette = rec_col
      receiver_plot_colors = rep('black', length(receiver_data_to_plot$station_name))
      for(i in 1:length(receiver_data_to_plot$station_name)){
        if(receiver_data_to_plot$station_name[i] %in% color_palette$station){
          receiver_plot_colors[i] = color_palette$colors[which(color_palette$station == receiver_data_to_plot$station_name[i])]
        }
      }
    }
    ## Plotting fish movements
    lines(lat~lon, data = indv_data, col = 'blue', lty = 1, lwd = 2)
    # points(lat~lon, data = indv_data, col = 'blue',cex = 0.6, pch = 19)
    
    points(lat~lon, data = receiver_data_to_plot, pch = 19, col = receiver_plot_colors, cex = 1)
    
    ## Adding BRFA boundaries
    brfa_e = as.data.frame(rbind(c(-157.68333333, 21.28333333), 
                                 c(-157.53333333, 21.28333333), 
                                 c(-157.53333333, 21.4166666), 
                                 c(-157.68333333, 21.4166666)))
    colnames(brfa_e) = c('lon', 'lat')
    
    brfa_f = as.data.frame(rbind(c(-157.5666667, 20.9666667), 
                                 c(-157.5666667, 21.0333333333),
                                 c(-157.3666667, 21.0333333333),
                                 c(-157.3666667, 20.9666667),
                                 c(-157.5666667, 20.9666667)))
    colnames(brfa_f) = c('lon', 'lat')
    
    lines(lat~lon, data = brfa_e, pch = 19, col = 'purple', cex = .6)
    lines(lat~lon, data = brfa_f, pch = 19, col = 'purple', cex = .6)
    # dev.off()
  }
  return(bathymetry)
}

plot_tag_detections = function(vue_data, receiver_data, start_date = FALSE, end_date = FALSE, date_format = "%Y-%m-%d %H:%M:%S", plot_lost = FALSE, remove_station = FALSE, region = "Makapuu", tag_ids = FALSE, rec_col = station_palette, title = FALSE, plot_receivers = "all"){
  ### Types of plots (plot_receivers) to make include:
  # all = plots all receivers regardless of time
  # start = plots only receivers that were deployed at the begning of the time interval given by start_date
  # end = plots only receivers that were deployed at the end of the time interval given by end_date
  # study = plots all receivers that were deployed at some point during the time interval given by start_date and end_date
  # start tag = plots only receivers that were deployed at the time the animal was tagged
  # end tag = plots only receivers that were deployed at the last detection of a tag
  # study tag = plots all receivers that were deployed during the time the tag was out
  if(plot_lost == FALSE){
    receiver_data = receiver_data[receiver_data$recovered == "", ]
  } 
  receiver_data = receiver_data[is.na(receiver_data$deployment_date) == FALSE, ]
  if(start_date == FALSE){
    start_date = min(vue_data$datetime)
  }else{
    start_date = as.POSIXct(start_date, format = date_format)
  }
  if(end_date == FALSE){
    end_date = max(vue_data$datetime)
  }else{
    end_date = as.POSIXct(end_date, format = date_format)
  }
  for(i in 1:length(unique(vue_data$tag_id))){
    indv_data = vue_data[vue_data$tag_id == unique(vue_data$tag_id)[i], ]
    if(plot_receivers == "all"){
      plot_receiver_data = receiver_data
    }else if(plot_receivers == "start"){
      plot_receiver_data = receiver_data[which(receiver_data$deployment_date <= start_date & (receiver_data$recovery_date > start_date | is.na(receiver_data$recovery_date) == TRUE)), ]
    }else if(plot_receivers == "end"){
      plot_receiver_data = receiver_data[which(receiver_data$deployment_date < end_date & (receiver_data$recovery_date > end_date | is.na(receiver_data$recovery_date) == TRUE)), ]
    }else if(plot_receivers == "study"){
      plot_receiver_data = receiver_data[which(receiver_data$deployment_date < end_date & (receiver_data$recovery_date > start_date | is.na(receiver_data$recovery_date) == TRUE)), ]
    }else if(plot_receivers == "start tag"){
      plot_receiver_data = receiver_data[which(receiver_data$deployment_date <= min(indv_data$datetime) & (receiver_data$recovery_date > min(indv_data$datetime) | is.na(receiver_data$recovery_date) == TRUE)), ]
    }else if(plot_receivers == "end tag"){
      plot_receiver_data = receiver_data[which(receiver_data$deployment_date <= max(indv_data$datetime) & (receiver_data$recovery_date > max(indv_data$datetime) | is.na(receiver_data$recovery_date) == TRUE)), ]
    }else if(plot_receivers == "study tag"){
      plot_receiver_data = receiver_data[which(receiver_data$deployment_date < max(indv_data$datetime) & (receiver_data$recovery_date > min(indv_data$datetime) | is.na(receiver_data$recovery_date) == TRUE)), ]
    }
    plot_movements(vue_data = indv_data, receiver_data = plot_receiver_data, region = region, rec_col = rec_col)
  }
}

animate_movement_gif = function(vue_data, receiver_data, start_date = FALSE, end_date = FALSE, date_format = "%Y-%m-%d %H:%M:%S", plot_lost = FALSE, remove_station = FALSE, region = "Makapuu", tag_ids = FALSE, rec_col = 'red', plot_receivers = "all", bin_by = "day"){
  if(plot_lost == FALSE){
    receiver_data = receiver_data[receiver_data$recovered == "", ]
  } 
  receiver_data = receiver_data[is.na(receiver_data$deployment_date) == FALSE, ]
  if(start_date == FALSE){
    start_date = min(vue_data$datetime)
  }else{
    start_date = as.POSIXct(start_date, format = date_format)
  }
  if(end_date == FALSE){
    end_date = max(vue_data$datetime)
  }else{
    end_date = as.POSIXct(end_date, format = date_format)
  }
  .vue_data = vue_data[vue_data$datetime >= start_date & vue_data$datetime < end_date, ]
  tag_ids = unique(.vue_data$tag_id)
  day_seq = floor_date(seq.POSIXt(start_date, end_date, by = bin_by), unit = bin_by)
  for(r in 2:length(day_seq)){
    title = paste("gif_image ", r-1, ".png", sep = "")
    plot_vue = .vue_data[.vue_data$datetime >= day_seq[r-1] & .vue_data$datetime < day_seq[r], ]
    plot_tag_detections(plot_vue, receiver_data, start_date = day_seq[r-1], end_date = day_seq[r], plot_lost = plot_lost, remove_station = remove_station, region = region, tag_ids = FALSE, plot_receivers = "study", rec_col = rec_col, title = title)
  }
#### Some Code here to make GIFS
}

#### Calculating spatial evenness
spatial_evenness = function(vue_data, receiver_data){
  ### function to calculate spacitail eveness based on Pielou 1966 from TinHan 2014
  ## outputs a dataframe first column is tag id, second column is spatial evenness index
  vue_data = vue_data[vue_data$station != 'Tagging Location', ]
  spatial_evenness_df = as.data.frame(matrix(data = 0, nrow = length(unique(vue_data$tag_id)), ncol = 2))
  colnames(spatial_evenness_df) = c('tag_id', 'spatial_evenness_metric')
  spatial_evenness_df$tag_id = unique(vue_data$tag_id)
  R = length(unique(receiver_data$station_name)) #could replaces "station_name" with "zone"
  for(i in 1:length(unique(vue_data$tag_id))){
    indv_data = vue_data[which(vue_data$tag_id == unique(vue_data$tag_id)[i]), ]
    spatial_sum = c()
    for(a in 1:length(unique(receiver_data$station_name))){
      rho_i = length(indv_data$datetime[which(as.character(indv_data$station) == as.character(receiver_data$station_name[a]))]) / length(indv_data$station)
      spatial_sum = c(spatial_sum, (rho_i * log(rho_i)))
    }
    #print(spatial_sum)
    spatial_evenness_df[i, 2] = (-1 * sum((spatial_sum[spatial_sum != 'NaN']))) / log(R)
  }
  return(spatial_evenness_df)
}

#### Determining days present vs. Absent
days_present = function(vue_data, print = TRUE){
  emptyout = c()
  registerDoParallel(cores = 7)
  emptyout = foreach(i = 1:length(unique(vue_data$tag_id))) %dopar% {
    indv_data = vue_data[vue_data$tag_id == unique(vue_data$tag_id)[i], ]
    dates_detected = floor(indv_data$study_date) - min(floor(indv_data$study_date)) + 1
    present_absent = rep(FALSE, range(dates_detected)[2]-range(dates_detected)[1]+1)
    present_absent[dates_detected] = TRUE
    present = c()
    absent = c()
    present_counter = 1
    absent_counter = 1
    if(length(present_absent) > 1){
      for(r in 2:length(present_absent)){
        if(present_absent[r-1] == present_absent[r] & present_absent[r-1] == TRUE){
          present_counter = present_counter + 1
        }else if(present_absent[r-1] != present_absent[r] & present_absent[r-1] == TRUE){
          present = c(present, present_counter)
          present_counter = 1
          absent_counter = 1
        }else if(present_absent[r-1] == present_absent[r] & present_absent[r-1] == FALSE){
          absent_counter = absent_counter + 1
        }else if(present_absent[r-1] != present_absent[r] & present_absent[r-1] == FALSE){
          absent = c(absent, absent_counter)
          absent_counter = 1
          present_counter = 1
        }
        if(r == length(present_absent)){
          present = c(present, present_counter)
        }
      }
    }else{
      present = 1
    }
    if(print == TRUE){
      png(paste(unique(vue_data$tag_id)[i], 'presence-absence histogram.png', sep = ' '))
    }
    par(mfrow = c(1, 2))
    hist(present, breaks = 300,
         xlim = c(0, 300),
         xlab = 'Days',
         main = 'Consecutive Days Present',
         col  = 'black')
    if(is.null(absent) == FALSE){
      hist(absent, breaks = 300,
           xlim = c(0, 300),
           xlab = 'Days',
           main = 'Consecutive Days Absent',
           col  = 'black')
    }
    dev.off()
  }
  beep(3)
}

#### Converting opakapaka FL to weight based on Uchiyama and Kazama 2003
length_to_weight = function(FL, a = 0.000381465, b = 2.79567){
  # Based on the formula Weight = a*FL^b
  # Default coefficients for pooled sex opakapaka data
  weight = a * (FL^b)
  return(weight)
}

#### Getting distance (km) between two consecutive points 
lldist <- function(point1, point2){
  #The following program computes the distance on the surface of the earth 
  # between two points point1 and point2 in KM
  # Both the points are of the form (Longitude, Latitude)
  #From: http://www.biostat.umn.edu/~sudiptob/Software/distonearth.R
  R <- 6371
  p1rad <- point1 * pi/180
  p2rad <- point2 * pi/180
  d <- sin(p1rad[2])*sin(p2rad[2])+cos(p1rad[2])*cos(p2rad[2])*cos(abs(p1rad[1]-p2rad[1]))  
  d <- acos(d)
  R*d
}

#### Distance each tag was tracked 
distance_tracked = function(vue_data){
  ## Function to measure the total distance in km between subsequent receivers
  ## that a fish was detected
  individual_distance = matrix(0, length(unique(vue_data$tag_id)), 1)
  for (i in 1:length(unique(vue_data$tag_id))){
    subset = vue_data[vue_data$tag_id == unique(vue_data$tag_id)[i],]
    for (a in 2:length(subset$lon)){
      individual_distance[i] = individual_distance[i] + lldist(point1 = c(subset$lon[a-1], subset$lat[a-1]), point2 = c(subset$lon[a], subset$lat[a]))
    }
    individual_distance[i] = round(individual_distance[i], , digits = 2)
  }
  total_distance_tracked = sum(individual_distance)
  print (sprintf("All fish were tracked a collective distance of %s km", total_distance_tracked))
  return(individual_distance)
}

#### Generating histogram of detections for each fish
tagging_histogram = function(vue_data){
  for(i in 1:length(unique(vue_data$tag_id))){
    indv_data = vue_data[unique(vue_data$tag_id)[i], ]
    title = sprintf('%s Study Date Histogram.png', unique(vue_data$tag_id)[i])
    png(title)
    par(mfrow = c(1,1))
    plot_title = sprintf('Detections of Tag %s', unique(vue_data$tag_id)[i])
    indv_data = vue_data[vue_data$tag_id == unique(vue_data$tag_id)[i], ]
    hist(indv_data$study_date,  
         breaks = max(vue_data$study_date), 
         main = plot_title, xlab = 'Study Date', 
         ylab = 'Transmissions Detected', ylim = c(0,30), 
         xlim = c(0, max(indv_data$study_date)))
    abline(v = indv_data$study_date[indv_data$station == "Tagging Location"]-.5, col = 'red')
    for(n in 3:length(indv_data$station)){
      if(indv_data$station[n] != indv_data$station[n-1]){
        abline(v = indv_data$study_date[n]-.5, col = 'blue')
      }
    }
    dev.off()
  }
}

#### Assigning a unique color to each Station
assign_color_palette = function(vue_data){
  color_palette = list()
  color_palette$colors = rainbow(length(unique(vue_data$station)))
  color_palette$station = unique(vue_data$station)
  return(color_palette)
}

### Generating Stripchart of detections for all individuals
generate_stripchart = function(vue_data, color_palette = "black"){
  if(class(color_palette) != "list"){
    palette = list()
    palette$colors = rep(color_palette, length(unique(vue_data$station)))
    palette$station = unique(vue_data$station)
    color_palette = palette
  }else{
    color_palette = color_palette
  }
  for(i in 1:length(unique(vue_data$tag_id))){
    indv_data = vue_data[vue_data$tag_id == unique(vue_data$tag_id)[i], ]
    title = sprintf('%s Stripchart.png', unique(vue_data$tag_id)[i])
    png(title, width = 625, height = 150)
    par(mfrow = c(1,1))
    plot_title = sprintf('Detections of Tag %s', unique(vue_data$tag_id)[i])
    stripchart(indv_data$study_date[1], xlim = c(min(indv_data$study_date), max(indv_data$study_date)), main = plot_title)  
    changing_station_index = 1 # Index includes first detection since first detection is always "Tagging Location"
    for(n in 2:length(indv_data$study_date)){
      if(indv_data$station[n] != indv_data$station[n-1]){
        changing_station_index = c(changing_station_index, n)
      }
    }
    changing_station_index = c(changing_station_index, length(indv_data$study_date))
    for(k in 2:length(changing_station_index)){
      stripchart(indv_data$study_date[changing_station_index[k-1]:changing_station_index[k]], 
                 col = color_palette$colors[color_palette$station == indv_data$station[changing_station_index[k-1]]], 
                 pch = 19, add = TRUE)
    }
  dev.off()
  }
}

### Movements in and out of BRFA

in_brfa_e = function(vue_data){
     (vue_data$lat > 21.28333333 & vue_data$lat < 21.4166666) & # BRFA E
     (vue_data$lon > -157.6833333 & vue_data$lon < -157.533333) # west and east
     }
in_brfa_f = function(vue_data){
     (vue_data$lat > 20.9166666 & vue_data$lat < 21.03333333) & # BRFA F # south and north
     (vue_data$lon > -157.566666 & vue_data$lon < -157.3666666) # west and east
     } 

brfa_movements = function(vue_data){
  # brfa_out_receivers = paste('Oahu - Makapuu BRFA ', c(1:5, 22:34), sep = "")
  brfa_crossings_in_to_out = matrix(data = 0, nrow = length(unique(vue_data$tag_id)), ncol = 1)
  brfa_crossings_out_to_in = matrix(data = 0, nrow = length(unique(vue_data$tag_id)), ncol = 1)
  time_tracked_in = matrix(data = 0, nrow = length(unique(vue_data$tag_id)), ncol = 1)
  time_tracked_out = matrix(data = 0, nrow = length(unique(vue_data$tag_id)), ncol = 1)
  vue_data$in_brfa = FALSE
  vue_data$in_brfa[in_brfa_e(vue_data)] = TRUE
  vue_data$in_brfa[in_brfa_f(vue_data)] = TRUE
  for(i in 1:length(unique(vue_data$tag_id))){
    indv_data = vue_data[vue_data$tag_id == unique(vue_data$tag_id)[i], ]
    if(indv_data$in_brfa[1] == FALSE){
      out_start_index = 1
      in_start_index = c()
    }else{
      out_start_index = c()
      in_start_index = 1
    }
    for(k in 2:length(indv_data$in_brfa)){
      if(indv_data$in_brfa[k] == TRUE & indv_data$in_brfa[k-1] == FALSE){ # if it was in and is now out
        brfa_crossings_in_to_out[i] = brfa_crossings_in_to_out[i] + 1 # moved in to out
        in_start_index = c(in_start_index, k) # note detection out
      }else if(indv_data$in_brfa[k] == FALSE & indv_data$in_brfa[k-1] == TRUE){ # if is now in and started out
        brfa_crossings_out_to_in[i] = brfa_crossings_out_to_in[i] + 1 # moved out to in
        out_start_index = c(out_start_index, k) # note detection in
      }
    }
      if(indv_data$in_brfa[length(indv_data$in_brfa)] == TRUE){ # if the last detection was in brfa
        in_start_index = c(in_start_index, length(indv_data$in_brfa)) #increment in
      }else if(indv_data$in_brfa[length(indv_data$in_brfa)] == FALSE){
        out_start_index = c(out_start_index, length(indv_data$in_brfa))
      }
    crossing_times = unique(sort(c(in_start_index, out_start_index)))
    time_vector = rep(0, length(crossing_times)-1)
    for(r in 2:length(crossing_times)){
      time_vector[r-1] = difftime(indv_data$datetime[crossing_times[r]], indv_data$datetime[crossing_times[r-1]], units = 'days')
    }
    #### Calculating time spent in and out of BRFAs
    time_tracked_1 = 0
    time_tracked_2 = 0
        for(c in 1:length(time_vector)){
          if(c%%2 == 1){ # if time vector position is odd (ie: time 1, 3, 5... n)
            time_tracked_1 = time_tracked_1 + time_vector[c]
          }else{
            time_tracked_2 = time_tracked_2 + time_vector[c]
          }
        }
        if(is.null(in_start_index[1]) == FALSE){
          if(in_start_index[1] == 1){
            time_tracked_in[i] = round(time_tracked_1, digits = 2)
            time_tracked_out[i] = round(time_tracked_2, digits = 2)
          }else{
            time_tracked_in[i] = round(time_tracked_2, digits = 2)
            time_tracked_out[i] = round(time_tracked_1, digits = 2)
        }
        }else{
          time_tracked_out[i] = round(time_tracked_1, digits = 2)
        }
  }
  brfa_crossings = cbind(brfa_crossings_in_to_out, brfa_crossings_out_to_in, time_tracked_in, time_tracked_out)
  return(brfa_crossings)
}

n_movements = function(vue_data){
  movements = matrix(nrow = length(unique(vue_data$tag_id)), ncol = 1, data = 0)
  for(i in 1:length(unique(vue_data$tag_id))){
    indv_data = vue_data[vue_data$tag_id == unique(vue_data$tag_id)[i], ]
    if(length(indv_data$datetime) >= 2){
      for(r in 2:length(indv_data$datetime)){
        if(indv_data$station[r] != indv_data$station[r-1]){
          movements[i] = movements[i] + 1
        }
      }
    }
  }
  return(movements)
}

max_movement = function(vue_data){
  max_movement = matrix(ncol = 2, nrow = length(unique(vue_data$tag_id)), data = 0)
  for(i in 1:length(unique(vue_data$tag_id))){
    indv_data = vue_data[vue_data$tag_id == unique(vue_data$tag_id)[i], ]
    max_lat = c(indv_data$lon[which.max(indv_data$lat)], indv_data$lat[which.max(indv_data$lat)])
    max_lon = c(indv_data$lon[which.max(indv_data$lon)], indv_data$lat[which.max(indv_data$lon)])
    min_lat = c(indv_data$lon[which.min(indv_data$lat)], indv_data$lat[which.min(indv_data$lat)])
    min_lon = c(indv_data$lon[which.min(indv_data$lon)], indv_data$lat[which.min(indv_data$lon)])
    max_min_coordinates = rbind(max_lat, max_lon, min_lat, min_lon)
    if(dim(unique(max_min_coordinates))[1] > 2){
      # square km distance of polygon with conversion to km
      max_movement[i,1] = areaPolygon(max_min_coordinates) * 1*10^-6
    }else if(dim(unique(max_min_coordinates))[1] == 2){
      # linear distance in km between two points with conversion to km
      max_movement[i,2] = distGeo(p1 = unique(max_min_coordinates)[1, ], p2 = unique(max_min_coordinates)[2, ]) / 1000
    }
  }
  colnames(max_movement) = c('max polygon area', 'max linear area')
  return(max_movement)
}

get_fork_length = function(vue_data, tagging_data){
  fork_length = c()
  for(i in 1:length(unique(vue_data$tag_id))){
    fork_length = c(fork_length, as.character(tagging_data$"fork_length(cm)"[which(tagging_data$vem_tag_id == unique(vue_data$tag_id)[i])]))
  }
  return(fork_length)
}

get_tagging_date = function(vue_data, tagging_data){
  tagging_date = c()
  for(i in 1:length(unique(vue_data$tag_id))){
    tagging_date = c(tagging_date, as.character(tagging_data$datetime[which(tagging_data$vem_tag_id == unique(vue_data$tag_id)[i])]))
  }
  return(tagging_date)
}

run_analysis = function(vue_data, tagging_data, start_date = FALSE, end_date = FALSE){
  vue_df = vue_data
  ### Adding in date and location of tagging
  vue_df = generate_tagging_detection(tagging_data = tagging_data, vue_data = vue_data)
  if(start_date != FALSE){
    # vue_data = filter(vue_data, datetime >= start_date)
    vue_df = vue_df[vue_df$datetime >= start_date, ]
  }
  if(end_date != FALSE){
    # vue_data = filter(vue_data, datetime <= end_date)
    vue_df = vue_df[vue_df$datetime <= end_date, ]
  }
  ### Adding in study date - adjusts all date times relative to start of study
  vue_df = generate_study_date(vue_df) 
  ### Making detection matrix - Each row is a Tag ID, each station in a column. 
  ### matrix values are number of transmissions for a tag at a given station
  ## Note: Excludes tagging location
  tag_detection_matrix = build_detection_matrix(vue_df[vue_df$station != 'Tagging Location', ])  
  ### Determining how many times a single tag was detected
  ## Note: Excludes tagging location
  n_detections_by_tag = rowSums(tag_detection_matrix)  
  ## How many tags were detected at 2 or more stations?
  ## Note: Excludes tagging location
  n_stations_by_tag = rowSums(tag_detection_matrix / tag_detection_matrix, na.rm = TRUE)  
  ## How many unique days were tags detected?
  ## Note: Excludes tagging
  unique_days_detected = calculate_days_detected(vue_df[vue_df$station != 'Tagging Location', ])  
  ## How many days was an individual tracked?
  time_at_liberty = calculate_time_at_liberty(vue_df)  
  ## How many transmissions were detected per day on average?
  transmissions_per_day = list()
  # for entire time at liberty
  transmissions_per_day$at_liberty = n_detections_by_tag / time_at_liberty
  # only for days a tag was detected
  transmissions_per_day$days_detected = n_detections_by_tag / unique_days_detected  
  ## How many days after tracking until individual appears on array?
  days_before_detection = calculate_days_before_detection(vue_df)  
  ## What percentage of days at liberty was a tag detected?
  detected_by_liberty = calculate_days_detected(vue_df) / ceiling(calculate_time_at_liberty(vue_df))
  ## How far was a tag tracked?
  track_distance = distance_tracked(vue_df)
  ## How many times was a tag detected changing station?
  movements = n_movements(vue_df)
  ## What stations was a tag detected on?
  detected_stations = stations_detected(vue_df)
  #### Movements into / out of BRFA for each fish
  brfa_stats = brfa_movements(vue_df)
  #### Number of fish tagged greater than L50 
  n_mature = length(which(as.numeric(as.character(tagging_data$"fork_length(cm)"[tagging_data$vem_tag_id %in% unique(vue_df$tag_id)])) >= 43))
  proportion_mature = round(n_mature / length(unique(vue_df$tag_id)), digits = 2)
  #### Homerange size
  homerange = max_movement(vue_df)
  #### Creating analysis list
  analysis_summary = list()
  analysis_summary$data = vue_df
  analysis_summary$tag_ids = unique(vue_df$tag_id)
  analysis_summary$tagging_date = get_tagging_date(vue_data = vue_data, tagging_data = tagging_data)
  analysis_summary$fork_length = get_fork_length(vue_data = vue_data, tagging_data = tagging_data)
  analysis_summary$tag_detection_matrix = tag_detection_matrix
  analysis_summary$n_detections_by_tag = n_detections_by_tag
  analysis_summary$n_stations_by_tag = n_stations_by_tag
  analysis_summary$unique_days_detected = unique_days_detected
  analysis_summary$time_at_liberty = time_at_liberty
  analysis_summary$transmissions_per_day = transmissions_per_day
  analysis_summary$days_before_detection = days_before_detection
  analysis_summary$percentage_of_days_at_liberty_detected = detected_by_liberty
  analysis_summary$track_distance = track_distance
  analysis_summary$movements = movements
  analysis_summary$detected_stations = detected_stations
  analysis_summary$brfa_stats = brfa_stats
  analysis_summary$n_mature = n_mature
  analysis_summary$proportion_mature = proportion_mature
  analysis_summary$homerange = homerange
  return(analysis_summary)
}

generate_analysis_report = function(vue_data, tagging_data, start_date, end_date){
  analysis_summary = run_analysis(vue_data = vue_data, tagging_data = tagging_data, start_date = start_date, end_date = end_date)
  sink("analysis_output.txt")
  for(i in 1:length(unique(analysis_summary$data$tag_id))){
    cat(paste("Transmitter ID: ", unique(analysis_summary$data$tag_id)[i], "\n"))
    cat(paste("Species: ", tagging_data$species[which(tagging_data$vem_tag_id == unique(analysis_summary$data$tag_id)[i])], "\n"))
    cat(paste("Fork Length: ", tagging_data$"fork_length(cm)"[which(tagging_data$vem_tag_id == unique(analysis_summary$data$tag_id)[i])], " cm", "\n"))
    cat(paste("Tagging Date: ", tagging_data$datetime[which(tagging_data$vem_tag_id == unique(analysis_summary$data$tag_id)[i])], "\n"))
    cat(paste("Tagging Location: ", tagging_data$area_of_capture[which(tagging_data$vem_tag_id == unique(analysis_summary$data$tag_id)[i])], "\n"))  
    cat(paste("Day of First Detected: ", analysis_summary$data$datetime[which(analysis_summary$data$tag_id == unique(analysis_summary$data$tag_id)[i] & analysis_summary$data$station != "Tagging Location")][1], "\n"))
    cat(paste("Day of Last Detection: ", analysis_summary$data$datetime[which(analysis_summary$data$tag_id == unique(analysis_summary$data$tag_id)[i])][length(which(analysis_summary$data$tag_id == unique(analysis_summary$data$tag_id)[i] & analysis_summary$data$station != "Tagging Location"))], "\n"))
    cat(paste("Days at Liberty: ", ceiling(analysis_summary$time_at_liberty[i]), "\n"))
    cat(paste("Unique Days Detected: ", analysis_summary$unique_days_detected[i], "\n"))
    cat(paste("Number of Detections (Total): ", length(which(analysis_summary$data$tag_id == unique(analysis_summary$data$tag_id)[i] & analysis_summary$data$station != "Tagging Location")), "\n"))
    cat(paste("Number of Detections / Day at Liberty: ", analysis_summary$transmissions_per_day$at_liberty[i], "\n"))
    cat(paste("Number of Detections / Day Detected: ", analysis_summary$transmissions_per_day$days_detected[i], "\n"))
    cat(paste("Number of Receivers Detected (Total): ", analysis_summary$n_stations_by_tag[i], "\n"))
    cat(paste("Number of receivers Detected / Days at Liberty: ", analysis_summary$n_stations_by_tag[i] / analysis_summary$time_at_liberty[i], "\n"))
    cat(paste("Movements from inside to outside BRFA (Total): ", analysis_summary$brfa_stats[i,1], "\n"))
    cat(paste("Movements from inside to outside BRFA / Day: ", analysis_summary$brfa_stats[i,1] / analysis_summary$time_at_liberty[i], "\n"))
    cat(paste("Movements from outside to inside BRFA (Total): ", analysis_summary$brfa_stats[i,2], "\n"))
    cat(paste("Movements from outside to inside BRFA / Day: ", analysis_summary$brfa_stats[i,2]/analysis_summary$time_at_liberty[i], "\n"))
    cat(paste("Time in BRFA: ", analysis_summary$brfa_stats[i,3], "\n"))
    cat(paste("Time out of BRFA: ", analysis_summary$brfa_stats[i,4], "\n"))
    cat(paste("Approximate Distance Tracked (Total): ", analysis_summary$track_distance[i], "km","\n"))
    cat(paste("Approximate Distance Tracked / Day: ", analysis_summary$track_distance[i] / analysis_summary$time_at_liberty[i],"km", "\n")) 
    cat(paste(" ", analysis_summary$detected_stations$unique[i], "\n"))
    if(analysis_summary$homerange[i,1] != 0){
      cat(paste("Area of detected homerange", round(analysis_summary$homerange[i,1], digits = 2), 'km^2'))
    }else if(analysis_summary$homerange[i,2] != 0){
      cat(paste("Maximum linear Distance", round(analysis_summary$homerange[i,2], digits = 2), 'km'))
    }
    cat("\n")
  }
  sink()
  return(analysis_summary)
}

create_analysis_csv = function(vue_data, tagging_data = tagging_data, start_date, end_date){
  analysis_out = run_analysis(vue_data = vue_data, tagging_data = tagging_data, start_date = start_date, end_date = end_date)
  tag_id = as.vector(analysis_out$tag_id)
  tagging_date = analysis_out$tagging_date
  fork_length = analysis_out$fork_length
  time_at_liberty = analysis_out$time_at_liberty
  n_detections = analysis_out$n_detections
  percent_of_detections = analysis_out$n_detections/sum(analysis_out$n_detections)
  detections_per_day = n_detections/time_at_liberty
  unique_days_detected = analysis_out$unique_days_detected
  distance_tracked = analysis_out$track_distance
  distance_per_day = distance_tracked / time_at_liberty
  receivers_detected = analysis_out$n_stations_by_tag
  movements = analysis_out$movements
  movements_by_day = movements/time_at_liberty
  brfa_crossings = rowSums(analysis_out$brfa_stats[, 1:2])
  time_tracked_in = analysis_out$brfa_stats[ ,3]
  time_tracked_out = analysis_out$brfa_stats[ ,4]
  brfa_crossings_per_day = brfa_crossings/time_at_liberty
  homerange_polygon = analysis_out$homerange[ ,1]
  homerange_linear = analysis_out$homerange[ ,2]
  analysis_df = data.frame(cbind(tag_id, tagging_date, time_at_liberty,n_detections, percent_of_detections, detections_per_day, unique_days_detected, distance_tracked, distance_per_day, receivers_detected, movements, movements_by_day, brfa_crossings, time_tracked_in, time_tracked_out, brfa_crossings_per_day, homerange_polygon, homerange_linear))
  write.csv(analysis_df, file = "analysis.csv")
}



### Assigning output directory
setwd(output_directory)


#### Filtering for false detections 
#vue_data$false_detection_index = remove_false_detections(vue_data, remove_detections = FALSE)
 #false_detections = vue_data[false_detection_index == FALSE, ]

#### Minimum average size of fish to tag ---------------------
#### Determining size minimums for tagging paka based on McCleave and Stred, 1975, and Adams et al. 1998 
## (see Parrish et al 2015 for sources). Tag weight should not exceed 2% of fish weight
paka_lengths = 1:100 ## hypothetical size distribution between 1cm and 100cm FL
paka_weights.kg = length_to_weight(paka_lengths) ## Convert sizes to KG
paka_weights.g = paka_weights.kg*1000
paka_weights_.02.g = paka_weights.g*.02
v13_weight = 10.2024 # grams
v13p_weight = 12.7698 # grams
## have to add 1 to each of these as they are under, not over estimates. 
## figured this out by looking at values prior to absoluting them, but probably 
## theres an automated way
which.min(abs(paka_weights_.02.g - v13_weight)) 
# minimum size of fish tagged with V13 = 14cm
which.min(abs(paka_weights_.02.g - v13p_weight))
# minimum size of fish tagged with V13p = 15cm

#### Generating Plots -----------------------------------------------------------------------
plot_receiver_maps = function(receiver_data, plot_lost = FALSE, start_date = FALSE, end_date = FALSE, date_format = "%Y-%m-%d %H:%M:%S", daily = TRUE, remove_station = FALSE, region = "Makapuu", rec_col = "red", print = TRUE){
  if(plot_lost == FALSE){
    receiver_data = receiver_data[receiver_data$recovered == "",]
  }
  if(remove_station != FALSE){
    receiver_data = receiver_data[!(receiver_data$station_name %in% remove_station), ]
  }
  receiver_data = receiver_data[is.na(receiver_data$deployment_date) == FALSE, ]
  activity_dates = sort(unique(c(as.character(receiver_data$deployment_date[is.na(receiver_data$deployment_date) == FALSE]), as.character(receiver_data$recovery_date[is.na(receiver_data$recovery_date) == FALSE]))))
    if(start_date != FALSE){
      # start_date = as.POSIXct(start_date, format = date_format)
      # receiver_data = receiver_data[which(receiver_data$deployment_date <= start_date), ]
      activity_dates = c(activity_dates, as.character(start_date))
      activity_dates = activity_dates[as.POSIXct(activity_dates, format = "%Y-%m-%d %H:%M:%S") >= start_date]
    }
  if(end_date != FALSE){
    # end_date = as.POSIXct(end_date, format = date_format)
    # receiver_data = receiver_data[which(receiver_data$recovery_date > end_date | is.na(receiver_data$recovery_date)), ]
    activity_dates = c(activity_dates, as.character(end_date))
    activity_dates = activity_dates[as.POSIXct(activity_dates, format = "%Y-%m-%d %H:%M:%S") <= end_date]
  }
  activity_dates = as.POSIXct(sort(activity_dates), format = "%Y-%m-%d %H:%M:%S")
  if(daily == TRUE){
    activity_dates = unique(floor_date(activity_dates, unit = "day"))
  }
  for(i in 1:length(activity_dates)){
    receivers_to_plot = receiver_data[which(receiver_data$deployment_date <= activity_dates[i] & 
                                              (is.na(receiver_data$recovery_date) == TRUE | receiver_data$recovery_date > activity_dates[i])), ]
    plot_receiver_map(receivers_to_plot, filename = paste(activity_dates[i], 'Receiver Map - Current Stations - Red', sep = " "), rec_col = "red", region = region)
    # dev.off()
    plot_receiver_map(receivers_to_plot, filename = paste(activity_dates[i], 'Receiver Map - Current Stations - Station Colors', sep = " "), rec_col = rec_col, region = region)
    # dev.off()
  }
  plot_receiver_map(receiver_data, filename = paste(min(activity_dates), "-", max(activity_dates), 'Receiver Map - All Stations - Red', sep = " "), rec_col = 'red', region = region)
  # dev.off()
  plot_receiver_map(receiver_data, filename = paste(min(activity_dates), "-", max(activity_dates), 'Receiver Map - All Stations - Station Colors', sep = " "), rec_col = rec_col, region = region)
  # dev.off()
}
# 
# plot_receiver_locations = function(vue_data, receiver_data = receiver_data, start_date, end_date){
# ##### Making Maps of Receiver Deployment Locations
# current_study_dates = c(start_date, end_date) # Last date choosen as last detection from BRFA 27 station which was found soon after off Maui
# receiver_data_to_plot = list()
# receiver_data_to_plot$all_receivers = receiver_data
# receiver_data_to_plot$current_study_all = receiver_data[which(receiver_data$recovery_date >= current_study_dates[2]), ]
# receiver_data_to_plot$current_study_recovered = receiver_data[which(receiver_data$deployment_date > current_study_dates[1] & receiver_data$recovery_date >= current_study_dates[2] & receiver_data$recovered == ''), ]
# receiver_data_to_plot$currently_deployed = receiver_data[is.na(receiver_data$recovery_date), ]
# 
# #### Plotting Maps 
# station_palette = assign_color_palette(vue_data)
# plot_receiver_map(receiver_data_to_plot$current_study_all, filename = 'Receiver Map - All Stations', rec_col = 'red')
# plot_receiver_map(receiver_data_to_plot$current_study_recovered, filename = 'Receiver Map - Stations Recovered - red', rec_col = 'red')
# plot_receiver_map(receiver_data_to_plot$current_study_recovered, filename = 'Receiver Map - Stations Recovered - color mapped', rec_col = station_palette)
# plot_receiver_map(receiver_data_to_plot$currently_deployed, filename = 'Receiver Map - Stations Redeployed - color mapped', rec_col = station_palette)
# plot_receiver_map(receiver_data_to_plot$currently_deployed, filename = 'Receiver Map - Stations Redeployed - color red', rec_col = station_palette)
# #### Movement maps for each fish on map of only receivers that were retrieved
# plot_movements(vue_data, receiver_data = receiver_data_to_plot$current_study_recovered, region = 'Makapuu', rec_col = station_palette)
# }

#### Creating day night plots
create_day_night_plot = function(vue_data, color_palette = FALSE, start_date = FALSE, end_date = FALSE, date_format = "%Y-%m-%d %H:%M:%S"){
  if(start_date == FALSE){
    start_date = min(vue_data$datetime)
  }else if(start_date != FALSE){
    start_date = as.POSIXct(start_date, date_format)
  }
  if(end_date == FALSE){
    end_date = max(vue_data$datetime)
  }else if(end_date != FALSE){
    end_date = as.POSIXct(end_date, date_format)
  }
  vue_data = vue_data[vue_data$datetime >= start_date, ]
  vue_data = vue_data[vue_data$datetime <= end_date, ]
  
  if(class(color_palette) == 'logical'){
    vue_data$plot_color = "black"
  }else if(class(color_palette$colors) == "character"){
    vue_data$plot_color = "black"
    for(i in 1:length(color_palette$station)){
      vue_data$plot_color[vue_data$station == color_palette$station[i]] = color_palette$color[i]
    }
  }else{
    vue_data$plot_color = color_palette
  }
  
  # Creating plot_date and plot_time columns
  vue_data$plot_date  = as.POSIXlt(as.character(vue_data$datetime),format = "%Y-%m-%d")
  vue_data$plot_time = as.numeric(format(as.POSIXlt(vue_data$datetime,format= "%H:%M:%S"),'%H'))+as.numeric(format(as.POSIXlt(vue_data$datetime,format= "%H:%M:%S"),'%M'))/60+as.numeric(format(as.POSIXlt(vue_data$datetime,format= "%H:%M:%S"),'%S'))/3600
  
  for(i in 1:length(unique(vue_data$tag_id))){
    ### Setting up graphics window
    title = paste(unique(vue_data$tag_id)[i], "Day Night Detection Plot.png")
    png(title)
    par(mfcol=c(1,1))
    
    ### Subsetting data
    indv_data = vue_data[vue_data$tag_id == unique(vue_data$tag_id)[i],]
    with(vue_data, plot(plot_date,plot_time,col=plot_color,main=unique(vue_data$tag_id)[1],pch=19,cex=1,ylim=c(0,24),type="n",xaxs="i",yaxs="i",xlab="Date", ylab="Time"))
    
     #if (nrow(indv_data)>1){
      
      sundate<-seq(min(vue_data$plot_date),max(vue_data$plot_date), by="day")
      
      sundateJ<-as.numeric(format(seq(min(vue_data$plot_date),max(vue_data$plot_date), by="day"),"%j"))
      
      
      
      sun<-suncalc(sundateJ,20.63229,-156.49693,UTC=TRUE)
      lines(sundate,sun$sunrise,col="326")
      lines(sundate,sun$sunset,col="326") 
      
      
      cord.x <- c(min(sundate),sundate,max(sundate)) 
      cord.y <- c(24, sun$sunset,24)
      polygon(cord.x,cord.y,col='grey')
      # with(indv_data, points(plot_date,plot_time,col=zone,pch=16,cex=.75))
      
      cord.x <- c(min(sundate),sundate,max(sundate)) 
      cord.y <- c(0, sun$sunrise,0)
      polygon(cord.x,cord.y,col='grey')
      with(indv_data, points(plot_date, plot_time, col = plot_color, pch = 19, cex = 1))
    }
    dev.off()
  }


#### Looking at receiver performance
## Detection history per receiver
plot_detections_by_receiver = function(vue_data){
  png('Detections By Receiver Stripchart.png')
stripchart(vue_data$study_date ~ as.factor(as.character(vue_data$receiver)), pch = 15,
           main = "Detections by Receiver")          
## Do detections have an hourly pattern?
hist(hour(vue_data$datetime), right = FALSE, breaks = 0:24, main = "Detections per Hour")
## Do detections have any observable daily patterns
hist(day(vue_data$datetime), right = False, breaks = min(vue_data$datetime:max(vue_data$datetime)))
hist(floor(vue_data$study_date), right = FALSE, breaks = 0:ceiling(max(vue_data$study_date)),
     main = 'Daily Detections at Each Receiver', xlab = 'Study Date', ylab = 'Detections')

par(mfrow = c(3, 4),oma=c(0,0,2,0)) # setting up plotting grid
for(i in 2:length(unique(vue_data$station))){
  indv_data = vue_data[vue_data$station == unique(vue_data$station)[i], ]
  hist(floor(indv_data$study_date), right = FALSE, breaks = 0:ceiling(max(vue_data$study_date)),
       main = unique(vue_data$station)[i], # individual plot label
       xlab = 'Study Date', # x axis label
       ylab = 'Detections') # y axis label
}
title("Detections Per Day", outer=TRUE)
dev.off()
}


#### History of detections
detection_stripcharts = function(vue_data, false_detection_index = FALSE, aggregate = FALSE){
  if(aggregate == TRUE){
## Looking at history of all detections
png('detection_stripchart_all.png')
par(mfrow = c(1, 1))
stripchart(vue_data$study_date ~ as.factor(vue_data$tag_id),
           main = "All Detections")
dev.off()
png('detection_stripchart_valid.png')
par(mfrow = c(1,1))
## Looking at history of "Valid" detections (Not questionable)
stripchart(vue_data$study_date[false_detection_index == TRUE] ~ as.factor(vue_data$tag_id[false_detection_index == TRUE]),
           main = "Valid Detections", col = 'darkgreen', pch = 15, add = FALSE)
dev.off()
## Looking at history of just questionable detections
png('detection_stripchart_questionable.png')
stripchart(vue_data$study_date[false_detection_index == FALSE] ~ as.factor(vue_data$tag_id[false_detection_index == FALSE]),
           main = "Questionable Detections", col = 'darkred', cex = .5, pch = 16, add = TRUE)
  }else if(aggregate == FALSE){
    png('detection_stripchart_all.png')
    par(mfrow = c(1, 3))
    stripchart(vue_data$study_date ~ as.factor(vue_data$tag_id),
               main = "All Detections")
    ## Looking at history of "Valid" detections (Not questionable)
    stripchart(vue_data$study_date[false_detection_index == TRUE] ~ as.factor(vue_data$tag_id[false_detection_index == TRUE]),
               main = "Valid Detections", col = 'darkgreen', pch = 15, add = FALSE)
    ## Looking at history of just questionable detections
    stripchart(vue_data$study_date[false_detection_index == FALSE] ~ as.factor(vue_data$tag_id[false_detection_index == FALSE]),
               main = "Questionable Detections", col = 'darkred', cex = .5, pch = 16, add = TRUE)
  }
  dev.off()
}

####### Function to execute program  -------
run = function(vue_data, receiver_data, tagging_data, start_date = FALSE, end_date = FALSE, date_format = "%Y-%m-%d %H:%M%:S", output_directory = FALSE, region = "Makapuu"){
  old_dir = getwd() # Saving current directory path
  setwd(create_save_directory(new_directory = TRUE)) # Creating new directory for run
  if(start_date != FALSE){ # Establishing start date
    start_date = as.POSIXct(start_date, format = date_format)
  }
  if(end_date != FALSE){ # Establishing end date
    end_date = as.POSIXct(end_date, format = date_format)
  }
  #false_detection_index = remove_false_detections(vue_data, remove_detections = FALSE) # Removing false detections
  analysis_summary = generate_analysis_report(vue_data = vue_data, tagging_data = tagging_data, start_date = start_date, end_date = end_date)
  #analysis_summary$false_detection_index = false_detection_index
  create_analysis_csv(vue_data = analysis_summary$data, tagging_data = tagging_data, start_date = start_date, end_date = end_date)
  #### Plotting receiver deployments
  current_dir = getwd()
  ## Mapping colors
  station_palette = assign_color_palette(vue_data)
  setwd(create_save_directory('Receiver Maps'))
  plot_receiver_maps(receiver_data = receiver_data, start_date = start_date, end_date = end_date, rec_col = station_palette, region = region)
  setwd(current_dir)
  #### Plotting movements of each tag tags
  setwd(create_save_directory('Tag Data')) # create new directory for storing individual tag outputs
  plot_tag_detections(analysis_summary$data, receiver_data, start_date = start_date, end_date = end_date, date_format = "%Y-%m-%d %H:%M:%S", plot_lost = FALSE, remove_station = FALSE, tag_ids = FALSE, rec_col = station_palette, plot_receivers = "study", region = region)
  #### Detection stripchart for each tag with color corrosponding to stations
  generate_stripchart(vue_data = analysis_summary$data, color_palette = station_palette)
  create_day_night_plot(vue_data = analysis_summary$data, color_palette = station_palette)
  #### Detection histogram for each tag 
  tag_detection_histogram(vue_data = analysis_summary$data, collate = FALSE)
  tag_detection_histogram(vue_data = analysis_summary$data, collate = TRUE)
  #### Plotting depths for tags with depth sensors
  plot_depths(vue_data = analysis_summary$data, individual_tags = TRUE, print = TRUE)
  plot_depths(vue_data = analysis_summary$data, individual_tags = FALSE, print = TRUE)
  setwd(current_dir) # return to main directory
  setwd(create_save_directory('Detection Stripcharts')) # Create directory for all tag detection strip chart
  detection_stripcharts(vue_data = analysis_summary$data, false_detection_index = false_detection_index, aggregate = TRUE)
  setwd(old_dir) # resetting to main project directory 
  print('SUCCESS!')
  beep(8) # SUCCESS!
    return(analysis_summary)
}

#### USAGE----------


##### Analysis - Feb 2016 ----------------------------------------------------------
#### Importing Data Files -----
vue_data = load_vemco(filename = 'VUE_Export_2016-January-11.csv', filepath ='/Users/stephenscherrer/Dropbox/Lab Folder/Oahu Receiver Data Files/')
receiver_data = load_receiver_data(filename = 'DEPLOYMENT_RECOVERY_LOG.csv', filepath = '/Users/stephenscherrer/Dropbox/Lab Folder/Oahu Receiver Data Files/')
tagging_data = load_tagging_data('/Users/stephenscherrer/Dropbox/Lab Folder/Oahu Receiver Data Files/Bottomfish_Tag_Master.csv')
vue_data$tag_id = as.numeric(as.character(vue_data$tag_id))
vue_data$station = as.character(vue_data$station)
vue_data = clean_vue_data(vue_data = vue_data, receiver_data = receiver_data)

#### Cleaning Data Files -----------------------------------
  ### Removing all tags not associated with study from vue
  tag_ids = c(898:927, 18249:18275, 57445:57466, 37965:37985)
  n_detections = list()
  n_detections$total = dim(vue_data)[1] # Total number of detections from all tags
#### Removing any detections not associated with the study or from tags previously deployed in range tests
  vue_data = remove_detections_before_tagging(vue_data, tagging_data)
#### Removing tags with issues
  dead_tags = c(37969, 57459)
  cross_tags = c(57451) # tags are at cross seamount. cannot find tags in hard copy logs. probably monchong tagged with BF tags.
  vue_data = vue_data[!(vue_data$tag_id %in% dead_tags, ]
  vue_data = vue_data[!(vue_data$tag_id %in% cross_tags, ]


#### Producing results for first phase of study
  phase1 = run(vue_data = vue_data, receiver_data = receiver_data, tagging_data = tagging_data,
               start_date = as.POSIXct("2012-04-13"), 
               end_date = as.POSIXct("2014-12-06"),
               region = "Oahu and Penguin Banks")
  # for(i in 1:1000){dev.off()}
#### Producing results for second phase of study
  phase2 = run(vue_data, receiver_data = receiver_data, tagging_data = tagging_data,
               start_date = as.POSIXct("2014-12-07"),
               end_date = FALSE,
               region = "Oahu and Penguin Banks")
  # for(i in 1:1000){dev.off()}
#### Rerunning analysis without botcam data. For plotting movement maps 
#### and distance tracked statistics
  vue_data = vue_data[!(vue_data$station %in% "With BotCam Crew"), ]
## Removing cross seamount from receiver_data
  receiver_data = clean_receiver_stations(receiver_data, region = c('Oahu', 'PB'), remove = FALSE)
#### Rerunning phase 1 analysis without botcam detections
  phase1 = run(vue_data = vue_data, receiver_data = receiver_data, tagging_data = tagging_data,
               start_date = as.POSIXct("2012-04-13"), 
               end_date = as.POSIXct("2014-12-06"),
               region = "Oahu and Penguin Banks")
  # for(i in 1:1000){dev.off()}
#### Rerunning phase 2 analyis without botcam detections
  phase2 = run(vue_data, receiver_data = receiver_data, tagging_data = tagging_data,
               start_date = as.POSIXct("2014-12-07"),
               end_date = FALSE,
               region = "Makapuu")
# for(i in 1:1000){dev.off()}
#### Minimum distance across BRFA E - across south border
  brfa_e_min_dim = lldist(point1 = c(-157.6833333, 21.28333333), point2 = c(-157.533333, 21.28333333))
  brfa_e_north_south = lldist(point1 = c(-157.533333, 21.4166666), point2 = c(-157.6833333, 21.28333333))
#### Minimum distance across BRFA F
  brfa_f_min_dim = lldist(point1 = c(-157.3666666, 20.9166666), point2 = c(-157.3666666, 21.03333333))
  brfa_f_east_west = lldist(point1 = c(-157.566666, 21.03333333), point2 = c(-157.3666666, 21.03333333))
#### Maximum homerange size
  homeranges = round(max_movement(vue_data), digits = 2)
  View(homeranges)

#### Saving Workspace Image ----------------------------------------------------------------
save.image(file = "workspace_image")
beep(2)


clean_receiver_stations = function(receiver_data, region = c('Oahu', 'PB'), remove = FALSE){
  index = c()
  for(i in 1:length(receiver_data$station_name)){
    if(strsplit(as.character(receiver_data$station_name[i]), split = " ")[[1]][1] %in% region == TRUE){
      index = c(index, i)
    }
  }
  if(remove == FALSE){
    receiver_data = receiver_data[index, ]
  }else if(remove == TRUE){
    receiver_data = receiver_data[!index, ]
  }
  receiver_data$station_name = as.character(receiver_data$station_name)
  return(receiver_data)
}


####
distance_between_receivers = function(receiver_data, start_date = FALSE, end_date = FALSE, include_lost = FALSE){
  if(start_date != FALSE){
    receiver_data = receiver_data[receiver_data$deployment_date <= start_date, ]
  }
  if(end_date != FALSE){
    receiver_data = receiver_data[which(receiver_data$recovery_date >= end_date | is.na(receiver_data$recovery_date)), ]
  }else{
    receiver_data = receiver_data[which(receiver_data$recovery_date > start_date | is.na(receiver_data$recovery_date)), ]
  }
  if(include_lost == FALSE){
    receiver_data = receiver_data[which(receiver_data$recovered == ""), ]
  }
  distances_between_receivers = list()
  distances_between_receivers$matrix = matrix(0, length(receiver_data$lat), length(receiver_data$lat))
  for(i in 1:length(receiver_data$lat)){
    for (a in 1:length(receiver_data$lat)){
      if(a != i){ # Added because when i = 17 and a = 17, lldist was producing NaNs.
      distances_between_receivers$matrix[i, a] = lldist(point1 = c(receiver_data$lon[i], receiver_data$lat[i]), point2 = c(receiver_data$lon[a], receiver_data$lat[a]))
    }
    }
  }
  distances_between_receivers$mean = mean(distances_between_receivers$matrix[which(distances_between_receivers$matrix > 0 & is.na(distances_between_receivers$matrix)==FALSE)])
  distances_between_receivers$sd = sd(distances_between_receivers$matrix[which(distances_between_receivers$matrix > 0 & is.na(distances_between_receivers$matrix)==FALSE)])
  distances_between_receivers$iqr = fivenum(distances_between_receivers$matrix[which(distances_between_receivers$matrix > 0 & is.na(distances_between_receivers$matrix)==FALSE)])[c(2,4)]
  return(distances_between_receivers)
}

#### Determining mean distance and sd for receivers in phase 2
  receiver_distance_phase_2 = distance_between_receivers(receiver_data, start_date = as.POSIXct("2015-02-28 00:00:00"), 
                                                                        end_date = as.POSIXct("2015-03-01 00:00:00"), include_lost = FALSE)
  # mean = 39.48634, sd = 19.37889, IQR = 4.377035, 16.558240

#### Determining mean distance and sd for receivers in phase 2.5
  receiver_distance_phase_2.5 = distance_between_receivers(receiver_data, start_date = as.POSIXct("2015-12-28 00:00:00"), 
                                                                          end_date = as.POSIXct("2015-12-29 00:00:00"), include_lost = FALSE)
  # mean = 10.41 km sd = 6.85 km, IQR = 4.899075, 15.394144

  receiver_distance_phase_1 = distance_between_receivers(receiver_data, start_date = as.POSIXct("2013-12-06 00:00:00"),
                                                                        end_date = as.POSIXct("2013-12-07 00:00:00"), include_lost = FALSE)
  # mean = 50.94945 km, sd = 27.96825 km, IQR = 30.89530, 69.65363


#### phase 1 dates
start_date = as.POSIXct('2012-04-13 16:05:30 HST')
end_date = as.POSIXct("2013-12-07 00:00:00")


#### Phase 2.1 dates
start_date = as.POSIXct('2015-03-16 16:05:30 HST')
end_date = as.POSIXct('2015-05-25 00:00:00')

#### Phase 2.2 dates
start_date = as.POSIXct('2015-05-30 00:00:00')
end_date = as.POSIXct('2015-11-29 00:00:00')

#### Phase 2.3 dates
start_date = as.POSIXct('2015-11-29 00:00:00')
end_date = as.POSIXct("2013-12-07 00:00:00")

#### Notes on tags 37960 moves between pinicle south and the mount

good_tracks_p2 = c(18249, 18251, 18253, 18259, 18260)
questionable_tracks_p2 = c(18250, 18252, 18254, 18255, 18256, 18257, 918, 919, 916, 910)
homerange = list(cbind(tag_ids, homerange))
homerange$good = homerange[[1]][homerange[[1]][ ,1] %in% good_tracks_p2, ]
homerange$quest = homerange[[1]][homerange[[1]][ ,1] %in% questionable_tracks_p2, ]

### Tags with tracks that persist at least a week after tagging
good_tracks_7day = c()
for(i in 1:length(unique(vue_data.good$tag_id))){
  indv_data = vue_data.good[vue_data.good$tag_id == unique(vue_data.good$tag_id)[i], ]
  if(abs(difftime(time1 = min(indv_data$datetime), time2 = max(indv_data$datetime), units = "days")) >= 7){
    good_tracks_7day = c(good_tracks_7day, unique(vue_data.good$tag_id)[i])
  }
}
quest_tracks_7day = c()
for(i in 1:length(unique(vue_data.quest$tag_id))){
  indv_data = vue_data.good[vue_data.quest$tag_id == unique(vue_data.quest$tag_id)[i], ]
  if(abs(difftime(time1 = min(indv_data$datetime), time2 = max(indv_data$datetime), units = "days")) >= 7){
    quest_tracks_7day = c(quest_tracks_7day, unique(vue_data.quest$tag_id)[i])
  }
}
good_abs_7days = c()
for(i in 1:length(unique(vue_data.good$tag_id))){
  indv_data = vue_data.good[vue_data.good$tag_id == unique(vue_data.good$tag_id)[i], ]
   days = calculate_days_detected(vue_data.good[vue_data.good$station != 'Tagging Location', ]) 
  if(days >=7){good_abs_7_days = c(good_abs_7_days, days[1])}
}
}
quest_abs_7day = c()
for(i in 1:length(unique(vue_data.quest$tag_id))){
  indv_data = vue_data.quest[vue_data.quest$tag_id == unique(vue_data.quest$tag_id)[i], ]
  days = calculate_days_detected(vue_data.quest[vue_data.quest$station != 'Tagging Location', ]) 
  if(days >=7){quest_abs_7_days = c(good_abs_7_days, days[1])}
}
}

#### track lengths for good and bad tags
track_dist = cbind(analysis_summary$tag_id, analysis_summary$track_distance)
track_dist.good = track_dist[track_dist[,1] %in% good_tracks_p2, ]
range(track_dist.good[,2])
fivenum(track_dist.good[,2])
track_dist.quest = track_dist[track_dist[,1] %in% questionable_tracks_p2, ]
range(track_dist.quest[,2])
fivenum(track_dist.quest[,2])

## How many fish changed locations
n_stations = cbind(analysis_summary$tag_id, analysis_summary$n_stations_by_tag)
n_stations.good = n_stations[n_stations[,1] %in% good_tracks_p2, ]
range(n_stations.good[,2])
fivenum(n_stations.good[,2])
n_stations.quest = n_stations[n_stations[,1] %in% questionable_tracks_p2, ]
range(n_stations.quest[,2])
fivenum(n_stations.quest[,2])

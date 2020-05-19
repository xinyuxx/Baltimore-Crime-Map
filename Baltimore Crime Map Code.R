# # ===================================================
# GBA464
# Author: Xinyu Huang
# Description: write a function that plots crimes 
#              incidence in Baltimore city
# Data: Baltimore crime data
# Source: https://data.baltimorecity.gov/
# ===================================================

# clear everything
rm(list = ls()) 

# libraries 
#   need to install.packages() these
#   let me know if installation does not work
library(maps)
library(maptools)

# download, unzip and read the shape file
url_zip <- 'https://dl.dropboxusercontent.com/s/chyvmlrkkk4jcgb/school_distr.zip'
if(!file.exists('school_distr.zip')) download.file(url_zip, 'school_distr.zip')     # download file as zip
unzip('school_distr.zip')   # unzip in the default folder
schdstr_shp <- readShapePoly('school.shp')  # read shape file
xlim <- schdstr_shp@bbox[1,]
ylim <- schdstr_shp@bbox[2,]


# download and load the crime csv data
#   link is https://dl.dropboxusercontent.com/s/4hg5ffdds9n2nx3/baltimore_crime.csv

url_zip <- 'https://dl.dropboxusercontent.com/s/4hg5ffdds9n2nx3/baltimore_crime.csv'
if(!file.exists('baltimore_crime.csv')) download.file(url_zip, 'baltimore_crime.csv')
df <- read.csv('baltimore_crime.csv', stringsAsFactors = F) 

# transform dates and time variables depending on what you need

df$CD = as.Date(df$CrimeDate, format = '%m/%d/%Y')
# df$CT = as.Date(df$CrimeTime, format = '%H:%M:%S')
df$month = as.numeric(format(df$CD, '%m'))
df$day = as.numeric(format(df$CD, '%d'))

# split coordinates into longitude and latitude, both as numeric

location.splt = strsplit(df$Location1, ',')
# split.and.extract = function(x, n) unlist(sub(c('\\(','\\)'),'',x[n]))

split.and.extract_1 = function(x) unlist(sub('\\(','',x[1]))
split.and.extract_2 = function(x) unlist(sub('\\)','',x[2]))

df$latitude = sapply(location.splt, function(x) split.and.extract_1(x))
df$longtitude = sapply(location.splt, function(x) split.and.extract_2(x))
df$latitude = as.numeric(df$latitude)
df$longtitude = as.numeric(df$longtitude)

# generate geographic and time patterns for crimes with keyword "ASSAULT"

df$assult = grepl('ASSAULT', df$Description)
df_plot = df[df$assult == T,]
df_plot$hour = substr(df_plot$CrimeTime, start = 1, stop = 2)
df_plot$hour = as.numeric(df_plot$hour)
# 6-11 12-17 18-23 0-5 >= & <=
par(mfrow = c(2, 2))
for (i in 0:3){
  plot(schdstr_shp, axes = T, main = paste0('hours:', i*6, '-', i*6+6))
  rows = df_plot$hour>=i*6 & df_plot$hour<=i*6+5
  points(df_plot[rows, 'longtitude'],df_plot[rows, 'latitude'], type = "p", col = rgb(1, 0, 0, 0.05), cex = 0.3)
}


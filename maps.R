

########
# SET WORKING DIRECTORY
dir <- '/Users/randyparr/Cloud/Documents/Important/School/Emory/HERT/map_email'
setwd(dir)
options(stringsAsFactors = FALSE)


########
# LIBRARIES

# match country names between datasets
library(countrycode) 

# plotting spatial data
library(ggplot2)
library(rnaturalearth)
library(sf)
# 'sf' requires package 'units'; 
# installation help for 'units' package: https://github.com/r-quantities/units

# palette
library(scales)


########
# DATA SET UP

# read in region-country data
reg_con <- read.table(
	'region_country.txt', 
	header = TRUE, 
	sep = '\t',
	fileEncoding = 'UTF-8', 
	quote = '')

# get 3 letter code for countries
reg_con$iso <- countrycode(
	reg_con$country, 
	origin = 'country.name', 
	destination = 'iso3c')

# get worldmap spatial data from rnaturalearth
worldmap <- ne_countries(
	scale = 'medium', 
	type = 'countries', 
	returnclass = 'sf')

# merge reg and worldmap on country code
dat <- merge(
	worldmap, 
	reg_con, 
	by.x = 'iso_a3', 
	by.y = 'iso', 
	all = TRUE)

# list of unique regions
regions <- sort(unique(reg_con$who_region))

# set colors
gray <- '#c4c4c4'
blue <- '#0072B2'

# set palette
world_pal <- setNames(
	hue_pal(l = 70)(6)[c(5,1,2,6,3,4)], 
	regions)

# function to get palette for region mapping
region_pal <- function(region){
	other_regions <- setdiff(regions, region)
	other_pal <- rep(gray, length(other_regions))
	reg_pal <- setNames(c(blue, other_pal), c(region, other_regions))
	return(reg_pal)
}


########
# WORLD MAP

# plot world map
world_p <- ggplot(data = dat) + 
	geom_sf(
		aes(fill = who_region), 
		lwd = 0.1, 
		color = 'white') + 
	theme_void() + 
	scale_fill_manual(
		values = world_pal, 
		na.value = gray) +
	theme(legend.position = 'none')

world_p

# save world map
ggsave('world_map.png', world_p)


########
# REGION MAPS

# read in data of rectangle coordinates
# coordinates obtained using: https://boundingbox.klokantech.com/
reg_loc <- read.table('reg_loc.txt', 
	header = TRUE, 
	sep = '\t',
	fileEncoding = 'UTF-8', 
	quote = '')

# function to get region map
region_map <- function(region){
	zoom <- reg_loc[reg_loc$region == region,]

	ggplot(data = dat) + 
		geom_sf(
			aes(fill = who_region), 
			lwd = 0.1, 
			color = 'white') +
		coord_sf(
			xlim = c(zoom$lon_min, zoom$lon_max), 
			ylim = c(zoom$lat_min, zoom$lat_max), 
			expand = FALSE) +
		theme_void() + 
		scale_fill_manual(
			values = region_pal(region), 
			na.value = gray) +
		theme(
			legend.position = 'none', 
			plot.margin = grid::unit(c(0,0,0,0), 'mm'))
}

# view region maps
region_map('Africa')
region_map('Americas')
region_map('Eastern Mediterranean')
region_map('Europe')
region_map('South-East Asia')
region_map('Western Pacific')

# save all region maps
for(region in regions){
	region_map(region)
	ggsave(paste0(region, '_map.png'))
}




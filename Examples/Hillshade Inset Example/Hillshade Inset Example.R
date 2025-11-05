# The following will be a tutorial on how to create hillshade and inset maps in R
# This requires the following::
# DEPENDENCIES
library(raster)
library(sf)
library(terra)
library(here)
library(colorspace)

# ------------------------------------------------------------------------------------------------
# Load in shapefile for inset
CA_state <- st_read(here::here('Data','Shapefiles','CA_State.shp'),
                    quiet = TRUE)
# ------------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------------------
# load in raster
# important to load this in as a raster object and not a rast object
# create hillshade raster (values -1 - 1) using raster package
DEM <- raster(here::here('Data','DEM','USGS_13_n42w123_20231102.tif'))
hillshade <- raster::hillShade(slope = terrain(DEM, opt = ('slope')), # angle of dem slope
                               aspect = terrain(DEM, opt = ('aspect')), # direction slopes are facing
                               angle = 45, # angle of sun in sky
                               direction = 315) # angle of light ray origin
# ------------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------------------
# this portion is only important if there are no other polygons to plot
# plotting a polygon before the raster will allow better control of the plot axes
# as a raster manipulates the pixels of the image itself, indexing the window to the bottom left
# not including margins
# while plotting a sf object includes margins and functions more like a traditional xy plot
hillshade_extent <- ext(hillshade)
hillshade_extent <- st_polygon(list(cbind(c(hillshade_extent[1],
                                            hillshade_extent[1],
                                            hillshade_extent[2],
                                            hillshade_extent[2],
                                            hillshade_extent[1]),
                                          c(hillshade_extent[3],
                                            hillshade_extent[4],
                                            hillshade_extent[4],
                                            hillshade_extent[3],
                                            hillshade_extent[3]))))
hillshade_extent <- st_sfc(hillshade_extent)
st_crs(hillshade_extent) <- 4326
# ------------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------------
# hillshade palette for visibility will default plot as terrain colors (annoying)
grays_palette <- sequential_hcl(n = 1000, palette = 'grays')
# ------------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------------------
# important to save to separate window for the purpose of inset placing
# inset will vary with device size, so changing saved plot size will change inset position
png(filename = here::here('Hillshade Example.png'),
    res = 300, width = 8, height = 9, units = 'in')
# ------------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------------------
# Making the axes, which will tightly hug the hillshade unlike if we were to set axes == T
# when we plot the base polygon
bb <- st_bbox(hillshade_extent)
bb_buff <- 0.01
x_seq <- seq(from = bb["xmin"] + bb_buff,
             to = bb["xmax"] - bb_buff,
             length.out = length(pretty(c(bb["xmin"], bb["xmax"]))))
y_seq <- seq(from = bb["ymin"] + bb_buff,
             to = bb["ymax"] - bb_buff,
             length.out = length(pretty(c(bb["ymin"], bb["ymax"]))))
# ------------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------------
# main plot
plot(st_geometry(hillshade_extent),
     axes = F) # turn automatic axes off
plot(hillshade,
     col = grays_palette,
     legend = F,
     add = T)
plot(st_geometry(hillshade_extent),
     add = T)
# ------------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------------------
# extent of the plotting window in real, not device, coordinates
# will be used for nice title and axis label placement
usr_device_coordinates <- par('usr')
x_range <- usr_device_coordinates[2] - usr_device_coordinates[1]
y_range <- usr_device_coordinates[4] - usr_device_coordinates[3]
# ------------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------------
# right edge as x axis
axis(1,
     at = x_seq[1:length(x_seq)],
     labels = round(x_seq[1:length(x_seq)],2),
     pos = bb["ymin"],
     lwd = 0,
     lwd.ticks = 1)

# left edge as y-axis
axis(2,
     at = y_seq[2:length(y_seq)],
     labels = round(y_seq[2:length(y_seq)],2),
     pos = bb["xmin"],
     lwd = 0,
     lwd.ticks = 1)
# ------------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------------
# title placement, will take some adjusting
text(x = usr_device_coordinates[1] + x_range*0.12,
     y = usr_device_coordinates[4] + y_range*0.03,
     labels = 'MAIN TITLE',
     pos = 4,
     cex = 1.6,
     xpd = NA)
text(x = usr_device_coordinates[1] + x_range*0.12,
     y = usr_device_coordinates[4],
     labels = 'SUB TITLE',
     cex = 1,
     pos = 4,
     col = 'gray45',
     xpd = NA)
text(x = usr_device_coordinates[1] + x_range/2,
     y = usr_device_coordinates[3] - y_range*0.06,
     labels = 'Easting',
     cex = 1.2,
     xpd = NA,
     col = 'gray20')
text(x = usr_device_coordinates[1] - x_range*0.03,
     y = usr_device_coordinates[3] + y_range/2,
     labels = 'Northing',
     srt = 90,
     cex = 1.2,
     xpd = NA,
     col = 'gray20')
# ------------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------------
# new inset coordinates
# IMPORTANT:
# Because we have plotted a raster image, the coordinate 0,
# is no longer the bottom left of the plot window, but of the whole device
# so we must manually adjust xleft and ybot until they align with our plot window
# all new objects will go to inset
# no longer possible to plot to main window
par(fig = c(0.112, # xleft
            0.4, # xright
            0.0293, # ybot
            0.4), # ytop
    new = TRUE) # new figure within same device
plot(st_geometry(st_transform(CA_state,4326)))
plot(st_geometry(hillshade_extent), col = 'red', add = T)
box()
# ------------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------------
# turn off device, clean up environment
invisible(dev.off())
invisible(gc())
# ------------------------------------------------------------------------------------------------
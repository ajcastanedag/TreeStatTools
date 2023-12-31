# Load Libraries
pacman::p_load(dplyr,sf,ggplot2, mapview, st, units, REdaS)

# Set WD
Root <- "" %>% setwd()

# Import GPKG 
Trees <- st_read("test_trees_diam.gpkg")

# Create centroids
centroids <- st_centroid(Trees) 

# Count total of shapes
TotalTrees <- length(centroids[[1]])

# Precompute centroid coordinates
centroid_coords <- st_coordinates(centroids)

# Initialize vectors to store results
min_lengths <- numeric(TotalTrees)
med_lengths <- numeric(TotalTrees)
mean_lengths <- numeric(TotalTrees)
max_lengths <- numeric(TotalTrees)

# Create progress bar
pb <- txtProgressBar(min = 1, max = TotalTrees, style = 3)

# Loop over all available tree shapes
for (i in 1:TotalTrees) {
  
  # Convert polygon into points
  PolPoints <- st_cast(Trees[i,], "POINT")
  
  # Initialize variables to store lengths
  Lengthvector <- numeric(length(PolPoints[[1]]))
  
  # Loop over all polygon points to create the lines between centroid and polygon edges
  for (j in 1:length(PolPoints[[1]])) {
    # Define a set of coordinates to create line from the two points
    coordinates <- matrix(c(
      st_coordinates(PolPoints[j,][1])[1], centroid_coords[i,][1],
      st_coordinates(PolPoints[j,][1])[2], centroid_coords[i,][2]
    ), ncol = 2)
    
    # Create an sf LineString object
    line <- st_linestring(coordinates) %>% st_sfc() %>% st_set_crs(st_crs(centroids))
    
    # Calculate length of line
    Length <- st_length(line)
    
    # Store the length in the vector
    Lengthvector[j] <- Length
  }
  
  # Calculate fields from the lines and store them in vectors
  min_lengths[i] <- round(min(Lengthvector), 2)
  med_lengths[i] <- round(median(Lengthvector), 2)
  mean_lengths[i] <- round(mean(Lengthvector), 2)
  max_lengths[i] <- round(max(Lengthvector), 2)
  
  # Increment progress bar
  setTxtProgressBar(pb, i)
  
  # Close progress bar when done
  if(i == TotalTrees){
    close(pb)
  }
}

# Assign the vectors to the centroids data frame
centroids$min <- min_lengths
centroids$med <- med_lengths
centroids$mean <- mean_lengths
centroids$max <- max_lengths

#Visual inspection
mapview(Trees) +
  mapview(centroids, col.regions = "green") 


# # Loop over all available tree shapes
# for(i in 1:TotalTrees){
#   
#   # Convert polygon into points
#   PolPoints <- st_cast(Trees[i,],"POINT")
#   
#   # Create rotated line object
#   combined_lines <- st_sfc(geometry = st_sfc()) %>% st_set_crs(st_crs(centroids))
#   
#   # Create a new vector to store lengths
#   Lengthvector <- vector()
#   
#   # Preclaculate centroid coordinate
#   CentroidCoordX <- st_coordinates(centroids[i,])[1]
#   CentroidCoordY <- st_coordinates(centroids[i,])[2]
#   
#   # Loop over all poligon points to create the lines between centroid and polygon edges
#   for(j in 1:length(PolPoints[[1]])){
#     
#     # Define a set of coordinates to create line from the two points
#     coordinates <- matrix(c(
#       c(st_coordinates(PolPoints[j,])[1], CentroidCoordX),
#       c(st_coordinates(PolPoints[j,])[2], CentroidCoordY)
#     ), ncol = 2)
#     
#     # Create an sf LineString object
#     line <- st_linestring(coordinates) %>% st_sfc() %>% st_set_crs(st_crs(centroids))
#     
#     # Calculate length of line
#     Length <- st_length(line)
#     
#     # Combine the rotated LINESTRING objects into a single sfc object
#     combined_lines <- st_combine(c(combined_lines, line))
#     
#     # Add up the lengths in the vector
#     Lengthvector <- c(Lengthvector,c(Length))
#     
#   }
#   
#   # Calculate fields from the lines and store them on the respective centroid 
#   centroids$min[i] <- round(min(Lengthvector),2)
#   centroids$med[i] <- round(median(Lengthvector),2)
#   centroids$mean[i] <- round(mean(Lengthvector),2)
#   centroids$max[i] <- round(max(Lengthvector),2)
#   
#   # Increment progress bar
#   setTxtProgressBar(pb, i)
#   
#   # Close progress bar when done
#   if(i == TotalTrees){
#     close(pb)
#   }
#   
# }

###############################################################################
###############################################################################

# Select tree Id (to do loop afterwards
TreeID <- 4

# Convert polygon into points
PolPoints <- st_cast(Trees[TreeID,],"POINT")

# Create rotated line object
combined_lines <- st_sfc(geometry = st_sfc()) %>% st_set_crs(st_crs(centroids))

# Create a new vector to store lengths
Lengthvector <- vector()

# Loop over all poligon points to create the lines between centroid and polygon edges
for(i in 1:length(PolPoints[[1]])){
  
  # Define a set of coordinates to create line from the two points
  coordinates <- matrix(c(
    c(st_coordinates(PolPoints[i,])[1], st_coordinates(centroids[TreeID,])[1]),
    c(st_coordinates(PolPoints[i,])[2], st_coordinates(centroids[TreeID,])[2])
  ), ncol = 2)
  
  # Create an sf LineString object
  line <- st_linestring(coordinates) %>% st_sfc() %>% st_set_crs(st_crs(centroids))
  
  # Calculate length of line
  Length <- st_length(line)
  
  # Combine the rotated LINESTRING objects into a single sfc object
  combined_lines <- st_combine(c(combined_lines, line))
  
  #
  Lengthvector <- c(Lengthvector,c(Length))
  
}

#Visual inspection
mapview(Trees[TreeID,]) +
  mapview(PolPoints, col.regions = "green") +
  mapview(centroids[TreeID,], col.regions = "red") +
  mapview(combined_lines)

# Convert vector to dataframe
LengthDF <- data.frame(Length = Lengthvector)

# Plot geom density
ggplot(LengthDF, aes(x=Length)) +
  geom_density(aes(fill = "red"), color = "black", alpha=0.3, lwd = .5, show.legend = F) +
  geom_vline(aes(color=paste0("Mean:",   round(mean(Lengthvector),2)) , xintercept=mean(Lengthvector)), linetype="solid",  size=1.0, show.legend = NA)+
  geom_vline(aes(color=paste0("Median:",   round(median(Lengthvector),2)) , xintercept=median(Lengthvector)), linetype="solid",  size=1.0, show.legend = NA)+
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(color = "Stats")

Length_Tree<-mean(LengthDF$Length)


# 
# 
# 
# 
# # Duplicate point and move upwards
# newpointUp <- centroids[TReeID,]
# newpointDown <- centroids[TReeID,]
# newpointUp[1,]$geometry[[1]][2] <- newpointUp[1,]$geometry[[1]][2] + 50
# newpointDown[1,]$geometry[[1]][2] <- newpointDown[1,]$geometry[[1]][2] - 50
# 
# # Define a set of coordinates to create line from the two points
# coordinates <- matrix(c(
#   c(st_coordinates(newpointUp)[1], st_coordinates(newpointDown[1,])[1]),
#   c(st_coordinates(newpointUp)[2], st_coordinates(newpointDown[1,])[2])
# ), ncol = 2)
# 
# # Create an sf LineString object
# line <- st_linestring(coordinates) %>% st_sfc() %>% st_set_crs(st_crs(centroids))
# 
# # Define rotation function
# rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2) 
# 
# # Create rotated line object
# combined_lines <- line
# 
# # Rotate line by 5
# for (i in seq(5, 355, by = 5)) {
#   
#   linerot <- ((line - st_centroid(line)) * rot(deg2rad(i)) + st_centroid(line)) %>%
#     st_sfc() %>% st_set_crs(st_crs(centroids))
#   
#   # Combine the rotated LINESTRING objects into a single sfc object
#   combined_lines <- st_combine(c(combined_lines, linerot))
#   
# }
# 
# # Intersect objects
# intersect <- st_intersection(combined_lines, Trees[TReeID,])
# 
# #Visual inspection
# mapview(Trees) + mapview(centroids) + mapview(intersect) 
# 
# # Create a new vector
# Lengthvector <- vector()
# 
# for(i in 1:length(intersect[[1]])){
#   TemporalLIne <- st_linestring(intersect[[1]][[i]]) %>% st_sfc() %>% st_set_crs(st_crs(centroids))
#   Length <- st_length(TemporalLIne)
#   Lengthvector <- c(Lengthvector,c(Length))
# }
# 
# # Convert vector to dataframe 
# LengthDF <- data.frame(Length = Lengthvector)
# 
# # Plot geom density
# ggplot(LengthDF, aes(x=Length)) +
#   geom_density(aes(fill = "red"), color = "black", alpha=0.3, lwd = .5, show.legend = F) +
#   geom_vline(aes(color=paste0("Mean:",   round(mean(Lengthvector),2)) , xintercept=mean(Lengthvector)), linetype="solid",  size=1.0, show.legend = NA)+
#   geom_vline(aes(color=paste0("Median:",   round(median(Lengthvector),2)) , xintercept=median(Lengthvector)), linetype="solid",  size=1.0, show.legend = NA)+
#   scale_x_continuous(expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0)) +
#   labs(color = "Stats")

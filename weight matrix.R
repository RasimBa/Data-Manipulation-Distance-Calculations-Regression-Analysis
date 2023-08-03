# Load the necessary library
library(spdep)
library(sf)
library(sp)
 

house_data_sample2 <- house_data_sample %>% 
  sample_n(5000, replace = FALSE)
 
house_data_sample2 <- st_transform(house_data_sample2, crs = 25832)

st_crs(house_data_sample2)
 
# Check for empty geometries
empty_geoms <- st_is_empty(house_data_sample2$geometry)

# Subset to remove empty geometries
house_data_sample2 <- house_data_sample2[!empty_geoms, ]

#creating fixed buffer at the range of 0-4000m to consider all observations
#within this buffer as a neighbor of each others

neighbors_nb <- dnearneigh(st_coordinates(house_data_sample2), d1 = 0, d2 = 4000,
                           longlat = FALSE)


# Convert the neighbor list to a row-standardized weights list
weights_list <- nb2listw(neighbors_nb, style = "W" , zero.policy = TRUE)

 


model_lag1 <- lagsarlm(log(adj_sellprice) ~ condition +  
                         PopulationDensity +
                         SettlementDensityPerKm +  EmploymentDensity_WO +
                         WorkplaceCentrality +
                         living_space + land_area + UpperCentreAccessibility +
                         factor(category_house) +
                         factor(parkplace) +  AirportAccessibility +
                         number_floors + factor(CityType) + 
                         DistanceToNearestRailway + DistanceToNearestCityCenter + 
                         PoliceStations +  railway_proximity 
                         + Unemployment + HighwayAccessibility + factor(Sea_dummy) +
                         distance_bands +
                         factor(heating_type), 
                       data = house_data_sample2, listw = weights_list,
                       method = "eigen", zero.policy = TRUE)



summary(model_lag1)


print(weights_list)


summary_model <- summary(model)
str(summary_model)


# Assuming 'model' is your fitted SAR object
summary_model <- summary(model_lag1)

# Extract coefficients, standard errors, and significance levels
coefficients <- summary_model$coefficients

# Extract the coefficients and p-values
coefficients_with_stars <- as.data.frame(summary_model$coefficients)
p_values <- coefficients_with_stars$`Pr(>|t|)`

# Add a new column with stars based on p-values
coefficients_with_stars$stars <- ifelse(p_values < 0.001, '***',
                                        ifelse(p_values < 0.01, '**',
                                               ifelse(p_values < 0.05, '*',
                                                      ifelse(p_values < 0.1, '.', ''))))

# If you want to print the coefficients along with stars, you can use:
print(coefficients_with_stars)





#------------------------------------------------------------------------------
#------------------------------------------------------------------------------


#The second method of creating weights matrix; so that, finding 5 nearest 
#neighbors of each observation, creating inverse distance matrix W = 1/distance
#as distance increase between observations, in contrast weight matrix W decrease
# The main problem of computing k nearest neighbors function is its unique
#coordinate requirement; which is not applicable in my case, since I dont have
#exact coordinates of houses instead the centroid of 1x1km grid. And there 
#are several houses that have identical coordinates because they locate in the 
#same grid: The solution was to add very small random noise to coordinates of 
#observations.

house_data_sample3 <- house_data_sample %>% 
  sample_n(5000, replace = FALSE)

# Extract the data frame component of the sf object
data_frame_component <- st_set_geometry(house_data_sample3, NULL)

# Find the rows with complete cases
complete_cases <- complete.cases(data_frame_component)

# Apply the complete cases to the original sf object
house_data_sample3 <- house_data_sample3[complete_cases, ]

house_data_sample3 <- st_transform(house_data_sample3, crs = 25832)

# Check for empty geometries
empty_geoms <- st_is_empty(house_data_sample3$geometry)

# Subset to remove empty geometries
house_data_sample3 <- house_data_sample3[!empty_geoms, ]

# Convert sf object to SpatialPointsDataFrame
spatial_points <- as(house_data_sample3, "Spatial")

# Extract the unique coordinates
coords <- coordinates(spatial_points)

# Ensure the coordinates are unique
coords <- coords + matrix(runif(nrow(coords)*2, -0.00000001, 0.00000002), ncol = 2)
 
# Identify 5 nearest neighbours
knn <- knn2nb(knearneigh(coords, k = 5))

# Create a weights list from the neighbours list
weights_list <- nb2listw(knn, style = "W", zero.policy = TRUE)

# Compute distance matrix
dist_matrix <- as.matrix(dist(coords))

# Compute inverse-distance matrix
# Add a small constant to avoid division by zero
inverse_dist_matrix <- 1 / (dist_matrix + 1e-10)

# Create a binary matrix from the neighbors list
neighbors_matrix <- nb2mat(knn, style = "W", zero.policy = TRUE)

# Multiply the binary neighborhood matrix by the inverse-distance matrix
# This will set the weights of non-neighbors to zero
weights_matrix <- neighbors_matrix * inverse_dist_matrix

# Convert the weights matrix to a list
weights_listw <- mat2listw(weights_matrix, style = "W")

print(weights_listw)
# Fit spatial lag model
model_lag2 <- lagsarlm(log(adj_sellprice) ~ condition +  
                         PopulationDensity +
                         SettlementDensityPerKm +  EmploymentDensity_WO +
                         WorkplaceCentrality +
                         living_space + land_area + UpperCentreAccessibility +
                         factor(category_house) +
                         factor(parkplace) +  AirportAccessibility +
                         number_floors + factor(CityType) + 
                         DistanceToNearestRailway + DistanceToNearestCityCenter + 
                         PoliceStations +   railway_proximity +
                         + Unemployment + HighwayAccessibility + factor(Sea_dummy) +
                         distance_bands +
                         factor(heating_type)  ,
                       data = house_data_sample3, listw = weights_listw,
                       method = "eigen", zero.policy = TRUE)


summary(model_lag2)


 



 
#-----------------------------------------------------------------------------
#----------------------------------------------------------------------------



##test if i would add noise directly to the longi, and lati 

coords <- st_coordinates(house_data_sample3$geometry)

house_data_sample3$Longitude <- coords[, "X"]
house_data_sample3$Latitude <- coords[, "Y"]


# Add small random noise to the coordinates
set.seed(123)
random_noise <- runif(nrow(house_data_sample3), -0.000001, 0.000001)
 
 
# Ensure that Longitude and Latitude are numeric
house_data_sample3$Longitude <- as.numeric(house_data_sample3$Longitude)
house_data_sample3$Latitude <- as.numeric(house_data_sample3$Latitude)

house_data_sample3$Longitude <- house_data_sample3$Longitude + random_noise
house_data_sample3$Latitude <- house_data_sample3$Latitude + random_noise


# First, create a matrix of the coordinates
coords <- cbind(house_data_sample3$Longitude, house_data_sample3$Latitude)

# Convert coords to a SpatialPoints object
sp_pts <- SpatialPoints(coords)
 

 




#--------------------------------------------------------------------

####illustration of weight matrix construction in case of fixed distance



# Create squares
squares <- st_sfc(st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))),
                  st_polygon(list(rbind(c(1,0), c(2,0), c(2,1), c(1,1), c(1,0)))))

squares <- st_sf(squares)

# Create circles (buffers) around squares
circles <- st_buffer(st_centroid(squares), dist = 1)  # Increase buffer distance to 1

# Convert to data frames for ggplot
squares_df <- st_coordinates(squares) %>% as.data.frame()
circle1 <- st_cast(circles[1,], "LINESTRING") # First circle
circle2 <- st_cast(circles[2,], "LINESTRING") # Second circle
circles_df1 <- st_coordinates(circle1) %>% as.data.frame() # Circle 1
circles_df2 <- st_coordinates(circle2) %>% as.data.frame() # Circle 2

# Generate random points within squares and circles
set.seed(123)
n_points <- 20
points_in_squares <- st_sample(squares, n_points)
points_in_circles <- st_sample(circles, n_points * 3) %>% st_sf()  # Increase number of points in circles to account for larger area

# Get the points that are in circles but not in squares
points_not_in_squares <- st_difference(points_in_circles, st_union(squares))

# Convert to data frame
points_in_circles_df <- st_coordinates(points_not_in_squares) %>% as.data.frame()

# Convert to data frames for ggplot
points_in_squares_df <- st_coordinates(points_in_squares) %>% as.data.frame()

# Get the coordinates of the centers of the squares
centers <- st_centroid(squares)
centers_df <- data.frame(st_coordinates(centers))
centers_df$label <- c("Grid A", "Grid B")



# Plot
ggplot() +
  geom_polygon(data = squares_df, aes(X, Y), fill = NA, color = "black") +
  geom_path(data = circles_df1, aes(X, Y), color = "red") + # Circle 1
  geom_path(data = circles_df2, aes(X, Y), color = "blue") + # Circle 2
  geom_point(data = points_in_squares_df, aes(X, Y), color = "black") +
  geom_point(data = points_in_circles_df, aes(X, Y), color = "black") +
  geom_text(data = centers_df, aes(X, Y, label = label), size = 5, color = "darkgreen") + # Add labels
  coord_equal() +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank())





#------------------------------------------#



#using inverse distance weighting with a fixed distance matrix involves
#giving more weight to observations that are closer and less weight to those 
#that are farther away, up to a specified fixed distance. Maybe that is better
#for my case , because i can add some noise to the coordinates, then for example
#within 2km of buffer more weights will be all houses that are within 1x1km^2 
#grid, however at least i can count for houses outside the grid after 1km
## 

model <- lm(log(adj_sellprice) ~ condition +  
                         PopulationDensity +
                         SettlementDensityPerKm +  EmploymentDensity_WO +
                         WorkplaceCentrality +
                         living_space + land_area + UpperCentreAccessibility +
                         factor(category_house) +
                         factor(parkplace) +  AirportAccessibility +
                         number_floors + factor(CityType) + 
                         DistanceToNearestRailway + DistanceToNearestCityCenter + 
                         PoliceStations +  railway_proximity 
                       + Unemployment + HighwayAccessibility + factor(Sea_dummy) +
                         distance_bands +
                         factor(heating_type), 
                       data = house_data_sample2)



summary(model)






#---------------------------------------
#------------------------

library(modelsummary)
install.packages("modelsummary")
library(kableExtra)
 library(gt)
library(broom)


ols_coeff <- tidy(model)
ols_coeff$stars <- symnum(ols_coeff$p.value, corr = FALSE, na = FALSE,
                          cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                          symbols = c("***", "**", "*", ".", " "))

spatial_coeff <- tidy(model_lag1)
spatial_coeff$stars <- symnum(spatial_coeff$p.value, corr = FALSE, na = FALSE,
                              cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                              symbols = c("***", "**", "*", ".", " "))


coeff_summary <- rbind(OLS = ols_coeff, Spatial = spatial_coeff)

final_output <- bind_rows(coeff_summary, final_summary)

library(kableExtra)
kable_output <- final_output %>%
  kable("html") %>%
  kable_styling()

# ... and so on

temp_file <- tempfile(fileext = ".html")
writeLines(as.character(kable_output), temp_file)

install.packages("webshot")
webshot::install_phantomjs()

webshot::webshot(temp_file, "summary_tablne.png")

 
#----------------------------------------
#----------------------------------------------------------


ols_data <- data.frame(
  Residual_standard_error = sigma(model),
  Multiple_R_squared = summary(model)$r.squared,
  Adjusted_R_squared = summary(model)$adj.r.squared,
  F_statistic = summary(model)$fstatistic[1],
  F_statistic_p_value = pf(summary(model)$fstatistic[1],
                           summary(model)$fstatistic[2],
                           summary(model)$fstatistic[3],
                           lower.tail = FALSE)
)




spatial_data <- data.frame(
  Rho = spatial_summary$rho,
  LR_test_value = as.numeric(spatial_summary$LR1$statistic),
  LR_test_p_value = as.numeric(spatial_summary$LR1$p.value),
  Asymptotic_standard_error = spatial_summary$rho.se,
  Wald_statistic = spatial_summary$Wald1$statistic,
  Log_likelihood = as.numeric(spatial_summary$LL),
  ML_residual_variance = spatial_summary$s2,
  Number_of_observations = length(spatial_summary$residuals),
  Number_of_parameters_estimated = spatial_summary$parameters,
  AIC = 2 * spatial_summary$parameters - 2 * as.numeric(spatial_summary$LL),
  LM_test_value = spatial_summary$LMtest, # This may need to be adjusted if LMtest has a specific structure
  LM_test_p_value = NA # Add the appropriate extraction for the LM test p-value if available
)

str(spatial_summary)


library(kableExtra)

# Add missing columns to ols_data with NA values
missing_columns_in_ols <- setdiff(names(spatial_data), names(ols_data))
ols_data[missing_columns_in_ols] <- NA

# Add missing columns to spatial_data with NA values
missing_columns_in_spatial <- setdiff(names(ols_data), names(spatial_data))
spatial_data[missing_columns_in_spatial] <- NA

# Reorder columns to match
ols_data <- ols_data[, names(spatial_data)]
spatial_data <- spatial_data[, names(ols_data)]

# Combine the data frames
final_summary <- rbind(OLS = ols_data, Spatial = spatial_data)





final_summary <- rbind(OLS = ols_data, Spatial = spatial_data)
kable_output <- final_summary %>%
  kable("html") %>%
  kable_styling()

# Save the HTML content
temp_file <- tempfile(fileext = ".html")
writeLines(as.character(kable_output), temp_file)

# Convert HTML to PNG
webshot::webshot(temp_file, "summary_table.png")
 
webshot::install_phantomjs()



#-----------------------------------------------------
#---------------------------------------------------------

# Extracting coefficients and standard errors
ols_summary <- summary(model)
lag_summary <- summary(model_lag1)

ols_coeff <- coef(ols_summary)
ols_se <- coef(summary(model))[, "Std. Error"]

lag_coeff <- coef(lag_summary)
lag_se <- coef(summary(model_lag1))[, "Std. Error"]

# Combine into a data frame
coeff_data <- data.frame(
  Variable = names(ols_coeff),
  OLS_Coeff = ols_coeff,
  OLS_SE = ols_se,
  Lag_Coeff = lag_coeff,
  Lag_SE = lag_se
)

# Extract other statistics
stats_data <- data.frame(
  Statistic = c("Rho", "Log likelihood", "AIC", "Adjusted R2"),
  OLS = c(NA, NA, AIC(model), ols_summary$adj.r.squared),
  Lag = c(model_lag1$rho, lag_summary$logLik, AIC(model_lag1), lag_summary$adj.r.squared)
)

library(kableExtra)

# Coefficients Table
coeff_table <- coeff_data %>%
  kable("html", align = "c") %>%
  kable_styling()

# Statistics Table
stats_table <- stats_data %>%
  kable("html", align = "c") %>%
  kable_styling()

# Combine Tables
final_table <- rbind(coeff_table, stats_table)


temp_file <- tempfile(fileext = ".html")
writeLines(as.character(final_table), temp_file)
webshot::webshot(temp_file, "summary_tabcle.png")

 
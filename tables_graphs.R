#............................DESCRIPTIVE STATISTICS...........................
# Install and load the summarytools package
#install.packages("summarytools")
library(summarytools)
# Install and load the psych package
#install.packages("psych")
library(psych)
library(broom)
library(ggplot2)


s_h <- house_data %>% 
  sample_n(5000, replace = FALSE)
s_tur <- wind_turbines_sf %>%
  sample_n(1500, replace = FALSE)

s_h <- st_transform(s_h, crs = 25832)
s_tur <- st_transform(s_tur, crs = 25832)



View(s_tur)

s_tur$install_date <- as.Date(dmy(s_tur$unit_date.install), 
                              "%Y-%m-%d")

# remove the original column with dd/mm/yyyy format
s_tur$unit_date.install <- NULL

# convert date columns to Date class
s_h$date_finish <- as.Date(s_h$date_finish, format="%Y%m%d")
s_tur$install_date <- as.Date(s_tur$install_date, format="%Y%m%d")


# Initialize an empty vector to store the nearest turbine_ids
nearest_turbine_ids <- vector("numeric", length = nrow(s_h))

# Initialize an empty vector to store the distances
distance <- vector("numeric", length = nrow(s_h))

# Loop over each house
for (i in seq_len(nrow(s_h))) {
  
  # Filter turbines that were installed before the house was sold
  valid_turbines <- s_tur[s_tur$install_date < s_h$date_finish[i], ]
  
  # Check if there are any valid turbines
  if (nrow(valid_turbines) > 0) {
    
    # Find the nearest valid turbine
    nearest_turbine <- st_nearest_feature(s_h[i, ], valid_turbines)
    
    # Get the ID of the nearest valid turbine
    nearest_turbine_ids[i] <- valid_turbines$turbine_id[nearest_turbine]
    
    # Get the distance to the nearest valid turbine
    distance[i] <- st_distance(s_h[i, ], valid_turbines[nearest_turbine, ])
    
  } else {
    
    # If there are no valid turbines, set the ID and distance to NA
    nearest_turbine_ids[i] <- NA
    distance[i] <- NA
  }
}

# Add the nearest turbine IDs and distances to the house data frame
s_h$turbine_id <- nearest_turbine_ids
s_h$distance <- distance
View(s_h) 

s_h$distance_bands <- cut(s_h$distance, breaks = 
                            c(0, 200, 500,1000, 3000, 6000, Inf), 
                          labels = c("0-200 m","200-500 m","500-1000 m", 
                                     "1000-3000 m", "3000-6000 m","over 6000 m"))
s_h$distance_bands <- relevel(s_h$distance_bands, 
                              ref = "3000-6000 m")



----------------------------------------------------------
  
#..............HISTOGRAM: percentage of turbines within each distance bands



#Calculate the percentage of wind turbines in each proximity category
turbine_counts <- table(s_h$distance_bands)
turbine_percentages <- prop.table(turbine_counts) * 100

# Create a data frame for the plot
plot_data <- data.frame(proximity = names(turbine_percentages),
                        percentage = as.numeric(turbine_percentages))

# Convert 'proximity' to an ordered factor in order to ordering x axis 
#ascending 
plot_data$proximity <- factor(plot_data$proximity, 
                              levels = c("0-200 m","200-500 m","500-1000 m", 
                                         "1000-3000 m", "3000-6000 m","over 6000 m"),
                              ordered = TRUE)

# Define a color palette
my_colors <- brewer.pal(6, "Set3")

# Create the bar plot
ggplot(plot_data, aes(x = proximity, y = percentage, fill = proximity)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.3, size = 3) +
  labs(x = "Proximity to Houses",
       y = "Percentage of Wind Turbines",
       title = "Distribution of Wind Turbines by Proximity to Houses") +
  scale_fill_manual(values = my_colors) +
  theme_light() +
  theme(legend.position = "none",  # Removes redundant legend
        plot.title = element_text(hjust = 0.5),  # Center the plot title
        axis.title = element_text(size = 12, face = "bold"),  # Format axis titles
        axis.text = element_text(size = 10))  # Format axis text


  
##alternative graph
-------------------------------------------------------------
  # Load required packages
  library(ggplot2)
library(RColorBrewer)

# Define a color palette
my_colors <- brewer.pal(6, "Set3")

# Create lollipop chart
ggplot(plot_data, aes(x = proximity, y = percentage)) +
  geom_segment(aes(x = proximity ,xend = proximity, y = 0 , yend = percentage), 
               color = "grey", linewidth = 0.5) +
  geom_point(aes(color = proximity), size = 4) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, size = 3.5) +
  labs(x = "Proximity to Houses",
       y = "Percentage of Wind Turbines",
       title = "Distribution of Wind Turbines by Proximity to Houses") +
  scale_color_manual(values = my_colors) +
  theme_light() +
  theme(legend.position = "none",  # Removes redundant legend
        plot.title = element_text(hjust = 0.5),  # Center the plot title
        axis.title = element_text(size = 12, face = "bold"),  # Format axis titles
        axis.text = element_text(size = 10))  # Format axis text


-----------------------------------------
  

#.......Create a graph: house selling price vs distance bands...............

 
ggplot(s_h, aes(x=distance_bands, y=adj_sellprice)) + 
  geom_point() +
  geom_smooth(method=lm, se=FALSE, color="red", linetype="dashed") +
  labs(x = "Distance Bands", y = "House Prices", 
       title = "Selling Price over Distance Bounds") +
  theme_minimal()


--------------------------------------------
  # Convert house prices to thousands
  s_h$adj_sellprice <- s_h$adj_sellprice / 1000

# Make sure the distance bands are ordered factor
s_h$distance_bands <- factor(s_h$distance_bands, 
                             levels = c("0-200 m","200-500 m","500-1000 m", 
                                        "1000-3000 m", "3000-6000 m","over 6000 m"),
                             ordered = TRUE)

drop_na(s_h)
 na.omit(s_h)
# Calculate mean house price for each distance band
 library(ggplot2)
 library(dplyr)
 
 
 # Calculate mean house price for each distance band
 mean_prices <- s_h %>% 
   group_by(distance_bands) %>% 
   summarise(mean_price = mean(adj_sellprice, na.rm = TRUE))

 
 # Create the plot
 p <- ggplot(s_h, aes(x = distance_bands, y = adj_sellprice)) +
   geom_jitter(color = "red", alpha = 0.2, width = 0.3, size = 1.2) +
   geom_crossbar(data = mean_prices, aes(y = mean_price, ymin = mean_price, ymax = mean_price),
                 color = "steelblue", width = 0.5) +
   scale_y_continuous("House Prices (in thousands)", labels = scales::comma, breaks = seq(50, 2500, by = 150)) +
   labs(x = "Distance Bands",
        title = "Distribution of House Prices over Distance Bands") +
   theme_minimal() +
   theme(panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(), 
         legend.position = "none")  # Remove grid and legend
 
 print(p)
 
 
--------------------------------------
   # Install packages if not installed
   if (!require(ggthemes)) install.packages("ggthemes")
 if (!require(ggrepel)) install.packages("ggrepel")
 
 library(ggthemes)
 library(ggrepel)
 
 # Create the plot
 p <- ggplot(s_h, aes(x = distance_bands, y = adj_sellprice)) +
   geom_jitter(color = "red", alpha = 0.3, width = 0.2, size = 1.1) +
   geom_crossbar(data = mean_prices, aes(y = mean_price, ymin = mean_price, ymax = mean_price),
                 color = "blue", width = 0.5) +
   scale_y_continuous("House Prices (in thousands)", labels = scales::comma, breaks = seq(0, 2500, by = 100)) +
   labs(x = "Distance Bands",
        title = "Distribution of House Prices over Distance Bands",
        caption = "Each point represents a house. The blue bars indicate the mean house price for each distance band.") +
   theme_tufte() + # Use a different theme
   theme(plot.caption = element_text(hjust = 0, face= "italic"),
         axis.title.x = element_text(face="bold"),
         axis.title.y = element_text(face="bold"),
         plot.title = element_text(face="bold")) + 
   scale_fill_brewer(palette = "Set2") # Different color palette
 
 # Save the plot with high resolution
 ggsave("hp_td.png", plot = p, width = 10, height = 8, dpi = 300)
 
 ---------------------------------------
   
 
#.......graphing house prices over each distance bandds:outlier visualization 


# Remove rows with NA values
s_h <- na.omit(s_h)

# Fit the linear regression model
model_r <- lm(log(adj_sellprice) ~   
                factor(condition) +  StandingWater_dummy + Sea_dummy + HarbourBasin_dummy+
                FlowingWater_dummy +
                living_space +
                land_area + 
                factor(parkplace) + 
                number_floors + factor(CityType) + DistanceToNearestRailway + 
                DistanceToNearestCityCenter + PoliceStations +
                factor(granny_flat) + Unemployment + HighwayAccessibility +
                DistanceToNearestAgriculturalArea,
              data = s_h)

# Get the residuals
residuals_data <- augment(model)

# Define a threshold for outliers.
threshold <- 2 * sd(residuals_data$.resid)

# Add a new variable to the data frame that indicates whether each point is an outlier
s_h$outlier <- ifelse(abs(residuals_data$.resid) > threshold, 
                             "Outlier", "Not Outlier")


ggplot(s_h, aes(x=distance_bounds, y=kaufpreis, color=outlier)) + 
  geom_point() +
  geom_smooth(method=lm, se=FALSE, color="red", linetype="dashed") +
  labs(x = "Distance Bounds", y = "Kaufpreis", title = "Selling Price over Distance Bounds") +
  theme_minimal()




#....................model prediction:visualization........................



# Predict the values using the model
s_h$predicted <- predict(model3, final_data)

# Plot the actual vs predicted values
ggplot(s_h, aes(x=adj_kaufpreis, y=predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Actual Values", y = "Predicted Values", 
       title = "Actual vs Predicted Values") +
  theme_minimal()






#...........Percentage of houses in each construction year category ........


#Calculate the percentage of houses in each baujahr category
house_counts <- table(final_data$baujahr_category)
house_percentages <- prop.table(house_counts) * 100

# Create a data frame for the plot
plot_data1 <- data.frame(construction_years = names(house_percentages),
                        percentage = as.numeric(house_percentages))

# Create the bar plot
ggplot(plot_data1, aes(x = construction_years, y = percentage, fill = 
                         construction_years)) +
  geom_bar(stat = "identity") +
  labs(x = "Construction Year Categories ",
       y = "Percentage of Houses",
       title = "Percentage of Houses in each Category") +
  scale_fill_manual(values = c("blue", "green", "red", "orange", "purple", "yellow")) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  theme_minimal()




 
#........."Distribution of House Prices by Construction Year Period......


# Create a boxplot...
ggplot(final_data, aes(x=baujahr_category, y=adj_kaufpreis/1000)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 5, 
               fill = "white") +
  labs(x = "Construction Year Period", y = "Adjusted Kaufpreis", 
       title = "Distribution of House Prices by Construction Year Period") +
  scale_y_continuous(breaks = seq(0, 5000, by = 100))
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
 

#......................
  # Calculate the mean, max, and min values for each category
  summary_stats <- final_data %>%
    group_by(baujahr_category) %>%
    summarise(mean_price = mean(adj_kaufpreis, na.rm = TRUE),
              max_price = max(adj_kaufpreis, na.rm = TRUE),
              min_price = min(adj_kaufpreis, na.rm = TRUE))

  # Reshape the data to a longer format
  summary_stats_long <- summary_stats %>%
    pivot_longer(cols = c(mean_price, max_price, min_price),
                 names_to = "statistic",
                 values_to = "price")
  
  # Create a line plot
  ggplot(summary_stats_long, aes(x = baujahr_category, y = price, color = statistic, group = statistic)) +
    geom_line() +
    geom_point() +
    labs(x = "Construction Year Period", y = "Adjusted Kaufpreis", 
         color = "Statistic",
         title = "Mean, Max, and Min House Prices by Construction Year Period") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  
  
  
####.......number of turbines installed over years......
  
  
  
  
  # Extract the year from install_date
  merged_data$install_year <- year(merged_data$install_date)
  
  # Count the number of turbines installed each year
  turbines_per_year <- merged_data %>%
    group_by(install_year) %>%
    summarise(num_turbines = n_distinct(turbine_id))
  
  # Create the plot
  ggplot(turbines_per_year, aes(x = install_year, y = num_turbines)) +
    geom_line() +
    geom_point() +
    labs(x = "Year", y = "Number of Turbines Installed", 
         title = "Number of Turbines Installed Over Years") +
    theme_minimal()
  
  
  
##......................number of turbines for each hub height category.....
  
  # Count the number of turbines for each hub_height_cat
  turbines_per_category <- final_data %>%
    group_by(hub_height_cat) %>%
    summarise(num_turbines = n_distinct(turbine_id))
  
  # Create the plot
  ggplot(turbines_per_category, aes(x = hub_height_cat, y = num_turbines)) +
    geom_bar(stat = "identity") +
    labs(x = "Hub Height Category", y = "Number of Turbines",
         title = "Number of Turbines in Each Hub Height Category") +
    theme_minimal()
  
  
 
  
  
#######..."Number of Turbines Installed Over Years at Each Distance Bands"..
  
  
  
  # Count the number of turbines installed each year at each distance_bands
  turbine_counts <- merged_data %>%
    group_by(year, distance_bounds) %>%
    summarise(turbine_count = n_distinct(turbine_id)) %>%
    ungroup()
  
  # Create line graph
  ggplot(turbine_counts, aes(x = year, y = turbine_count, color = distance_bounds)) +
    geom_line(size=1) +
    labs(x = "Year", y = "Number of Turbines Installed", 
         title = "Number of Turbines Installed Over Years at Each Distance Bounds",
         color = "Distance Bounds") +
    theme_minimal()
  
  ####number of houses based on proximity to wind turbines
  

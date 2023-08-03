
#...............Descriptive statistics wind turbines and houses.......................
library(ggplot2)
library(corrplot)
library(png)
#install.packages("patchwork")
library(patchwork)
library(knitr)
library(kableExtra)


#Summary table for house data

table1::label(dataaa_filtered$sellprice) <- "House Price"
table1::label(dataaa_filtered$adj_sellprice) <- "Adjused House Price" 
table1::label(dataaa_filtered$number_rooms) <- "Number of Rooms"
table1::label(dataaa_filtered$bathroom) <- "Number of Bathrooms"
table1::label(dataaa_filtered$number_floors) <- "Number of Floors"
table1::label(dataaa_filtered$bedroom) <- "Number of Bedrooms"
table1::label(dataaa_filtered$land_area) <- "Land Area (in square meters)"
table1::label(dataaa_filtered$living_space) <- "Living Space (in square meters)"
table1::label(dataaa_filtered$construction_year) <- "Construction Year"
#table1::label(dataaa_filtered$ads_duration) <- "Duration of Advertisement (in days)"


# Generate the table
table1::table1(~sellprice + number_rooms + adj_sellprice+ bathroom +
                 number_floors + bedroom + land_area +
                 living_space + construction_year   
               , data = dataaa_filtered)
# Generate the table with the desired variable order
my_table <- table1::table1(
  ~ sellprice +  adj_sellprice + number_rooms +bathroom +
    number_floors + bedroom + land_area +
    living_space + construction_year,
  data = dataaa_filtered
)

# Display the table
print(my_table)







#Filtering turbine data for some visualizations

#number of turbines installed each year  

turbines_by_year <- wind_turbines_sf %>%
  mutate(year = year(dmy(unit_date.install))) %>%
  group_by(year) %>%
  summarise(num_turbines = n())

average_count <- mean(turbines_by_year$num_turbines)

 

##Here I am trying to find the years with the number of installed turbines
#less than 15, and i will not include them into the graph

# Filter years where the number of installed turbines is less than 15
years_less_than_15 <- turbines_by_year %>%
  filter(num_turbines < 15)

# Print the years
print(years_less_than_15$year) ##in the years 1983, 88, 89, 90 there are less than
#15 turbines installed
 

###geom line

# Filter the data to include only the years starting from 1990
turbines_by_year_filtered <- turbines_by_year %>%
  filter(year >= 1990)

# Find the rows with the max and min number of turbines
max_turbines <- turbines_by_year_filtered[turbines_by_year_filtered$num_turbines == 
                                            max(turbines_by_year_filtered$num_turbines), ]
min_turbines <- turbines_by_year_filtered[turbines_by_year_filtered$num_turbines == 
                                            min(turbines_by_year_filtered$num_turbines), ]

ggplot(turbines_by_year_filtered, aes(x = year, y = num_turbines)) +
  geom_line(color = "grey") +
  geom_point(color = "black") +
  geom_point(data = max_turbines, color = "steelblue") +  
  geom_point(data = min_turbines, color = "red") +  
  geom_hline(yintercept = average_count, color = "orange", linetype = "dashed", size = 0.5) +
  labs(x = "Year", y = "Number of Turbines", title = "The Number of Turbines Installed Each Year in Germany") +
  scale_x_continuous(breaks = seq(1990, max(turbines_by_year_filtered$year), by = 3), limits = c(1990, max(turbines_by_year_filtered$year))) +
  scale_y_continuous(breaks = seq(0, max(turbines_by_year_filtered$num_turbines), by = 100)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


######..................turbine height and turbine rotor diameter.


colSums(is.na(wind_turbines_sf)) #hub.height 495, and d_rotor 281 missing values

na.omit(wind_turbines_sf)
# Filter out rows with NA values in r_rotor and hub_height
#wind_turbines_sf <- wind_turbines_sf[!is.na(wind_turbines_sf$r_rotor) & 
                                      # !is.na(wind_turbines_sf$hub_height), ]


wind_turbines_sf$hub.height <- as.numeric(wind_turbines_sf$hub.height)
wind_turbines_sf$d_rotor <- as.numeric(wind_turbines_sf$d_rotor) 
 

 

# Plot
# Calculate average hub height and rotor diameter for each year
avg_height_rotor_by_year <- wind_turbines_sf %>%
  mutate(year = year(dmy(unit_date.install))) %>%
  group_by(year) %>%
  summarise(average_hub_height = mean(hub.height, na.rm = TRUE),
            average_diameter_rotor = mean(d_rotor, na.rm = TRUE))

# Convert data to long format
avg_height_rotor_by_year_long <- avg_height_rotor_by_year %>%
  pivot_longer(cols = c(average_hub_height, average_diameter_rotor),
               names_to = "measurement",
               values_to = "value")

 
# Plot data
ggplot(avg_height_rotor_by_year_long, aes(x = year, y = value, color = measurement)) +
  geom_line() +
  labs(x = "Year", y = "Average Value", 
       title = "The Trend in Average Hub Height and Rotor Size by Year") +
  scale_color_discrete(name = "Measurement",
                       labels = c("Average Hub Height", "Average Rotor Diameter")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


#corr(wind_turbines_sf$hub.height, wind_turbines_sf$d_rotor)

cor(wind_turbines_sf$hub.height, wind_turbines_sf$d_rotor, 
    use = "complete.obs") # because the columns contain NA values, i use "use"
#function. there is high correlation of 0.83 between height of turbine and 
#diameter of rotor. The graph also illustartes that over years height and diameter
# increases with the same trend




#....The number of turbines in each geographic area type



# First, we'll save the result of counting the turbines in each category to a new dataframe
turbine_counts <- merged_wind_turbines_sf %>%
  group_by(geographic_area_type) %>%
  summarise(n = n(), .groups = "drop")

# Now, we'll create the bar plot

color_map <- c("supplement area" = "#f5eca6", 
               "center" = "#d1bb11", 
               "closer commuter connection area" ="#a39317", 
               "not belonging to metropolitan area" = "#363004", 
               "further commuter integration area" = "#736816")

ggplot(turbine_counts, aes(x = reorder(geographic_area_type, n), y = n, fill = geographic_area_type)) +
  geom_bar(stat = "identity", color = "black") +
  coord_flip() +
  labs(x = "Geographic Area Type", y = "Number of Turbines",
       title = "Number of Turbines in Each Geographic Area Type",
       fill = "Geographic Area Type") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_manual(values = color_map)




# Turbines visualised in Germany(map) -------------------------------------

# Load the district data.
germany_districts <- st_read("/Users/rasimbaghirli/Desktop/THESIS_CODING/gadm41_DEU_shp/gadm41_DEU_2.shp")

View(germany_districts)

# Spatially join the turbines to the districts
turbines_in_districts <- st_join(germany_districts, wind_turbines_sf, join = st_intersects)

# Count the number of turbines in each district
turbine_counts <- turbines_in_districts %>%
  group_by(NAME_1, NAME_2) %>%
  summarise(turbines = n(), .groups = "drop")

# Convert back to sf object
turbine_counts <- st_as_sf(turbine_counts)

# Replace NA values with 0
turbine_counts$turbines[is.na(turbine_counts$turbines)] <- 0


p <- ggplot() +
  geom_sf(data = turbine_counts, aes(fill = turbines), color = "red", size = 20) +
  geom_sf(data = wind_turbines_sf, color = "black", size = 0.4) +
  scale_fill_gradient(low = "peachpuff", high = "darkred") +
  theme_minimal() +
  labs(title = "Wind Turbines in Germany by District", fill = "No. of Turbines") +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.line = element_line(colour = "black")
  ) +
  coord_sf(xlim = c(5, 16), ylim = c(47.2, 55))

# Save the plot
#ggsave("mpp.png", plot = p, width = 10, height = 10, units = "in")


 


# Convert wind_turbines_sf to a data frame and add x and y coordinates
wind_turbines_df <- as.data.frame(wind_turbines_sf)
wind_turbines_df$x <- st_coordinates(wind_turbines_sf)[, 1]
wind_turbines_df$y <- st_coordinates(wind_turbines_sf)[, 2]


turbine_map <- ggplot() +
  geom_sf(data = turbine_counts, aes(fill = turbines), color = "#0f0f0f", size = 900) +
  geom_point(data = wind_turbines_df, aes(x = x, y = y), color = "#065934", size = 1.6, shape = 4) +
  scale_fill_gradient(low = "peachpuff", high = "#800e04") +
  theme_minimal() +
  labs(title = "Wind Turbines in Germany by District", fill = "Number of Turbines") +
  theme(
    plot.title = element_text(size = 25, face = "bold"),
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.line = element_blank(), # remove the axis line
    axis.text = element_blank(), # remove axis text
    axis.ticks = element_blank(), # remove axis ticks
    plot.margin = margin(0,0,0,0,"cm") # remove margins
  ) +
  coord_sf(xlim = c(5, 16), ylim = c(47.2, 55))

## Adding image to the plot 


turbine_image <- readPNG("turbine.png", native = TRUE)
turbine_image

p_image <- turbine_map +
  inset_element(p=turbine_image, 
                left = 0.01,
                bottom = 0.01,
                right = 0.25,
                top = 0.25)

 

print(p_image)
# Save the plot
ggsave("newwimage.png", plot = p_image, width = 16, height = 16, units = "in")



#----Correlation plot for house variables.

dataaa_filtered_copy <- dataaa_filtered

# Rename columns
names(dataaa_filtered_copy)[names(dataaa_filtered_copy) == "bathroom"] <- "Bathroom"
names(dataaa_filtered_copy)[names(dataaa_filtered_copy) == "number_rooms"] <- "Number of Rooms"
names(dataaa_filtered_copy)[names(dataaa_filtered_copy) == "bedroom"] <- "Bedroom"
names(dataaa_filtered_copy)[names(dataaa_filtered_copy) == "living_space"] <- "Living Space"
names(dataaa_filtered_copy)[names(dataaa_filtered_copy) == "land_area"] <- "Land Area"
names(dataaa_filtered_copy)[names(dataaa_filtered_copy) == "construction_year"] <- "Year of Construction"

# Now calculate correlation
corr <- cor(dataaa_filtered_copy[c("Bathroom", "Number of Rooms", 
                              "Bedroom", "Living Space", "Land Area", 
                              "Year of Construction")], 
            use = "complete.obs")

# And create your corrplot
corrplot(corr, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", 
         number.cex = 0.7, 
         order = "hclust", 
         addrect = 2, 
         rect.col = "black", 
         rect.lwd = 2, 
         title = "Correlations Between House Characteristics", 
         mar = c(0,0,1,0)) 



 

#....The number of turbines in each central place category


# First, we'll save the result of counting the turbines in each category to a new dataframe
turbine_counts_center <- merged_wind_turbines_sf %>%
  group_by(central_place_classification) %>%
  summarise(n = n(), .groups = "drop")

# Now, we'll create the bar plot

color_map_center <- c("No Central Place"= "#363004",
                      "Low Central Place" = "#736816",
                      "Middle Central Place" ="#a39317",
                      "High Central Place"= "#d1bb11")

ggplot(turbine_counts_center, aes(x = reorder(central_place_classification, n), y = n, fill = central_place_classification)) +
  geom_bar(stat = "identity", color = "black") +
  coord_flip() +
  labs(x = "Central Place Classification", y = "Number of Turbines",
       title = "Number of Turbines in Each Central Place Classification",
       fill = "Central Place Classification") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_manual(values = color_map_center)


na.omit(merged_wind_turbines_sf)



# Summarise the data
# Generate the summary table
# Convert to a regular dataframe and rename the 'federal_state' column
merged_wind_turbines_df <- as.data.frame(merged_wind_turbines_sf)
merged_wind_turbines_df <- rename(merged_wind_turbines_df, `Federal State` = federal_state)

merged_wind_turbines_df <- merged_wind_turbines_df %>% drop_na(hub.height, d_rotor, net_rated_power)

merged_wind_turbines_df$hub.height <- as.numeric(merged_wind_turbines_df$hub.height)
merged_wind_turbines_df$d_rotor <- as.numeric(merged_wind_turbines_df$d_rotor) 
merged_wind_turbines_df$net_rated_power <- as.numeric(merged_wind_turbines_df$net_rated_power) 

# Generate the summary table
# convert plant_date.install to Date if it's not already
merged_wind_turbines_df$plant_date.install <- as.Date(merged_wind_turbines_df$plant_date.install)
class(merged_wind_turbines_df$plant_date.install)
# Generate the summary table
summary_table <- merged_wind_turbines_df %>%
  group_by(`Federal State`) %>%
  summarise(
    `Hub Height (m)` = round(mean(hub.height, na.rm = TRUE), 1),
    `Rotor Diameter (m)` = round(mean(d_rotor, na.rm = TRUE), 1),
    `Rated Power (kW)` = round(mean(net_rated_power, na.rm = TRUE), 1),
    `Total Turbines` = n(),
    .groups = "drop"
  )

# Format the table for display
summary_table %>%
  kable("html") %>%
  kable_styling("striped", full_width = F) %>%
  add_header_above(c(" " = 1, "Average Wind Turbine Measures by Federal State" = 4)) %>%
  column_spec(1, bold = T, border_right = T) %>%
  column_spec(2:5, bold = F)



View(merged_wind_turbines_df)




#Summary table for locality characteristics in municipality level

table1::label(dataaa_filtered_socio_demo$Population) <- "Total Population Size"
table1::label(dataaa_filtered_socio_demo$PopulationDensity) <- "Population Density per km² " 
table1::label(dataaa_filtered_socio_demo$Unemployment) <- "Unemployment Rate (in %)"
table1::label(dataaa_filtered_socio_demo$HighwayAccessibility) <- "Accessibility to the Nearest Highway in Minute"
table1::label(dataaa_filtered_socio_demo$SettlementDensityPerKm) <- "Total Inhabitants per km² Settlement and Trafic Area"
table1::label(dataaa_filtered_socio_demo$UpperCentreAccessibility) <- "Accessibility to the Nearest City Center in Minute"
table1::label(dataaa_filtered_socio_demo$PublicTransportStops) <- "Total Public Transport Stops"
 




# Generate the table
table1::table1(~ Population+ PopulationDensity + Unemployment + HighwayAccessibility +
                 SettlementDensityPerKm + UpperCentreAccessibility +  PublicTransportStops
               , data = dataaa_filtered_socio_demo)
 
 






# ...................Distribution of house sell prices

house_data$adj_sellprice_thousands <- house_data$adj_sellprice / 1000

ggplot(house_data, aes(x=adj_sellprice_thousands)) +
  geom_histogram(binwidth=50, fill="steelblue", color="black", alpha=5) +
  theme_bw() +
  theme(
    plot.title = element_text(size=20, face="bold", hjust=0.5),
    axis.title.x = element_text(size=16, face="bold"),
    axis.title.y = element_text(size=16, face="bold"),
    axis.text.x = element_text(size=14, angle=45, hjust=1),
    axis.text.y = element_text(size=14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, linewidth=1.5)
  ) +
  scale_x_continuous(labels = scales::comma_format()) +
  scale_y_continuous(labels = function(x) paste0(x / 1000, "k")) +
  labs(title="Distribution of House Selling Prices (in thousands)",
       x="House Selling Prices (in thousands)", y="Frequency (in thousands)")



 
 

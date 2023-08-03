# Install and load packages



pkg <- c("dplyr", "lubridate", "tidyr", "sf", "mice", "stringr", 
         "viridis", "RColorBrewer", "readxl", "knitr", "kableExtra", 
         "gt", "table1", "writexl", "ggplot2", "corrplot", "png", 
         "patchwork", "summarytools", "psych", "broom", "nngeo", 
         "jsonlite", "geojsonio", "units", "car", "stargazer", "rgdal", 
         "fixest", "lmtest", "foreign", "nlme", "spdep", "spatialreg")

#load all packages
lapply(pkg, require, character.only = TRUE)


#-----------------------------------------------------------------------




#.........Wind Turbine Data; Data Cleaning and Manupulation


mydata <- read.csv("turbines.csv", header = TRUE, sep = ';')

#finding empty columns
#names(mydata)[empty_cols]
empty_count <- sapply(mydata, function(x) sum(x == ""))

# Print the counts
print(empty_count)

mydata <- subset(mydata, select = -c(Zuschlagnummer..EEG.KWK.Ausschreibung.,
                                     Spannungsebene
))
mydata <- subset(mydata, select = -c(Elektrische.KWK.Leistung,
                                     Inbetriebnahmedatum.der.KWK.Anlage,
                                     Thermische.Nutzleistung.in.kW,
                                     MaStR.Nr..der.KWK.Anlage,
                                     Installierte.Leistung,
                                     EEG.Anlagenschlüssel,
                                     MaStR.Nr..der.EEG.Anlage,
                                     Netzbetreiberprüfung,
                                     Name.des.Anschluss.Netzbetreibers,
                                     MaStR.Nr..des.Anschluss.Netzbetreibers,
                                     MaStR.Nr..der.Genehmigung,
                                     Volleinspeisung.oder.Teileinspeisung,
                                     X.MaStR.Nr..des.Anlagenbetreibers,
                                     Name.des.Anlagenbetreibers..nur.Org..
                                     
))
mydata <- subset(mydata, select = -c(MaStR.Nr..der.Lokation,
                                     Technologie.der.Stromerzeugung,
                                     Lage.der.Einheit,
                                     Nutzbare.Speicherkapazität.in.kWh,
                                     Hersteller.der.Windenergieanlage,
                                     Typenbezeichnung,
                                     Hauptbrennstoff.der.Einheit,
                                     Name.des.Windparks,
                                     Anzahl.der.Solar.Module,
                                     Hauptausrichtung.der.Solar.Module,
                                     Anzeige.Name.der.Einheit,
                                     Energieträger
))

#mydata <- subset(mydata, select = -Gemeindeschlüssel)

mydata <- subset(mydata, select = -c(Hausnummer, Straße))

mydata <- subset(mydata, select = -Datum.der.geplanten.Inbetriebnahme)

mydata <- subset(mydata, select = -Datum.der.endgültigen.Stilllegung)

#mydata <- subset(mydata, select = -c(Flurstück, Gemarkung
#))
mydata <- subset(mydata, select = -Letzte.Aktualisierung)


mydata <- subset(mydata, select = -Bruttoleistung.der.Einheit
)

##### changing column names

colnames(mydata)[which(names(mydata) == 
                         "Koordinate..Breitengrad..WGS84.")] <- "lati"
colnames(mydata)[which(names(mydata) == 
                         "Koordinate..Längengrad..WGS84.")] <- "longi"
colnames(mydata)[which(names(mydata) == 
                         "Ort")] <- "city"
colnames(mydata)[which(names(mydata) == 
                         "Postleitzahl")] <- "postcode"
colnames(mydata)[which(names(mydata) ==
                         "Betriebs.Status")] <- "oper_status"
colnames(mydata)[which(names(mydata) ==
                         "Nabenhöhe.der.Windenergieanlage")] <- "hub.height"
colnames(mydata)[which(names(mydata) ==
                         "Registrierungsdatum.der.Einheit")] <- "reg.date.unit"

colnames(mydata)[which(names(mydata) ==
                         "Inbetriebnahmedatum.der.Einheit")] <- "unit_date.install"
colnames(mydata)[which(names(mydata) ==
                         "Inbetriebnahmedatum.der.EEG.Anlage")] <- "plant_date.install"
colnames(mydata)[which(names(mydata) ==
                         "Rotordurchmesser.der.Windenergieanlage")] <- "d_rotor"
colnames(mydata)[which(names(mydata) ==
                         "reg.data.unit")] <- "reg.date.unit"
colnames(mydata)[which(names(mydata) == "MaStR.Nr..der.Einheit")] <- "turbine_id"

colnames(mydata)[which(names(mydata) == "Gemeindeschlüssel")] <- "municipality_code"

colnames(mydata)[which(names(mydata) == "Gemarkung")] <- "district"
colnames(mydata)[which(names(mydata) == "Bundesland")] <- "federal_state"
colnames(mydata)[which(names(mydata) == "Flurstück")] <- "parcel"
colnames(mydata)[which(names(mydata) == "Nettonennleistung.der.Einheit")] <- "net_rated_power"
######changing values in oper_status column 

mydata[mydata=="In Betrieb"] <- "In Operation"
mydata[mydata == "In Planung"] <- "In Planning"
mydata[mydata == "Endgültig stillgelegt"] <- "Closed"
mydata[mydata == "Vorübergehend stillgelegt"] <- "Temporarily Shut Down"



#### dealing with missing values


sum(is.na(mydata))
colSums(is.na(mydata))

mydata[!complete.cases(mydata),]

#mydata[!(is.na(mydata$postcode) | mydata$postcode==""), ] #to removeblank cells
#for specific columns
#mydata[!(is.na(mydata$lati) | mydata$lati==""), ]
#mydata[!(is.na(mydata$longi) | mydata$longi==""), ]
#mydata[!(is.na(mydata$lati) | mydata$lati==""), ]

mydata[mydata == ''] <- NA #replacing all empty cells with NAs
na.omit(mydata) #omitting NA values
colSums(is.na(mydata)) #shows sum of NAs for each column
#mydata <- mydata %>% drop_na() #drops all NAs from the dataframe





#####lubridate, as_data

#lubridate::as_date(mydata$unit_date.install)




mydata$longi <- str_replace_all(mydata$longi, ",", ".") #substituting commas with dots
mydata$lati <- str_replace_all(mydata$lati, ",", ".") #since coordinate system has to be 
# coded with dots.


mydata$longi <- as.numeric(mydata$longi) #converting characters to numeric values
mydata$lati <- as.numeric(mydata$lati)
str(mydata)


mydata_no_na <- mydata %>% drop_na(longi, lati, unit_date.install) #droping NAs only for longi
#and lati columns in order to use sf function, we left with 24614 observations.
empty_counts_ <- sapply(mydata_no_na, function(x) sum(x == ""))

# Print the counts
print(empty_counts_)

colSums(is.na(mydata_no_na))
nrow(mydata_no_na)
# Convert the data frames to sf objects


wind_turbines_sf <- st_as_sf(mydata_no_na , coords = c("longi", "lati"), 
                             crs = 4326)


na.omit(wind_turbines_sf)


table(wind_turbines_sf$oper_status)



#......Since turbine data contains municipality codes, i merge it with 
#other dataset in order to identify whether a turbine is located in urban or
#rural areas

 

excel_file <- "Referenz Gemeinden, Kreise, NUTS.xlsx"

# Specify the name of the sheet you want to extract
sheet_name <- "Gemeinden-Gemeindeverbände"

# Read the sheet from the Excel file
municipality_data <- read_excel(excel_file, sheet = sheet_name)

# Create a CSV file name based on the sheet name
csv_file <- paste0(sheet_name, ".csv")

# Write the data to a CSV file
write.csv(municipality_data, file = csv_file, row.names = FALSE)

# Read the data again, this time skipping the first row
munici_data <- read.csv("Gemeinden-Gemeindeverbände.csv", skip = 1, 
                        header = FALSE, sep = ',')

 
colnames(munici_data) <- munici_data[1,]

# Remove the second row from the data
munici_data <- munici_data[-1,]

rownames(munici_data) <- NULL
colnames(munici_data)

colnames(munici_data)[which(names(munici_data) == 
                              "Gemeinden Kennziffer")] <- "municipality_code"


class(wind_turbines_sf$municipality_code)
class(munici_data$municipality_code)

#.............Because municipality_code in the turbine dataset is
#... integer i will convert it intocharacter before i merge thw two data sets

#convert to character
wind_turbines_sf$municipality_code <- as.character(wind_turbines_sf$municipality_code)

merged_wind_turbines_sf <- merge(wind_turbines_sf, munici_data, by = 
                                   "municipality_code")
#View(merged_wind_turbines_sf)


unmatched_rows <- anti_join(wind_turbines_sf, munici_data, 
                            by = "municipality_code")
#View(unmatched_rows)

# we lost over 1000 observations some due to NA values in municipality_code 
#column of the turbine data, and some due to unmatched codes.

 


colnames(merged_wind_turbines_sf)[which(names(merged_wind_turbines_sf) == 
                                          "Großstadtregionaler Einzugsbereich Name")] <-
  "geographic_area_type"


colnames(merged_wind_turbines_sf)[which(names(merged_wind_turbines_sf) == 
                                          "Zentralörtliche Einstufung (zusammengefasst) Name")] <-
  "central_place_classification"



#chaning some value names in specific needed columns


merged_wind_turbines_sf$geographic_area_type[merged_wind_turbines_sf$geographic_area_type==
                                               "Ergänzungsgebiet"] <- "supplement area"

merged_wind_turbines_sf$geographic_area_type[merged_wind_turbines_sf$geographic_area_type == 
                                               "Zentrum"] <- "center"

merged_wind_turbines_sf$geographic_area_type[merged_wind_turbines_sf$geographic_area_type == 
                                               "engerer Pendlerverflechtungsbereich"] <- "closer commuter connection area"

merged_wind_turbines_sf$geographic_area_type[merged_wind_turbines_sf$geographic_area_type == 
                                               "nicht zu Großstadtregion gehörend"] <- "not belonging to metropolitan area"

merged_wind_turbines_sf$geographic_area_type[merged_wind_turbines_sf$geographic_area_type ==
                                               "weiterer Pendlerverflechtungsbereich"] <- "further commuter integration area"




merged_wind_turbines_sf$central_place_classification[merged_wind_turbines_sf$central_place_classification==
                                                       "Oberzentrum und höher"] <- "High Central Place"

merged_wind_turbines_sf$central_place_classification[merged_wind_turbines_sf$central_place_classification == 
                                                       "Mittelzentrum"] <- "Middle Central Place"

merged_wind_turbines_sf$central_place_classification[merged_wind_turbines_sf$central_place_classification == 
                                                       "Grundzentrum und niedriger"] <- "Low Central Place"

merged_wind_turbines_sf$central_place_classification[merged_wind_turbines_sf$central_place_classification == 
                                                       "keine zentralörtliche Einstufun"] <- "No Central Place"





empty_count_final <- sapply(merged_wind_turbines_sf, function(x) sum(x == ""))

# Print the counts
print(empty_count_final) #there is no empty columns anymore, but NAs still are there









#------------------------------------------------------------------------------







#........House Data; Data Cleaning and Manupulation

readRDS("house_data_raw.rds") -> dataaa # i will need the row house data for some
#calculations

# Select rows with duplicated Unique_ID
duplicated_rows <- dataaa[duplicated(dataaa$uniqueID_gen) | 
                            duplicated(dataaa$uniqueID_gen, fromLast = TRUE), ]

# Print the duplicated rows
print(duplicated_rows)
colSums(is.na(dataaa))

dataaa$date_start <- paste(dataaa$ajahr, dataaa$amonat, "01", sep = "-")

# Convert to date type
dataaa$date_start <- as.Date(dataaa$date_start)
print(dataaa$date_start)
print(dataaa["date_start"])


# Merge month and year columns - date when advertisement is deleted 
dataaa$date_finish <- paste(dataaa$ejahr, dataaa$emonat, "01", sep = "-")
dataaa$date_finish <- as.Date(dataaa$date_finish)
print(dataaa["date_finish"])

sum(is.na(dataaa$r1_id))


# Select columns with more than 5ml NAs ----------------------------------


selected_cols <- colSums(is.na(dataaa)) > 5000000
selected_cols
dataaa[,selected_cols]

colSums(is.na(dataaa))


# delete columns containing over 5ml NAs
dataaa <- subset(dataaa, select = - selected_cols)
colnames(data)

dataaa <- subset(dataaa, select = -c(energieeffizienzklasse, letzte_modernisierung, 
                                     mieteinnahmenpromonat, bauphase, nebenraeume, 
                                     ev_kennwert, nutzflaeche, rollstuhlgerecht, 
                                     ev_wwenthalten, energieausweistyp, 
                                     ajahr, amonat, ejahr, emonat, 
                                     flag_aufzug, flag_gaestewc, 
                                     flag_einliegerwohnung, flag_baujahr, 
                                     flag_denkmalobjekt, flag_kaufvermietet, 
                                     flag_keller, denkmalobjekt, 
                                     ferienhaus, parkplatzpreis, blid, 
                                     gaestewc))

dataaa <- subset(dataaa, select = -c(flag_ferienhaus, flag_parkplatz))


#NAs and deal with NAs  -------------------------------------------------


colSums(is.na(dataaa))
sum(is.na(data)) 


#dataaa <- dataaa[complete.cases(dataaa$anzahletagen, 
#dataaa$wohnflaeche, dataaa$r1_id, 
#dataaa$schlafzimmer, dataaa$kaufpreis, 
#dataaa$zimmeranzahl, dataaa$grundstuecksflaeche), 

#dataaa$ausstattung, dataaa$kaufvermietet, dataaa$einliegerwohnung, dataaa$]

table(dataaa$ausstattung)

nrow(dataaa)
dataaa$objektzustand <- factor(dataaa$objektzustand)
dataaa$objektzustand <- droplevels(dataaa$objektzustand, 
                                   exclude = "Dilapidated")

#adding house id column 
dataaa$house_id <- paste0("house_id", seq_along(dataaa$r1_id))

# Filter for house prices between 60,000 and 2,500,000
dataaa_filtered <- subset(dataaa, 
                          kaufpreis >= 60000 & kaufpreis <= 2500000 &
                            zimmeranzahl >= 1 & zimmeranzahl <= 20 &
                            wohnflaeche >= 40 & wohnflaeche <= 800 &
                            grundstuecksflaeche >= 20 & grundstuecksflaeche <=
                            6000 &
                            kategorie_Haus %in% c("Single-family house", "Bungalow",
                                                  "Mansion","Semi-detached house",
                                                  "Farmhouse","Terraced house") &
                            baujahr > 1950 &
                            schlafzimmer > 0 & schlafzimmer <= 8 &
                            anzahletagen <= 5)

dataaa_filtered <- dplyr::rename(dataaa_filtered, 
                                 bedroom = schlafzimmer,
                                 postcode = plz,
                                 sellprice = kaufpreis,
                                 parkplace = parkplatz,
                                 construction_year = baujahr,
                                 condition = objektzustand,
                                 granny_flat = einliegerwohnung,
                                 cellar = keller,
                                 land_area = grundstuecksflaeche,
                                 living_space = wohnflaeche,
                                 number_floors = anzahletagen,
                                 number_rooms = zimmeranzahl,
                                 category_house = kategorie_Haus,
                                 bathroom=badezimmer,
                                 elevator=aufzug,
                                 heating_type=heizungsart,
                                 furnishing = ausstattung,
                                 leased = kaufvermietet)


#generating unique house_id identifiers for each house
dataaa_filtered$house_id <- paste0("house_id", seq_along(dataaa_filtered$r1_id))

#View(dataaa_filtered)

########....................downloading inflation data of germany 2009-2020


inf <- read.csv("inflation_2009_2020.csv",header = FALSE, sep = ';')
View(inf)
na.omit(inf)
###### converting columns into numeric and replacing - character with NA
inf <- inf %>%
  mutate(V3 = as.numeric(V3),
         V4 = as.numeric(V4),
         V5 =  as.numeric(V5))

# Delete the first six rows
inf <- inf[-c(1:6, 151:154), ]

# Reset the row names
rownames(inf) <- NULL
names(inf) <- c("years", "months", "cons_pr_index", 
                "change_on_previous_years_month", 
                "change_on_previous_month")


# Replace "-" with NA in the "change_on_previous_month" column
inf$change_on_previous_month <- ifelse(inf$change_on_previous_month ==
                                         "-", NA, 
                                       inf$change_on_previous_month)

# Define a vector of month names and corresponding numeric values
month_names <- c("January", "February", "March", "April", 
                 "May", "June", "July", "August", "September", 
                 "October", "November", "December")

# create a vector of 2-digit numbers (01, 02, ..., 12)
month_numbers <- sprintf("%02d", 1:12) 

# Use match() function to convert month names to numbers
inf$months <- month_numbers[match(inf$months, month_names)]

# Merge months and years columns 
inf$date <- paste(inf$years, inf$months, "01", sep = "-")

# Convert to date type
inf$date <- as.Date(inf$date)
print(inf$date)
print(inf["date"])

na.omit(inf)

nrow(inf)

####deleting years and months column
inf <- subset(inf, select = -c(years, months))

## to shift the date column to the from
inf <- inf %>% select(date, everything())



# Merge the two data frames based on the date columns

dataaa_filtered <- inner_join(dataaa_filtered, inf, by = c("date_finish" = "date"))


#..................adjusting house prices for inflation..................
dataaa_filtered$adj_sellprice <- dataaa_filtered$sellprice / 
  (dataaa_filtered$cons_pr_index / 100)


#creating advertisement duration column
dataaa_filtered$ads_duration <- dataaa_filtered$date_finish - dataaa_filtered$date_start


class(dataaa_filtered$ads_duration) 
dataaa_filtered$ads_duration <- as.numeric(dataaa_filtered$ads_duration)









#--------------------------------------------------------------------------









#.....Merging House Data with Inkar, House Geocoordinates and other datasets.



  


#reading the dataframe that contains municipality ID codes and names of the houses

readRDS("municipalities.rds") -> house_munici_codes


#.........Merging the data with house municipality codes and names with the 
# row house data


dataaa_munici_codes <- merge(dataaa, house_munici_codes, by = "uniqueID_gen")


#merging the same data that contains house municipality codes and 
# names with the filtered house data

dataaa_filtered_municipality <- merge(dataaa_filtered, house_munici_codes, 
                                      by = "uniqueID_gen", all.x = TRUE)

colSums(is.na(dataaa_filtered_municipality))#there are 405497 houses that 
#house_municipality data frame has no info about that


# Read the Excel file that holds municipality reference codes



excel_reference <- read_excel("Gemeindereferenzen.xlsx")
# Replace scientific notation with regular numbers in gem19 and gem19rs columns
excel_reference$gem19 <- format(excel_reference$gem19, scientific = FALSE)
excel_reference$gem19rs <- format(excel_reference$gem19rs, scientific = FALSE)




# Convert all columns to character
excel_reference <- as.data.frame(lapply(excel_reference, as.character))

# Check the updated class of the columns
print(sapply(excel_reference, class))

# Convert the gem19_0 column to a character type
excel_reference$gem19_0 <- as.character(excel_reference$gem19_0)
excel_reference$gem19 <- as.character(excel_reference$gem19)
excel_reference$gem19rs <- as.character(excel_reference$gem19rs)
excel_reference$name19 <- as.character(excel_reference$name19)

# Write the data frame to a CSV file
write.csv(excel_reference, "Gemeindereferenzen.csv", row.names = FALSE)

# Read the CSV file, colClasses = c("gem19_0" = "character") interrupts r 
#to treat the column  gem19_0 as numeric which deletes 0s in the beginning 
munici_reference <- read.csv("Gemeindereferenzen.csv", 
                             colClasses = c("gem19_0" = "character",
                                            "gem19rs" = "character",
                                            "gem19" = "character",
                                            "name19" = "character"))



 
#now the filtered house data is getting merged with the municipality 
#reference data that contains different versions of the municipality code
#this important to merge the house data with inkar data, because inkar data
# contains gem19 style municipality codes, that house data has gem19_0.



dataaa_filtered_munici_codes <- dataaa_filtered_municipality %>%
  left_join(munici_reference[c("gem19_0", "gem19rs", "gem19")], 
            by = c("AGS_gem0" = "gem19_0"))







#.......Here reading the INKAR data that contains socio-demographic variables

socio_demo_data <- read.csv("socio_demo_data.csv", header = TRUE, sep = ';')



#since the 1s rows show year of the last available data for a variable, we 
#delete the 1st rows.


socio_demo_data <- socio_demo_data[-1,]

rownames(socio_demo_data) <- NULL # i want the rows start from 1 again

#counting empty rows
empty_rows <- sapply(socio_demo_data, function(x) sum(x == ""))

# Print the counts
print(empty_rows) #Einwohner.je.Arzt=5095, Kinosäle=31
#Schüler.an.allgemeinbildenden.Schulen.je.1.000.Einwohner=205. I have to replace
#them with NA


####........Changing column names

new_names <- c(
  "CentralLocationStatus" = "Zentralörtlicher.Status..zusammengefasst.",
  "CityType" = "Stadt..Gemeindetyp",
  "Population" = "Bevölkerung",
  "PopulationDensity" = "Einwohnerdichte",
  "SettlementAndTrafficArea" = "Siedlungs..und.Verkehrsfläche",
  "SettlementDensityPerKm" = "Siedlungsdichte.in.km.",
  "PoliceStations" = "Polizeidienststellen",
  "EmploymentDensity_AO" = "Beschäftigtendichte..AO.",
  "EmploymentDensity_WO" = "Beschäftigtendichte..WO.",
  "WorkplaceCentrality" = "Arbeitsplatzzentralität",
  "Unemployment" = "Arbeitslosigkeit",
  "TrainStations" = "Bahnhaltestellen",
  "BusStops" = "Bushaltestellen",
  "HighwayAccessibility" = "Erreichbarkeit.Autobahnen",
  "AirportAccessibility" = "Erreichbarkeit.Flughäfen",
  "UpperCentreAccessibility" = "Erreichbarkeit.von.Oberzentren",
  "PublicTransportStops" = "ÖV.Haltestellen",
  "ResidentsPerDoctor" = "Einwohner.je.Arzt",
  "StudentsPer1000Residents" = "Schüler.an.allgemeinbildenden.Schulen.je.1.000.Einwohner",
  "gem_19" = "Kennziffer",
  "municipality_name" = "Raumeinheit"
)

# Renaming the columns
socio_demo_data <- rename(socio_demo_data, !!!new_names)


# Get the names of character columns, because gem_19 column is integer,
#we cannot replace empty rows with NA for that

char_cols <- names(socio_demo_data)[sapply(socio_demo_data, is.character)]
char_cols


# Only replace "" with NA in those columns
socio_demo_data <- socio_demo_data %>%
  mutate(across(all_of(char_cols), ~na_if(., "")))





# Count the number of "0,00" values in each column
zero_counts <- apply(socio_demo_data, 2, function(x) sum(x == "0,00", 
                                                         na.rm = TRUE))

print(zero_counts)

# Calculate the percentage of "0,00" values in each column
zero_percentages <- apply(socio_demo_data, 2, function(x) {
  zero_count <- sum(x == "0,00", na.rm = TRUE)
  non_zero_count <- sum(x != "0,00", na.rm = TRUE)
  
  # Return the percentage
  (zero_count / (zero_count + non_zero_count)) * 100
})

print(zero_percentages) #Kinosäle, Krankenhäuser.insgesamt , 
#U..Straßenbahn.Abfahrten,
#Bahnhaltestellen in all these column '0,00' values make up over 70% of the 
#total values, thus i will drop them , because more likely the variables
#will suffer from lack of variablity, This could potentially violate the
#assumption of normally distributed errors in my regression model
#for now i keep Polizeidienststellen(64%).



###-.......deleting some columns

socio_demo_data<- subset(socio_demo_data, 
                         select = -c(Kinosäle, Krankenhäuser.insgesamt,
                                     U..Straßenbahn.Abfahrten,
                                     TrainStations,Aggregat))




#next step convert integer gem_19 to character, converting some character 
#columns to numeric, changing decimal point from (,) to (.)


# I want to delete ,00 for values of the "CentralLocationStatus", 
# "CityType" columns and Specify the columns to mutate

cols_to_mutate <- c("CentralLocationStatus", "CityType")

# Remove the comma and trailing zeros
socio_demo_data <- socio_demo_data %>%
  mutate_at(vars(cols_to_mutate), ~gsub(",00", "", .))

# Convert back to character
socio_demo_data <- socio_demo_data %>%
  mutate_at(vars(cols_to_mutate), ~as.character(.))



# I want to replace decimal separator with dot for all columns below
#and convert columns to numeric

columns_to_mutate <- c("Population", "PopulationDensity", 
                       "SettlementAndTrafficArea", 
                       "SettlementDensityPerKm", "PoliceStations",
                       "EmploymentDensity_AO", 
                       "EmploymentDensity_WO", "WorkplaceCentrality", 
                       "Unemployment", 
                       "BusStops", "HighwayAccessibility", 
                       "AirportAccessibility", "UpperCentreAccessibility", 
                       "PublicTransportStops", "ResidentsPerDoctor", 
                       "StudentsPer1000Residents")

# Remove the thousands separator, replace the comma with a period 
#and convert to numeric



socio_demo_data <- socio_demo_data %>%
  mutate_at(vars(columns_to_mutate), ~as.numeric(gsub(",", ".", 
                                                      gsub("\\.", "", .))))

sapply(socio_demo_data, class)

###gem_19 is as integer that i will convert to character since it contains info
#about the ID codes of the municipalities


#socio_demo_data$gem_19 <- as.character(socio_demo_data$gem_19)


socio_demo_data <- socio_demo_data %>%
  mutate(CityType = as.character(CityType),  CentralLocationStatus = 
           as.character(CentralLocationStatus,
                        gem_19 =as.character(gem_19)))


socio_demo_data <- socio_demo_data %>% mutate(gem_19 = as.character(gem_19))


#merging filtered house data with socio demoghrapic data
#first i merged the two datasets, there 1,6mil unmatched values
#by checking unique values i detected some whitespaces, so i will first
#remove those before i merge the two datasets



unique_values <- unique(dataaa_filtered_munici_codes$gem19)
unique_values


# for testing i tried to find matches for the value equals to "1001000"
#i could not find exact match, thus i used partial string matching technique , 
#i found a match. Meaning that there spaces and so on interrupts for
#exact matches

count <- sum(grepl("1001000", dataaa_filtered_munici_codes$gem19))
count

dataaa_filtered_munici_codes$gem19 <- str_trim(dataaa_filtered_munici_codes$gem19)


dataaa_filtered_socio_demo <- dataaa_filtered_munici_codes %>%
  left_join(socio_demo_data, by = c("gem19" = "gem_19"))

 

#...................Reading House Geocoordinates dataset........


house_geo_points <-st_read("grid_centroid.geojson")
 

length(unique(house_geo_points$r1_id))

#changing col name to r1_id
colSums(is.na(dataaa_filtered_socio_demo))
colnames(house_geo_points)[which(names(house_geo_points) ==
                                   "idm")] <- "r1_id" 


#merging house dataset with house coordinates

house_data <- left_join(dataaa_filtered_socio_demo,
                        house_geo_points, by = "r1_id")


#converting house_data to sf object
st_geometry(house_data) <- house_data$geometry  





##..........Digital Land Scape model; Municipality borders

# Specify the full file path to the shapefile directory

shapefile_dir <- "/Users/rasimbaghirli/Desktop/THESIS_CODING/vg250_ebenen_0101"

# Read the shapefile
municipality_borders <- st_read(dsn = shapefile_dir)


# Calculate the centroids
municipality_centroids <- st_centroid(municipality_borders)

# Check the resulting data
#View(municipality_centroids)


### I will merge the house_data with municipality_centroids, thus we can 
#calculate the distance between each house and the center of the municipality 
#it locates.



st_crs(municipality_centroids)
st_crs(house_data)



# Transform the CRS to WGS84
municipality_centroids  <- st_transform(municipality_centroids,
                                        "+proj=longlat +datum=WGS84")

# Check the new CRS
st_crs(municipality_centroids)



#the next step is merge the dataset that contains info about the central classi-
#fication of municipalities with municipality_centroid dataset, then filter
#the data for upper center municipalities. After that I will find the distance
#between each house and its neighbor municipality that has upper center status.



municipality_centroids <- left_join(municipality_centroids, munici_reference, 
                                    by = 
                                      c("AGS_0" = "gem19_0"))





#deleting the columns that not needed
municipality_centroids <- subset(municipality_centroids, 
                                 select = -c(OBJID,GEN,SN_V1,DLM_ID,
                                             BEGINN,ADE,GF,BSG,BEZ,IBZ,BEM,
                                             NBD,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,
                                             FK_S3,NUTS,WSK,DLM_ID))

 

colnames(municipality_centroids)[which(names(municipality_centroids) == 
                                         "gem19")] <- "gem_19"


class(municipality_centroids$gem_19)
class(socio_demo_data$gem_19)

unique(municipality_centroids$gem_19)
unique(socio_demo_data$gem_19)



###gem_19 column from municipality_centroids contain whitespaces that interrupt 
#R to merge datasets. With the code chunk below, I will remove whitespace


municipality_centroids$gem_19 <- trimws(municipality_centroids$gem_19)
socio_demo_data$gem_19 <- trimws(socio_demo_data$gem_19)



municipality_centroids <- municipality_centroids %>% left_join(socio_demo_data,
                                                               by = "gem_19")

#somehow select function did not work during joining process to select the
#needed columns, thus I just delete the columns I dont need

municipality_centroids <- subset(municipality_centroids, 
                                 select = -c(SettlementAndTrafficArea,
                                             EmploymentDensity_AO,
                                             Unemployment,
                                             AirportAccessibility,ResidentsPerDoctor,
                                             Population,SettlementDensityPerKm,
                                             EmploymentDensity_WO,BusStops,
                                             UpperCentreAccessibility,
                                             StudentsPer1000Residents,PopulationDensity,
                                             WorkplaceCentrality,PublicTransportStops
                                             
                                 ))

municipality_centroids <- subset(municipality_centroids, 
                                 select = -c(HighwayAccessibility, municipality_name,
                                             PoliceStations))



##Creating new dataset with only Upper central location status which gets value
#of 1

municipality_centroids_upper <- municipality_centroids %>%
  filter(CentralLocationStatus == 1)

 

#----------------------------------------------------------------------






#.......................Distance CAlculations............









# Transform to ETRS89 / UTM zone 32N (EPSG:25832)

house_data <- st_transform(house_data, 25832)

municipality_centroids_upper <- st_transform(municipality_centroids_upper, 25832)

house_data <- house_data[!st_is_empty(house_data), ]

# Find nearest neighbors
nearest_city <- st_nn(house_data, municipality_centroids_upper, returnDist = TRUE)

# Add nearest neighbor ID and distance to house_data_sample
house_data$NearestCityCenterID <- municipality_centroids_upper$ID[unlist(nearest_city$nn)]
house_data$DistanceToNearestCityCenter <- unlist(nearest_city$dist)

# Convert distance to 1000km by dividing by 1000
house_data$DistanceToNearestCityCenter <- house_data$DistanceToNearestCityCenter / 1000


 




####.....Doing some spatial Analysis in order to find the distance between
#nearest railways, water area, traffic road and so on.

################Bahn data


dlm_files <- list.files(path = "/Users/rasimbaghirli/Desktop/THESIS_CODING/dlm250.gk3.shape.ebenen/dlm250_ebenen")
print(dlm_files)



# Read the railways shapefile
railways_file_path <- "/Users/rasimbaghirli/Desktop/THESIS_CODING/dlm250.gk3.shape.ebenen/dlm250_ebenen/ver03_l.shp"
railways_data <- st_read(railways_file_path)

# Transform the railways data to the same CRS as the house data

#house_data_sample <- st_transform(house_data_sample, 25832)
railways_data <- st_transform(railways_data, crs = 25832)
house_data <- st_transform(house_data, 25832)
#View(railways_data)

unique(railways_data$OBJART_TXT)


#in this case I don't need to control for SeilbahnSchwebebahn which is cable
#lines for Schwebebahn, Ski-, Schlepplift, Materialseilbahn.


#Bahnstrecke :'Railway' is a specific section of the network of railways, 
#identified by a name and/or number. Railway lines can consist of one or two tracks.
#all S-Bahn, Strassen Bahn, U Ban and so on.





SeilbahnSchwebebahn_sf <- railways_data[railways_data$OBJART_TXT == 
                                          "AX_SeilbahnSchwebebahn", ]

Bahnstrecke_sf <- railways_data[railways_data$OBJART_TXT == "AX_Bahnstrecke", ]


 


# Remove empty geometries from the house and railways data

house_data <- house_data[!st_is_empty(house_data), ]

Bahnstrecke_sf <- Bahnstrecke_sf[!st_is_empty(Bahnstrecke_sf), ]

Bahnstrecke_sf <- st_transform(Bahnstrecke_sf, crs = 25832)

# Find the nearest neighbor (railway) for each house
nearest_railway <- st_nn(house_data, Bahnstrecke_sf, returnDist = TRUE)

# Add the nearest railway ID and distance to the house_data_sample dataframe
house_data$NearestRailwayID <- Bahnstrecke_sf$ID[unlist(nearest_railway$nn)]
house_data$DistanceToNearestRailway <- unlist(nearest_railway$dist)

# Convert the distance to kilometers
#house_data_sample$DistanceToNearestRailway <- 
#house_data_sample$DistanceToNearestRailway 

# Define proximity categories based on distance
house_data$railway_proximity <- cut(house_data$DistanceToNearestRailway,
                                           breaks = c(0, 20, 50, 100, 150, 200, Inf),
                                           labels = c(" 0-20 m"," 20-50 m" ," 50-100 m", 
                                                      "100-150 m", " 150-200 m", 
                                                      " further away"))


house_data$railway_proximity <- relevel(house_data$railway_proximity, 
                                               ref = " further away")

unique(house_data$railway_proximity)
house_data$railway_proximity <- as.factor(house_data$railway_proximity)

 




######....Reading the data contains info about the water types and their geometry



#wateraxis_file_path_ <- "/Users/rasimbaghirli/Desktop/THESIS_CODING/dlm250.gk3.shape.ebenen/dlm250_ebenen/gew01_l.shp"
#wateraxis_data <- st_read(wateraxis_file_path)


waterbody_file_path <- "/Users/rasimbaghirli/Desktop/THESIS_CODING/dlm250.gk3.shape.ebenen/dlm250_ebenen/gew01_f.shp"
waterbody_data <- st_read(waterbody_file_path)


unique(waterbody_data$OBJART_TXT)

warnings() ##warnings about the usage of decimal points instead of periods



# Recode the values in the "OBJART_TXT" column
waterbody_data <- waterbody_data %>%
  mutate(WaterTypes = case_when(
    OBJART_TXT == "AX_StehendesGewaesser" ~ "Standing Water",
    OBJART_TXT == "AX_Fliessgewaesser" ~ "Flowing Water",
    OBJART_TXT == "AX_Meer" ~ "Sea",
    OBJART_TXT == "AX_Hafenbecken" ~ "Harbour Basin",
    TRUE ~ OBJART_TXT  # Keep the original value if none of the above conditions match
  )) %>%
  select(-OBJART_TXT)  # Remove the original column

# Rename the column
names(waterbody_data)[names(waterbody_data) == "OBJART_TXT"] <- "WaterTypes"

## For this analysis I will create 4 different dataframes with respect to
#WaterTypes in waterbody_data: Then for each dataset I will run st_nn 
#function separately in order to find the nearest distance between houses
#and its nearest watertype. Due to this distance I will create dummy variables
#again for each watertype representing whether is there any e.g standing water,
#sea within 1000-500m(1), 500-0 m(2), or not(0).



##matching waterbody_data's crs to house_data_sample

waterbody_data <- st_transform(waterbody_data, crs = st_crs(house_data))

#creating separate data frames for each water type

StandingWater_sf <- waterbody_data[waterbody_data$WaterTypes == "Standing Water", ]
FlowingWater_sf <- waterbody_data[waterbody_data$WaterTypes == "Flowing Water", ]
HarbourBasin_sf <- waterbody_data[waterbody_data$WaterTypes == "Harbour Basin", ]
Sea_sf <- waterbody_data[waterbody_data$WaterTypes == "Sea", ]




#.....for simplicity I will work again with sample house data


nearest_standing_water <- st_nn(house_data, StandingWater_sf, returnDist = TRUE)
nearest_flowing_water <- st_nn(house_data, FlowingWater_sf, returnDist = TRUE)
nearest_harbour_basin <- st_nn(house_data, HarbourBasin_sf, returnDist = TRUE)
nearest_sea <- st_nn(house_data, Sea_sf, returnDist = TRUE)



house_data$StandingWater_dummy <- ifelse(nearest_standing_water$dist > 6000, 0,
                                                ifelse(nearest_standing_water$dist <= 6000 & nearest_standing_water$dist > 3000, 1,
                                                       ifelse(nearest_standing_water$dist <= 3000 & nearest_standing_water$dist > 1000, 2, 
                                                              ifelse(nearest_standing_water$dist <= 1000, 3, NA))))

house_data$FlowingWater_dummy <- ifelse(nearest_flowing_water$dist > 6000, 0,
                                               ifelse(nearest_flowing_water$dist <= 6000 & nearest_flowing_water$dist > 3000, 1,
                                                      ifelse(nearest_flowing_water$dist <= 3000 & nearest_flowing_water$dist > 1000, 2, 
                                                             ifelse(nearest_flowing_water$dist <= 1000, 3, NA))))

house_data$HarbourBasin_dummy <- ifelse(nearest_harbour_basin$dist > 6000, 0,
                                               ifelse(nearest_harbour_basin$dist <= 6000 & nearest_harbour_basin$dist > 3000, 1,
                                                      ifelse(nearest_harbour_basin$dist <= 3000 & nearest_harbour_basin$dist > 1000, 2, 
                                                             ifelse(nearest_harbour_basin$dist <= 1000, 3, NA))))


house_data$Sea_dummy <- ifelse(nearest_sea$dist > 6000, 0,
                                      ifelse(nearest_sea$dist <= 6000 & nearest_sea$dist > 3000, 1,
                                             ifelse(nearest_sea$dist <= 3000 & nearest_sea$dist > 1000, 2, 
                                                    ifelse(nearest_sea$dist <= 1000, 3, NA))))



# Convert the dummy variables to factors
house_data$StandingWater_dummy <- as.factor(house_data$StandingWater_dummy)
house_data$FlowingWater_dummy <- as.factor(house_data$FlowingWater_dummy)
house_data$HarbourBasin_dummy <- as.factor(house_data$HarbourBasin_dummy)
house_data$Sea_dummy <- as.factor(house_data$Sea_dummy)







#####...... Reading the dlm data for Agricultural areas.


agriculturalArea_file_path <- "/Users/rasimbaghirli/Desktop/THESIS_CODING/dlm250.gk3.shape.ebenen/dlm250_ebenen/veg01_f.shp"
agriculturalArea <- st_read(agriculturalArea_file_path)


unique(agriculturalArea$OBJART_TXT)

View(agriculturalArea)

st_crs(agriculturalArea)

agriculturalArea <- st_transform(agriculturalArea, crs = st_crs(house_data))




# Find the nearest neighbor (agricultural area) for each house
nearest_agriculturalArea <- st_nn(house_data, agriculturalArea, returnDist = TRUE)

# Add the nearest agricultural area ID and distance to the house_data_sample dataframe
house_data$NearestAgriculturalAreaID <- agriculturalArea$ID[unlist(nearest_agriculturalArea$nn)]
house_data$DistanceToNearestAgriculturalArea <- unlist(nearest_agriculturalArea$dist)

# Convert the distance to kilometers
#house_data_sample$DistanceToNearestAgriculturalArea <- 
#house_data_sample$DistanceToNearestAgriculturalArea  

# Define proximity categories based on distance
house_data$agriculturalArea_proximity <- cut(house_data$DistanceToNearestAgriculturalArea,
                                                    breaks = c(0, 250, 500, 1000, 1500, 2000, Inf),
                                                    labels = c(" 0-250 m", " 250-500 m", 
                                                               " 500-1000 m", " 1-1.5 km", 
                                                               " 1.5-2 km", " further away"))


house_data$agriculturalArea_proximity <- relevel(house_data$agriculturalArea_proximity, 
                                                        ref = " further away")

####...........................Vegetation Area...................

#####Looking at the distance between each house and its nearest vegetation area
#The data contain info in the combination of several types of vegetations so called
#Heide, Moor, Sumpf, Unland Vegetationslose Flaeche:In this analysis i will not look at each 
#of those alone.

#'Heide' is a mostly sandy area with typical shrubs, grasses and a 
#small number of trees.


#''Moor' is an uncultivated area whose upper layer consists of
# peat or decomposed plant remains.


#''Swamp' is a waterlogged, intermittently submerged terrain. Spots in the 
#'ground that are temporarily wet after rainfall are not recorded as 'swamps'.

#'Unland/area without vegetation' is an area that is not used for agriculture 
#'on a permanent basis, such as rocky areas that do not protrude from the relief 
#'of the terrain, areas of sand or ice, shore strips along water bodies and 
#succession areas.

#So all above are kind of useless vegetational areas that expected to 
#decrease house price, if a house is close to that.


vegetationArea_file_path <- "/Users/rasimbaghirli/Desktop/THESIS_CODING/dlm250.gk3.shape.ebenen/dlm250_ebenen/veg03_f.shp"
vegetationArea <- st_read(vegetationArea_file_path)


#unique(vegetationArea_file_path$OBJART_TXT)

#View(vegetationArea)

unique(vegetationArea$OBJART_TXT)

vegetationArea <-  st_transform(vegetationArea, crs = st_crs(house_data))


# Remove empty geometries from the house and vegetationArea data
house_data <- house_data[!st_is_empty(house_data), ]
vegetationArea <- vegetationArea[!st_is_empty(vegetationArea), ]

# Find the nearest neighbor (vegetationArea) for each house
nearest_vegetationArea <- st_nn(house_data, vegetationArea, returnDist = TRUE)

# Add the nearest vegetationArea ID and distance to the house_data_sample dataframe
house_data$NearestVegetationAreaID <- vegetationArea$ID[unlist(nearest_vegetationArea$nn)]
house_data$DistanceToNearestVegetationArea <- unlist(nearest_vegetationArea$dist)

# Convert the distance to kilometers
#house_data_sample$DistanceToNearestVegetationArea <- 
# house_data_sample$DistanceToNearestVegetationArea  

# Define proximity categories based on distance
house_data$vegetationArea_proximity <- cut(house_data_sample$DistanceToNearestVegetationArea,
                                                  breaks = c(0, 250, 500, 1000, 1500, 2000, Inf),
                                                  labels = c(" 0-250 m", " 250-500 m", 
                                                             " 500-1000 m", " 1-1.5 km", 
                                                             " 1.5-2 km", " further away"))


house_data$vegetationArea_proximity <- relevel(house_data$vegetationArea_proximity, 
                                                      ref = " further away")





#######. Nearest turbine......




merged_wind_turbines_sf <- st_transform(merged_wind_turbines_sf, crs = 25832)

merged_wind_turbines_sf$install_date <- 
  as.Date(dmy(merged_wind_turbines_sf$unit_date.install), 
                              "%Y-%m-%d")

# removing the original column with dd/mm/yyyy format
merged_wind_turbines_sf$unit_date.install <- NULL


# converting date columns to Date class
house_data$date_finish <- as.Date(house_data$date_finish, format="%Y%m%d")

merged_wind_turbines_sf$install_date <- 
  as.Date(merged_wind_turbines_sf$install_date, format="%Y%m%d")


# Initializing an empty vector in order to store the nearest turbine_ids
nearest_turbine_ids <- vector("numeric", length = nrow(house_data))

# Initializing an empty vector to store the distances
distance <- vector("numeric", length = nrow(house_data))

# Looping over each house
for (i in seq_len(nrow(house_data))) {
  
  # Filtering all turbines that were installed before the house was sold
  valid_turbines <- merged_wind_turbines_sf[merged_wind_turbines_sf$install_date < house_data_sample$date_finish[i], ]
  
  # Check if there are any valid turbines
  if (nrow(valid_turbines) > 0) {
    
    # Find the nearest valid turbine using st_nn()
    nearest_turbine <- st_nn(house_data[i, ], valid_turbines, k = 1, returnDist = TRUE)
    
    # Getting the ID of the nearest valid turbine
    nearest_turbine_ids[i] <- valid_turbines$turbine_id[unlist(nearest_turbine[[1]])[1]]
    
    # Get the distance to the nearest valid turbine from the output of st_nn()
    distance[i] <- unlist(nearest_turbine[[2]])[1]
    
  } else {
    
    # If there are no valid turbines, set the ID and distance to NA
    nearest_turbine_ids[i] <- NA
    distance[i] <- NA
  }
}

# Add the nearest turbine IDs and distances to the house data frame
house_data$turbine_id <- nearest_turbine_ids
house_data$distance_to_nearest_turbine <- distance



house_datae$distance_bands <- cut(house_data$distance_to_nearest_turbine,
                                        breaks = 
                                          c(0, 250, 500,1000, 3000, 6000, Inf), 
                                        labels = c(" 0-250 m"," 250-500 m"," 500-1000 m",
                                                   " 1-3 km", " 3-6 km", " control area"))

##chosing reference group

house_data$distance_bands <- relevel(house_data$distance_bands, 
                                            ref = " control area")


house_data$distance_bands <- as.factor(house_data$distance_bands)





#-----------------------------------------------------------------------





#.......Creating some other variables for regression

house_data$Age_sqrt <- house_data$Age^2
 

house_data$DistanceToNearestRailway_sqrt <- 
  house_data$DistanceToNearestRailway^2

house_data$DistanceToNearestCityCenter_sqrt <- 
  house_data$DistanceToNearestCityCenter^2

house_data$DistanceToNearestVegetationArea_sqrt <- 
  house_data$DistanceToNearestVegetationArea^2

house_data$DistanceToNearestAgriculturalArea_sqrt <- 
  house_data$DistanceToNearestAgriculturalArea^2
  

house_data$parkplace_dummy <- ifelse(house_data$parkplace == 1, 1, 0)

house_data <- house_data %>%
  mutate(Age = year(date_finish) - construction_year)  

# Subset the data frame to include only rows where Age is greater 
#than or equal to 0

house_data <- house_data[house_data$Age >= 0, ]

# Subset munici_data
munici_data_sub <- munici_data[c("municipality_code",
                                 "Stadt- und Gemeindetyp (Bundesländer) Name")]

# Join with house_data_sample
house_data <- left_join(house_data, munici_data_sub, 
                               by = c("gem19" = "municipality_code"))



colnames(house_data)[which(names(house_data) == 
                                    "Stadt- und Gemeindetyp (Bundesländer) Name")] <-
  "state_muniType"




# create a new column 'state' with extracted state names
house_data$state <- str_extract(house_data$state_muniType, 
                                       "(?<=\\().*?(?=\\))")


# replace NA values with 'unknown'
house_data$state[is.na(house_data$state)] <- 'unknown'


# Extract the year from date_finish column
house_data$year <- as.numeric(format(house_data$date_finish, "%Y"))
#house_data$year <- as.character(house_data$year)



#------------------------------------------------------------------------------








#..............Regression Models: FE_OLS, SP_LAG, SP_ERROR, SP_SAC/SARAR



equation1 <- log(adj_sellprice) ~ living_space + land_area + number_floors +
  Age +
  PopulationDensity + PoliceStations + SettlementDensityPerKm +  
  WorkplaceCentrality + EmploymentDensity_WO + Unemployment + 
  HighwayAccessibility +  DistanceToNearestRailway + 
  DistanceToNearestCityCenter + factor(parkplace) +  distance_bands + 
  railway_proximity + factor(agriculturalArea_proximity) + 
  factor(vegetationArea_proximity) + StandingWater_dummy + Sea_dummy + 
  HarbourBasin_dummy + FlowingWater_dummy + condition +  + factor(furnishing) +
  factor(heating_type) + factor(category_house)+
  factor(granny_flat) + factor(CityType) 
  
  
  
equation2 <- equation1 <- log(adj_sellprice) ~ living_space + land_area + 
  number_floors + Age +
  PopulationDensity + PoliceStations + SettlementDensityPerKm +  
  WorkplaceCentrality + EmploymentDensity_WO + Unemployment + 
  HighwayAccessibility +  DistanceToNearestRailway_sqrt + 
  DistanceToNearestCityCenter_sqrt + DistanceToNearestVegetationArea_sqrt +
  + DistanceToNearestAgriculturalArea_sqrt + factor(parkplace) + 
  distance_bands + StandingWater_dummy + Sea_dummy + 
  HarbourBasin_dummy + FlowingWater_dummy + condition + factor(furnishing) +
  factor(heating_type) + factor(category_house) +
  factor(granny_flat) + factor(CityType) 




#...................Fixed Effects Model....................

model_FE <- feols( equation1 |factor(state_muniType) + factor(year) ,
              data = house_data, cluster = "state")

summary(model_FE)






#-----------------------------------------------------------------------


#................Spatial Lag Model.................................

#Weights Matrix


coords <- st_coordinates(house_data)


# Calculating Weight Matrix based on fixed distance

neighbors_nb <- dnearneigh(st_coordinates(house_data_sample), d1 = 0, d2 = 4000,
                           longlat = FALSE)


# Convert the neighbor list to a row-standardized weights list
weights_list <- nb2listw(neighbors_nb, style = "W" ,zero.policy = TRUE)


model_error <- errorsarlm(equation2, data = house_data, 
                           listw = weights_list,
                          method = "eigen", zero.policy = TRUE)


summary(model_error, Nagelkerke = TRUE)

#--------------------------------------------------------------------------



#.................Spatial Lag Model....................................




model_spatial <- lagsarlm(equation2, data = house_data, 
                          listw = weights_list,
                           zero.policy = TRUE)


summary(model_spatial, Nagelkerke = TRUE)


#.....................SAC/SARAR Model................................


model_sarar <- sacsarlm(equation2,
                        data = house_data_sample, listw = weights_list,
                        method = "eigen", zero.policy = TRUE)


summary(model_sarar, Nagelkerke = TRUE)

#------------------Durbin Error Model----------------------------
 
model_durbin_error <- errorsarlm(equation2,
                                 data = house_data, listw = weights_list, 
                                 zero.policy = TRUE, etype = "mixed")

summary(model_durbin_error, Nagelkerke = TRUE)

#----------------------------------------------------------------------





#....................Tests; Residuals


residuals <- resid(model_FE)
durbinWatsonTest(residuals)

plot(residuals(modell_fix))

residuals <- residuals(modell_FE)

hist(residuals, main = "Histogram of Residuals", xlab = "Residuals")

qqnorm(residuals, main = "Q-Q Plot of Residuals")
qqline(residuals, col = "red")



# compute fitted values
fitted_values <- fitted(model_FE)
# compute residuals
residuals <- resid(model_FE)

# create the plot
plot(fitted_values, residuals, 
     xlab = "Fitted values", 
     ylab = "Residuals",
     main = "Residuals vs Fitted values")


##...........Durbin Watson Test

durbinWatsonTest(residuals)

anova(model_sarar, model_lag, model_error, model_durbin_error)

#------------------- Impcats -----------------------

impacts(model_sarar, listw=weights_list)



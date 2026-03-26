# empty workspace
library(ggplot2)
library(tidyr)
library(dplyr)
library(grid)
library(gridExtra)
library(png)
library(httr)
library(jsonlite)

rm(list = ls())

url <- "https://five.epicollect.net/api/export/entries/amphibien-ostviertel-2026?format=csv"
table <- as.data.frame(read.csv(url))

#read.csv("form-1__erfassung.csv")) offlien version

str(table)

table$ec5_uuid <- NULL
table$created_at <- NULL
table$uploaded_at <- NULL
table$X12_Gab_es_auch_Molch <- NULL
table$X16_Tote_Tiere_auf_de <- NULL
table$X18_Nheres_zu_den_Tot <- NULL
table$X19_Sonstige_Bemerkun <- NULL

str(table)

table$X1_Name_Erfasserin <- as.factor(table$X1_Name_Erfasserin)
table$X2_Datum <- as.Date(table$X2_Datum, format = "%d/%m/%Y")
table$X3_Tageszeit <- as.factor(table$X3_Tageszeit)
table$X4_Feuchtigkeit_am_Bo <- as.factor(table$X4_Feuchtigkeit_am_Bo)

table[ , 6:12] <- lapply(table[ , 6:12], as.numeric)
table[ , 13:16][is.na(table[ , 13:16])] <- 0

str(table)

# Sylke U. & Thomas F. richtig benennen
levels(table$X1_Name_Erfasserin)

levels(table$X1_Name_Erfasserin)[levels(table$X1_Name_Erfasserin) == "Thomas und Sylke"] <- "Sylke & Thomas"
levels(table$X1_Name_Erfasserin)[levels(table$X1_Name_Erfasserin) == "Sylke und Thomas"] <- "Sylke & Thomas"
levels(table$X1_Name_Erfasserin)[levels(table$X1_Name_Erfasserin) == "Jan Philip Runge"] <- "JP Runge"
levels(table$X1_Name_Erfasserin)[levels(table$X1_Name_Erfasserin) == "Sophie und Rebecca"] <- "Sophie & Rebecca"

colnames(table)



# Funddaten gesamt
selected_columns <- c(
  "X6_Bismarckstr_Weibch","X7_Bismarckstr_Mnnche","X13_Bismarckstr_Molch","X8_Reinkeweg_Weibchen",
  "X9_Reinkeweg_MnnchenA","X14_Reinkeweg_MolcheA","X10_CalsowTennis_Weib",
  "X11_CalsowTennis_Mnnc","X15_CalsowTennis_Molc","X17_Gesamtzahl_Totfun")# Calculate sums for the selected columns
sums_df <- data.frame(
  Variable = selected_columns,
  Sum = sapply(selected_columns, function(col) sum(table[[col]], na.rm = TRUE)))

# Reorder the variables based on the sum and edit labels
sums_df$Variable <- factor(sums_df$Variable, levels = sums_df$Variable[order(sums_df$Sum, decreasing = TRUE)])

custom_labels <- c(
  "X6_Bismarckstr_Weibch" = "Bismarckstraße Weiblich",
  "X7_Bismarckstr_Mnnche" = "Bismarckstraße Männlich",
  "X13_Bismarckstr_Molch" = "Bismarckstraße Molche",
  "X8_Reinkeweg_Weibchen" = "Reinkeweg Weiblich",
  "X9_Reinkeweg_MnnchenA" = "Reinkeweg Männlich",
  "X14_Reinkeweg_MolcheA" = "Reinkeweg Molche",
  "X10_CalsowTennis_Weib" = "Tennis Weiblich",
  "X11_CalsowTennis_Mnnc" = "Tennis Männlich",
  "X15_CalsowTennis_Molc" = "Tennis Molche",
  "X17_Gesamtzahl_Totfun" = "Totfunde"
)

# Plot using ggplot2
ggplot(sums_df, aes(x = Variable, y = Sum)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Funddaten", x = "", y = "Anzahl") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate axis labels for readability
  scale_x_discrete(labels = custom_labels)

# Save the plot as a PNG file
ggsave("Funddaten.png", 
       width = 8, height = 6, dpi = 300, bg = "white")  # You can adjust width, height, and dpi as needed


# Straßenweise Auswertung
# Sum individuals for Bismarckstraße and Reinkeweg and Tennis
table$Bismarckstraße_Total <- rowSums(table[, c(
  "X6_Bismarckstr_Weibch",
  "X7_Bismarckstr_Mnnche",
  "X13_Bismarckstr_Molch")], na.rm = TRUE)

table$Reinkeweg_Total <- rowSums(table[, c(
  "X8_Reinkeweg_Weibchen",
  "X9_Reinkeweg_MnnchenA",
  "X14_Reinkeweg_MolcheA")], na.rm = TRUE)

table$Tennis_Total <- rowSums(table[, c(
  "X10_CalsowTennis_Weib",
  "X11_CalsowTennis_Mnnc",
  "X15_CalsowTennis_Molc")], na.rm = TRUE)

# Create a new dataframe for plotting, keeping only Date and the totals for each street
sum_by_date_street <- data.frame(
  Date = table$X2_Datum,
  Bismarckstraße = table$Bismarckstraße_Total,
  Tennis = table$Tennis_Total,
  Reinkeweg = table$Reinkeweg_Total,
  Totfunde = table$X17_Gesamtzahl_Totfun
)

# Summarize by Date for each street
sum_by_date_street <- aggregate(. ~ Date, data = sum_by_date_street, sum)

# Calculate cumulative sum for both Bismarckstraße and Calsowstraße
sum_by_date_street$Bismarckstraße_Cumulative <- cumsum(sum_by_date_street$Bismarckstraße)
sum_by_date_street$Totfunde_Cumulative <- cumsum(sum_by_date_street$Totfunde)
sum_by_date_street$Reinkeweg_Cumulative <- cumsum(sum_by_date_street$Reinkeweg)
sum_by_date_street$Tennis_Cumulative <- cumsum(sum_by_date_street$Tennis)

# Plot using ggplot2
ggplot(sum_by_date_street, aes(x = Date)) +
  geom_line(aes(y = Bismarckstraße_Cumulative, color = "Bismarckstraße"), size = 1) +  # Cumulative line for Bismarckstraße
  geom_line(aes(y = Reinkeweg_Cumulative, color = "Reinkeweg"), size = 1) +  # Cumulative line for Calsowstraße
  geom_line(aes(y = Tennis_Cumulative, color = "Tennis"), size = 1) +  # Cumulative line for Calsowstraße
  geom_point(aes(y = Bismarckstraße_Cumulative, color = "Bismarckstraße"), size = 2) +  # Points for Bismarckstraße
  geom_point(aes(y = Reinkeweg_Cumulative, color = "Reinkeweg"), size = 2) +  # Points for Calsowstraße
  geom_point(aes(y = Tennis_Cumulative, color = "Tennis"), size = 2) +  # Points for Calsowstraße
  geom_line(aes(y = Totfunde_Cumulative, color = "Totfunde"), size = 1, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Aufsummierung der gesammelten Tiere pro Standort", x = "", y = "Anzahl") +
  scale_color_manual(name = "Standort", values = c("Totfunde"= "grey","Bismarckstraße" = "blue", "Reinkeweg" = "red", "Tennis" = "green")) +  # Custom colors
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate axis labels for readability

# Save the plot as a PNG file
ggsave("Straßenweise.png", 
       width = 8, height = 6, dpi = 300, bg = "white")  # You can adjust width, height, and dpi as needed


#jetzt nach Erfasser/in
# Summing the total individuals collected by Erfasserin
sum_by_erfasserin <- table %>%
  group_by(X1_Name_Erfasserin) %>%
  summarise(Total_Individuals = sum(
    X6_Bismarckstr_Weibch, X7_Bismarckstr_Mnnche,
    X13_Bismarckstr_Molch, X8_Reinkeweg_Weibchen, X9_Reinkeweg_MnnchenA,
    X14_Reinkeweg_MolcheA, X10_CalsowTennis_Weib, X11_CalsowTennis_Mnnc, X15_CalsowTennis_Molc,
    na.rm = TRUE))

# Reorder the factor levels of Erfasserin based on the Total_Individuals
sum_by_erfasserin$X1_Name_Erfasserin <- factor(sum_by_erfasserin$X1_Name_Erfasserin, 
                                               levels = sum_by_erfasserin$X1_Name_Erfasserin[order(sum_by_erfasserin$Total_Individuals, decreasing = TRUE)])

# Plotting the total sums by Erfasserin
ggplot(sum_by_erfasserin, aes(x = X1_Name_Erfasserin, y = Total_Individuals)) +
  geom_bar(stat = "identity", fill = "skyblue") +  # Bar plot
  theme_minimal() +
  labs(title = "Lebendfunde nach Person", x = "", y = "Anzahl") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate axis labels for readability

# Save the plot as a PNG file
ggsave("Lebendfunde nach Erfasser_in.png", 
       width = 8, height = 6, dpi = 300, bg = "white")  # You can adjust width, height, and dpi as needed


#Feuchtigkeit

levels(table$X4_Feuchtigkeit_am_Bo)

# Define the levels in the desired order for the ordinal variable
feuchtigkeit_levels <- c("trocken", "trocken, frisch", "frisch", "frisch, feucht", "feucht", "nass", "Regen")

table$X4_Feuchtigkeit_am_Bo <- as.character(table$X4_Feuchtigkeit_am_Bo)

table$X4_Feuchtigkeit_am_Bo[table$X4_Feuchtigkeit_am_Bo == "feucht, frisch"] <- "frisch, feucht"
table$X4_Feuchtigkeit_am_Bo[table$X4_Feuchtigkeit_am_Bo == "nass, Regen"] <- "Regen"

table$X4_Feuchtigkeit_am_Bo[table$X4_Feuchtigkeit_am_Bo == "Regen, nass"] <- "Regen"
# Convert the X4_Feuchtigkeit_am_Bo variable to an ordered factor (ordinal)
table$X4_Feuchtigkeit_am_Bo <- factor(table$X4_Feuchtigkeit_am_Bo, 
                                      levels = feuchtigkeit_levels, 
                                      ordered = TRUE)

# Check the transformation
levels(table$X4_Feuchtigkeit_am_Bo)

# Summing the living individuals (sum of the relevant columns for living) and Totfunde by Feuchtigkeit
table$Living_Individuals <- rowSums(table[, c(
  "X6_Bismarckstr_Weibch","X7_Bismarckstr_Mnnche","X13_Bismarckstr_Molch",
  "X8_Reinkeweg_Weibchen","X9_Reinkeweg_MnnchenA","X14_Reinkeweg_MolcheA",
  "X10_CalsowTennis_Weib","X11_CalsowTennis_Mnnc","X15_CalsowTennis_Molc")], na.rm = TRUE)
# Create a new data frame to summarize the totals by Feuchtigkeit
sum_by_feuchtigkeit <- table %>%
  group_by(X4_Feuchtigkeit_am_Bo) %>%
  summarise(Living_Sum = sum(Living_Individuals, na.rm = TRUE),
            Totfunde_Sum = sum(X17_Gesamtzahl_Totfun, na.rm = TRUE))

# Filter out rows where both Living and Totfunde sums are 0 (to remove empty categories from the plot)
sum_by_feuchtigkeit_filtered <- sum_by_feuchtigkeit %>%
  filter(Living_Sum > 0 | Totfunde_Sum > 0)

# Reshape the data for easier plotting (long format)
sum_by_feuchtigkeit_long <- sum_by_feuchtigkeit_filtered %>%
  pivot_longer(cols = c("Living_Sum", "Totfunde_Sum"),
               names_to = "Type", 
               values_to = "Sum")

# Ensure X4_Feuchtigkeit_am_Bo is a factor and ordered according to the filtered data
sum_by_feuchtigkeit_long$X4_Feuchtigkeit_am_Bo <- factor(sum_by_feuchtigkeit_long$X4_Feuchtigkeit_am_Bo,
                                                         levels = unique(sum_by_feuchtigkeit_long$X4_Feuchtigkeit_am_Bo))

#
ggplot(sum_by_feuchtigkeit_long, aes(x = X4_Feuchtigkeit_am_Bo, y = Sum, fill = Type)) +
geom_bar(stat = "identity", position = "dodge") +  # Bars for both Living and Totfunde
  scale_fill_manual(values = c("Living_Sum" = "green", "Totfunde_Sum" = "red"), 
                    labels = c("Lebendfunde", "Totfunde")) +  # Color the bars and rename the legend labels
  theme_minimal() +
  labs(
    title = "Summe der Fundzahlen nach Feuchtigkeit",
    x = "",
    y = "Anzahl",
    fill = "Typ"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate axis labels for readability
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 14),  # Center the title
    legend.position = c(0.1, 0.9),  # Position the legend at the top-left inside the plot
    legend.title = element_blank(),  # Remove the legend title
    legend.background = element_blank()  # Remove the background behind the legend
  )

# Save the plot as a PNG file
ggsave("Funde nach Feuchtigkeit.png", 
       width = 8, height = 6, dpi = 300, bg = "white")  # You can adjust width, height, and dpi as needed

#Temperatur
# Temperaturkurven für Weibchen, Männchen, Molche und Totfunde
temperature_plot_data <- table %>%
  mutate(
    Weibchen = X6_Bismarckstr_Weibch + X8_Reinkeweg_Weibchen + X10_CalsowTennis_Weib,
    Männchen = X7_Bismarckstr_Mnnche + X9_Reinkeweg_MnnchenA + X11_CalsowTennis_Mnnc,
    Molche = X13_Bismarckstr_Molch + X14_Reinkeweg_MolcheA + X15_CalsowTennis_Molc,
    Totfunde = X17_Gesamtzahl_Totfun
  ) %>%
  select(X5_Temperatur_in_Cels, Weibchen, Männchen, Molche, Totfunde) %>%
  filter(!is.na(X5_Temperatur_in_Cels)) %>%
  group_by(X5_Temperatur_in_Cels) %>%
  summarise(across(
    c(Weibchen, Männchen, Molche, Totfunde),
    \(x) sum(x, na.rm = TRUE)
  )) %>%
  pivot_longer(
    cols = c(Weibchen, Männchen, Molche, Totfunde),
    names_to = "Typ",
    values_to = "Anzahl"
  )

ggplot(temperature_plot_data, aes(x = X5_Temperatur_in_Cels, y = Anzahl, color = Typ)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Einfluss der (geschätzten) Temperatur", x = "°C", y = "Anzahl", color = "Typ") +
  scale_color_manual(values = c("Weibchen" = "deeppink", "Männchen" = "blue", "Molche" = "darkgreen", "Totfunde" = "grey"))

ggsave("Funde nach Temperatur.png", 
       width = 8, height = 6, dpi = 300, bg = "white")





#Tageszeit

# Create a new data frame to summarize the totals by Tageszeit (Time of Day)
sum_by_tageszeit <- table %>%
  group_by(X3_Tageszeit) %>%
  summarise(Living_Sum = sum(Living_Individuals, na.rm = TRUE),
            Totfunde_Sum = sum(X17_Gesamtzahl_Totfun, na.rm = TRUE))

# Filter out rows where both Living and Totfunde sums are 0
sum_by_tageszeit_filtered <- sum_by_tageszeit %>%
  filter(Living_Sum > 0 | Totfunde_Sum > 0)

# Reshape the data for easier plotting (long format)
sum_by_tageszeit_long <- sum_by_tageszeit_filtered %>%
  pivot_longer(cols = c("Living_Sum", "Totfunde_Sum"), 
               names_to = "Type", 
               values_to = "Sum")

# Plot using ggplot2 with German labels and filtered data
ggplot(sum_by_tageszeit_long, aes(x = X3_Tageszeit, y = Sum, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bars for both Living and Totfunde
  scale_fill_manual(values = c("Living_Sum" = "green", "Totfunde_Sum" = "red"), 
                    labels = c("Lebendfunde", "Totfunde")) +  # Color the bars and set legend labels
  theme_minimal() +
  labs(
    title = "Funde nach Tageszeit",
    x = "",
    y = "Anzahl",
    fill = "Typ"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate axis labels for readability
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 14),  # Center the title
    legend.position = c(0.9, 0.9),  # Position the legend at the top-left inside the plot
    legend.title = element_blank(),  # Remove the legend title
    legend.background = element_blank()  # Remove the background behind the legend
  )

# Save the plot as a PNG file
ggsave("Funde nach Tageszeit.png", 
       width = 8, height = 6, dpi = 300, bg = "white")  # You can adjust width, height, and dpi as needed


#Gesamtzahlen

# Gesamtzahl lebend gefundene Kröten (weiblich + männlich) - Bismarckstraße und Calsowstraße
total_living_toads <- sum(
  table$X6_Bismarckstr_Weibch,
  table$X7_Bismarckstr_Mnnche,
  table$X8_Reinkeweg_Weibchen,
  table$X9_Reinkeweg_MnnchenA,
  table$X10_CalsowTennis_Weib,
  table$X11_CalsowTennis_Mnnc,
  na.rm = TRUE
)

total_living_female_toads <- sum(
  table$X6_Bismarckstr_Weibch,
  table$X8_Reinkeweg_Weibchen,
  table$X10_CalsowTennis_Weib,
  na.rm = TRUE
)

total_living_male_toads <- sum(
  table$X7_Bismarckstr_Mnnche,
  table$X9_Reinkeweg_MnnchenA,
  table$X11_CalsowTennis_Mnnc,
  na.rm = TRUE
)

total_living_molche <- sum(
  table$X13_Bismarckstr_Molch,
  table$X14_Reinkeweg_MolcheA,
  table$X15_CalsowTennis_Molc,
  na.rm = TRUE
)

total_dead_finds <- sum(table$X17_Gesamtzahl_Totfun, na.rm = TRUE)

# Ausgabe der Ergebnisse
cat("Gesamtzahl lebend gefundene Kröten:", total_living_toads, 
    "(Weiblich:", total_living_female_toads, ", Männlich:", total_living_male_toads, ")\n")
cat("Gesamtzahl lebend gefundene Molche:", total_living_molche, "\n")
cat("Gesamtzahl der Totfunde:", total_dead_finds, "\n")


# Liste der relevanten PNG-Dateien in der gewünschten Reihenfolge
image_files <- c(
  "Straßenweise.png",
  "Funddaten.png",
  "Funde nach Temperatur.png",
  "Funde nach Tageszeit.png",
  "Funde nach Feuchtigkeit.png",
  "Lebendfunde nach Erfasser_in.png"
)

# Format the date as DD.MM.YYYY
pdf(paste0("Auswertung_", format(Sys.Date(), "%d.%m.%Y"), ".pdf"), width = 8.27, height = 5.83)

# Jede PNG auf eine neue Seite im PDF einfügen
for (img_file in image_files) {
  if (file.exists(img_file)) {
    img <- readPNG(img_file)
    grid.newpage()
    grid.raster(img)
  } else {
    warning(paste("Datei nicht gefunden:", img_file))
  }
}

dev.off()



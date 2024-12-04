# '#' indicates a comment line (not executed)

# Create a unique identifier for each combination
# 'data$' references a column in the dataframe
# '$Treatment' creates a new column 
# 'paste()' function combines strings
# '`CULTIVAR NAME`' uses backticks for column names with spaces
# 'sep = "_"' specifies the separator between combined strings
data$Treatment <- paste(data$`CULTIVAR NAME`, data$`SPRAY TREATMENT`, sep = "_")

# 'split()' divides the dataframe into list of smaller dataframes
# 'data' is the input dataframe
# '$Treatment' is the column used for splitting
split_data <- split(data, data$Treatment)

# 'lapply()' applies a function to each element of a list
# 'function(df)' defines an anonymous function
# '{ }' contains the function body
# 'audpc()' calculates Area Under Disease Progress Curve
# '$`DISEASE SEVERITY`' references disease severity column
# '$DAP' references days after planting column
audpc_results <- lapply(split_data, function(df) { 
  audpc(df$`DISEASE SEVERITY`, df$DAP) 
})

# 'do.call(rbind)' combines list elements into a dataframe
# 'names(audpc_results)' gets the treatment names
# 'function(treatment)' creates another anonymous function
# 'data.frame()' creates a dataframe for each treatment
audpc_results_df <- do.call(rbind, lapply(names(audpc_results), 
function(treatment) { 
data.frame(Treatment = treatment, AUDPC = audpc_results[[treatment]])
  }))

# 'print()' displays the results
print(audpc_results_df)








# Load the 'agricolae' package
library(agricolae)
library(dplyr)
library(ggplot2)
library(tidyr)



                 ####### for one factor ########
#data
data<-disease_severity_rating_2020_2024

# Split the data by cultivar 
split_data <- split(data, data$`CULTIVAR NAME`) 

# Calculate AUDPC for each cultivar 
audpc_results <- lapply(split_data, function(df) { 
  audpc(df$`Disease Severity %`, df$DAP) }) 

# Combine results
audpc_results_df <- do.call(rbind, lapply(names(audpc_results), 
function(cultivar) { 
data.frame(CULTIVAR = cultivar, AUDPC = audpc_results[[cultivar]]) })) 

print(audpc_results_df)





                 ####### for two factors ########

# Create a unique identifier for each combination of Cultivar and Spray
data$Treatment <- paste(data$`CULTIVAR NAME`, data$`SPRAY TREATMENT`, sep = "_") 

# Split the data by the combination of Cultivar and Spray 
split_data <- split(data, data$Treatment) 

# Calculate AUDPC for each combination 
audpc_results <- lapply(split_data, function(df) { 
  audpc(df$`DISEASE SEVERITY`, df$DAP) }) 

# Combine results 
audpc_results_df <- do.call(rbind, lapply(names(audpc_results), 
function(treatment) { 
  data.frame(Treatment = treatment, AUDPC = audpc_results[[treatment]])
  })) 
print(audpc_results_df)



                     ######## Plotting #######

#prepare data
split_results <- audpc_results_df %>%
  separate(Treatment, 
           into = c("Cultivar", "Spray"), 
           sep = "_")

#bargraph
ggplot(split_results, 
       aes(x = Cultivar, 
           y = AUDPC, 
           fill = Spray)) +
  geom_bar(stat = "identity", 
           position = "dodge") +
  labs(title = "AUDPC by Cultivar and Spray Treatment",
       x = "Cultivar Name",
       y = "Area Under Disease Progress Curve (AUDPC)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#alternative

#boxplot
ggplot(split_results, 
       aes(x = Cultivar, 
           y = AUDPC, 
           color = Spray)) +
  geom_boxplot() +
  theme_minimal()

#violin
ggplot(split_results, 
       aes(x = Cultivar, 
           y = AUDPC, 
           fill = Spray)) +
  geom_violin(position = position_dodge(0.8)) +
  theme_minimal()





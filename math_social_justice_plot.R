#Creating visualization for math and social justice course
#Specifically making visualization for the SUCCESS convention 
#Our graphic is looking specifically at "Availability of Employment-related undoc-resources"

#Load in libraries
library(tidyverse)

#Load in data 
my_data <- read_csv("math_social_justice.csv")

#Clean the data 
new_data <- my_data %>%
  pivot_longer(
    cols = c("prop_resources_for_prospective", "prop_resources_for_current")
  ) %>%
  select(X1, name, value)

class_data <- new_data %>%
  mutate(class_category = case_when(X1 == "Not present" ~ "Not Available",
                                    X1 == "Considered, but no action" ~ "Not Available",
                                    X1 == "Unknown" ~ "Unknown",
                                    X1 == "In development" ~ "In Development",
                                    X1 == "Available, but not sustainable" ~ "Available",
                                    X1 == "Institutionalized" ~ "Available"))

#Creating graph 
#This plot has our original cleaned data 
my_plot <- new_data %>%
  ggplot(aes(x = X1, y = value, fill = name)) +
  geom_col(position = "dodge") +
  labs(
    x = "Availability of Resources",
    y = "Proportion",
    title = "Proportion of College Financial Aid Resources Available for Undocumented Students",
    fill = ""
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 22),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15)
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_fill_discrete(labels = c("Current Students", "Prospective Students"))

#This plot is using the class categories 
class_plot <- class_data %>%
  ggplot(aes(x = class_category, y = value, fill = name)) +
  geom_col(position = "dodge") +
  labs(
    x = "Availability of Resources",
    y = "Proportion",
    title = "Proportion of College Financial Aid Resources Available for Undocumented Students",
    fill = ""
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 22),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15)
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_fill_discrete(labels = c("Current Students", "Prospective Students"))

    


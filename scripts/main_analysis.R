# Ebola Sierra Leone analysis
# Christopher Nkanta
# 2026-09-27


# Load packages ----
if(!require(pacman)) install.packages("pacman")
pacman::p_load(
  tidyverse,
  janitor,
  here # new package we will use soon
)

# temporary replacement for the inspectdf package, which was taken off CRAN
pacman::p_load_current_gh("the-graph-courses/inspectdf")



# Load data ----
ebola_sierra_leone <- read_csv(here("data/ebola_sierra_leone.csv"))
view(ebola_sierra_leone)

# Cases by district ----
district_tab <- tabyl(ebola_sierra_leone, district)
arrange_district_tab <- arrange(district_tab, -n)

# Write/save to Output
write_csv(x = arrange_district_tab, 
      file = "output/district tabulation.csv")

# Visualize categorical variables ----
categ_vars_plot<- show_plot(inspect_cat(ebola_sierra_leone))
categ_vars_plot

ggsave(categ_vars_plot, filename = "output/categorical_plot.png",  width = 10, height = 10)
ggsave(categ_vars_plot, filename = "output/categorical_plot.pdf", width = 10, height = 10)


# Visualize numeric variables ----
num_vars_plot <- show_plot(inspect_num(ebola_sierra_leone))
num_vars_plot
ggsave(num_vars_plot, filename = "output/numerical_plot.png",  width = 10, height = 10)



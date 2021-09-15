#-------------#
# Script Init #
#-------------#

### Libraries ###
library(ggplot2)
library(purrr)
library(dplyr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(readr)

### Helpers ###
import::from("helpers/helpers.R", weight, preferences)

### Load Data ###
import::from("DataLoad.R", preferenceData, capacityTbl, mergeData, req_points)

#--------------------#
# Optimization Model #
#--------------------#

# Set random generator seed
set.seed(1234)

# Number of chaperones
n <- length(preferenceData)

# Number of Events
m <- nrow(capacityTbl)

# Required Chaperone Points
p <- capacityTbl$points

# Create capabity vector
capacity <- capacityTbl %>% arrange(id) %>% pull(capacity)

### Optim Model ###
model <-
  MIPModel() %>%
  
  # 1 iff student i is assigned to course m
  add_variable(x[i, j], i = 1:n, j = 1:m, type = "binary") %>%
  
  # maximize the preferences
  set_objective(sum_expr(weight(i, j) * x[i, j], i = 1:n, j = 1:m), sense = "min") %>%
  
  # we cannot exceed the capacity of a course
  add_constraint(sum_expr(x[i, j], i = 1:n) <= capacity[j], j = 1:m)

# Set constraints for chaperone points
for(.x in 1:n){
  model <- 
    model %>% 
    # Each chaperone needs required points for events
    add_constraint(sum_expr(x[i, j] * p[j], j = 1:m) == mergeData %>% filter(i == .x) %>% pull(req_points), i = .x) # %>%
}
  
# Gather results
result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))

#-----------------#
# Analyze Results #
#-----------------#

# Gather and merge optim results with descriptive info
solution <-
  result %>% 
  get_solution(x[i,j]) %>%
  filter(value == 1L) %>%  
  select(i, j) %>% 
  inner_join(mergeData, by = c("i", "j" = "id"))

# Observe name level breakdown
solution %>% group_by(name) %>% summarize(best = min(preference), worst = max(preference), avg = mean(preference)) %>% arrange(desc(best))

# Find people with less than 3 points
solution %>% group_by(name) %>% summarize(sum = sum(points)) %>% filter(sum != 3L)

# Make sure no one breaches capacity
solution %>% group_by(event) %>% summarise(count = n(), capacity = mean(capacity)) %>% filter(count > capacity)

solution %>% 
  group_by(name) %>% 
  group_split() %>% 
  map_df(~{
    ..name <- .x$name %>% unique()
    ..events <- mergeData %>% filter(name == ..name) %>% pull(event)
    ..res <- .x$event %in% ..events
    .x %>% bind_cols(tibble(in_perf = ..res))
  }) %>% 
  filter(in_perf == FALSE)

### Do all events have people ###
capacityTbl %>% 
  inner_join(
    solution %>% group_by(event) %>% summarize(count = n())
  ) %>% 
  filter(count == 0)

### Average Capacity Selected ###
solution %>%
  group_by(name) %>% 
  summarize(avg_capacity = mean(capacity)) %>% 
  arrange(avg_capacity)

### Popularity of events ###
solution %>%
  inner_join(capacityTbl) %>% 
  group_by(event) %>% 
  summarize(avg = mean(preference), count = n()) %>% 
  arrange(avg)

# Breakdown by preference
solution %>%
  group_by(preference) %>%
  summarise(count = n()) %>% 
  mutate(alloc = count / sum(count))

# Build plot data
plot_data <- 
  solution %>%
  mutate(event = factor(event), preference = factor(preference)) %>%
  group_by(event, preference) %>%
  summarise(count = n()) %>% 
  tidyr::complete(preference, fill = list(count = 0))

# Make event plot
ggplot(plot_data, aes(x = event, y = count, fill = preference)) +
  geom_bar(stat = "identity") + 
  viridis::scale_fill_viridis(discrete = TRUE) + 
  geom_hline(yintercept = 11) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Wrirte result CSV
solution %>% write_csv("solutions.csv")
  
solution

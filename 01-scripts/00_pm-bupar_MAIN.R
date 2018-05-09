# check installed packages and install only necessary ones ####
c_necessary_packages <- c(
  'bupaR',
  'edeaR',
  'processmapR',
  'eventdataR',
  'readr',
  'tidyverse',
  'DiagrammeR',
  'ggplot2',
  'stringr',
  'lubridate'  
)
c_missing_packages <- c_necessary_packages[!(c_necessary_packages %in% installed.packages()[,"Package"])]
if(length(c_missing_packages) > 0) install.packages(c_missing_packages)


# load libraries ####
library(bupaR)
library(edeaR)
library(processmapR)
library(eventdataR)
library(readr)
library(tidyverse)
library(DiagrammeR)
library(ggplot2)
library(stringr)
library(lubridate)

# load BPI Challenge 2017 data set ####
data <- readr::read_csv('./00-data/loanapplicationfile.csv',
                         locale = locale(date_names = 'en',
                                         encoding = 'ISO-8859-1'))

# change timestamp to date var
data$starttimestamp = as.POSIXct(data$`Start Timestamp`, 
                                 format = "%Y/%m/%d %H:%M:%S")

data$endtimestamp = as.POSIXct(data$`Complete Timestamp`, 
                               format = "%Y/%m/%d %H:%M:%S")

# remove blanks from var names
names(data) <- str_replace_all(names(data), c(" " = "_" , "," = "" ))


# transform data into eventlog
events <- bupaR::activities_to_eventlog(
  head(data, n = 10000),
  case_id = 'Case_ID',
  activity_id = 'Activity',
  resource_id = 'Resource',
  timestamps = c('starttimestamp', 'endtimestamp')
)

events <- bupaR::activities_to_eventlog(
  data,
  case_id = 'Case_ID',
  activity_id = 'Activity',
  resource_id = 'Resource',
  timestamps = c('starttimestamp', 'endtimestamp')
)

# statistics eventlog ####

events %>% 
  summary

events %>% 
  activity_frequency(level = "activity") 

events %>% 
  activity_frequency(level = "activity") %>% 
  plot()


# filter all cases where one specific activity was present
events %>% 
  filter_activity_presence(activities = c('A_Cancelled')) %>% 
  activity_frequency(level = "activity") 
  


# process map ####
events %>%
  filter_activity_frequency(percentage = 1.0) %>% # show only most frequent activities
  filter_trace_frequency(percentage = .80) %>%    # show only the most frequent traces
  process_map(render = F) %>% 
  export_graph(file_name = './02-output/01_pm-bupar_process map.png',
               file_type = 'PNG')


# process map - performance ####
events %>%
  filter_activity_frequency(percentage = 1.0) %>% # show only most frequent activities
  filter_trace_frequency(percentage = .80) %>%    # show only the most frequent traces
  process_map(performance(mean, "mins"),
              render = F) %>% 
  export_graph(file_name = './02-output/02_pm-bupar_process map performance.png',
               file_type = 'PNG')


# precedent matrix ####
precedence_matrix <- events %>%
  filter_activity_frequency(percentage = 1.0) %>% # show only most frequent activities
  filter_trace_frequency(percentage = .80) %>%    # show only the most frequent traces
  precedence_matrix() %>% 
  plot()

ggsave('./02-output/03_pm-bupar_process precedence matrix.png', precedence_matrix)
rm(precedence_matrix)


# trace explorer
trace_explorer <- events %>%
  trace_explorer(coverage = 0.5)

ggsave('./02-output/04_pm-bupar_trace explorer.png', trace_explorer, width = 12)
rm(trace_explorer)

# idotted chart
chart <- events %>%
  dotted_chart()

chart

# resource map ####
events %>%
  filter_activity_frequency(percentage = .1) %>% # show only most frequent resources
  filter_trace_frequency(percentage = .8) %>%    # show only the most frequent traces
  resource_map(render = F) %>% 
  export_graph(file_name = './02-output/05_pm-bupar_resource map.png',
               file_type = 'PNG')


# resource matrix ####
resource_matrix <- events %>%
  filter_activity_frequency(percentage = 1.0) %>% # show only most frequent activities
  filter_trace_frequency(percentage = .80) %>%    # show only the most frequent traces
  resource_matrix() %>% 
  plot()

ggsave('./02-output/06_pm-bupar_resource matrix.png', resource_matrix)
rm(resource_matrix)


# process map where one activity was at least once present ####
events %>%
  filter_activity_presence(activities = c('A_Cancelled')) %>% 
  filter_activity_frequency(percentage = 1.0) %>% # show only most frequent activities
  filter_trace_frequency(percentage = .80) %>%    # show only the most frequent traces
  process_map(render = F) %>% 
  export_graph(file_name = './02-output/07_pm-bupar_process map cancelled.png',
               file_type = 'PNG')


# process map where one activity was at least once present in Feb 2016 ####
events %>%
  filter_time_period(interval = c(ymd(20160101), end_point = ymd(20160102)),
                     filter_method = 'start') %>% 
  filter_activity_presence(activities = c('A_Cancelled')) %>% 
  filter_activity_frequency(percentage = 1.0) %>% # show only most frequent activities
  filter_trace_frequency(percentage = .80) %>%    # show only the most frequent traces
  process_map(render = F) %>% 
  export_graph(file_name = './02-output/08_pm-bupar_process map cancelled time intervall.png',
               file_type = 'PNG')


# Conditional Process Analysis ####
events %>%
  filter_activity_frequency(percentage = 1.0) %>% # show only most frequent activities
  filter_trace_frequency(percentage = .80) %>%    # show only the most frequent traces
  throughput_time('log', units = 'hours')

events %>%
  filter_activity_frequency(percentage = 1.0) %>% # show only most frequent activities
  filter_trace_frequency(percentage = .80) %>%    # show only the most frequent traces
  throughput_time('case', units = 'hours')

  
events %>%
  filter_activity_frequency(percentage = 1.0) %>% # show only most frequent activities
  filter_trace_frequency(percentage = .80) %>%    # show only the most frequent traces
  group_by(`(case)_ApplicationType`) %>% 
  throughput_time('log', units = 'hours')
  
plot <- events %>%
  filter_activity_frequency(percentage = 1.0) %>% # show only most frequent activities
  filter_trace_frequency(percentage = .80) %>%    # show only the most frequent traces
  group_by(`(case)_ApplicationType`) %>% 
  throughput_time('log', units = 'hours') %>% 
  plot()

plot

ggsave('./02-output/08_pm-bupar_throughput application type.png', plot)
rm(plot)

events %>%
  filter_activity_frequency(percentage = 1.0) %>% # show only most frequent activities
  filter_trace_frequency(percentage = .80) %>%    # show only the most frequent traces
  group_by(`(case)_LoanGoal`) %>% 
  throughput_time('log', units = 'hours')

plot <- events %>%
  filter_activity_frequency(percentage = 1.0) %>% # show only most frequent activities
  filter_trace_frequency(percentage = .80) %>%    # show only the most frequent traces
  group_by(`(case)_LoanGoal`) %>% 
  throughput_time('log', units = 'hours') %>% 
  plot()

plot

ggsave('./02-output/09_pm-bupar_throughput loan goal.png', plot)
rm(plot)

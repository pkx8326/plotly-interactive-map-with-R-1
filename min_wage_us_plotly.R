library(plotly)
library(tidyverse)

#read file into a dataframe
minwage_df <- read_csv("Minimum Wage Data.csv")

#view minimum wage dataframe
View(minwage_df)

#filter only 3 relevant columns
minwage_df <- minwage_df %>%
  select(Year, State, State.Minimum.Wage)

View(minwage_df)

#change a column name
minwage_df <- minwage_df %>%
  mutate(Wage=State.Minimum.Wage) %>%
  select(-State.Minimum.Wage)

View(minwage_df)

#read file with state codes information
statecodes <- read_csv("state codes.csv")

View(statecodes)

#Joining dataframes (statecodes and minwage_df)
#select only state codes "Code" because plotly only works with 2-letter abbrivation of the state code
minwage_df <- minwage_df %>%
  inner_join(statecodes, by.x = State, by.y = State) %>%
  select(Year, Code, Wage)

View(minwage_df)

#creating map
minwage_map <- plot_geo(minwage_df,
                        locationmode = "USA-states",
                        frame = ~Year) %>%
  add_trace(locations = ~Code,
            z = ~Wage,
            color = ~Wage)
minwage_map

#at this point, we have the "world map", but we just want the map of the US.

#we need to add another layer with 'layout()'

minwage_map <- plot_geo(minwage_df,
                        locationmode = "USA-states",
                        frame = ~Year) %>%
  add_trace(locations = ~Code,
            z = ~Wage,
            color = ~Wage) %>%
  layout(geo = list(scope = 'usa'))
minwage_map

#Change the min and max wage scale (reflected in the legend)
minwage_map <- plot_geo(minwage_df,
                        locationmode = "USA-states",
                        frame = ~Year) %>%
  add_trace(locations = ~Code,
            z = ~Wage,
            zmin = 0,
            zmax = max(minwage_df$Wage),
            color = ~Wage,
            colorscale = 'Magma', reversescale=T) %>%
  layout(geo = list(scope = 'usa'))
minwage_map

#add more information to pop-up labels
#must create a new column with full state names 

minwage_df <- minwage_df %>%
  inner_join(statecodes, by.x = State, by.y = State) %>%
  select(Year, State, Code, Wage) %>%
  mutate(popup_labels = paste0(State,"\n$",Wage))

View(minwage_df)

#Create the map again from the modified data
minwage_map <- plot_geo(minwage_df,
                        locationmode = "USA-states",
                        frame = ~Year) %>%
  add_trace(locations = ~Code,
            z = ~Wage,
            zmin = 0,
            zmax = max(minwage_df$Wage),
            color = ~Wage,
            colorscale = 'Magma', reversescale=T,
            text = ~popup_labels) %>%
  layout(geo = list(scope = 'usa'))
minwage_map

#Still, there's too much information in the pop-up (hover info. We need to reduce this information)

minwage_map <- plot_geo(minwage_df,
                        locationmode = "USA-states",
                        frame = ~Year) %>%
  add_trace(locations = ~Code,
            z = ~Wage,
            zmin = 0,
            zmax = max(minwage_df$Wage),
            color = ~Wage,
            colorscale = 'Magma', reversescale=T,
            text = ~popup_labels,
            hoverinfo = 'text') %>%
  layout(geo = list(scope = 'usa'))

minwage_map

#Changing aesthetics
#Changing background color and transparency

labs = list(
  bgcolor = "#EEEEEE",
  bordercolor = "black"
)

minwage_map <- plot_geo(minwage_df,
                        locationmode = "USA-states",
                        frame = ~Year) %>%
  add_trace(locations = ~Code,
            z = ~Wage,
            zmin = 0,
            zmax = max(minwage_df$Wage),
            color = ~Wage,
            colorscale = 'RdBu', reversescale=T,
            text = ~popup_labels,
            hoverinfo = 'text') %>%
  layout(geo = list(scope = 'usa'),
         title = "Minimum Wage ($/hour) in the US\n1968 - 2017") %>%
  style(hoverlabel = labs) %>%
  config(displayModeBar = F) %>%
  colorbar(tickprefix = "$")

minwage_map

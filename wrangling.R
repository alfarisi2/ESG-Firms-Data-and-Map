library(tidyverse)
library(readxl)
library(plotly)
library(gt)
library(DT)
library(reactable)
library(htmltools)
library(leaflet)
library(sf)
library(ggtext)
library(fontawesome)
sysfonts::font_add(family = "Font Awesome 6 Brands",
                   regular = "C:/Users/MyBook Hype AMD/OneDrive - Islamic Online University/learning R/Font Awesome 6 Free-Regular-400.otf")
showtext::showtext_auto()
sysfonts::font_families()
showtext::showtext_auto()


esg <- read_excel("ESGFiDa.1.2.1.xlsx", 
                  sheet = "ESG information firms")

glimpse(esg)
esg |> count(is.na(esg))

esg <- esg |> 
  mutate(Name_ESG = factor(Name_ESG),
         Type = factor(Type),
         Country = factor(Country),
         Continent = factor(Continent),
         Year_Founded = as.numeric(Year_Founded),
         Headquarters = factor(Headquarters))

esg_country_count <- esg|> 
  count(Country, sort = TRUE)


# Generate a vector of distinct colors
library(scales)
dynamic_colors <- hue_pal()(length(unique(esg_country_count$Country)))

esg_country_count |> 
  ggplot(aes(x = reorder(Country, n), y = n, fill = Country)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = n), vjust = -0.5, size = 4) +
  scale_fill_manual(values = dynamic_colors) +
  theme_bw()+
  labs(title = "Number of ESG Firms Based on Country",
       x = "Country",
       y = "Number") +
  theme(legend.position = "none")

esg_type_count <- 
  esg |> 
  count(Type, sort = TRUE)

esg_type_count |> 
  ggplot(aes(x = reorder(Type, n), y = n)) +
  geom_segment(aes(x = reorder(Type, n), xend = reorder(Type, n), y = 0, yend = n), color = "darkgreen", linewidth = 1) +
  geom_point(size = 7.5, color = "red") +
  geom_text(aes(label = n), size = 3.5, color = "white", 
            hjust = 0.5, vjust = 0.4,
            fontface = "bold") +
  theme_minimal() +
  labs(
    title = "Type of ESG Firms",
    x = "Type",
    y = "Count",
    caption = "GitHub: <span style='font-family: \"Font Awesome 6 Brands\"; font-weight: 900;'>&#xf09b;</span> alfarisi2"
  ) + 
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 80, by = 20)) +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_markdown(hjust = 0), element_text(family = "Font Awesome 6 Brands")) +
  coord_flip()

esg_country_count |> 
  ggplot(aes(x = reorder(Country,n), y = n )) +
  geom_segment(aes(x = reorder(Country, n), xend = reorder(Country, n), y = 0, yend = n),color = "darkgreen", linewidth = 1) +
  geom_point(size = 7.5, color = "red") +
  geom_text(aes(label = n), hjust = 0.5, vjust = 0.4, size = 3.5,
            color = "white", fontface = "bold") +
  theme_minimal() +
  labs(
    title = "ESG Firms Based on Country",
    x = "Country",
    y = "Count",
    caption = "source: DOI 10.17605/OSF.IO/B7ZGD - graph: alfarisirm.bsky.social") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

esg_continent_perc <- 
  esg |> count(Continent) |> 
  mutate(percentage = n / sum(n) * 100,
         Label = paste0(round(percentage, 2), "%"))

esg_continent_perc |> 
  ggplot(aes(x = "", y = percentage, fill = Continent)) +
  geom_col(width = 1) +
  geom_text(aes(label = Label),
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set2") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Percentage of ESG Firms by Continent") +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))
#donut    
esg_continent_perc |> 
  ggplot(aes(x = 2, y = percentage, fill = Continent)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = Label),
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Dark2") +
  theme_void() +
  labs(title = "Percentage of ESG Firms by Continent") +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  xlim(1, 2)

print(esg_continent_perc)  



esg_continent_perc <- esg_continent_perc |> 
  mutate(end_angle = cumsum(percentage) * 2 * pi / 100,
         start_angle = lag(end_angle, default = 0))

# Create the donut chart
esg_continent_perc |> 
  ggplot(aes(x = 2, y = percentage, fill = Continent)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(theta = "y") +
  xlim(0.0625, 2.5) + # Adjust the limits to create the donut hole
  theme_void() +
  theme(legend.title = element_blank(),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = Label), 
            position = position_stack(vjust = 0.5), size = 4, color = "white") +
  labs(title = "Percentage of ESG Firms by Continent") +
  scale_fill_brewer(palette = "Dark2")
  
  
#line

esg_year <- 
  esg |> 
  group_by(Year_Founded) |> 
  summarise(Count = n())

if (is.factor(esg_year$Year_Founded)) {
  esg_year$Year_Founded <- as.character(esg_year$Year_Founded)
  esg_year$Year_Founded <- as.numeric(esg_year$Year_Founded)
} else if (is.character(esg_year$Year_Founded)){
  esg_year$Year_Founded <- as.numeric(esg_year$Year_Founded)
}


str(esg_year)

esg_year$by_decade <- floor(esg_year$Year_Founded / 10) * 10

esg_year |> 
  ggplot(aes(Year_Founded, Count)) +
  geom_line(color = "green", linewidth = 1) +
  geom_point(size = 2, color = "darkgreen") +
    labs(title = "Number of Firms Each Year",
       x = "Year",
       y = "Number") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

esg_decade <- 
  esg_year |> 
  group_by(by_decade) |> 
  summarise(Num = n())

esg_decade$label <- paste0(esg_decade$by_decade, "\n ", esg_decade$Num)

esg_decade |> 
  ggplot(aes(by_decade, Num)) +
  geom_line(color = "green", linewidth = 1) +
  geom_point(size = 2, color = "red") +
  scale_y_continuous(breaks = c(1:10)) +
  geom_text(aes(label = label)) +
  labs(title = "Number of ESG-Related Firms Established Until 2020s",
       x = "Decade",
       y = "Number") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

lineplot <- esg_decade |> 
  ggplot(aes(by_decade, Num)) +
  geom_line(color = "#2ca02c", linewidth = 1) +
  geom_point(size = 2, color = "red") +
  scale_y_continuous(breaks = c(1:10)) +
    labs(title = "Number of ESG-Related Firms Established Until 2020s",
       x = "Decade",
       y = "Number") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

lineplot <- ggplotly(lineplot)
lineplot

#table
esg_arr <- esg |> 
  select(Name_ESG, Headquarters, Year_Founded) |> 
  arrange(Year_Founded)
glimpse(esg_arr)

No <- 1:143

esg_arr <- cbind(No, esg_arr)


gt_table <- gt(esg_arr) |> 
  tab_header(title = md("**ESG-Related Firms**"),
             subtitle = md("*A List From 1846 To 2021*")) |> 
  tab_style(style = list(cell_fill(color = "#7fbf7b"),
                         cell_text(color = "#f7f7f7")),
            locations = cells_title(groups = c("title", "subtitle"))) |> 
  tab_style(style = list(cell_fill(color = "white"),
                         cell_text(color = "black", weight = "bold", style = "italic")),
            locations = cells_column_labels(everything())) |> 
  cols_label(Name_ESG = "Name",
             Year_Founded = "Year Founded") |> 
  cols_width(No ~ px(40),
             Name_ESG ~ px(200),
             Headquarters ~ px(150),
             `Year_Founded` ~ px(120)) |> 
  tab_style(style = cell_text(align = "center"),
            locations = cells_body(columns = c(No, `Year_Founded`))) |> 
  tab_style(cell_fill(color = "white"),
            locations = cells_body()) |> 
  cols_align(align = "left",
             columns = everything())

# Make the table interactive
gt_table <- gt_table |> opt_interactive()

# Custom CSS to center the entire table
custom_css <- "
.container {
  display: flex;
  justify-content: center;
  width: 100%;
}
.gt_table {
  margin: 0 auto;
}
"

# Wrap the gt table in a div with the custom CSS class
centered_table <- div(
  class = "container",
  style = "width: 100%; text-align: center;",
  gt_table
)

# Render the table with the custom CSS
htmltools::browsable(
  tagList(
    tags$style(custom_css),
    centered_table
  )
)
#caption jangan lupa

esg_arr |> gt() |> 
  tab_header(title = md("**ESG-Related Firms**"),
             subtitle = md("*A List From 1846 To 2021*")) |> 
  tab_style(style = list(cell_fill(color = "#7fbf7b"),
                         cell_text(color = "#f7f7f7")),
            locations = cells_title(groups = c("title", "subtitle"))) |> 
  tab_style(style = list(cell_fill(color = "white"),
                         cell_text(color = "black", weight = "bold", style = "italic")),
            locations = cells_column_labels(everything())) |> 
  cols_label(Name_ESG = "Name",
             Year_Founded = "Year Founded") |> 
  tab_style(style = cell_text(align = "center"),
            locations = cells_column_labels(everything()) ) |> 
  tab_style(style = cell_text(align = "center"),
            locations = cells_body(columns = c(No, `Year_Founded`))) |> 
  tab_style(cell_fill(color = "white"),
            locations = cells_body()) |> 
  cols_align(align = "left",
             columns = everything()) |> 
  opt_interactive()

city_map <- read_excel("D:/dataset/osf/esgfida/city.xlsx", 
                   sheet = "Sheet2")
city_map$City <- factor(city_map$City)
city_map$id <- 1:nrow(city_map)
city_map <- city_map |> 
  select(id, everything())
str(city_map)

headquarter <- esg |> count(Headquarters)
str(headquarter)

city_coordinate <- 
  city_map |> 
  group_by(City, Latitude, Longitude) |> 
  count()
  
merged_data <- left_join(headquarter, city_coordinate, by=c("Headquarters" = "City"))

merged_data_sf <- st_as_sf(merged_data, coords = c("Longitude", "Latitude"), crs = 4326)

icons <- awesomeIcons(  icon = 'fa-solid fa-map-marker',
                        iconColor = 'black',
                        library = 'fa',
                        markerColor = "red")

fa_icon_html <- "<i class='fa-solid fa-hands-holding-child' style='color: black; font-size: 20px;'></i>"

leaflet(merged_data_sf) |> 
  addTiles() |> 
    addMarkers(
    #label = ~merged_data$Headquarters,
    #labelOptions = labelOptions(noHide = FALSE,
     #                           direction = "top",
      #                          textsize = "10px"),
    icon = makeIcon(iconUrl = fa_icon_html, iconWidth = 24, iconHeight = 24),
    clusterOptions = markerClusterOptions(),
    popup = ~paste("City:", merged_data$Headquarters,
                   "- No of ESG Firms:", merged_data$n.x)) |> 
  setView(lng = mean(merged_data_sf$geometry[[1]][1]),
          lat = mean(merged_data_sf$geometry[[1]][1]), zoom = 1)


leaflet(merged_data_sf) |> 
  addTiles() |> 
  addAwesomeMarkers(icon = icons,
      #label = ~merged_data$Headquarters,
    #labelOptions = labelOptions(noHide = FALSE,
    #                           direction = "top",
    #                          textsize = "10px"),
    clusterOptions = markerClusterOptions(),
    popup = ~paste("City:", merged_data$Headquarters,
                   "- No of ESG Firms:", merged_data$n.x)) |> 
  setView(lng = mean(merged_data_sf$geometry[[1]][1]),
          lat = mean(merged_data_sf$geometry[[1]][1]), zoom = 1)

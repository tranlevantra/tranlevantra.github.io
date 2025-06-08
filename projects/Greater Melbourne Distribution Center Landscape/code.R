library(dplyr)
j

#read excel file
code <- read_excel("/Users/tranlevantra/Desktop/meshblock.xlsx")
code |> filter(STATE_NAME_2021== "Victoria") |> 
  select(MB_CODE_2021, SA1_CODE_2021) -> code


vic_sheets <- c("Table 2", "Table 2.1")

meshblock_count <- map_df(
  vic_sheets,
  ~ read_excel("/Users/tranlevantra/Desktop/Mesh Block Counts, 2021.xlsx", sheet = .x, skip = 6)
)

meshblock_count |> select(MB_CODE_2021, Person) -> meshblock_count

meshblock_count |> str()

SA1_shapefile <- read_sf("/Users/tranlevantra/Desktop/SA1/SA1_2021_AUST_GDA2020.shp")
SA1_shapefile |> filter(STE_NAME21 == "Victoria") |> 
  rename(SA1_CODE_2021 = SA1_CODE21) |>
  select(SA1_CODE_2021) -> SA1_shapefile


SA1_shapefile |> filter(!st_is_empty(geometry)) -> SA1_shapefile
code
meshblock_count
SA1_shapefile

# Join the dataframes
code |> 
  left_join(meshblock_count, by = "MB_CODE_2021") |> 
  group_by(SA1_CODE_2021) |> 
  summarise(Person = sum(Person, na.rm = TRUE)) |> 
  left_join(SA1_shapefile, by = "SA1_CODE_2021") |> 
  st_as_sf() -> SA1_count_shapefile

#using purr to st_sample 
SA1_count_shapefile

SA1_count_shapefile <- read_sf("projects/Greater Melbourne Distribution Center Landscape/resources/SA1_count_shapefile/SA1_count_shapefile.shp")
points_sf <- read_sf("projects/Greater Melbourne Distribution Center Landscape/resources/points_sf/points_sf.shp")
fullfillmentCenter_sf <- read_sf("projects/Greater Melbourne Distribution Center Landscape/resources/fullfillmentCenter_sf/fullfillmentCenter_sf.shp")

SA1_count_shapefile

SA1_count_shapefile |> 
  mutate(n_dots = floor(Person / 100))  |> 
  filter(n_dots > 0) |> st_sample(size = n_dots)


ggplot() +
  geom_sf(data = SA1_count_shapefile) +
  geom_sf(data = points_sf) +
  theme_minimal() +
  labs(title = "Population Density in Victoria",
       fill = "Population") +
  theme(legend.position = "bottom")

ggplot() +
  geom_sf(data = SA1_count_shapefile, fill = "grey95", color = "white", size = 0.1) +
  geom_sf(data = points_sf, color = "orange", size = 0.3, alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Population Density in Victoria",
    subtitle = "Each <span style='color:orange'>&#9679;</span> = 100 people",
    caption = "Source: ABS Census Data 2021",
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 8, hjust = 1)
  )


library(ggplot2)
library(ggtext)  # allows HTML in text elements

ggplot() +
  geom_sf(data = SA1_count_shapefile, fill = "grey95", color = "white", size = 0.1) +
  geom_sf(data = points_sf, color = "orange", size = 0.3, alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Population Density in Victoria",
    subtitle = "Each <span style='color:orange'>&#9679;</span> = 100 people",
    caption = "Source: ABS Census Data 2021"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_markdown(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 9, hjust = 1)
  )

SA1_count_shapefile |>                          # <1>
  mutate(n_dots = floor(Person / 100))  |>     
  filter(n_dots > 0) |>  st_sample(size =SA1_count_shapefile$n_dots) |>  # <2>
  st_sf() -> points_sf


SA1_count_shapefile |> 
  mutate(n_dots = floor(Person / 100))  |>
  filter(n_dots > 0) -> SA1_count_shapefile

st_sample(SA1_count_shapefile, size = SA1_count_shapefile$n_dots) -> points_sf

st_sample(SA1_count_shapefile[1:10,], size = SA1_count_shapefile$n_dots[1:10]) -> points_sf

points_sf |> st_sf()



# analysis ----------------------------------------------------------------

# Create the map
map <- leaflet() %>%
  addProviderTiles("OpenStreetMap") %>%
  fitBounds(
    lng1 = 144.72903, lat1 = -38.05242,
    lng2 = 145.23470, lat2 = -37.57800
  ) |>
  
  # Population layer
  leafgl::addGlPoints(
    data = points_sf,
    fillColor = "orange",
    fillOpacity = 0.5,
    radius = 2,
    group = "Population Density Dots"
  ) |>
  
  addCircleMarkers(
    data = fullfillmentCenter_sf,
    group = "After COVID",
    radius = ~dplyr::case_when(
      Area == "Small" ~ 4,
      Area == "Medium" ~ 6,
      Area == "Large" ~ 8,
      TRUE ~ 10
    ),
    color = ~dplyr::case_when(
      Type == "inhouse" ~ "red",
      Type == "3PL" ~ "navy",
      TRUE ~ "grey"  
    ),
    stroke = TRUE,
    weight = 0.1,
    fillOpacity = 0.5,
    label = ~Name,
    popup = ~paste0(
      "<b>Name:</b> ", Name, "<br>",
      "<b>Size:</b> ", Area, "<br>",
      "<b>Since:</b> ", Since, "<br>",
      "<b>Note:</b> ", Note, "<br>"
    )
  ) |> 
  addCircleMarkers(
    data = fullfillmentCenter_sf %>% dplyr::filter(Since < 2020),
    group = "Before COVID",
    radius = ~dplyr::case_when(
      Area == "Small" ~ 4,
      Area == "Medium" ~ 6,
      Area == "Large" ~ 8,
      TRUE ~ 10  
    ),
    color = ~dplyr::case_when(
      Type == "inhouse" ~ "red",
      Type == "3PL" ~ "navy",
      TRUE ~ "grey" 
    ),
    stroke = TRUE,
    weight = 0.1,
    fillOpacity = 0.5,
    label = ~Name,
    popup = ~paste0(
      "<b>Name:</b> ", Name, "<br>",
      "<b>Size:</b> ", Area, "<br>",
      "<b>Since:</b> ", Since, "<br>",
      "<b>Note:</b> ", Note, "<br>"
    )
  ) |> 
  
  addLegend(
    position = "bottomright",
    colors = c("Inhouse" = "red", "3PL" = "navy"),
    labels = c("Inhouse", "3PL"),
    title = "Warehouse Type"
  ) |>
  
  # Custom control with unique ID
  addControl(
    html = "<div id='dotKey' style='background:white; padding:4px; font-size:12px; border-radius:4px; display:none;'>
              <span style='display:inline-block; width:10px; height:10px; background:orange; border-radius:50%; margin-right:4px;'></span> = 100 persons
            </div>",
    position = "bottomleft"
  ) |>
  
  addLayersControl(
    baseGroups = c("After COVID", "Before COVID"),
    overlayGroups = c("Population Density Dots"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Add JS interactivity: toggle control based on checkbox
map <- htmlwidgets::onRender(m, "
  function(el, x) {
    var map = this;
    map.on('overlayadd', function(e) {
      if (e.name === 'Population Density Dots') {
        document.getElementById('dotKey').style.display = 'block';
      }
    });
    map.on('overlayremove', function(e) {
      if (e.name === 'Population Density Dots') {
        document.getElementById('dotKey').style.display = 'none';
      }
    });
  }
")

m


leaflet() %>%
  addProviderTiles("OpenStreetMap") %>%
  fitBounds(
    lng1 = 144.72903, lat1 = -38.05242,
    lng2 = 145.23470, lat2 = -37.57800
  ) |>
  
  # Population layer
  leafgl::addGlPoints(
    data = points_sf,
    fillColor = "rgba(255,165,0,0.5)",
    fillOpacity = 0.5,
    radius = 2,
    group = "Population Density Dots"
  ) |>
  
  # Fulfillment Center markers
  addCircleMarkers(
    data = fullfillmentCenter_sf,
    group = "After COVID",
    radius = ~dplyr::case_when(
      Area == "Small" ~ 4,
      Area == "Medium" ~ 6,
      Area == "Large" ~ 8,
      TRUE ~ 10
    ),
    color = ~dplyr::case_when(
      Type == "inhouse" ~ "red",
      Type == "3PL" ~ "navy",
      TRUE ~ "grey"
    ),
    stroke = TRUE,
    fillOpacity = 0.5,
    label = ~Name,
    popup = ~paste0("<b>Name:</b> ", Name, "<br>",
                    "<b>Size:</b> ", Area, "<br>",
                    "<b>Since:</b> ", Since, "<br>",
                    "<b>Note:</b> ", Note, "<br>")
  ) |>
  
  addCircleMarkers(
    data = fullfillmentCenter_sf %>% dplyr::filter(Since < 2020),
    group = "Before COVID",
    radius = ~dplyr::case_when(
      Area == "Small" ~ 4,
      Area == "Medium" ~ 6,
      Area == "Large" ~ 8,
      TRUE ~ 10
    ),
    color = ~dplyr::case_when(
      Type == "inhouse" ~ "red",
      Type == "3PL" ~ "navy",
      TRUE ~ "grey"
    ),
    stroke = TRUE,
    fillOpacity = 0.5,
    label = ~Name,
    popup = ~paste0("<b>Name:</b> ", Name, "<br>",
                    "<b>Size:</b> ", Area, "<br>",
                    "<b>Since:</b> ", Since, "<br>",
                    "<b>Note:</b> ", Note, "<br>")
  ) |>
  
  addLegend(
    position = "bottomright",
    colors = c("Inhouse" = "red", "3PL" = "navy"),
    labels = c("Inhouse", "3PL"),
    title = "Warehouse Type"
  ) |>
  addControl(
    html = "
  <div id='dotKey' style='background:white; padding:4px; font-size:12px; border-radius:4px; display:none;'>
    <span style='display:inline-block; width:10px; height:10px; background:orange; border-radius:50%; margin-right:4px;'></span> = 100 persons
  </div>
  <script>
    function toggleDotKeyControl(e) {
      var layerName = e.name;
      var dotKey = document.getElementById('dotKey');
      if (!dotKey) return;
      if (layerName === 'Population Density Dots') {
        if (e.type === 'overlayadd') {
          dotKey.style.display = 'block';
        } else if (e.type === 'overlayremove') {
          dotKey.style.display = 'none';
        }
      }
    }

    // Wait for the map to be fully loaded
    document.addEventListener('DOMContentLoaded', function() {
      var mapInterval = setInterval(function() {
        if (typeof window.Leaflet !== 'undefined' || typeof window.L !== 'undefined') {
          var map = document.querySelector('.leaflet-container')?._leaflet_map;
          if (map) {
            map.on('overlayadd', toggleDotKeyControl);
            map.on('overlayremove', toggleDotKeyControl);
            clearInterval(mapInterval);
          }
        }
      }, 200);
    });
  </script>",
    position = "bottomleft"
  )|>
  
  addLayersControl(
    baseGroups = c("After COVID", "Before COVID"),
    overlayGroups = c("Population Density Dots"),
    options = layersControlOptions(collapsed = FALSE)
  )




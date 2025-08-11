library(dplyr)
library(ggplot2)
library(readr) 
library(plotly)
library(forcats)

quest1 <- read_csv("data/quest1.csv")
quest3 <- read_csv("data/quest3.csv")




quest1 |>
  filter(grepl("income", country, ignore.case = TRUE)) |>
  ggplot(aes(
    x = DayNO,
    y = `Total Injected People`,
    color = country
  )) +
  geom_line() +
  labs(
    x = "Day",
    y = "Total Injected People",
    title = "Comparison of Vaccination Speed by Income Segment",
    legend = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_blank()
  ) |>
  ggplotly(tooltip = c("x", "y", "color"))
  
(quest1 |>
    filter(grepl("income", country, ignore.case = TRUE)) |>
    ggplot(aes(
      x = DayNO,
      y = `Total Injected People`,
      color = country
    )) +
    geom_line() +
    labs(
      x = "Day",
      y = "Total Injected People",
      title = "Comparison of Vaccination Speed by Income Segment",
      color = ""
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.y = element_blank()
    )) |>
  ggplotly(tooltip = c("x", "y"))




(quest3 |> 
  count(`Vaccine Type`)  |>                                    # get frequency
  mutate(`Vaccine Type` = fct_reorder(`Vaccine Type`, n)) |> # sort by freq
  ggplot(aes(
    x = `Vaccine Type`,
    y = n,
    text = paste("Used by", n, "countries")                   # hover text
  )) +
  geom_col(fill = "#1f78b4") +
  coord_flip() +
  labs(
    x = "",
    y = "",
    title = "Vaccine Type Popularity"
  ) +
  theme(
    axis.text.y = element_text(hjust = 0, margin = margin(r = 10)),
    axis.ticks = element_blank()
  )
) |> ggplotly(tooltip = "text")

ggplotly(p, tooltip = "text")

quest3 |> 
  mutate(`Vaccine Type` = fct_infreq(`Vaccine Type`)) |> 
  ggplot(aes(x = `Vaccine Type`)) +
  geom_bar() +
  coord_flip() +
  labs(x = "", y = "", title = "Vaccine Type Popularity") +
  theme(axis.text.y = element_text(hjust = 0, margin = margin(r = 10)),
        axis.ticks = element_blank())


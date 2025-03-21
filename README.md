# Tugas1
tugas 1 psd b

---
title: "Brexit"
date: "`r Sys.Date()`"
output: html_document
---

```{r load-packages, message = FALSE, echo = FALSE}
library(tidyverse)
```

In September 2019, YouGov survey asked 1,639 GB adults the following question:

> In hindsight, do you think Britain was right/wrong to vote to leave EU?
>
>- Right to leave  
>- Wrong to leave  
>- Don't know

The data from the survey are in `data/brexit.csv`.

```{r message = FALSE}
brexit <- read_csv("data/brexit.csv")
```

In the course video we made the following visualisation.

```{r}
brexit <- brexit %>%
  mutate(
    region = fct_relevel(region, "london", "rest_of_south", "midlands_wales", "north", "scot"),
    region = fct_recode(region, London = "london", `Rest of South` = "rest_of_south", `Midlands / Wales` = "midlands_wales", North = "north", Scotland = "scot")
  )

ggplot(brexit, aes(y = opinion, fill = opinion)) +
  geom_bar() +
  facet_wrap(~region, nrow = 1, labeller = label_wrap_gen(width = 12)) +
  guides(fill = FALSE) +
  labs(
    title = "Was Britain right/wrong to vote to leave EU?",
    subtitle = "YouGov Survey Results, 2-3 September 2019",
    caption = "Source: bit.ly/2lCJZVg",
    x = NULL, y = NULL
  ) +
  scale_fill_manual(values = c(
    "Wrong" = "#ef8a62",
    "Right" = "#67a9cf",
    "Don't know" = "gray"
  )) +
  theme_minimal()
```

In this application exercise we tell different stories with the same data.

### Exercise 1 - Free scales

Add `scales = "free_x"` as an argument to the `facet_wrap()` function. How does the visualisation change? How is the story this visualisation telling different than the story the original plot tells?

```{r}
#answer
library(ggplot2)

brexit <- read_csv("C:/Users/Lenovo/Documents/pengantar sains data (psd)/brexit.csv")

ggplot(brexit, aes(y = opinion, fill = opinion)) +
  geom_bar() +
  facet_wrap(~region,
             nrow = 1, labeller = label_wrap_gen(width = 12),
             scales = "free_x"
  ) +
  guides(fill = FALSE) +
  labs(
    title = "Was Britain right/wrong to vote to leave EU?",
    subtitle = "YouGov Survey Results, 2-3 September 2019",
    caption = "Source: bit.ly/2lCJZVg",
    x = NULL, y = NULL
  ) +
  scale_fill_manual(values = c(
    "Wrong" = "#ef8a62",
    "Right" = "#67a9cf",
    "Don't know" = "gray"
  )) +
  theme_minimal()
```


### Exercise 2 - Comparing proportions across facets

First, calculate the proportion of wrong, right, and don't know answers in each category and then plot these proportions (rather than the counts) and then improve axis labeling. How is the story this visualisation telling different than the story the original plot tells? **Hint:** You'll need the **scales** package to improve axis labeling, which means you'll need to load it on top of the document as well.

```{r}
# code goes here
library(tidyverse)
library(scales)

brexit <- read_csv("C:/Users/Lenovo/Documents/pengantar sains data (psd)/brexit.csv")

brexit_prop <- brexit %>%
  count(region, opinion) %>%
  group_by(region) %>%
  mutate(prop = n / sum(n))

ggplot(brexit_prop, aes(region, prop, fill = opinion)) +
  geom_col() +
  facet_wrap(~region, nrow = 1) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("Wrong" = "#ef8a62", "Right" = "#67a9cf", "Don't know" = "gray")) +
  labs(title = "Was Britain right/wrong to vote to leave EU?",
       subtitle = "Proportion of responses per region (YouGov Survey, Sep 2019)",
       y = "Proportion of Responses") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        strip.text = element_text(size = 12, face = "bold"))
```

### Exercise 3 - Comparing proportions across bars

Recreate the same visualisation from the previous exercise, this time dodging the bars for opinion proportions for each region, rather than faceting by region and then improve the legend. How is the story this visualisation telling different than the story the previous plot tells?

```{r}
# code goes here
library(tidyverse)
library(scales)

brexit <- read_csv("C:/Users/Lenovo/Documents/pengantar sains data (psd)/brexit.csv")

brexit_prop <- brexit %>%
  count(region, opinion) %>%
  group_by(region) %>%
  mutate(prop = n / sum(n))

brexit_prop$region <- factor(brexit_prop$region, levels = unique(brexit_prop$region))

ggplot(brexit_prop, aes(x = region, y = prop, fill = opinion)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +  # Bandingkan opini dalam satu region
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +  # Pastikan 0 - 100%
  scale_fill_manual(values = c("Wrong" = "#ef8a62", "Right" = "#67a9cf", "Don't know" = "gray"),
                    name = "Opinion", labels = c("Wrong Decision", "Right Decision", "Uncertain")) +
  labs(title = "Was Britain right/wrong to vote to leave EU?",
       subtitle = "Proportion of responses per region (YouGov Survey, Sep 2019)",
       x = "Region", y = "Proportion of Responses") +
  theme_minimal() +
  theme(legend.position = "right",  # Pindahkan legenda ke kanan
        legend.title = element_text(face = "bold"),
        axis.text.x = element_text(face = "bold", angle = 45, hjust = 1))
```

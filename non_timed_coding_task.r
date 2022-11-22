library(tidyverse)
library(sf)
library(ggpubr)
library(caret)
library(stargazer)


# Load data from CSV
data_raw <- read_csv("All_years.csv")
# Load State shapefiles for spatial plots
states <- st_read("shapefiles/cb_2018_us_state_20m.shp") %>%
    rename(statefip = STATEFP) %>%
    mutate(statefip = as.numeric(statefip))
race_codes <- read_csv("race_codes.csv")

# Coerce data into dataframe
# Change [2,1] binary variables to [1,0]
# Adjust personal income by inflation
# Rename coverage variables
# Pivot table to long format
data <- data_raw %>%
    mutate(
        hcovany = hcovany - 1,
        hinscaid = hinscaid - 1,
        hinscare = hinscare - 1,
        inctot = inctot / cpi99
    ) %>%
    rename(
        any = hcovany,
        medicaid = hinscaid,
        medicare = hinscare
    ) %>%
    pivot_longer(
        c(any, medicaid, medicare),
        names_to = "coverage_type",
        values_to = "coverage_status"
    )

# Aggregate coverage by year
# Use perseon weights (perwt) for aggreagation
data_by_year <- data %>%
    group_by(year, coverage_type) %>%
    summarise(coverage_rate = sum(coverage_status * perwt) / sum(perwt)) %>%
    ungroup() %>%
    group_by(coverage_type) %>%
    mutate(
        coverage_change = (coverage_rate - lag(coverage_rate)) / lag(coverage_rate), # nolint
        coverage_change_pp = coverage_rate - lag(coverage_rate)
    ) %>%
    ungroup()

# Plot any coverage
line_plot <- ggplot(
    filter(data_by_year, coverage_type == "any"),
    aes(year, coverage_rate)) +
    geom_line() +
    theme_minimal() +
    scale_x_continuous(breaks = 2008:2018) +
    ylab("Proportion of Americans Covered by Health Insurance") # nolint

change_bar_plot <- ggplot(
    filter(data_by_year, coverage_type == "any"),
    aes(year, coverage_change, fill = coverage_change)) +
    geom_bar(stat = "identity", color = "black") +
    theme_minimal() +
    scale_x_continuous(breaks = 2008:2018) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_gradient2(
        low = "red",
        mid = "white",
        high = "blue",
        midpoint = 0
    ) +
    ylab("Change in Heatlh Coverage") +
    theme(legend.position = "none")

figure_1 <- ggarrange(
    line_plot,
    change_bar_plot,
    heights = c(1, 0.5),
    ncol = 1, nrow = 2,
    align = "v"
) %>%
annotate_figure(
    top = text_grob(
        "Health Insurance Coverage Over Time",
        face = "bold",
        size = 14
    ),
    fig.lab = "Figure 1",
    fig.lab.face = "bold"
)

# Plot medicaid and maedicare coverage

medicare_medicaid_line_plot <- ggplot(
    filter(data_by_year, coverage_type != "any"),
    aes(year, coverage_rate)) +
    geom_line(aes(color = coverage_type)) +
    theme_minimal() +
    scale_x_continuous(breaks = 2008:2018) +
    ylab("Proportion of Americans Covered by Medicaid/Medicare") + # nolint
    theme(legend.position = "none")

medicare_medicaid_change_bar_plot <- ggplot(
    filter(data_by_year, coverage_type != "any"),
    aes(year, coverage_change, fill = coverage_type)) +
    geom_bar(stat = "identity", position = position_dodge(), color = "black") +
    theme_minimal() +
    scale_x_continuous(breaks = 2008:2018) +
    scale_y_continuous(labels = scales::percent) +
    ylab("Change in Coverage") +
    scale_fill_discrete(name = "Type of Coverage")

figure_2 <- ggarrange(
    medicare_medicaid_line_plot,
    medicare_medicaid_change_bar_plot,
    heights = c(1, 0.5),
    ncol = 1, nrow = 2,
    align = "v"
) %>%
annotate_figure(
    top = text_grob(
        "Medicaid and Medicare Coverage Over Time",
        face = "bold",
        size = 14
    ),
    fig.lab = "Figure 2",
    fig.lab.face = "bold"
)


# Analysis by race
# Do not look at multi-ethnic, left for later analysis

data_by_race <- data %>%
    filter(!(race %in% c(8, 9))) %>%
    mutate(race_name = ifelse(
        race == 1, "White", ifelse(
            race == 2, "Black", ifelse(
                race == 3, "Native American", ifelse(
                    race %in% (4:6), "Asian / Pacific Islander", "Other"
                )
            )
        )
    )) %>%
    group_by(year, race_name, coverage_type) %>%
    summarise(coverage_rate = sum(coverage_status * perwt) / sum(perwt)) %>%
    ungroup() %>%
    group_by(coverage_type, race_name) %>%
    mutate(
        coverage_change = (coverage_rate - lag(coverage_rate)) / lag(coverage_rate), # nolint
        coverage_change_pp = coverage_rate - lag(coverage_rate)
    ) %>%
    ungroup()

race_any_line <- ggplot(
    filter(data_by_race, coverage_type == "any"),
    aes(year, coverage_rate, color = race_name)) +
    geom_line() +
    theme_minimal() +
    scale_x_continuous(breaks = 2008:2018) +
    ylab("Proportion Covered by any Health Insurance, by Race") +
    scale_color_discrete(name = "Race")
race_any_bar <- ggplot(
    filter(data_by_race, coverage_type == "any"),
    aes(year, coverage_change, fill = race_name)) +
    geom_bar(stat = "identity", color = "black", position = position_dodge()) +
    theme_minimal() +
    scale_x_continuous(breaks = 2008:2018) +
    scale_y_continuous(labels = scales::percent) +
    ylab("Change in Heatlh Coverage") +
    scale_fill_discrete(name = "Race")

race_medicaid_line <- ggplot(
    filter(data_by_race, coverage_type == "medicaid"),
    aes(year, coverage_rate, color = race_name)) +
    geom_line() +
    theme_minimal() +
    scale_x_continuous(breaks = 2008:2018) +
    ylab("Proportion Covered by Medicaid") +
    theme(legend.position = "none")
race_medicaid_bar <- ggplot(
    filter(data_by_race, coverage_type == "medicaid"),
    aes(year, coverage_change, fill = race_name)) +
    geom_bar(stat = "identity", color = "black", position = position_dodge()) +
    theme_minimal() +
    scale_x_continuous(breaks = 2008:2018) +
    scale_y_continuous(labels = scales::percent) +
    ylab("Change in Medicade Coverage") +
    theme(legend.position = "none")

race_medicare_line <- ggplot(
    filter(data_by_race, coverage_type == "medicare"),
    aes(year, coverage_rate, color = race_name)) +
    geom_line() +
    theme_minimal() +
    scale_x_continuous(breaks = 2008:2018) +
    ylab("Proportion Covered by Medicare") +
    theme(legend.position = "none")
race_medicare_bar <- ggplot(
    filter(data_by_race, coverage_type == "medicare"),
    aes(year, coverage_change, fill = race_name)) +
    geom_bar(stat = "identity", color = "black", position = position_dodge()) +
    theme_minimal() +
    scale_x_continuous(breaks = 2008:2018) +
    scale_y_continuous(labels = scales::percent) +
    ylab("Change in Medicare Coverage") +
    theme(legend.position = "none")


figure_3 <- ggarrange(
    ggarrange(
        race_any_line,
        race_any_bar,
        ncol = 1, nrow = 2,
        heights = c(1, 0.5),
        common.legend = TRUE,
        legend = "right"
    ),
    ggarrange(
        race_medicaid_line,
        race_medicare_line,
        race_medicaid_bar,
        race_medicare_bar,
        ncol = 2, nrow = 2,
        heights = c(0.5, 0.5, 0.3, 0.3)
    ),
    heights = c(0.5, 0.5),
    ncol = 1, nrow = 2
) %>%
annotate_figure(
    top = text_grob(
        "Health Care Coverage Over Time, by Race",
        face = "bold",
        size = 14
    ),
    fig.lab = "Figure 3",
    fig.lab.face = "bold"
)

figure_3

data_by_state <- data %>%
    group_by(statefip, coverage_type) %>%
    summarise(
      coverage_rate = sum(coverage_status * perwt) / sum(perwt),
      coverage_change = (sum(ifelse(year == 2018, (coverage_status * perwt) / perwt, 0)) - 
                           sum(ifelse(year == 2008, (coverage_status * perwt) / perwt, 0))) / sum(ifelse(year == 2018, (coverage_status * perwt) / perwt, 0))
    ) %>%
    ungroup()

spatial_states_anycoverage <- states %>%
    left_join(
        filter(data_by_state, coverage_type == "any"),
        by = "statefip"
        ) %>%
    filter(statefip %in% c(01, 04:13, 16:56)) # Filter non-continental states

state_map <- ggplot(spatial_states_anycoverage) +
    geom_sf(aes(fill = coverage_rate)) +
    theme_void() +
    scale_fill_viridis_c(option = "plasma", name = "Coverage Rate \n(Any Insurance)") +
    theme(legend.position = "bottom")

spatial_states_medicaid <- states %>%
    left_join(
        filter(data_by_state, coverage_type == "medicaid"),
        by = "statefip"
        ) %>%
    filter(statefip %in% c(01, 04:13, 16:56)) # Filter non-continental states

state_map_medicaid <- ggplot(spatial_states_medicaid) +
    geom_sf(aes(fill = coverage_rate)) +
    theme_void() +
    scale_fill_viridis_c(option = "plasma", name = "Medicaid \nCoverage Rate") +
    theme(legend.position = "bottom")

spatial_states_medicare <- states %>%
    left_join(
        filter(data_by_state, coverage_type == "medicare"),
        by = "statefip"
        ) %>%
    filter(statefip %in% c(01, 04:13, 16:56)) # Filter non-continental states

state_map_medicare <- ggplot(spatial_states_medicare) +
    geom_sf(aes(fill = coverage_rate)) +
    theme_void() +
    scale_fill_viridis_c(option = "plasma", name = "Medicare \nCoverage Rate") +
    theme(legend.position = "bottom")

state_change_bar <- ggplot(
    st_drop_geometry(spatial_states_anycoverage),
    aes(
        reorder(NAME, coverage_change),
        coverage_change, 
        fill = coverage_change)
    ) +
    geom_bar(stat = "identity", color = "black") +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_viridis_b() +
    ylab("Change in Any Heatlh Coverage") +
    xlab(NULL) +
    theme(legend.position = "none") +
    coord_flip()

figure_4 <- ggarrange(
    ggarrange(
        state_map,
        state_map_medicaid,
        state_map_medicare,
        ncol = 1, nrow = 3
    ),
    state_change_bar,
    ncol = 2, nrow = 1
) %>%
annotate_figure(
    top = text_grob(
        "Heatlh Coverage by State",
        face = "bold",
        size = 14
    ),
    fig.lab = "Figure 4",
    fig.lab.face = "bold"
)
figure_4

#Don't run Logit regression again if you don;t need to, very CPU intensiv
#model_any <- glm(
  #coverage_status ~ as.factor(sex) + age + as.factor(marst) + inctot,
  #family = binomial(link = 'logit'),
  #data = filter(data,coverage_type == "any"))

#model_medicaid <- glm(
  #coverage_status ~ as.factor(sex) + age + as.factor(marst) + inctot,
  #family = binomial(link= 'logit'),
  #data = filter(data,coverage_type == "medicaid"))

#model_medicare <- glm(
#  coverage_status ~ as.factor(sex) + age + as.factor(marst) + inctot,
#  family = binomial(link = 'logit'),
#  data = filter(data,coverage_type == "medicare"))

#stargazer(model_any, model_medicaid, model_medicare)
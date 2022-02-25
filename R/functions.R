read_pollen <- function(species, years = 1979:2019, file = "data/Pollen Stockholm dayly values 1979-2019.xlsx"){
  map_df(species, read_sheet, file = file) %>% 
    filter(year %in% years)
}

read_sheet <- function(species, file = "data/Pollen Stockholm dayly values 1979-2019.xlsx"){
  assertthat::assert_that(species %in% readxl::excel_sheets("data/Pollen Stockholm dayly values 1979-2019.xlsx"),
                          msg = paste(species, "in not a sheet in ", file))
  suppressMessages(readxl::read_excel(file, sheet = species)) %>% 
    mutate(day = lubridate::yday(...1)) %>% 
    select(day, `2019`:`1979`) %>% 
    pivot_longer(`2019`:`1979`, names_to = "year") %>% 
    mutate(value = replace_na(value, 0), year = as.numeric(year), species = species) %>% 
    arrange(year, day)
}

ridge_plot <- function(data, species, scaling = 3){
  .species <- species
  data %>% 
    mutate(year = as.factor(year)) %>% 
    filter(value > 0, species %in% .species) %>% 
    group_by(species) %>% 
    mutate(height = scaling * value / max(value)) %>% 
    ungroup() %>% 
    ggplot(aes(x = day, y = year, height = height)) + 
    ggridges::geom_ridgeline() + ggridges::theme_ridges() +
    facet_wrap(~species) + scale_y_discrete(breaks = seq(1975, 2019, by = 5))
}

tile_plot <- function(data, species){
  .species <- species
  data %>% 
    mutate(year = as.factor(year)) %>% 
    filter(value > 0, species %in% .species) %>% 
    group_by(species) %>% 
    mutate(value =  value / max(value)) %>% 
    ungroup() %>% 
    ggplot(aes(x = day, y = year, fill = log(value))) + 
    geom_tile(show.legend = FALSE) + ggridges::theme_ridges() +
    facet_wrap(~species) + scale_y_discrete(breaks = seq(1975, 2019, by = 5)) +
    scale_fill_gradient(low = "yellow", high = "red")
}

season_summaries <- function(data, species){
  data %>% filter(value > 0) %>% 
    group_by(year, species) %>% 
    summarise(first_day = min(day),
              high_start = min(day[value > max(value)/10]),
              mean_day = sum(day * value) / sum(value),
              median_day = min(day[cumsum(value) > sum(value)/2]),
              season = max(day)-min(day),
              high_season = sum((value > max(value)/10)), 
              .groups = "drop") %>% 
    ungroup() %>% 
    pivot_longer(first_day:high_season, names_to = "measure") %>% 
    arrange(species, year)
}

trend_plot <- function(summaries, species, measures){
  .species <- species
  .measures <- measures
  summaries %>% 
    filter(species %in% .species,
           measure %in% .measures) %>% 
    ggplot(aes(x = year, y = value, color = measure)) + 
    geom_point(alpha = .3) + 
    geom_smooth(se = FALSE, method = "lm", formula = "y ~ x") + 
    facet_wrap(~species) +
    theme_bw()
}

trend_table <- function(summaries, species, measures){
  .species <- species
  .measures <- measures
  summaries %>% 
    filter(species %in% .species,
           measure %in% .measures) %>% 
    mutate(year = year - mean(year)) %>% 
    nest_by(species, measure) %>% 
    mutate(linear_fit = list(lm(value ~ year, data = data) %>% summary()),
           mean = linear_fit$coefficients["(Intercept)", "Estimate"],
           trend = linear_fit$coefficients["year", "Estimate"],
           p_value = linear_fit$coefficients["year", "Pr(>|t|)"]) %>% 
    select(-data, -linear_fit) %>% 
    ungroup()
}

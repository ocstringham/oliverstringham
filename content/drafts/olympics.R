library(tidyverse)

olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

olympics %>% count(team, sort = TRUE)

# explore last names
df = olympics %>% 
  filter(team %in% c("United States", "Great Britain", "Australia")) %>% 
  mutate(name = str_remove(name, ",\\sJr\\.$|,\\sSr.$")) %>% # rm Jr. and Sr.
  mutate(name = str_remove(name, ",\\s[I]+[V]*$")) %>%  # rm II, III, IV, Vs
  mutate(name = str_remove_all(name, "\\([^()]*\\)") %>% str_trim()) %>% # remove anything in brackets
  mutate(last_name = str_extract(name, "(?<=\\s)[A-z-']+$"), .after=name) %>% # get last name letter
  mutate(starts_with = str_extract(last_name, "^[A-z]{1}"), .after=last_name) %>% 
  mutate(starts_with = factor(tolower(starts_with), levels = letters)) %>% 
  mutate(medal = factor(medal, levels = c("Gold", "Silver", "Bronze"))) %>% 
  mutate(half_century = floor(year/50)*50, .after=year)


# get medals by name
df_name = df %>% 
  filter(!is.na(medal)) %>% 
  group_by(name, starts_with, half_century) %>% 
  count()


# get num medals by letter
df_name %>% 
  ggplot(aes(x = starts_with, y = n)) + # , group = half_century, color = half_century
  geom_boxplot() + 
  # geom_point() +
  # geom_line() + 
  # geom_bar(stat='identity') +
  scale_y_log10() + 
  facet_wrap(~half_century) +
  NULL

## not much there



## what about age? age distribution of medals
library(ggridges)

df_age = 
  olympics %>% 
  filter(!is.na(medal)) %>% 
  mutate(medal = factor(medal, levels = c("Gold", "Silver", "Bronze"))) %>% 
  mutate(half_century = floor(year/50)*50, .after=year) %>% 
  mutate(decade = floor(year/10)*10, .after=year) %>% 
  group_by(age, medal, half_century) %>% 
  count()
  
# df_age %>% 
#   ggplot(aes(x = age, y = n)) +
#   geom_histogram(stat = 'identity') +
#   facet_grid(decade~medal)

df_age %>% 
  ggplot(aes(x = age, y = half_century, height = n)) +
  geom_density_ridges(stat='identity') +
  facet_wrap(~medal)
  
  
  


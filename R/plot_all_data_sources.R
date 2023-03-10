# data sources plot 
# jane.sullivan@noaa.gov
# last updated march 2023

# set up ----

library(tidyverse)
library(ggthemes)
theme_set(theme_classic(base_size = 16))

dat_path <- 'data/experimental_age_data' 
out_path <- file.path('outputs/growth')
dir.create(out_path)

# take black out of colorblind theme
scale_fill_colorblind7 = function(.ColorList = 2L:8L, ...){
  scale_fill_discrete(..., type = colorblind_pal()(8)[.ColorList])
}

# Color
scale_color_colorblind7 = function(.ColorList = 2L:8L, ...){
  scale_color_discrete(..., type = colorblind_pal()(8)[.ColorList])
}

# data ----

# amazing file that haley oleynik assembled
dat <- read_csv('data/processed/SST_meta-data.csv')

unique(dat$Year)
unique(dat$category)
unique(dat$group)

dat %>% filter(group == 'weights') %>% distinct(category)
dat %>% filter(category == 'L_Combo' & present == 1)

dat <- dat %>% 
  mutate(Group = case_when(group == 'catch' ~ 'Catch',
                           group == 'discards' ~ 'Discards',
                           group == 'survey' ~ 'Abundance indices',
                           group == 'lengthcomps' ~ 'Length compositions',
                           group == 'weights' ~ 'Mean body weight'),
         Group = factor(Group, 
                        levels = c('Catch', 'Abundance indices', 'Length compositions',
                                   'Discards', 'Mean body weight'),
                        ordered = TRUE),
         Fleet = case_when(category %in% c('Ntrawl', 'L_Ntrawl', 'D_Ntrawl', 'W_Ntrawl') ~ 'Trawl North',
                           category %in% c('Strawl', 'L_Strawl', 'D_Strawl', 'W_Strawl') ~ 'Trawl South',
                           category %in% c('Nother', 'L_Nother', 'D_Nother', 'W_Nother') ~ 'Non-trawl North',
                           category %in% c('Sother', 'L_Sother', 'D_Sother', 'W_Sother') ~ 'Non-trawl South',
                           category %in% c('AFSCTriennialSurvey1', 'L_TriennialShelf1') ~ 'AFSC Triennial Shelf Survey 1',
                           category %in% c('AFSCTriennialSurvey2', 'L_TriennialShelf2') ~ 'AFSC Triennial Shelf Survey 2',
                           category %in% c('AFSCSlopeSurvey', 'L_AFSCSlope') ~ 'AFSC Slope Survey',
                           category %in% c('NWFSCSlopeSurvey', 'L_NWFSCSlope') ~ 'NWFSC Slope Survey',
                           category %in% c('ComboSurvey', 'L_Combo') ~ 'NWFSC Combo Survey'),
         Fleet = factor(Fleet,
                        levels = c('Trawl North', 'Trawl South', 
                                   'Non-trawl North', 'Non-trawl South',
                                   'AFSC Triennial Shelf Survey 1', 'AFSC Triennial Shelf Survey 2',
                                   'AFSC Slope Survey', 'NWFSC Slope Survey', 'NWFSC Combo Survey'),
                        ordered = TRUE))

dat <- dat %>% 
  group_by(Group, Fleet) %>% 
  mutate(min = min(Year[present == 1])) %>% 
  filter(Year >= min) 

unique(dat$Fleet)
dat %>% 
  ggplot(aes(x = Year, y = Fleet, col = Fleet, fill = Fleet, shape = factor(present))) +
  geom_point(size = 3) + 
  facet_wrap(~Group, ncol = 1, scales = 'free_y') +
  scale_y_discrete(position = 'right', limits = rev) +
  scale_shape_manual(values = c(1, 19)) +
  labs(x = NULL, y = NULL) +
  theme_bw(base_size = 16) +
  theme(legend.position = 'none')

ggsave('outputs/assessment_data_timeseries.png', 
       dpi=300, height=10, width=15, units="in")
# Libraries

library(dplyr)
library(flexdashboard)
library(ggplot2)
library(ggridges)

# Data

df_O3 <- readr::read_rds("data/df_O3_clima_2008_2013.rds")

df_tarde_DPII <- df_O3 %>%
  filter(siteid == 1, hour %in% 12:16) %>% 
  group_by(date) %>%
  summarise_at(vars(o3_mass_conc, 
                    tp, rd, hm, ws, pp,
                    dv_ti_0to199m_9am, 
                    dv_ti_200to499m_9am),
               funs(mean),
               na.rm = TRUE) %>% 
  ungroup() %>% 
  mutate(month = lubridate::month(date),
         month = as.factor(month))

medidas_manha <- df_O3 %>%
  filter(siteid == 1, hour %in% 6:10) %>% 
  group_by(date) %>%
  summarise_at(vars(tp, rd, hm, ws, pp),
               funs(mean),
               na.rm = TRUE)  %>%
  rename(morning_tp = tp,
         morning_rd = rd,
         morning_hm = hm,
         morning_ws = ws,
         morning_pp = pp)

df_tarde_DPII <- df_tarde_DPII %>%  
  left_join(medidas_manha, by = "date")
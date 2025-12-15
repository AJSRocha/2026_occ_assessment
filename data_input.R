library(dplyr)


for(i in c(1995:2024)){
  load(paste0('Z:/PNAB/vendas-dia/vd_', i, '.Rdata'))
  assign(paste0('vd_', i),
         mget(ls(pattern = paste0('vd_*', i)))[[1]] %>%
           filter(!is.na(EESPECIE)) %>% 
           mutate(week = lubridate::week(IDATVEND)) %>% 
           # filter(EESPECIE %in% c('OCC', 'OCT'))) %>% 
           group_by(IEMBARCA, IPORTO, PORTO, zona, IDATVEND,
                    EGRUPART, EARTE, ESUBARTE,
                    year_sale, month_sale, quarter_sale, week,
                    id_venda, CFR, Event.Start.Date, Event.End.Date,
                    Vessel.Name, Construction.Year, Loa, Ton.Gt,
                    Gear.Main.Code, Gear.Sec.Code, Gear.3rd.Code,
                    Gear.4th.Code, Gear.5th.Code, Gear.6th.Code,
                    Power.Main) %>% 
           summarise(QVENDA_total = sum(QVENDA),
                     OCC = sum(QVENDA[EESPECIE %in% c('OCC', 'OCT')])))}

vd = do.call("rbind", mget(ls(pattern = "^vd_*")))
rm(list = ls(pattern = 'vd_'))

names(vd)[grepl('OCC', names(vd))] = 'QVENDA'
names(vd)

vd = vd %>% 
  filter(EGRUPART != 'PS' & 
           zona == '27.9.a.s.a')

# vd = vd %>% 
#   mutate(week = lubridate::week(IDATVEND))
#   

df_effort =
  vd %>%
  group_by(year_sale, week, IEMBARCA, PORTO) %>%
  summarise(Power.Main.raw = mean(Power.Main, na.rm = T),
            Power.main = trunc(Power.Main.raw/50) * 50,
            catch_i = sum(QVENDA),
            effort_i = n_distinct(IDATVEND)) %>% 
  mutate(catch_i = case_when(catch_i == 0 ~ 0.1, 
                               T ~ catch_i)
         # effort_otb = case_when(effort_otb  < 150 ~ mean(effort_otb),
         #                        T ~ effort_otb)
  )


df_effort = df_effort %>%
    group_by(year_sale,  week) %>%
    summarise(catch = sum(catch_i, na.rm =T),
              effort = sum(effort_i, na.rm =T))



save(vd,
     file = '.data/condensed_data.Rdata')

save(df_effort,
     file = '.data/initial_data_occ_sumario.Rdata')

# save(vd, file = '.data/initial_data_occ_sumario_otb.Rdata')



library(Runuran)

load('Z:/PNAB/portos_slv/portos_slv.Rdata')
load(".data/nautilus_occ.Rdata")
load(".data/naut_occ_2023.Rdata")
load('.data/initial_data_occ_sumario.Rdata')

# actualiza dados de lota para tabela que inclui os de 2023
bio_occ = occ_2023

bio_occ %>% group_by(ano, trim, estrategia_amostragem) %>%
  summarise(cona = length(unique(id_viagem)))

bio21 =
  bio_occ %>%
  filter(estrategia_amostragem == "Species Focus") %>%
  filter(Sampling_type != 'OnBoard') %>% 
  transmute(id_viagem = factor(id_viagem),
            id_denominacao = factor(id_denominacao),
            id_caixa = factor(id_caixa),
            id_spp = factor(id_spp),
            id_indiv = factor(id_indiv),
            REGIAO = factor( 
              case_when(regiao == "Sul" ~ "27.9.a.s.a",
                        regiao == "SW" ~ "27.9.a.c.s",
                        regiao == "NW" ~ "27.9.a.c.n")),
            DATA = as.POSIXct(data_venda, format = '%Y-%m-%d'),
            trim = factor(trim),
            ANO = factor(ano),
            PORTO = factor(lota),
            codporto = factor(iporto),
            cat_com = factor(cat_com),
            especie_am = factor("OCC"),
            land_kg = peso_total_dom,
            peso_amostrado_dom = peso_amostrado_dom,
            peso_total_caixa = peso_total_caixa,
            peso_am_caixa = peso_am_caixa,
            peso_total_spp = peso_total_spp,
            peso_am_spp = peso_am_spp,
            n_total_caixa = n_total_caixa,
            n_amostrados_caixa = n_amostrados_caixa,
            n_total_spp = n_total_spp,
            n_amostrado_comprimentos = n_amostrado_comprimentos,
            n_nao_observados_tot = n_nao_observados_tot,
            SEXO = factor(sexo),
            E_ESTOM = factor(estado_replecao_estomago),
            EST_MATURACAO = factor(case_when(estado_maturacao == "1 - imatura" ~ 1,
                                             estado_maturacao == "1 - imaturo" ~ 1,
                                             estado_maturacao == unique(bio_occ$estado_maturacao)[2] ~ 2, 
                                             estado_maturacao == "3 - matura" ~ 3,
                                             estado_maturacao == "3 - maturo" ~ 3,
                                             estado_maturacao == "4 - postura" ~ 4,
                                             estado_maturacao == "4 - senil" ~ 4,
                                             estado_maturacao == "5 - senil" ~ 5)),
            comp_manto = comp_manto,
            peso_total = peso_total,
            peso_eviscerado = peso_eviscerado,
            m_peso_complex_espermatoforico = m_peso_complex_espermatoforico,
            peso_testiculo = peso_testiculo,
            peso_gland_oviducal = peso_gland_oviducal,
            peso_ovario = peso_ovario,
            peso_gland_digestiva = peso_gland_digestiva)

bio21 =
  merge(bio21, portos_slv[,c("codporto","nome")],
        by.x = 'codporto',
        by.y = 'codporto',
        all.x = T,
        all.y = F)


OTB =
  unique(lota_occ$arte_eu)[grepl('OTB',unique(lota_occ$arte_eu)) |
                             grepl('TRAWL',unique(lota_occ$arte_eu))]
PS =
  unique(lota_occ$arte_eu)[grepl('PS_',unique(lota_occ$arte_eu)) ]


lota_naut_1 =
  lota_occ %>% 
  # filter(estrategia_amostragem == "Concurrent Sampling") %>%
  # filter(Sampling_type != 'OnBoard') %>% 
  transmute(id_viagem = factor(id_viagem),
            id_denominacao = factor(id_denominacao),
            id_caixa = factor(id_caixa),
            id_spp = factor(id_spp),
            REGIAO = factor( 
              case_when(zona == "Sul" ~ "27.9.a.s.a",
                        zona == "SW" ~ "27.9.a.c.s",
                        zona == "NW" ~ "27.9.a.c.n")),
            DATA = as.POSIXct(data_venda, format = '%Y-%m-%d'),
            ANO = factor(ano),
            MES = gsub('(?<=\\b|-)0',
                       '',
                       format(DATA, format = '%m'),
                       perl=TRUE),
            codporto = factor(codporto),
            PORTO = factor(lota),
            GEAR = factor(
              case_when(arte_eu %in% OTB ~ 'OTB',
                        arte_eu %in% PS ~ 'PS',
                        TRUE ~ 'MIS')),
            
            cat_com = factor(cat_com),
            especie_am = factor("OCC"),
            land_kg = peso_total_dom,
            # cat_com = factor(cat_com),
            peso_total_dom = peso_total_dom,
            peso_amostrado_dom = peso_amostrado_dom,
            peso_total_caixa = peso_total_caixa,
            peso_am_caixa = peso_am_caixa,
            peso_total_spp = peso_total_spp,
            peso_am_spp = peso_am_spp,
            n_total_caixa = n_total_caixa,
            n_amostrados_caixa = n_amostrados_caixa,
            n_total_spp = n_total_spp,
            n_amostrado_comprimentos = n_amostrado_comprimentos,
            n_nao_observados = n_nao_observados,
            n_nao_observados_tot = n_nao_observados_tot,
            classe_comp = classe_comp)

lota_naut_1 =
  merge(lota_naut_1, portos_slv[,c("codporto","nome")],
        by.x = 'codporto',
        by.y = 'codporto',
        all.x = T,
        all.y = F)


# lotas guardadas no nautilus em que dados foram registados com pesos

load(".data/index_metiers.Rdata")

df_bio =
  bio_occ %>% 
  left_join(meties2[,c('id', 'arte_eu')],
            by = c('id_viagem' = 'id'))

lota_naut_2 =
  df_bio %>%
  #possivel outlier?
  filter(estrategia_amostragem == "Concurrent Sampling") %>%
  filter(Sampling_type != 'OnBoard') %>% 
  filter(peso_total < 10000) %>% 
  filter(ano <= 2023) %>% 
  transmute(id_viagem = factor(id_viagem),
            id_denominacao = factor(id_denominacao),
            id_caixa = factor(id_caixa),
            id_spp = factor(id_spp),
            id_indiv = factor(id_indiv),
            REGIAO = factor( 
              case_when(regiao == "Sul" ~ "27.9.a.s.a",
                        regiao == "SW" ~ "27.9.a.c.s",
                        regiao == "NW" ~ "27.9.a.c.n")),
            DATA = as.POSIXct(data_venda, format = '%Y-%m-%d'),
            MES = gsub('(?<=\\b|-)0',
                       '',
                       format(DATA, format = '%m'),
                       perl=TRUE), 
            trim = factor(trim),
            ANO = factor(ano),
            PORTO = factor(lota),
            codporto = factor(iporto),
            GEAR = factor(
              case_when(arte_eu %in% OTB ~ 'OTB',
                        arte_eu %in% PS ~ 'PS',
                        TRUE ~ 'MIS')),
            cat_com = factor(cat_com),
            especie_am = factor("OCC"),
            land_kg = peso_total_dom,
            peso_amostrado_dom = peso_amostrado_dom,
            peso_total_caixa = peso_total_caixa,
            peso_am_caixa = peso_am_caixa,
            peso_total_spp = peso_total_spp,
            peso_am_spp = peso_am_spp,
            n_total_caixa = n_total_caixa,
            n_amostrados_caixa = n_amostrados_caixa,
            n_total_spp = n_total_spp,
            n_amostrado_comprimentos = n_amostrado_comprimentos,
            n_nao_observados_tot = n_nao_observados_tot,
            comp_manto = comp_manto,
            peso_total = peso_total) %>%
  # correcção aos barcos de peniche para os quais nao foi possivel obter info dos ts e, consequentemente extrapolar à viagem
  mutate(land_kg = case_when(is.na(peso_total_spp) & PORTO == 'Peniche' ~ peso_am_spp,
                             T ~ land_kg),
         peso_total_spp = case_when(is.na(peso_total_spp) & PORTO == 'Peniche' ~ peso_am_spp,
                                    T ~ peso_total_spp))


lota_naut_2 =
  merge(lota_naut_2, portos_slv[,c("codporto","nome")],
        by.x = 'codporto',
        by.y = 'codporto',
        all.x = T,
        all.y = F)


lota_naut_2$month = case_when(lota_naut_2$MES %in% c('10', '11', '12') ~ lota_naut_2$MES,
                              T ~ paste0("0",lota_naut_2$MES))

lota_naut_2 = lota_naut_2 %>%
  filter(REGIAO == '27.9.a.s.a') %>%
  mutate(week = lubridate::week(DATA), #antes era isoweek()
         peso_total = peso_total/1000,
         # week = case_when(week == 53 ~ 52, T ~ week)
         )

ajuste = loess(peso_total ~ week,
               data = lota_naut_2,
               degree =2)


# acrescenta semanas que faltam
predicos = predict(ajuste, newdata = c(2:52), se = T)

cobaia = data.frame(week = c(2:52),
                    res = predicos$fit,
                    res.se = predicos$se.fit)
# cobaia = cobaia %>%
#   mutate(mes = stringr::str_pad(mes, 2, pad = "0"))

# cobaia$mbw = imputeTS::na_interpolation(cobaia$res)
# cobaia$se = imputeTS::na_interpolation(cobaia$res.se)

# Gera série de estimativas para TS com +- 2 Standard errors

#adiciona estimativa de mbw no dataframe principal
df_effort = df_effort %>%
  left_join(.,
            cobaia,
            by = c('week' = 'week'))

############### chunk novo para interpolar
df_rand = df_effort %>% ungroup() %>%  
  select(year_sale,  week,
         res, res.se) %>% 
  filter(!is.na(res))
###############

df_rand = df_rand %>%
  rowwise() %>%
  mutate(mbw_rand = urnorm(1, mean = res, sd = 6*res.se)) %>%
  ungroup()

df_effort = 
df_effort %>% ungroup() %>% 
  select(year_sale,
         week,
         catch,
         effort) %>% 
  left_join(.,
            df_rand,
            by = c('year_sale' = 'year_sale',
                   'week' = 'week'))

df_effort$mbw = imputeTS::na_interpolation(df_effort$mbw_rand)


df_effort %>%
  ggplot() +
  geom_line(aes(x = week,
                y = res),
            group = 1) +
  geom_line(aes(x = week,
                y = mbw_rand),
            group = 1,
            color = 'red') +
  facet_wrap(year_sale ~.) +
  theme_bw()


save(df_effort,
     file = '.data/initial_data_occ_mbw.Rdata')

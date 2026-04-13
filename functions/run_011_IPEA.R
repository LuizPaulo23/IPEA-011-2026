#' @title Respondendo a questão 2 
#' @details Dependente da pasta bd - definir como diretório \* 
#' @author Luiz Paulo Tavares Gonçalves

# Configurações de ambientes ===================================================

base::rm(list = ls())
base::getwd()

# Clonar o repositório e definir o bd como diretório \* 
# git clone https://github.com/LuizPaulo23/IPEA-011-2026

setwd("/home/luizpaulo/Github/IPEA_011/IPEA-011-2026/db")

# Dependências \* ==============================================================

if (!require(pacman)) install.packages("pacman"); library(pacman)

pacman::p_load(PNADcIBGE, scales, zoo, tidyverse, ggplot2, DataExplorer, survey)

# Tema para os plotes \* 

theme_standard_ipea = ggplot2::theme_minimal(base_size = 12) +
                      ggplot2::theme(plot.title = element_text(face = "bold", size = 14),
                                     plot.subtitle = element_text(size = 11),
                                     legend.position = "bottom",
                                     legend.title = element_text(face = "bold"),
                                     panel.grid.minor = element_blank(),
                                     panel.grid.major.x = element_blank(),
                                     strip.text = element_text(face = "bold"),
                                     axis.title.y = element_text(margin = margin(r = 10)))


# Importando Banco de Dados \* =================================================
# Importar os dados da pasta bd -- arquivo rds com os txt filtrado - de 2013 a 2019

data_raw_pnad <- readRDS("pnad_data_raw.rds")

# Curadoria: verificação geral da base de dados \* --------------------------------------------

DataExplorer::plot_intro(data = data_raw_pnad)
data_raw_pnad %>% dplyr::glimpse()

# Filtros, limpeza e organização do banco de dados \* 
# Evolução da taxa de conclusão do Ensino Médio para jovens de 15 a 17 anos

data_clean = data_raw_pnad %>% 
             dplyr::rename(age = V2009, 
                           race = V2010, 
                           schooling = VD3004, 
                           year = Ano, 
                           quarter = Trimestre, 
                           w = V1033) %>% 
             dplyr::filter(age >= 15 & age <= 17) %>% 
             janitor::clean_names() 
            
DataExplorer::plot_intro(data_clean)

# Rotulando por região e nível educacional \* 

data_clean = data_clean %>% 
             dplyr::mutate(time_quarter = zoo::as.yearqtr(paste(year, quarter), format = "%Y %q")) %>% 
             dplyr::mutate(uf = dplyr::case_when(
               uf == 11 ~ "Rondônia",
               uf == 12 ~ "Acre",
               uf == 13 ~ "Amazonas",
               uf == 14 ~ "Roraima",
               uf == 15 ~ "Pará",
               uf == 16 ~ "Amapá",
               uf == 17 ~ "Tocantins",
               uf == 21 ~ "Maranhão",
               uf == 22 ~ "Piauí",
               uf == 23 ~ "Ceará",
               uf == 24 ~ "Rio Grande do Norte",
               uf == 25 ~ "Paraíba",
               uf == 26 ~ "Pernambuco",
               uf == 27 ~ "Alagoas",
               uf == 28 ~ "Sergipe",
               uf == 29 ~ "Bahia",
               uf == 31 ~ "Minas Gerais",
               uf == 32 ~ "Espírito Santo",
               uf == 33 ~ "Rio de Janeiro",
               uf == 35 ~ "São Paulo",
               uf == 41 ~ "Paraná",
               uf == 42 ~ "Santa Catarina",
               uf == 43 ~ "Rio Grande do Sul",
               uf == 50 ~ "Mato Grosso do Sul",
               uf == 51 ~ "Mato Grosso",
               uf == 52 ~ "Goiás",
               uf == 53 ~ "Distrito Federal",
               TRUE ~ NA_character_
             ),
          
               region = dplyr::case_when(uf %in% c("Rondônia","Acre","Amazonas","Roraima","Pará","Amapá","Tocantins") ~ "Norte",
                                                     uf %in% c("Maranhão","Piauí","Ceará","Rio Grande do Norte","Paraíba","Pernambuco","Alagoas","Sergipe","Bahia") ~ "Nordeste",
                                                     uf %in% c("Minas Gerais","Espírito Santo","Rio de Janeiro","São Paulo") ~ "Sudeste",
                                                     uf %in% c("Paraná","Santa Catarina","Rio Grande do Sul") ~ "Sul",
                                                     uf %in% c("Mato Grosso do Sul","Mato Grosso","Goiás","Distrito Federal") ~ "Centro-Oeste"), 
                           schooling = dplyr::case_when(
                             schooling == 1 ~ "Sem instrução e menos de 1 ano de estudo",
                             schooling == 2 ~ "Fundamental incompleto ou equivalente",
                             schooling == 3 ~ "Fundamental completo ou equivalente",
                             schooling == 4 ~ "Médio incompleto ou equivalente",
                             schooling == 5 ~ "Médio completo ou equivalente",
                             schooling == 6 ~ "Superior incompleto ou equivalente",
                             schooling == 7 ~ "Superior completo",
                             TRUE ~ NA_character_
                           ),
             race = dplyr::case_when(
               race == 1 ~ "Branca",
               race == 2 ~ "Preta",
               race == 3 ~ "Amarela",
               race == 4 ~ "Parda",
               race == 5 ~ "Indígena",
               race == 9 ~ "Ignorado",
               TRUE ~ NA_character_
             ),
                           completed_high_school = schooling %in% c("Superior incompleto ou equivalente",
                                                                    "Superior completo",
                                                                    "Médio completo ou equivalente")) 


rm(data_raw_pnad)

# Composição amostral \* 

sample_race = data_clean %>% 
              dplyr::group_by(time_quarter, race) %>% 
              dplyr::summarise(n = n()) %>% print()
              

# unique(data_clean$schooling)
# Aplicação obrigatória dos pesos amostrais e do desenho complexo da amostra do IBGE.

desenho <- survey::svydesign(ids = ~upa,
                             strata = ~estrato,
                             weights = ~w,
                             data = data_clean, nest = TRUE)
rm(data_clean)

data_rate <- survey::svyby(~completed_high_school,
                           ~time_quarter + region + race,
                            desenho,
                            svymean, na.rm = TRUE)

getwd()
saveRDS(data_rate, "rate_survey.rds")

# Para um processamento mais rápido pode importar a taxa calculada \* 

# data_rate <- base::readRDS("rate_survey.rds")

# Visualização \* 

df_rate <- data_rate %>%
           dplyr::transmute(time_quarter,
                            region,
                            race,
                            rate = completed_high_schoolTRUE,
                            se = se.completed_high_schoolTRUE)


# plot 1 \* --------------------------------------------------------------------------------

df_rate %>% 
  filter(race != "Ignorado") %>%
  ggplot(aes(time_quarter, rate, color = race)) +
  geom_smooth(se = FALSE, linewidth = 1.2, method = "loess") +
  facet_wrap(~region) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = c(
    "Branca"   = "#1f4e79",  
    "Preta"    = "#c00000",  
    "Parda"    = "#548235", 
    "Amarela"  = "#bf9000",  
    "Indígena" = "#7030a0")) +
  labs(
    title = "Taxa de Conclusão do Ensino Médio com 15-17 anos",
    subtitle = "Com Tendência Suavizada",
    x = "Trimestre",
    y = "Taxa",
    color = "",
  ) +
  theme_standard_ipea
  

# plot2 - Média geral \* -------------------------------------------------------------------

df_plot = df_rate %>% 
  filter(race != "Ignorado") %>% 
  mutate(year = substr(time_quarter, 1, 4)) %>% 
  filter(year %in% c("2013", "2019")) %>% 
  group_by(year, region, race) %>% 
  summarise(avg = mean(rate), .groups = "drop")

ggplot(df_plot, aes(
  x = year,
  y = avg,
  group = interaction(region, race),
  color = race
)) +
  geom_line(alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ region) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = c(
    "Branca"   = "#1f4e79",  
    "Preta"    = "#c00000",  
    "Parda"    = "#548235", 
    "Amarela"  = "#bf9000",  
    "Indígena" = "#7030a0"
  )) +
  theme_standard_ipea+
  labs(x = "Ano", y = "Taxa média", color = "Raça/cor", 
       title = "Taxa Média de Conclusão do Ensino Médio - 2013 e 2019")

# plot 3 \* --------------------------------------------------------------------------------

# df_plot <- df_rate %>%
#   filter(race %in% c("Branca", "Preta"))
#   
# df_plot <- df_plot %>%
#   mutate(
#     lower = rate - 1.96 * se,
#     upper = rate + 1.96 * se
#   )
# 
# ggplot(df_plot,
#        aes(x = time_quarter, y = rate, color = race, fill = race)) +
#   
#   geom_ribbon(aes(ymin = lower, ymax = upper),
#               alpha = 0.2,
#               color = NA) +
#   
#   geom_line(linewidth = 1.2) +
#   
#   facet_wrap(~region) +
#   
#   scale_y_continuous(labels = percent_format()) +
#   
#   scale_color_manual(values = c(
#     "Branca" = "#1f4e79",
#     "Preta"  = "#c00000"
#   )) +
#   
#   scale_fill_manual(values = c(
#     "Branca" = "#1f4e79",
#     "Preta"  = "#c00000"
#   )) +
#   
#   labs(
#     title = "Taxa de Conclusão do Ensino Médio (15–17 anos)",
#     subtitle = "Branca vs Preta com intervalo de confiança (95%)",
#     x = "Trimestre",
#     y = "Taxa",
#     color = "",
#     fill = ""
#   ) +
#   
#   theme_standard_ipea

# Gap racial \* ----------------------------------------------------------------

df_gap <- df_rate %>%
  filter(race %in% c("Branca", "Preta")) %>%
  
  select(time_quarter, region, race, rate, se) %>%
  
  tidyr::pivot_wider(
    names_from = race,
    values_from = c(rate, se)
  ) %>%
  
  mutate(
    gap = rate_Branca - rate_Preta,
    
    # aproximação delta method (IC do gap)
    se_gap = sqrt(se_Branca^2 + se_Preta^2),
    
    lower = gap - 1.96 * se_gap,
    upper = gap + 1.96 * se_gap
  )


ggplot(df_gap, aes(x = time_quarter, y = gap)) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "#c00000",
              alpha = 0.2) +
  
  geom_line(color = "#c00000", linewidth = 1.2) +
  
  facet_wrap(~region) +
  
  scale_y_continuous(labels = percent_format()) +
  
  labs(
    title = "Gap racial na conclusão do Ensino Médio",
    subtitle = "Diferença entre Brancos e Pretos (com IC 95%)",
    x = "",
    y = "Gap (p.p.)"
  ) +
  
  theme_standard_ipea


# Para pardos: 


df_gap_parda <- df_rate %>%
  filter(race %in% c("Branca", "Parda")) %>%
  
  select(time_quarter, region, race, rate, se) %>%
  
  pivot_wider(
    names_from = race,
    values_from = c(rate, se)
  ) %>%
  
  mutate(
    gap = rate_Branca - rate_Parda,
    
    se_gap = sqrt(se_Branca^2 + se_Parda^2),
    
    lower = gap - 1.96 * se_gap,
    upper = gap + 1.96 * se_gap
  )

ggplot(df_gap_parda, aes(x = time_quarter, y = gap)) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "#548235",
              alpha = 0.2) +
  
  geom_line(color = "#548235", linewidth = 1.2) +
  
  facet_wrap(~region) +
  
  scale_y_continuous(labels = percent_format()) +
  
  labs(
    title = "Gap racial na conclusão do Ensino Médio",
    subtitle = "Diferença entre Brancos e Pardos (com IC 95%)",
    x = "",
    y = "Gap (p.p.)"
  ) +
  
  theme_standard_ipea

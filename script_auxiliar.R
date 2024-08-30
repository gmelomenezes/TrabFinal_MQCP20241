pacman::p_load(haven, tidyverse, readxl, sjlabelled, dplyr, foreign, rmarkdown, ggplot2,
               geepack, plm, extrafont, ggrepel)

# LAPOP ####
# Lendo banco de dados do LAPOP
lapop <- readRDS("dados/lapop/Grand_Merge_2004-2023_LAPOP_AmericasBarometer_v1.0_w.rds")

# Transformações do banco de dados para obter a resposta em relação à confiança dos partidos políticos
lapop_confianca <- lapop %>%
  mutate(pais = case_when(pais != 16 ~ as_label(pais),
                          pais == 16 ~ "Venezuela",
                          pais == 24 ~ "Guyana")) %>%
  # Filtrando países da América Latina
  filter(pais %in% c("Argentina", "Brasil", "Colombia", "Guatemala", "El Salvador", "México",
                     "Nicaragua", "Costa Rica", "Panamá", "Ecuador", "Bolivia", "Perú",
                     "Paraguay", "Chile", "Uruguay", "República Dominicana", "Venezuela"),
         # Filtrando respostas não válidas (Não responder, Não sei, etc.)
         b21 >= 0,
         year >= 2006) %>%
  # Realizando transformação linear para obter uma variação de 0 a 10
  mutate(conf_part = (b21 - 1) * (10 / (7 - 1))) %>%
  select(pais, year, idnum, conf_part) %>%
  group_by(pais, year) %>%
  summarize(conf_part = mean(conf_part, na.rm = TRUE))

# Latinobarômetro ####

# Mapeando dos códigos dos países segundo ISO
iso_country_codes <- c("32" = "Argentina", "68" = "Bolivia", "76" = "Brazil", "152" = "Chile",
                       "170" = "Colombia", "188" = "Costa Rica", "192" = "Cuba", "214" = "Dominican Republic",
                       "222" = "El Salvador", "218" = "Ecuador", "320" = "Guatemala", "340" = "Honduras",
                       "484" = "Mexico", "558" = "Nicaragua", "591" = "Panama", "600" = "Paraguay",
                       "604" = "Peru", "630" = "Puerto Rico", "740" = "Suriname", "858" = "Uruguay",
                       "862" = "Venezuela")

# Criando automação para processar todos os arquivos do latinobarômetro que é separado por ano
process_latinobarometro <- function(year, file_path, var_map) {
  lbmt <- read_dta(file_path)
  
  # Transformações do banco de dados para obter a resposta em relação à confiança dos partidos políticos 
  lbmt_conf <- lbmt %>%
    select(all_of(var_map$numero), all_of(var_map$pais), all_of(var_map$p27j)) %>%
    mutate(conf_part = case_when(
      # Como a escala do latinobarômetro é invertida (1 para maior confiança, 4 para menor), inverti os valores para que ficasse mais didático
      .data[[var_map$p27j]] == 4 ~ 1,
      .data[[var_map$p27j]] == 3 ~ 2,
      .data[[var_map$p27j]] == 2 ~ 3,
      .data[[var_map$p27j]] == 1 ~ 4),
      # Realizando transformação linear para obter uma variáção de 0 a 10
      conf_part = (conf_part - 1) * (10 / (4 - 1)),
      pais = as.character(.data[[var_map$pais]]),
      pais = recode(pais, !!!iso_country_codes),
      year = year) %>%
    # Retirando país que não consta informado no questionário
    filter(pais != 724) %>%
    group_by(pais, year) %>%
    summarize(conf_part = mean(conf_part, na.rm = TRUE), .groups = 'drop')
  
  return(lbmt_conf)
}

# Mapeamento das variáveis de interesse para cada ano (nome do país, ano - número - e a pergunta sobre confiança nos partidos políticos)
var_map_list <- list(
  `1995` = list(pais = "pais", numero = "numero", p27j = "p27j"),
  `1996` = list(pais = "pais", numero = "numero", p27j = "p33j"),
  `1997` = list(pais = "idenpa", numero = "numinves", p27j = "sp63g"),
  `1998` = list(pais = "idenpa", numero = "numinves", p27j = "sp38g"),
  `2000` = list(pais = "idenpa", numero = "numinves", p27j = "P35ST_G"),
  `2001` = list(pais = "idenpa", numero = "numinves", p27j = "p61stg"),
  `2002` = list(pais = "idenpa", numero = "numinves", p27j = "p34stf"),
  `2003` = list(pais = "idenpa", numero = "numinves", p27j = "p21std"),
  `2004` = list(pais = "idenpa", numero = "numinves", p27j = "p34std"),
  `2005` = list(pais = "idenpa", numero = "numinves", p27j = "p47stb")
)

# Caminhos dos arquivos para automatização
years <- c("1995", "1996", "1997", "1998", "2000", "2001", "2002", "2003", "2004", "2005")
file_paths <- paste0("dados/latinobarometro/Latinobarometro_", years, "_datos_english_v2014_06_27.dta")

# Processamento dos dados e combinação dos resultados
results <- lapply(seq_along(years), function(i) {
  process_latinobarometro(years[i], file_paths[i], var_map_list[[years[i]]])
})

latinobarometro <- bind_rows(results)

#{latinobarometro <- bind_rows(transformed_data)
  
#var_attributes <- attributes(lbmt95$pais)}

# Base de dados LAPOP + Latinobarômetro ####
# Combinando as bases de dados LAPOP e Latinobarômetro (Confiança nos Partidos Políticos)
conf_part <- rbind(latinobarometro, lapop_confianca) %>%
  mutate(country = case_when(pais == "Brasil" ~ "Brazil",
                             pais == "México" ~ "Mexico",
                             pais == "Panamá" ~ "Panama",
                             pais == "Perú" ~ "Peru",
                             pais == "República Dominicana" ~ "Dominican Republic",
                             TRUE ~ pais),
         year = as.numeric(year),
         country = toupper(country)) %>%
  select(country, year, conf_part)

saveRDS(conf_part, file = "confianca_partidaria.rds")

# Estatísticas da Base de Dados
media_conf_part <- mean(conf_part$conf_part)
desv_padr_conf_part <- sd(conf_part$conf_part)

# Estatísticas por país
conf_part %>%
  group_by(country) %>%
  summarise(media = mean(conf_part, na.rm = T)) %>%
  arrange(media)

conf_part %>%
  group_by(country) %>%
  summarise(desv_pad = sd(conf_part, na.rm = T)) %>%
  arrange(desv_pad)

# Mainwaring e Su (2021) - Volatilidade Eleitoral ####

# Lendo os bancos de volatilidade eleitoral e retirando eleições anteriores a 1995
leg <- read_dta("dados/mainwaring e su/LAEVD lower chamber elections replication dataset.dta") %>%
  filter(election_year >= 1995)

pres <- read_dta("dados/mainwaring e su/LAEVD presidential elections replication dataset.dta") %>%
  filter(election_year >= 1995)

# Banco de dados combinado ####

# Combinando banco de volatilidade legislativa com os resultados de confiança nos partidos políticos
leg_comb <- leg %>%
  left_join(conf_part, by = c("country" = "country", "election_year" = "year")) %>%
  
  # Verificando onde a combinação falhou (conf_part está NA) e tentar o ano anterior à eleição
  mutate(year_match = if_else(is.na(conf_part),
                              election_year - 1,
                              election_year)) %>%
  
  # Recombinando com conf_part usando year_match
  left_join(conf_part, by = c("country" = "country", "year_match" = "year"), suffix = c("", "_alt")) %>%
  
  # Selecionando os valores retos e garantir que o ano seja reto
  mutate(
    conf_part = coalesce(conf_part, conf_part_alt),
    election_year = coalesce(election_year, year_match)) %>%
  # Retirando as variáveis year_match e conf_part_alt que não serão mais úteis a partir de agora
  select(-year_match, -conf_part_alt)

saveRDS(leg_comb, file = "legislativo_combinado.rds")

# Combinando banco de volatilidade presidencial com os resultados de confiança nos partidos políticos
pres_comb <- pres %>%
  left_join(conf_part, by = c("country" = "country", "election_year" = "year")) %>%
  
  # Verificando onde a combinação falhou (conf_part está NA) e tentar o ano anterior à eleição
  mutate(year_match = if_else(is.na(conf_part),
                              election_year - 1,
                              election_year)) %>%
  
  # Recombinando com conf_part usando year_match
  left_join(conf_part, by = c("country" = "country", "year_match" = "year"), suffix = c("", "_alt")) %>%
  
  # Selecionando os valores retos e garantir que o ano seja reto
  mutate(
    conf_part = coalesce(conf_part, conf_part_alt),
    election_year = coalesce(election_year, year_match)) %>%
  # Retirando as variáveis year_match e conf_part_alt que não serão mais úteis a partir de agora
  select(-year_match, -conf_part_alt)

saveRDS(pres_comb, file = "presidencial_combinado.rds")

# Gráficos ####

## Gráfico de histograma da variável confiança nos partidos políticos ####
hist_conf <- ggplot(conf_part, aes(x = conf_part)) + 
  geom_histogram(binwidth = 0.5, fill = "grey", color = "black") + 
  theme_minimal() + 
  labs(x = "Confiança nos Partidos Políticos",
       y = "Frequência") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

## Gráfico de dispersão com as médias e desvios padrões das confiança por país ####
disp_conf <-
  ggplot(conf_part %>% mutate(country = case_when(country == "ARGENTINA" ~ "Argentina",
                                                country == "BOLIVIA" ~ "Bolívia",
                                                country == "BRAZIL" ~ "Brasil",
                                                country == "CHILE" ~ "Chile",
                                                country == "COLOMBIA" ~ "Colômbia",
                                                country == "COSTA RICA" ~ "Costa Rica",
                                                country == "CUBA" ~ "Cuba",
                                                country == "DOMINICAN REPUBLIC" ~ "República Dominicana",
                                                country == "ECUADOR" ~ "Equador",
                                                country == "EL SALVADOR" ~ "El Salvador",
                                                country == "GUATEMALA" ~ "Guatemala",
                                                country == "HONDURAS" ~ "Honduras",
                                                country == "MEXICO" ~ "México",
                                                country == "NICARAGUA" ~ "Nicarágua",
                                                country == "PANAMA" ~ "Panamá",
                                                country == "PARAGUAY" ~ "Paraguai",
                                                country == "PERU" ~ "Peru",
                                                country == "URUGUAY" ~ "Uruguai",
                                                country == "VENEZUELA" ~ "Venezuela")) %>%
       group_by(country) %>%
         summarise(media = mean(conf_part, na.rm = T),
                   desv_pad = sd(conf_part, na.rm = T)) %>%
         arrange(media),
  aes(x = media, y = desv_pad, label = country)) +
  geom_point(color = "black", size = 2) +
  geom_text_repel(size = 4) +
  #geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  geom_vline(xintercept = media_conf_part, na.rm = T, linetype = "dotted") +
  geom_hline(yintercept = desv_padr_conf_part, na.rm = T, linetype = "dotted") +
  theme_minimal() +
  labs(x = "Média",
       y = "Desvio Padrão") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 10))

## Gráfico de barras com médias e desvios padrão da confiança nos partidos por país ####
barras_conf <- 
  ggplot(conf_part %>% mutate(country = case_when(country == "ARGENTINA" ~ "Argentina",
                                                country == "BOLIVIA" ~ "Bolívia",
                                                country == "BRAZIL" ~ "Brasil",
                                                country == "CHILE" ~ "Chile",
                                                country == "COLOMBIA" ~ "Colômbia",
                                                country == "COSTA RICA" ~ "Costa Rica",
                                                country == "CUBA" ~ "Cuba",
                                                country == "DOMINICAN REPUBLIC" ~ "Rep. Dom.",
                                                country == "ECUADOR" ~ "Equador",
                                                country == "EL SALVADOR" ~ "El Salvador",
                                                country == "GUATEMALA" ~ "Guatemala",
                                                country == "HONDURAS" ~ "Honduras",
                                                country == "MEXICO" ~ "México",
                                                country == "NICARAGUA" ~ "Nicarágua",
                                                country == "PANAMA" ~ "Panamá",
                                                country == "PARAGUAY" ~ "Paraguai",
                                                country == "PERU" ~ "Peru",
                                                country == "URUGUAY" ~ "Uruguai",
                                                country == "VENEZUELA" ~ "Venezuela")) %>%
         group_by(country) %>%
         summarise(media = mean(conf_part, na.rm = T),
                   desv_pad = sd(conf_part, na.rm = T)) %>%
         arrange(media),
       aes(x = reorder(country, media), y = media)) +
  geom_bar(stat = "identity", fill = "#595959") +
  geom_errorbar(aes(ymin = media - desv_pad, ymax = media + desv_pad), width = 0.2,
                color = "#AFAFAF") +
  geom_text(aes(label = round(media, 2)), vjust = -0.5, hjust = 0.5, size = 5,
            color = "black", fontface = "bold", lineheight = 0.8) +
  theme_minimal() +
  labs(y = "Média da Confiança nos Partidos",
       x = NULL) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 10))

## Gráfico de linha da evolução da confiança nos anos por país ####
linha_conf <-
  ggplot(conf_part %>% mutate(country = case_when(
  country == "ARGENTINA" ~ "Argentina",
  country == "BOLIVIA" ~ "Bolívia",
  country == "BRAZIL" ~ "Brasil",
  country == "CHILE" ~ "Chile",
  country == "COLOMBIA" ~ "Colômbia",
  country == "COSTA RICA" ~ "Costa Rica",
  country == "CUBA" ~ "Cuba",
  country == "DOMINICAN REPUBLIC" ~ "República Dominicana",
  country == "ECUADOR" ~ "Equador",
  country == "EL SALVADOR" ~ "El Salvador",
  country == "GUATEMALA" ~ "Guatemala",
  country == "HONDURAS" ~ "Honduras",
  country == "MEXICO" ~ "México",
  country == "NICARAGUA" ~ "Nicarágua",
  country == "PANAMA" ~ "Panamá",
  country == "PARAGUAY" ~ "Paraguai",
  country == "PERU" ~ "Peru",
  country == "URUGUAY" ~ "Uruguai",
  country == "VENEZUELA" ~ "Venezuela")) , aes(x = year, y = conf_part)) +
  geom_line(color = "darkgrey", size = 1) +
  geom_point(color = "black", size = 2) +
  facet_wrap(~country, ncol = 6) +
  scale_x_continuous(breaks = seq(1995, 2023, by = 7),
                     labels = seq(1995, 2023, by = 7)) +
  theme_minimal() +
  labs(x = "Ano",
       y = "Confiança nos Partidos Políticos") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 10),
        panel.spacing.x = unit(0.5, "cm"))

## Gráfico de histograma da variável controle da corrupção por país ####
hist_corrup <- ggplot(leg_comb, aes(x = con_corruption)) + 
  geom_histogram(binwidth = 0.5, fill = "grey", color = "black") + 
  theme_minimal() + 
  labs(x = "Controle da Corrupção",
       y = "Frequência",
       title = "Figura 3 - Distribuição do Índice de Controle da Corrupção na América Latina") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

## Gráfico de barras com médias e desvios padrão do controle da corrupção por país ####
barra_corrup <-
  ggplot(leg_comb %>% mutate(country = case_when(country == "ARGENTINA" ~ "Argentina",
                                                country == "BOLIVIA" ~ "Bolívia",
                                                country == "BRAZIL" ~ "Brasil",
                                                country == "CHILE" ~ "Chile",
                                                country == "COLOMBIA" ~ "Colômbia",
                                                country == "COSTA RICA" ~ "Costa Rica",
                                                country == "CUBA" ~ "Cuba",
                                                country == "DOMINICAN REPUBLIC" ~ "Rep. Dom.",
                                                country == "ECUADOR" ~ "Equador",
                                                country == "EL SALVADOR" ~ "El Salvador",
                                                country == "GUATEMALA" ~ "Guatemala",
                                                country == "HONDURAS" ~ "Honduras",
                                                country == "MEXICO" ~ "México",
                                                country == "NICARAGUA" ~ "Nicarágua",
                                                country == "PANAMA" ~ "Panamá",
                                                country == "PARAGUAY" ~ "Paraguai",
                                                country == "PERU" ~ "Peru",
                                                country == "URUGUAY" ~ "Uruguai",
                                                country == "VENEZUELA" ~ "Venezuela")) %>%
         group_by(country) %>%
         summarise(media = mean(con_corruption, na.rm = T),
                   desv_pad = sd(con_corruption, na.rm = T)) %>%
         arrange(media),
       aes(x = reorder(country, media), y = media)) +
  geom_bar(stat = "identity", fill = "#595959") +
  geom_errorbar(aes(ymin = media - desv_pad, ymax = media + desv_pad), width = 0.2,
                color = "#AFAFAF") +
  geom_text(aes(label = round(media, 2), vjust = ifelse(media < 0, 1.5, -0.5)), 
            hjust = 0.5, size = 5, color = "black", fontface = "bold", lineheight = 0.8) +
  theme_minimal() +
  labs(y = "Média do Controle da Corrupção",
       x = NULL) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 10))

## Gráfico de linha da evolução do controle da corrupção nos anos por país ####
linhas_corrup <- ggplot(leg_comb %>% 
         mutate(country = case_when(
           country == "ARGENTINA" ~ "Argentina",
           country == "BOLIVIA" ~ "Bolívia",
           country == "BRAZIL" ~ "Brasil",
           country == "CHILE" ~ "Chile",
           country == "COLOMBIA" ~ "Colômbia",
           country == "COSTA RICA" ~ "Costa Rica",
           country == "CUBA" ~ "Cuba",
           country == "DOMINICAN REPUBLIC" ~ "República Dominicana",
           country == "ECUADOR" ~ "Equador",
           country == "EL SALVADOR" ~ "El Salvador",
           country == "GUATEMALA" ~ "Guatemala",
           country == "HONDURAS" ~ "Honduras",
           country == "MEXICO" ~ "México",
           country == "NICARAGUA" ~ "Nicarágua",
           country == "PANAMA" ~ "Panamá",
           country == "PARAGUAY" ~ "Paraguai",
           country == "PERU" ~ "Peru",
           country == "URUGUAY" ~ "Uruguai",
           country == "VENEZUELA" ~ "Venezuela")),
       aes(x = election_year, y = con_corruption)) +
  geom_line(color = "darkgrey", size = 1) +
  geom_point(color = "black", size = 2) +
  facet_wrap(~country, ncol = 6) +
  scale_x_continuous(breaks = seq(1995, 2017, by = 5),
                     labels = seq(1995, 2017, by = 5)) +
  theme_minimal() +
  labs(x = NULL,
       y = "COntrole da Corrupção") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 10),
        panel.spacing.x = unit(0.5, "cm"))

## Gráfico de dispersão da confiança nos partidos e controle da corrupção ####
disp_corrup <- 
  ggplot(leg_comb %>% mutate(country = case_when(country == "ARGENTINA" ~ "Argentina",
                                                country == "BOLIVIA" ~ "Bolívia",
                                                country == "BRAZIL" ~ "Brasil",
                                                country == "CHILE" ~ "Chile",
                                                country == "COLOMBIA" ~ "Colômbia",
                                                country == "COSTA RICA" ~ "Costa Rica",
                                                country == "CUBA" ~ "Cuba",
                                                country == "DOMINICAN REPUBLIC" ~ "República Dominicana",
                                                country == "ECUADOR" ~ "Equador",
                                                country == "EL SALVADOR" ~ "El Salvador",
                                                country == "GUATEMALA" ~ "Guatemala",
                                                country == "HONDURAS" ~ "Honduras",
                                                country == "MEXICO" ~ "México",
                                                country == "NICARAGUA" ~ "Nicarágua",
                                                country == "PANAMA" ~ "Panamá",
                                                country == "PARAGUAY" ~ "Paraguai",
                                                country == "PERU" ~ "Peru",
                                                country == "URUGUAY" ~ "Uruguai",
                                                country == "VENEZUELA" ~ "Venezuela")),
       aes(x = con_corruption, y = conf_part, label = country)) +
  geom_point(color = "black", size = 1) +
  geom_smooth(method = "lm", formula = y ~ x, color = "royalblue", linetype = "dashed") +
  geom_smooth(method = "loess", formula = y ~ x, color = "#C21807", linetype = "dotted") +
  theme_minimal() +
  labs(x = "Controle da Corrupção",
       y = "Confiança nos Partidos Políticos") +
  annotate("text", x = Inf, y = Inf,
           label = paste("Spearman =", round(cor_spear_conf_cor, 2)),
           hjust = 1.2, vjust = 2, size = 4, color = "#C21807") +
  annotate("text", x = Inf, y = Inf,
           label = paste("Pearson =", round(cor_pear_conf_cor, 2)),
           hjust = 1.35, vjust = 3.5, size = 4, color = "royalblue") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 10))

## Gráfico de barras com médias e desvios padrão da volatilidade legislativa por país ####
barras_volleg <- 
  ggplot(leg_comb %>% mutate(country = case_when(country == "ARGENTINA" ~ "Argentina",
                                               country == "BOLIVIA" ~ "Bolívia",
                                               country == "BRAZIL" ~ "Brasil",
                                               country == "CHILE" ~ "Chile",
                                               country == "COLOMBIA" ~ "Colômbia",
                                               country == "COSTA RICA" ~ "Costa Rica",
                                               country == "CUBA" ~ "Cuba",
                                               country == "DOMINICAN REPUBLIC" ~ "Rep. Dom.",
                                               country == "ECUADOR" ~ "Equador",
                                               country == "EL SALVADOR" ~ "El Salvador",
                                               country == "GUATEMALA" ~ "Guatemala",
                                               country == "HONDURAS" ~ "Honduras",
                                               country == "MEXICO" ~ "México",
                                               country == "NICARAGUA" ~ "Nicarágua",
                                               country == "PANAMA" ~ "Panamá",
                                               country == "PARAGUAY" ~ "Paraguai",
                                               country == "PERU" ~ "Peru",
                                               country == "URUGUAY" ~ "Uruguai",
                                               country == "VENEZUELA" ~ "Venezuela")) %>%
         group_by(country) %>%
         summarise(media = mean(volatility, na.rm = T),
                   desv_pad = sd(volatility, na.rm = T)) %>%
         arrange(media),
       aes(x = reorder(country, media), y = media)) +
  geom_bar(stat = "identity", fill = "#595959") +
  geom_errorbar(aes(ymin = media - desv_pad, ymax = media + desv_pad), width = 0.2,
                color = "#AFAFAF") +
  geom_text(aes(label = round(media, 2), vjust = ifelse(media < 0, 1.5, -0.5)), 
            hjust = 0.5, size = 5, color = "black", fontface = "bold", lineheight = 0.8) +
  theme_minimal() +
  labs(y = "Média da Volatilidade Legislativa Total",
       x = NULL) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 10))


# Testes de hipótese ####

# Transformando os dataframes em painéis
leg_comb_df <- pdata.frame(leg_comb, index = c("country", "election_year"))
pres_comb_df <- pdata.frame(pres_comb, index = c("country", "election_year"))

# Retirando NAs das variáveis a serem utilizadas no modelo
leg_comb_df_fltrd <- leg_comb_df %>% filter(!is.na(conf_part),
                                 !is.na(enp),
                                 !is.na(gdp_growth1),
                                 !is.na(party_id))

saveRDS(leg_comb_df_fltrd, file = "legislativo_pronto_gee.rds")

pres_comb_df_fltrd <- pres_comb_df %>% filter(!is.na(conf_part),
                                            !is.na(enp),
                                            !is.na(gdp_growth1),
                                            !is.na(party_id))

saveRDS(pres_comb_df_fltrd, file = "presidencial_pronto_gee.rds")

## Modelos de regressão #####
# Modelo de regressão GEE com volatilidade legislativa total como dependente
leg_gee <- geeglm(volatility ~ enp + gdp_growth1 + party_id + conf_part,
                  data = leg_comb_df_fltrd,
                  id = country,
                  family = poisson,
                  corstr = "ar1")

summary(leg_gee)



# Modelo de regressão GEE com volatilidade legislativa extrassistema como dependente
leg_new_gee <- geeglm(newparties ~ enp + gdp_growth1 + party_id + conf_part,
                     data = leg_comb_df_fltrd, 
                     id = country,
                     family = poisson,
                     corstr = "ar1")

summary(leg_new_gee)

# Modelo de regressão GEE com volatilidade legislativa intrassistema como dependente
leg_wthin_gee <- geeglm(withinsv ~ enp + gdp_growth1 + party_id + conf_part,
                        data = leg_comb_df_fltrd, 
                        id = country,
                        family = poisson,
                        corstr = "ar1")

summary(leg_wthin_gee)

# Modelo de regressão GEE com volatilidade presidencial total como dependente
pres_gee <- geeglm(volatility ~ enp + gdp_growth1 + party_id + conf_part,
                        data = pres_comb_df_fltrd, 
                        id = country,
                        family = poisson,
                        corstr = "ar1")

summary(pres_gee)

# Modelo de regressão GEE com volatilidade presidencial extrassistema como dependente
pres_new_gee <- geeglm(newparties ~ enp + gdp_growth1 + party_id + conf_part,
                       data = pres_comb_df_fltrd, 
                       id = country,
                       family = poisson,
                       corstr = "ar1")

summary(pres_new_gee)

# Modelo de regressão GEE com volatilidade presidencial intrassistema como dependente
pres_wthin_gee <- geeglm(withinsv ~ enp + gdp_growth1 + party_id + conf_part,
                       data = pres_comb_df_fltrd, 
                       id = country,
                       family = poisson,
                       corstr = "ar1")

summary(pres_wthin_gee)

## Correlações ####

leg_comb_fltrd <- data.frame(leg_comb_df %>% filter(!is.na(conf_part),
                                         !is.na(con_corruption)))

cor_spear_conf_cor <- cor(leg_comb_fltrd$conf_part, leg_comb_fltrd$con_corruption, method = "spearman")

cor_pear_conf_cor <- cor(leg_comb_fltrd$conf_part, leg_comb_fltrd$con_corruption, method = "pearson")

### Matriz de correlação ####

leg_comb_fltrd_2 <- data.frame(leg_comb_df %>% filter(!is.na(conf_part),
                                                      !is.na(con_corruption),
                                                      !is.na(gdp_growth1),
                                                      !is.na(enp),
                                                      !is.na(party_id)) %>%
                                 select(conf_part, con_corruption, gdp_growth1,
                                        enp, party_id))

matriz_cor <- cor(leg_comb_fltrd_2, use = "complete.obs", method = "pearson")
print(matriz_cor)


tidy_leg_gee <- tidy(leg_gee) %>% 
  mutate(term = case_when(term == "enp" ~ "NEP",
                          term == "gdp_growth1" ~ "Crescimento do PIB",
                          term == "party_id" ~ "Identificação",
                          #term == "conf_part" ~ "Confiança",
                          term == "(Intercept)" ~ "Intercepto"),
         estimate = round(estimate, 2),
         std.error = round(std.error, 2),
         p.value = round(p.value, 2))

ggplot(leg_comb %>% mutate(country = case_when(country == "ARGENTINA" ~ "Argentina",
                                               country == "BOLIVIA" ~ "Bolívia",
                                               country == "BRAZIL" ~ "Brasil",
                                               country == "CHILE" ~ "Chile",
                                               country == "COLOMBIA" ~ "Colômbia",
                                               country == "COSTA RICA" ~ "Costa Rica",
                                               country == "CUBA" ~ "Cuba",
                                               country == "DOMINICAN REPUBLIC" ~ "República Dominicana",
                                               country == "ECUADOR" ~ "Equador",
                                               country == "EL SALVADOR" ~ "El Salvador",
                                               country == "GUATEMALA" ~ "Guatemala",
                                               country == "HONDURAS" ~ "Honduras",
                                               country == "MEXICO" ~ "México",
                                               country == "NICARAGUA" ~ "Nicarágua",
                                               country == "PANAMA" ~ "Panamá",
                                               country == "PARAGUAY" ~ "Paraguai",
                                               country == "PERU" ~ "Peru",
                                               country == "URUGUAY" ~ "Uruguai",
                                               country == "VENEZUELA" ~ "Venezuela")),
       aes(x = party_id, y = withinsv, label = country)) +
  geom_point(color = "black", size = 1) +
  geom_smooth(method = "lm", formula = y ~ x, color = "royalblue", linetype = "dashed") +
  theme_minimal() +
  labs(x = "Partidarismo",
       y = "Volatilidade") +
  annotate("text", x = Inf, y = Inf,
           label = paste("Pearson =", round(cor_pear_conf_cor, 2)),
           hjust = 1.35, vjust = 3.5, size = 4, color = "royalblue") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 10))

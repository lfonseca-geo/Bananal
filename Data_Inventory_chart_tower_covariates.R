

library(tidyverse)
library(lubridate)

# 1. Definir as variáveis de interesse do modelo tET_6
vars_interesse <- c("ET", "vpd_tow", "Ki", "UmidSol_20cm", "UmidSol_293cm")

# 2. Processamento e limpeza da disponibilidade de dados
df_inventory <- df %>%
  dplyr::filter(year(date) >= 2004 & year(date) <= 2016) %>%
  select(date, all_of(vars_interesse)) %>%
  
  # REGRA DE NEGÓCIO: Zeros ou valores <= 0 em sensores de fluxo/radiação costumam ser falha/ausência de dado
  mutate(
    ET           = ifelse(is.na(ET) | ET <= 0, NA, ET),
    vpd_tow      = ifelse(is.na(vpd_tow) | vpd_tow <= 0, NA, vpd_tow),
    Ki           = ifelse(is.na(Ki) | Ki <= 0, NA, Ki), # Ki diurno > 0; ajuste se houver medição noturna
    UmidSol_20cm = ifelse(is.na(UmidSol_20cm) | UmidSol_20cm <= 0, NA, UmidSol_20cm),
    UmidSol_293cm= ifelse(is.na(UmidSol_293cm) | UmidSol_293cm <= 0, NA, UmidSol_293cm)
  ) %>%
  
  # Transformar em formato longo (Long format)
  pivot_longer(cols = -date, names_to = "Variavel", values_to = "Valor") %>%
  
  # Definir Status do dado (1 para presente/válido, 0 para falha)
  mutate(Status = ifelse(is.na(Valor), 0, 1)) %>%
  
  # Agrupar por Mês (ou Dia) para otimizar a visualização em geom_tile
  mutate(Data_Mes = as.Date(cut(date, breaks = "month"))) %>%
  group_by(Data_Mes, Variavel) %>%
  summarise(
    Prop_Valida = mean(Status, na.rm = TRUE), # % de dados válidos no mês
    .groups = "drop"
  ) %>%
  
  # Categorizar o status final para o gráfico
  mutate(
    Status_Cat = case_when(
      Prop_Valida == 0 ~ "Missing data / Failure (0%)",
      Prop_Valida > 0 & Prop_Valida < 0.5 ~ "parcial available (< 50%)",
      TRUE ~ "Available (≥ 50%)"
    )
  )

# 3. Ordenar as variáveis para exibição no eixo Y (opcional)
df_inventory$Variavel <- factor(
  df_inventory$Variavel, 
  levels = c("UmidSol_293cm", "UmidSol_20cm", "Ki", "vpd_tow", "ET"),
  labels = c("Soil Moisture (293 cm)", 
             "Soil Moisture (20 cm)", 
             "Solar Radiation (Ki)", 
             "VPD (Tower)", 
             "Evapotranspiration (ET)") # Nomes formatados para o artigo
)
# 4. Construção do gráfico de inventário de dados (Data Inventory Chart)
ggplot(df_inventory, aes(x = Data_Mes, y = Variavel, fill = Status_Cat)) +
  geom_tile(color = "gray95", size = 0.1) + # Cria os blocos retangulares estilo heatmap
  scale_fill_manual(
    values = c(
      "Available (≥ 50%)"   = "#2b83ba", # Azul (Dados presentes)
      "parcial available (< 50%)"      = "#abdda4", # Verde claro (Alguma falha)
      "Missing data / Failure (0%)" = "#d7191c"  # Vermelho / Branco para falha total
    ),
    name = "Disponibilidade Mensal:"
  ) +
  scale_x_date(
    date_breaks = "1 year", 
    date_labels = "%Y", 
    expand = c(0, 0)
  ) +
  labs(
    title = "Monthly data availablity (2004 - 2016)",
    #subtitle = "Monthly data availablity (2004 - 2016)",
    x = "year",
    y = "Sensor / "
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.text.y = element_text(face = "bold", color = "black"),
    axis.text.x = element_text(color = "black"),
    plot.title = element_text(face = "bold", hjust = 0),
    plot.subtitle = element_text(color = "gray30")
  )

# Salvar em PNG de alta resolução (300 DPI)
ggsave(
  filename = "fig/Data_Inventory_tET6.png",
  plot = last_plot(),
  width = 10,           # Largura em polegadas (in)
  height = 5,           # Altura em polegadas (in)
  units = "in",
  dpi = 300,            # Alta resolução para publicação
  bg = "white"          # Fundo branco garantido
)

# Salvar em TIFF (frequentemente exigido por revistas)
ggsave(
  filename = "fig/Data_Inventory_tET6.tiff",
  plot = last_plot(),
  width = 10,
  height = 5,
  units = "in",
  dpi = 300,
  compression = "lzw",  # Compressão sem perda para diminuir tamanho do arquivo
  bg = "white"
)

# Salvar em PDF (Vetorial - Qualidade infinita sem pixelar)
ggsave(
  filename = "figuras/Data_Inventory_tET6.pdf",
  plot = last_plot(),
  width = 10,
  height = 5,
  units = "in",
  device = cairo_pdf
)



# figura com todas as variáveis da torre
library(tidyverse)
library(lubridate)

# 1. Definir as variáveis de interesse atualizadas
vars_interesse <- c(
  "LE", "Pres", "q", "Net", "ET", 
  "vpd_tow", "Tar", "Prec.new", 
  "Ki", "UmidSol_20cm", "UmidSol_293cm"
)

# 2. Processamento e limpeza da disponibilidade de dados
df_inventory <- df %>%
  dplyr::filter(year(date) >= 2004 & year(date) <= 2016) %>%
  select(date, any_of(vars_interesse)) %>%
  
  # REGRA DE NEGÓCIO: Tratamento de NAs e Zeros por tipo de variável
  mutate(
    across(
      c(ET, LE, Net, Ki, Prec.new, UmidSol_20cm, UmidSol_293cm), 
      ~ ifelse(is.na(.) | . <= 0, NA, .)
    ),
    across(
      c(vpd_tow, Pres, q, Tar),
      ~ ifelse(is.na(.), NA, .) 
    )
  ) %>%
  
  # Transformar em formato longo (Long format)
  pivot_longer(cols = -date, names_to = "Variavel", values_to = "Valor") %>%
  
  # Definir Status do dado (1 para presente/válido, 0 para falha)
  mutate(Status = ifelse(is.na(Valor), 0, 1)) %>%
  
  # Agrupar por Mês para otimizar a visualização em geom_tile
  mutate(Data_Mes = as.Date(cut(date, breaks = "month"))) %>%
  group_by(Data_Mes, Variavel) %>%
  summarise(
    Prop_Valida = mean(Status, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  # Categorizar o status final para o gráfico
  mutate(
    Status_Cat = case_when(
      Prop_Valida == 0 ~ "Missing data / Failure (0%)",
      Prop_Valida > 0 & Prop_Valida < 0.5 ~ "Partial available (< 50%)",
      TRUE ~ "Available (≥ 50%)"
    )
  )

# 3. Ordenar as variáveis e definir os labels para o eixo Y
niveis_originais <- c("Prec.new", "Tar", "vpd_tow", "ET", "Net", "q", "Pres", "LE", "UmidSol_293cm", "UmidSol_20cm", "Ki")
nomes_formatados <- c("Precipitation (Prec)", "Tar (Temperature)", "VPD_tow (Vapour Pressure)", "ET (Evapotranspiration)", "Net (Net Radiation)", "q (Specific Humidity)", "Press (Pressure)", "LE (Latent Heat)", "Soil Moisture (293 cm)", "Soil Moisture (20 cm)", "Global Radiation (Ki)")

df_inventory$Variavel <- factor(
  df_inventory$Variavel, 
  levels = niveis_originais,
  labels = nomes_formatados
)

# 4. Construção do gráfico de inventário de dados
ggplot(df_inventory, aes(x = Data_Mes, y = Variavel, fill = Status_Cat)) +
  geom_tile(color = "gray95", size = 0.1) + 
  scale_fill_manual(
    values = c(
      "Available (≥ 50%)"           = "#2b83ba", 
      "Partial available (< 50%)"   = "#abdda4", 
      "Missing data / Failure (0%)" = "#d7191c"  
    ),
    name = "Monthly Availability:"
  ) +
  scale_x_date(
    date_breaks = "1 year", 
    date_labels = "%Y", 
    expand = c(0, 0)
  ) +
  labs(
    title = "Monthly data availability (2004 - 2016)",
    #subtitle = "Monthly data availability (2004 - 2016)",
    x = "Year",
    y = "Variable"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.text.y = element_text(face = "bold", color = "black"),
    axis.text.x = element_text(color = "black", angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0),
    plot.subtitle = element_text(color = "gray30")
  )

# 5. Salvar a figura em alta resolução
ggsave(
  filename = "fig/Data_Inventory_Updated.png",
  plot = last_plot(),
  width = 10,           
  height = 6,           
  units = "in",
  dpi = 300,            
  bg = "white"          
)


# Gráfico com todas as variáveis e umidade do solo

# Figura com todas as variáveis da torre (Série temporal completa + Umidade do Solo)
library(tidyverse)
library(lubridate)

# 1. Definir as variáveis de interesse atualizadas
vars_interesse <- c(
  "ET", "LE", "Pres", "q", "Tar", "Ki", "Net", "Prec.new",
  "vpd_tow", "UmidSol_10cm", "UmidSol_20cm", "UmidSol_80cm", 
  "UmidSol_150cm", "UmidSol_220cm", "UmidSol_293cm"
)

# 2. Processamento e limpeza da disponibilidade de dados
df_inventory <- df %>%
  dplyr::filter(year(date) >= 2004 & year(date) <= 2016) %>%
  select(date, any_of(vars_interesse)) %>%
  
  # REGRA DE NEGÓCIO: Tratamento de NAs e Zeros por tipo de variável
  mutate(
    across(
      c(ET, LE, Net, Ki, Prec.new, 
        UmidSol_10cm, UmidSol_20cm, UmidSol_80cm, 
        UmidSol_150cm, UmidSol_220cm, UmidSol_293cm), 
      ~ ifelse(is.na(.) | . <= 0, NA, .)
    ),
    across(
      c(vpd_tow, Pres, q, Tar),
      ~ ifelse(is.na(.), NA, .) 
    )
  ) %>%
  
  # Transformar em formato longo (Long format)
  pivot_longer(cols = -date, names_to = "Variavel", values_to = "Valor") %>%
  
  # Definir Status do dado (1 para presente/válido, 0 para falha)
  mutate(Status = ifelse(is.na(Valor), 0, 1)) %>%
  
  # Agrupar por Mês para otimizar a visualização em geom_tile
  mutate(Data_Mes = as.Date(cut(date, breaks = "month"))) %>%
  group_by(Data_Mes, Variavel) %>%
  summarise(
    Prop_Valida = mean(Status, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  # Categorizar o status final para o gráfico
  mutate(
    Status_Cat = case_when(
      Prop_Valida == 0 ~ "Missing data / Failure (0%)",
      Prop_Valida > 0 & Prop_Valida < 0.5 ~ "Partial available (< 50%)",
      TRUE ~ "Available (≥ 50%)"
    )
  )

# 3. Ordenar as variáveis para o eixo Y
# No ggplot2, o 1º elemento fica na base (bottom) e o último fica no topo (top).
# A lista abaixo é ordenada de baixo para cima para renderizar do topo (ET) à base (293 cm).
niveis_originais <- c(
  "UmidSol_293cm", "UmidSol_220cm", "UmidSol_150cm", "UmidSol_80cm", 
  "UmidSol_20cm", "UmidSol_10cm", "vpd_tow", "Prec.new", 
  "Net", "Ki", "Tar", "q", "Pres", "LE", "ET"
)

nomes_formatados <- c(
  "Soil moisture 293 cm (UmidSol_293cm)",
  "Soil moisture 220 cm (UmidSol_220cm)",
  "Soil moisture 150 cm (UmidSol_150cm)",
  "Soil moisture 80 cm (UmidSol_80cm)",
  "Soil moisture 20 cm (UmidSol_20cm)",
  "Soil moisture 10 cm (UmidSol_10cm)",
  "Vapour pressure deficit (VPD_tow)",
  "Precipitation (Prec)",
  "Net radiation (Net)",
  "Global radiation (Ki)",
  "Air temperature (Tar)",
  "Specific humidity (q)",
  "Pressure (Press)",
  "Latent heat (LE)",
  "Evapotranspiration (ET)"
)

df_inventory$Variavel <- factor(
  df_inventory$Variavel, 
  levels = niveis_originais,
  labels = nomes_formatados
)

# 4. Construção do gráfico de inventário de dados
ggplot(df_inventory, aes(x = Data_Mes, y = Variavel, fill = Status_Cat)) +
  geom_tile(color = "gray95", size = 0.1) + 
  scale_fill_manual(
    values = c(
      "Available (≥ 50%)"           = "#2b83ba", 
      "Partial available (< 50%)"   = "#abdda4", 
      "Missing data / Failure (0%)" = "#d7191c"  
    ),
    name = "Monthly Availability:"
  ) +
  scale_x_date(
    date_breaks = "1 year", 
    date_labels = "%Y", 
    expand = c(0, 0)
  ) +
  labs(
    title = "Monthly data availability (2004 - 2016)",
    x = "Year",
    y = "Variable"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.text.y = element_text(face = "bold", color = "black"),
    axis.text.x = element_text(color = "black", angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0)
  )

# 5. Salvar a figura em alta resolução (altura aumentada para 8.5 in devido ao número de variáveis)
ggsave(
  filename = "fig/Data_Inventory_All_Tower_Vars.png",
  plot = last_plot(),
  width = 10,           
  height = 8.5,           
  units = "in",
  dpi = 300,            
  bg = "white"          
)
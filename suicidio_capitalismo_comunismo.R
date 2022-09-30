
# Suicídio ---------------------------------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 30/09/22 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/suicide -------------------------------------------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

### Morte por suicídio é uma questão extremamente complexa que causa dor a centenas
### de milhares de pessoas a cada ano em todo o mundo. O objetivo desses dados é contribuir
### para um informativo dados abertos sobre caminhos para prevenir o suicídio. Se você
### está lidando com pensamentos suicidas você pode receber ajuda imediata por visitar
### o Suicide.org ou fazer uma ligação para o 1-800-SUICIDE dos Estados Unidos.

### Cada suicídio é uma tragédia. A Organização Mundial de Saúde e o  Global Burden of Disease
### estimam que quase 800 mil pessoas morrem por suicídio todos os anos. Isto é uma
### pessoa a cada 40 segundos.

### Com o tempo, intervenções baseadas em evidências, os suicídios podem ser prevenidos.

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(cols4all)
library(hrbrthemes)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

suic <- read.csv("suicide-death-rates.csv")
view(suic)
names(suic)

# Manipular dados --------------------------------------------------------------------------------------------------------------------------

suic <- suic %>%
  select(-Code) %>%
  rename(taxa_suicidio = Deaths...Self.harm...Sex..Both...Age..Age.standardized..Rate.) %>%
  view()

suic1 <- suic %>%
  filter(Entity %in% c("United States", "Germany", "Japan",
                       "Cuba", "China", "North Korea")) %>%
  group_by(Entity) %>%
  summarise(media = mean(taxa_suicidio),
            sd = sd(taxa_suicidio), n = n(),
            se = sd/sqrt(n)) %>%
  view()

suic2 <- suic %>%
  filter(Entity %in% c("United States", "Germany", "Japan",
                       "Cuba", "China", "North Korea")) %>%
  view()

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

c4a("safe", 6)

ggplot(suic1, aes(x = fct_reorder(Entity, media), 
                  y = media, fill = Entity)) +
  geom_col(width = 0.9) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                width = 0.2, size = 0.8) +
  scale_fill_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733",
                               "#332288", "#AA4499")) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  labs(x = "Países", y = "Porcentagem de suicídio") +
  theme_ipsum(axis_title_size = 16,
              axis_text_size = 14) +
  theme(legend.position = "none", 
        axis.text = element_text(colour = "black"))

ggplot(suic2, aes(x = Year, y = taxa_suicidio,
                  group = Entity, col = Entity)) +
  geom_point(shape = 15, size = 2.5) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733",
                               "#332288", "#AA4499")) +
  labs(x = "Tempo (anos)", y = "Porcentagem de suicídio",
       color = "Países") +
  theme_ipsum(axis_title_size = 16,
              axis_text_size = 14) +
  theme(axis.text = element_text(colour = "black"))

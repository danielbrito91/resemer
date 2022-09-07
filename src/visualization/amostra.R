library(tidyverse)
library(hrbrthemes)
library(gt)
library(lubridate)

seguranca <- read_csv("data/interim/seguranca.csv",locale = readr::locale(encoding = "UTF-8"))
programas <- read_csv("data/interim/coordenadores.csv",locale = readr::locale(encoding = "UTF-8"))

paleta <- data.frame(
        cerulean_blue = "#0957ceff",
        royal_blue_dark= "#1b346eff",
        egyptian_blue= "#0a3792ff",
        macaroni_and_cheese= "#f3c08eff",
        red_ryb= "#fb2a28ff")

names(seguranca) <- c("id",
                      "status",
                      "data",
                      "programa",
                      "regiao",
                      "genero",
                      "nascimento",
                      "raca",
                      "posicao",
                      "Intubação Orotraqueal",
                      "Atendimento de PCR",
                      "Atendimento de Paciente Politraumatizado",
                      "Passagem de acesso Venoso Central",
                      "Passagem de cateter de Shilley",
                      "Passagem de acesso Venoso Periférico",
                      "Realização de toracocentese",
                      "Realização de paracentese",
                      "Realização de punção lombar",
                      "Colocação de dreno de tórax",
                      "Realização de bloqueio nervoso periférico",
                      "Realização de cardioversão",
                      "Realização de pericardiocentese",
                      "Realização de punção suprapúbica",
                      "Instalação de acesso intraósseo",
                      "Realização de drenagem de abscesso",
                      "Realização de cricotireoidostomia",
                      "Passagem de linha arterial/PA invasiva",
                      "Gasometria arterial")

names(programas) <- c("programa", "coord_titulado_abramede", "percent_profs_titulados")

seguranca_valida <- seguranca %>% filter(lubridate::month(seguranca$data) < 4)

tab_n <- seguranca_valida %>% 
        mutate(idade = interval(dmy(nascimento), date(ymd_hms(data))) / years(1),
               idade = floor(idade)) %>% 
        group_by(posicao) %>% 
        summarise(Amostra = n(),
                  Masculino = sum(genero=="Masculino"),
                  Feminino = sum(genero=="Feminino"),
                "Range" = paste(range(idade), collapse = " - "),
                  "Média" = round(mean(idade)),
                "Programas de residência" = n_distinct(programa)) %>% 
        rename(" " = posicao) %>% 
        gt() %>% 
        tab_spanner(label = "Gênero", columns=c("Masculino", "Feminino")) %>% 
        tab_spanner(label = "Idade", columns = c("Range", "Média")) %>% 
        gt::tab_header(md("**Amostra do estudo**")) %>% 
        grand_summary_rows(columns=c("Amostra", "Masculino", "Feminino"),
                             fns = list(Total = ~sum(.)),
                             formatter = fmt_number, decimals = 0) 

tab_n %>% gtsave("reports/figures/tab_n.png")

respostas_por_programa <- seguranca_valida %>% 
        mutate(programa = as.factor(programa)) %>% 
        mutate(programa = fct_lump(programa, 7, other_level = "Outros programas")) %>% 
        count(programa) %>% 
        mutate(programa = fct_relevel(fct_reorder(programa, n), "Outros programas")) %>% 
        ggplot(aes(x = programa, y = n)) +
        geom_col(aes(fill = (programa=="Outros programas")), show.legend = FALSE) +
        geom_text(aes(label = n), hjust=1.5, show.legend = FALSE, col="white") +
        scale_fill_manual(values=c(paleta$royal_blue_dark, "gray")) +
        scale_color_manual(values=c(paleta$royal_blue_dark, "darkgray")) +
        coord_flip() +
        ylim(0, 44) +
        labs(y="Número de residentes que responderam",
             x="",
             title = "Programas de residência\ncom mais respostas") +
        theme_ipsum_tw(grid="X")

ggsave(respostas_por_programa, filename = "reports/figures/respostas_por_programa.png", height = 5, width = 10, dpi = 700)

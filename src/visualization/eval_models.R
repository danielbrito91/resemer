library(sjPlot)
library(hrbrthemes)
library(MASS)

paleta <- data.frame(
        cerulean_blue = "#0957ceff",
        royal_blue_dark= "#1b346eff",
        egyptian_blue= "#0a3792ff",
        cinza= "gray",
        red_ryb= "#fb2a28ff")

seguranca <- read.csv("data/processed/seguranca_final.csv")

logit_ordenado <- readRDS("models/ordered_logit.rda")
lm <- readRDS("models/lm.rda")

plot_model(logit_ordenado,
           rm.terms = c("Muito inseguro|Inseguro", "Inseguro|Neutro", "Neutro|Seguro", "Seguro|Muito seguro"),
           transform = NULL,
           sort.est = TRUE,
           vline.color = "grey80",
           show.values = TRUE,
           value.offset = .3,
           title = "Fatores associados à segurança nos procedimentos") +
        #labs(subtitle = "Segurança variando de 0 (Muito Inseguro) a 4 (Muito Seguro)\nIntercept: gênero Feminino, R1, atendimento de Paciente Politraumatizado, segurança = 3.38114 (entre Seguro [3] e Muito Seguro [4])") +
        theme_ipsum_es()

procedimentos_plot <- plot_model(logit_ordenado,
           terms = c("Procedimento_Atendimento.de.Paciente.Politraumatizado",
                     "Procedimento_Atendimento.de.PCR",
                     "Procedimento_Colocação.de.dreno.de.tórax" ,
                     "Procedimento_Gasometria.arterial"      ,                
                     "Procedimento_Instalação.de.acesso.intraósseo",
                     "Procedimento_Intubação.Orotraqueal",                    
                     "Procedimento_Passagem.de.acesso.Venoso.Central",
                     "Procedimento_Passagem.de.acesso.Venoso.Periférico"  ,   
                     "Procedimento_Passagem.de.cateter.de.Shilley" ,
                     "Procedimento_Passagem.de.linha.arterial.PA.invasiva",   
                     "Procedimento_Realização.de.bloqueio.nervoso.periférico",
                     "Procedimento_Realização.de.cardioversão",               
                     "Procedimento_Realização.de.cricotireoidostomia",
                     "Procedimento_Realização.de.drenagem.de.abscesso",       
                     "Procedimento_Realização.de.paracentese",
                     "Procedimento_Realização.de.pericardiocentese" ,         
                     "Procedimento_Realização.de.punção.lombar",
                      "Procedimento_Realização.de.punção.suprapúbica" ),
           type="std2",
           sort.est = TRUE,
           vline.color = "grey80",
           show.values = TRUE,
           show.p = FALSE,
           value.offset = .3,
           title = "") +
        #labs(subtitle = "Segurança variando de 0 (Muito Inseguro) a 4 (Muito Seguro)\nIntercept: gênero Feminino, R1, atendimento de Paciente Politraumatizado, segurança = 3.38114 (entre Seguro [3] e Muito Seguro [4])") +
        theme_ipsum_es(grid="X") +
        scale_color_manual(values = c(paleta$red_ryb, paleta$royal_blue_dark)) +
        labs(y = "Razão de chance",
             title = "Segurança autopercebida na execução de procedimentos",
             subtitle = "Análise dos procedimentos em relação à execução de toracocentese",
             caption = " IC95 dos coeficientes estandarizados")



ggsave(procedimentos_plot, filename = "reports/figures/procedimentos.png", height = 6, width = 12, dpi = 300)

outros_plot <- plot_model(logit_ordenado,
           terms = c("Titulados_de.20..a.40.",
                     "Titulados_de.41..a.60." ,                               
                     "Titulados_mais.que.80.",
                     "posicao_R2",
                     "posicao_R3"),
           type="std2",
           sort.est = TRUE,
           vline.color = "grey80",
           show.values = TRUE,
           show.p = FALSE,
           value.offset = .2,
           title = "") +
        #labs(subtitle = "Segurança variando de 0 (Muito Inseguro) a 4 (Muito Seguro)\nIntercept: gênero Feminino, R1, atendimento de Paciente Politraumatizado, segurança = 3.38114 (entre Seguro [3] e Muito Seguro [4])") +
        theme_ipsum_es(grid="X") +
        scale_color_manual(values = c(paleta$royal_blue_dark)) +
        labs(y = "Razão de chance",
             title = "Segurança autopercebida na execução de procedimentos",
             subtitle = "Demais fatores analisados, considerando R1 e percentual de profs. titulados < 20% como ref.",
             caption = " IC95 dos coeficientes estandarizados")

ggsave(outros_plot, filename = "reports/figures/outros.png", height = 6, width = 12, dpi = 300)
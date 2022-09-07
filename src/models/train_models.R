library(tidyverse)
library(MASS)
library(tidymodels)

seguranca_programa <- read.csv("data/processed/seguranca_final.csv")
seguranca <- seguranca_programa %>% 
        mutate(genero = as.factor(genero),
               posicao = as.factor(posicao),
               Procedimento = as.factor(Procedimento),
               percent_profs_titulados = as.factor(percent_profs_titulados),
               Seguranca = as.factor(Seguranca)) %>% 
       # mutate(Procedimento = fct_relevel(Procedimento, "Gasometria arterial")) %>% #Gaso no intercept
        mutate(Procedimento = fct_relevel(Procedimento, "Realização de toracocentese"),
               percent_profs_titulados = fct_relevel(percent_profs_titulados, "menos de 20%"),
               Seguranca = fct_relevel(Seguranca,
                                       "Muito inseguro",
                                       "Inseguro",
                                       "Neutro",
                                       "Seguro",
                                       "Muito seguro")) %>%  #toracocentese no intercept
        dplyr::select(posicao,
                      Procedimento,
                      percent_profs_titulados,
                      Seguranca) %>% 
        drop_na() %>% 
        mutate(percent_profs_titulados = fct_recode(percent_profs_titulados,
                "de 20% a 40%" = "20% a 40%",
                "de 41% a 60%" = "41% a 60%"
        )) %>% 
        rename(Titulados = percent_profs_titulados)

vars <- recipe(Seguranca ~ ., data=seguranca) %>%
        step_dummy(#one_hot = TRUE,
                   all_nominal(),
                   -all_outcomes()) %>%
        prep() %>%
        juice()

fit <- polr(Seguranca ~ ., data = vars, Hess = TRUE)

saveRDS(fit, file="models/ordered_logit.rda")

####
seg_numeric <-
        seguranca %>%
                mutate(Seguranca = as.numeric(Seguranca))

vars_num <- recipe(Seguranca ~ ., data=seg_numeric) %>%
        step_dummy(#one_hot = TRUE,
                all_nominal(),
                -all_outcomes()) %>%
        step_normalize(idade) %>% 
        prep() %>%
        juice()

fit_num <- lm(Seguranca ~ ., vars_num)
saveRDS(fit_num, file="models/lm.rda")
library(tidyverse) 
library(here) 
library(PNSIBGE) # Extração de microdados PNS
library(skimr)
library(lme4)
library(ggeffects)
library(merTools)


# Construção do banco de dados####
# Seleção de variáveis
vars <- c(
  "P005", # gravidez (2 = NÃO)
  "C006", # sexo/gênero
  "C008", # idade 
  "C009", # raça/idade
  "P00102", # "o sr. sabe o seu peso?" (1 = SIM)
  "P00103", # peso informado
  "P00402", # "o Sr. sabe a sua altura?" (1 = SIM)
  "P00403", # altura informada 
  "VDF003", # renda per capita
  "VDD004A") # nível de instrução

# Importação de dados via PNSIBGE
raw_19 <- get_pns(year = 2019,
                  vars = vars, # seleção das variáveis 
                  selected = TRUE, # seleção de respondentes do questionário
                  design = FALSE) # banco desestruturado (sem modo survey)
# Salvar banco
write.csv(raw_19, 
          "dados/raw/raw_19.csv", 
          row.names = FALSE)

# Carregar banco
raw_19 <- read.csv("dados/raw/raw_19.csv")

# Construção de variáveis
pns19 <- raw_19 |>
  dplyr::filter(C008 >= 18)  |>  # maiores de 18 anos
  dplyr::filter(C009 != "Ignorado")  |>  # exclusão de "Ignorados" da variável raça
  dplyr::filter(P00102 == "Sim" & P00402 == "Sim")  |>  # sabe o peso & a altura
  dplyr::filter(P005 != "Sim"| is.na(P005))  |>  # exclusão de mulheres grávidas
  dplyr::mutate(
    age = factor(
      cut(C008, c(breaks = quantile(C008, 
                                    probs = seq(0, 1, 1/3), 
                                    na.rm = TRUE)), # variável idade por tercil
          labels = c("young", "mid", "old"), 
          right = FALSE, 
          include.lowest = TRUE)), 
    gender = factor(case_when( 
      C006 == "Homem" ~ "male",
      C006 == "Mulher" ~ "female"),
      levels = c("male", "female")), # variável gênero
    race = factor(case_when(
      C009 == "Branca" ~ "white",
      C009 == "Preta" ~ "black",
      C009 == "Parda" ~ "brown"),
      levels = c("white", "black", "brown")), # variável raça/etnia
    income = factor(
      cut(VDF003, c(breaks = quantile(VDF003, 
                                      probs = seq(0, 1, 1/3), 
                                      na.rm = TRUE)), 
          labels = c("ilow", "imid", "ihigh"), # variável renda por tercil
          right = FALSE, 
          include.lowest = TRUE)),
    education = factor(case_when(
      VDD004A %in% c("Sem instrução", 
                     "Fundamental incompleto ou equivalente",
                     "Fundamental completo ou equivalente") ~ "elow",
      VDD004A %in% c("Médio incompleto ou equivalente", 
                     "Médio completo ou equivalente") ~ "emid",
      VDD004A %in% c("Superior incompleto ou equivalente", "Superior completo") ~ "ehigh"),
      levels = c("elow", "emid", "ehigh")), # variável educação por tercil
    bmi = P00103/((P00403/100)^2), # variável IMC
    obesity = case_when(  
      bmi >= 30 ~ 1,
      TRUE ~ 0))  |>  # variável estado nutricional (obesidade = sim/não)
  dplyr::select(C008, age, gender, race, income, education, bmi, obesity) # seleção das variáveis

pns19 <- drop_na(pns19) # --> 70452 obs. 

# Criação de estratos sociais
pns19 <- pns19  |> 
  dplyr::mutate(stratum = paste(age,
                                gender, 
                                race, 
                                income, 
                                education, 
                                sep = "")) |> 
  dplyr::mutate(stratum = factor(stratum)) |> 
  dplyr::mutate(id = row_number()) |> 
  dplyr::group_by(stratum) |> 
  dplyr::mutate(strata_n = n())

# Salvar banco
write.csv(estrato, 
          "dados/estratos.csv", 
          row.names = FALSE)

# Carregar banco
pns19 <- read.csv("dados/pns19.csv")

# Análise - Tutorial: #### Evans et al. (2024)
# IMC ####
# Modelo 1A | Regressão linear com dois níveis com covariáveis#### 
modelo1A <- lmer(bmi ~ (1|stratum), 
                data=pns19)
# Predição 
pns19$m1Am <- predict(modelo1A)

# Modelo 1B | Regressão linear com dois níveis sem covariáveis#### 
modelo1B <- lmer(bmi ~ age + gender + race + income + education + (1|stratum), 
                data=pns19)
# Predição
m1Bm <- predictInterval(modelo1B, 
                         level=0.95, 
                         include.resid.var=FALSE)
m1Bm <- mutate(m1Bm, id=row_number())
m1Bu <- REsim(modelo1B)

# OBESIDADE ####
# Modelo 2A | Regressão logística com dois níveis com covariáveis#### 
modelo2A <- glmer(obesity ~ (1|stratum), 
                 data=pns19, 
                 family=binomial)

# Odds ratio
tab_model(modelo2A, show.se=T)

# Preditores
pns19$m2Axbu <- predict(modelo2A, type="response")
pns19$m2Axb <- predict(modelo2A, type="response", re.form=NA)

# Modelo 2B | Regressão logística com dois níveis com covariáveis#### 
modelo2B <- glmer(obesity ~ age + gender + race + income + education + 
                   (1|stratum), 
                 data=pns19, 
                 family=binomial)

# Odds ratio
tab_model(modelo2B, show.se=T)

# Predição
m2Bm <- predictInterval(modelo2B, 
                         level=0.95, 
                         include.resid.var=FALSE)

m2Bm <- mutate(m2Bm, id=row_number())

pns19$m2BmF <- predict(modelo2B, re.form=NA)

# Predição
m2Bm_prob <- predictInterval(modelo2B, 
                              level=0.95, 
                              include.resid.var=FALSE, 
                             type="probability") 

m2Bm_prob <- mutate(m2Bm_prob, id=row_number())

pns19$m2Bxb <- predict(modelo2B, re.form=NA, type="response")

m2Bu <- REsim(modelo2B)

# Novo banco#### 
pns2 <- pns19  |> 
  left_join(m1Bm, by = "id") |> 
  rename(
    m1Bmfit = fit,
    m1Bmupr = upr,
    m1Bmlwr = lwr
  ) %>%
  left_join(m2Bm_prob, by = "id") |> 
  rename(
    m2Bmfit = fit,
    m2Bmupr = upr,
    m2Bmlwr = lwr
  ) %>%
  left_join(m2Bm, by = "id") |> 
  rename(
    m2BmfitL = fit,
    m2BmuprL = upr,
    m2BmlwrL = lwr
  )

# Colapsar tudo 
estrato <- pns2 |> 
  group_by (age,
            gender,
            race,
            income,
            education,
            stratum,
            strata_n,
            m1Am,
            m1Bmfit,
            m1Bmupr,
            m1Bmlwr,
            m2Bmfit,
            m2Bmupr,
            m2Bmlwr, 
            m2BmfitL,
            m2BmuprL,
            m2BmlwrL,
            m2BmF
            ) |> 
  summarize(across(c(bmi, obesity), 
                   mean, 
                   .names = "mean_{col}"),
            .groups = "drop")

estrato <- estrato |> 
  dplyr::mutate(mean_obesity = mean_obesity*100)  

# Ranking 
estrato <- estrato  |> 
  dplyr::mutate(rank = rank(m2Bmfit))

ggplot(estrato, aes(y=m1Bmfit, x=rank)) +
  geom_point() +
  geom_pointrange(aes(ymin=m1Bmlwr, ymax=m1Bmupr)) +
  ylab("Predicted BMI, Model 1B") +
  xlab("Stratum Rank") + 
  theme_bw()

ggplot(estrato, aes(y = m2Bmfit, x = rank, color = stratum)) +
  geom_point() +
  geom_pointrange(aes(ymin = m2Bmlwr, ymax = m2Bmupr)) +
  ylab("Predicted BMI, Model 1B") +
  xlab("Stratum Rank") + 
  theme_bw() +
  labs(color = "Stratum") +
  theme(legend.position = "none")

# Tabela 3 #### 
tab_model(modelo1A, 
          modelo1B, 
          modelo2A, 
          modelo2B,
          p.style="stars")

# VPC### 
vc1a <-as.data.frame(VarCorr(modelo1A))
vc1b <-as.data.frame(VarCorr(modelo1B))
vc2a <-as.data.frame(VarCorr(modelo2A))
vc2b <-as.data.frame(VarCorr(modelo2B))

PCV1 <- ((vc1a[1,4] - vc1b[1,4]) / vc1a[1,4])*100
PCV1
PCV2 <- ((vc2a[1,4] - vc2b[1,4]) / vc2a[1,4])*100
PCV2

# AUC###
AUC2A <- auc(pns2$obesity, pns2$m2Axbu)

AUC2AF <- auc(pns2$obesity, pns2$m2Axb)

AUC2B <- auc(pns2$obesity, pns2$m2Bmfit)
AUC2BF <- auc(pns2$obesity, pns2$m2Bxb)

estrato$n100plus <- ifelse(estrato$strata_n>=100, 1,0)
estrato$n50plus <- ifelse(estrato$strata_n>=50, 1,0)
estrato$n30plus <- ifelse(estrato$strata_n>=30, 1,0)
estrato$n20plus <- ifelse(estrato$strata_n>=20, 1,0)
estrato$n10plus <- ifelse(estrato$strata_n>=10, 1,0)
estrato$nlessthan10 <- ifelse(estrato$strata_n<10, 1,0)
  
table(estrato$nlessthan10)


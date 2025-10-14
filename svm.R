
# Pacotes -----------------------------------------------------------------

# Carregar pacotes
library(modeldata)
library(dplyr)
library(skimr)
library(tidyr)
library(splitTools)
library(e1071)
library(caret)



# Dados -------------------------------------------------------------------

# Carregar dados
data(credit_data)
dados <- dplyr::as_tibble(credit_data)

# Pequena análise exploratória
skimr::skim(dados)

# Remoção de observações ausentes
dados <- tidyr::drop_na(dados)


# Modelagem ---------------------------------------------------------------

# Separação de amostras
set.seed(1984)
amostras <- splitTools::partition(
  y = dados$Status,
  p = c(treino = 0.7, teste = 0.3),
  type = "stratified"
  )

dados_treino <- dados[amostras$treino, ]
dados_teste <- dados[amostras$teste, ]

# Treino do algoritmo
modelo <- e1071::svm(formula = Status ~., data = dados_treino)

# Produzir previsões
previsao <- predict(modelo, dados_teste)
previsao

# Calcular acurácia
caret::confusionMatrix(previsao, dados_teste$Status)

# Desafio: Explorando Dados de Filmes
# Você vai carregar, limpar e analisar um conjunto de dados sobre filmes,
# explorando informações como notas, gêneros e orçamentos.
# 1. Baixar e Carregar os Dados
#   a. Use o dataset de filmes do Kaggle:
#       I. TMDB 5000 Movies Dataset (https://www.kaggle.com/datasets/tmdb/tmdb-movie-metadata)
# 2. Limpeza e Preparação dos Dados
#   a. Verifique se há valores ausentes (NA).
#   b. Remova colunas que não serão usadas.
#   c. Converta colunas para o tipo adequado (ex: números para numeric).
# 3. Análise de Dados
#   a. Qual o filme com maior bilheteria?
#   b. Qual o orçamento médio dos filmes?
#   c. Existe uma correlação entre orçamento e nota média dos filmes?
#   d. Quais os 5 filmes mais bem avaliados com mais de 1000 votos?
#   e. Gere um gráfico mostrando a relação entre orçamento e arrecadação.
# Extra: Quer um Desafio a Mais?
# 1. Tente criar um histograma da distribuição das notas médias dos filmes!
#---------------------------------------------------------------------------

# Instalando e carregando pacotes necessários
packages <- c("tidyverse")
installed_packages <- rownames(installed.packages())
if (any(!packages %in% installed_packages)) {
  install.packages(packages[!packages %in% installed_packages])
}
library(tidyverse)

# Baixando e carregando dados
# Definição do caminho dos dados (modifique se necessário)
data_path <- "C:/Users/santo/Documents/GitHubPublished/Rprojects/dataBase/tmdb_5000_movies.csv"
# Verifica se o arquivo existe antes de carregar
if (!file.exists(data_path)) {
  stop("\nArquivo de dados não encontrado! Verifique o caminho e tente novamente.\n")
} else {
   cat("\nDados Carregados...\n")
}
# Importando os dados
dataImport <- read.csv(data_path, stringsAsFactors = FALSE)

# Tratamento dos dados
# Exibir valores ausentes por coluna
na_count <- colSums(is.na(dataImport))
message("\nValores ausentes por coluna:\n")
print(na_count)
# Preenchendo valores ausentes
dataImport <- dataImport %>%
  mutate(
    budget = ifelse(is.na(budget) | budget == 0, median(budget, na.rm = TRUE), budget),
    revenue = ifelse(is.na(revenue) | revenue == 0, median(revenue, na.rm = TRUE), revenue),
    vote_average = ifelse(is.na(vote_average), median(vote_average, na.rm = TRUE), vote_average),
    vote_count = ifelse(is.na(vote_count), median(vote_count, na.rm = TRUE), vote_count),
    original_title = ifelse(is.na(original_title) | original_title == "", "Desconhecido", original_title),
    genres = ifelse(is.na(genres) | genres == "", "Desconhecido", genres)
  )
# Conversão de tipos de dados
dataMovies <- dataImport %>%
  select(original_title, genres, budget, revenue, vote_average, vote_count) %>%
  mutate(
    budget = as.numeric(budget),
    revenue = as.numeric(revenue),
    vote_average = as.numeric(vote_average),
    vote_count = as.integer(vote_count),
    original_title = as.character(original_title),
    genres = as.character(genres)
  )
message("\nDados processados com sucesso! Linhas:", nrow(dataMovies))
cat("\n\n⚠️Valores ausentes(NA) e valores 0 das colunas substituídos pela mediana dos valores válidos da mesma coluna⚠️\n\n")
# Análises
message("Análises 🔍💻")
# 1. Qual o filme com maior bilheteria?
highestGrossing <- dataMovies %>% arrange(desc(revenue)) %>% slice(1)
message("\n\n1. Filme com maior bilheteria: ", highestGrossing$original_title,
        " - Receita: $", format(highestGrossing$revenue, big.mark = ","))

# 2. Orçamento médio dos filmes (ajustado após preenchimento)
meanBudget <- mean(dataMovies$budget)
message("\n2. Orçamento médio dos filmes (após preenchimento de valores ausentes): $",
        format(meanBudget, big.mark = ","))

# 3. Correlação entre orçamento e nota média
budgetVoteAvgCor <- cor(dataMovies$budget, dataMovies$vote_average, use = "complete.obs")
# Classificação da correlação
correlation_category <- case_when(
  budgetVoteAvgCor == 1 ~ "Correlação Positiva Perfeita",
  budgetVoteAvgCor >= 0.7 ~ "Correlação Positiva Forte",
  budgetVoteAvgCor >= 0.3 ~ "Correlação Positiva Moderada",
  budgetVoteAvgCor > 0 ~ "Correlação Positiva Fraca",
  budgetVoteAvgCor == 0 ~ "Nenhuma Correlação",
  budgetVoteAvgCor >= -0.29 ~ "Correlação Negativa Fraca",
  budgetVoteAvgCor >= -0.69 ~ "Correlação Negativa Moderada",
  budgetVoteAvgCor > -1 ~ "Correlação Negativa Forte",
  TRUE ~ "Correlação Negativa Perfeita"
)
message("\n3. Correlação entre orçamento e nota média: ", 
        round(budgetVoteAvgCor, 3), " (", correlation_category, ")")

# 4. Top 5 filmes mais bem avaliados com mais de 1000 votos
top5PopularMovies <- dataMovies %>%
  filter(vote_count > 1000) %>%
  arrange(desc(vote_average)) %>%
  slice(1:5) %>%
  select(original_title)
message("\n4. Top 5 filmes mais bem avaliados com mais de 1000 votos:")
print(top5PopularMovies)

message("Gráficos📊")
# 5. Gráfico: Relação entre orçamento e arrecadação
graphBudgetRevenue <- ggplot(dataMovies, aes(x = budget, y = revenue)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "Relação entre Orçamento e Receita",
       x = "Orçamento (USD)", y = "Receita (USD)") +
  theme_minimal()
# Salvando o gráfico
output_dir <- "C:/Users/santo/Documents/GitHubPublished/Rprojects/projects/graphics"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
ggsave(file.path(output_dir, "budgetRevenue.png"), plot = graphBudgetRevenue, width = 8, height = 6, units = "in")
message("\nGráfico 'budgetRevenue.png' salvo em ", output_dir)

# Extra: Histograma das notas médias
graphVoteAvg <- ggplot(dataMovies, aes(x = vote_average)) +
  geom_histogram(bins = 20, fill = "skyblue", color = "black") +
  labs(title = "Distribuição das Notas Médias dos Filmes",
       x = "Nota Média", y = "Frequência") +
  theme_minimal()
ggsave(file.path(output_dir, "graphVoteAvg.png"), plot = graphVoteAvg, width = 8, height = 6, units = "in")
message("\nGráfico 'graphVoteAvg.png' salvo em ", output_dir)

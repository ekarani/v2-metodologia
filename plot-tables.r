local_lib <- "./R_packages"
.libPaths(c(local_lib, .libPaths()))


# Instale um pacote (substitua "ggplot2" pelo nome do pacote desejado)
#install.packages("ggplot2")
#install.packages("corrplot")


# Carregue o pacote
library(ggplot2)
library(corrplot)


# Defina o caminho para o arquivo CSV
caminho_para_csv <- "./dataset.csv"

# Use a função read.csv() para ler os dados do CSV e armazená-los em um objeto de dados
dados <- read.csv(caminho_para_csv)

linhas_completas <- complete.cases(dados)

dados_limpos <- dados[linhas_completas, ]

colnames(dados_limpos) <- c("Data_Hora", "Aceito_Participar", "Escolaridade", "Genero", "Participacao_Competicao",
                     "Interesse_Mentoria", "Interesse_Grupos_Treinamento", "Interesse_Experiencia",
                     "Desencorajada", "Grupo_Academico", "Vagas_Exclusivas", "Competicoes_Exclusivas",
                     "Sugestoes_Comentarios")

colunas_selecionadas <- dados_limpos[, c("Participacao_Competicao",
                     "Interesse_Mentoria", "Interesse_Grupos_Treinamento", "Interesse_Experiencia",
                     "Desencorajada", "Grupo_Academico", "Vagas_Exclusivas", "Competicoes_Exclusivas")]

correlation <- cor(colunas_selecionadas, method = "pearson")
corrplot(correlation, method = "number", type = "upper", tl.col = "black")



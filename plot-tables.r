local_lib <- "./R_packages"
.libPaths(c(local_lib, .libPaths()))


# Instale um pacote (substitua "ggplot2" pelo nome do pacote desejado)
install.packages("ggplot2")
install.packages("corrplot")


# Carregue o pacote
library(ggplot2)


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

correlation1 <- cor(dados_limpos$Interesse_Mentoria, dados_limpos$Participacao_Competicao)

# Crie uma matriz de correlação vazia
m1<- matrix(0, nrow = 2, ncol = 2)

# Defina os coeficientes de correlação na matriz
m1[1, 2] <- correlation1
m1[2, 1] <- correlation1

# Defina a diagonal principal como 1
diag(m1) <- 1


correlation2 <- cor(dados_limpos$Interesse_Grupos_Treinamento, dados_limpos$Participacao_Competicao)
m2<- matrix(0, nrow = 2, ncol = 2)
m2[1, 2] <- correlation2
m2[2, 1] <- correlation2
diag(m2) <- 1

correlation3 <- cor(dados_limpos$Interesse_Experiencia, dados_limpos$Participacao_Competicao)
m3<- matrix(0, nrow = 2, ncol = 2)
m3[1, 2] <- correlation3
m3[2, 1] <- correlation3
diag(m3) <- 1

correlation4 <- cor(dados_limpos$Desencorajada, dados_limpos$Participacao_Competicao)
m4<- matrix(0, nrow = 2, ncol = 2)
m4[1, 2] <- correlation4
m4[2, 1] <- correlation4
diag(m4) <- 1

correlation5 <- cor(dados_limpos$Grupo_Academico, dados_limpos$Participacao_Competicao)
m5<- matrix(0, nrow = 2, ncol = 2)
m5[1, 2] <- correlation5
m5[2, 1] <- correlation5
diag(m5) <- 1

correlation6 <- cor(dados_limpos$Vagas_Exclusivas, dados_limpos$Participacao_Competicao)
m6<- matrix(0, nrow = 2, ncol = 2)
m6[1, 2] <- correlation6
m6[2, 1] <- correlation6
diag(m6) <- 1

correlation7 <- cor(dados_limpos$Competicoes_Exclusivas, dados_limpos$Participacao_Competicao)
m7<- matrix(0, nrow = 2, ncol = 2)
m7[1, 2] <- correlation7
m7[2, 1] <- correlation7
diag(m7) <- 1



# Produzindo matriz de correlação

# Carregue a biblioteca corrplot
library(corrplot)

# Plot da matriz de correlação
corrplot(m1, method = "number", type = "upper", tl.col = "black")
corrplot(m2, method = "number", type = "upper", tl.col = "black")
corrplot(m3, method = "number", type = "upper", tl.col = "black")
corrplot(m4, method = "number", type = "upper", tl.col = "black")
corrplot(m5, method = "number", type = "upper", tl.col = "black")
corrplot(m6, method = "number", type = "upper", tl.col = "black")
corrplot(m7, method = "number", type = "upper", tl.col = "black")



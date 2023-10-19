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


pergunta1 <- dados_limpos$Interesse_Mentoria
grafico1 <- ggplot(data = dados_limpos, aes( x = pergunta1)) +
	 geom_histogram(binwidth = 1, fill = "lightblue", color = "black") + 
	 labs(title = "Distribuição Pergunta 1", x = "Respostas", y = "Frequência")
ggsave("pergunta1.png", plot = grafico1, width = 6, height = 4, units = "in", dpi = 300)


pergunta2 <- dados_limpos$Interesse_Grupos_Treinamento
grafico2 <- ggplot(data = dados_limpos, aes( x = pergunta2)) +
	 geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
	 labs(title = "Distribuição Pergunta 2", x = "Respostas", y = "Frequência")
ggsave("pergunta2.png", plot = grafico2, width = 6, height = 4, units = "in", dpi = 300)


pergunta3 <- dados_limpos$Interesse_Experiencia
pergunta4 <- dados_limpos$Desencorajada
pergunta5 <- dados_limpos$Grupo_Academico
pergunta6 <- dados_limpos$Vagas_Exclusivas
pergunta7 <- dados_limpos$Competicoes_Exclusivas

grafico3 <- ggplot(data = dados_limpos, aes( x = pergunta3)) + 
	geom_histogram(binwidth = 1, fill = "lightblue", color = "black") + 
	labs(title = "Distribuição Pergunta 3", x = "Respostas", y = "Frequência")
ggsave("pergunta3.png", plot = grafico3, width = 6, height = 4, units = "in", dpi = 300)

grafico4 <- ggplot(data = dados_limpos, aes( x = pergunta4)) + 
	geom_histogram(binwidth = 1, fill = "lightblue", color = "black") + 
	labs(title = "Distribuição Pergunta 4", x = "Respostas", y = "Frequência")
ggsave("pergunta4.png", plot = grafico4, width = 6, height = 4, units = "in", dpi = 300)

grafico5 <- ggplot(data = dados_limpos, aes( x = pergunta5)) + 
	geom_histogram(binwidth = 1, fill = "lightblue", color = "black") + 
	labs(title = "Distribuição Pergunta 5", x = "Respostas", y = "Frequência")
ggsave("pergunta5.png", plot = grafico5, width = 6, height = 4, units = "in", dpi = 300)

grafico6 <- ggplot(data = dados_limpos, aes( x = pergunta6)) + 
	geom_histogram(binwidth = 1, fill = "lightblue", color = "black") + 
	labs(title = "Distribuição Pergunta 6", x = "Respostas", y = "Frequência")
ggsave("pergunta6.png", plot = grafico6, width = 6, height = 4, units = "in", dpi = 300)

grafico7 <- ggplot(data = dados_limpos, aes( x = pergunta7)) + 
	geom_histogram(binwidth = 1, fill = "lightblue", color = "black") + 
	labs(title = "Distribuição Pergunta 7", x = "Respostas", y = "Frequência")
ggsave("pergunta7.png", plot = grafico7, width = 6, height = 4, units = "in", dpi = 300)






colunas_selecionadas <- dados_limpos[, c("Participacao_Competicao",
                     "Interesse_Mentoria", "Interesse_Grupos_Treinamento", "Interesse_Experiencia",
                     "Desencorajada", "Grupo_Academico", "Vagas_Exclusivas", "Competicoes_Exclusivas")]




correlation <- cor(colunas_selecionadas, method = "pearson")
corrplot(correlation, method = "number", type = "upper", tl.col = "black")



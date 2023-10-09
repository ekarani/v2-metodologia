caminho_para_csv <- "./dataset.csv"
dados <- read.csv(caminho_para_csv)

linhas_completas <- complete.cases(dados)

dados_limpos <- dados[linhas_completas, ]


colnames(dados_limpos) <- c("Data_Hora", "Aceito_Participar", "Escolaridade", "Genero", "Participacao_Competicao",
                     "Interesse_Mentoria", "Interesse_Grupos_Treinamento", "Interesse_Experiencia",
                     "Desencorajada", "Grupo_Academico", "Vagas_Exclusivas", "Competicoes_Exclusivas",
                     "Sugestoes_Comentarios")

dados_limpos$Participacao_Competicao <- replace(dados_limpos$Participacao_Competicao, dados_limpos$Participacao_Competicao == "Sim", 1)
dados_limpos$Participacao_Competicao <- replace(dados_limpos$Participacao_Competicao, dados_limpos$Participacao_Competicao == "Não", 0)

cat("Estatística Descritiva da amostra: \n")
summary(dados_limpos)

cat("---------------------------------- \n")
cat("Cálculo do coeficiente de correlação entre as respostas e a participação prévia em competições: \n")
# Calculando Correlação:

correlation1 <- cor(dados_limpos$Interesse_Mentoria, dados_limpos$Participacao_Competicao)
cat(sprintf("- Correlação pergunta 1 e participação: %s\n", correlation1))
correlation2 <- cor(dados_limpos$Interesse_Grupos_Treinamento, dados_limpos$Participacao_Competicao)
cat(sprintf("- Correlação pergunta 2 e participação: %s\n", correlation2))
correlation3 <- cor(dados_limpos$Interesse_Experiencia, dados_limpos$Participacao_Competicao)
cat(sprintf("- Correlação pergunta 3 e participação: %s\n", correlation3))
correlation4 <- cor(dados_limpos$Desencorajada, dados_limpos$Participacao_Competicao)
cat(sprintf("- Correlação pergunta 4 e participação: %s\n", correlation4))
correlation5 <- cor(dados_limpos$Grupo_Academico, dados_limpos$Participacao_Competicao)
cat(sprintf("- Correlação pergunta 5 e participação: %s\n", correlation5))
correlation6 <- cor(dados_limpos$Vagas_Exclusivas, dados_limpos$Participacao_Competicao)
cat(sprintf("- Correlação pergunta 6 e participação: %s\n", correlation6))
correlation7 <- cor(dados_limpos$Competicoes_Exclusivas, dados_limpos$Participacao_Competicao)
cat(sprintf("- Correlação pergunta 7 e participação: %s\n", correlation7))


nivel_confianca <- 0.95
margem_erro <- 0.10  # 10% de margem de erro

cat("---------------------------------- \n")
cat("Inferência sobre a média populacional a partir da amostra \n")
cat("(Nível de confiança 0.95 e margem de erro 0.10) \n")
media_amostral <- mean(dados_limpos$Interesse_Mentoria)
n <- length(dados_limpos$Interesse_Mentoria)
graus_liberdade <- n - 1 
desvio_padrao <- sd(dados_limpos$Interesse_Mentoria)
valor_critico <- qt((1 + nivel_confianca) / 2, df = graus_liberdade)
erro_padrao <- margem_erro * (desvio_padrao / valor_critico)
intervalo_confianca1 <- c(media_amostral - erro_padrao, media_amostral + erro_padrao)
cat(sprintf("- Intervalo de confiança para a pergunta 1:\n"))
cat(intervalo_confianca1, "\n")


media_amostral <- mean(dados_limpos$Interesse_Grupos_Treinamento)
n <- length(dados_limpos$Interesse_Grupos_Treinamento)
graus_liberdade <- n - 1  
desvio_padrao <- sd(dados_limpos$Interesse_Grupos_Treinamento)
valor_critico <- qt((1 + nivel_confianca) / 2, df = graus_liberdade)
erro_padrao <- margem_erro * (desvio_padrao / valor_critico)
intervalo_confianca2 <- c(media_amostral - erro_padrao, media_amostral + erro_padrao)
cat(sprintf("- Intervalo de confiança para a pergunta 2:\n"))
cat(intervalo_confianca2, "\n")

media_amostral <- mean(dados_limpos$Interesse_Experiencia)
n <- length(dados_limpos$Interesse_Experiencia)
graus_liberdade <- n - 1
desvio_padrao <- sd(dados_limpos$Interesse_Experiencia)
valor_critico <- qt((1 + nivel_confianca) / 2, df = graus_liberdade)
erro_padrao <- margem_erro * (desvio_padrao / valor_critico)
intervalo_confianca3 <- c(media_amostral - erro_padrao, media_amostral + erro_padrao)
cat(sprintf("- Intervalo de confiança para a pergunta 3:\n"))
cat(intervalo_confianca3, "\n")

media_amostral <- mean(dados_limpos$Desencorajada)
n <- length(dados_limpos$Desencorajada)
graus_liberdade <- n - 1
desvio_padrao <- sd(dados_limpos$Desencorajada)
valor_critico <- qt((1 + nivel_confianca) / 2, df = graus_liberdade)
erro_padrao <- margem_erro * (desvio_padrao / valor_critico)
intervalo_confianca4 <- c(media_amostral - erro_padrao, media_amostral + erro_padrao)
cat(sprintf("- Intervalo de confiança para a pergunta 4:\n"))
cat(intervalo_confianca4, "\n")

media_amostral <- mean(dados_limpos$Grupo_Academico)
n <- length(dados_limpos$Grupo_Academico)
graus_liberdade <- n - 1
desvio_padrao <- sd(dados_limpos$Grupo_Academico)
valor_critico <- qt((1 + nivel_confianca) / 2, df = graus_liberdade)
erro_padrao <- margem_erro * (desvio_padrao / valor_critico)
intervalo_confianca5 <- c(media_amostral - erro_padrao, media_amostral + erro_padrao)
cat(sprintf("- Intervalo de confiança para a pergunta 5:\n"))
cat(intervalo_confianca5, "\n")

media_amostral <- mean(dados_limpos$Vagas_Exclusivas)
n <- length(dados_limpos$Vagas_Exclusivas)
graus_liberdade <- n - 1
desvio_padrao <- sd(dados_limpos$Vagas_Exclusivas)
valor_critico <- qt((1 + nivel_confianca) / 2, df = graus_liberdade)
erro_padrao <- margem_erro * (desvio_padrao / valor_critico)
intervalo_confianca6 <- c(media_amostral - erro_padrao, media_amostral + erro_padrao)
cat(sprintf("- Intervalo de confiança para a pergunta 6:\n"))
cat(intervalo_confianca6,"\n")

media_amostral <- mean(dados_limpos$Competicoes_Exclusivas)
n <- length(dados_limpos$Competicoes_Exclusivas)
graus_liberdade <- n - 1
desvio_padrao <- sd(dados_limpos$Competicoes_Exclusivas)
valor_critico <- qt((1 + nivel_confianca) / 2, df = graus_liberdade)
erro_padrao <- margem_erro * (desvio_padrao / valor_critico)
intervalo_confianca7 <- c(media_amostral - erro_padrao, media_amostral + erro_padrao)
cat(sprintf("- Intervalo de confiança para a pergunta 7:\n"))
cat(intervalo_confianca7, "\n")



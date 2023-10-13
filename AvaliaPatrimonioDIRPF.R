# Extracao de informacoes dos arquivos PDFs da Declaração de Ajuste Anual do Imposto de Renda de Pessoa Física (DIRPF)
# Objetivo: avaliar relacao entre variacao patrimonial e os rendimentos liquidos
# Valido para Exercicios iguais ou maiores a 2021

#### Bibliotecas ####

library(pdftools)
library(stringr)
library(dplyr)

#### Parametros iniciais
diretorio_origem <- "diretorio/arquivos"
diretorio_destino <- "diretorio/resultado"

#### Relacao de arquivos ####
# configura diretorio
setwd(diretorio_origem)

# vetor com arquivos do diretorio
arquivos <- list.files()
# seleciona apenas os PDFs
arquivos <- arquivos[grep("\\.pdf$", arquivos)]


#### Loop para leitura de cada arquivo ####

# cria variaveis do loop
texto <- NA
avalia_patrimonio <- NA
erros <- NA

# inicio do loop
for (i in 1:length(arquivos)) {
  # vetor que armazena cada pagina em uma posicao
  texto <- pdf_text(arquivos[i]) 

    ### CPF via expressao regular
    cpf <- gsub("[^0-9]", "", regmatches(texto, regexpr("\\d{3}\\.\\d{3}\\.\\d{3}-\\d{2}", texto)))
    cpf <- cpf[1]
    
    # CPF do conjuge
    padrao <- "CPF do cônjuge ou companheiro"
    cpf_conjuge <- NA
    indice <- which(str_detect(texto, padrao))
    if (length(indice)!=0) {
      cpf_conjuge <- texto[indice]
      cpf_conjuge <- str_extract(cpf_conjuge, paste0(padrao,"\\(a\\): (\\d{3}\\.\\d{3}\\.\\d{3}-\\d{2})"))
      cpf_conjuge <- as.numeric(gsub("\\D", "", cpf_conjuge))
    }

    
    # quantidade de dependentes
    qtd_dependentes <- sum(str_count(texto, fixed("Dependente mora com o titular da declaração?")))
    
    ### Exercicio via expressao regular
    exercicio <- gsub(".*EXERCÍCIO (\\d{4}).*", "\\1", texto)
    exercicio <- exercicio[1]
    
    ### Calendario via expressao regular
    calendario <- sub(".*ANO-CALENDÁRIO (\\d{4}).*", "\\1", texto)
    calendario <- calendario[1]
    
    ### Informacoes gerais extraidas do Resumo final
    # Defina as duas strings delimitadoras, de inicio e fim
    inicio <- "TRIBUTAÇÃO UTILIZANDO AS DEDUÇÕES LEGAIS"
    fim <- "OUTRAS INFORMAÇÕES   "
    
    # pagina inicial do resumo
    indice_inicio <- which(str_detect(texto, inicio))
    
    # Pula para proximo arquivo caso nao tenha o padrao definido 
    if (length(indice_inicio) == 0) {
      erros <- append(erros, paste(cpf, arquivos[i] ))
      next  # Pula para a próxima iteração
    }
    
    # define a primeira pagina para extrair informacoes a partir dela
    indice_inicio <- min(indice_inicio)
    
    ### Seleciona trecho que contem string para captura
    trecho1 <- texto[indice_inicio:length(texto)]
    trecho1 <- paste0(trecho1, collapse = " ")
    
    # Dividir a string em linhas
    linhas <- strsplit(trecho1, "\n")[[1]]
    
    # Extrair descrições e valores
    descricao <- str_extract(linhas, "^[^0-9]+")
    valor <- str_extract(linhas, "[0-9,.]+$")
    
    
    ### Data frame principal
    df <- data.frame(descricao = descricao, valor = valor, stringsAsFactors = FALSE)
    # Limpa descricao
    df$descricao <- str_squish(df$descricao)
    # Substitui virgula por ponto para padrao brasileiro
    df$valor <- gsub(",", "", df$valor)
    df$valor <- as.numeric(df$valor)
    # remove nulos
    df <- df %>% filter(!is.na(valor))
    
    ### Rendimentos
    rendimento <- df[str_detect(df$descricao, "TOTAL"),][1,2]
    
    ### Deducoes
    deducoes <- df[str_detect(df$descricao, "TOTAL"),][2,2]
    
    ### Imposto pago
    imposto <- df[str_detect(df$descricao, "Total do imposto pago"),][1,2]
    
    ### Bens e direitos
    bens_direitos_inicial <- df[str_detect(df$descricao, "Bens e direitos em"),][1,2]
    bens_direitos_final <- df[str_detect(df$descricao, "Bens e direitos em"),][2,2]
    
    ### Dividas
    dividas_inicial <- df[str_detect(df$descricao, "Dívidas e ônus reais em"),][1,2]
    dividas_final <- df[str_detect(df$descricao, "Dívidas e ônus reais em"),][2,2]
    
    ### Consolidacao dos dados capturados
    
    declaracao <- data.frame(
      cpf = cpf,
      cpf_conjuge = cpf_conjuge,
      qtd_dependentes = qtd_dependentes,
      exercicio = exercicio,
      calendario = calendario,
      rendimento = rendimento,
      deducoes = deducoes,
      imposto = imposto,
      bens_direitos_inicial = bens_direitos_inicial,
      bens_direitos_final = bens_direitos_final,
      dividas_inicial = dividas_inicial,
      dividas_final = dividas_final,
      stringsAsFactors = FALSE)
    
    # Inclui colunas para analise
    
    declaracao <- declaracao %>%
      mutate(
        patrimonio_inicial = bens_direitos_inicial - dividas_inicial, # patrimonio: bens, direitos menos obrigacoes
        patrimonio_final = bens_direitos_final - dividas_final, # patrimonio: bens, direitos menos obrigacoes
        renda_liquida = rendimento - deducoes - imposto) %>% # rendimento apos dedocoes e pagamento de imposto
      mutate(variacao_patrimonio = patrimonio_final - patrimonio_inicial) %>% # variacao bruta do patrimonio
      mutate(relacao_renda = variacao_patrimonio / renda_liquida)  # relacao com o rendimento liquido
    
    # Data frame final
    avalia_patrimonio <- rbind(avalia_patrimonio,declaracao )
}

# Exclui repeticoes e filtra exercicios superiores ou iguais a 2021
avalia_patrimonio <- avalia_patrimonio[!is.na(avalia_patrimonio$cpf),] %>% unique() %>% filter(as.numeric(exercicio) >= 2021)


#### Grava resultado ####
setwd(diretorio_destino)
write.csv2(avalia_patrimonio, "avalia_patrimonio.csv", row.names = F)
write.table(erros,"erros.txt",row.names = F, quote = F, col.names = F)

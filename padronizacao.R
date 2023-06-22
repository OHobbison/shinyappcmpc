# Função para padronização do DF com as medições e validações gerais
col_name <- c(
  'CD_PROC_IMPORTA_HUB',
  'CD_IMPORTACAO_HUB',
  'CD_NIVEL',
  'CD_MEDICAO',
  'CD_MEDICAO_PLANTIO',
  'CD_MEDICAO_PARCELA',
  'ID_REGIAO',
  'ID_PROJETO',
  'CD_TALHAO',
  'NRO_PARCELA',
  'NUM_FILA',
  'NUM_ARVORE',
  'NUM_FUSTE',
  'TIP_ARV',
  'CAP',
  'HT')

padronizar <- function(df) {
  
  
  dados <- rbindlist(lapply(df$datapath, read_csv2,
                            locale=locale(encoding ="latin1",
                                          decimal_mark = ","),
                            col_names=F,
                            skip=1,
                            col_types=list('c','c','c','c','c','c','c','c','c','i','i','i','i','i','d','d'),
                            ))
  
  dados <- tibble(dados)
  
  colnames(dados) <- col_name
  
  # Resumo Parcelas | verificação das parcelas com menos do que 4 alturas medidas
  df_status <- dados %>%
    # Remoção das linhas com valores nulos
    na.omit() %>%
    
    # Cálculo da quantidade de alturas medidas por parcela, pelos campos qie identificam a parcela e o arquivo de importação
    count(CD_PROC_IMPORTA_HUB, CD_MEDICAO_PARCELA) %>%
    
    # Validação da regra de 4 alturas por parcela
    #FLG_ERRO_ALTURA==0 -> Ok
    #FLG_ERRO_ALTURA==1 -> Menos de 4 alturas
    #FLG_ERRO_ALTURA==2 -> Mais de 4 alturas
    mutate(FLG_ERRO_ALTURA = ifelse(n == 4, 0, ifelse(
      n < 4,1,2)),
      QTD_ARVORE = n) %>%
    # Seleção das colunas de interesse
    select(CD_PROC_IMPORTA_HUB, CD_MEDICAO_PARCELA, QTD_ARVORE, FLG_ERRO_ALTURA) %>%
    
    # Remução de possíveis duplicatas
    distinct()
  
  # Verificação dos erros:
  # - 1: alturas medidas, sem CAPs medidos
  # - 2: árvores com o tipo nulo
  
  dados <- dados %>%
    
    # Aplicação da regra
    mutate(FLG_ERRO1 = ifelse(is.na(CAP) & !is.na(HT), 1, 0),
           FLG_ERRO2 = ifelse(is.na(TIP_ARV), 1, 0)) %>%
    # Consolidação dos erros
    left_join(df_status, by=c('CD_PROC_IMPORTA_HUB', 'CD_MEDICAO_PARCELA')) %>%
    # Criação:
    # - de um identificador por slinha
    # - da coluna com os CAP transformado para DAP
    # - da identificação das árvores medidas
    mutate(NUM_INDEX = row_number(),
           DAP = CAP / pi,
           cap_1 = 1/CAP,
           log_ht=log(HT))
  
  c=0
  betas <- data.frame
  #AJUSTA UMA EQUAÇÃO DE CURTIS PARA CADA CD_PROC_IMPORTA HUB
  n_med =unique(dados$CD_PROC_IMPORTA_HUB)
  for (i in n_med){
    data = dados %>%
      filter(CD_PROC_IMPORTA_HUB == i)
    
    dados_ajuste <- data %>%
      filter(!is.na(HT))
    
    modelo_outlayer <- linear_regression(dados_ajuste$cap_1,dados_ajuste$log_ht)
    b0 <- modelo_outlayer$B0  # valor de B0
    b1 <- modelo_outlayer$B1  # valor de
    
    if(c==0){
      betas <- data.frame(CD_PROC_IMPORTA_HUB = i, b0 = b0, b1 = b1)
      c=c+1
    }else{
      row <- data.frame(CD_PROC_IMPORTA_HUB = i, b0 = b0, b1 = b1)
      betas <- rbind(betas, row) 
      c=c+1
      
    }
  }
  dados <- merge(dados, betas, by = "CD_PROC_IMPORTA_HUB", all.x = TRUE)
  
  
  dados$HT_CURTIS =curtis(dados$CAP,dados$b0,dados$b1) 


  
  dados = dados %>%
    mutate(OUTLAYER = ifelse((abs(HT-HT_CURTIS)/HT) >= 0.3,"Possível outlayer","HT Campo"))
  
  dados = dados %>%
    select(-cap_1,-log_ht)
  rm(df_status)
  
  return(list(dados = dados))
  
}

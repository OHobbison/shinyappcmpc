# col_train <- c(
#   "CD_MEDICAO","CD_MEDICAO_PARCELA","DAP",
#   "HT","DAP_MAXIMO","DAP_MEDIO","HT_MEDIO",
#   "DAP_RELATIVO")

# col_normalizacao <- c("DAP","HT","DAP_MAXIMO",
#                       "DAP_MEDIO","HT_MEDIO","DAP_RELATIVO")

col_train <- c(
  "CD_MEDICAO","CD_MEDICAO_PARCELA","DAP",
  "HT","DAP_MAXIMO","DAP_MEDIO","HT_MEDIO")

col_normalizacao <- c("DAP","HT","DAP_MAXIMO",
                      "DAP_MEDIO","HT_MEDIO")

run_rna_r <- function(data){
    data = data.frame(data)
    data$CAP <- as.numeric(data$CAP)
    data$HT <- as.numeric(data$HT)
    data$DAP = data$CAP / 3.14159265359
    contador = 0
    c=0
    nmed <- unique(data$CD_MEDICAO)
    for(i in nmed) {
      print(paste("Iniciando cod medição",i))
      contador = contador + 1
      
      # Seleção das árvores pertecentes ao código de medição
      arvore <- data[data$CD_MEDICAO == i, ]
      
      # Seleção das árvores com valor de DAP maiores do que zero e não nulo
      arvore_valida <- (arvore$DAP>0 & !is.na(arvore$DAP))
      
      # Cálculo do DAP médio por talhão
      dap_medio <- aggregate(
        x = list(DAP_MEDIO = arvore$DAP[arvore_valida]),
        by = list(CD_MEDICAO = arvore$CD_MEDICAO[arvore_valida]),
        FUN = mean)
      
      arvore <- merge(x = arvore, y = dap_medio, by=c('CD_MEDICAO'))
      
      rm(dap_medio)
      
      # Cálculo do DAP máximo por talhão
      dap_max <- aggregate(
        x = na.omit(list(DAP_MAXIMO = arvore$DAP[arvore_valida])),
        by = (list(CD_MEDICAO = arvore$CD_MEDICAO[arvore_valida])),
        FUN = max,na.rm=T)
      
      arvore <- merge(x = arvore, y = dap_max, by=c('CD_MEDICAO'))
      
      rm(dap_max)
      
      # Cálculo do DAP relativo (Validar se está de acordo com a metodologia da CMPC)
      arvore$DAP_RELATIVO <- arvore$DAP / arvore$DAP_MEDIO
      
      # Seleção das árvores:
      # com valor de DAP maiores do que zero
      # com valor de HT maiores do que zero
      # com valor de DAP não nulas
      # com valor de HT não nulas
      arvore_valida <- (arvore$DAP>0 & !is.na(arvore$DAP) & arvore$HT > 0 & !is.na(arvore$HT))

      
      # Cálculo da HT média por talhão
      ht_medio <- aggregate(
        x = list(HT_MEDIO = arvore$HT[arvore_valida]),
        by = list(CD_MEDICAO = arvore$CD_MEDICAO[arvore_valida]),
        FUN = mean)
      
      arvore <- merge(x = arvore, y = ht_medio, by=c('CD_MEDICAO'))
      
      rm(ht_medio)
      
      #  Seleção das árvores para o treinamento da RNA
      arvore_treino <- arvore[col_train]
      #base_treino <- arvore[col_treino][arvore_valida,]
      base_treino <- arvore[col_train][,]
      
      #Verifica se há valores nulos, se tiver, substitui por 0
      if (length(base_treino[is.na(base_treino$DAP),]$DAP)>0){
        base_treino[is.na(base_treino$DAP),]$DAP <- 0
      }
      if (length(base_treino[is.na(base_treino$HT),]$HT)!=0){
        base_treino[is.na(base_treino$HT),]$HT <- 0
      }
      
      min_max_cols = list()
      
      for(coluna in col_normalizacao) {
        
        
        colValues = base_treino[coluna][!is.na(base_treino[coluna])]
        
        min_max_cols[[coluna]] = minmax_normalizer(values = colValues)
        
        
        base_treino[coluna][!is.na(base_treino[coluna])] = unlist(min_max_cols[[coluna]]['norm_values'], use.names = FALSE)
        
        if(coluna != 'HT') {
          arvore_treino[coluna] = unlist(minmax_normalizer(
            values = arvore_treino[coluna],
            min_value = min_max_cols[[coluna]]$min,
            max_value = min_max_cols[[coluna]]$max)['norm_values'], use.names = FALSE)
        }
        
      }
      #Filtra base de treino 
      base_treino <- base_treino[col_train][arvore_valida,]
      # Variáveis para validação se todas as parcelas possuem árvores medidas
      n_parcela_arvore_treino <- length(unique(arvore_treino$CD_MEDICAO_PLANTIO))
      n_parcela_base_treino <- length(unique(base_treino$CD_MEDICAO_PLANTIO))
      
      # Dados de treinamento
      X_train = torch_tensor(as.matrix(base_treino[c('DAP', 'DAP_MAXIMO', 'DAP_MEDIO', 'HT_MEDIO')]), 
                             dtype = torch_float(),
                             torch_device('cpu'))
      y_train = torch_tensor(as.matrix(base_treino[,'HT']), 
                             dtype = torch_float(),
                             torch_device('cpu'))
      train_ds = tensor_dataset(X_train, y_train)
      train_dl = dataloader(train_ds, batch_size = nrow(arvore_treino))
      
      # Dados a serem estimados
      X_predict = torch_tensor(as.matrix(arvore_treino[c('DAP', 'DAP_MAXIMO', 'DAP_MEDIO', 'HT_MEDIO')]), 
                               dtype = torch_float(),
                               device = torch_device("cpu"))$unsqueeze(1)
      
      #View(as.matrix(arvore_treino[c('DAP', 'DAP_MAXIMO', 'DAP_MEDIO', 'HT_MEDIO')]))
      
      
      if(n_parcela_arvore_treino == n_parcela_base_treino) {
        # Treinamento e Estimação das alturas caso a condição seja válida
        #Definindo callbacks
        
        callback_e_s = luz_callback_early_stopping(
          monitor = "train_loss",
          min_delta = 0.01,
          patience = 20,
          mode = "min")
        
        
        
        print("Utilizando net 1")
        rna = net
        
        fitted <- net %>%
          setup(
            #loss = nn_l1_loss(reduction = "mean"),
            loss = nn_mse_loss(),
            optimizer = optim_rprop,
            metrics = list(luz_metric_mae(),luz_metric_mse())) %>%
          set_hparams() %>%
          set_opt_hparams(lr = 0.01,
                          step_sizes=c(1e-06, 50)) %>%
          fit(data = train_dl,
              epochs = 1000,
              verbose = F,
              accelerator = accelerator(),
              callbacks= list(callback_e_s))
              
        htest <- ifelse(arvore$DAP == 0, 0, as.numeric(predict(fitted, X_predict)))
        htest <- minmax_denormalizer(values = htest,
                                     min_value = min_max_cols[['HT']]$min[1],
                                     max_value = min_max_cols[['HT']]$max[1])
        
        
        #Verificação se é árvore medida ou árvore de campo
        arvore$HTEST_RNA <- htest
        arvore$HT_FINAL <- ifelse(arvore$HT > 0 & !is.na(arvore$HT), arvore$HT, arvore$HTEST_RNA)
        arvore$ORIGEM <- ifelse(is.na(arvore$HT) & is.na(arvore$DAP),"N medido",ifelse(arvore$HT > 0 & !is.na(arvore$HT), 'Campo', 'Estimada'))
        #Avaliação da rede mape deve ser menor que 25
        #r2 deve ser >= 0 
        #altura máxima deve set menor de 65 metros
        #Altura mínima deve set maior que 30 centímetros
        
        metrics = get_metrics(arvore)
        mape = metrics[1]
        r2= metrics[2]
        min_ht= metrics[3]
        max_ht= metrics[4]
        
        used_net = 'net 1'
        iterator = 0
        while (mape >= 25 || max_ht > 650 || min_ht < 3 || r2 <= 0) {
          if (iterator <1){
            used_net = 'net 2'
            iterator = iterator +1
            print("Utilizando net 2")
            rna = net_2
            
            callback_e_s = luz_callback_early_stopping(
              monitor = "train_loss",
              min_delta = 0.01,
              patience = 50,
              mode = "min")
            
            fitted <- rna %>%
              setup(
                loss = nn_mse_loss(),
                optimizer = optim_rprop,
                metrics = list()) %>%
              set_hparams() %>%
              set_opt_hparams(lr = 0.01) %>%
              fit(data = train_dl,
                  epochs = 1000,
                  verbose = F,
                  accelerator = accelerator(),
                  callbacks= list(callback_e_s))
            
            # dim(X_train)
            # dim(X_predict)
            #plot(fitted)
            
            htest <- ifelse(arvore$DAP == 0, 0, as.numeric(predict(fitted, X_predict)))
            htest <- minmax_denormalizer(values = htest,
                                         min_value = min_max_cols[['HT']]$min[1],
                                         max_value = min_max_cols[['HT']]$max[1])
            
            
            #Verificação se é árvore medida ou árvore de campo
            arvore$HTEST_RNA <- htest
            arvore$HT_FINAL <- ifelse(arvore$HT > 0 & !is.na(arvore$HT), arvore$HT, arvore$HTEST_RNA)
            arvore$ORIGEM <- ifelse(is.na(arvore$HT) & is.na(arvore$DAP),"N medido",ifelse(arvore$HT > 0 & !is.na(arvore$HT), 'Campo', 'Estimada'))
            #Avaliação da rede
            
            metrics = get_metrics(arvore)
            mape = metrics[1]
            r2= metrics[2]
            min_ht= metrics[3]
            max_ht= metrics[4]
            print("Métricas net 2")
            print(metrics)
          }else if(iterator<2){
            iterator = iterator +1 
            print("Utilizando net 3")
            used_net = 'net 3'
            rna = net_3
            
            callback_e_s = luz_callback_early_stopping(
              monitor = "train_loss",
              min_delta = 0.01,
              patience = 50,
              mode = "min")
            
            fitted <- rna %>%
              setup(
                loss = nn_mse_loss(),
                optimizer = optim_rprop,
                metrics = list()) %>%
              set_hparams() %>%
              set_opt_hparams(lr = 0.01) %>%
              fit(data = train_dl,
                  epochs = 1000,
                  verbose = F,
                  accelerator = accelerator(),
                  callbacks= list(callback_e_s))
            
            # dim(X_train)
            # dim(X_predict)
            #plot(fitted)
            
            htest <- ifelse(arvore$DAP == 0, 0, as.numeric(predict(fitted, X_predict)))
            htest <- minmax_denormalizer(values = htest,
                                         min_value = min_max_cols[['HT']]$min[1],
                                         max_value = min_max_cols[['HT']]$max[1])
            
            
            #Verificação se é árvore medida ou árvore de campo
            arvore$HTEST_RNA <- htest
            arvore$HT_FINAL <- ifelse(arvore$HT > 0 & !is.na(arvore$HT), arvore$HT, arvore$HTEST_RNA)
            arvore$ORIGEM <- ifelse(is.na(arvore$HT) & is.na(arvore$DAP),"N medido",ifelse(arvore$HT > 0 & !is.na(arvore$HT), 'Campo', 'Estimada'))
            #Avaliação da rede
            
            metrics = get_metrics(arvore)
            mape = metrics[1]
            r2= metrics[2]
            min_ht= metrics[3]
            max_ht= metrics[4]
            
            print("Métricas net 3")
            print(metrics)
            
          }else if(iterator < 3){
            iterator = iterator +1 
            print("Utilizando net 4")
            used_net = 'net 4'
            rna = net_4
            
            callback_e_s = luz_callback_early_stopping(
              monitor = "train_loss",
              min_delta = 0.01,
              patience = 50,
              mode = "min")
            
            fitted <- rna %>%
              setup(
                loss = nn_mse_loss(),
                optimizer = optim_rprop,
                metrics = list()) %>%
              set_hparams() %>%
              set_opt_hparams(lr = 0.01) %>%
              fit(data = train_dl,
                  epochs = 1000,
                  verbose = F,
                  accelerator = accelerator(),
                  callbacks= list(callback_e_s))
            
            # dim(X_train)
            # dim(X_predict)
            #plot(fitted)
            
            htest <- ifelse(arvore$DAP == 0, 0, as.numeric(predict(fitted, X_predict)))
            htest <- minmax_denormalizer(values = htest,
                                         min_value = min_max_cols[['HT']]$min[1],
                                         max_value = min_max_cols[['HT']]$max[1])
            
            
            #Verificação se é árvore medida ou árvore de campo
            arvore$HTEST_RNA <- htest
            arvore$HT_FINAL <- ifelse(arvore$HT > 0 & !is.na(arvore$HT), arvore$HT, arvore$HTEST_RNA)
            #Avaliação da rede
            
            metrics = get_metrics(arvore)
            mape = metrics[1]
            r2= metrics[2]
            min_ht= metrics[3]
            max_ht= metrics[4]
            
            print("Métricas net 4")
            print(metrics)
          }else if(iterator>=3){
            print("Caiu no loop final, aplicando rna 1")
            print("Utilizando net 1  e finalizando.")
            used_net = 'net 1 - Loop final' 
            rna = net
            
            callback_e_s = luz_callback_early_stopping(
              monitor = "train_loss",
              min_delta = 0.01,
              patience = 50,
              mode = "min")
            
            fitted <- rna %>%
              setup(
                loss = nn_mse_loss(),
                optimizer = optim_rprop,
                metrics = list()) %>%
              set_hparams() %>%
              set_opt_hparams(lr = 0.01) %>%
              fit(data = train_dl,
                  epochs = 1000,
                  verbose = F,
                  accelerator = accelerator(),
                  callbacks= list(callback_e_s))
            # dim(X_train)
            # dim(X_predict)
            #plot(fitted)
            
            htest <- ifelse(arvore$DAP == 0, 0, as.numeric(predict(fitted, X_predict)))
            htest <- minmax_denormalizer(values = htest,
                                         min_value = min_max_cols[['HT']]$min[1],
                                         max_value = min_max_cols[['HT']]$max[1])
            
            
            #Verificação se é árvore medida ou árvore de campo
            arvore$HTEST_RNA <- htest
            arvore$HT_FINAL <- ifelse(arvore$HT > 0 & !is.na(arvore$HT), arvore$HT, arvore$HTEST_RNA)
            arvore$ORIGEM <- ifelse(is.na(arvore$HT) & is.na(arvore$CAP),"N medido",ifelse(arvore$HT > 0 & !is.na(arvore$HT), 'Campo', 'Estimada'))
            #Avaliação da rede
            
            metrics = get_metrics(arvore)
            mape = metrics[1]
            r2= metrics[2]
            min_ht= metrics[3]
            max_ht= metrics[4]
            break
          }
          
        }
        print(paste("RNA aplicada! Modelo utilizado = ",used_net,".",sep=""))
        
        
        
      } else {
        
        print(i)
        
      }
      if(c == 0) {
        
        consolidado_rna <- arvore
        c=c+1
        
        
      } else {
        
        consolidado_rna <- rbind(consolidado_rna, arvore)
        c=c+1
        
      }
    }
    return(consolidado_rna)
}



#Redes neurais

net <- torch::nn_module(
  "Net",
  initialize = function() {

    self$model <- nn_sequential(
      nn_linear(4, 3),
      nn_sigmoid(),
      nn_linear(3, 1),
      nn_sigmoid()
    )

  },

  forward = function(x) {

    x %>% self$model()

  }
)

net_2 <- torch::nn_module(
  "Net",
  initialize = function() {
    
    self$model <- nn_sequential(
      nn_linear(4, 4),
      nn_sigmoid(),
      nn_linear(4, 1),
      nn_sigmoid()
    )
    
  },
  
  forward = function(x) {
    
    x %>% self$model()
    
  }
)

net_3 <- torch::nn_module(
  "Net",
  initialize = function() {
    
    self$model <- nn_sequential(
      nn_linear(4, 3),
      nn_sigmoid(),
      nn_linear(3, 2),
      nn_sigmoid(),
      nn_linear(2, 1),
      nn_sigmoid(),
    )
    
  },
  
  forward = function(x) {
    
    x %>% self$model()
    
  }
)


net_4 <- torch::nn_module(
  "Net",
  initialize = function() {
    
    self$model <- nn_sequential(
      nn_linear(4, 4),
      nn_sigmoid(),
      nn_linear(4, 4),
      nn_sigmoid(),
      nn_linear(4, 1),
      nn_sigmoid(),
    )
    
  },
  
  forward = function(x) {
    
    x %>% self$model()
    
  }
)




#Função que normaliza os dados
minmax_normalizer <- function(values, min_value = NULL, max_value = NULL) {
  if (is.null(min_value) || is.null(max_value)) {
    min_value <- min(values)
    max_value <- max(values)
  }
  
  if (min_value == max_value) {
    norm_values <- rep(0.5, length(values))
  } else {
    norm_values <- (values - min_value) / (max_value - min_value)
  }
  
  d <- list()
  d$min <- min_value
  d$max <- max_value
  d$norm_values <- norm_values
  
  return(d)
}
#Função que desnormaliza os dados
minmax_denormalizer <- function(values, min_value, max_value) {
  desnorm_values <- values * (max_value - min_value) + min_value
  
  return(as.numeric(desnorm_values))
}

get_r2 = function(y_true,y_pred){
  sst = sum((y_true - mean(y_true))^2)
  sse = sum((y_pred - y_true)^2)
  r2 = 1 -(sse/sst)
  return(r2)
  
}


get_metrics = function(arvore){
  
  base_metricas = arvore %>%
    filter(ORIGEM=="Campo")
  
  y_true = base_metricas$HT
  y_pred = base_metricas$HTEST_RNA
  
  mape = mean(abs((y_true-y_pred)/y_true))*100
  r2 = get_r2(y_true,y_pred)
  
  min_ht = min(arvore$HTEST_RNA, na.rm=TRUE)
  max_ht = max(arvore$HTEST_RNA, na.rm=TRUE)
  
  return(list(mape,r2,min_ht,max_ht))
}

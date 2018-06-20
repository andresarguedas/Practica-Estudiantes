analisis <- function(connection, inicio = 2000, fin = NULL) {
  require(RPostgreSQL)
  require(magrittr)
  require(xlsx)
  require(dplyr)
  est <- dbGetQuery(connection, "SELECT carne, ano, nombre_curso, sigla 
                    FROM cursos 
                    WHERE sigla = 'XS1110' OR sigla = 'XS0111';")
  est1 <- aggregate(est$ano, by = list(est$carne), FUN = min)
  u <- unique(est$ano)
  u <- u[order(u)]
  if(is.null(fin)) {
    fin <- max(u)
  }
  if(inicio > fin) {
    stop(paste("La fecha de inicio (", inicio, 
               ") es mayor a la fecha de finalización (", fin, ")", sep = ""))
  }
  if(inicio < min(u)) {
    message(paste("La fecha de inicio (", inicio, 
               ") es menor al primer año disponible (", min(u),
               ") por lo que se empezará desde el ", min(u), sep = ""))
    inicio <- min(u)
  }
  if(fin > max(u)) {
    message(paste("La fecha de finalización (", fin, 
               ") es mayor al último año disponible (", max(u),
               ") por lo que se terminará en el ", max(u), sep = ""))
    fin <- max(u)
  }
  M <- length(u)
  df <- data.frame(matrix(0, ncol = M + 1, nrow = M + 1), 
                   row.names = c(as.character(u), "Retiro"))
  colnames(df) <- c(as.character(u), "Graduados")
  l.c <- c("XS0111", "XS0113", "XS0121", "XS0211", "XS0212", "XS0220", "XS0221", 
           "XS0222", "XS0223", "XS0312", "XS0313", "XS0314", "XS0321", "XS0322",
           "XS0323", "XS0324", "XS0411", "XS0412", "XS0413", "XS0414", "XS0421",
           "XS0422", "XS0423", "XS0424", "MA0001", "XS1110", "LM1030", "XE0156",
           "XS1130", "MA1021", "XS2210", "XS2310", "XS2110", "MA1004", "XS2230",
           "XS2330", "XS2130", "MA1023", "XS3150", "XS3110", "XS3310", "XS3010",
           "XS3170", "XS3130", "XS3210", "XS3510", "XS4410", "XS4510", "XS4110",
           "XS4010", "XS4430", "XS4530", "XS4050", "XS4030", "MA0213", "MA0232",
           "MA0313", "XS3220", "XP5028", "XP5033", "PS0001", "CP1212", "CP1500",
           "FS0107", "FS0101", "FS0115", "B 0350", "LM2003", "LM2004", "LM1032",
           "LM1031", "MA0125")
  for(j in 1:M) {
    est. <- subset(est1, x == u[j])$Group.1
    l <- list()
    for(i in 1:length(est.)) {
      l[[i]] <- dbGetQuery(connection, paste("SELECT carne, ano, nombre_curso, sigla 
                                       FROM cursos 
                                       WHERE carne = '",
                                       est.[i],
                                       "' AND ano >= ", 
                                       u[j], 
                                       ";", 
                                       sep = ""))
      l[[i]] <- subset(l[[i]], l[[i]]$sigla %in% l.c)
    }
    for(i in 1:length(l)) {
      l[[i]] <- unique(l[[i]]$ano)
    }
    l %<>% unlist()
    l %<>% table()
    for(i in 1:length(l)) {
      w <- which(colnames(df) == names(l)[i])
      df[j, w] <- l[i]
    }
  }
  grad <- dbGetQuery(connection, "SELECT carne FROM graduados;")
  for(i in 1:nrow(grad)) {
    if(grepl("^[0-9]", grad[i, 1])) {
      grad[i, 1] <- NA
    }
  }
  grad %<>% na.omit()
  for(i in 1:nrow(grad)) {
    grad[i, 1] %<>% gsub("A", "200", .) %>%
                    gsub("B", "201", .) %>%
                    substr(., 1, 4)
  }
  gradl <- table(grad)
  for(i in 1:length(gradl)) {
    df[which(rownames(df) == names(gradl)[i]), ncol(df)] <- as.numeric(gradl[i])
  }
  #est1$graduados <- 0
  #for(k in 1:nrow(est1)) {
  #  if(est1$Group.1[k] %in% grad$carne) {
  #    wh <- which(grad$carne == est1$Group.1[k])
  #    est1$graduados[k] <- grad$ano[wh]
  #  }
  #}
  #d <- aggregate(est1$Group.1, by = list(est1$graduados), FUN = length)
  #d <- d[-1, ]
  #for(i in 1:nrow(d)) {
  #  df[nrow(df), which(colnames(df) == as.character(d$Group.1[i]))] <- d$x[i]
  #}
  df %<>% select(., paste(inicio:fin), "Graduados")
  df <- df[c(paste(inicio:fin), "Retiro"), ]
  M1 <- nrow(df)
  cs <- apply(df, 2, sum)
  for(i in 1:(ncol(df) - 1)) {
    if(i == 1) {
      df[M1, 1] <- 0
    } else {
      df[M1, i] <- cs[i - 1] + df[i, i] - cs[i]
    }
  }
  write.xlsx(df, paste("tabla de cohortes ", inicio, "-", fin, ".xlsx", sep = ""))
  message(paste("Los resultados se salvaron en el archivo '" , 
                paste("tabla de cohortes ", inicio, "-", fin, ".xlsx", sep = ""), 
                "'", sep = ""))
  return(df)
}

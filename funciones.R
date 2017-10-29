analisis <- function(connection) {
  require(RPostgreSQL)
  require(magrittr)
  est <- dbGetQuery(connection, "SELECT carne, ano, nombre_curso, sigla 
                    FROM cursos 
                    WHERE sigla = 'XS1110' OR sigla = 'XS0111';")
  est1 <- aggregate(est$ano, by = list(est$carne), FUN = min)
  u <- unique(est$ano)
  u <- u[order(u)]
  M <- length(u)
  df <- data.frame(matrix(0, ncol = M + 1, nrow = M), 
                   row.names = as.character(u))
  colnames(df) <- c(as.character(u), "Graduados")
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
  return(df)
}

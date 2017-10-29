foo <- function(connection) {
  est <- dbGetQuery(connection, "SELECT carne, ano, nombre_curso, sigla 
                    FROM cursos 
                    WHERE sigla = 'XS1110' OR sigla = 'XS0111';")
  est1 <- aggregate(est$ano, by = list(est$carne), FUN = min)
  u <- unique(est$ano)
  u <- u[order(u)]
  M <- length(u)
  df <- data.frame(matrix(0, ncol = M, nrow = M + 1), 
                   row.names = c(as.character(u), "Graduados"))
  colnames(df) <- as.character(u)
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
  grad <- dbGetQuery(connection, "SELECT carne, EXTRACT(year FROM fecha_juramentacion) AS ano
                   FROM graduados;")
  est1$graduados <- 0
  for(k in 1:nrow(est1)) {
    if(est1$Group.1[k] %in% grad$carne) {
      wh <- which(grad$carne == est1$Group.1[k])
      est1$graduados[k] <- grad$ano[wh]
    }
  }
  d <- aggregate(est1$Group.1, by = list(est1$graduados), FUN = length)
  d <- d[-1, ]
  for(i in 1:nrow(d)) {
    df[nrow(df), which(colnames(df) == as.character(d$Group.1[i]))] <- d$x[i]
  }
  return(df)
}

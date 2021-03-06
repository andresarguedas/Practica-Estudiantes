c("#698B22")library(readxl)
library(magrittr)
library(dplyr)
library(lubridate)
library(RPostgreSQL)
library(readr)

options(scipen = 999)

Graduados <- read_excel("F:/Practica II/Graduados.xlsx")
Cursos <- read_excel("F:/Practica II/Historial de calificaciones.xlsx")
Estudiantes <- read_excel("F:/Practica II/Información Personal Estudiantes.xlsx")

# Arregla el carne en los casos en los que no esta bien:

u <- unique(Cursos[, 2:5])
u <- u[, c(1, 3, 2, 4)]
a <- Estudiantes[, 3:6]
b <- anti_join(a, u)
b1 <- anti_join(b[, 2:4], u[, 2:4])
bf <- anti_join(b, b1)
bf[, 1] <- inner_join(bf[, 2:4], u)[, 4]
for (i in 1:nrow(bf)) {
  num <- which(Estudiantes$APELLIDO1 %in% bf[i, 2] & 
                 Estudiantes$APELLIDO2 %in% bf[i, 3] & 
                 Estudiantes$NOMBRE %in% bf[i, 4])
  Estudiantes[num, 3] <- bf[i, 1]
}

# Agrega el año de ingreso a la UCR con base en el carne:

s <- substr(Estudiantes$CARNE, start = 1, stop = 2)
s %<>% gsub("A", "200", .)
s %<>% gsub("B", "201", .)

num <- which(is.na(s == Estudiantes$AÑO_INGRESO_UCR))
Estudiantes[num, 2] <- s[num]

names(Estudiantes)[2] <- "ANO_INGRESO_UCR"
names(Cursos)[11] <- "ANO"

names(Graduados) %<>% tolower()
names(Cursos) %<>% tolower()
names(Estudiantes) %<>% tolower()

Graduados$fecha_juramentacion %<>% ymd()
Cursos$nombre_curso %<>% chartr('ÁÉÍÓÚÑ','AEIOUN', .)
Estudiantes$canton %<>% chartr('ÁÉÍÓÚÑ','AEIOUN', .)
Estudiantes$distrito %<>% chartr('ÁÉÍÓÚÑ','AEIOUN', .)

conn <- dbConnect(PostgreSQL(), host = "localhost", user = "postgres", 
                  dbname = "estudiantes", password = "")

postgresqlpqExec(conn, "SET client_encoding = 'windows-1252'")

dbWriteTable(conn, "graduados", Graduados)
dbWriteTable(conn, "estudiantes", Estudiantes)
dbWriteTable(conn, "cursos", Cursos)

dbGetQuery(conn, 'ALTER TABLE estudiantes DROP COLUMN "row.names";')
dbGetQuery(conn, 'ALTER TABLE estudiantes ADD PRIMARY KEY ("carne");')
dbGetQuery(conn, 'ALTER TABLE graduados DROP COLUMN "row.names";')
dbGetQuery(conn, 'ALTER TABLE graduados ADD PRIMARY KEY ("id");')
dbGetQuery(conn, 'ALTER TABLE cursos DROP COLUMN "row.names";')
dbGetQuery(conn, 'ALTER TABLE cursos ADD PRIMARY KEY ("id");')

est <- dbGetQuery(conn, "SELECT carne, ano, nombre_curso, sigla FROM cursos WHERE nombre_curso = 'ESTADISTICA INTRODUCTORIA I' OR nombre_curso = 'ESTADISTICA INTRODUCTORIA';")
est1 <- aggregate(est$ano, by = list(est$carne), FUN = min)
est2 <- aggregate(est1$Group.1, by = list(est1$x), FUN = length)


est_04 <- subset(est1, x == 2004)$Group.1

l <- list()

for(i in 1:length(est_04)) {
  l[[i]] <- dbGetQuery(conn, paste("SELECT carne, ano, nombre_curso, sigla FROM cursos WHERE carne = '", est_04[i], "' AND ano >= 2004;", sep = ""))
}

l1 <- list()

for(i in 1:length(l)) {
  l1[[i]] <- unique(l[[i]]$ano)
}

l2 <- unlist(l1)
table(l2)

grad <- dbGetQuery(conn, "SELECT carne, EXTRACT(year FROM fecha_juramentacion) AS ano
                   FROM graduados;")

# Agregando nuevos datos: del 2015

grad2015<- read_delim("F:/Practica II/Estudiantes graduados escuela de Estadística, años 2015 y 2016.csv", ";", escape_double = FALSE, trim_ws = TRUE)

names(grad2015) <- tolower(names(grad2015))
names(grad2015)[1] <- "carne"
names(grad2015)[5] <- "fecha_juramentacion"
grad2015$fecha_juramentacion <- ymd(paste(grad2015$fecha_juramentacion, "-1-1", sep = ""))
grad2015$cod_titulo <- as.character(grad2015$cod_titulo)
grad2015 <- grad2015[, -8]

grad2015. <- anti_join(grad2015, grad)

M <- dbGetQuery(conn, "SELECT max(id) FROM graduados;")
M %<>% as.numeric()

grad2015.$id <- seq(M + 1, M + nrow(grad2015.))

dbWriteTable(conn, "graduados", value = grad2015., append = TRUE, 
             row.names = FALSE)

est2015 <- read_delim("F:/Practica II/Estudiantes matriculados escuela de Estadística, años 2015 y 2016.csv", 
                      ";", escape_double = FALSE, trim_ws = TRUE)

names(est2015) <- tolower(names(est2015))
names(est2015)[11] <- "ano_ingreso_ucr"
est2015$ano_ingreso_ucr %<>% as.character()
est2015$fecha_nacimiento %<>% dmy() %>% ymd()
est2015$canton %<>% chartr('ÁÉÍÓÚÑ','AEIOUN', .)
est2015$distrito %<>% chartr('ÁÉÍÓÚÑ','AEIOUN', .)
est2015$provincia %<>% chartr('ÁÉÍÓÚÑ','AEIOUN', .)

est <- dbGetQuery(conn, "SELECT * FROM estudiantes;")
est$fecha_nacimiento %<>% as.Date() %>% ymd()

est2015. <- anti_join(est2015, est, by = "carne")

M <- dbGetQuery(conn, "SELECT max(id) FROM estudiantes;")
M %<>% as.numeric()

est2015.$id <- seq(M + 1, M + nrow(est2015.))

est2015. <- est2015.[, c(12, 11, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)]

est2015.$promedio_admision <- 999

dbWriteTable(conn, "estudiantes", value = est2015., append = TRUE, 
             row.names = FALSE)

cur2015 <- read_delim("F:/Practica II/Notas de los Estudiantes escuela de Estadística, años 2015 y 2016.csv", 
                      ";", escape_double = FALSE, trim_ws = TRUE)

names(cur2015) %<>% tolower()
names(cur2015)[2] <- "ano"

cur2015$nota_ordinaria_alf %<>% as.numeric()
cur2015$nota_ordinaria_alf[is.na(cur2015$nota_ordinaria_alf)] <- 0
cur2015$periodo %<>% as.character()
cur2015$nombre_curso %<>% chartr('ÁÉÍÓÚÑ','AEIOUN', .)

cur <- dbGetQuery(conn, "SELECT * FROM cursos;")

names(cur)
cur2015. <- anti_join(cur2015, cur)

M <- dbGetQuery(conn, "SELECT max(id) FROM cursos;")
M %<>% as.numeric()

cur2015.$id <- seq(M + 1, M + nrow(cur2015.))
names(cur2015.)[7] <- "nota_ordinaria_num"

cur2015. <- cur2015.[, c(11, 1, 5, 4, 6, 8, 9, 7, 10, 3, 2)]

dbWriteTable(conn, "cursos", value = cur2015., append = TRUE, 
             row.names = FALSE)


# Estudiantes del 2014:

est <- dbGetQuery(conn, "SELECT * FROM estudiantes;")
est$fecha_nacimiento %<>% as.Date() %>% ymd()

est20141 <- read_delim("F:/Practica II/Estudiantes I - 2014.csv", 
                       ";", escape_double = FALSE, trim_ws = TRUE)

names(est20141) %<>% tolower()
s <- est20141$carne
s1 <- grep("^[0-9]", s, value = F)
s[s1] <- paste("19", substr(s[s1], 1, 2), sep = "")
s %<>% gsub("A", "200", .)
s %<>% gsub("B", "201", .)
s %<>% substr(., 1, 4)

est20141$ano_ingreso_ucr <- s

est20141$fecha_nacimiento %<>% dmy()
est20141$fecha_nacimiento %<>% ymd()
est20141$canton %<>% chartr('ÁÉÍÓÚÑ','AEIOUN', .)
est20141$distrito %<>% chartr('ÁÉÍÓÚÑ','AEIOUN', .)
est20141$provincia %<>% chartr('ÁÉÍÓÚÑ','AEIOUN', .)

est. <- anti_join(est20141, est, by = "carne")

M <- dbGetQuery(conn, "SELECT max(id) FROM estudiantes;")
M %<>% as.numeric()

est.$id <- seq(M + 1, M + nrow(est.))

est. <- est.[, c(13, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]

dbWriteTable(conn, "estudiantes", value = est., append = TRUE, 
             row.names = FALSE)

est <- dbGetQuery(conn, "SELECT * FROM estudiantes;")
est$fecha_nacimiento %<>% as.Date() %>% ymd()

est20142 <- read_delim("F:/Practica II/Estudiantes II - 2014.csv", 
                       ";", escape_double = FALSE, trim_ws = TRUE)

names(est20142) %<>% tolower()
s <- est20142$carne
s1 <- grep("^[0-9]", s, value = F)
s[s1] <- paste("19", substr(s[s1], 1, 2), sep = "")
s %<>% gsub("A", "200", .)
s %<>% gsub("B", "201", .)
s %<>% substr(., 1, 4)

est20142$ano_ingreso_ucr <- s

est20142$fecha_nacimiento %<>% dmy()
est20142$fecha_nacimiento %<>% ymd()
est20142$canton %<>% chartr('ÁÉÍÓÚÑ','AEIOUN', .)
est20142$distrito %<>% chartr('ÁÉÍÓÚÑ','AEIOUN', .)
est20142$provincia %<>% chartr('ÁÉÍÓÚÑ','AEIOUN', .)

est20142 <- est20142[, -c(2, 3)]

est.. <- anti_join(est20142, est, by = "carne")

M <- dbGetQuery(conn, "SELECT max(id) FROM estudiantes;")
M %<>% as.numeric()

est..$id <- seq(M + 1, M + nrow(est..))

est.. <- est..[, c(13, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]

dbWriteTable(conn, "estudiantes", value = est.., append = TRUE, 
             row.names = FALSE)

# Graduados del 2014:

grad <- dbGetQuery(conn, "SELECT carne, EXTRACT(year FROM fecha_juramentacion) AS ano
                   FROM graduados;")

grad20141 <- read_delim("F:/Practica II/Graduados I - 2014.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)

names(grad20141) %<>% tolower()
grad20141$fecha_juramentacion %<>% dmy()
grad20141$ano <- year(grad20141$fecha_juramentacion)
grad20141$cod_titulo %<>% as.character()

grad. <- anti_join(grad20141, grad)

M <- dbGetQuery(conn, "SELECT max(id) FROM graduados;")
M %<>% as.numeric()

grad. <- grad.[, -8]

grad.$id <- seq(M + 1, M + nrow(grad.))

grad. <- grad.[, c(8, 1, 2, 3, 4, 5, 6, 7)]

dbWriteTable(conn, "graduados", value = grad., append = TRUE, 
             row.names = FALSE)

grad20142 <- read_delim("F:/Practica II/Graduados II - 2014.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)

names(grad20142) %<>% tolower()
grad20142$fecha_juramentacion %<>% dmy()
grad20142$ano <- year(grad20142$fecha_juramentacion)
grad20142$cod_titulo %<>% as.character()

grad <- dbGetQuery(conn, "SELECT carne, EXTRACT(year FROM fecha_juramentacion) AS ano
                   FROM graduados;")

grad.. <- anti_join(grad20142, grad)

M <- dbGetQuery(conn, "SELECT max(id) FROM graduados;")
M %<>% as.numeric()

grad.. <- grad..[, -8]

grad..$id <- seq(M + 1, M + nrow(grad..))

grad.. <- grad..[, c(8, 1, 2, 3, 4, 5, 6, 7)]

dbWriteTable(conn, "graduados", value = grad.., append = TRUE, 
             row.names = FALSE)

# Notas del 2014:

notas20141 <- read_delim("F:/Practica II/Notas I - 2014.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)

names(notas20141) %<>% tolower()

notas20141$nota_ordinaria_alf %<>% as.numeric()
notas20141$nota_ordinaria_alf[is.na(notas20141$nota_ordinaria_alf)] <- 0
notas20141$periodo <- "1"
notas20141$ano <- 2014
notas20141$creditos %<>% gsub(",0", "", .)
notas20141$creditos %<>% as.numeric()

notas <- dbGetQuery(conn, "SELECT * FROM cursos;")
uni <- unique(cbind(notas$sigla, notas$nombre_curso))
notas20141$nombre_curso <- ""
for(i in 1:nrow(notas20141)) {
  notas20141$nombre_curso[i] <- uni[which(uni == notas20141$sigla[i])[1], 2]
}

notas. <- anti_join(notas20141, notas)

M <- dbGetQuery(conn, "SELECT max(id) FROM cursos;")
M %<>% as.numeric()

notas.$id <- seq(M + 1, M + nrow(notas.))
names(notas.)[7] <- "nota_ordinaria_num"
notas. <- notas.[, -5]

notas. <- notas.[, c(11, 1, 3, 2, 4, 5, 10, 6, 7, 8, 9)]

dbWriteTable(conn, "cursos", value = notas., append = TRUE, 
             row.names = FALSE)

notas20142 <- read_delim("F:/Practica II/Notas II - 2014.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)

names(notas20142) %<>% tolower()
names(notas20142)[1] <- "ano"
notas20142$nota_ordinaria_alf %<>% as.numeric()
notas20142$nota_ordinaria_alf[is.na(notas20142$nota_ordinaria_alf)] <- 0
notas20142 <- notas20142[, -7]
notas20142$periodo %<>% as.character()

notas <- dbGetQuery(conn, "SELECT * FROM cursos;")

notas.. <- anti_join(notas20142, notas)

M <- dbGetQuery(conn, "SELECT max(id) FROM cursos;")
M %<>% as.numeric()

notas..$id <- seq(M + 1, M + nrow(notas..))
names(notas..)[9] <- "nota_ordinaria_num"

notas.. <- notas..[, c(11, 3, 5, 4, 6, 7, 8, 9, 10, 2, 1)]

dbWriteTable(conn, "cursos", value = notas.., append = TRUE, 
             row.names = FALSE)

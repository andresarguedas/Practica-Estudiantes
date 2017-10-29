library(readxl)
library(magrittr)
library(dplyr)
library(lubridate)
library(RPostgreSQL)
library(readr)

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
                  dbname="estudiantes", password = "")

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

# Agregando nuevos datos:

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

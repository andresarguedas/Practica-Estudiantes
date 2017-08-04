library(readxl)

Graduados <- read_excel("F:/Practica II/Graduados.xlsx")
Cursos <- read_excel("F:/Practica II/Historial de calificaciones.xlsx")
Estudiantes <- read_excel("F:/Practica II/Información Personal Estudiantes.xlsx")

library(RPostgreSQL)
conn <- dbConnect(PostgreSQL(), host = "localhost", user = "postgres", 
                  dbname="estudiantes", password = "")

postgresqlpqExec(conn, "SET client_encoding = 'windows-1252'")

dbWriteTable(conn, "graduados", Graduados)
dbWriteTable(conn, "estudiantes", Estudiantes)
dbWriteTable(conn, "cursos", Cursos)

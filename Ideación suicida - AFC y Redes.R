# Librer?as

library(haven) #Leer .sav
library(psych) #Datos descriptivos y m?s
library(dplyr) #Funci?n %>% y recortar la base de datos
library(lavaan)#AFC y SEM
library(semTools)#Invarianza
library(semPlot)#graficos de aFC
library(EFAtools)#omega Y AFE
library(openxlsx)#Guardar
library(MVN)#normalidad
library(performance)
library(boot)
library(bootnet)
library(qgraph)
library(NetworkComparisonTest)
library(readxl)
da <- read_excel("D:/Bases de datos/Satisfaccion.xlsx")
View(da)
#Referencias
citation("boot")

##----------------------------------------------------------------------------------------------------------
#Colocar la ruta donde se exportar?n las hojas de c?lculo 
setwd('D:/Trabajos realizados/Satis')# F:/Psicologia/Analisis en R

#Importar datos
da <- read_excel("D:/Bases de datos/Satisfaccion.xlsx")

#Ver variables
names(da)

##----------------------------------------------------------------------------------------------------------
#Análisis de ítems
# Factor general
de<-dplyr::select (da,S1, S2, S3, S4, S5)

##---------------------------------------------------------------------------------------------------------
#Porcentaje de respuesta por ítem (F, %, M,DE, g1, g2, ihc, h2, id)
Tabla1<- rbind(table(de$is01), table(de$is02), table(de$is03),table(de$is04), table(de$is05))

#Modificar               
Tabla2<-prop.table(Tabla1, margin = 1)
TablaFrecuencia = Tabla2*100
TablaFrecuencia 
#Sobrescribir y crear un objeto
TablaFrecuencia <-as.data.frame(TablaFrecuencia)
TablaFrecuencia <- round(TablaFrecuencia,2) 
TablaFrecuencia
#Reemplazar por el factor que se desea evaluar

##----------------------------------------------------------------------------------------------------------
#Creación de objetos para el análisis de ítem
Descriptivos<-describe(de)  #Para M, DE, G1, G2

#Coeficiente de variación
CV <- (Descriptivos$sd /  Descriptivos$mean *100)
CV

#Para el IHC0
Matriz_G<-polychoric(de)#corr(F1)= Matriz Pearson, tetrachoric(F1)
Matriz_G
AlfaGeneral<-psych::alpha(Matriz_G$rho) #Para el IHC
AlfaGeneral

#AFE para comunalidad
AFEfactor<-fa(de,nfactors = 1,fm = "minres",rotate ="oblimin",cor = "poly")# de querer usar una matriz tetracorica "tet"
AFEfactor#Crear tabla con los datos que se necesitar?n en el an?lisis de ?tems

# Crear tabla (Modificar segun cantidad de ítems en la dimensión list(c(1:?)
TablaAnalisis <- list(c(1:5),Descriptivos$mean,Descriptivos$sd, Descriptivos$skew,Descriptivos$kurtosis,
                      AlfaGeneral$item.stats$r.drop, AlfaGeneral$alpha.drop$std.alpha,AFEfactor$communality)

TablaAnalisis <-as.data.frame(TablaAnalisis)
TablaAnalisis <- TablaAnalisis[,-1]

#Dar formato a los resultados
TablaAnalisis <- TablaAnalisis %>% 
  mutate_if(is.numeric, round, digits = 2)

#Nombrar y exportar en Excel
names(TablaAnalisis)<- c("M","DE","g1","g2","IHC","Alfa.drop","h2")
TablaAnalisis #AlfaGeneral$item.stats$r.cor#para alf si se elimina de la escalo o factor

#Concatenar ambos resultados para la tabla final de an?lisis de ?tems
TablaFinal <- list(cbind(TablaFrecuencia,TablaAnalisis))
TablaFinal
write.xlsx(TablaFinal, "Ideación.xlsx", colNames=TRUE, rowNames=TRUE)

##----------------------------------------------------------------------------------------------------------
#EMPEZAR LUEGO DE HABER EJECUTADO TODAS LAS DIMENSIONES
#Matriz de correlación poliecóricas
Matriz_G<-polychoric(de)
ImprimirMatriz <- as.data.frame(Matriz_G$rho)
ImprimirMatriz <- ImprimirMatriz %>% 
  mutate_if(is.numeric, round, digits = 2)
write.xlsx(ImprimirMatriz, "matriz.xlsx")

mvn(de, mvnTest  = "mardia")

##----------------------------------------------------------------------------------------------------------
# Creando un subconjunto de índices de ajuste que examinaremos en todos los modelos
#Análisis factorial confirmatorio
fit.subset<-c("chisq.scaled", "pvalue.scaled",
              "df.scaled","cfi.scaled", 
              "tli.scaled", "rmsea.scaled",
              "rmsea.ci.lower.scaled","rmsea.ci.upper.scaled",
              "srmr","wrmr")

# Probaremos modelos
#1) Modelo unidimensional congenerico
UNI.CFA.CONG<-'G=~ S1 + S2 + S3 + S4 + S5'

#Estimación
fit.UNI.CFA.CONG<-cfa(model = UNI.CFA.CONG, data =de,
                      estimator="MLR", 
                      mimic="Mplus", 
                      ordered = F) 

#std.lv=TRUE Colocar si es necesario igualar a 1 la regresión

# Evaluación del modelo
fitmeasures(fit.UNI.CFA.CONG, fit.subset)

# Generar Path Diagram con colores personalizados
semPaths(fit.UNI.CFA.CONG, 
         whatLabels = "std",  # Muestra valores estandarizados en los bordes
         style = "lisrel", 
         layout = "tree3", 
         intercepts = FALSE, 
         residuals = TRUE, 
         thresholds = FALSE, 
         rotation = 1, 
         sizeMan = 8, 
         sizeLat = 16, 
         shapeMan = "rectangle", 
         shapeLat = "circle", 
         edge.label.cex = 2,  # Tamaño de etiquetas en los caminos
         label.prop = 2,
         # Personalización de colores
         color = list(lat = "orange", man = "orange"), # Nodos en naranja
         edge.color = "blue", # Conexiones en azul sólido
         pastel = FALSE, # Mantiene colores sólidos
         esize = 2 # Grosor de los bordes
)


# Echemos un vistazo a las cargas estandarizadas
select(arrange(arrange(parameterestimates(fit.UNI.CFA.CONG, standardized = T)[1:5,], rhs),lhs),
       # solo ver un subconjunto de resultados de interés
       lhs:est, z, se, pvalue,std.all,)

#Indice de modificaciÓn
modindices(fit.UNI.CFA.CONG,sort=TRUE, maximum.number = 15)

# Extraer residuales
residuales <- residuals(fit.UNI.CFA.CONG, type = "cor")
residuales <- residuales$cov
residuales

#Matriz de correlación de los residuales
residuales <- as.data.frame(residuales)
write.xlsx(residuales, "matriz de residuales.xlsx")


# Calcular índices de modificación
mod_indices <- modindices(fit.UNI.CFA.CONG)

# Filtrar índices de modificación para residuales correlacionados
mod_indices_residuales <- mod_indices[mod_indices$op == "~~", ]
print(mod_indices_residuales)

##----------------------------------------------------------------------------------------------------------
#Cargas factoriales estandarizadas
Cargas <- standardizedSolution(fit.UNI.CFA.CONG)
Cargas

#Confiabilidad
Real1<-reliability(fit.UNI.CFA.CONG,return.total = TRUE)
Real1<-round(Real1,3)
Real1

# Compute omega y H
OMEGA(fit.UNI.CFA.CONG, g_name = "G")

# Extraer cargas factoriales estandarizadas
lambda <- select(arrange(arrange(parameterestimates(fit.UNI.CFA.CONG, standardized = T)[1:5,], rhs),lhs),
                                # solo ver un subconjunto de resultados de interés
                                lhs:est, z, se, pvalue,std.all)

lambda <- lambda$std.all
lambda

# Calcular el coeficiente H (Fórmula de Hancock & Mueller, 2001)

# Aplicar la fórmula de H según la imagen
H <- 1 / (1 + (1 / sum(lambda^2 / (1 - lambda^2))))
H

# Función para calcular H basado en la fórmula de la imagen
calcular_H <- function(de, indices) {
  lambda_boot <- de[indices]  # Resampleo de datos
  H_boot <- 1 / (1 + (1 / sum(lambda_boot^2 / (1 - lambda_boot^2))))
  return(H_boot)
}

# Ejecutar Bootstrap con 2000 iteraciones
boot_H <- boot(data = lambda, statistic = calcular_H, R = 1000)

# Calcular intervalos de confianza del 95%
IC_H <- boot.ci(boot_H, type = "perc")  # Método percentil

# Mostrar resultados
cat("Coeficiente H:", calcular_H(lambda), "\n")
cat("Intervalo de Confianza 95%:", IC_H$percent[4:5], "\n")


# Definir el modelo AFC con base en tu estructura
modelo_AFC <- '
  G =~ is01 + is02 + is03 + is04 + is05
'

# Ajustar el modelo a la base de datos `de`
fit.UNI.CFA.CONG <- cfa(modelo_AFC, data = de, ordered = colnames(de), estimator = "WLSMV")

# Extraer resultados de confiabilidad
Real1 <- reliability(fit.UNI.CFA.CONG, return.total = TRUE)

# Convertir a dataframe y asignar nombres de fila
Real1 <- as.data.frame(Real1)
rownames(Real1) <- c("alpha", "alpha.ord", "omega", "omega2", "omega3", "avevar")

# Obtener nombres de los coeficientes disponibles
coeficientes <- rownames(Real1)

# Mostrar los coeficientes y verificar que están bien estructurados
print(Real1)

# Función de Bootstrap para estimar los intervalos de confianza de cada coeficiente
calcular_confiabilidad_boot <- function(data, indices) {
  datos_boot <- data[indices, , drop = FALSE]  # Evitar que se convierta en vector si solo tiene una fila
  
  fit_boot <- tryCatch(
    cfa(modelo_AFC, data = datos_boot, ordered = colnames(de), estimator = "WLSMV"),
    error = function(e) return(rep(NA, length(coeficientes)))  # Manejar errores
  )
  
  if (is.na(fit_boot)) {
    return(rep(NA, length(coeficientes)))
  }
  
  # Calcular confiabilidad en la muestra bootstrap
  confiabilidad_boot <- tryCatch(
    reliability(fit_boot, return.total = TRUE),
    error = function(e) return(rep(NA, length(coeficientes)))
  )
  
  return(as.numeric(confiabilidad_boot))  # Convertir a vector numérico
}

# Ejecutar Bootstrap con 1000 iteraciones
boot_confiabilidad <- boot(data = de, statistic = calcular_confiabilidad_boot, R = 2000)

# Filtrar valores NA en los resultados de bootstrap
boot_confiabilidad$t <- na.omit(boot_confiabilidad$t)

# Calcular los intervalos de confianza 95% para cada coeficiente
IC_confiabilidad <- lapply(1:length(coeficientes), function(i) {
  boot.ci(boot_confiabilidad, type = "perc", index = i)
})

# Mostrar resultados finales para cada coeficiente
for (i in 1:length(coeficientes)) {
  cat("Coeficiente:", coeficientes[i], "=", Real1[i, "G"], "\n")
  cat("Intervalo de Confianza 95%: (", IC_confiabilidad[[i]]$percent[4], "-", IC_confiabilidad[[i]]$percent[5], ")\n\n")
}
##----------------------------------------------------------------------------------------------------------
#Validez basada en la relación con otras variables
#Cortar la base de datos o dataframe
df<-dplyr::select (da,Ideación_suicida,Depresión)

#Renombrar
names(df)<- c("Ideación suicida", "Depresión")

#crear directaente la tabla de correlaciones en APA en formato word
apa.cor.table(df, filename = "correlaciÓnapa.doc", 
              table.number = 2, show.conf.interval = FALSE, 
              landscape = TRUE)

#Valiez Predictiva mediante regresión logistica
names(da)
dg<-dplyr::select (da,Depresion_niveles,Ideación_suicida_dico)
#Renombrar
names(dg)<- c("Ideación suicida", "Depresión")

# Definir el modelo de regresión logística
modelo_logistico <- glm(Ideación_suicida_dico ~ Depresion_niveles, data = dg, family = binomial)


# **1️⃣ Prueba Omnibus (Razón de Verosimilitud - Chi-cuadrado)**
anova(modelo_logistico, test = "Chisq")

# **2️⃣ R² de Nagelkerke**
r2_nagelkerke(modelo_logistico)

# **3️⃣ Cálculo de OR con IC 90%**
or_ci_90 <- exp(cbind(OddsRatio = coef(modelo_logistico), confint(modelo_logistico, level = 0.90)))

# Mostrar Odds Ratio con IC 90%
print(or_ci_90)

#---------------------------------------------------------------------------------------------------------------------
#0) Modelo base
My_model<-'G=~ S1 + S2 + S3 + S4 + S5'

# Creando un subconjunto de índices de ajuste que examinaremos en todos los modelos
fit.subset<-c("chisq.scaled", 
              "df.scaled","cfi.scaled", 
              "rmsea.scaled",
              "srmr")


#Invariance #Para grupo etario solo se cambia "Sexo" y/o viseversa 
inv.sex.conf.wu<- measEq.syntax(configural.model = My_model, estimator="WLSMV", #Cambiar el estimador por ML, WLSMV
                                ID.fac = "std.lv", parameterization = "theta",
                                group = "Etario", data=da, ordered=T,
                                ID.cat = "Wu.Estabrook.2016",return.fit=TRUE)

# Evaluación del modelo
fitmeasures(inv.sex.conf.wu, fit.subset)

summary(inv.sex.conf.wu, fit.measures=TRUE)

#Invariance #Para grupo etario solo se cambia "Sexo" y/o viseversa 
inv.sex.thresh.wu<- measEq.syntax(configural.model = My_model,estimator="WLSMV", #Cambiar el estimador por ML, WLSMV
                                ID.fac = "std.lv", parameterization = "theta",
                                group = "Etario", data=da, ordered=T,
                                ID.cat = "Wu.Estabrook.2016",return.fit=TRUE,
                                group.equal = c("thresholds"))

# Evaluación del modelo
fitmeasures(inv.sex.thresh.wu, fit.subset)

summary(inv.sex.thresh.wu, fit.measures=TRUE)

## compare their fit to test threshold invariance
anova(inv.sex.conf.wu, inv.sex.thresh.wu)

#Considerar la matriz de datos para el uso del estimador
#group = "Sexo" Indicar los datos sociodemográficos 
#Cuando se trabaje matrices de correlación pearson se debe eliminar # ordered= paste0("E",1:24)y cambiar el estimador por ML

inv.sex.metric.wu<- measEq.syntax(configural.model = My_model,estimator="WLSMV", 
                                  ID.fac = "std.lv", parameterization = "theta",  
                                  group = "Etario", orthogonal=TRUE, data=da, ordered=T,
                                  parameterization = "delta",
                                  ID.cat = "Wu.Estabrook.2016",return.fit=TRUE, 
                                  group.equal = c("thresholds","loadings"),
                                  long.equal  = c("thresholds","loadings"))

# Evaluación del modelo
fitmeasures(inv.sex.metric.wu, fit.subset)


summary(inv.sex.metric.wu, fit.measures=TRUE)

inv.sex.scalar.wu<- measEq.syntax(configural.model = My_model,estimator="WLSMV", 
                                  ID.fac = "std.lv", parameterization = "theta", 
                                  group = "Etario", orthogonal=TRUE, data=da, ordered=T,
                                  parameterization = "theta",
                                  ID.cat = "Wu.Estabrook.2016",return.fit=TRUE, 
                                  group.equal = c("thresholds","loadings","intercepts"),
                                  long.equal  = c("thresholds","loadings","intercepts"))

fitmeasures(inv.sex.scalar.wu, fit.subset)

summary(inv.sex.scalar.wu, fit.measures=TRUE)

inv.sex.stric.wu<- measEq.syntax(configural.model = My_model,estimator="WLSMV",
                                 ID.fac = "std.lv", parameterization = "theta",  
                                 group = "Etario", orthogonal=TRUE, data=da, ordered=T,
                                 parameterization = "theta", 
                                 ID.cat = "Wu.Estabrook.2016",return.fit=TRUE, 
                                 group.equal = c("thresholds","loadings","intercepts","residuals"),
                                 long.equal  = c("thresholds","loadings","intercepts","residuals"))

fitmeasures(inv.sex.stric.wu, fit.subset)

summary(inv.sex.stric.wu, fit.measures=TRUE)

inv.sex.struc.wu<- measEq.syntax(configural.model = My_model,estimator="WLSMV",
                                 ID.fac = "std.lv", parameterization = "theta",  
                                 group = "Etario", orthogonal=TRUE, data=da, ordered=T,
                                 parameterization = "theta", 
                                 ID.cat = "Wu.Estabrook.2016",return.fit=TRUE, 
                                 group.equal = c("thresholds","loadings","intercepts","residuals","lv.variances","lv.covariances"),
                                 long.equal  = c("thresholds","loadings","intercepts","residuals","lv.variances","lv.covariances"))

#datos mejor organizados
fIT<-lavaan::anova(inv.sex.struc.wu,inv.sex.stric.wu,inv.sex.scalar.wu,inv.sex.metric.wu, inv.sex.conf.wu)
fIT

compareFit(inv.sex.struc.wu,inv.sex.stric.wu,inv.sex.scalar.wu,inv.sex.metric.wu, inv.sex.conf.wu)
#organizamos los indices de ajuste (WLSMV, MLM, MLR)
fit.stats <- rbind(fitmeasures(inv.sex.conf.wu, fit.measures = c("chisq.scaled", "df.scaled","pvalue.scaled", "cfi.scaled","rmsea.scaled","srmr")),
                   fitmeasures(inv.sex.metric.wu, fit.measures = c("chisq.scaled", "df.scaled","pvalue.scaled", "cfi.scaled","rmsea.scaled","srmr")),
                   fitmeasures(inv.sex.scalar.wu, fit.measures = c("chisq.scaled", "df.scaled","pvalue.scaled", "cfi.scaled","rmsea.scaled","srmr")),
                   fitmeasures(inv.sex.stric.wu, fit.measures = c("chisq.scaled", "df.scaled","pvalue.scaled", "cfi.scaled","rmsea.scaled","srmr")),
                   fitmeasures(inv.sex.struc.wu, fit.measures = c("chisq.scaled", "df.scaled","pvalue.scaled", "cfi.scaled","rmsea.scaled","srmr")))
rownames(fit.stats) <- c("Configural", "Métrica","Fuerte", "Estricta","Estructural")
colnames(fit.stats) <- c("χ²","gl","p","CFI","RMSEA","SRMR")
fit.stats



#Guardar resultados
fit.stats <- as.data.frame(fit.stats)

# Calcular diferencias entre modelos consecutivos
fit.stats$Δχ2 <- c(NA, diff(fit.stats$`χ²`))          # Diferencia en χ²
fit.stats$Δgl <- c(NA, diff(fit.stats$`gl`))          # Diferencia en grados de libertad
fit.stats$ΔCFI <- c(NA, diff(fit.stats$CFI))          # Diferencia en CFI
fit.stats$ΔRMSEA <- c(NA, diff(fit.stats$RMSEA))      # Diferencia en RMSEA
fit.stats$ΔSRMR <- c(NA, diff(fit.stats$SRMR))   

# Mostrar la tabla con las diferencias
fit.stats

Invarianza <- round(Invarianza,3)
write.xlsx(fit.stats,"inv.eta.xlsx",colNames=TRUE, rowNames=TRUE)

#Analisis de medias latentes
inv.sex.means<-cfa(My_model, data=du, group="Etario",estimator="WLSMV", ordered= T,orthogonal=T, 
                   group.equal=c("thresholds","loadings", "intercepts"),
                   group.partial= c("means"))

Cargas <- standardizedSolution(inv.sex.means)
Cargas
summary(inv.sex.means, fit.measures = TRUE, standardized=T)

#---------------------------------------------------------------------------------------------------------------------
#Redes

items <- c(
  "¿Con qué frecuencia has pensado en hacerte daño?",
  "¿Con qué frecuencia has creído que no merecías vivir?",
  "¿Con qué frecuencia te has preguntado qué pasaría si pusieses fin a tu vida?",
  "¿Con qué frecuencia has pensado en suicidarte?",
  "¿Con qué frecuencia has deseado no existir?")

# Observed variables:
obsvars <- c("Ítem 1", "Ítem 2", "Ítem 3", "Ítem 4", "Ítem 5")

summary(de) # computer mínimo, máximo, rango, media, etc. de los datos “bfi”
dim(de) #número de variables y casos de la base “bfi”
names(de) #Nombres de las variables en la base “bfi”
describe(de) #Estadísticos descriptivos de la base “bfi”

# computar la correlación entre las variables de la base, 5 ítems de naturaleza ordinal
corMat <- cor_auto(de) 
Groups <-c(rep("Ideación Suicida",5)) # generar grupos de ítems que se corresponden con las dimensiones,
#cada dimensión contiene 5 ítems

#Grafico en gris
Graph_lasso <- qgraph(corMat, graph = "glasso", layout = "spring", threshold=T,
                      tuning = 0.25, sampleSize = nrow(de), theme = "gray", groups = Groups)

# estimar la red con 5 ítem y  dimensiones con el método GLASSO, colores grises

# estimar la red con 5 ítem y 1 dimensión con el método GLASSO, Figura 5 del trabajo en color.
Graph_lasso <- qgraph(corMat, graph = "glasso", layout = "spring", threshold=T,
                      tuning = 0.25, sampleSize = nrow(de), groups = Groups, palette = "colorblind",
                      threshold = TRUE)

#Estimar los índices de centralidad, Figura 6 del trabajo
centralityPlot(Graph_lasso, include = c("Strength", "Closeness", "Betweenness", "ExpectedInfluence"))
Centralidad <- centralityTable(Graph_lasso)

# Guardar la tabla en un archivo Excel
write.xlsx(Centralidad, "Tabla_Centralidad.xlsx")

# 1. Cargar matriz de correlaciones o datos
# Suponiendo que tienes una matriz de correlaciones en un archivo CSV:
cor_matrix <- as.matrix(read.csv("tu_matriz_de_correlaciones.csv", row.names = 1))

# 2. Construir la red psicométrica
graph <- qgraph(corMat, graph = "cor", layout = "spring", threshold = 0.2)

# Convertir a objeto igraph
g <- as.igraph(graph)

# 3. Calcular coeficiente de agrupamiento (C)
C <- transitivity(g, type = "average")

# 4. Calcular longitud promedio de los caminos más cortos (L)
L <- mean_distance(g, directed = FALSE)

# 5. Generar una red aleatoria equivalente
random_graph <- erdos.renyi.game(vcount(g), p = edge_density(g), type = "gnp")

# Calcular coeficiente de agrupamiento en la red aleatoria
C_random <- transitivity(random_graph, type = "average")

# Calcular longitud promedio de los caminos más cortos en la red aleatoria
L_random <- mean_distance(random_graph, directed = FALSE)

# 6. Calcular índice de small-worldness
S <- (C / C_random) / (L / L_random)

# 7. Interpretación
if (S > 1) {
  cat("La red presenta propiedades de mundo pequeño (S =", S, ")\n")
} else {
  cat("La red no presenta estructura de mundo pequeño (S =", S, ")\n")
}
#---------------------------------------------------------------------------------------------------------------------
# Estimación de la red
red_estimada <- estimateNetwork(de, default = "EBICglasso",
                                tuning = 0.5, threshold = TRUE,
                                labels = obsvars, corMethod = "spearman")


# Visualización de la red
print(red_estimada)


plot(red_estimada, layout = "spring", esize=0.5,groups = Groups, palette = "colorblind",
       labels = obsvars, edge.labels=TRUE, legend.cex=0.50)


#Analizar colinealidad
library("networktools") #Analiaza colinealidad
gb <- goldbricker(de, p = 0.5, method = "hittner2003", threshold = 0.25, corMin= 0.5, progressbar= TRUE)

#Colores de los nodos
colores_nodos <- "orange"  # Todos los nodos en naranja
# Estimated network:

plot(red_estimada, 
     palette = "colorblind",
     color = colores_nodos,
     layout = "spring",
     labels = obsvars, 
     legend.mode = 'style2',
     label.cex = 1, 
     label.prop = 1, 
     legend.cex = 0.4,
     edge.label.cex= 1.5,
     vsize = 6,
     edge.labels = TRUE,
     label.color = 'black',
     font = 2, esize=8)

centralityPlot(red_estimada, labels = obsvars, 
               include = c("Strength", "Closeness", 
                           "Betweenness", "ExpectedInfluence"))

Centralidad <- centralityTable(labels = obsvars, red_estimada)
Centralidad
clusteringPlot(red_estimada)
clusteringTable(red_estimada)

# Guardar la tabla en un archivo Excel
write.xlsx(Centralidad, "Tabla_Centralidad.xlsx")

# Análisis de bootstrapping no paramétrico
resultados_boot <- bootnet(red_estimada, nBoots = 1000, nCores = 8) # Ajusta nCores según tu sistema

# Plot bootstrapped edge CIs:
plot(resultados_boot, labels = T, order = "sample")

# Realizar el bootstrap para intervalos de confianza
bootstrap_results <- bootnet(red_estimada, nBoots = 1000, type = "nonparametric")

# Visualizar los resultados
plot(resultados_boot, "edge")


# Ejecutar bootstrapping con eliminación de nodos
# Realizar el bootstrapping por nodos
### Case-drop bootstrap ###
# Bootstrap 1000 values, using 8 cores:
Results2 <- bootnet(red_estimada, nBoots = 1000, nCores = 8, 
                    type = "case", 
                    statistics = c("Strength", "Closeness", 
                                   "Betweenness", "ExpectedInfluence"))


# Plot centrality stability:
plot(Results2, statistics = c("Strength", "Closeness", 
                              "Betweenness", "ExpectedInfluence"))

# Compute CS-coefficients:
corStability(Results2)

Results2 <- bootnet(red_estimada, nBoots = 1000, nCores = 8, 
                    type = "case")
# Plot edge:
plot(Results2, statistics = c("edge"))

names(da)
#---------------------------------------------------------------------------------------------------------------------
#Invarianza de redes
#Se extrae las variables 
di<-dplyr::select (da,S1, S2, S3, S4, S5, Género)
du<-dplyr::select (da,S1, S2, S3, S4, S5, Etario)


#Prueba de comparación de redes EN FUNCION DEL SEXO
Hombres  <- subset(di, Género == "1")
Hombres <- Hombres[, !colnames(Hombres) %in% "Género"]

Mujer <- subset(di, Género == "2")
Mujer <- Mujer[, !colnames(Mujer) %in% "Género"]


library(qgraph)


library(NetworkComparisonTest)
#Prueba de invarianza
results <- NCT(
  data1 = Hombres,
  data2 = Mujer,
  test.edges = TRUE,         # Prueba de invarianza en las conexiones específicas
  test.centrality = TRUE,    # Prueba de invarianza en los índices de centralidad
  paired = FALSE,            # Si los datos son independientes
  it = 1000                  # Número de permutaciones
)

#Resultados
# Resultados de la invarianza global
print(results)


library(gridExtra)
network_male <- estimateNetwork(Hombres, default = "EBICglasso", corMethod = "cor_auto",
                                tuning = 0.25, threshold = TRUE, labels = obsvars)

network_female <- estimateNetwork(Mujer, default = "EBICglasso", corMethod = "cor_auto",
                                  tuning = 0.25, threshold = TRUE, labels = obsvars)



#Creating hyperparameter *max_value*
max_value <- max(
  max(abs(network_male$graph)), # from network 1
  max(abs(network_female$graph)) # or from network 2?
)

#' Creating hyperparameter *net_layout*
net_layout <- averageLayout(network_male,
                            network_female,
                            layout = "spring")

#Colores de los nodos
colores_nodos <- "orange"  # Todos los nodos en naranja
par(mfrow = c(2, 2))
plot(network_male, title = "Hombres",
     layout = net_layout, #' *fixed layout*
     maximum = max_value, #' *fixed edge width*
     palette = "colorblind", color = colores_nodos,
     label.cex = 1, # scalar on label size
     label.color = 'black', # string on label colors
     label.prop = 0.9, # proportion of the width of the node that the label scales
     edge.labels=TRUE,curveAll=2,edge.label.cex=1.5,
     # Edges (pp. 33-34)
     negDashed = T, # should negative edges be dashed?
     font = 2, 
     # Legend (p. 35-36)
     legend.cex = 0.27, # scalar of the legend
     legend.mode = 'style2', # default is 'style1'
     # names for each node to plot in legend
     
     # Generical graphical arguments (p. 36)
     font = 2)

plot(network_female, title = "Mujeres",
     layout = net_layout, #' *fixed layout*
     maximum = max_value, #' *fixed edge width*
     palette = "colorblind", color = colores_nodos,
     label.cex = 1, # scalar on label size
     label.color = 'black', # string on label colors
     label.prop = 0.9, # proportion of the width of the node that the label scales
     edge.labels=TRUE,curveAll=2,edge.label.cex=1.5,
     # Edges (pp. 33-34)
     negDashed = T, # should negative edges be dashed?
     
     # Legend (p. 35-36)
     legend.cex = 0.27, # scalar of the legend
     legend.mode = 'style2', # default is 'style1'
     # names for each node to plot in legend
     
     # Generical graphical arguments (p. 36)
     font = 2)

#Comprobación de la estructura invariante de la red
#Al hacer esto, lo que queremos básicamente es saber si hay grandes diferencias entre los bordes de ambas redes.
#Para conocer nuestro valor de diferencia más alto, la estadística M , simplemente ejecutamos:

M <-
  max(
    abs(c(network_male$graph) - c(network_female$graph))
  )

cat("The biggest edge difference is:", M)

#Comprobación de la fuerza global invariante 
#Otro aspecto importante es la fuerza general de la red. 
#Digamos que una red está más activa que la otra. 
#También podemos comprobarlo. Esta estadística se llama S . 
#Vamos a comprobar la diferencia de fuerza entre una red y la otra.

S <-
  abs(
    sum(
      abs(c(network_male$graph)) -
        abs(c(network_female$graph))
    )
  )/2

cat("Strength difference between the two networks is:", S)

#Ejecución y generación de informes de NetworkComparisonTest 
#Para ejecutar estas pruebas, la función es de hecho muy sencilla.

set.seed(123) # random permutation seed
nct_results <- NCT(network_male, network_female,
                   it = 1000,
                   progressbar = T)

nct_results


#Comprobación de la invariancia de las medidas de centralidad
# También podríamos investigar posibles diferencias de centralidad.
# Para ello, cambiemos el argumento test.centralitya TRUE.

nct_test_centrality <- NCT(network_male, network_female,
                           it = 1000, test.centrality = T,
                           p.adjust.methods = "BH",
                           centrality = c("closeness",
                                          "betweenness",
                                          "strength",
                                          "expectedInfluence"),
                           progressbar = T)

central.dif<- nct_test_centrality$diffcen.pval

# Guardar la tabla en un archivo Excel
write.xlsx(central.dif, "central.dif.sex.xlsx")

#Prueba de comparación de redes EN FUNCION DEL GRUPO ETARIO

Adulto_joven  <- subset(du, Etario == "1")
Adulto_joven <- Adulto_joven[, !colnames(Adulto_joven) %in% "Etario"]

Adulto_medio <- subset(du, Etario == "2")
Adulto_medio <- Adulto_medio[, !colnames(Adulto_medio) %in% "Etario"]


library(qgraph)


library(NetworkComparisonTest)
#Prueba de invarianza
results <- NCT(
  data1 = Adulto_joven,
  data2 = Adulto_medio,
  test.edges = TRUE,         # Prueba de invarianza en las conexiones específicas
  test.centrality = TRUE,    # Prueba de invarianza en los índices de centralidad
  paired = FALSE,            # Si los datos son independientes
  it = 1000                  # Número de permutaciones
)

#Resultados
# Resultados de la invarianza global
print(results)


library(gridExtra)
network_joven <- estimateNetwork(Adulto_joven, default = "EBICglasso", corMethod = "spearman",
                                 tuning = 0.25, threshold = TRUE, labels = obsvars)

network_medio <- estimateNetwork(Adulto_medio, default = "EBICglasso", corMethod = "spearman",
                                 tuning = 0.25, threshold = TRUE, labels = obsvars)


#' Creating hyperparameter *max_value*
max_value <- max(
  max(abs(network_joven$graph)), # from network 1
  max(abs(network_medio$graph)) # or from network 2?
)

#' Creating hyperparameter *net_layout*
net_layout <- averageLayout(network_joven,
                            network_medio, 
                            layout = "spring")
#Colores de los nodos
colores_nodos <- "orange"  # Todos los nodos en naranja

par(mfrow = c(1, 2))
plot(network_joven, title = "Adulto joven",
     layout = net_layout, #' *fixed layout*
     maximum = max_value, #' *fixed edge width*
     palette = "colorblind", color = colores_nodos,
     label.cex = 1, # scalar on label size 
     label.color = 'black', # string on label colors
     label.prop = 0.9, # proportion of the width of the node that the label scales
     edge.labels=TRUE, curveAll=2, edge.label.cex=1.5,
     # Edges (pp. 33-34)
     negDashed = T, # should negative edges be dashed?
     
     # Legend (p. 35-36)
     legend.cex = 0.27, # scalar of the legend
     legend.mode = 'style2', # default is 'style1'
     # names for each node to plot in legend
     
     # Generical graphical arguments (p. 36)
     font = 2)

plot(network_medio, title = "Adulto medio",
     layout = net_layout, #' *fixed layout*
     maximum = max_value, #' *fixed edge width*
     palette = "colorblind", color = colores_nodos,
     label.cex = 1, # scalar on label size
     label.color = 'black', # string on label colors
     label.prop = 0.9, # proportion of the width of the node that the label scales
     edge.labels=TRUE, curveAll=2,edge.label.cex=1.5,
     # Edges (pp. 33-34)
     negDashed = T, # should negative edges be dashed?
     
     # Legend (p. 35-36)
     legend.cex = 0.27, # scalar of the legend
     legend.mode = 'style2', # default is 'style1'
     # names for each node to plot in legend
     
     # Generical graphical arguments (p. 36)
     font = 2)

#Comprobación de la estructura invariante de la red
#Al hacer esto, lo que queremos básicamente es saber si hay grandes diferencias entre los bordes de ambas redes.
#Para conocer nuestro valor de diferencia más alto, la estadística M , simplemente ejecutamos:

M <-
  max(
    abs(c(network_joven$graph) - c(network_medio$graph))
  )

cat("The biggest edge difference is:", M)

#Comprobación de la fuerza global invariante 
#Otro aspecto importante es la fuerza general de la red. 
#Digamos que una red está más activa que la otra. 
#También podemos comprobarlo. Esta estadística se llama S . 
#Vamos a comprobar la diferencia de fuerza entre una red y la otra.

S <-
  abs(
    sum(
      abs(c(network_joven$graph)) -
        abs(c(network_medio$graph))
    )
  )/2

cat("Strength difference between the two networks is:", S)

#Ejecución y generación de informes de NetworkComparisonTest 
#Para ejecutar estas pruebas, la función es de hecho muy sencilla.

set.seed(123) # random permutation seed
nct_results <- NCT(network_joven, network_medio,
                   it = 1000,
                   progressbar = F)

nct_results

#Comprobación de la resistencia invariante de los bordes 
#Si nuestra M fuera significativa, podríamos solicitar análisis post hoc . 
#Van Borkulo et al. (2017) recomiendan verificar posibles diferencias en los bordes 
#utilizando la corrección de Bonferroni-Holm. Podríamos solicitarlo directamente, 
#pero tendríamos que realizar otro análisis NCT que ahora solicite directamente 
#que se prueben diferencias en los bordes específicos.

nct_test_edges <- NCT(network_joven, network_medio, 
                      it = 1000, test.edges = T,
                      p.adjust.methods = "BH",
                      progressbar = F)

nct_test_edges

#Comprobando qué bordes son diferentes
#Cuando probamos diferencias de borde específicas, terminamos con un objeto largo de todas las pruebas posibles. 
#Creé una función para organizar solo las diferencias de borde con un p igual o inferior a 0,05 
#(esto se puede cambiar usando el argumento alpha :

difference_value <- function(NCT, alpha = 0.05){
  
  diff_edges <- NCT$einv.pvals %>% dplyr::filter(`p-value` <= alpha)
  
  for (i in 1:nrow(diff_edges)) {
    var_1 <- as.character(diff_edges[i, 1])
    var_2 <- as.character(diff_edges[i, 2])
    
    value_net_1 <- NCT$nw1[var_1, var_2]
    value_net_2 <- NCT$nw2[var_1, var_2]
    
    abs_difference <- abs(value_net_1 - value_net_2)
    p_value <- diff_edges$`p-value`[i]
    
    cat("Test Edge", i, "\n----\n")
    cat(var_1, "and", var_2)
    cat("\nNetwork 1:", value_net_1,
        "\nNetwork 2:", value_net_2)
    cat("\nAbsolute difference:", abs_difference,
        "with p-value =", p_value, "\n----\n")
  }
}

difference_value(nct_test_edges)

#Comprobación de la invariancia de las medidas de centralidad
#También podríamos investigar posibles diferencias de centralidad. Para ello, cambiemos el argumento test.centralitya TRUE.

nct_test_centrality <- NCT(network_joven, network_medio,
                           it = 1000, test.centrality = T,
                           p.adjust.methods = "BH",
                           centrality = c("closeness",
                                          "betweenness",
                                          "strength",
                                          "expectedInfluence"),
                           progressbar = F)


central.dif<- nct_test_centrality$diffcen.pval

# Guardar la tabla en un archivo Excel
write.xlsx(central.dif, "central.dif.eta.xlsx")

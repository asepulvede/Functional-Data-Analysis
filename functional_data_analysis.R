library(readxl)
library(dplyr)
library(fda)
library(fda.usc)
library(funHDDC)
library(GGally)


data <- read_excel("C:/Users/Abelino/OneDrive/Escritorio/Univerisad/Sexto semestre/Econometría 1/datos_infection_rates.xlsx")
zx <- data.matrix(data, rownames.force = NA)
newdata <- matrix(nrow = length(zx[,1]), ncol = length(zx[1,]))

newdata[,1] <- zx[,1]
for(i in 1:length(zx[,1])){
  for(j in 2:length(zx[1,])){
    newdata[i,j] <- zx[i,j] -zx[i,j-1]
  }
}


ass <- t(newdata)

## Análisis funcional de los datos
basisj <- create.bspline.basis(c(0,228),45)
fdobjs <- smooth.basis(seq(0, 227), ass, basisj)$fd
plot(fdobjs,title="Tasa de Incidencia por cada 10.000 hab", xlab= "Days: from 30/03/2020 to 27/05/2022", ylab= "confirmed cases", col= 1)


## Test de permutaciones
#Clustering para separar los dos grupos
derivadafd <- deriv.fd(fdobjs) #derivo las funciones porque quiero ver las tasas
res.uni <- funHDDC(fdobjs,K=2,model="AkBkQkDk",init="kmeans",threshold=0.3)
plot(derivadafd,col=res.uni$class,xlab="Days: from 30/03/2020 to 27/05/2022", ylab= "infection rate")

#grupo 1 y 2
select1 <- fd(derivadafd$coefs[,which(res.uni$class==1)], derivadafd$basis)
select2 <- fd(derivadafd$coefs[,which(res.uni$class==2)], derivadafd$basis)
plot(select1)

#Test de permutaciones
tres <- tperm.fd(select1,select2)


##Análisis funcional comunal
cerrillos <- newdata[1,]
cerro_navia <- newdata[2,]
conchali <- newdata[3,]
el_bosque <- newdata[4,]
estacion_central <- newdata[5,]
huechuraba <- newdata[6,]
independencia <- newdata[7,]
la_cisterna <- newdata[8,]
la_florida <- newdata[9,]
la_pintana <- newdata[10,]
la_granja <- newdata[11,]
la_reina <- newdata[12,]
las_condes <- newdata[13,]
lo_barneceha <- newdata[14,]
lo_espejo <- newdata[15,]
lo_prado <- newdata[16,]
maicul <- newdata[17,]
maipu <- newdata[18,]
nonoa <- newdata[19,]
pedro_aguirre <- newdata[20,]
penalolen <- newdata[21,]
providencia <- newdata[22,]
pudahuel <- newdata[23,]
quilicura <- newdata[24,]
quinta_normal <- newdata[25,]
recoleta <- newdata[26,]
renca <- newdata[27,]
san_miguel <- newdata[28,]
san_joaquin <- newdata[29,]
san_ramon <- newdata[30,]
santiago <- newdata[31,]
vitacura <- newdata[32,]

x <- seq(0,227)
basisobj <- create.bspline.basis(c(0,228),45)
x1_t <- smooth.basis(x,cerrillos,basisobj)$fd
x2_t <- smooth.basis(x,cerro_navia,basisobj)$fd
x3_t <- smooth.basis(x,conchali,basisobj)$fd
x4_t <- smooth.basis(x,el_bosque,basisobj)$fd
x5_t <- smooth.basis(x,estacion_central,basisobj)$fd
x6_t <- smooth.basis(x,huechuraba,basisobj)$fd
x7_t <- smooth.basis(x,independencia,basisobj)$fd
x8_t <- smooth.basis(x,la_cisterna,basisobj)$fd
x9_t <- smooth.basis(x,la_florida,basisobj)$fd
x10_t <- smooth.basis(x,la_pintana,basisobj)$fd
x11_t <- smooth.basis(x,la_granja,basisobj)$fd
x12_t <- smooth.basis(x,la_reina,basisobj)$fd
x13_t <- smooth.basis(x,las_condes,basisobj)$fd
x14_t <- smooth.basis(x,lo_barneceha,basisobj)$fd
x15_t <- smooth.basis(x,lo_espejo,basisobj)$fd
x16_t <- smooth.basis(x,lo_prado,basisobj)$fd
x17_t <- smooth.basis(x,maicul,basisobj)$fd
x18_t <- smooth.basis(x,maipu,basisobj)$fd
x19_t <- smooth.basis(x,nonoa,basisobj)$fd
x20_t <- smooth.basis(x,pedro_aguirre,basisobj)$fd
x21_t <- smooth.basis(x,penalolen,basisobj)$fd
x22_t <- smooth.basis(x,providencia,basisobj)$fd
x23_t <- smooth.basis(x,pudahuel,basisobj)$fd
x24_t <- smooth.basis(x,quilicura,basisobj)$fd
x25_t <- smooth.basis(x,quinta_normal,basisobj)$fd
x26_t <- smooth.basis(x,recoleta,basisobj)$fd
x27_t <- smooth.basis(x,renca,basisobj)$fd
x28_t <- smooth.basis(x,san_miguel,basisobj)$fd
x29_t <- smooth.basis(x,san_joaquin,basisobj)$fd
x30_t <- smooth.basis(x,san_ramon,basisobj)$fd
x31_t <- smooth.basis(x,santiago,basisobj)$fd
x32_t <- smooth.basis(x,vitacura,basisobj)$fd

plot(x28_t, main="title",xlab= "Days: from 30/03/2020 to 27/05/2022", ylab= "Incidence rate", col= 4)
lines(x3_t, lwd = 1, col = "khaki1")
lines(x14_t, lwd = 1, col = "sienna")
lines(x2_t, lwd = 1, col = "orangered")
lines(x4_t, lwd = 1, col = "green2")
lines(x1_t, lwd = 1, col = "hotpink")
lines(x7_t, lwd = 1, col = "red4")
lines(x8_t, lwd = 1, col = "seagreen2")
lines(x5_t, lwd = 1, col = "lightgoldenrod1")
lines(x10_t, lwd = 1, col = "#9ACD32")
lines(x11_t, lwd = 1, col = "#698B22")
lines(x12_t, lwd = 1, col = "orchid")
lines(x13_t, lwd = 1, col = "deeppink3")
lines(x4_t, lwd = 1, col = "deeppink4")
lines(x15_t, lwd = 1, col = "gold")
lines(x16_t, lwd = 1, col = "navajowhite")
lines(x18_t, lwd = 1, col = "red")
lines(x17_t, lwd = 1, col = "paleturquoise2")
lines(x9_t, lwd = 1, col = "turquoise2")
lines(x19_t, lwd = 1, col = "maroon1")
lines(x20_t, lwd = 1, col = "goldenrod2")
lines(x6_t, lwd = 1, col = "blue3")
lines(x22_t, lwd = 1, col = "hotpink")
lines(x23_t, lwd = 1, col = "orange")
lines(x24_t, lwd = 1, col = "orange2")
lines(x25_t, lwd = 1, col = "pink")
lines(x26_t, lwd = 1, col = "red3")
lines(x27_t, lwd = 1, col = "sienna1")
lines(x21_t, lwd = 1, col = "darkolivegreen1")
lines(x29_t, lwd = 1, col = "#43CD80")
lines(x30_t, lwd = 1, col = "#BCEE68")
lines(x32_t, lwd = 1, col = "#8B2252")


derivada_eval <- eval.fd(x,derivadafd)

##Test diferencia de medias
sum_tiempos <- function(derivada_eval){
  lista <- NULL
  for(i in 1:length(derivada_eval[,1])){
    s=0
    lista[i] <- sum(derivada_eval[i,])
  }
  return(lista)
}

permutations <- function(derivada_eval){
  for(i in 1:length(derivada_eval[1,])){
    a <- runif(1,0,1)
    if(a<0.5){
      derivada_eval[,i] <- -derivada_eval[,i]
    }
  }
  return(derivada_eval)
}

perm_test <- function(n,derivada_eval){
  matrizPerm <- matrix(nrow = n, ncol = length(derivada_eval[,1]))
  for(i in 1:n){
    perm <- permutations(derivada_eval)
    vect <- sum_tiempos(perm)
    matrizPerm[i,] <- vect
  }
  return(t(matrizPerm))
}


#Se calcula el p-valor
n=100000
prueba<- perm_test(n,derivada_eval)
tobs <- sum_tiempos(derivada_eval) 
suma <- numeric(length(derivada_eval[,1]))
for(i in 1:length(derivada_eval[,1])){
  sum <-0
  for(j in 1:n){
    if(prueba[i,j]>tobs[i]){
      print(1)
      suma[i] <- suma[i]+1
    } else {
      suma[i] <- suma[i]
    }
  }
}
p_value <- suma/n
plot(p_value)
abline(h=0.05, col="blue")

##Realizamos regresión lineal múltiple
datos <- read_excel("C:/Users/Abelino/OneDrive/Escritorio/Univerisad/Sexto semestre/Econometría 1/datosRegresion.xlsx")
attach(datos)

#revisamos autocorrelación entre los predictores y la var. dependente
ggpairs(datos, lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")

#modelos de regresion
reg<- lm(Tasa_incidencia_prom_mensual~Indice_Mov_Ext+Indice_Mov_Int+IDSE+`Ingreso_percapita_mensual(miles$)`+`Pobreza(%)`+
           `Escolaridad(Años)`+`Vivienda_aceptable(%)`+`Alcantarillado_(%)`+EVN+IDH,data=datos)
summary(reg)

step(object = reg, direction = "both", trace = 1)

reg2<-lm(formula = Tasa_incidencia_prom_mensual ~ Indice_Mov_Int + 
           IDSE + `Ingreso_percapita_mensual(miles$)` + `Pobreza(%)` + 
           `Escolaridad(Años)` + `Vivienda_aceptable(%)` + `Alcantarillado_(%)` + 
           IDH, data = datos)
summary(reg2)

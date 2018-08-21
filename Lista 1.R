####################################################################
#################### 1ª LISTA DE EXERCÍCIOS ########################
####################################################################


# Configurando o terminal(diretório) -------------------------------
setwd("D:/Desktop/Monitoria_Exp")


# Questão 1 --------------------------------------------------------
dados<-read.table("prod.txt", header = F)
dados<-as.matrix(dados); dados <- as.vector(dados)

### a) Soma
soma <- sum(dados) 

### b) Soma de quadrados
somaquad <- sum((dados)^2)

### c) Média amostral
media <- mean(dados)

### d) Soma de desvios
somadesv <- sum(dados-media)

### e) Soma de quadrados de desvios
somaquaddesv <- sum((dados-media)^2)


### contagem
n <- length(dados)

### f) Variância amostral
vari <- somaquaddesv/(n-1)
vari2 <- var(dados)
vari3 <- (1/(n-1))* (somaquad - (sum(dados)^2 /n)) # equação da lista

### g) Desvio padrão amostral
desv <- sqrt(somaquaddesv/(n-1))
desv2 <- sd(dados)

### h) Coeficiente de variação
cv <- (desv/media)*100


### i) 
### somando a constante K (k=10)
dados2 <- dados + 10; dados2


### Soma
soma <- sum(dados2) 

### Soma de quadrados
somaquad <- sum((dados2)^2)

### Média amostral
media <- mean(dados2)

### Soma de desvios
somadesv <- sum(dados2-media)

### Soma de quadrados de desvios
somaquaddesv <- sum((dados2-media)^2)


### contagem
n <- length(dados2)

### Variância amostral
vari <- somaquaddesv/(n-1)
vari2 <- var(dados2)
vari3 <- (1/(n-1))* (somaquad - (sum(dados2)^2 /n)) # equação da lista

### Desvio padrão amostral
desv <- sqrt(somaquaddesv/(n-1))
desv2 <- sd(dados2)

#### Coeficiente de variação
cv <- (desv/media)*100


### j) 
### multiplicando a constante K (k=10)
dados3 <- dados*10 ;dados3


### Soma
soma <- sum(dados3) 

### Soma de quadrados
somaquad <- sum((dados3)^2)

### Média amostral
media <- mean(dados3)

### Soma de desvios
somadesv <- sum(dados3-media)

### Soma de quadrados de desvios
somaquaddesv <- sum((dados3-media)^2)

### contagem
n <- length(dados3)

### Variância amostral
vari <- somaquaddesv/(n-1)
vari2 <- var(dados3)
vari3 <- (1/(n-1))* (somaquad - (sum(dados3)^2 /n)) # equação da lista

### Desvio padrão amostral
desv <- sqrt(somaquaddesv/(n-1))
desv2 <- sd(dados3)

### Coeficiente de variação
cv <- (desv/media)*100



# Questão 2 -------------------------------------------------------
dados <- read.table("dap.txt", h=F)
dados <- as.matrix(dados); dados <-as.vector(dados)


### a) Histograma
histog <- hist(dados, breaks = 13, 
               xlim = c(5,25), 
               xlab= "DAP", 
               ylab= "Frequência",
               col= "magenta", 
               main= ("Histograma dos dados"))

### adionando a média
abline(v = mean(dados),
       col = "royalblue",
       lwd = 2)

### adicionando a mediana
abline(v = median(dados),
       col = "red",
       lwd = 2)

### adicionando legenda
legend(x = "topright", 
       c( "Média", "Mediana"),
       col = c( "royalblue", "red"),
       lwd = c(2, 2))


### b) intervalo de confiança da média
n <-length(dados)
erro <- qnorm(0.975)*sd(dados)/sqrt(n)

mean(dados)-erro
mean(dados)+erro

IC <- cbind((mean(dados)-erro),mean(dados),(mean(dados)+erro))

### distribuição t
error <- qt(0.975,df=n-1)*sd(dados)/sqrt(n)

mean(dados)-error
mean(dados)+error

### funções prontas
t.test(dados,
       conf.level=0.95)

#install.packages("fBasics")
require(fBasics)
basicStats(dados, ci=0.95)


### c) intervalo de confinaça para variância
df <- length(dados) - 1
vari<- var(dados)
lower = vari * df / qchisq(0.05/2, df, lower.tail = FALSE)
upper = vari * df / qchisq(1 - 0.05/2, df, lower.tail = FALSE)
c(lower = lower, variance = vari, upper = upper)


ICV=c(vari*(n-1)/qchisq(0.975,n-1),
      + vari*(n-1)/qchisq(0.025,n-1))


qqnormPlot(dados)
shapiro.test(dados)


# Questão 3 ---------------------------------------------------------
dados <-read.table("apl.txt", header=TRUE)
str(dados)
### a)
# (nula)       H0: Não existe diferença entre os tratamentos
# (alernativa) H1: Há diferença entre as médias dos tratamentos

### b) 
modelo <- lm(prod ~ trat, data= dados)
anova(modelo)

###c)
media <- aggregate(x=dados$prod,by=list(dados$trat),FUN=mean)
sd <- aggregate(x=dados$prod,by=list(dados$trat),FUN=sd)
n <- aggregate(x=dados$prod,by=list(dados$trat),FUN=length)

Erro_pd <-(sd$x)/(sqrt(n$x))
Tabela <- cbind(media, Erro_pd)

### construindo gráfico

means <-Tabela$x
error <-Tabela$Erro_pd

se.sup<-means+error
se.inf<-means-error

bp <- barplot(means, beside = T, ylim=c(0,max(se.sup*1.2)), col=c("forestgreen", "palegreen"), ylab = "t.ha")
arrows(bp,se.sup,bp,se.inf, code=3,angle=90,length=0.05)

legend("top", 
       legend=c("Trat 1", "Trat 2"),
       fill=c("forestgreen", "palegreen"),
       bty="n",
       ncol=2)


###d)
vari <- aggregate(x=dados$prod,by=list(dados$trat),FUN=var)

###e) 
t1 <- dados$prod[1:8]
t2 <- dados$prod[9:16]

dados1 <- data.frame(t1,t2)

t.test(dados1$t1,dados1$t2)



# Questão 4 ---------------------------------------------------------
dados <- read.table("eucalip.txt", header = TRUE)
str(dados)

### a) soma de produtos
n <- length(dados$arvore)
somap <- sum(dados$x * dados$y) - (sum(dados$x)*sum(dados$y))/n


### b) covariância
cov <- somap / (n-1)
cov2 <- cov(dados$x, dados$y)
cov3 <- var(dados$x, dados$y)

### c) coeficiente linear de pearson
corl <- cov2/ sqrt(var(dados$x)* var(dados$y))
corl2 <- cor(dados$x, dados$y)

### d)
x <- scale(dados$x)
y <- scale(dados$y)

x1 <- (dados$x - mean(dados$x)) /sd(dados$x)
y1 <- (dados$y - mean(dados$y)) /sd(dados$y)

corp <- var(x,y)

### e)

corsp <- cor(dados$x, dados$y, method = "spearman")


### f,g,h) 
reg <- lm(x~ y, data=dados)
anova(reg)
summary(reg)

plot(dados$y ~ dados$x, 
     xlab= "Método X", 
     ylab="Método Y",
     pch = 19,
     col = "#08123C",
     family="serif")
abline(reg)

### inserindo modelo ajustado e r²
coeficientes <- reg$coefficients
texto <- sprintf('y = %.2f + %.2fx, r² = %.2f', coeficientes[1], coeficientes[2], summary(reg)$r.squared)
text(0.54, 0.65, texto, family="serif" )



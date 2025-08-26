require(glue)
set.seed(247005)
options(scipen = 999)
#CONTEXTOS
contextos = c(
  "10", #2
  "111", #3
  "1100",
  "1101", #4
  "10100",
  "10001",
  "00101",
  "10101", #5
  "100000",
  "010000",
  "001000",
  "101000",
  "000001",
  "011001",
  "100011",
  "010011",
  "001011",
  "011011", #6
  "0000000",
  "1110000",
  "1011000",
  "0111000",
  "0000100",
  "1100100",
  "1100001",
  "0001001",
  "0111001",
  "0000011",
  "0101011",
  "0111011",
  "1111011", #7
  "01000000",
  "00110000",
  "00011000",
  "10011000",
  "11111000",
  "11000100",
  "01001001",
  "11001001",
  "00101001",
  "10101001",
  "01101001",
  "01111001",
  "01000011",
  "11000011",
  "10110011",
  "11110011", #8
  "011000000",
  "010110000",
  "110110000",
  "101111000",
  "001000100",
  "101000100",
  "100100100",
  "010100100",
  "110100100",
  "100100001",
  "010100001",
  "110100001",
  "111101001",
  "011111001",
  "111111001",
  "100110011",
  "001110011",
  "101110011",
  "101101011",
  "011101011", #9
  "0111000000",
  "1111000000",
  "0001111000",
  "1001111000",
  "0000100100",
  "1000100100",
  "0000100001",
  "1000100001",
  "0011101001",
  "1011101001",
  "0000110011",
  "1000110011",
  "0001101011",
  "1001101011",
  "0111101011",
  "1111101011" #10
)

#checagem de propriedade de sufixo:
for(i in 1:(length(contextos)-1)){
  print(i)
  for(j in (i+1):length(contextos)){
    if(stringr::str_detect(contextos[j], glue("{contextos[i]}$"))){
      print(glue("Propriedade violada: {contextos[i]} é sufixo de {contextos[j]}."))
      break
    }
    print(glue::glue("{j} ok"))
  }
} #tudo ok

#checagem que a árvore cobre todas as combinações possíveis:
c_2_10 = apply(expand.grid(replicate(10, c("0", "1"), simplify = FALSE)), 1, paste0, collapse = "")

flag = 0
for(c in c_2_10){
  for(s in 1:(length(contextos)+1)){
    if(stringr::str_detect(c,glue("{contextos[s]}$"))) break
    if(s==83){
      flag = 1
      print(glue("Sem cobertura para {c}."))
    }
  }
  if(flag==1) break
} #tudo ok


#Criação das probabilidades
probabilidades = numeric(length(contextos))
p = 0.3
probabilidades[1:2] = c(0, 0)
for(i in 3:length(probabilidades)){
  prob = rnorm(1, mean = p, sd = 0.2)
  if(prob>=0.95) prob = 0.94 #quero evitar probabilidades muito extremas na amostra de teste.
  if(prob<=0.05) prob = 0.06
  probabilidades[i] = prob
  print(prob)
  p = 1-prob #evitar probabilidades parecidas entre contextos parecidos.
}

#Exportação de contextos e probabilidades para fácil acesso
library(tidyverse)
#df_amostra = tibble(contexto = contextos, prob = probabilidades)
#write_csv(df_amostra, 'parametros_geradores_amostra.csv')

#função de simulação
sim_cemav_bin_2 = function(contextos, probabilidades, n, amostra_inicial = c(), text = T, show_process=F){
  k = max(nchar(contextos))
  if(show_process) print(paste("ordem:", k))
  if(length(amostra_inicial)==0) amostra_inicial = paste(sample(c(0,1),2*k,TRUE), collapse='')
  
  l = nchar(amostra_inicial)
  if(show_process) print(paste("amostra inicial:", amostra_inicial))
  
  amostra = amostra_inicial
  
  for(i in (l+1):(n+l)){
    passado_relevante = substr(amostra, nchar(amostra)-k+1, nchar(amostra))
    
    c = which(apply(array(paste0(contextos,"$")), 1, function(x) grepl(x,passado_relevante)))
    
    if(length(c)==0){
      stop(glue::glue("SEM CONTEXTO PARA {passado_relevante}"))
    }
    
    else{
      x = sample(c(0,1),1,T,c(1-probabilidades[c],probabilidades[c]))
      amostra = paste0(amostra,x)
    }
    if(show_process) if(i %in% round((1:100)*(n+l)/100, digits = 0))print(glue::glue("{round((i)*100/(n+l))}%"))
  }
  amostra = substr(amostra, l+1,n+l)
  
  if(text) return(amostra)
  else return(strsplit(amostra, ''))
}

n = 10^5

amostra = sim_cemav_bin_2(contextos, probabilidades, n, show_process = T)

cat(amostra, file = "amostra_skel_100k.txt")


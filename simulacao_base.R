library(glue)
set.seed(247005)
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
    print(j)
    if(stringr::str_detect(contextos[j], glue("{contextos[i]}$"))){
      print(glue("Propriedade violada: {contextos[i]} é sufixo de {contextos[j]}."))
      break
    }
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
probabilidades[1:2] = c(1, 0)
for(i in 3:length(probabilidades)){
  prob = rnorm(1, mean = p, sd = 0.2)
  if(prob>=1) prob = 0.95
  if(prob<=0) prob = 0.05
  probabilidades[i] = prob
  print(prob)
  p = 1-prob #evitar probabilidades parecidas entre contextos parecidos. Talvez seja melhor usar sort(contextos).
}


#junção
contextos_lista = setNames(as.list(probabilidades), contextos)

#SIMULAÇÃO


ordem = 10
n = 1e7

# Contexto inicial
passado = paste(sample(c("0", "1"), ordem, replace = TRUE), collapse = "") #seq de 10 com probs 0.5.
res = character(n + ordem) #preparando o resultado, evitar mudanças de tamanho no meio deixa mais rápido.
res[1:ordem] = strsplit(passado, "")[[1]] #grava o passado inicial no resultado.

# Simulação
for (i in (ordem + 1):(n + ordem)) { #itera n passos
  for (l in min(nchar(passado), ordem):0) { #itera começando pelo maior contexto possível
    candidato = substr(passado, start = nchar(passado) - l + 1, stop = nchar(passado)) #extrai l últimos símbolos do passado
    if (!is.null(contextos_lista[[candidato]])) { #se for contexto, lista[[cand]] vai retornar as probabilidades
      probs = c(contextos_lista[[candidato]], 1-contextos_lista[[candidato]])
      break
    }
  }
  
  u = sample(c("0", "1"), size = 1, prob = probs) #gera o próximo
  res[i] = u
  
  passado = paste0(passado, u)
  if (nchar(passado) > ordem) {
    passado = substr(passado, start = 2, stop = ordem + 1) #trim para garantir que só se considera até ordem símbolos
  }
}

resultados = res[(ordem + 1):(n + ordem)]

cat(paste(resultados, collapse = ""), file = "amostra_o10_k3.txt")

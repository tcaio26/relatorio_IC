library(data.tree)
library(tidyverse)

string = '10001'
str_vector = str_split_1(string,'')

print(raiz, 'c0', 'c1')

raiz = Node$new('raiz')
raiz$c1 = raiz$c0 = 1

raiz$c1 = raiz$c1+1
raiz$AddChild('0', c0 = 0, c1 = 1)
raiz$AddChild('1', c0 = 0, c1 = 1)

raiz$c0 == 1

node = raiz$children[[2]]
node = raiz
node = node$children[[1]]
node$c1

#função para mudar a arvore
algo = function(u, z){
  z = z+1
  if(u == 0){
    if(raiz$c0==1){
      
    }
    for(i in 1:length(z)){
      
    }
  }
}

#looop
raiz = Node$new('raiz')
raiz$c1 = raiz$c0 = 1
u = as.numeric(str_vector[1])
raiz$c0 = raiz$c0 + (1-u)
raiz$c1 = raiz$c1 + u
raiz$AddChild('0', c0 = (1-u), c1 = u)
raiz$AddChild('1', c0 = (1-u), c1 = u)

for(i in 2:length(str_vector)){
  u = as.numeric(str_vector[i])
  z = as.numeric(rev(str_vector[1:(i-1)]))
  if(u==0){
    
  }
}
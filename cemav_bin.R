#' @title Cadeia estocástica de memória variável binária
#'
#' @description
#' Simula uma cadeia de memoria variavel binária com a função contexto qk
#'
#' @param n Número de iterações a serem geradas.
#' @param passado Vetor de passado da cadeia, caso deixe vazio será gerado automaticamente com p(1)=1/2.
#' @param tam_passado Tamanho do passado a ser gerado, caso forneça um passado, é irrelevante.
#' @param q0 valor entre 0 e 1, P(X=1|X_(-1)=1).
#' @param qk função f(k) in (0,1), P(X=1|X(-1)=...=X(-k)=0, X(-k-1)=1)
#' @param q_inf valor entre 0 e 1, P(X=1|X(-1)=...=0)
#'
#' @return uma lista:
#' \itemize{
#'  \item \code{serie} valores simulados
#'  \item \code{regen} tempos regenerativos
#'  \item \code{prob} probabilidade empírica de X=1
#'  \item \code{q} valores de qk
#' }
#'
#' @export
cemav_bin = function(n, passado=NA, tam_passado = 100, q0 = 1/2, qk = function(x) 1/(x+1), q_inf = 0){
  if(any(is.na(passado))) passado = sample(c(0,1), tam_passado, T)
  serie = c(passado)
  regeneracoes = c(max(which(serie==1))-length(serie))
  prob_emp = c(sum(passado)/length(passado))
  prob_q = c()
  for(i in 1:n){
    k = length(serie)-max(which(serie==1))
    probab = ifelse(k>0&k<Inf,qk(k),ifelse(k==0,q0,q_inf))
    x = sample(c(0,1),1,T,c(1-probab, probab))
    serie = append(serie, x)
    if(x==1) regeneracoes = append(regeneracoes, i)
    prob_emp = append(prob_emp, sum(serie)/length(serie))
    prob_q = append(prob_q, probab)
  }
  return(list(serie = serie, regen = regeneracoes, prob = prob_emp, q = prob_q))
}

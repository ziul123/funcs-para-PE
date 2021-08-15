# Vetor com os dados nas quantidades em freqs
# Por exemplo, dados = (2,3) e freqs = (1,2)
# fica (2,3,3)
repete <- function(dados, freqs) {
	tmp <- c()
	for (i in 1:length(dados)) {
		tmp <- append(tmp, rep(dados[i], freqs[i]))
	}
	tmp
}


# Desvio padrão populacional
dp <- function(x) {
	sqrt((length(x)-1)/length(x)) * sd(x)
}

# Soma de PG finita de n numeros com termo inicial a1 e razão q
pg_sum <- function(a1, q, n) {
	a1 * (1 - q**n)/(1 - q)
}

# Soma de PG infinita com termo inicial a1 e razão q
inf_pg_sum <- function(a1, q) {
	a1/(1-q)
}

# Variância de uma variável aletória com valores x e probabilidades prob
varc <- function(x, prob) {
	tmp <- vector("integer", length(x))
	
	for (i in 1:length(x)) {
		tmp[i] <- x[i]**2
	}

	sprintf("%f - %f = %f", sum(tmp*prob),
		sum(x*prob)**2,
		sum(tmp*prob) - sum(x*prob)**2
		)
	sum(tmp*prob) - sum(x*prob)**2
}

# Variancia de uma variável aleatória que segue distribuição de bernoulli
var_bern <- function(prob) {
	prob * (1-prob)
}

# Soma dos termos em range de uma distribuição binomial com tamanho s e probabilidade prob
sbinom <- function(range, s, prob) {
	sum(dbinom(range, size = s, p = prob))
}

# Esperança de uma variável aleatória que segue distribuição binomial
esp_binom <- function(s, prob) {
	s * prob
}

# Variancia de uma variável aleatória que segue distribuição de binomial
var_binom <- function(s, prob) {
	s * prob * (1-prob)
}

# Esperança de uma variável aleatória que segue distribuição geometrica
esp_geom <- function(prob) {
	1/prob
}

# Variancia de uma variável aleatória que segue distribuição de geometrica
var_geom <- function(prob) {
	(1-prob)/(prob**2)
} 

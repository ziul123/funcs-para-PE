############
# Módulo 1 #
############

# Vetor com os dados nas quantidades em freqs
# Por exemplo, dados = (2,3) e freqs = (1,2)
# fica (2,3,3)
repete <- function(dados, freqs){
	tmp <- c()
	for (i in 1:length(dados)) {
		tmp <- append(tmp, rep(dados[i], freqs[i]))
	}
	tmp
}


# Desvio padrão populacional
dp <- function(x){
	sqrt((length(x)-1)/length(x)) * sd(x)
}

# Soma de PG finita de n numeros com termo inicial a1 e razão q
pg_sum <- function(a1, q, n){
	a1 * (1 - q**n)/(1 - q)
}

# Soma de PG infinita com termo inicial a1 e razão q
inf_pg_sum <- function(a1, q){
	a1/(1-q)
}

# Variância de uma variável aletória com valores x e probabilidades prob
varc <- function(x, prob){
	tmp <- vector("integer", length(x))
	
	for (i in 1:length(x)) {
		tmp[i] <- x[i]**2
	}

	sum(tmp*prob) - sum(x*prob)**2
}

# Variancia de uma variável aleatória que segue distribuição de bernoulli
var_bern <- function(prob){
	prob * (1-prob)
}

# Soma dos termos em range de uma distribuição binomial com tamanho s e probabilidade prob
sbinom <- function(range, s, prob){
	sum(dbinom(range, size = s, p = prob))
}

# Esperança de uma variável aleatória que segue distribuição binomial
esp_binom <- function(s, prob){
	s * prob
}

# Variancia de uma variável aleatória que segue distribuição de binomial
var_binom <- function(s, prob){
	s * prob * (1-prob)
}

# Esperança de uma variável aleatória que segue distribuição geometrica
esp_geom <- function(prob){
	1/prob
}

# Variancia de uma variável aleatória que segue distribuição de geometrica
var_geom <- function(prob){
	(1-prob)/(prob**2)
} 



############
# Módulo 2 #
############


# Tamanho da amostra para a média
tam_m <- function(vari, err, conf, d = 2){
	a <- 1-conf
	z <- round(qnorm(1-a/2), d)
	ceiling(vari*z**2/err**2)

}

# Tamanho da amostra para a proporção
tam_p <- function(err, conf, p = 1/2, d = 2){
	a <- 1-conf
	z <- round(qnorm(1-a/2), d)
	ceiling(p*(1-p)*z**2/err**2)
}



############
# Módulo 3 #
############


ic_media_var <- function(m_a, conf, dp, n){
	a <- 1-conf
	x <- m_a
	s <- dp
	l <- x-qnorm(1-a/2)*(s/sqrt(n))
	u <- x+qnorm(1-a/2)*(s/sqrt(n))
	sprintf("[%f ; %f]", l, u)
}


ic_media_n_var <- function(m_a, conf, dp_a, n){
	a <- 1-conf
	x <- m_a
	s <- dp_a
	l <- x-qt(1-a/2, n-1)*(s/sqrt(n))
	u <- x+qt(1-a/2, n-1)*(s/sqrt(n))
	sprintf("[%f ; %f]", l, u)
}


ic_prop_ot <- function(p_a, conf, n){
	a <- 1-conf
	p <- p_a
	l <- p-qnorm(1-a/2)*(sqrt(p*(1-p))/sqrt(n))
	u <- p+qnorm(1-a/2)*(sqrt(p*(1-p))/sqrt(n))
	sprintf("[%f ; %f]", l, u)
}


ic_prop_con <- function(p_a, conf, n){
	a <- 1-conf
	p <- p_a
	l <- p-qnorm(1-a/2)*(1/(2*sqrt(n)))
	u <- p+qnorm(1-a/2)*(1/(2*sqrt(n)))
	sprintf("[%f ; %f]", l, u)
}


#caso 1 -> = vs !=
#caso 2 -> = vs > ou <= vs >
#caso 3 -> = vs < ou >= vs <
#retorna a aceitação de H0
t_hip_media_var <- function(m_a, m, var, n, sig, caso){
	c <- qnorm(sig) * sqrt(var/n)
	et <- m_a - m
	Ha <- switch(caso,
		abs(et) >= c,
		et >= c,
		et <= c
	)
	!Ha
}



#caso 1 -> = vs !=
#caso 2 -> = vs > ou <= vs >
#caso 3 -> = vs < ou >= vs <
#retorna a aceitação de H0
t_hip_media_n_var <- function(m_a, m, var_a, n, sig, caso){
	c <- qt(sig, n-1) * sqrt(var/n)
	et <- m_a - m
	Ha <- switch(caso,
		abs(et) >= c,
		et >= c,
		et <= c
	)
	!Ha
}


#caso 1 -> = vs !=
#caso 2 -> = vs > ou <= vs >
#caso 3 -> = vs < ou >= vs <
p_valor_media_var <- function(m_a, m, var, n, caso){
	et <- (m_a - m)/sqrt(var/n)
	p <- switch(caso,
		2 * (1-pnorm(abs(et))),
		1-pnorm(et),
		pnorm(et)
	)
	p
}


#caso 1 -> = vs !=
#caso 2 -> = vs > ou <= vs >
#caso 3 -> = vs < ou >= vs <
p_valor_media_n_var <- function(m_a, m, var_a, n, caso){
	et <- (m_a - m)/sqrt(var_a/n)
	p <- switch(caso,
		2 * (1-pt(abs(et), n-1)),
		1-pt(et,n-1),
		pt(et,n-1)
	)
	p
}


#caso 1 -> = vs !=
#caso 2 -> = vs > ou <= vs >
#caso 3 -> = vs < ou >= vs <
p_valor_prop <- function(prop_a, prop, n, caso){
	et <- (prop_a - prop)/sqrt((prop*(1-prop))/n)
	p <- switch(caso,
		2 * (1-pnorm(abs(et))),
		1-pnorm(et),
		pnorm(et)
	)
	p
}
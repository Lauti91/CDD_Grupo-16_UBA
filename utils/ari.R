# ==============================================================
# ari(): índice de Rand ajustado entre dos particiones
# ==============================================================

ari <- function(a, b) {
  t <- table(a, b); n <- sum(t); s <- function(x) sum(choose(x, 2))
  sa <- s(rowSums(t)); sb <- s(colSums(t)); sij <- s(t); e <- sa * sb / choose(n, 2)
  (sij - e) / ((sa + sb) / 2 - e)
}

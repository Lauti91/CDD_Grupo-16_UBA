# ==============================================================
# ordenar_clusters(): renumera los clusters (1 = menor mediana de x)
# ==============================================================

ordenar_clusters <- function(cl, x) {
  med <- tapply(x, cl, median)
  nuevo <- rank(med, ties.method = "first")
  factor(unname(nuevo[as.character(cl)]), levels = seq_along(med))
}

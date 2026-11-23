# --------------------------------------------------------------
# pca_exacta(): ACP de FactoMineR calculado siempre con la SVD exacta
# FactoMineR > 2.9 usa irlba (aproximado) si ncp < la mitad de las variables; con 124 x 7 puede no converger
# Solución: todas las dimensiones (SVD exacta) y recorte a ncp
# Se recorta ind$coord porque HCPC() usa todas sus columnas
# --------------------------------------------------------------
pca_exacta <- function(datos, ncp) {
  res <- FactoMineR::PCA(datos, scale.unit = TRUE, ncp = ncol(datos), graph = FALSE)
  dims <- seq_len(ncp)
  for (parte in c("coord", "cos2", "contrib")) res$ind[[parte]] <- res$ind[[parte]][, dims, drop = FALSE]
  for (parte in c("coord", "cor", "cos2", "contrib")) res$var[[parte]] <- res$var[[parte]][, dims, drop = FALSE]
  res$svd$U <- res$svd$U[, dims, drop = FALSE]
  res$svd$V <- res$svd$V[, dims, drop = FALSE]
  res$call$ncp <- ncp
  res
}

# --------------------------------------------------------------
# pca_exacta(): ACP de FactoMineR calculado siempre con la SVD exacta
# En las versiones recientes de FactoMineR (posteriores a la 2.9), PCA() usa un
# método aproximado (irlba, que arranca desde un vector al azar) cuando se piden
# menos dimensiones que la mitad de las variables. Con matrices chicas como la
# nuestra (124 x 7) puede no converger y dar coordenadas algo distintas según la
# computadora. Para que el resultado no dependa de la versión ni de la máquina, se
# calcula el ACP con todas las dimensiones (SVD exacta) y se conservan las ncp primeras.
# HCPC() agrupa con todas las columnas de ind$coord, por eso se recortan.
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

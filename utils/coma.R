# ==============================================================
# coma(): números con coma decimal y punto de miles
# ==============================================================

coma <- function(x, d = 2, signo = FALSE)
  formatC(x, format = "f", digits = d, decimal.mark = ",", big.mark = ".", flag = if (signo) "+" else "")

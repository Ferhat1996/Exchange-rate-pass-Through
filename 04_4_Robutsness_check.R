
#****************************___04_4_Robutsness_check__************************#


#*******************************__Models__**************************************

# CR ile IPI sıralamasını değiştir.
var_model_a_star  <- cbind(v_NR, v_CCI, v_CR, v_IPI, v_sa_core_CPI)
colnames(var_model_a_star ) <- c("v_NR", "v_CCI", "v_CR", "v_IPI", "v_sa_core_CPI")

var_model_a <- cbind(v_NR, v_CCI, v_IPI, v_CR, v_sa_core_CPI)
colnames(var_model_a) <- c("v_NR", "v_CCI", "v_IPI", "v_CR", "v_sa_core_CPI")

var_model_b_star  <- cbind(v_core_rir, v_NR, v_CR, v_CCI,  v_IPI, v_sa_core_CPI)
colnames(var_model_b_star ) <- c("v_core_rir", "v_NR", "v_CR", "v_CCI", "v_IPI", "v_sa_core_CPI")

var_model_b  <- cbind(v_core_rir, v_NR, v_CCI, v_CR, v_IPI, v_sa_core_CPI)
colnames(var_model_b ) <- c("v_core_rir", "v_NR", "v_CCI", "v_CR", "v_IPI", "v_sa_core_CPI")

var_model_c_star  <- cbind(v_M3, v_NR, v_CR, v_CCI,  v_IPI, v_sa_core_CPI)
colnames(var_model_c_star ) <- c("v_M3", "v_NR", "v_CR", "v_CCI", "v_IPI", "v_sa_core_CPI")

var_model_c <- cbind(v_M3, v_NR, v_CCI, v_CR, v_IPI, v_sa_core_CPI)
colnames(var_model_c ) <- c("v_M3", "v_NR", "v_CCI", "v_CR", "v_IPI", "v_sa_core_CPI")

var_model_d_star  <- cbind(v_sd_CR, v_NR, v_CR, v_CCI,  v_IPI, v_sa_core_CPI)
colnames(var_model_d_star ) <- c("v_sd_CR", "v_NR", "v_CR", "v_CCI", "v_IPI", "v_sa_core_CPI")

var_model_d  <- cbind(v_sd_CR, v_NR, v_CCI, v_CR, v_IPI, v_sa_core_CPI)
colnames(var_model_e ) <- c("v_sd_CR", "v_NR", "v_CCI", "v_CR", "v_IPI", "v_sa_core_CPI")





#*******************************__Cov_Matrix__**************************************

var_model_results_a <- VAR(var_model_a, p = 5, type = "const")
var_model_results_a_star <- VAR(var_model_a_star, p = 5, type = "const")

var_model_results_b <- VAR(var_model_b, p = 5, type = "const")
var_model_results_b_star <- VAR(var_model_b_star, p = 5, type = "const")

var_model_results_c <- VAR(var_model_c, p = 5, type = "const")
var_model_results_c_star <- VAR(var_model_c_star, p = 5, type = "const")

var_model_results_d <- VAR(var_model_e, p = 5, type = "const")
var_model_results_d_star <- VAR(var_model_e_star, p = 5, type = "const")

# Her bir model için İrrelevant Düzgün Yanıtı (Impulse Response) hesaplama
IR_a <- irf(var_model_results_a, impulse = "v_CR", response = "v_sa_core_CPI", n.ahead = 36, boot = TRUE, ortho = TRUE, runs = 100)
IR_a_star <- irf(var_model_results_a_star, impulse = "v_CR", response = "v_sa_core_CPI", n.ahead = 36, boot = TRUE, ortho = TRUE, runs = 100)

IR_b <- irf(var_model_results_b, impulse = "v_CR", response = "v_sa_core_CPI", n.ahead = 36, boot = TRUE, ortho = TRUE, runs = 100)
IR_b_star <- irf(var_model_results_b_star, impulse = "v_CR", response = "v_sa_core_CPI", n.ahead = 36, boot = TRUE, ortho = TRUE, runs = 100)

IR_c <- irf(var_model_results_c, impulse = "v_CR", response = "v_sa_core_CPI", n.ahead = 36, boot = TRUE, ortho = TRUE, runs = 100)
IR_c_star <- irf(var_model_results_c_star, impulse = "v_CR", response = "v_sa_core_CPI", n.ahead = 36, boot = TRUE, ortho = TRUE, runs = 100)

IR_d <- irf(var_model_results_d, impulse = "v_CR", response = "v_sa_core_CPI", n.ahead = 36, boot = TRUE, ortho = TRUE, runs = 100)
IR_d_star <- irf(var_model_results_d_star, impulse = "v_CR", response = "v_sa_core_CPI", n.ahead = 36, boot = TRUE, ortho = TRUE, runs = 100)



plot(IR_a)


# Her bir model için kümülatif etki yanıtlarını hesaplayıp yazdırma
cat("M1: ", sum(IR_a$irf$v_CR), "\n")
cat("M1*: ", sum(IR_a_star$irf$v_CR), "\n")
cat("M2: ", sum(IR_b$irf$v_CR), "\n")
cat("M2*: ", sum(IR_b_star$irf$v_CR), "\n")
cat("M3: ", sum(IR_c$irf$v_CR), "\n")
cat("M3*: ", sum(IR_c_star$irf$v_CR), "\n")
cat("M4: ", sum(IR_d$irf$v_CR), "\n")
cat("M4*: ", sum(IR_d_star$irf$v_CR), "\n")




# Grafiği çizme
plot(1:8, type = "n", ylim = c(0.03, 0.06), ylab = "Cumulative IR", xlab = "Models", main = " Ortho = TRUE - Sum of Impulse Response in 36 months for v_CR to v_sa_core_CPI", xaxt = "n", cex.main = 2)
points(1, sum(IR_a$irf$v_CR), col = "red", pch = 19, cex = 6)
points(2, sum(IR_a_star$irf$v_CR), col = "red", pch = 17, cex = 6)
points(3, sum(IR_b$irf$v_CR), col = "blue", pch = 19, cex = 6)
points(4, sum(IR_b_star$irf$v_CR), col = "blue", pch = 17, cex = 6)
points(5, sum(IR_c$irf$v_CR), col = "orange", pch = 19, cex = 6)
points(6, sum(IR_c_star$irf$v_CR), col = "orange", pch = 17, cex = 6)
points(7, sum(IR_d$irf$v_CR)*5, col = "green", pch = 19, cex = 6)
points(8, sum(IR_d_star$irf$v_CR)*5, col = "green", pch = 17, cex = 6)
axis(1, at = 1:8, labels = c("M1", "M1*", "M2", "M2*", "M3", "M3*", "M4", "M4*"), las = 2)
abline(h = seq(0, 1, by = 0.005), col = "lightgray", lty = "dotted")











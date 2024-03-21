library(ggplot2)
library(dplyr)
library(tidyverse)
library(fields)
library(maps)
library(sp)
library(mgcv)

cwd <- getwd()
load("datasets_project.RData")

model1 <- gam(TB ~ s(Timeliness, k=5, bs='cs') + s(Urbanisation, k=5, bs='cs') +
                offset(log(Population)), family=poisson(link='log'), data=TBdata)

summary(model1)
par(mfrow=c(1,2))
plot(model1)
par(mfrow=c(2,2))
gam.check(model1) # residual plots and additional summary info


model2 <- gam(TB ~ s(Timeliness, k=20, bs='cs') + s(Urbanisation, k=20, bs='cs') +
                   s(Density, k=20, bs='cs') + s(Poor_Sanitation, k=20, bs='cs') +
                offset(log(Population)), family=poisson(link='log'), data=TBdata)

summary(model2)
par(mfrow=c(2,2))
plot(model2)
par(mfrow=c(2,2))
gam.check(model2) # residual plots and additional summary info


model3 <- gam(TB ~ s(Timeliness, k=40, bs='cs') + s(Urbanisation, k=40, bs='cs') +
                s(Density, k=40, bs='cs') + s(Poor_Sanitation, k=40, bs='cs') +
                s(Poverty, k=40, bs='cs') + s(Unemployment, k=40, bs='cs') +
                offset(log(Population)), family=poisson(link='log'), data=TBdata)

summary(model3)
par(mfrow=c(3,2))
plot(model3)
par(mfrow=c(2,2))
gam.check(model3) # residual plots and additional summary info

model4 <- gam(TB ~ s(Timeliness, k=60, bs='cs') + s(Urbanisation, k=66, bs='cs') +
                s(Density, k=62, bs='cs') + s(Poor_Sanitation, k=68, bs='cs') +
                s(Poverty, k=70, bs='cs') + s(Unemployment, k=67, bs='cs') +
                s(lat, lon, k=65, bs='tp') + 
                offset(log(Population)), family=poisson(link='log'), data=TBdata)

summary(model4)
par(mfrow=c(4,2))
plot(model4)
par(mfrow=c(2,2))
gam.check(model4) # residual plots and additional summary info






library(ggplot2)
theme_set(theme_bw())
library(patchwork)

residual_plots <- function(model){
  resid <- residuals(model)
  fit <- fitted(model)
  df = data.frame(residuals = resid, fitted_values = fit)
  
  # QQ plot
  qq <- ggplot(df, aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line()
    # need to add correct x and y labels and color the straight line red
  
  # Residuals vs Fitted plots
  rvf <- ggplot(df, aes(x = fitted_values, y = residuals)) + 
    geom_point() + 
    geom_hline(yintercept = 0, linetype = "dashed") +
    xlim(1, 9)
  
  # Histograms of residuals
  hist <- ggplot(df, aes(x = residuals)) + 
    geom_histogram(bins = 30)
  
  return(list(qq, rvf, hist))
}

plots1 <- residual_plots(model1)
plots2 <- residual_plots(model2)
plots3 <- residual_plots(model3)
plots4 <- residual_plots(model4)

model_residuals <- (plots1[[1]] | plots1[[2]] | plots1[[3]]) /
                   (plots2[[1]] | plots2[[2]] | plots2[[3]]) /
                   (plots3[[1]] | plots3[[2]] | plots3[[3]]) /
                   (plots4[[1]] | plots4[[2]] | plots4[[3]])
model_residuals



plots1[[2]]









# Assuming 'model1' and 'model2' are your GAM model objects
resid1 <- residuals(model1)
fitted1 <- fitted(model1)
resid2 <- residuals(model2)
fitted2 <- fitted(model2)
resid3 <- residuals(model3)
fitted3 <- fitted(model3)
resid4 <- residuals(model4)
fitted4 <- fitted(model4)

# Data frames for plotting
df1 <- data.frame(residuals = resid1, fitted_values = fitted1)
df2 <- data.frame(residuals = resid2, fitted_values = fitted2)

# QQ plots
qq1 <- ggplot(df1, aes(sample = residuals)) + 
  stat_qq() + 
  stat_qq_line() +
  labs(title = "Model 1 QQ Plot")

qq2 <- ggplot(df2, aes(sample = residuals)) + 
  stat_qq() + 
  stat_qq_line() +
  labs(title = "Model 2 QQ Plot")

# Residuals vs Fitted plots
rvf1 <- ggplot(df1, aes(x = fitted_values, y = residuals)) + 
  geom_point() + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Model 1 Residuals vs Fitted")

rvf2 <- ggplot(df2, aes(x = fitted_values, y = residuals)) + 
  geom_point() + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Model 2 Residuals vs Fitted")

# Histograms of residuals
hist1 <- ggplot(df1, aes(x = residuals)) + 
  geom_histogram(bins = 30) +
  labs(title = "Model 1 Residuals Histogram")

hist2 <- ggplot(df2, aes(x = residuals)) + 
  geom_histogram(bins = 30) +
  labs(title = "Model 2 Residuals Histogram")

# Combine plots
combined_plot <- (qq1 | rvf1 | hist1) / (qq2 | rvf2 | hist2)

# Display the combined plot
combined_plot




library(grid)

# Assuming the composite figure is plotted
grid.text("Column 1 Title", x = 0.25, y = 0.96, gp = gpar(fontsize = 14))
grid.text("Column 2 Title", x = 0.75, y = 0.96, gp = gpar(fontsize = 14))

grid.text("Row 1 Label", x = 0.02, y = 0.75, rot = 90, gp = gpar(fontsize = 14))
grid.text("Row 2 Label", x = 0.02, y = 0.25, rot = 90, gp = gpar(fontsize = 14))




my_gam_check <- function (b, old.style = FALSE, type = c("deviance", "pearson", 
                                                         "response"), k.sample = 5000, k.rep = 200, rep = 0, level = 0.9, 
                          rl.col = 2, rep.col = "gray80", ...) {
  type <- match.arg(type)
  resid <- residuals(b, type = type)
  linpred <- if (is.matrix(b$linear.predictors) && !is.matrix(resid)) 
    napredict(b$na.action, b$linear.predictors[, 1])
  else napredict(b$na.action, b$linear.predictors)
  if (is.null(.Platform$GUI) || .Platform$GUI != "RStudio") 
    old.par <- par(mfrow = c(1, 3))
  if (old.style) 
    qqnorm(resid, ...)
  else qq.gam(b, rep = rep, level = level, type = type, rl.col = rl.col, 
              rep.col = rep.col, ...)
  plot(linpred, resid, xlab = "linear predictor", #main = "Resids vs. linear pred.",
       ylab = "residuals", ...)
  hist(resid, xlab = "Residuals", main = " ")#, 
       #...)
  fv <- if (inherits(b$family, "extended.family")) 
    predict(b, type = "response")
  else fitted(b)
  if (is.matrix(fv) && !is.matrix(b$y)) 
    fv <- fv[, 1]
  #plot(fv, napredict(b$na.action, b$y), xlab = "Fitted Values", 
       #ylab = "Response", main = "Response vs. Fitted Values", 
       #...)
  gamm <- !(b$method %in% c("GCV", "GACV", "UBRE", "REML", 
                            "ML", "P-ML", "P-REML", "fREML", "NCV"))
  if (gamm) {
    cat("\n'gamm' based fit - care required with interpretation.")
    cat("\nChecks based on working residuals may be misleading.")
  }
  else {
    cat("\nMethod:", b$method, "  Optimizer:", b$optimizer)
    if (!is.null(b$outer.info)) {
      if (b$optimizer[2] %in% c("newton", "bfgs")) {
        boi <- b$outer.info
        cat("\n", boi$conv, " after ", boi$iter, " iteration", 
            sep = "")
        if (boi$iter == 1) 
          cat(".")
        else cat("s.")
        cat("\nGradient range [", min(boi$grad), ",", 
            max(boi$grad), "]", sep = "")
        cat("\n(score ", b$gcv.ubre, " & scale ", b$sig2, 
            ").", sep = "")
        ev <- eigen(boi$hess)$values
        if (min(ev) > 0) 
          cat("\nHessian positive definite, ")
        else cat("\n")
        cat("eigenvalue range [", min(ev), ",", max(ev), 
            "].\n", sep = "")
      }
      else {
        cat("\n")
        print(b$outer.info)
      }
    }
    else {
      if (length(b$sp) == 0) 
        cat("\nModel required no smoothing parameter selection")
      else {
        cat("\nSmoothing parameter selection converged after", 
            b$mgcv.conv$iter, "iteration")
        if (b$mgcv.conv$iter > 1) 
          cat("s")
        if (!b$mgcv.conv$fully.converged) 
          cat(" by steepest\ndescent step failure.\n")
        else cat(".\n")
        cat("The RMS", b$method, "score gradient at convergence was", 
            b$mgcv.conv$rms.grad, ".\n")
        if (b$mgcv.conv$hess.pos.def) 
          cat("The Hessian was positive definite.\n")
        else cat("The Hessian was not positive definite.\n")
      }
    }
    if (!is.null(b$rank)) {
      cat("Model rank = ", b$rank, "/", length(b$coefficients), 
          "\n")
    }
  }
  cat("\n")
  kchck <- k.check(b, subsample = k.sample, n.rep = k.rep)
  if (!is.null(kchck)) {
    cat("Basis dimension (k) checking results. Low p-value (k-index<1) may\n")
    cat("indicate that k is too low, especially if edf is close to k'.\n\n")
    printCoefmat(kchck, digits = 3)
  }
  if (is.null(.Platform$GUI) || .Platform$GUI != "RStudio") 
    par(old.par)
}



par(mfrow = c(4, 3), mar = c(4, 4, 2, 1) + 0.1, oma = c(3, 3, 3, 3))
my_gam_check(model1)
my_gam_check(model2)
my_gam_check(model3)
my_gam_check(model4)

mtext("QQ Plot", side = 3, outer = TRUE, line = 0, at = 0.2)#, cex=0.5)
mtext("Residuals vs Linear Preds", side = 3, outer = TRUE, line = 0, at = 0.525)#, cex=0.5)
mtext("Histogram of Residuals", side = 3, outer = TRUE, line = 0, at = 0.85)#, cex=0.5)

mtext("Model 1", side = 2, outer = TRUE, line = 1, at = 0.9)
mtext("Model 2", side = 2, outer = TRUE, line = 1, at = 0.65)
mtext("Model 3", side = 2, outer = TRUE, line = 1, at = 0.4)
mtext("Model 4", side = 2, outer = TRUE, line = 1, at = 0.15)

library(grid)

# Assuming the composite figure is plotted
grid.text("QQ Plot", x = 0.25, y = 0.96, gp = gpar(fontsize = 14))
grid.text("Residuals vs Linear Predictions", x = 0.5, y = 0.96, gp = gpar(fontsize = 14))
grid.text("Histogram of Residuals", x = 0.75, y = 0.96, gp = gpar(fontsize = 14))

grid.text("Row 1 Label", x = 0.02, y = 0.75, rot = 90, gp = gpar(fontsize = 14))
grid.text("Row 2 Label", x = 0.02, y = 0.25, rot = 90, gp = gpar(fontsize = 14))


gam.check(model2)


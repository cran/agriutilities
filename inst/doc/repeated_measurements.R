## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----warning=FALSE, message=FALSE, fig.width = 9------------------------------
library(ggpubr)
library(agriutilities)
library(tidyr)
library(dplyr)
library(tibble)

if (requireNamespace("asreml", quietly = TRUE)) {
  library(asreml)
  head(grassUV) %>% print()
  grassUV %>%
    ggplot(
      aes(x = Time, y = y, group = Plant, color = Plant)
    ) +
    geom_point() +
    geom_line() +
    facet_wrap(~Tmt) +
    theme_minimal(base_size = 15)
}

## ----warning=FALSE, message=FALSE, fig.width = 9------------------------------
if (requireNamespace("asreml", quietly = TRUE)) {
  tmp <- grassUV %>%
    group_by(Time, Plant) %>%
    summarise(mean = mean(y, na.rm = TRUE)) %>%
    spread(Time, mean) %>%
    column_to_rownames("Plant")

  a <- covcor_heat(
    matrix = cor(tmp),
    legend = "none",
    size = 4.5
  ) +
    ggtitle(label = "Pearson Correlation")

  b <- tmp %>%
    cor(use = "pairwise.complete.obs") %>%
    as.data.frame() %>%
    rownames_to_column(var = "Time") %>%
    gather("DAP2", "corr", -1) %>%
    type.convert(as.is = FALSE) %>%
    mutate(corr = ifelse(Time < DAP2, NA, corr)) %>%
    mutate(DAP2 = as.factor(DAP2)) %>%
    ggplot(
      aes(x = Time, y = corr, group = DAP2, color = DAP2)
    ) +
    geom_point() +
    geom_line() +
    theme_minimal(base_size = 15) +
    color_palette(palette = "jco") +
    labs(color = "Time", y = "Pearson Correlation") +
    theme(legend.position = "top")
  ggarrange(a, b)
}

## ----warning=FALSE, message=FALSE---------------------------------------------
if (requireNamespace("asreml", quietly = TRUE)) {
  # Identity variance model.
  model_0 <- asreml(
    fixed = y ~ Time + Tmt + Tmt:Time,
    residual = ~ id(Plant):idv(Time),
    data = grassUV
  )

  # Simple correlation model; homogeneous variance form.
  model_1 <- asreml(
    fixed = y ~ Time + Tmt + Tmt:Time,
    residual = ~ id(Plant):corv(Time),
    data = grassUV
  )

  # Exponential (or power) model; homogeneous variance form.
  model_2 <- asreml(
    fixed = y ~ Time + Tmt + Tmt:Time,
    residual = ~ id(Plant):expv(Time),
    data = grassUV
  )

  # Exponential (or power) model; heterogeneous variance form.
  model_3 <- asreml(
    fixed = y ~ Time + Tmt + Tmt:Time,
    residual = ~ id(Plant):exph(Time),
    data = grassUV
  )

  # Antedependence variance model of order 1
  model_4 <- asreml(
    fixed = y ~ Time + Tmt + Tmt:Time,
    residual = ~ id(Plant):ante(Time),
    data = grassUV
  )

  # Autoregressive model of order 1; homogeneous variance form.
  model_5 <- asreml(
    fixed = y ~ Time + Tmt + Tmt:Time,
    residual = ~ id(Plant):ar1v(Time),
    data = grassUV
  )

  # Autoregressive model of order 1; heterogeneous variance form.
  model_6 <- asreml(
    fixed = y ~ Time + Tmt + Tmt:Time,
    residual = ~ id(Plant):ar1h(Time),
    data = grassUV
  )

  # Unstructured variance model.
  model_7 <- asreml(
    fixed = y ~ Time + Tmt + Tmt:Time,
    residual = ~ id(Plant):us(Time),
    data = grassUV
  )
}

## ----warning=FALSE, message=FALSE, fig.width = 9------------------------------
if (requireNamespace("asreml", quietly = TRUE)) {
  models <- list(
    "idv" = model_0,
    "corv" = model_1,
    "expv" = model_2,
    "exph" = model_3,
    "ante" = model_4,
    "ar1v" = model_5,
    "ar1h" = model_6,
    "us" = model_7
  )

  summary_models <- data.frame(
    model = names(models),
    aic = unlist(lapply(models, function(x) summary(x)$aic)),
    bic = unlist(lapply(models, function(x) summary(x)$bic)),
    loglik = unlist(lapply(models, function(x) summary(x)$loglik)),
    nedf = unlist(lapply(models, function(x) summary(x)$nedf)),
    param = unlist(lapply(models, function(x) attr(summary(x)$aic, "param"))),
    row.names = NULL
  )

  summary_models %>% print()

  summary_models %>%
    ggplot(
      aes(x = reorder(model, -bic), y = bic, group = 1)
    ) +
    geom_point(size = 2) +
    geom_text(aes(x = model, y = bic + 5, label = param), size = 5) +
    geom_line() +
    theme_minimal(base_size = 15) +
    labs(x = NULL, y = "BIC")
}

## ----warning=FALSE, message=FALSE, fig.width = 9------------------------------
if (requireNamespace("asreml", quietly = TRUE)) {
  mat <- extract_rcov(model_4, time = "Time", plot = "Plant")
  print(mat)

  # Plotting  Matrix
  p1 <- covcor_heat(matrix = mat$corr, legend = "none", size = 4.5) +
    ggtitle(label = "Correlation Matrix (ante)")
  p1

  p2 <- covcor_heat(
    matrix = mat$vcov,
    corr = FALSE,
    legend = "none",
    size = 4.5,
    digits = 1
  ) +
    ggtitle(label = "Covariance Matrix (ante)")
  p2
  ggarrange(p1, p2)
}

## ----warning=FALSE, message=FALSE, fig.width = 9------------------------------
if (requireNamespace("asreml", quietly = TRUE)) {
  ggarrange(a, p1)
}

## ----warning=FALSE, message=FALSE, fig.width = 9------------------------------
if (requireNamespace("asreml", quietly = TRUE)) {
  pvals <- predict(model_4, classify = "Tmt:Time")$pvals
  grassUV %>%
    ggplot(
      aes(x = Time, y = y, group = Tmt, color = Tmt)
    ) +
    geom_point(alpha = 0.5, size = 3) +
    geom_line(data = pvals, mapping = aes(y = predicted.value)) +
    scale_color_manual(values = c("red", "grey50")) +
    facet_wrap(~Tmt) +
    theme_minimal(base_size = 15) +
    theme(legend.position = "none")
}


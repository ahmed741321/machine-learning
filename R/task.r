

# قيم المحاور X و Y
x <- c(155, 165, 175, 185, 195)
y <- c(55, 65, 75, 85, 95, 105)

# مصفوفة التكرارات (frequency)
frequency <- matrix(c(
  2, 0, 0, 0, 0, 0,
  4, 4, 0, 0, 0, 0,
  1, 6, 3, 1, 0, 0,
  0, 1, 4, 1, 1, 0,
  0, 0, 0, 0, 1, 1
), nrow = 5, byrow = TRUE)

# إنشاء DataFrame باستخدام x كفهرس (index) و y كأعمدة (columns)
df <- as.data.frame(frequency)

# تعيين أسماء الصفوف والأعمدة
rownames(df) <- x
colnames(df) <- y


# حساب المتوسطات بناءً على التكرارات
mean_x <- sum(as.numeric(rownames(df)) * rowSums(df)) / sum(frequency)
mean_y <- sum(as.numeric(colnames(df)) * colSums(df)) / sum(frequency)

# حساب التباين المشترك Cov(x, y)
cov_xy <- sum(frequency * outer(as.numeric(rownames(df)) - mean_x, as.numeric(colnames(df)) - mean_y)) / sum(frequency)

# حساب التباين (Variance) لقيم X و Y بناءً على التكرارات
var_x <- sum(rowSums(frequency) * (as.numeric(rownames(df)) - mean_x)^2) / sum(frequency)
var_y <- sum(colSums(frequency) * (as.numeric(colnames(df)) - mean_y)^2) / sum(frequency)

# حساب n(X) و n(Y) من مصفوفة التكرارات
n_X <- rowSums(frequency)
n_Y <- colSums(frequency)

# حساب مجموع التكرارات الكلي (n)
n_total <- sum(frequency)

# طباعة التعبيرات بدون حساب القيم
cov_xy_var_x_expr <- cov_xy / var_x
cov_xy_var_y_expr <- cov_xy / var_y

y_update_expr <- sprintf("%f + %f * (x - %f)", mean_y, cov_xy / var_x, mean_x)
x_update_expr <- sprintf("%f + %f * (y - %f)", mean_x, cov_xy / var_y, mean_y)

# طباعة التعبيرات
cat("Updated Y expression:", y_update_expr, "\n")
cat("Updated X expression:", x_update_expr, "\n")
cat("Cov_xy / Var_x expression:", cov_xy_var_x_expr, "\n")
cat("Cov_xy / Var_y expression:", cov_xy_var_y_expr, "\n")

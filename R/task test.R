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

# إنشاء مصفوفات جديدة لتخزين القيم المكررة
x_repeated <- numeric(0)
y_repeated <- numeric(0)

# تكرار القيم بناءً على التكرارات
for (i in 1:nrow(frequency)) {
  for (j in 1:ncol(frequency)) {
    if (frequency[i, j] > 0) {
      x_repeated <- c(x_repeated, rep(x[i], frequency[i, j]))
      y_repeated <- c(y_repeated, rep(y[j], frequency[i, j]))
    }
  }
}

# عرض القيم المكررة

# تحويل القيم المكررة إلى جدول تلخيصي
data <- table(x_repeated, y_repeated)

# عرض النقاط المكررة
plot(x_repeated, y_repeated, main = "Scatter Plot of Repeated Points", 
     xlab = "X Values", ylab = "Y Values", pch = 19, col = "blue")

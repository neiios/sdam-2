library(readr)

# Duomenų nuskaitymas
wine <- read_delim("./datasets/winequality-red.csv", delim = ";")
colnames(wine) <- gsub(" ", "_", colnames(wine))

# Duomenų priešanalizė / išskirčių pašalinimas
PR <- mapply(anyNA, wine)
PR

boxplot(wine)

outliers <- boxplot.stats(wine$total_sulfur_dioxide)$out
wine <- wine[-which(wine$total_sulfur_dioxide %in% outliers),]
boxplot(wine)

outliers <- boxplot.stats(wine$free_sulfur_dioxide)$out
wine <- wine[-which(wine$free_sulfur_dioxide %in% outliers),]
boxplot(wine)

outliers <- boxplot.stats(wine$residual_sugar)$out
wine <- wine[-which(wine$residual_sugar %in% outliers),]
boxplot(wine)

outliers <- boxplot.stats(wine$fixed_acidity)$out
wine <- wine[-which(wine$fixed_acidity %in% outliers),]
boxplot(wine)

outliers <- boxplot.stats(wine$quality)$out
wine <- wine[-which(wine$quality %in% outliers),]
boxplot(wine)

outliers <- boxplot.stats(wine$total_sulfur_dioxide)$out
wine <- wine[-which(wine$total_sulfur_dioxide %in% outliers),]
boxplot(wine)

# Aprašomoji statistika
summary(wine)

apply(wine,2,sd)

# Koreliacijos tarp rodiklių
source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(wine)

library(Hmisc)
rcorr(as.matrix(wine))

attach(wine)
model <- lm(quality~chlorides+sulphates
            +citric_acid+alcohol
            +volatile_acidity + pH)

summary(model)

confint(model)

# Prielaidų analizė
library(QuantPsyc)
library(lmtest)
plot(model)

# 1. Ar liekamosios paklaidos homoskedastiškos?
plot(model, which=3)
bptest(model)

# 2. Ar paklaidos normalios?
plot(model, which=2)
shapiro.test(model$residuals[0:5000])

# 3. Ar nėra išskirčių?
plot(model, which=4)
library(car)
outlierTest(model)

# 4. Ar regresoriai nėra multikolinearūs?
vif(model)

# 5. Prognozė
prognoze <- predict(model, data.frame("chlorides"=0.070, "sulphates"=0.5, 
                                      "citric_acid"=0, "alcohol"=10, 
                                      "volatile_acidity"=0.7, "pH"=3.5))
prognoze

plot(prognoze)

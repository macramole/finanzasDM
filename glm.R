df_small.indexes <- createDataPartition( abril_dataset$clase, p = .20, list = FALSE)

df_small = abril_dataset[df_small.indexes, ]
db.cantnulls(df_small)

# df_small = df_small[,colnames(df_small) %in% c("numero_de_cliente","foto_mes","participa",
#                            "tpaquete1","tpaquete2","tpaquete3","tpaquete4","tpaquete5","tpaquete6", "tpaquete8") ]

summary(df_small)

model = glm(formula = clase ~ ., family = binomial(link = "logit"), na.action = na.exclude, data = df_small)
summary(model)

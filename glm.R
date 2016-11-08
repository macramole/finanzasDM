df = db.getDataset(db.BINARIA1)
df = db.nonulls(df)

df = db.getBigDataset( cual = db.BINARIA1, discret = F)
df$clase =factor(df$clasebinaria1)
df$clase = ifelse(df$clase == "NEG", 0, 1)
df$clasebinaria1 = NULL


df = db.nonulls(df)
claseIndex = which( colnames(df) == "clase" )

s = 1
set.seed( seeds[s] )

# df.mat = as.matrix( df[training.indexes, -claseIndex] )
# df.mat = cbind(df.mat, as.numeric( df[training.indexes, claseIndex] ) - 1)

df.indexes <- createDataPartition( df$clase, p = .70, list = FALSE)





# df_small = df[df_small.indexes, ]
# db.cantnulls(df_small)

# df_small = df_small[,colnames(df_small) %in% c("numero_de_cliente","foto_mes","participa",
#                            "tpaquete1","tpaquete2","tpaquete3","tpaquete4","tpaquete5","tpaquete6", "tpaquete8") ]

summary(df_small)

t0 =  Sys.time()
model.full = glm(formula = clase ~ ., family = binomial(link = "logit"), na.action = na.exclude, data = df[df.indexes, ])
model.null = glm(formula = clase ~ 1, family = binomial(link = "logit"), na.action = na.exclude, data = df[df.indexes, ])
model.step = model.null
model.step = step( model.step, 
                   scope = list( lower = model.null, upper = model.full ) , 
                   direction = c("forward"))
t1 =  Sys.time()
str(df[df.indexes, 100:188])

summary(model.full)

save(model.step, file = "logit_forward_model.save")

tiempos[s] <-  as.numeric(  t1 - t0, units = "secs" )
paste(tiempos[s] / 60,"minutos")

prediction = predict(model.step, df[-df.indexes,], type = "response" )

ganancias = ganancia.binaria1.umbral( prediction, df[-df.indexes, ] )


model.better = glm( formula = clase ~ ., 
                    family = binomial(link = "logit"), 
                    na.action = na.exclude, 
                    data = df[df.indexes, ])



sink("glmresult.txt")
summary(model$coefficients)
sink()

write.table(model$coefficients, file = "glmcoef.tsv")
head(model$residuals, n = 100)
plot(model$residuals)
plot(density(model$residuals))

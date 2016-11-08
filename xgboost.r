library(xgboost)

df = db.getBigDataset( cual = db.BINARIA1, discret = F)
df = db.nonulls(df)

claseIndex = which( colnames(df) == "clase" )

s = 1
set.seed( seeds[s] )
training.indexes <- createDataPartition( df$clase, p = .70, list = FALSE)
# df.training    <- df[ indexes,]
# df.testing  <- df[-indexes,]

# vweights <- ifelse( df.training$clase =='BAJA+2', 31, 1 )



tiempos = c()
ganancias = c()

t0 =  Sys.time()  
model = xgboost(  data = as.matrix( df[training.indexes, -claseIndex] ),
                  label = as.numeric( df[training.indexes, claseIndex] ) - 1,
                  missing = db.NULL_VALUE,
                  nrounds = 10,
                  params = list(
                    # objective = "multi:softprob",
                    objective = "binary:logistic",
                    eval_metric = "auc",
                    # num_class = 3,
                    eta = 0.3,
                    gamma = 0,
                    max_depth = 6,
                    min_child_weight = 1,
                    max_delta_step = 0,
                    subsample = 0.5,
                    colsample_bytree = 1,
                    colsample_bylevel = 1,
                    alpha = 0,
                    lambda = 0,
                    scale_pos_weight = sum( df[training.indexes, claseIndex] == "NEG" ) / sum( df[training.indexes, claseIndex] == "POS" ),
                    nthread = 4
                  )
              )
t1 =  Sys.time()

# save(model, file = "xgboost.model")
# load(file = "xgboost.model")
# 
# varImportance = xgb.importance( feature_names = colnames(df), 
#                                 model = model, 
#                                 data = as.matrix( df[training.indexes, -claseIndex] ), 
#                                 label = as.numeric( df[training.indexes, claseIndex] ) - 1 )

tiempos[s] <-  as.numeric(  t1 - t0, units = "secs" )
paste(tiempos[s] / 60,"minutos")

testing.predict = predict(model, as.matrix(df[-training.indexes, -claseIndex]) )
# testing.predict = matrix( data = testing.predict, ncol = 3, nrow = nrow( df[-training.indexes,] ), byrow = T )
# testing.predict = matrix( data = testing.predict, ncol = 2, nrow = nrow( df[-training.indexes,] ), byrow = T )
# colnames( testing.predict ) = c("BAJA+1", "BAJA+2", "CONTINUA")
# colnames( testing.predict ) = c("NEG", "POS")
# ganancias[s] = ganancia.ternaria( testing.predict,  df[-training.indexes, claseIndex], 0.1 ) / 0.30
# plot( testing.predict ~ df[-training.indexes, claseIndex] )
ganancias = ganancia.binaria1.umbral( testing.predict,  df[-training.indexes, claseIndex] )
# ganancias[s] = 
ganancias
# ganancias[s] = ganancia.ternaria( testing.predict,  testClass ) / 0.30

cat(tiempos[s], " | ", ganancias[s], " | ", umbrales[s], "\n")


# sink("xgboost.txt")
# str(df, list.len = ncol(df))
# sink()

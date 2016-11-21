library(xgboost)
source("db.R")

df.checkpoint = read.table("db/checkpoint/checkpoint.2.importantes.tsv", sep = "\t", row.names = 1)
df.all = db.getDatasetImportantes(discret = F)
claseIndex = which( colnames(df.all) == "clase" )

#XGBOOST
dfNoNulls = db.nonulls( df.all, nullValue = 0 )

#lo paso a binaria
dfNoNulls$clasebinaria1 = as.factor ( ifelse( dfNoNulls$clase == "BAJA+2", "POS", "NEG" ) )
dfNoNulls = dfNoNulls[, -claseIndex]
newClaseIndex = which( colnames(dfNoNulls) == "clasebinaria1" )
colnames(dfNoNulls)[newClaseIndex] = "clase"

vnround = 460
vmax_depth = 5
vmin_child_weight = 10

model.xgboost = xgboost(  data = as.matrix( dfNoNulls[, -claseIndex] ),
                          label = as.numeric( dfNoNulls[, claseIndex] ) - 1,
                          # missing = db.NULL_VALUE,
                          nrounds = vnround,
                          params = list(
                            objective = "multi:softprob",
                            # objective = "binary:logistic",
                            eval_metric = "merror",
                            num_class = 2,
                            eta = 0.01,
                            
                            max_depth = vmax_depth,
                            min_child_weight = vmin_child_weight,
                            max_delta_step = 0,
                            subsample = 0.7,
                            colsample_bytree = 0.4,
                            colsample_bylevel = 1,
                            
                            alpha = 0,
                            lambda = 0.1,
                            gamma = 0.01,
                            
                            # scale_pos_weight = sum( df[training.indexes, claseIndex] == "NEG" ) / sum( df[training.indexes, claseIndex] == "POS" ),
                            nthread = 4
                          )
)

rm(dfNoNulls)
gc()

## resultados
df.checkpoint.noNulls = db.nonulls( df.checkpoint, nullValue = 0 )
# df.checkpoint.noNulls$clasebinaria1 = as.factor ( ifelse( df.checkpoint.noNulls$clase == "BAJA+2", "POS", "NEG" ) )
# df.checkpoint.noNulls = df.checkpoint.noNulls[, -claseIndex]
# newClaseIndex = which( colnames(df.checkpoint.noNulls) == "clasebinaria1" )
# colnames(df.checkpoint.noNulls)[newClaseIndex] = "clase"
predictions.xgboost = predict(  model.xgboost, as.matrix(df.checkpoint.noNulls) )
predictions.xgboost = matrix( data = predictions.xgboost, ncol = 2, nrow = nrow( df.checkpoint.noNulls ), byrow = T )
predictions.xgboost = predictions.xgboost[,2]


##votacion
names(predictions.xgboost) = rownames(df.checkpoint)

ids = names( Filter( function(x) { x >= 250/8000 }, predictions.xgboost ) )
write.table(ids, file = "garber_3.txt", quote = F, row.names = F, col.names = F)

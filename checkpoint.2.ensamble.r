library(ranger)
library(xgboost)
source("db.R")

df.checkpoint = read.table("db/checkpoint/checkpoint.2.importantes.tsv", sep = "\t", row.names = 1)
df.all = db.getDatasetImportantes(discret = F)
claseIndex = which( colnames(df.all) == "clase" )

umbralesModels = c(0.0558, 250/8000)

#ranger
discretized = db.discretize.soft(df.all)
discretized = db.nonulls(discretized)

canttrees = 300
vmin.node.size = 100
vimportance = "impurity"

# vweights <- ifelse( df$clase =='BAJA+2', 31, 1 )

model.ranger = ranger( 
  dependent.variable.name = "clase",
  data = discretized, 
  num.trees = canttrees,
  importance = vimportance,
  # case.weights = vweights,
  num.threads = 4,
  min.node.size = vmin.node.size,
  probability = T
  # save.memory = T
)

rm(discretized)
gc()

# result = predict(  model, dfTest , type = "response")
# predictions = result$predictions
# predictions[,2]

#XGBOOST
dfNoNulls = db.nonulls( df.all, nullValue = 0 )

#lo paso a binaria
dfNoNulls$clasebinaria1 = as.factor ( ifelse( dfNoNulls$clase == "BAJA+2", "POS", "NEG" ) )
dfNoNulls = dfNoNulls[, -claseIndex]
newClaseIndex = which( colnames(dfNoNulls) == "clasebinaria1" )
colnames(dfNoNulls)[newClaseIndex] = "clase"

vnround = 400
vmax_depth = 15
vmin_child_weight = 5

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
df.checkpoint.discretized = db.discretize.soft(df.checkpoint)
df.checkpoint.discretized = db.nonulls(df.checkpoint.discretized)
result.ranger = predict(  model.ranger, df.checkpoint.discretized , type = "response")
predictions.ranger = result$predictions
predictions.ranger = predictions.ranger[,2]

df.checkpoint.noNulls = db.nonulls( df.checkpoint, nullValue = 0 )
df.checkpoint.noNulls$clasebinaria1 = as.factor ( ifelse( df.checkpoint.noNulls$clase == "BAJA+2", "POS", "NEG" ) )
df.checkpoint.noNulls = df.checkpoint.noNulls[, -claseIndex]
newClaseIndex = which( colnames(df.checkpoint.noNulls) == "clasebinaria1" )
colnames(df.checkpoint.noNulls)[newClaseIndex] = "clase"
predictions.xgboost = predict(  model.xgboost, as.matrix(df.checkpoint.noNulls) )
predictions.xgboost = matrix( data = predictions.xgboost, ncol = 2, nrow = nrow( df.checkpoint.noNulls ), byrow = T )
predictions.xgboost = predictions.xgboost[,2]


##votacion

votos.ranger = ifelse( predictions.ranger > umbralesModels[1], 1, 0 )
votos.xgboost = ifelse( predictions.xgboost > umbralesModels[2], 1, 0 )

votos.ensamble = Reduce('+', list(votos.ranger, votos.xgboost))
names(votos.ensamble) = rownames(df.checkpoint)

ids = names( Filter( function(x) { x == 2 }, votos.ensamble ) )
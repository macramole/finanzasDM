source("ternaria.funciones.R")

abril_dataset = db.getBigDataset()
abril_dataset = db.nonulls(abril_dataset)
db.cantnulls(abril_dataset)

checkpoint_dataset = db.discretize.soft(checkpoint_dataset)
checkpoint_dataset = db.discretize.tend(checkpoint_dataset)
checkpoint_dataset = db.clean(checkpoint_dataset)
checkpoint_dataset = db.nonulls(checkpoint_dataset)
db.cantnulls(checkpoint_dataset)

#a veces hay mas campos mirar bien que onda
#checkpoint_dataset[, setdiff(colnames(checkpoint_dataset), colnames(abril_dataset) )] = NULL

library(ranger)
canttrees = 300
vmin.node.size = 2200
vimportance = "none"
umbral_average = 0.446

#Asigno pesos <7750, 250>  es equivalente a  <31, 1>  , le pongo 15 porque algunos son neg
vweights <- ifelse( abril_dataset$clase =='BAJA+2', 31, 1 )

t0 =  Sys.time()
model = ranger( 
  dependent.variable.name = "clase",
  data = abril_dataset, 
  num.trees = canttrees,
  importance = vimportance,
  case.weights = vweights,
  num.threads = 1,
  min.node.size = vmin.node.size,
  probability = T
  # save.memory = T
)
t1 =  Sys.time()

#calculo la ganancia en checkpoint
# checkpoint_prediccion  = predict(  model, checkpoint_dataset , type = "prob")
checkpoint_prediccion  = predict(  model, checkpoint_dataset , type = "response")
# checkpoint_pos_ids = getPositivosDeLista(checkpoint_prediccion, vhojas_positivas)
checkpoint_pos_ids = rownames(checkpoint_dataset)[ checkpoint_prediccion$predictions[,"BAJA+2"] > umbral_average ]

write.table(checkpoint_pos_ids, file = "garber_1.txt", row.names = F, col.names = F, quote = F)




checkpoint_pos_ids.1 = read.table("garber_1.txt")
checkpoint_pos_ids.2 = read.table("garber_2.txt")


length(intersect(checkpoint_pos_ids.1$V1, checkpoint_pos_ids.2$V1))

source("ternaria.funciones.R")

abril_dataset = db.getDataset(db.TERNARIA, F)
abril_dataset = db.nonulls(abril_dataset)
db.cantnulls(abril_dataset)

checkpoint_dataset = db.getCheckpoint()
checkpoint_dataset = db.nonulls(checkpoint_dataset)
db.cantnulls(checkpoint_dataset)


# head(abril_dataset)

vcp = 0.005
vminsplit = 400
vminbucket = 1
vmaxdepth = 6
        
abril_inValidation <- createDataPartition( abril_dataset$clase, p = .70, list = FALSE)
abril_dataset_training    <- abril_dataset[  abril_inValidation, ]
abril_dataset_validation  <- abril_dataset[ -abril_inValidation, ]
          
#Asigno pesos <7750, 250>  es equivalente a  <31, 1>  , le pongo 15 porque algunos son neg
vweights <- ifelse( abril_dataset_training$clase =='BAJA+2', 31, 1 )
    
t0 =  Sys.time()
model <- rpart( clase ~ .,
                data = abril_dataset_training,
                method="class",
                xval=0,
                maxsurrogate=1,
                surrogatestyle=1,
                x = F,
                y = F,
                weights = vweights,
                cp=vcp,
                minsplit=vminsplit,
                minbucket=vminbucket,
                maxdepth=vmaxdepth )
t1 =  Sys.time()

checkpoint_pos_ids_1 = checkpoint_pos_ids

# library(rpart.plot)
# prp(model, type = 1, extra = 4)

#determino las hojas con ganancia positiva en VALIDATION
abril_validation_prediccion  = predict(  model, abril_dataset_validation , type = "prob")
vhojas_positivas = hojas_positivas( abril_validation_prediccion,  abril_dataset_validation$clase )
          
#calculo la ganancia en checkpoint
checkpoint_prediccion  = predict(  model, checkpoint_dataset , type = "prob")
checkpoint_pos_ids = getPositivosDeLista(checkpoint_prediccion, vhojas_positivas)

write.table(checkpoint_pos_ids, file = "garber_1.txt", row.names = F, col.names = F)




library(ranger)
canttrees = 500
vmin.node.size = 1500
vimportance = "none"

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
checkpoint_pos_ids = rownames(checkpoint_dataset)[ checkpoint_prediccion$predictions[,"BAJA+2"] > 0.5 ]

write.table(checkpoint_pos_ids, file = "garber_2.txt", row.names = F, col.names = F)




checkpoint_pos_ids.1 = read.table("garber_1.txt")
checkpoint_pos_ids.2 = read.table("garber_2.txt")


length(intersect(checkpoint_pos_ids.1$V1, checkpoint_pos_ids.2$V1))
        
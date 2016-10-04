library(rpart)
library(caret)

seeds <- c( 442619, 664579, 235813 , 502841, 351551 )



       ########################
       ##      TERNARIA      ##
       ########################

abril_dataset = db.getDataset()

#model params
vcp <- 1 ;
vminsplit <- 50 ;
vminbucket <- 8 ;
vmaxdepth <- 8

# vcpValues = c(0, 0.0001, 0.001, 0.005)
# vminsplitValues = c(20,50,200)
# vminbucketValues = c(2,5) #padre / cada número
# vmaxdepthValues = c(5,10,15,20)

vcpValues = c(0)
vminsplitValues = c(18,19,20,21,22)
vminbucketValues = c(1,2,3) #padre / cada número
vmaxdepthValues = c(9,10,11,12,13)

table(abril_dataset$clase)


run(abril_dataset, "ternaria", ganancia.ternaria, vcpValues, vminsplitValues, vminbucketValues, vmaxdepthValues )



     ########################
     ##      BINARIA1      ##
     ########################

db.connect()
dataset_binaria = db.getDataset( db.BINARIA1 )

vcpValues = c(0)
vminsplitValues = c(18,19,20,21,22)
vminbucketValues = c(1,2,3) #padre / cada número
vmaxdepthValues = c(9,10,11,12,13)

# vcpValues = c(0)
# vminsplitValues = c(20)
# vminbucketValues = c(2) #padre / cada número
# maxdepthValues = c(11)

# Cada corrida con 4 seeds tarda: 
tiempo = 71.2 * 4
cantCorridas = length(vcpValues) * length(vminsplitValues) * length(vminbucketValues) * length(maxdepthValues)
  
cat( "Tardará:",
  round(tiempo * cantCorridas / 60 / 60), "horas" )

# TEST
# vcpValues = c(0)
# vminsplitValues = c(18)
# vminbucketValues = c(1,2,3) #padre / cada número
# vmaxdepthValues = c(9)

#proporcion
t = table(dataset_binaria$clase)
t[2] / t[1]

# TEST
# dataset_binaria_reducida = dataset_binaria[ createDataPartition( dataset_binaria$clase, p = .20, list = F ), ]
# 
# #proporcion
# t = table(dataset_binaria_reducida$clase)
# t[2] / t[1]
# 
# run(dataset_binaria_reducida, "binaria1_20%", ganancia.binaria1, vcpValues, vminsplitValues, vminbucketValues, vmaxdepthValues )

run(dataset_binaria, "binaria1", ganancia.binaria1, vcpValues, vminsplitValues, vminbucketValues, vmaxdepthValues )



     ###########################
     ##      OVERSAMPLED      ##
     ###########################

db.connect()
dataset_binaria = db.getDataset( db.BINARIA1 )
dataset_binaria_oversampled = upSample( dataset_binaria, as.factor(dataset_binaria$clase) )
dataset_binaria_oversampled = dataset_binaria_oversampled[,-172]
str(dataset_binaria_oversampled[171])

vcpValues = c(0)
vminsplitValues = c(18,19,20,21,22)
vminbucketValues = c(1,2,3) #padre / cada número
vmaxdepthValues = c(9,10,11,12,13)

#esta es la mayor por el momento
vcpValues = c(0)
vminsplitValues = c(20)
vminbucketValues = c(2) #padre / cada número
vmaxdepthValues = c(11)

# Cada corrida con 4 seeds tarda: 
tiempo = 71.2 * 4
cantCorridas = length(vcpValues) * length(vminsplitValues) * length(vminbucketValues) * length(vmaxdepthValues)
cantCorridas

cat( "Tardará:",
     round(tiempo * cantCorridas / 60 / 60), "horas" )

# TEST
# vcpValues = c(0)
# vminsplitValues = c(18)
# vminbucketValues = c(1,2,3) #padre / cada número
# vmaxdepthValues = c(9)

#proporcion
t = table(dataset_binaria$clase)
t[2] / t[1]
t = table(dataset_binaria_oversampled$clase)
t[2] / t[1]

# TEST
# dataset_binaria_reducida = dataset_binaria[ createDataPartition( dataset_binaria$clase, p = .20, list = F ), ]
# 
# #proporcion
# t = table(dataset_binaria_reducida$clase)
# t[2] / t[1]
# 
# run(dataset_binaria_reducida, "binaria1_20%", ganancia.binaria1, vcpValues, vminsplitValues, vminbucketValues, vmaxdepthValues )

run(dataset_binaria_oversampled, "binaria1_oversampled", ganancia.oversampled, vcpValues, vminsplitValues, vminbucketValues, vmaxdepthValues )













#trainingList = c()

#for( s in 1:length(seeds) )
#{
#  set.seed( seed[s] )
#  trainingList = c(trainingList, createDataPartition( abril_dataset$clase, p = .70))
#}
?"%dopar%"
#aca poner bien los parámetros
paramsGrid = expand.grid( cp = c(0, 0.0001, 0.001),
                          #minsplit = c(20,50,200),
                          #minbucket = c(10,25,100,4,10,40),
                          maxdepth = c(5,10,15,20)
                          );

ctrl <- trainControl(method = "LGOCV",
                     number = 1,
                     p = 0.7,
                     #seeds = seeds,
                     #summaryFunction = ganancia.ternaria,
                     #classProbs = TRUE,
                     verboseIter = TRUE
                     #index = trainingList,
                     #savePredictions = TRUE
                     )

fit.ternaria = ?train( x = abril_dataset[,-ncol(abril_dataset)], 
                       y = abril_dataset[,ncol(abril_dataset)], 
                      method = "rpart2", 
                      cp = 0 )
                      #trControl = ctrl, 
                      #verbose = TRUE, 
                      #tuneGrid = paramsGrid)

fit.ternaria
=
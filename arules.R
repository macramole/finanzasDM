library(arules)

df = db.getDataset( historicas = F )
claseIndex = which( colnames(df) == "clase" )

table(df$clase)

# df_small = df[ 1:5000, ]
# df_small = db.discretize(df_small[, -claseIndex], 20)
# df_small = cbind(df_small, df[1:5000, "clase"])
# colnames(df_small)[ncol(df_small)] = "clase"
# head(df_small)

df_discret = db.discretize(df[, -claseIndex], 20)
df_discret = cbind(df_discret, df$clase)
colnames(df_discret)[ncol(df_discret)] = "clase"

rm("df")
gc()

table(df_discret$clase)

norulesIndex = which( colnames(df_discret) %in% c("Visa_cuenta_estado","Master_cuenta_estado", "Visa_Finiciomora", "Master_Finiciomora", "Visa_marca_atraso", "Master_marca_atraso") )

rules <- apriori(df_discret[,-norulesIndex], 
                 parameter = list(support=0.0001, confidence = 250/8000, maxlen = 3),
                 appearance = list(rhs = c("clase=BAJA+2"),
                                   none = c("clase=BAJA+1","clase=CONTINUA"),
                                   default="lhs"))

df_rules = as(rules,"data.frame")
# head(df_rules)
write.table(df_rules, "rules_3.tsv", sep = "\t", row.names = F, col.names = T)

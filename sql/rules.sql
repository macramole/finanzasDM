create table data_rules as
select
	case when
		mdescubierto_preacordado = 0 and ( Visa_marca_atraso = 1 or Master_marca_atraso = 1 or Visa_Finiciomora is not null or Master_Finiciomora is not null )
	then 
		1 
	else 
		0
	end as rule_1,
	case when
		ttarjeta_visa = 0 and ( Visa_marca_atraso = 1 or Visa_Finiciomora is not null )
	then 
		1 
	else 
		0
	end as rule_2,
	case when
		 mcuenta_corriente_Paquete < -1560 and mdescubierto_preacordado=0
	then 
		1 
	else 
		0
	end as rule_3,


from
    data





{ttarjeta_visa=0,Visa_mconsumototal=[ -29444,  27849)} => {clase=BAJA+2}
{ttarjeta_visa=0,Visa_mconsumospesos=[ -29444,  27849)} => {clase=BAJA+2}
{cliente_antiguedad=[  1.0, 27.9),ttarjeta_visa=0} => {clase=BAJA+2}
{marketing_coss_selling=6,mcuenta_corriente_Paquete=[-290698,  -1560)} => {clase=BAJA+2}
{ttarjeta_visa=0,thomebanking=0} => {clase=BAJA+2}
{marketing_coss_selling=7,ttarjeta_visa=0} => {clase=BAJA+2}


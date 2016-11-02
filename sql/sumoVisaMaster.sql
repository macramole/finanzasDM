create table visamaster as
select
    numero_de_cliente,
    IFNULL(Master_mfinanciacion_limite,0) + IFNULL(Visa_mfinanciacion_limite,0) as VisaMaster_mfinanciacion_limite,
    IFNULL(Master_msaldototal,0) + IFNULL(Visa_msaldototal,0) as VisaMaster_msaldototal,
    IFNULL(Master_msaldopesos,0) + IFNULL(Visa_msaldopesos,0) as VisaMaster_msaldopesos,
    IFNULL(Master_msaldodolares,0) + IFNULL(Visa_msaldodolares,0) as VisaMaster_msaldodolares,
    IFNULL(Master_mconsumototal,0) + IFNULL(Visa_mconsumototal,0) as VisaMaster_mconsumototal,
    IFNULL(Master_mconsumospesos,0) + IFNULL(Visa_mconsumospesos,0) as VisaMaster_mconsumospesos,
    IFNULL(Master_mconsumosdolares,0) + IFNULL(Visa_mconsumosdolares,0) as VisaMaster_mconsumosdolares,
    IFNULL(Master_mlimitecompra,0) + IFNULL(Visa_mlimitecompra,0) as VisaMaster_mlimitecompra,
    IFNULL(Master_madelantopesos,0) + IFNULL(Visa_madelantopesos,0) as VisaMaster_madelantopesos,
    IFNULL(Master_madelantodolares,0) + IFNULL(Visa_madelantodolares,0) as VisaMaster_madelantodolares,
    IFNULL(Master_mpagado,0) + IFNULL(Visa_mpagado,0) as VisaMaster_mpagado,
    IFNULL(Master_mpagospesos,0) + IFNULL(Visa_mpagospesos,0) as VisaMaster_mpagospesos,
    IFNULL(Master_mpagosdolares,0) + IFNULL(Visa_mpagosdolares,0) as VisaMaster_mpagosdolares,
    IFNULL(Master_mpagominimo,0) + IFNULL(Visa_mpagominimo,0) as VisaMaster_mpagominimo, 
    IFNULL(Master_tconsumos,0) + IFNULL(Visa_tconsumos,0) as VisaMaster_tconsumos, 
    IFNULL(Master_tadelantosefectivo,0) + IFNULL(Visa_tadelantosefectivo,0) as VisaMaster_tadelantosefectivo, 
    IFNULL(Master_marca_atraso,0) + IFNULL(Visa_marca_atraso,0) as VisaMaster_marca_atraso,
	case when
		Visa_cuenta_estado > 10 and Master_cuenta_estado > 10 
	then 
		2
	else 
		case when
		    Visa_cuenta_estado > 10 or Master_cuenta_estado > 10
		then 
		    1
		else
			0
		end
	end as VisaMaster_cuenta_estado,
	case when
		Visa_finiciomora is not null and Master_finiciomora is not null
	then 
		2
	else 
		case when
			Visa_finiciomora is not null or Master_finiciomora is not null
		then 
		    1
		else
			0
		end
	end as VisaMaster_finiciomora
	
	
from
    data
;

ALTER TABLE visamaster ADD COLUMN VisaMaster_mconsumototal_div_mlimitecompra double;

update
    visamaster
set
    VisaMaster_mconsumototal_div_mlimitecompra = case VisaMaster_mlimitecompra when null or 0 then null else (VisaMaster_mconsumototal / VisaMaster_mlimitecompra) end
;

create table data_visamaster as
select
    *,
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
	case when
		Visa_cuenta_estado is null and Master_cuenta_estado is null 
	then 
		0 
	else 
		case when
		    Visa_cuenta_estado is not null and Master_cuenta_estado is not null
		then 
		    2
		else
		    case when
		        Visa_cuenta_estado is null or Master_cuenta_estado is null
		    then
		        1
		    else
		        0
		    end
		end
	end as VisaMaster_cant_tarjetas
from
    data

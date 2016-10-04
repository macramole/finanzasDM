select
    Master_cuenta_estado,
    clase,
    count(*)
from
    data_visamaster
where
    foto_mes = 201604
group by
    Master_cuenta_estado, clase
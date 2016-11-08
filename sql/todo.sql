create table visamaster as
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


create table historicas as
select
    numero_de_cliente,
    AVG(marketing_activo_ultimos90dias) AS marketing_activo_ultimos90dias_avg,
	AVG(cliente_vip) AS cliente_vip_avg,
	AVG(cliente_sucursal) AS cliente_sucursal_avg,
	AVG(cliente_edad) AS cliente_edad_avg,
	AVG(cliente_antiguedad) AS cliente_antiguedad_avg,
	AVG(mrentabilidad) AS mrentabilidad_avg,
	AVG(mrentabilidad_annual) AS mrentabilidad_annual_avg,
	AVG(mcomisiones) AS mcomisiones_avg,
	AVG(mactivos_margen) AS mactivos_margen_avg,
	AVG(mpasivos_margen) AS mpasivos_margen_avg,
	AVG(marketing_coss_selling) AS marketing_coss_selling_avg,
	AVG(tpaquete1) AS tpaquete1_avg,
	AVG(tpaquete2) AS tpaquete2_avg,
	AVG(tpaquete3) AS tpaquete3_avg,
	AVG(tpaquete4) AS tpaquete4_avg,
	AVG(tpaquete5) AS tpaquete5_avg,
	AVG(tpaquete6) AS tpaquete6_avg,
	AVG(tpaquete7) AS tpaquete7_avg,
	AVG(tpaquete8) AS tpaquete8_avg,
	AVG(tpaquete9) AS tpaquete9_avg,
	AVG(tcuentas) AS tcuentas_avg,
	AVG(tcuenta_corriente) AS tcuenta_corriente_avg,
	AVG(mcuenta_corriente_Nopaquete) AS mcuenta_corriente_Nopaquete_avg,
	AVG(mcuenta_corriente_Paquete) AS mcuenta_corriente_Paquete_avg,
	AVG(mcuenta_corriente_dolares) AS mcuenta_corriente_dolares_avg,
	AVG(tcaja_ahorro) AS tcaja_ahorro_avg,
	AVG(mcaja_ahorro_Paquete) AS mcaja_ahorro_Paquete_avg,
	AVG(mcaja_ahorro_Nopaquete) AS mcaja_ahorro_Nopaquete_avg,
	AVG(mcaja_ahorro_dolares) AS mcaja_ahorro_dolares_avg,
	AVG(mdescubierto_preacordado) AS mdescubierto_preacordado_avg,
	AVG(mcuentas_saldo) AS mcuentas_saldo_avg,
	AVG(ttarjeta_debito) AS ttarjeta_debito_avg,
	AVG(ctarjeta_debito_transacciones) AS ctarjeta_debito_transacciones_avg,
	AVG(mautoservicio) AS mautoservicio_avg,
	AVG(ttarjeta_visa) AS ttarjeta_visa_avg,
	AVG(ctarjeta_visa_transacciones) AS ctarjeta_visa_transacciones_avg,
	AVG(mtarjeta_visa_consumo) AS mtarjeta_visa_consumo_avg,
	AVG(ttarjeta_master) AS ttarjeta_master_avg,
	AVG(ctarjeta_master_transacciones) AS ctarjeta_master_transacciones_avg,
	AVG(mtarjeta_master_consumo) AS mtarjeta_master_consumo_avg,
	AVG(cprestamos_personales) AS cprestamos_personales_avg,
	AVG(mprestamos_personales) AS mprestamos_personales_avg,
	AVG(cprestamos_prendarios) AS cprestamos_prendarios_avg,
	AVG(mprestamos_prendarios) AS mprestamos_prendarios_avg,
	AVG(cprestamos_hipotecarios) AS cprestamos_hipotecarios_avg,
	AVG(mprestamos_hipotecarios) AS mprestamos_hipotecarios_avg,
	AVG(tplazo_fijo) AS tplazo_fijo_avg,
	AVG(mplazo_fijo_dolares) AS mplazo_fijo_dolares_avg,
	AVG(mplazo_fijo_pesos) AS mplazo_fijo_pesos_avg,
	AVG(tfondos_comunes_inversion) AS tfondos_comunes_inversion_avg,
	AVG(mfondos_comunes_inversion_pesos) AS mfondos_comunes_inversion_pesos_avg,
	AVG(mfondos_comunes_inversion_dolares) AS mfondos_comunes_inversion_dolares_avg,
	AVG(ttitulos) AS ttitulos_avg,
	AVG(mtitulos) AS mtitulos_avg,
	AVG(tseguro_vida_mercado_abierto) AS tseguro_vida_mercado_abierto_avg,
	AVG(tseguro_auto) AS tseguro_auto_avg,
	AVG(tseguro_vivienda) AS tseguro_vivienda_avg,
	AVG(tseguro_accidentes_personales) AS tseguro_accidentes_personales_avg,
	AVG(tcaja_seguridad) AS tcaja_seguridad_avg,
	AVG(mmonedas_extranjeras) AS mmonedas_extranjeras_avg,
	AVG(minversiones_otras) AS minversiones_otras_avg,
	AVG(tplan_sueldo) AS tplan_sueldo_avg,
	AVG(mplan_sueldo) AS mplan_sueldo_avg,
	AVG(mplan_sueldo_manual) AS mplan_sueldo_manual_avg,
	AVG(cplan_sueldo_transaccion) AS cplan_sueldo_transaccion_avg,
	AVG(tcuenta_debitos_automaticos) AS tcuenta_debitos_automaticos_avg,
	AVG(mcuenta_debitos_automaticos) AS mcuenta_debitos_automaticos_avg,
	AVG(ttarjeta_visa_debitos_automaticos) AS ttarjeta_visa_debitos_automaticos_avg,
	AVG(mttarjeta_visa_debitos_automaticos) AS mttarjeta_visa_debitos_automaticos_avg,
	AVG(ttarjeta_master_debitos_automaticos) AS ttarjeta_master_debitos_automaticos_avg,
	AVG(mttarjeta_master_debitos_automaticos) AS mttarjeta_master_debitos_automaticos_avg,
	AVG(tpagodeservicios) AS tpagodeservicios_avg,
	AVG(mpagodeservicios) AS mpagodeservicios_avg,
	AVG(tpagomiscuentas) AS tpagomiscuentas_avg,
	AVG(mpagomiscuentas) AS mpagomiscuentas_avg,
	AVG(ccajeros_propios_descuentos) AS ccajeros_propios_descuentos_avg,
	AVG(mcajeros_propios_descuentos) AS mcajeros_propios_descuentos_avg,
	AVG(ctarjeta_visa_descuentos) AS ctarjeta_visa_descuentos_avg,
	AVG(mtarjeta_visa_descuentos) AS mtarjeta_visa_descuentos_avg,
	AVG(ctarjeta_master_descuentos) AS ctarjeta_master_descuentos_avg,
	AVG(mtarjeta_master_descuentos) AS mtarjeta_master_descuentos_avg,
	AVG(ccuenta_descuentos) AS ccuenta_descuentos_avg,
	AVG(mcuenta_descuentos) AS mcuenta_descuentos_avg,
	AVG(ccomisiones_mantenimiento) AS ccomisiones_mantenimiento_avg,
	AVG(mcomisiones_mantenimiento) AS mcomisiones_mantenimiento_avg,
	AVG(ccomisiones_otras) AS ccomisiones_otras_avg,
	AVG(mcomisiones_otras) AS mcomisiones_otras_avg,
	AVG(tcambio_monedas) AS tcambio_monedas_avg,
	AVG(ccambio_monedas_compra) AS ccambio_monedas_compra_avg,
	AVG(mcambio_monedas_compra) AS mcambio_monedas_compra_avg,
	AVG(ccambio_monedas_venta) AS ccambio_monedas_venta_avg,
	AVG(mcambio_monedas_venta) AS mcambio_monedas_venta_avg,
	AVG(ctransferencias_recibidas) AS ctransferencias_recibidas_avg,
	AVG(mtransferencias_recibidas) AS mtransferencias_recibidas_avg,
	AVG(ctransferencias_emitidas) AS ctransferencias_emitidas_avg,
	AVG(mtransferencias_emitidas) AS mtransferencias_emitidas_avg,
	AVG(cextraccion_autoservicio) AS cextraccion_autoservicio_avg,
	AVG(mextraccion_autoservicio) AS mextraccion_autoservicio_avg,
	AVG(ccheques_depositados) AS ccheques_depositados_avg,
	AVG(mcheques_depositados) AS mcheques_depositados_avg,
	AVG(ccheques_emitidos) AS ccheques_emitidos_avg,
	AVG(mcheques_emitidos) AS mcheques_emitidos_avg,
	AVG(ccheques_depositados_rechazados) AS ccheques_depositados_rechazados_avg,
	AVG(mcheques_depositados_rechazados) AS mcheques_depositados_rechazados_avg,
	AVG(ccheques_emitidos_rechazados) AS ccheques_emitidos_rechazados_avg,
	AVG(mcheques_emitidos_rechazados) AS mcheques_emitidos_rechazados_avg,
	AVG(tcallcenter) AS tcallcenter_avg,
	AVG(ccallcenter_transacciones) AS ccallcenter_transacciones_avg,
	AVG(thomebanking) AS thomebanking_avg,
	AVG(chomebanking_transacciones) AS chomebanking_transacciones_avg,
	AVG(tautoservicio) AS tautoservicio_avg,
	AVG(cautoservicio_transacciones) AS cautoservicio_transacciones_avg,
	AVG(tcajas) AS tcajas_avg,
	AVG(tcajas_consultas) AS tcajas_consultas_avg,
	AVG(tcajas_depositos) AS tcajas_depositos_avg,
	AVG(tcajas_extracciones) AS tcajas_extracciones_avg,
	AVG(tcajas_otras) AS tcajas_otras_avg,
	AVG(ccajeros_propio_transacciones) AS ccajeros_propio_transacciones_avg,
	AVG(mcajeros_propio) AS mcajeros_propio_avg,
	AVG(ccajeros_ajenos_transacciones) AS ccajeros_ajenos_transacciones_avg,
	AVG(mcajeros_ajenos) AS mcajeros_ajenos_avg,
	AVG(tmovimientos_ultimos90dias) AS tmovimientos_ultimos90dias_avg,
	AVG(Master_marca_atraso) AS Master_marca_atraso_avg,
	AVG(Master_cuenta_estado) AS Master_cuenta_estado_avg,
	AVG(Master_Fvencimiento) AS Master_Fvencimiento_avg,
	AVG(Master_Finiciomora) AS Master_Finiciomora_avg,
	AVG(Master_fultimo_cierre) AS Master_fultimo_cierre_avg,
	AVG(Master_fechaalta) AS Master_fechaalta_avg,
	AVG(Master_tconsumos) AS Master_tconsumos_avg,
	AVG(Master_tadelantosefectivo) AS Master_tadelantosefectivo_avg,
	AVG(Visa_marca_atraso) AS Visa_marca_atraso_avg,
	AVG(Visa_cuenta_estado) AS Visa_cuenta_estado_avg,
	AVG(Visa_Fvencimiento) AS Visa_Fvencimiento_avg,
	AVG(Visa_Finiciomora) AS Visa_Finiciomora_avg,
	AVG(Visa_fultimo_cierre) AS Visa_fultimo_cierre_avg,
	AVG(Visa_fechaalta) AS Visa_fechaalta_avg,
	AVG(Visa_tconsumos) AS Visa_tconsumos_avg,
	AVG(Visa_tadelantosefectivo) AS Visa_tadelantosefectivo_avg,
	AVG(VisaMaster_mfinanciacion_limite) AS VisaMaster_mfinanciacion_limite_avg,
	AVG(VisaMaster_msaldototal) AS VisaMaster_msaldototal_avg,
	AVG(VisaMaster_msaldopesos) AS VisaMaster_msaldopesos_avg,
	AVG(VisaMaster_msaldodolares) AS VisaMaster_msaldodolares_avg,
	AVG(VisaMaster_mconsumototal) AS VisaMaster_mconsumototal_avg,
	AVG(VisaMaster_mconsumospesos) AS VisaMaster_mconsumospesos_avg,
	AVG(VisaMaster_mconsumosdolares) AS VisaMaster_mconsumosdolares_avg,
	AVG(VisaMaster_mlimitecompra) AS VisaMaster_mlimitecompra_avg,
	AVG(VisaMaster_madelantopesos) AS VisaMaster_madelantopesos_avg,
	AVG(VisaMaster_madelantodolares) AS VisaMaster_madelantodolares_avg,
	AVG(VisaMaster_mpagado) AS VisaMaster_mpagado_avg,
	AVG(VisaMaster_mpagospesos) AS VisaMaster_mpagospesos_avg,
	AVG(VisaMaster_mpagosdolares) AS VisaMaster_mpagosdolares_avg,
	AVG(VisaMaster_mpagominimo) AS VisaMaster_mpagominimo_avg,
	AVG(VisaMaster_tconsumos) AS VisaMaster_tconsumos_avg,
	AVG(VisaMaster_tadelantosefectivo) AS VisaMaster_tadelantosefectivo_avg,
	AVG(VisaMaster_marca_atraso) AS VisaMaster_marca_atraso_avg,
	AVG(VisaMaster_cuenta_estado) AS VisaMaster_cuenta_estado_avg,
	AVG(VisaMaster_finiciomora) AS VisaMaster_finiciomora_avg,
	AVG(VisaMaster_mconsumototal_div_mlimitecompra) AS VisaMaster_mconsumototal_div_mlimitecompra_avg,
	MAX(marketing_activo_ultimos90dias) AS marketing_activo_ultimos90dias_max,
	MAX(cliente_vip) AS cliente_vip_max,
	MAX(cliente_sucursal) AS cliente_sucursal_max,
	MAX(cliente_edad) AS cliente_edad_max,
	MAX(cliente_antiguedad) AS cliente_antiguedad_max,
	MAX(mrentabilidad) AS mrentabilidad_max,
	MAX(mrentabilidad_annual) AS mrentabilidad_annual_max,
	MAX(mcomisiones) AS mcomisiones_max,
	MAX(mactivos_margen) AS mactivos_margen_max,
	MAX(mpasivos_margen) AS mpasivos_margen_max,
	MAX(marketing_coss_selling) AS marketing_coss_selling_max,
	MAX(tpaquete1) AS tpaquete1_max,	
	MAX(tpaquete2) AS tpaquete2_max,
	MAX(tpaquete3) AS tpaquete3_max,
	MAX(tpaquete4) AS tpaquete4_max,
	MAX(tpaquete5) AS tpaquete5_max,
	MAX(tpaquete6) AS tpaquete6_max,
	MAX(tpaquete7) AS tpaquete7_max,
	MAX(tpaquete8) AS tpaquete8_max,
	MAX(tpaquete9) AS tpaquete9_max,
	MAX(tcuentas) AS tcuentas_max,
	MAX(tcuenta_corriente) AS tcuenta_corriente_max,
	MAX(mcuenta_corriente_Nopaquete) AS mcuenta_corriente_Nopaquete_max,
	MAX(mcuenta_corriente_Paquete) AS mcuenta_corriente_Paquete_max,
	MAX(mcuenta_corriente_dolares) AS mcuenta_corriente_dolares_max,
	MAX(tcaja_ahorro) AS tcaja_ahorro_max,
	MAX(mcaja_ahorro_Paquete) AS mcaja_ahorro_Paquete_max,
	MAX(mcaja_ahorro_Nopaquete) AS mcaja_ahorro_Nopaquete_max,
	MAX(mcaja_ahorro_dolares) AS mcaja_ahorro_dolares_max,
	MAX(mdescubierto_preacordado) AS mdescubierto_preacordado_max,
	MAX(mcuentas_saldo) AS mcuentas_saldo_max,
	MAX(ttarjeta_debito) AS ttarjeta_debito_max,
	MAX(ctarjeta_debito_transacciones) AS ctarjeta_debito_transacciones_max,
	MAX(mautoservicio) AS mautoservicio_max,
	MAX(ttarjeta_visa) AS ttarjeta_visa_max,
	MAX(ctarjeta_visa_transacciones) AS ctarjeta_visa_transacciones_max,
	MAX(mtarjeta_visa_consumo) AS mtarjeta_visa_consumo_max,
	MAX(ttarjeta_master) AS ttarjeta_master_max,
	MAX(ctarjeta_master_transacciones) AS ctarjeta_master_transacciones_max,
	MAX(mtarjeta_master_consumo) AS mtarjeta_master_consumo_max,
	MAX(cprestamos_personales) AS cprestamos_personales_max,
	MAX(mprestamos_personales) AS mprestamos_personales_max,
	MAX(cprestamos_prendarios) AS cprestamos_prendarios_max,
	MAX(mprestamos_prendarios) AS mprestamos_prendarios_max,
	MAX(cprestamos_hipotecarios) AS cprestamos_hipotecarios_max,
	MAX(mprestamos_hipotecarios) AS mprestamos_hipotecarios_max,
	MAX(tplazo_fijo) AS tplazo_fijo_max,
	MAX(mplazo_fijo_dolares) AS mplazo_fijo_dolares_max,
	MAX(mplazo_fijo_pesos) AS mplazo_fijo_pesos_max,
	MAX(tfondos_comunes_inversion) AS tfondos_comunes_inversion_max,
	MAX(mfondos_comunes_inversion_pesos) AS mfondos_comunes_inversion_pesos_max,
	MAX(mfondos_comunes_inversion_dolares) AS mfondos_comunes_inversion_dolares_max,
	MAX(ttitulos) AS ttitulos_max,
	MAX(mtitulos) AS mtitulos_max,
	MAX(tseguro_vida_mercado_abierto) AS tseguro_vida_mercado_abierto_max,
	MAX(tseguro_auto) AS tseguro_auto_max,
	MAX(tseguro_vivienda) AS tseguro_vivienda_max,
	MAX(tseguro_accidentes_personales) AS tseguro_accidentes_personales_max,
	MAX(tcaja_seguridad) AS tcaja_seguridad_max,
	MAX(mmonedas_extranjeras) AS mmonedas_extranjeras_max,
	MAX(minversiones_otras) AS minversiones_otras_max,
	MAX(tplan_sueldo) AS tplan_sueldo_max,
	MAX(mplan_sueldo) AS mplan_sueldo_max,
	MAX(mplan_sueldo_manual) AS mplan_sueldo_manual_max,
	MAX(cplan_sueldo_transaccion) AS cplan_sueldo_transaccion_max,
	MAX(tcuenta_debitos_automaticos) AS tcuenta_debitos_automaticos_max,
	MAX(mcuenta_debitos_automaticos) AS mcuenta_debitos_automaticos_max,
	MAX(ttarjeta_visa_debitos_automaticos) AS ttarjeta_visa_debitos_automaticos_max,
	MAX(mttarjeta_visa_debitos_automaticos) AS mttarjeta_visa_debitos_automaticos_max,
	MAX(ttarjeta_master_debitos_automaticos) AS ttarjeta_master_debitos_automaticos_max,
	MAX(mttarjeta_master_debitos_automaticos) AS mttarjeta_master_debitos_automaticos_max,
	MAX(tpagodeservicios) AS tpagodeservicios_max,
	MAX(mpagodeservicios) AS mpagodeservicios_max,
	MAX(tpagomiscuentas) AS tpagomiscuentas_max,
	MAX(mpagomiscuentas) AS mpagomiscuentas_max,
	MAX(ccajeros_propios_descuentos) AS ccajeros_propios_descuentos_max,
	MAX(mcajeros_propios_descuentos) AS mcajeros_propios_descuentos_max,
	MAX(ctarjeta_visa_descuentos) AS ctarjeta_visa_descuentos_max,
	MAX(mtarjeta_visa_descuentos) AS mtarjeta_visa_descuentos_max,
	MAX(ctarjeta_master_descuentos) AS ctarjeta_master_descuentos_max,
	MAX(mtarjeta_master_descuentos) AS mtarjeta_master_descuentos_max,
	MAX(ccuenta_descuentos) AS ccuenta_descuentos_max,
	MAX(mcuenta_descuentos) AS mcuenta_descuentos_max,
	MAX(ccomisiones_mantenimiento) AS ccomisiones_mantenimiento_max,
	MAX(mcomisiones_mantenimiento) AS mcomisiones_mantenimiento_max,
	MAX(ccomisiones_otras) AS ccomisiones_otras_max,
	MAX(mcomisiones_otras) AS mcomisiones_otras_max,
	MAX(tcambio_monedas) AS tcambio_monedas_max,
	MAX(ccambio_monedas_compra) AS ccambio_monedas_compra_max,
	MAX(mcambio_monedas_compra) AS mcambio_monedas_compra_max,
	MAX(ccambio_monedas_venta) AS ccambio_monedas_venta_max,
	MAX(mcambio_monedas_venta) AS mcambio_monedas_venta_max,
	MAX(ctransferencias_recibidas) AS ctransferencias_recibidas_max,
	MAX(mtransferencias_recibidas) AS mtransferencias_recibidas_max,
	MAX(ctransferencias_emitidas) AS ctransferencias_emitidas_max,
	MAX(mtransferencias_emitidas) AS mtransferencias_emitidas_max,
	MAX(cextraccion_autoservicio) AS cextraccion_autoservicio_max,
	MAX(mextraccion_autoservicio) AS mextraccion_autoservicio_max,
	MAX(ccheques_depositados) AS ccheques_depositados_max,
	MAX(mcheques_depositados) AS mcheques_depositados_max,
	MAX(ccheques_emitidos) AS ccheques_emitidos_max,
	MAX(mcheques_emitidos) AS mcheques_emitidos_max,
	MAX(ccheques_depositados_rechazados) AS ccheques_depositados_rechazados_max,
	MAX(mcheques_depositados_rechazados) AS mcheques_depositados_rechazados_max,
	MAX(ccheques_emitidos_rechazados) AS ccheques_emitidos_rechazados_max,
	MAX(mcheques_emitidos_rechazados) AS mcheques_emitidos_rechazados_max,
	MAX(tcallcenter) AS tcallcenter_max,
	MAX(ccallcenter_transacciones) AS ccallcenter_transacciones_max,
	MAX(thomebanking) AS thomebanking_max,
	MAX(chomebanking_transacciones) AS chomebanking_transacciones_max,
	MAX(tautoservicio) AS tautoservicio_max,
	MAX(cautoservicio_transacciones) AS cautoservicio_transacciones_max,
	MAX(tcajas) AS tcajas_max,
	MAX(tcajas_consultas) AS tcajas_consultas_max,
	MAX(tcajas_depositos) AS tcajas_depositos_max,
	MAX(tcajas_extracciones) AS tcajas_extracciones_max,
	MAX(tcajas_otras) AS tcajas_otras_max,
	MAX(ccajeros_propio_transacciones) AS ccajeros_propio_transacciones_max,
	MAX(mcajeros_propio) AS mcajeros_propio_max,
	MAX(ccajeros_ajenos_transacciones) AS ccajeros_ajenos_transacciones_max,
	MAX(mcajeros_ajenos) AS mcajeros_ajenos_max,
	MAX(tmovimientos_ultimos90dias) AS tmovimientos_ultimos90dias_max,
	MAX(Master_marca_atraso) AS Master_marca_atraso_max,
	MAX(Master_cuenta_estado) AS Master_cuenta_estado_max,
	MAX(Master_Fvencimiento) AS Master_Fvencimiento_max,
	MAX(Master_Finiciomora) AS Master_Finiciomora_max,
	MAX(Master_fultimo_cierre) AS Master_fultimo_cierre_max,
	MAX(Master_fechaalta) AS Master_fechaalta_max,
	MAX(Master_tconsumos) AS Master_tconsumos_max,
	MAX(Master_tadelantosefectivo) AS Master_tadelantosefectivo_max,
	MAX(Visa_marca_atraso) AS Visa_marca_atraso_max,
	MAX(Visa_cuenta_estado) AS Visa_cuenta_estado_max,
	MAX(Visa_Fvencimiento) AS Visa_Fvencimiento_max,
	MAX(Visa_Finiciomora) AS Visa_Finiciomora_max,
	MAX(Visa_fultimo_cierre) AS Visa_fultimo_cierre_max,
	MAX(Visa_fechaalta) AS Visa_fechaalta_max,
	MAX(Visa_tconsumos) AS Visa_tconsumos_max,
	MAX(Visa_tadelantosefectivo) AS Visa_tadelantosefectivo_max,
	MAX(VisaMaster_mfinanciacion_limite) AS VisaMaster_mfinanciacion_limite_max,
	MAX(VisaMaster_msaldototal) AS VisaMaster_msaldototal_max,
	MAX(VisaMaster_msaldopesos) AS VisaMaster_msaldopesos_max,
	MAX(VisaMaster_msaldodolares) AS VisaMaster_msaldodolares_max,
	MAX(VisaMaster_mconsumototal) AS VisaMaster_mconsumototal_max,
	MAX(VisaMaster_mconsumospesos) AS VisaMaster_mconsumospesos_max,
	MAX(VisaMaster_mconsumosdolares) AS VisaMaster_mconsumosdolares_max,
	MAX(VisaMaster_mlimitecompra) AS VisaMaster_mlimitecompra_max,
	MAX(VisaMaster_madelantopesos) AS VisaMaster_madelantopesos_max,
	MAX(VisaMaster_madelantodolares) AS VisaMaster_madelantodolares_max,
	MAX(VisaMaster_mpagado) AS VisaMaster_mpagado_max,
	MAX(VisaMaster_mpagospesos) AS VisaMaster_mpagospesos_max,
	MAX(VisaMaster_mpagosdolares) AS VisaMaster_mpagosdolares_max,
	MAX(VisaMaster_mpagominimo) AS VisaMaster_mpagominimo_max,
	MAX(VisaMaster_tconsumos) AS VisaMaster_tconsumos_max,
	MAX(VisaMaster_tadelantosefectivo) AS VisaMaster_tadelantosefectivo_max,
	MAX(VisaMaster_marca_atraso) AS VisaMaster_marca_atraso_max,
	MAX(VisaMaster_cuenta_estado) AS VisaMaster_cuenta_estado_max,
	MAX(VisaMaster_finiciomora) AS VisaMaster_finiciomora_max,
	MAX(VisaMaster_mconsumototal_div_mlimitecompra) AS VisaMaster_mconsumototal_div_mlimitecompra_max,
	MIN(marketing_activo_ultimos90dias) AS marketing_activo_ultimos90dias_min,
	MIN(cliente_vip) AS cliente_vip_min,
	MIN(cliente_sucursal) AS cliente_sucursal_min,
	MIN(cliente_edad) AS cliente_edad_min,
	MIN(cliente_antiguedad) AS cliente_antiguedad_min,
	MIN(mrentabilidad) AS mrentabilidad_min,
	MIN(mrentabilidad_annual) AS mrentabilidad_annual_min,
	MIN(mcomisiones) AS mcomisiones_min,
	MIN(mactivos_margen) AS mactivos_margen_min,
	MIN(mpasivos_margen) AS mpasivos_margen_min,
	MIN(marketing_coss_selling) AS marketing_coss_selling_min,
	MIN(tpaquete1) AS tpaquete1_min,
	MIN(tpaquete2) AS tpaquete2_min,
	MIN(tpaquete3) AS tpaquete3_min,
	MIN(tpaquete4) AS tpaquete4_min,
	MIN(tpaquete5) AS tpaquete5_min,
	MIN(tpaquete6) AS tpaquete6_min,
	MIN(tpaquete7) AS tpaquete7_min,
	MIN(tpaquete8) AS tpaquete8_min,
	MIN(tpaquete9) AS tpaquete9_min,
	MIN(tcuentas) AS tcuentas_min,
	MIN(tcuenta_corriente) AS tcuenta_corriente_min,
	MIN(mcuenta_corriente_Nopaquete) AS mcuenta_corriente_Nopaquete_min,
	MIN(mcuenta_corriente_Paquete) AS mcuenta_corriente_Paquete_min,
	MIN(mcuenta_corriente_dolares) AS mcuenta_corriente_dolares_min,
	MIN(tcaja_ahorro) AS tcaja_ahorro_min,
	MIN(mcaja_ahorro_Paquete) AS mcaja_ahorro_Paquete_min,
	MIN(mcaja_ahorro_Nopaquete) AS mcaja_ahorro_Nopaquete_min,
	MIN(mcaja_ahorro_dolares) AS mcaja_ahorro_dolares_min,
	MIN(mdescubierto_preacordado) AS mdescubierto_preacordado_min,
	MIN(mcuentas_saldo) AS mcuentas_saldo_min,
	MIN(ttarjeta_debito) AS ttarjeta_debito_min,
	MIN(ctarjeta_debito_transacciones) AS ctarjeta_debito_transacciones_min,
	MIN(mautoservicio) AS mautoservicio_min,
	MIN(ttarjeta_visa) AS ttarjeta_visa_min,
	MIN(ctarjeta_visa_transacciones) AS ctarjeta_visa_transacciones_min,
	MIN(mtarjeta_visa_consumo) AS mtarjeta_visa_consumo_min,
	MIN(ttarjeta_master) AS ttarjeta_master_min,
	MIN(ctarjeta_master_transacciones) AS ctarjeta_master_transacciones_min,
	MIN(mtarjeta_master_consumo) AS mtarjeta_master_consumo_min,
	MIN(cprestamos_personales) AS cprestamos_personales_min,
	MIN(mprestamos_personales) AS mprestamos_personales_min,
	MIN(cprestamos_prendarios) AS cprestamos_prendarios_min,
	MIN(mprestamos_prendarios) AS mprestamos_prendarios_min,
	MIN(cprestamos_hipotecarios) AS cprestamos_hipotecarios_min,
	MIN(mprestamos_hipotecarios) AS mprestamos_hipotecarios_min,
	MIN(tplazo_fijo) AS tplazo_fijo_min,
	MIN(mplazo_fijo_dolares) AS mplazo_fijo_dolares_min,
	MIN(mplazo_fijo_pesos) AS mplazo_fijo_pesos_min,
	MIN(tfondos_comunes_inversion) AS tfondos_comunes_inversion_min,
	MIN(mfondos_comunes_inversion_pesos) AS mfondos_comunes_inversion_pesos_min,
	MIN(mfondos_comunes_inversion_dolares) AS mfondos_comunes_inversion_dolares_min,
	MIN(ttitulos) AS ttitulos_min,
	MIN(mtitulos) AS mtitulos_min,
	MIN(tseguro_vida_mercado_abierto) AS tseguro_vida_mercado_abierto_min,
	MIN(tseguro_auto) AS tseguro_auto_min,
	MIN(tseguro_vivienda) AS tseguro_vivienda_min,
	MIN(tseguro_accidentes_personales) AS tseguro_accidentes_personales_min,
	MIN(tcaja_seguridad) AS tcaja_seguridad_min,
	MIN(mmonedas_extranjeras) AS mmonedas_extranjeras_min,
	MIN(minversiones_otras) AS minversiones_otras_min,
	MIN(tplan_sueldo) AS tplan_sueldo_min,
	MIN(mplan_sueldo) AS mplan_sueldo_min,
	MIN(mplan_sueldo_manual) AS mplan_sueldo_manual_min,
	MIN(cplan_sueldo_transaccion) AS cplan_sueldo_transaccion_min,
	MIN(tcuenta_debitos_automaticos) AS tcuenta_debitos_automaticos_min,
	MIN(mcuenta_debitos_automaticos) AS mcuenta_debitos_automaticos_min,
	MIN(ttarjeta_visa_debitos_automaticos) AS ttarjeta_visa_debitos_automaticos_min,
	MIN(mttarjeta_visa_debitos_automaticos) AS mttarjeta_visa_debitos_automaticos_min,
	MIN(ttarjeta_master_debitos_automaticos) AS ttarjeta_master_debitos_automaticos_min,
	MIN(mttarjeta_master_debitos_automaticos) AS mttarjeta_master_debitos_automaticos_min,
	MIN(tpagodeservicios) AS tpagodeservicios_min,
	MIN(mpagodeservicios) AS mpagodeservicios_min,
	MIN(tpagomiscuentas) AS tpagomiscuentas_min,
	MIN(mpagomiscuentas) AS mpagomiscuentas_min,
	MIN(ccajeros_propios_descuentos) AS ccajeros_propios_descuentos_min,
	MIN(mcajeros_propios_descuentos) AS mcajeros_propios_descuentos_min,
	MIN(ctarjeta_visa_descuentos) AS ctarjeta_visa_descuentos_min,
	MIN(mtarjeta_visa_descuentos) AS mtarjeta_visa_descuentos_min,
	MIN(ctarjeta_master_descuentos) AS ctarjeta_master_descuentos_min,
	MIN(mtarjeta_master_descuentos) AS mtarjeta_master_descuentos_min,
	MIN(ccuenta_descuentos) AS ccuenta_descuentos_min,
	MIN(mcuenta_descuentos) AS mcuenta_descuentos_min,
	MIN(ccomisiones_mantenimiento) AS ccomisiones_mantenimiento_min,
	MIN(mcomisiones_mantenimiento) AS mcomisiones_mantenimiento_min,
	MIN(ccomisiones_otras) AS ccomisiones_otras_min,
	MIN(mcomisiones_otras) AS mcomisiones_otras_min,
	MIN(tcambio_monedas) AS tcambio_monedas_min,
	MIN(ccambio_monedas_compra) AS ccambio_monedas_compra_min,
	MIN(mcambio_monedas_compra) AS mcambio_monedas_compra_min,
	MIN(ccambio_monedas_venta) AS ccambio_monedas_venta_min,
	MIN(mcambio_monedas_venta) AS mcambio_monedas_venta_min,
	MIN(ctransferencias_recibidas) AS ctransferencias_recibidas_min,
	MIN(mtransferencias_recibidas) AS mtransferencias_recibidas_min,
	MIN(ctransferencias_emitidas) AS ctransferencias_emitidas_min,
	MIN(mtransferencias_emitidas) AS mtransferencias_emitidas_min,
	MIN(cextraccion_autoservicio) AS cextraccion_autoservicio_min,
	MIN(mextraccion_autoservicio) AS mextraccion_autoservicio_min,
	MIN(ccheques_depositados) AS ccheques_depositados_min,
	MIN(mcheques_depositados) AS mcheques_depositados_min,
	MIN(ccheques_emitidos) AS ccheques_emitidos_min,
	MIN(mcheques_emitidos) AS mcheques_emitidos_min,
	MIN(ccheques_depositados_rechazados) AS ccheques_depositados_rechazados_min,
	MIN(mcheques_depositados_rechazados) AS mcheques_depositados_rechazados_min,
	MIN(ccheques_emitidos_rechazados) AS ccheques_emitidos_rechazados_min,
	MIN(mcheques_emitidos_rechazados) AS mcheques_emitidos_rechazados_min,
	MIN(tcallcenter) AS tcallcenter_min,
	MIN(ccallcenter_transacciones) AS ccallcenter_transacciones_min,
	MIN(thomebanking) AS thomebanking_min,
	MIN(chomebanking_transacciones) AS chomebanking_transacciones_min,
	MIN(tautoservicio) AS tautoservicio_min,
	MIN(cautoservicio_transacciones) AS cautoservicio_transacciones_min,
	MIN(tcajas) AS tcajas_min,
	MIN(tcajas_consultas) AS tcajas_consultas_min,
	MIN(tcajas_depositos) AS tcajas_depositos_min,
	MIN(tcajas_extracciones) AS tcajas_extracciones_min,
	MIN(tcajas_otras) AS tcajas_otras_min,
	MIN(ccajeros_propio_transacciones) AS ccajeros_propio_transacciones_min,
	MIN(mcajeros_propio) AS mcajeros_propio_min,
	MIN(ccajeros_ajenos_transacciones) AS ccajeros_ajenos_transacciones_min,
	MIN(mcajeros_ajenos) AS mcajeros_ajenos_min,
	MIN(tmovimientos_ultimos90dias) AS tmovimientos_ultimos90dias_min,
	MIN(Master_marca_atraso) AS Master_marca_atraso_min,
	MIN(Master_cuenta_estado) AS Master_cuenta_estado_min,
	MIN(Master_Fvencimiento) AS Master_Fvencimiento_min,
	MIN(Master_Finiciomora) AS Master_Finiciomora_min,
	MIN(Master_fultimo_cierre) AS Master_fultimo_cierre_min,
	MIN(Master_fechaalta) AS Master_fechaalta_min,
	MIN(Master_tconsumos) AS Master_tconsumos_min,
	MIN(Master_tadelantosefectivo) AS Master_tadelantosefectivo_min,
	MIN(Visa_marca_atraso) AS Visa_marca_atraso_min,
	MIN(Visa_cuenta_estado) AS Visa_cuenta_estado_min,
	MIN(Visa_Fvencimiento) AS Visa_Fvencimiento_min,
	MIN(Visa_Finiciomora) AS Visa_Finiciomora_min,
	MIN(Visa_fultimo_cierre) AS Visa_fultimo_cierre_min,
	MIN(Visa_fechaalta) AS Visa_fechaalta_min,
	MIN(Visa_tconsumos) AS Visa_tconsumos_min,
	MIN(Visa_tadelantosefectivo) AS Visa_tadelantosefectivo_min,
	MIN(VisaMaster_mfinanciacion_limite) AS VisaMaster_mfinanciacion_limite_min,
	MIN(VisaMaster_msaldototal) AS VisaMaster_msaldototal_min,
	MIN(VisaMaster_msaldopesos) AS VisaMaster_msaldopesos_min,
	MIN(VisaMaster_msaldodolares) AS VisaMaster_msaldodolares_min,
	MIN(VisaMaster_mconsumototal) AS VisaMaster_mconsumototal_min,
	MIN(VisaMaster_mconsumospesos) AS VisaMaster_mconsumospesos_min,
	MIN(VisaMaster_mconsumosdolares) AS VisaMaster_mconsumosdolares_min,
	MIN(VisaMaster_mlimitecompra) AS VisaMaster_mlimitecompra_min,
	MIN(VisaMaster_madelantopesos) AS VisaMaster_madelantopesos_min,
	MIN(VisaMaster_madelantodolares) AS VisaMaster_madelantodolares_min,
	MIN(VisaMaster_mpagado) AS VisaMaster_mpagado_min,
	MIN(VisaMaster_mpagospesos) AS VisaMaster_mpagospesos_min,
	MIN(VisaMaster_mpagosdolares) AS VisaMaster_mpagosdolares_min,
	MIN(VisaMaster_mpagominimo) AS VisaMaster_mpagominimo_min,
	MIN(VisaMaster_tconsumos) AS VisaMaster_tconsumos_min,
	MIN(VisaMaster_tadelantosefectivo) AS VisaMaster_tadelantosefectivo_min,
	MIN(VisaMaster_marca_atraso) AS VisaMaster_marca_atraso_min,
	MIN(VisaMaster_cuenta_estado) AS VisaMaster_cuenta_estado_min,
	MIN(VisaMaster_finiciomora) AS VisaMaster_finiciomora_min,
	MIN(VisaMaster_mconsumototal_div_mlimitecompra) AS VisaMaster_mconsumototal_div_mlimitecompra_min
from
    visamaster
group by
    numero_de_cliente
;


ALTER TABLE data_visamaster ADD COLUMN VisaMaster_mconsumototal_div_mlimitecompra double;

update
    data_visamaster
set
    VisaMaster_mconsumototal_div_mlimitecompra = case VisaMaster_mlimitecompra when null or 0 then null else (VisaMaster_mconsumototal / VisaMaster_mlimitecompra) end

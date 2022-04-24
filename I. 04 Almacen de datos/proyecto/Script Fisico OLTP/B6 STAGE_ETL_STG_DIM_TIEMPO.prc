CREATE OR REPLACE PROCEDURE ETL_STG_DIM_TIEMPO
 as
  V_NOMBRE_PROCESO   VARCHAR2(30):= 'ETL_STG_DIM_TIEMPO';
  V_FEC_INICIO       DATE;
  V_FEC_FIN          DATE;
  V_COMENTARIO       VARCHAR2(255);
  V_CANT_REG         NUMBER(10)  := 0;
  V_CORRECTO         VARCHAR2(1) := 'N'; -- INDICADOR DE QUE SI EL PROCESO ESTA CORRECTO O NO
  V_VFILENAME        VARCHAR2(30);
 -- V_FONO_SMS         VARCHAR2(10) := '603060700';
V_FECHA_MIN DATE;
V_FECHA_MAX DATE;
V_COUNT number;
v_cantidad_dias number;

begin
v_fec_inicio := SYSDATE;
v_count:=0 ;

select max(orderdate)+1 fecha_max ,min(orderdate) fecha_min ,max(orderdate)- min(orderdate) cantidad_dias
INTO V_FECHA_MAX,V_FECHA_MIN,v_cantidad_dias
from pedidos.orders; 


execute immediate 'TRUNCATE TABLE STG_DIM_TIEMPO';

FOR V_COUNT IN 0..v_cantidad_dias
LOOP

INSERT INTO STG_DIM_TIEMPO 
select trunc(V_FECHA_MIN + V_COUNT) fecha
,to_number(to_char(V_FECHA_MIN + V_COUNT,'dd')) dia

,to_char(V_FECHA_MIN + V_COUNT,'Day') dia_semena
,DECODE( substr(to_char(V_FECHA_MIN + V_COUNT+1,'Day'),1,3),'Sun','No','Sat','No','Si') dia_laboral
,decode(trunc(V_FECHA_MIN + V_COUNT)+10 , to_date('20130501','yyyymmdd'),'Si','No') dia_feriado 

,to_number(to_char(V_FECHA_MIN + V_COUNT,'mm')) mes
,to_char(V_FECHA_MIN + V_COUNT,'Mon') mes_cadena
,to_char(V_FECHA_MIN + V_COUNT,'yyyymm') periodo
,to_char(V_FECHA_MIN + V_COUNT,'Q') trimestre
, to_char(V_FECHA_MIN + V_COUNT,'Q') semestre
,to_number(to_char(V_FECHA_MIN + V_COUNT,'yyyy')) anio
from  dual;
commit; 

END LOOP;
 -- FIN CODIGO DEL PROCESO
 v_cant_reg:=v_cantidad_dias;
  v_fec_fin    := SYSDATE;
  v_comentario := 'EL PROCESO '||v_nombre_proceso||' fue un exito';
  v_correcto   := 'S';

  P_Insertar_Info_Proc(v_nombre_proceso,
                       v_fec_inicio,
                       v_fec_fin,
                       v_comentario,
                       v_cant_reg,
                       v_correcto );
  COMMIT;
  
exception when others then 

    v_fec_fin    := V_FECHA_MIN + V_COUNT;
     v_comentario :=  ('ERROR AL ACTUALIZAR '||v_nombre_proceso||' '||SQLCODE||' '||SQLERRM);
     P_Insertar_Info_Proc(v_nombre_proceso,
                          v_fec_inicio,
                          v_fec_fin,
                          v_comentario,
                          v_cant_reg,
                          v_correcto )                        ;
     COMMIT  ;
 

end;

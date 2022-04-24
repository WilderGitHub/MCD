CREATE OR REPLACE PROCEDURE ETL_FACT_PEDIDOS IS -- (PI_FECHA_INICIAL VARCHAR2, PI_FECHA_FINAL VARCHAR2)  IS

  V_NOMBRE_PROCESO   VARCHAR2(30):= 'ETL_FACT_PEDIDOS';
  V_FEC_INICIO       DATE;
  V_FEC_FIN          DATE;
  V_COMENTARIO       VARCHAR2(255);
  V_CANT_REG         NUMBER(10)  := 0;
  V_CORRECTO         VARCHAR2(1) := 'N'; 

  -- VARIABLES DEL PROCESO
  V_FECHA_INICIO DATE;
  V_FECHA_FIN DATE;
  v_total_diferencias    NUMBER(10) := 0;

BEGIN
  v_fec_inicio := SYSDATE;
  
  --V_FECHA_INICIO :=TO_DATE(PI_FECHA_INICIAL,'YYYYMMDD');
  --V_FECHA_FIN    :=TO_DATE(PI_FECHA_FINAL,'YYYYMMDD');
  -- CODIGO DEL PROCESO

--   EXECUTE IMMEDIATE 'TRUNCATE TABLE  STAR.FACT_VENTA PARTITION(P20191019)  '
    DELETE FROM PSTAR.FACT_PEDIDOS
    --WHERE FECHA BETWEEN V_FECHA_INICIO AND V_FECHA_FIN;
    ;
    commit;
-- EXECUTE IMMEDIATE ('ALTER TABLE FACT_VENTAS TRUNCATE PARTITION P' || PI_FECHA_INICIAL ) ; 
    INSERT INTO PSTAR.FACT_PEDIDOS (ORDERID,IDW_CLIENTE,CUSTOMERID,IDW_EMPLEADO,EMPLOYEEID
        ,IDW_PRODUCTO,ORDERDATE,REQUIREDDATE,SHIPPEDDATE
        ,IDW_SHIPPER,SHIPVIA,SHIPNAME,FREIGHT
        ,IDW_GEOGRAFIA,UNITPRICE,QUANTITY,DISCOUNT,FECHA_CARGA)

    SELECT  ORDERID,IDW_CLIENTE,CUSTOMERID,IDW_EMPLEADO,EMPLOYEEID
        ,IDW_PRODUCTO,ORDERDATE,REQUIREDDATE,SHIPPEDDATE
        ,IDW_SHIPPER,SHIPVIA,SHIPNAME,FREIGHT
        ,IDW_GEOGRAFIA,UNITPRICE,QUANTITY,DISCOUNT,FECHA_CARGA

    FROM PSTAGE.STG_FACT_PEDIDOS;
    
    V_CANT_REG:= SQL%ROWCOUNT;
    COMMIT;
    
  v_fec_fin    := SYSDATE;
  v_comentario := 'EL PROCESO '||v_nombre_proceso||' CULMINO SATISFACTORIAMENTE ';
  v_correcto   := 'S';

  P_Insertar_Info_Proc(v_nombre_proceso,
                       v_fec_inicio,
                       v_fec_fin,
                       v_comentario,
                       v_cant_reg,
                       v_correcto )
                       ;
  COMMIT;

EXCEPTION
   WHEN OTHERS THEN
     v_fec_fin    := SYSDATE;
     v_comentario :=  ('ERROR ACTUALIZANDO '||v_nombre_proceso||' '||SQLCODE||' '||SQLERRM);
     P_Insertar_Info_Proc(v_nombre_proceso,
                          v_fec_inicio,
                          v_fec_fin,
                          v_comentario,
                          v_cant_reg,
                          v_correcto )
                          ;
     COMMIT  ;



END;
/

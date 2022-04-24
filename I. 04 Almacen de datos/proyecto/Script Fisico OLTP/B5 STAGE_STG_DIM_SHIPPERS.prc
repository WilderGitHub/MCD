CREATE OR REPLACE PROCEDURE ETL_STG_DIM_SHIPPERS IS

  V_NOMBRE_PROCESO   VARCHAR2(30):= 'ETL_STG_DIM_SHIPPERS';
  V_FEC_INICIO       DATE;
  V_FEC_FIN          DATE;
  V_COMENTARIO       VARCHAR2(255);
  V_CANT_REG         NUMBER(10)  := 0;
  V_CORRECTO         VARCHAR2(1) := 'N'; 

  v_total_diferencias    NUMBER(10) := 0;

BEGIN
  v_fec_inicio := SYSDATE;

  -- CODIGO DEL PROCESO
 
    EXECUTE IMMEDIATE 'TRUNCATE TABLE STG_DIM_SHIPPERS';
    INSERT INTO STG_DIM_SHIPPERS (IDW_SHIPPER,SHIPPERID,SHIPPERS,PHONE
                ,LOG_USER,START_DATE,END_DATE,CURRENT_FLAG,PK_SYSTEM_SOURCE,SYSTEM_SOURCE)
    
    SELECT NVL( (SELECT IDW_SHIPPER
                  FROM PSTAR.DIM_SHIPPERS 
                    WHERE SHIPPERID  = S.SHIPPERID
                    ) ,-1) IDW_SHIPPER
                    ,S.SHIPPERID,S.COMPANYNAME,S.PHONE
                    ,SUBSTR(Sys_Context('USERENV','OS_USER'),1,10) LOG_USER
                    , SYSDATE START_DATE, SYSDATE END_DATE, 1 CURRENT_FLAG, 1 PK_SYSTEM_SOURCE, 'A' SYSTEM_SOURCE
                          
    FROM STG_SHIPPERS S
        --WHERE  CUSTOMER=P.PEDIDOS.CUSTOMERID 
    ;
     V_CANT_REG := SQL%ROWCOUNT;
    COMMIT;
    
 
    
    
  -- FIN CODIGO DEL PROCESO

  v_fec_fin    := SYSDATE;
  v_comentario := 'EL PROCESO '||v_nombre_proceso||' FUE EXITOSO';
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
     v_comentario :=  ('ERROR AL EJECUTAR '||v_nombre_proceso||' '||SQLCODE||' '||SQLERRM);
     P_Insertar_Info_Proc(v_nombre_proceso,
                          v_fec_inicio,
                          v_fec_fin,
                          v_comentario,
                          v_cant_reg,
                          v_correcto )
                          ;
     COMMIT  ;



END;





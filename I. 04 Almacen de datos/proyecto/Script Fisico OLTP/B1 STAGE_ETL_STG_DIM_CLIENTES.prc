CREATE OR REPLACE PROCEDURE ETL_STG_DIM_CLIENTES IS

  V_NOMBRE_PROCESO   VARCHAR2(30):= 'ETL_STG_DIM_CLIENTES';
  V_FEC_INICIO       DATE;
  V_FEC_FIN          DATE;
  V_COMENTARIO       VARCHAR2(255);
  V_CANT_REG         NUMBER(10)  := 0;
  V_CORRECTO         VARCHAR2(1) := 'N'; 

  v_total_diferencias    NUMBER(10) := 0;

BEGIN
  v_fec_inicio := SYSDATE;

  -- CODIGO DEL PROCESO
 
    EXECUTE IMMEDIATE 'TRUNCATE TABLE STG_DIM_CLIENTES';
    INSERT INTO STG_DIM_CLIENTES (IDW_CLIENTE, CUSTOMERID, CUSTOMER, CUSTOMER_CONTACT, CONTACTTITLE, ADDRESS, CITY, REGION, POSTALCODE, COUNTRY, PHONE, FAX, LOG_USER, START_DATE, END_DATE, CURRENT_FLAG, PK_SYSTEM_SOURCE, SYSTEM_SOURCE)
    --INSERT INTO STG_DIM_CLIENTES (IDW_CLIENTES, ID_CLIENTE, NOMBRE, AP_PATERNO, AP_MATERNO, CI, ID_SUBRUBRO, SUBRUBRO, ID_RUBRO, RUBRO, ID_CANAL_SISTEMA, START_DATE, END_DATE, FLAG, USER_DW)
    
    SELECT NVL( (SELECT IDW_CLIENTE
                  FROM PSTAR.DIM_CLIENTES 
                    WHERE CUSTOMERID  = P.CUSTOMERID
                    ) ,-1) IDW_CLIENTE
                    , P.CUSTOMERID, P.COMPANYNAME, P.CONTACTNAME, P.CONTACTTITLE, P.ADDRESS, P.CITY, P.REGION, P.POSTALCODE, P.COUNTRY, P.PHONE, P.FAX
                    ,SUBSTR(Sys_Context('USERENV','OS_USER'),1,10) LOG_USER
                    , SYSDATE START_DATE, SYSDATE END_DATE, 1 CURRENT_FLAG, 1 PK_SYSTEM_SOURCE, 'A' SYSTEM_SOURCE
                          
    FROM STG_CUSTOMERS P
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





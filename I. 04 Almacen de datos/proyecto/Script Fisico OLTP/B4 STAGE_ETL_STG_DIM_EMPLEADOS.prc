CREATE OR REPLACE PROCEDURE ETL_STG_DIM_EMPLEADOS IS

  V_NOMBRE_PROCESO   VARCHAR2(30):= 'ETL_STG_DIM_EMPLEADOS';
  V_FEC_INICIO       DATE;
  V_FEC_FIN          DATE;
  V_COMENTARIO       VARCHAR2(255);
  V_CANT_REG         NUMBER(10)  := 0;
  V_CORRECTO         VARCHAR2(1) := 'N'; 

  v_total_diferencias    NUMBER(10) := 0;

BEGIN
  v_fec_inicio := SYSDATE;

  -- CODIGO DEL PROCESO
 
    EXECUTE IMMEDIATE 'TRUNCATE TABLE STG_DIM_EMPLEADOS';
    INSERT INTO STG_DIM_EMPLEADOS (IDW_EMPLEADO,EMPLOYEEID,LASTNAME,FIRSTNAME,TITLE,TITLEOFCOURTESY,BIRTHDATE,HIREDATE
    ,ADDRESS,CITY,REGION,POSTALCODE,COUNTRY,HOMEPHONE,EXTENSION,REPORTSTO,LOG_USER,START_DATE,END_DATE,CURRENT_FLAG,PK_SYSTEM_SOURCE,SYSTEM_SOURCE)
    
    SELECT  NVL((SELECT IDW_EMPLEADO
                 FROM PSTAR.DIM_EMPLEADOS 
                 WHERE EMPLOYEEID  = E.EMPLOYEEID) ,-1) IDW_EMPLEADO
                 ,E.EMPLOYEEID,E.LASTNAME,E.FIRSTNAME,E.TITLE,E.TITLEOFCOURTESY,E.BIRTHDATE,E.HIREDATE,E.ADDRESS,E.CITY,E.REGION,E.POSTALCODE,E.COUNTRY
                 ,E.HOMEPHONE,E.EXTENSION,E.REPORTSTO
                ,SUBSTR(Sys_Context('USERENV','OS_USER'),1,10) LOG_USER
                 ,SYSDATE START_DATE, SYSDATE END_DATE, 1 CURRENT_FLAG, 1 PK_SYSTEM_SOURCE, 'A' SYSTEM_SOURCE
                          
    FROM STG_EMPLOYEES E
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





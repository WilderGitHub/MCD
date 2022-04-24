CREATE OR REPLACE PROCEDURE ETL_DIM_CLIENTES IS

  V_NOMBRE_PROCESO   VARCHAR2(30):= 'ETL_DIM_CLIENTES';
  V_FEC_INICIO       DATE;
  V_FEC_FIN          DATE;
  V_COMENTARIO       VARCHAR2(255);
  V_CANT_REG         NUMBER(10)  := 0;
  V_CORRECTO         VARCHAR2(1) := 'N';

  v_total_diferencias    NUMBER(10) := 0;

BEGIN
  v_fec_inicio := SYSDATE;

  -- CODIGO DEL PROCESO
      
    FOR V_REG IN ( SELECT IDW_CLIENTE,CUSTOMERID,CUSTOMER,CUSTOMER_CONTACT,CONTACTTITLE,ADDRESS,CITY,REGION,POSTALCODE,COUNTRY,PHONE,FAX
                   FROM STG_DIM_CLIENTES
                   MINUS
                   SELECT IDW_CLIENTE,CUSTOMERID,CUSTOMER,CUSTOMER_CONTACT,CONTACTTITLE,ADDRESS,CITY,REGION,POSTALCODE,COUNTRY,PHONE,FAX
                   FROM PSTAR.DIM_CLIENTES)
LOOP
    
         IF V_REG.IDW_CLIENTE !='-1' THEN
    
            UPDATE  PSTAR.DIM_CLIENTES
            SET  CUSTOMERID = V_REG.CUSTOMERID
            ,CUSTOMER = V_REG.CUSTOMER
            ,CUSTOMER_CONTACT = V_REG.CUSTOMER_CONTACT
            ,CONTACTTITLE = V_REG.CONTACTTITLE
            
            ,ADDRESS= V_REG.ADDRESS
            ,CITY= V_REG.CITY
            ,REGION= V_REG.REGION
            ,POSTALCODE= V_REG.POSTALCODE
            ,COUNTRY= V_REG.COUNTRY
            ,PHONE= V_REG.PHONE
            ,FAX= V_REG.FAX
            
            WHERE IDW_CLIENTE = V_REG.IDW_CLIENTE;

            COMMIT;
            V_TOTAL_DIFERENCIAS := V_TOTAL_DIFERENCIAS+1;
       END IF;
    
END LOOP ;



/************ SI NO EXISTE DIFERENCIAS SIGNIFICA QUE EXISTEN NUEVOS REGISTROS ***/

    INSERT INTO PSTAR.DIM_CLIENTES (IDW_CLIENTE,CUSTOMERID,CUSTOMER,CUSTOMER_CONTACT,CONTACTTITLE,ADDRESS,CITY,REGION,POSTALCODE,COUNTRY,
                                PHONE,FAX,LOG_USER,START_DATE,END_DATE,CURRENT_FLAG,PK_SYSTEM_SOURCE,SYSTEM_SOURCE)
    SELECT SEQ_IDW_CUSTOMER.NEXTVAL IDW_CLIENTE,CUSTOMERID,CUSTOMER,CUSTOMER_CONTACT,CONTACTTITLE,ADDRESS,CITY,REGION,POSTALCODE,COUNTRY,
                                PHONE,FAX,SUBSTR(Sys_Context('USERENV','OS_USER'),1,10) LOG_USER,SYSDATE START_DATE,SYSDATE END_DATE,1 CURRENT_FLAG,1 PK_SYSTEM_SOURCE,'A' SYSTEM_SOURCE
                
    FROM STG_DIM_CLIENTES
    WHERE IDW_CLIENTE = -1;
    V_CANT_REG := SQL%ROWCOUNT; 
    COMMIT;
    

    
  -- FIN CODIGO DEL PROCESO

  v_fec_fin    := SYSDATE;
  v_comentario := 'PROCESO '||v_nombre_proceso||' EXITOSO. SE ACTUALIZARON :'||V_TOTAL_DIFERENCIAS ||' NUEVOS : ' ||V_CANT_REG;
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
     v_comentario :=  ('ERROR EN EL PROCESO '||v_nombre_proceso||' '||SQLCODE||' '||SQLERRM);
     P_Insertar_Info_Proc(v_nombre_proceso,
                          v_fec_inicio,
                          v_fec_fin,
                          v_comentario,
                          v_cant_reg,
                          v_correcto )
                          ;
     COMMIT  ;
END;

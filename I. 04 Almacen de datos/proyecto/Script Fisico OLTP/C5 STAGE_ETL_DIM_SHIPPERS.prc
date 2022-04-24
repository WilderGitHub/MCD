CREATE OR REPLACE PROCEDURE ETL_DIM_SHIPPERS IS

  V_NOMBRE_PROCESO   VARCHAR2(30):= 'ETL_DIM_SHIPPERS';
  V_FEC_INICIO       DATE;
  V_FEC_FIN          DATE;
  V_COMENTARIO       VARCHAR2(255);
  V_CANT_REG         NUMBER(10)  := 0;
  V_CORRECTO         VARCHAR2(1) := 'N';

  v_total_diferencias    NUMBER(10) := 0;

BEGIN
  v_fec_inicio := SYSDATE;

  -- CODIGO DEL PROCESO
      
    FOR V_REG IN ( SELECT IDW_SHIPPER,SHIPPERID,SHIPPERS,PHONE
                   FROM STG_DIM_SHIPPERS
                   MINUS
                   SELECT IDW_SHIPPER,SHIPPERID,SHIPPERS,PHONE
                   FROM PSTAR.DIM_SHIPPERS)
LOOP
    
         IF V_REG.IDW_SHIPPER !='-1' THEN
    
            UPDATE  PSTAR.DIM_SHIPPERS
            SET  IDW_SHIPPER=V_REG.IDW_SHIPPER,SHIPPERID=V_REG.SHIPPERID,SHIPPERS=V_REG.SHIPPERS,PHONE=V_REG.PHONE
            
            WHERE IDW_SHIPPER = V_REG.IDW_SHIPPER;

            COMMIT;
            V_TOTAL_DIFERENCIAS := V_TOTAL_DIFERENCIAS+1;
       END IF;
    
END LOOP ;



/************ SI NO EXISTE DIFERENCIAS SIGNIFICA QUE EXISTEN NUEVOS REGISTROS ***/

    INSERT INTO PSTAR.DIM_SHIPPERS (IDW_SHIPPER,SHIPPERID,SHIPPERS,PHONE,LOG_USER,START_DATE,END_DATE,CURRENT_FLAG,PK_SYSTEM_SOURCE,SYSTEM_SOURCE)
    SELECT SEQ_IDW_SHIPPER.NEXTVAL IDW_SHIPPER
        ,SHIPPERID,SHIPPERS,PHONE
        ,SUBSTR(Sys_Context('USERENV','OS_USER'),1,10) LOG_USER,SYSDATE START_DATE,SYSDATE END_DATE,1 CURRENT_FLAG,1 PK_SYSTEM_SOURCE,'A' SYSTEM_SOURCE
                
    FROM STG_DIM_SHIPPERS
    WHERE IDW_SHIPPER = -1;
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

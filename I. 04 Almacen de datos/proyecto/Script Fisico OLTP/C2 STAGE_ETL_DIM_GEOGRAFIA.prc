CREATE OR REPLACE PROCEDURE ETL_DIM_GEOGRAFIA IS

  V_NOMBRE_PROCESO   VARCHAR2(30):= 'ETL_DIM_GEOGRAFIA';
  V_FEC_INICIO       DATE;
  V_FEC_FIN          DATE;
  V_COMENTARIO       VARCHAR2(255);
  V_CANT_REG         NUMBER(10)  := 0;
  V_CORRECTO         VARCHAR2(1) := 'N';

  v_total_diferencias    NUMBER(10) := 0;

BEGIN
  v_fec_inicio := SYSDATE;

  -- CODIGO DEL PROCESO
      
    FOR V_REG IN ( SELECT IDW_GEOGRAFIA,ADDRESS,CITY,REGION,POSTALCODE,COUNTRY
                   FROM STG_DIM_GEOGRAFIA
                   MINUS
                   SELECT IDW_GEOGRAFIA,ADDRESS,CITY,REGION,POSTALCODE,COUNTRY
                   FROM PSTAR.DIM_GEOGRAFIA)
LOOP
    
         IF V_REG.IDW_GEOGRAFIA !='-1' THEN
    
            UPDATE  PSTAR.DIM_GEOGRAFIA
            SET  ADDRESS = V_REG.ADDRESS
            ,CITY = V_REG.CITY
            ,REGION= V_REG.REGION
            ,POSTALCODE= V_REG.POSTALCODE
            ,COUNTRY= V_REG.COUNTRY
                       
            WHERE IDW_GEOGRAFIA = V_REG.IDW_GEOGRAFIA;

            COMMIT;
            V_TOTAL_DIFERENCIAS := V_TOTAL_DIFERENCIAS+1;
       END IF;
    
END LOOP ;



/************ SI NO EXISTE DIFERENCIAS SIGNIFICA QUE EXISTEN NUEVOS REGISTROS ***/

    INSERT INTO PSTAR.DIM_GEOGRAFIA (IDW_GEOGRAFIA,ADDRESS,CITY,REGION,POSTALCODE,COUNTRY)
    SELECT SEQ_IDW_GEOGRAFIA.NEXTVAL IDW_GEOGRAFIA,ADDRESS,CITY,REGION,POSTALCODE,COUNTRY
                
    FROM STG_DIM_GEOGRAFIA
    WHERE IDW_GEOGRAFIA = -1;
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

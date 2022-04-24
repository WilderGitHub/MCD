CREATE OR REPLACE PROCEDURE ETL_STG_DIM_GEOGRAFIA IS

  V_NOMBRE_PROCESO   VARCHAR2(30):= 'ETL_STG_DIM_GEOGRAFIA';
  V_FEC_INICIO       DATE;
  V_FEC_FIN          DATE;
  V_COMENTARIO       VARCHAR2(255);
  V_CANT_REG         NUMBER(10)  := 0;
  V_CORRECTO         VARCHAR2(1) := 'N'; 

  v_total_diferencias    NUMBER(10) := 0;

BEGIN
  v_fec_inicio := SYSDATE;

  -- CODIGO DEL PROCESO

    EXECUTE IMMEDIATE 'TRUNCATE TABLE STG_DIM_GEOGRAFIA';
    
     
   
 INSERT INTO  STG_DIM_GEOGRAFIA(IDW_GEOGRAFIA,ADDRESS,CITY,REGION,POSTALCODE,COUNTRY)
SELECT NVL((SELECT  GEO.IDW_GEOGRAFIA  IDW_GEOGRAFIA
            FROM PSTAR.DIM_GEOGRAFIA GEO
            WHERE  GEO.IDW_GEOGRAFIA = G.IDW_GEOGRAFIA),-1) AS IDW_GEOGRAFIA
 , G.ADDRESS,G.CITY,G.REGION,G.POSTALCODE,G.COUNTRY
    FROM STG_GEOGRAFIA G
    ;
       V_CANT_REG:= SQL%ROWCOUNT;
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

-- ALTER TABLE STG_DIM_GEOGRAFIA 
-- MODIFY COUNTRY VARCHAR2( 15 )
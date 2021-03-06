CREATE OR REPLACE PROCEDURE ETL_DIM_PRODUCTOS IS

  V_NOMBRE_PROCESO   VARCHAR2(30):= 'ETL_DIM_PRODUCTOS';
  V_FEC_INICIO       DATE;
  V_FEC_FIN          DATE;
  V_COMENTARIO       VARCHAR2(255);
  V_CANT_REG         NUMBER(10)  := 0;
  V_CORRECTO         VARCHAR2(1) := 'N'; 
 -- V_VFILENAME        VARCHAR2(30);
 -- V_FONO_SMS         VARCHAR2(10) := 'XXXXX';

  -- VARIABLES DEL PROCESO

  v_total_diferencias    NUMBER(10) := 0;

BEGIN
  v_fec_inicio := SYSDATE;

  -- CODIGO DEL PROCESO
    FOR V_REG IN ( SELECT IDW_PRODUCTO,PRODUCTID,PRODUCTNAME,QUANTITYPERUNIT,UNITPRICE,UNITSINSTOCK,UNITSONORDER,REORDERLEVEL,DISCONTINUED,
                                CATEGORYID,CATEGORYNAME,DESCRIPTION,SUPPLIERID,SUPPLIER,SUPPLIERCONTACT,SUPPLIERTITLE,
                                ADDRESS,CITY,REGION,POSTALCODE,COUNTRY,PHONE,FAX
                   FROM STG_DIM_PRODUCTOS
                   MINUS
                   SELECT IDW_PRODUCTO,PRODUCTID,PRODUCTNAME,QUANTITYPERUNIT,UNITPRICE,UNITSINSTOCK,UNITSONORDER,REORDERLEVEL,DISCONTINUED,
                    CATEGORYID,CATEGORYNAME,DESCRIPTION,SUPPLIERID,SUPPLIER,SUPPLIERCONTACT,SUPPLIERTITLE,
                    ADDRESS,CITY,REGION,POSTALCODE,COUNTRY,PHONE,FAX
                    
                   FROM PSTAR.DIM_PRODUCTOS)
LOOP
    
        --- si encuentre el codigo en la dim , quiere decir que existe cambios en los campos
        -- si no encuentra el codigo quiere decir que es un nuevo registro que se debe insertar
            
      IF V_REG.IDW_PRODUCTO !='-1' THEN
    
        UPDATE  PSTAR.DIM_PRODUCTOS
        SET   PRODUCTID=V_REG.PRODUCTID,PRODUCTNAME=V_REG.PRODUCTNAME,QUANTITYPERUNIT=V_REG.QUANTITYPERUNIT
              ,UNITPRICE=V_REG.UNITPRICE,UNITSINSTOCK=V_REG.UNITSINSTOCK,UNITSONORDER=V_REG.UNITSONORDER
              ,REORDERLEVEL=V_REG.REORDERLEVEL,DISCONTINUED=V_REG.DISCONTINUED,CATEGORYID=V_REG.CATEGORYID
              ,CATEGORYNAME=V_REG.CATEGORYNAME,DESCRIPTION=V_REG.DESCRIPTION,SUPPLIERID=V_REG.SUPPLIERID
              ,SUPPLIER=V_REG.SUPPLIER,SUPPLIERCONTACT=V_REG.SUPPLIERCONTACT,SUPPLIERTITLE=V_REG.SUPPLIERTITLE
              ,ADDRESS=V_REG.ADDRESS,CITY=V_REG.CITY,REGION=V_REG.REGION,POSTALCODE=V_REG.POSTALCODE
              ,COUNTRY=V_REG.COUNTRY,PHONE=V_REG.PHONE,FAX=V_REG.FAX 
        WHERE   IDW_PRODUCTO   = V_REG.IDW_PRODUCTO;
             
        COMMIT;
   /************ SI NO EXISTE DIFERENCIAS SIGNIFICA QUE EXISTEN NUEVOS REGISTROS ***/

    V_TOTAL_DIFERENCIAS := V_TOTAL_DIFERENCIAS+1;
    END IF ;
COMMIT;

END LOOP ;

    INSERT INTO PSTAR.DIM_PRODUCTOS (IDW_PRODUCTO,PRODUCTID,PRODUCTNAME,QUANTITYPERUNIT,UNITPRICE,UNITSINSTOCK,UNITSONORDER,REORDERLEVEL
                    ,DISCONTINUED,CATEGORYID,CATEGORYNAME,DESCRIPTION,SUPPLIERID,SUPPLIER,SUPPLIERCONTACT,SUPPLIERTITLE
                    ,ADDRESS,CITY,REGION,POSTALCODE,COUNTRY,PHONE,FAX,LOG_USER,START_DATE,END_DATE,CURRENT_FLAG,PK_SYSTEM_SOURCE,SYSTEM_SOURCE)
    SELECT  SEQ_IDW_PRODUCT.NEXTVAL IDW_PRODUCTO
            ,PRODUCTID,PRODUCTNAME,QUANTITYPERUNIT,UNITPRICE,UNITSINSTOCK,UNITSONORDER,REORDERLEVEL
            ,DISCONTINUED,CATEGORYID,CATEGORYNAME,DESCRIPTION,SUPPLIERID,SUPPLIER,SUPPLIERCONTACT,SUPPLIERTITLE
            ,ADDRESS,CITY,REGION,POSTALCODE,COUNTRY,PHONE,FAX
            ,SUBSTR(Sys_Context('USERENV','OS_USER'),1,10) LOG_USER,SYSDATE START_DATE,SYSDATE END_DATE,1 CURRENT_FLAG,1 PK_SYSTEM_SOURCE,'A' SYSTEM_SOURCE
            
    FROM STG_DIM_PRODUCTOS
    WHERE IDW_PRODUCTO = -1 ;
  V_CANT_REG:= SQL%ROWCOUNT;
  
    COMMIT;



    
    
  -- FIN CODIGO DEL PROCESO

  v_fec_fin    := SYSDATE;
  v_comentario := 'PROCESO '||v_nombre_proceso||' EXITOSO. ACTUALIZADOS :'||V_TOTAL_DIFERENCIAS ||' NUEVOS : ' ||V_CANT_REG;
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

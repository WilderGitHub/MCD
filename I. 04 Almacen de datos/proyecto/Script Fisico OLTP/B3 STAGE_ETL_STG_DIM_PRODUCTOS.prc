CREATE OR REPLACE PROCEDURE ETL_STG_DIM_PRODUCTOS IS

  V_NOMBRE_PROCESO   VARCHAR2(30):= 'ETL_STG_DIM_PRODUCTOS';
  V_FEC_INICIO       DATE;
  V_FEC_FIN          DATE;
  V_COMENTARIO 	   	 VARCHAR2(255);
  V_CANT_REG         NUMBER(10)  := 0;
  V_CORRECTO         VARCHAR2(1) := 'N'; 
  
  BEGIN
  v_fec_inicio := SYSDATE;

  -- CODIGO DEL PROCESO

	EXECUTE IMMEDIATE 'TRUNCATE TABLE STG_DIM_PRODUCTOS';
    COMMIT;

   INSERT INTO STG_DIM_PRODUCTOS(IDW_PRODUCTO,PRODUCTID,PRODUCTNAME,QUANTITYPERUNIT,UNITPRICE,UNITSINSTOCK,UNITSONORDER,REORDERLEVEL,DISCONTINUED,
                                CATEGORYID,CATEGORYNAME,DESCRIPTION,SUPPLIERID,SUPPLIER,SUPPLIERCONTACT,SUPPLIERTITLE,
                                ADDRESS,CITY,REGION,POSTALCODE,COUNTRY,PHONE,FAX,
                                LOG_USER,START_DATE,END_DATE,CURRENT_FLAG,PK_SYSTEM_SOURCE,SYSTEM_SOURCE)
    SELECT NVL((SELECT A.IDW_PRODUCTO
            FROM PSTAR.DIM_PRODUCTOS A 
            WHERE   A.PRODUCTID = PRO.PRODUCTID
            ),-1) IDW_PRODUCTO
            ,PRO.PRODUCTID,PRO.PRODUCTNAME,PRO.QUANTITYPERUNIT,PRO.UNITPRICE,PRO.UNITSINSTOCK,PRO.UNITSONORDER,PRO.REORDERLEVEL,PRO.DISCONTINUED
            ,CAT.CATEGORYID,CAT.CATEGORYNAME,CAT.DESCRIPTION
            ,SUP.SUPPLIERID,SUP.COMPANYNAME,SUP.CONTACTNAME,SUP.CONTACTTITLE
            ,SUP.ADDRESS,SUP.CITY,SUP.REGION,SUP.POSTALCODE,SUP.COUNTRY,SUP.PHONE,SUP.FAX -- PODEMOS OPTIMIZAR CON LA DIM_GEOGRAFIA
            ,SUBSTR(Sys_Context('USERENV','OS_USER'),1,10) LOG_USER
            ,SYSDATE START_DATE,SYSDATE END_DATE,1 CURRENT_FLAG,1 PK_SYSTEM_SOURCE,'A' SYSTEM_SOURCE
            
    FROM STG_PRODUCTS PRO, STG_CATEGORIES CAT, STG_SUPPLIERS SUP
    
    WHERE  --PRO.PRODUCTID=A.PRODUCTID AND
        PRO.CATEGORYID=CAT.CATEGORYID
    AND    PRO.SUPPLIERID=SUP.SUPPLIERID
     ;
    
    V_CANT_REG := SQL%ROWCOUNT;
	COMMIT;
	
	
    SELECT COUNT(1)
    INTO V_CANT_REG
    FROM STG_DIM_PRODUCTOS ;

	
  -- FIN CODIGO DEL PROCESO

  v_fec_fin    := SYSDATE;
  v_comentario := 'EL PROCESO '||v_nombre_proceso||' FUE UN EXITO ';
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
     v_comentario :=  ('ERROR EN PROCESO '||v_nombre_proceso||' '||SQLCODE||' '||SQLERRM);
     P_Insertar_Info_Proc(v_nombre_proceso,
             			  v_fec_inicio,
                    	  v_fec_fin,
		        		  v_comentario,
		   				  v_cant_reg,
	  	        		  v_correcto )
						  ;
     COMMIT	 ;



END;

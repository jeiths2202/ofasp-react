PGM PARM(&TYPE)
  DCL VAR(&TYPE) TYPE(*CHAR) LEN(10)
  DCL VAR(&MSG) TYPE(*CHAR) LEN(100)
  DCL VAR(&TIMESTAMP) TYPE(*CHAR) LEN(20)
  
  MONMSG MSGID(CPF0000) EXEC(GOTO CMDLBL(ERROR))
  
  /* Report generation CL procedure */
  RTVSYSVAL SYSVAL(QDATETIME) RTNVAR(&TIMESTAMP)
  CHGVAR VAR(&MSG) VALUE('Generating ' *CAT &TYPE *CAT ' report at ' *CAT &TIMESTAMP)
  SNDPGMMSG MSG(&MSG)
  
  /* Process based on report type */
  SELECT
    WHEN COND(&TYPE *EQ 'EMPLOYEE') THEN(DO)
      CALL PGM(EMPFILEJAVA) PARM('*ALL' 'RPT')
      SNDPGMMSG MSG('Employee report generated')
    ENDDO
    
    WHEN COND(&TYPE *EQ 'SUMMARY') THEN(DO)
      CALL PGM(EMPFILEJAVA) PARM('*ALL' 'SUM')
      SNDPGMMSG MSG('Summary report generated')
    ENDDO
    
    OTHERWISE CMD(DO)
      SNDPGMMSG MSG('Unknown report type - generating default report')
      CALL PGM(EMPFILEJAVA) PARM('*ALL' 'RPT')
    ENDDO
  ENDSELECT
  
  SNDPGMMSG MSG('Report generation completed successfully')
  RETURN
  
  ERROR:
    CHGVAR VAR(&MSG) VALUE('Error generating report: ' *CAT &TYPE)
    SNDPGMMSG MSG(&MSG)
    MONMSG MSGID(CPF0000)
  
ENDPGM
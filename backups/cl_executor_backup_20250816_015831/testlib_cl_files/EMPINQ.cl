PGM PARM(&EMPID)
  DCL VAR(&EMPID) TYPE(*CHAR) LEN(8)
  DCL VAR(&MSG) TYPE(*CHAR) LEN(100)
  
  MONMSG MSGID(CPF0000) EXEC(GOTO CMDLBL(ERROR))
  
  /* Employee inquiry CL command processor */
  CHGVAR VAR(&MSG) VALUE('Employee inquiry for ID: ' *CAT &EMPID)
  SNDPGMMSG MSG(&MSG)
  
  /* Call Java program to process EMPLOYEE.FB */
  CALL PGM(EMPFILEJAVA) PARM(&EMPID 'INQ')
  
  SNDPGMMSG MSG('Employee inquiry completed successfully')
  RETURN
  
  ERROR:
    CHGVAR VAR(&MSG) VALUE('Error in employee inquiry for ID: ' *CAT &EMPID)
    SNDPGMMSG MSG(&MSG)
    MONMSG MSGID(CPF0000)
  
ENDPGM
PGM PARM(&EMPID)
  DCL VAR(&EMPID) TYPE(*CHAR) LEN(8)
  DCL VAR(&MSG) TYPE(*CHAR) LEN(100)
  
  MONMSG MSGID(CPF0000) EXEC(GOTO CMDLBL(ERROR))
  
  /* Employee delete CL command processor */
  CHGVAR VAR(&MSG) VALUE('Deleting employee: ' *CAT &EMPID)
  SNDPGMMSG MSG(&MSG)
  
  /* Call Java program to delete from EMPLOYEE.FB */
  CALL PGM(EMPFILEJAVA) PARM(&EMPID 'DEL')
  
  SNDPGMMSG MSG('Employee deleted successfully')
  RETURN
  
  ERROR:
    CHGVAR VAR(&MSG) VALUE('Error deleting employee: ' *CAT &EMPID)
    SNDPGMMSG MSG(&MSG)
    MONMSG MSGID(CPF0000)
  
ENDPGM
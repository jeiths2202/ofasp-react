PGM PARM(&EMPID &FIELD &VALUE)
  DCL VAR(&EMPID) TYPE(*CHAR) LEN(8)
  DCL VAR(&FIELD) TYPE(*CHAR) LEN(10)
  DCL VAR(&VALUE) TYPE(*CHAR) LEN(50)
  DCL VAR(&MSG) TYPE(*CHAR) LEN(100)
  
  MONMSG MSGID(CPF0000) EXEC(GOTO CMDLBL(ERROR))
  
  /* Employee update CL command processor */
  CHGVAR VAR(&MSG) VALUE('Updating employee: ' *CAT &EMPID *CAT ' field: ' *CAT &FIELD)
  SNDPGMMSG MSG(&MSG)
  
  /* Call Java program to update EMPLOYEE.FB */
  CALL PGM(EMPFILEJAVA) PARM(&EMPID 'UPD' &FIELD &VALUE)
  
  SNDPGMMSG MSG('Employee updated successfully')
  RETURN
  
  ERROR:
    CHGVAR VAR(&MSG) VALUE('Error updating employee: ' *CAT &EMPID)
    SNDPGMMSG MSG(&MSG)
    MONMSG MSGID(CPF0000)
  
ENDPGM
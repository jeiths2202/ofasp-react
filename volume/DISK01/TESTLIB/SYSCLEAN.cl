PGM
  DCL VAR(&MSG) TYPE(*CHAR) LEN(100)
  DCL VAR(&COUNT) TYPE(*DEC) LEN(5 0)
  
  MONMSG MSGID(CPF0000) EXEC(GOTO CMDLBL(ERROR))
  
  /* System cleanup CL procedure */
  SNDPGMMSG MSG('Starting system cleanup process')
  
  /* Clean temporary files */
  CHGVAR VAR(&COUNT) VALUE(0)
  DLTF FILE(QTEMP/*ALL)
  MONMSG MSGID(CPF2125) /* File not found */
  
  /* Clean job logs older than 7 days */
  DLTJOBLOG JOB(*) DAYS(7)
  MONMSG MSGID(CPF1301) /* No job logs found */
  
  /* Clean spool files */
  DLTSPLF FILE(*ALL) SELECT(*ALL) DAYS(7)
  MONMSG MSGID(CPF3314) /* No spool files found */
  
  SNDPGMMSG MSG('System cleanup completed successfully')
  RETURN
  
  ERROR:
    CHGVAR VAR(&MSG) VALUE('Error during system cleanup')
    SNDPGMMSG MSG(&MSG)
    MONMSG MSGID(CPF0000)
  
ENDPGM
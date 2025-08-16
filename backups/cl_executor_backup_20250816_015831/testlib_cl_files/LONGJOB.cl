PGM
  DCL VAR(&MSG) TYPE(*CHAR) LEN(100)
  DCL VAR(&COUNTER) TYPE(*DEC) LEN(5 0)
  DCL VAR(&PROGRESS) TYPE(*CHAR) LEN(10)
  
  MONMSG MSGID(CPF0000) EXEC(GOTO CMDLBL(ERROR))
  
  /* Long running job CL (3+ minutes) for resource monitoring test */
  SNDPGMMSG MSG('Starting long running job - will run for 3+ minutes')
  
  CHGVAR VAR(&COUNTER) VALUE(1)
  
  /* Main processing loop - runs for approximately 3 minutes */
  LOOP_START:
    IF COND(&COUNTER *LE 180) THEN(DO)
      /* Simulate heavy processing */
      CHGVAR VAR(&PROGRESS) VALUE(%CHAR(&COUNTER))
      CHGVAR VAR(&MSG) VALUE('Processing step ' *CAT &PROGRESS *CAT ' of 180')
      SNDPGMMSG MSG(&MSG)
      
      /* Call EMPLOYEE.FB processing to consume CPU/Memory */
      CALL PGM(EMPFILEJAVA) PARM('*ALL' 'SCAN')
      MONMSG MSGID(CPF0000)
      
      /* Small delay to prevent system overload */
      DLYJOB DLY(1)
      
      CHGVAR VAR(&COUNTER) VALUE(&COUNTER + 1)
      GOTO CMDLBL(LOOP_START)
    ENDDO
  
  SNDPGMMSG MSG('Long running job completed successfully after 3+ minutes')
  RETURN
  
  ERROR:
    CHGVAR VAR(&MSG) VALUE('Error in long running job at step ' *CAT %CHAR(&COUNTER))
    SNDPGMMSG MSG(&MSG)
    MONMSG MSGID(CPF0000)
  
ENDPGM
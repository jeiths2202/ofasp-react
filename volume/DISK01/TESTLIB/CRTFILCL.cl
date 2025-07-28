/* CRTFILCL - Create File CL Program */
/* Calls CRTFILE01 Java program to create datasets */

PGM

DCL VAR(&FILENAME) TYPE(*CHAR) LEN(10)
DCL VAR(&LIBRARY) TYPE(*CHAR) LEN(10)
DCL VAR(&VOLUME) TYPE(*CHAR) LEN(10)

/* Get parameters */
IF (&FILENAME = ' ') THEN(CHGVAR VAR(&FILENAME) VALUE('TEST.FILE'))
IF (&LIBRARY = ' ') THEN(CHGVAR VAR(&LIBRARY) VALUE('TESTLIB'))
IF (&VOLUME = ' ') THEN(CHGVAR VAR(&VOLUME) VALUE('DISK01'))

/* Display start message */
SNDPGMMSG MSG('CRTFILCL: Starting file creation process...') TOPGMQ(*EXT)

/* Call Java program CRTFILE01 */
CALL PGM(CRTFILE01) PARM(&FILENAME &LIBRARY &VOLUME)

/* Display completion message */
SNDPGMMSG MSG('CRTFILCL: File creation process completed') TOPGMQ(*EXT)

ENDPGM
/* DLTFILCL - Delete File CL Program */
/* Calls DLTFILE01 Java program to delete datasets */

PGM

DCL VAR(&FILENAME) TYPE(*CHAR) LEN(10)
DCL VAR(&LIBRARY) TYPE(*CHAR) LEN(10)
DCL VAR(&VOLUME) TYPE(*CHAR) LEN(10)

/* Get parameters */
IF (&FILENAME = ' ') THEN(CHGVAR VAR(&FILENAME) VALUE('TEST.FILE'))
IF (&LIBRARY = ' ') THEN(CHGVAR VAR(&LIBRARY) VALUE('TESTLIB'))
IF (&VOLUME = ' ') THEN(CHGVAR VAR(&VOLUME) VALUE('DISK01'))

/* Display start message */
SNDPGMMSG MSG('DLTFILCL: Starting file deletion process...') TOPGMQ(*EXT)

/* Call Java program DLTFILE01 */
CALL PGM(DLTFILE01) PARM(&FILENAME &LIBRARY &VOLUME)

/* Display completion message */
SNDPGMMSG MSG('DLTFILCL: File deletion process completed') TOPGMQ(*EXT)

ENDPGM
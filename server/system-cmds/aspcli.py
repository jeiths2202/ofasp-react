# aspcli.py
# Fujitsu ASP command reimplementation (CLI version)

import argparse
import os
from datetime import datetime

# UTF-8 인코딩 설정
os.environ['PYTHONIOENCODING'] = 'utf-8'

from asp_commands import (
    CRTLIB, DLTLIB, WRKLIB, CRTFILE, DLTFILE, DSPFD,
    WRKOBJ, CALL, WRKVOL, WRKSPLF, WRKMSG,
    DSPJOB, SAVLIB, RSTLIB, SNDMSG, RCVMSG
)

VOLUME_ROOT = "/home/aspuser/app/volume"
BACKUP_DIR = os.path.join(VOLUME_ROOT, "/home/aspuser/app/volume/BACKUP")
os.makedirs(BACKUP_DIR, exist_ok=True)

# CLI entry
if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="aspcli - Fujitsu ASP command interpreter")
    parser.add_argument("command", help="ASP command (e.g., CRTLIB, CALL, WRKLIB, etc.)")
    parser.add_argument("params", nargs="*", help="Command parameters")
    args = parser.parse_args()

    full_command = args.command + ' ' + ','.join(args.params)

    command_map = {
        "CRTLIB": CRTLIB,
        "DLTLIB": DLTLIB,
        "WRKLIB": WRKLIB,
        "CRTFILE": CRTFILE,
        "DLTFILE": DLTFILE,
        "DSPFD": DSPFD,
        "WRKOBJ": WRKOBJ,
        "CALL": CALL,
        "WRKVOL": WRKVOL,
        "WRKSPLF": WRKSPLF,
        "WRKMSG": WRKMSG,
        "DSPJOB": DSPJOB,
        "SAVLIB": SAVLIB,
        "RSTLIB": RSTLIB,
        "SNDMSG": SNDMSG,
        "RCVMSG": RCVMSG,
    }

    if args.command in command_map:
        command_map[args.command](full_command)
    else:
        print(f"[ERROR] 알 수 없는 명령입니다: {args.command}")

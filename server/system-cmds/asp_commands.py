import subprocess
import os
from datetime import datetime
import shutil

VOLUME_ROOT = "/home/aspuser/app/volume"



def WRKOBJ(command):
    # 예: WRKOBJ LIB-SALES,VOL-DISK01
    params = dict(item.split('-') for item in command.replace('WRKOBJ ', '').split(','))
    lib = params.get('LIB')
    vol = params.get('VOL')

    if not lib or not vol:
        print("[ERROR] LIB 또는 VOL 파라미터가 누락되었습니다.")
        return

    lib_path = os.path.join(VOLUME_ROOT, vol, lib)

    if not os.path.isdir(lib_path):
        print(f"[ERROR] 라이브러리 '{lib}'는 볼륨 '{vol}'에 존재하지 않습니다.")
        return

    files = os.listdir(lib_path)
    if not files:
        print(f"[INFO] 라이브러리 '{lib}'에 객체가 없습니다.")
        return

    print(f"[INFO] 라이브러리 '{lib}' 내 객체 목록 (볼륨: {vol}):")
    for f in files:
        f_path = os.path.join(lib_path, f)
        size = os.path.getsize(f_path)
        mtime = datetime.fromtimestamp(os.path.getmtime(f_path)).strftime('%Y-%m-%d %H:%M:%S')
        print(f"  📄 {f.ljust(20)} | 크기: {str(size).rjust(6)} Byte | 수정일: {mtime}")

def DSPFD(command):
    # 예: DSPFD FILE(SALES/REPORT),VOL-DISK01
    main_part, *others = command.replace('DSPFD ', '').split(',')
    file_lib, file_name = main_part.replace('FILE(', '').replace(')', '').split('/')
    params = dict(item.split('-') for item in others if '-' in item)
    vol = params.get('VOL')

    if not vol:
        print("[ERROR] VOL 파라미터가 누락되었습니다.")
        return

    file_path = os.path.join(VOLUME_ROOT, vol, file_lib, file_name)

    if not os.path.isfile(file_path):
        print(f"[ERROR] 파일 '{file_name}'는 볼륨 '{vol}'의 라이브러리 '{file_lib}'에 존재하지 않습니다.")
        return

    size = os.path.getsize(file_path)
    modified = datetime.fromtimestamp(os.path.getmtime(file_path)).strftime('%Y-%m-%d %H:%M:%S')
    created = datetime.fromtimestamp(os.path.getctime(file_path)).strftime('%Y-%m-%d %H:%M:%S')
    is_empty = size == 0

    print(f"[INFO] 파일 정의 정보:")
    print(f"  📁 파일 경로       : {file_path}")
    print(f"  📄 파일 이름       : {file_name}")
    print(f"  📦 파일 크기       : {size} Byte")
    print(f"  🕒 생성일시         : {created}")
    print(f"  🕒 최종 수정일시     : {modified}")
    print(f"  📉 파일이 비어 있음  : {'예' if is_empty else '아니오'}")


def RCVMSG(command):
    params = dict(item.split('-', 1) for item in command.replace('RCVMSG ', '').split(','))
    user = params.get('USER')

    if not user:
        print("[ERROR] USER 파라미터가 누락되었습니다.")
        return

    user_file = os.path.join(VOLUME_ROOT, "MSGQ", "users", f"{user}.msg")
    if not os.path.isfile(user_file):
        print(f"[INFO] 사용자 '{user}'에게 도착한 메시지가 없습니다.")
        return

    print(f"[INFO] 사용자 '{user}'의 수신 메시지:")
    with open(user_file, 'r') as f:
        for line in f:
            print(" ", line.strip())
def SNDMSG(command):
    params = dict(item.split('-', 1) for item in command.replace('SNDMSG ', '').split(','))
    user = params.get('TO')
    message = params.get('MSG')

    if not user or not message:
        print("[ERROR] TO 또는 MSG 파라미터가 누락되었습니다.")
        return

    user_dir = os.path.join(VOLUME_ROOT, "MSGQ", "users")
    os.makedirs(user_dir, exist_ok=True)
    user_file = os.path.join(user_dir, f"{user}.msg")

    timestamp = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
    with open(user_file, 'a') as f:
        f.write(f"[{timestamp}] {message}\n")

    print(f"[INFO] 사용자 '{user}'에게 메시지를 전송했습니다.")
    log_message("INFO", f"SNDMSG TO-{user}: {message}")
def RSTLIB(command):
    params = dict(item.split('-') for item in command.replace('RSTLIB ', '').split(','))
    backup_file = params.get('FILE')

    if not backup_file:
        print("[ERROR] FILE 파라미터가 누락되었습니다.")
        return

    backup_path = os.path.join(BACKUP_DIR, backup_file)
    if not os.path.isfile(backup_path):
        print(f"[ERROR] 백업 파일이 존재하지 않습니다: {backup_path}")
        return

    try:
        with tarfile.open(backup_path, "r:gz") as tar:
            tar.extractall(path=VOLUME_ROOT)
        print(f"[INFO] 복원 완료: {backup_path}")
        log_message("INFO", f"RSTLIB → {backup_file} 복원 성공")
    except Exception as e:
        print(f"[ERROR] 복원 실패: {e}")
        log_message("ERROR", f"RSTLIB 실패: {e}")
import tarfile

def SAVLIB(command):
    params = dict(item.split('-') for item in command.replace('SAVLIB ', '').split(','))
    lib = params.get('LIB')
    vol = params.get('VOL')

    if not lib or not vol:
        print("[ERROR] LIB 또는 VOL 파라미터가 누락되었습니다.")
        return

    lib_path = os.path.join(VOLUME_ROOT, vol, lib)
    if not os.path.isdir(lib_path):
        print(f"[ERROR] 라이브러리 '{lib}'는 볼륨 '{vol}'에 존재하지 않습니다.")
        return

    timestamp = datetime.now().strftime("%Y%m%d%H%M%S")
    backup_name = f"{lib}_{vol}_{timestamp}.tar.gz"
    backup_path = os.path.join(BACKUP_DIR, backup_name)

    with tarfile.open(backup_path, "w:gz") as tar:
        tar.add(lib_path, arcname=f"{lib}")

    print(f"[INFO] 라이브러리 '{lib}'가 백업되었습니다: {backup_path}")
    log_message("INFO", f"SAVLIB {lib} → {backup_name}")
def DSPJOB():
    log_path = os.path.join(VOLUME_ROOT, "JOBLOG", "job.log")
    if not os.path.isfile(log_path):
        print("[INFO] 실행된 잡 이력이 없습니다.")
        return

    print("[INFO] 잡 이력 (최신순):")
    with open(log_path, "r") as f:
        lines = f.readlines()
        for line in reversed(lines[-10:]):  # 최근 10개만 표시
            job_id, lib, prog, start, end, status = line.strip().split(',')
            print(f"  🔹 잡ID: {job_id}")
            print(f"     ├ 프로그램: {lib}/{prog}")
            print(f"     ├ 시작시각 : {start}")
            print(f"     ├ 종료시각 : {end}")
            print(f"     └ 상태     : {status}")
def record_job(lib, prog, status, start_time, end_time):
    job_dir = os.path.join(VOLUME_ROOT, "JOBLOG")
    os.makedirs(job_dir, exist_ok=True)
    log_path = os.path.join(job_dir, "job.log")
    job_id = datetime.now().strftime("%Y%m%d%H%M%S")
    with open(log_path, "a") as f:
        f.write(f"{job_id},{lib},{prog},{start_time},{end_time},{status}\n")
def WRKMSG():
    log_path = os.path.join(VOLUME_ROOT, "MSGQ", "system.log")
    if not os.path.isfile(log_path):
        print("[INFO] 메시지 큐에 저장된 메시지가 없습니다.")
        return

    print("[INFO] 시스템 메시지 큐:")
    with open(log_path, "r") as f:
        for line in f:
            print(" ", line.strip())
def log_message(level, message):
    msgq_dir = os.path.join(VOLUME_ROOT, "MSGQ")
    os.makedirs(msgq_dir, exist_ok=True)
    log_path = os.path.join(msgq_dir, "system.log")
    now = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
    with open(log_path, "a") as f:
        f.write(f"[{level.upper()}] {now} {message}\n")
def WRKVOL(command=None):
    print("[INFO] 볼륨 현황:")

    if not os.path.isdir(VOLUME_ROOT):
        print("[INFO] 현재 등록된 볼륨이 없습니다.")
        return

    for vol in os.listdir(VOLUME_ROOT):
        vol_path = os.path.join(VOLUME_ROOT, vol)
        if not os.path.isdir(vol_path):
            continue

        lib_count = 0
        file_count = 0
        total_size = 0

        for lib in os.listdir(vol_path):
            lib_path = os.path.join(vol_path, lib)
            if not os.path.isdir(lib_path):
                continue

            lib_count += 1
            for obj in os.listdir(lib_path):
                obj_path = os.path.join(lib_path, obj)
                if os.path.isfile(obj_path):
                    file_count += 1
                    total_size += os.path.getsize(obj_path)

        print(f"  📦 볼륨명        : {vol}")
        print(f"     ├ 라이브러리 수 : {lib_count}")
        print(f"     ├ 총 파일 수     : {file_count}")
        print(f"     └ 디스크 사용량 : {total_size:,} Byte")

def CALL(command):
    # 예: CALL PGM-HELLO,VOL-DISK01
    params = dict(item.split('-') for item in command.replace('CALL ', '').split(','))
    pgm = params.get('PGM')
    vol = params.get('VOL')

    if not pgm or not vol:
        print("[ERROR] PGM 또는 VOL 파라미터가 누락되었습니다.")
        return

    # 경로 구성
    parts = pgm.split('/')
    if len(parts) != 2:
        print("[ERROR] PGM은 라이브러리/프로그램명 형식이어야 합니다. 예: PGM-SALES/HELLO")
        return

    lib, prog = parts
    prog_path = os.path.join(VOLUME_ROOT, vol, lib, prog)

    if not os.path.isfile(prog_path):
        print(f"[ERROR] 프로그램 '{prog}'는 볼륨 '{vol}'의 라이브러리 '{lib}'에 존재하지 않습니다.")
        return

    # 실행 방식 결정
    if prog_path.endswith('.py'):
        cmd = ['python3', prog_path]
    elif prog_path.endswith('.sh'):
        cmd = ['bash', prog_path]
    else:
        print(f"[ERROR] 실행 불가능한 확장자입니다: {prog_path}")
        return

    try:
        print(f"[INFO] 프로그램 실행 시작: {prog_path}")
        result = subprocess.run(cmd, check=True, text=True, capture_output=True)
        print("[OUTPUT]")
        print(result.stdout)
    except subprocess.CalledProcessError as e:
        print(f"[ERROR] 실행 실패:\n{e.stderr}")
def DSPFD(command):
    # 예: DSPFD FILE(SALES/REPORT),VOL-DISK01
    main_part, *others = command.replace('DSPFD ', '').split(',')
    file_lib, file_name = main_part.replace('FILE(', '').replace(')', '').split('/')
    params = dict(item.split('-') for item in others if '-' in item)
    vol = params.get('VOL')

    if not vol:
        print("[ERROR] VOL 파라미터가 누락되었습니다.")
        return

    file_path = os.path.join(VOLUME_ROOT, vol, file_lib, file_name)

    if not os.path.isfile(file_path):
        print(f"[ERROR] 파일 '{file_name}'는 볼륨 '{vol}'의 라이브러리 '{file_lib}'에 존재하지 않습니다.")
        return

    size = os.path.getsize(file_path)
    modified = datetime.fromtimestamp(os.path.getmtime(file_path)).strftime('%Y-%m-%d %H:%M:%S')
    created = datetime.fromtimestamp(os.path.getctime(file_path)).strftime('%Y-%m-%d %H:%M:%S')
    is_empty = size == 0

    print(f"[INFO] 파일 정의 정보:")
    print(f"  📁 파일 경로       : {file_path}")
    print(f"  📄 파일 이름       : {file_name}")
    print(f"  📦 파일 크기       : {size} Byte")
    print(f"  🕒 생성일시         : {created}")
    print(f"  🕒 최종 수정일시     : {modified}")
    print(f"  📉 파일이 비어 있음  : {'예' if is_empty else '아니오'}")

def DLTFILE(command):
    # 예: DLTFILE FILE(ACCTLIB/CUSTMAST),VOL-DISK01
    main_part, *others = command.replace('DLTFILE ', '').split(',')
    file_lib, file_name = main_part.replace('FILE(', '').replace(')', '').split('/')
    params = dict(item.split('-') for item in others if '-' in item)
    vol = params.get('VOL')

    if not vol:
        print("[ERROR] VOL 파라미터가 누락되었습니다.")
        return
def DLTLIB(command):
    # 예: DLTLIB LIB-ACCTLIB,VOL-DISK01
    params = dict(item.split('-') for item in command.replace('DLTLIB ', '').split(','))
    lib = params.get('LIB')
    vol = params.get('VOL')

    if not lib or not vol:
        print("[ERROR] LIB 또는 VOL 파라미터가 누락되었습니다.")
        return

    path = os.path.join(VOLUME_ROOT, vol, lib)

    if not os.path.exists(path):
        print(f"[ERROR] 라이브러리 '{lib}'는 볼륨 '{vol}'에 존재하지 않습니다.")
        return

    try:
        shutil.rmtree(path)
        print(f"[INFO] 라이브러리 '{lib}'가 볼륨 '{vol}'에서 삭제되었습니다.")
    except Exception as e:
        print(f"[ERROR] 삭제 실패: {e}")

def CRTLIB(command):
    # 예: CRTLIB LIB-ACCTLIB,VOL-DISK01
    params = dict(item.split('-') for item in command.replace('CRTLIB ', '').split(','))
    lib = params.get('LIB')
    vol = params.get('VOL')

    if not lib or not vol:
        print("[ERROR] LIB 또는 VOL 파라미터가 누락되었습니다.")
        return

    path = os.path.join(VOLUME_ROOT, vol, lib)
    os.makedirs(path, exist_ok=True)
    print(f"[INFO] 라이브러리 '{lib}'가 볼륨 '{vol}'에 생성되었습니다: {path}")

def CRTFILE(command):
    # 예: CRTFILE FILE(ACCTLIB/CUSTMAST),VOL-DISK01,ENT-100
    main_part, *others = command.replace('CRTFILE ', '').split(',')
    file_lib, file_name = main_part.replace('FILE(', '').replace(')', '').split('/')
    params = dict(item.split('-') for item in others if '-' in item)
    vol = params.get('VOL')

    if not vol:
        print("[ERROR] VOL 파라미터가 누락되었습니다.")
        return

    lib_path = os.path.join(VOLUME_ROOT, vol, file_lib)
    file_path = os.path.join(lib_path, file_name)

    if not os.path.exists(lib_path):
        print(f"[ERROR] 라이브러리 '{file_lib}'가 존재하지 않습니다. 먼저 CRTLIB 명령을 실행하세요.")
        return

    with open(file_path, 'w') as f:
        f.write("")  # 빈 데이터셋 생성

    print(f"[INFO] 파일 '{file_name}'가 라이브러리 '{file_lib}'에 생성되었습니다: {file_path}")
    
def WRKLIB():
    print(f"[INFO] '{VOLUME_ROOT}' 내의 모든 라이브러리 목록:")
    if not os.path.exists(VOLUME_ROOT):
        print("[INFO] 아직 생성된 볼륨이 없습니다.")
        return

    for vol in os.listdir(VOLUME_ROOT):
        vol_path = os.path.join(VOLUME_ROOT, vol)
        if os.path.isdir(vol_path):
            print(f" 볼륨: {vol}")
            for lib in os.listdir(vol_path):
                print(f"   - 라이브러리: {lib}")
                
def WRKSPLF():
    splf_root = os.path.join(VOLUME_ROOT, "SPLF")

    if not os.path.isdir(splf_root):
        print("[INFO] 현재 존재하는 스풀 파일이 없습니다.")
        return

    print("[INFO] 스풀 파일 목록:")
    for lib in os.listdir(splf_root):
        lib_path = os.path.join(splf_root, lib)
        if not os.path.isdir(lib_path):
            continue

        for logfile in os.listdir(lib_path):
            log_path = os.path.join(lib_path, logfile)
            size = os.path.getsize(log_path)
            mtime = datetime.fromtimestamp(os.path.getmtime(log_path)).strftime('%Y-%m-%d %H:%M:%S')
            print(f"  📄 {lib}/{logfile.ljust(20)} | 크기: {size:>6} Byte | 수정일: {mtime}")

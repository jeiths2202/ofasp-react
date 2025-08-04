#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
OpenASP Configuration Check Script
�ݒ�t�@�C���̌��؂ƃv���O�������̃J�E���g
"""

import sys
import json
import os
from datetime import datetime

def check_config_file(config_path):
    """
    �ݒ�t�@�C�����`�F�b�N���ď���Ԃ�
    
    Args:
        config_path (str): �ݒ�t�@�C���̃p�X
        
    Returns:
        dict: �`�F�b�N����
    """
    result = {
        'exists': False,
        'valid_json': False,
        'program_count': 0,
        'type_counts': {},
        'version': 'unknown',
        'error_message': None
    }
    
    try:
        # �t�@�C�����݃`�F�b�N
        if not os.path.exists(config_path):
            result['error_message'] = f"�ݒ�t�@�C����������܂���: {config_path}"
            return result
        
        result['exists'] = True
        
        # �t�@�C���ǂݍ���
        with open(config_path, 'r', encoding='utf-8') as f:
            data = json.load(f)
        
        result['valid_json'] = True
        
        # �v���O�������̉��
        programs = data.get('programs', {})
        result['program_count'] = len(programs)
        
        # �o�[�W�������
        result['version'] = data.get('version', 'unknown')
        
        # �^�C�v�ʃJ�E���g
        type_counts = {}
        for program_name, program_info in programs.items():
            program_type = program_info.get('TYPE', 'UNKNOWN')
            type_counts[program_type] = type_counts.get(program_type, 0) + 1
        
        result['type_counts'] = type_counts
        
        # �ǉ����
        result['description'] = data.get('description', '')
        result['last_updated'] = data.get('last_updated', '')
        
        # �p�b�P�[�W�\�����
        if 'package_structure' in data:
            result['package_structure'] = data['package_structure']
        
        # �^�C�v�ݒ���
        if 'type_settings' in data:
            result['type_settings'] = list(data['type_settings'].keys())
        
    except json.JSONDecodeError as e:
        result['error_message'] = f"JSON���@�G���[: {str(e)}"
    except Exception as e:
        result['error_message'] = f"�ݒ�t�@�C���ǂݍ��݃G���[: {str(e)}"
    
    return result

def print_config_summary(result):
    """
    �ݒ�t�@�C���̌������ʂ�\��
    
    Args:
        result (dict): check_config_file()�̌���
    """
    print("?? �ݒ�t�@�C����������")
    print("=" * 30)
    
    if not result['exists']:
        print("? �t�@�C���s����")
        if result['error_message']:
            print(f"   �G���[: {result['error_message']}")
        return
    
    print("? �t�@�C������")
    
    if not result['valid_json']:
        print("? JSON���@�G���[")
        if result['error_message']:
            print(f"   �G���[: {result['error_message']}")
        return
    
    print("? JSON���@����")
    print(f"?? ���v���O������: {result['program_count']}��")
    
    if result['version'] != 'unknown':
        print(f"?? �o�[�W����: {result['version']}")
    
    if result['type_counts']:
        print("?? �^�C�v�ʓ���:")
        for ptype, count in result['type_counts'].items():
            print(f"   ? {ptype}: {count}��")
    
    if 'type_settings' in result:
        print(f"??  �T�|�[�g�^�C�v: {', '.join(result['type_settings'])}")
    
    if result.get('last_updated'):
        print(f"?? �ŏI�X�V: {result['last_updated']}")

def validate_programs(config_path):
    """
    �v���O�����ݒ�̏ڍ׌���
    
    Args:
        config_path (str): �ݒ�t�@�C���̃p�X
        
    Returns:
        list: ���؃G���[�̃��X�g
    """
    errors = []
    
    try:
        with open(config_path, 'r', encoding='utf-8') as f:
            data = json.load(f)
        
        programs = data.get('programs', {})
        type_settings = data.get('type_settings', {})
        
        for program_name, program_info in programs.items():
            # �K�{�t�B�[���h�`�F�b�N
            required_fields = ['TYPE', 'PGM', 'DESCRIPTION']
            for field in required_fields:
                if field not in program_info:
                    errors.append(f"�v���O���� '{program_name}': �K�{�t�B�[���h '{field}' ���s��")
            
            # �^�C�v�ݒ�`�F�b�N
            program_type = program_info.get('TYPE')
            if program_type and program_type not in type_settings:
                errors.append(f"�v���O���� '{program_name}': ����`�̃^�C�v '{program_type}'")
            
            # Java �p�b�P�[�W���`�F�b�N
            if program_type == 'JAVA':
                pgm = program_info.get('PGM', '')
                if '.' in pgm and not pgm.startswith('com.openasp.'):
                    errors.append(f"�v���O���� '{program_name}': ��W��Java�p�b�P�[�W '{pgm}'")
    
    except Exception as e:
        errors.append(f"���ؒ��ɃG���[������: {str(e)}")
    
    return errors

def create_sample_config(output_path):
    """
    �T���v���ݒ�t�@�C���𐶐�
    
    Args:
        output_path (str): �o�̓t�@�C���p�X
    """
    sample_config = {
        "description": "SMED MAP�� MAIN �v���O���� �}�b�s���O�ݒ�",
        "version": "2.0",
        "last_updated": datetime.now().strftime("%Y-%m-%d"),
        "programs": {
            "MENU": {
                "TYPE": "JAVA",
                "PGM": "com.openasp.menu.MenuProgram",
                "DESCRIPTION": "���C�����j���[���"
            },
            "LOGO": {
                "TYPE": "JAVA",
                "PGM": "com.openasp.login.LoginProgram",
                "DESCRIPTION": "���O�C�����"
            },
            "PGM1": {
                "TYPE": "JAVA",
                "PGM": "com.openasp.core.PGM1",
                "DESCRIPTION": "��{���O�C�������v���O����"
            },
            "PGM2": {
                "TYPE": "JAVA",
                "PGM": "com.openasp.core.PGM2",
                "DESCRIPTION": "��{���[�U�[�v���O����"
            }
        },
        "type_settings": {
            "JAVA": {
                "executor": "java",
                "jar_path": "/home/aspuser/app/server/java_jars/ofasp.jar",
                "main_class": "com.openasp.launcher.OpenASPLauncher",
                "timeout": 60,
                "encoding": "UTF-8"
            },
            "COBOL": {
                "executor": "dlcall",
                "library_path": "/home/aspuser/app/server/cobol_modules",
                "timeout": 120,
                "encoding": "UTF-8"
            },
            "SHELL": {
                "executor": "bash",
                "script_path": "/home/aspuser/app/server/shell_modules",
                "timeout": 300,
                "encoding": "UTF-8"
            }
        }
    }
    
    try:
        with open(output_path, 'w', encoding='utf-8') as f:
            json.dump(sample_config, f, indent=2, ensure_ascii=False)
        print(f"? �T���v���ݒ�t�@�C������: {output_path}")
    except Exception as e:
        print(f"? �T���v���ݒ�t�@�C���������s: {e}")

def main():
    """���C���֐�"""
    if len(sys.argv) < 2:
        print("�g�p�@: python3 check_config.py <�ݒ�t�@�C���p�X> [�I�v�V����]")
        print("�I�v�V����:")
        print("  --validate    �ڍ׌��؂����s")
        print("  --create-sample <�p�X>  �T���v���ݒ�t�@�C���𐶐�")
        print("  --count-only  �v���O�������̂ݏo��")
        sys.exit(1)
    
    config_path = sys.argv[1]
    
    # �I�v�V��������
    if '--create-sample' in sys.argv:
        sample_path_idx = sys.argv.index('--create-sample') + 1
        if sample_path_idx < len(sys.argv):
            create_sample_config(sys.argv[sample_path_idx])
        else:
            create_sample_config('smed_pgm_sample.json')
        return
    
    # �ݒ�t�@�C���`�F�b�N
    result = check_config_file(config_path)
    
    if '--count-only' in sys.argv:
        # �v���O�������̂ݏo�́i�r���h�X�N���v�g�p�j
        print(result['program_count'])
        return
    
    # �ڍ׏��\��
    print_config_summary(result)
    
    # �ڍ׌���
    if '--validate' in sys.argv and result['valid_json']:
        print("\n?? �ڍ׌��ؒ�...")
        errors = validate_programs(config_path)
        
        if errors:
            print("??  ���؃G���[:")
            for error in errors:
                print(f"   ? {error}")
        else:
            print("? ���؊���: �G���[�Ȃ�")

if __name__ == "__main__":
    main()

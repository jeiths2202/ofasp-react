"""Setup configuration for ASP Manager."""

from setuptools import setup, find_packages
import os

# Read the README file
readme_path = os.path.join(os.path.dirname(__file__), 'README.md')
if os.path.exists(readme_path):
    with open(readme_path, 'r', encoding='utf-8') as f:
        long_description = f.read()
else:
    long_description = "ASP Manager - A professional curses-based system management tool"

# Read requirements
requirements_path = os.path.join(os.path.dirname(__file__), 'requirements.txt')
if os.path.exists(requirements_path):
    with open(requirements_path, 'r', encoding='utf-8') as f:
        requirements = [line.strip() for line in f.readlines() 
                       if line.strip() and not line.startswith('#')]
else:
    requirements = ['psutil>=5.9.0']

setup(
    name='aspmgr',
    version='1.0.0',
    description='A professional curses-based system management tool',
    long_description=long_description,
    long_description_content_type='text/markdown',
    author='ASP Manager Team',
    author_email='aspmgr@example.com',
    url='https://github.com/example/aspmgr',
    packages=find_packages(),
    include_package_data=True,
    package_data={
        'aspmgr': [
            'data/*.json',
            'data/*.yaml',
            'data/*.yml'
        ]
    },
    install_requires=requirements,
    python_requires='>=3.7',
    entry_points={
        'console_scripts': [
            'aspmgr=aspmgr.__main__:run',
        ],
    },
    classifiers=[
        'Development Status :: 4 - Beta',
        'Environment :: Console :: Curses',
        'Intended Audience :: System Administrators',
        'License :: OSI Approved :: MIT License',
        'Operating System :: POSIX :: Linux',
        'Operating System :: Unix',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.7',
        'Programming Language :: Python :: 3.8',
        'Programming Language :: Python :: 3.9',
        'Programming Language :: Python :: 3.10',
        'Programming Language :: Python :: 3.11',
        'Topic :: System :: Systems Administration',
        'Topic :: System :: Monitoring',
        'Topic :: Utilities',
    ],
    keywords='system management curses tui monitoring processes',
    project_urls={
        'Documentation': 'https://github.com/example/aspmgr/docs',
        'Source': 'https://github.com/example/aspmgr',
        'Tracker': 'https://github.com/example/aspmgr/issues',
    },
)
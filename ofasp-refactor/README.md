# 🚀 OpenASP Refactor

Modern web-based refactoring platform for migrating legacy ASP systems to open-source technologies.

[![TypeScript](https://img.shields.io/badge/TypeScript-007ACC?style=flat&logo=typescript&logoColor=white)](https://www.typescriptlang.org/)
[![React](https://img.shields.io/badge/React-20232A?style=flat&logo=react&logoColor=61DAFB)](https://reactjs.org/)
[![Tailwind CSS](https://img.shields.io/badge/Tailwind_CSS-38B2AC?style=flat&logo=tailwind-css&logoColor=white)](https://tailwindcss.com/)
[![Node.js](https://img.shields.io/badge/Node.js-43853D?style=flat&logo=node.js&logoColor=white)](https://nodejs.org/)

## ✨ Features

### 🔄 **Code Refactoring**
- **COBOL to Java**: Advanced syntax conversion with modern Java patterns
- **CL to Shell/JavaScript**: Command language migration with full compatibility
- **Real-time preview** with syntax highlighting and error detection

### 🖥️ **EBCDIC/ASCII Conversion**
- **Source Code Conversion**: Real-time conversion with SOSI handling
- **Dataset Conversion**: Batch processing for large files
- **Python Flask Backend**: High-performance conversion engine (port 3003)
- **Multiple Encodings**: US, JP, JAK, KEIS, KR support

### 💻 **ASP System Command Terminal**
- **Command History**: Navigate through last 10 commands with arrow keys
- **Auto-completion**: Tab key completion for ASP commands
- **Smart Focus**: Auto-focus on SMED map input fields
- **Real-time Info**: Live system status (user, volume, time)

### 🎯 **Core Features (핵심 기능)**

#### 1. **SJIS to Unicode / Unicode to SJIS 변환 기능**
- **완전한 양방향 변환**: 서버의 SJIS 파일과 웹 UI의 UTF-8 간 seamless 변환
- **일본어 텍스트 지원**: mojibake 없는 완벽한 더블바이트 문자 처리
- **Flask API 자동 처리**: `encoding='shift_jis'` 파라미터로 투명한 변환
- **파일 저장 최적화**: 확장자 없이 SJIS 형식으로 정확한 저장
- **실시간 검증**: 저장된 파일의 인코딩 자동 확인 및 검증

#### 2. **WebTerminal과 SMED Maps 연동**
- **CALL 명령어 지원**: `CALL PGM-TestProgram.TESTLIB,VOL-DISK01` 완전 구현
- **Java 프로그램 실행**: JAR 기반 ASP 프로그램의 직접 실행
- **SMED 맵 표시**: 80x24 터미널에서 완벽한 화면 맵 렌더링
- **Function Key 처리**: F1-F12 키를 Java 프로그램으로 전달
- **세션 관리**: 프로그램별 상태 유지 및 인터랙티브 처리

### 📚 **Documentation System**
- **710 ASP Commands** mapped to open-source alternatives
- **Interactive search** with filtering by category and priority
- **Technical implementation guides** with detailed technology stacks

### 🎨 **Modern Interface**
- **Dark/Light themes** with smooth transitions
- **Bilingual support** (Japanese/Korean) with context-aware switching
- **Responsive design** optimized for all screen sizes

### 🔒 **Security & Authentication**
- **Session-based authentication** with secure login system
- **Role-based access control** for different user levels
- **External access support** via ngrok with proper security measures

## 🚀 Quick Start

```bash
# Clone the repository
git clone https://github.com/jeiths2202/ofasp-react.git
cd ofasp-react/ofasp-refactor

# Install dependencies
npm install

# Start Python conversion service (port 3003)
cd python-service
FLASK_PORT=3003 python -c "from src.api.app import api; api.run()" &

# Start React application (port 3005)
cd ..
PORT=3005 npm start
```

Visit `http://localhost:3005` to access the application.

### 🎯 Core Features Access

#### SJIS/Unicode Conversion
- **SMED Map Editor**: Real-time Japanese text editing with SJIS encoding
- **File Operations**: Load/Save SMED files with automatic encoding conversion
- **Test Example**: Load "MAINMENU" from catalog to see Japanese text display

#### WebTerminal SMED Integration  
- **Terminal Access**: Built-in ASP command terminal
- **CALL Commands**: Execute `CALL PGM-TestProgram.TESTLIB,VOL-DISK01`
- **Interactive Maps**: Full SMED map display with Function Key support

## 📖 Available Scripts

| Command | Description |
|---------|-------------|
| `npm start` | Start development server on port 3005 |
| `npm run start:ja` | Start with Japanese as default language |
| `npm run start:ko` | Start with Korean as default language |
| `npm run build` | Build for production |
| `npm test` | Run test suite |

## 🏗️ Architecture

```
OpenASP Refactor
├── 🎨 Frontend (React + TypeScript)
│   ├── Code refactoring interface
│   ├── Documentation system
│   ├── SMED Map Editor (with SJIS support)
│   └── WebTerminal (with CALL command integration)
├── 🔧 Backend Services
│   ├── Python conversion service (Flask, port 3003)
│   ├── Flask API Server (port 8000) - SJIS/UTF-8 conversion
│   ├── Java Program Executor (JAR-based ASP programs)
│   ├── File server (Express.js)
│   ├── RAG system (TensorFlow.js)
│   └── Authentication layer
├── 💾 Data Layer
│   ├── SMED Files (SJIS encoding in /volume/)
│   ├── Catalog.json (Resource management)
│   ├── Java Programs (JAR files with ASP logic)
│   └── Configuration files (smed_pgm.json)
└── 📚 Documentation
    ├── ASP command mapping (710 commands)
    ├── Technical implementation guides
    ├── SJIS/Unicode conversion documentation
    └── WebTerminal/SMED integration guides
```

## 🎯 Core Modules

### Code Refactoring Engine
- **COBOL Parser**: Advanced syntax analysis and conversion
- **CL Translator**: Command language to modern script migration
- **EBCDIC Converter**: Python Flask backend with SOSI handling
- **Validation System**: Real-time error detection and suggestions

### SJIS/Unicode Conversion Engine ⭐
- **Automatic Encoding Detection**: SJIS 파일 자동 감지 및 UTF-8 변환
- **Flask API Integration**: `encoding='shift_jis'` 기반 transparent conversion
- **File Operations**: 확장자 관리 없이 정확한 SJIS 저장
- **Real-time Validation**: 저장된 파일의 인코딩 검증 시스템
- **Japanese Text Support**: mojibake 방지 완벽한 더블바이트 처리

### WebTerminal SMED Integration ⭐  
- **CALL Command Processor**: ASP CALL 명령어 완전 구현
- **Java Program Executor**: JAR 기반 프로그램 실행 엔진
- **SMED Map Renderer**: 80x24 터미널 화면 맵 표시 시스템
- **Function Key Handler**: F1-F12 키 이벤트 Java 프로그램 전달
- **Session Management**: 프로그램별 상태 유지 및 인터랙티브 처리

### ASP System Terminal
- **Command Interface**: Interactive terminal for ASP system commands
- **History Navigation**: Arrow key navigation through command history
- **Auto-completion**: Smart Tab completion for ASP commands
- **Cursor Management**: Auto-focus on SMED input fields

### Documentation Platform
- **Command Mapping**: 710 ASP commands → Open source alternatives
- **Technology Guides**: Node.js, PostgreSQL, React, Docker implementations
- **Interactive Search**: Filter by complexity, priority, and category

### RAG-Powered Chat
- **TensorFlow.js Integration**: Real vector embeddings for document search
- **Multilingual Support**: Japanese/Korean context-aware responses
- **PDF Processing**: Automatic content extraction and indexing

## 🌐 Technology Stack

| Layer | Technology |
|-------|------------|
| **Frontend** | React 19, TypeScript, Tailwind CSS |
| **Backend** | Python Flask, Node.js, Express.js, TensorFlow.js |
| **Database** | PostgreSQL, Vector embeddings |
| **Authentication** | JWT, bcrypt, Session management |
| **Deployment** | Docker, nginx, PM2 |
| **External** | ngrok, bore.pub (tunneling) |

## 📱 Screenshots

| Feature | Preview |
|---------|---------|
| **COBOL Refactoring** | Modern IDE-like interface with real-time conversion |
| **Command Mapping** | Interactive table with 710 ASP commands |
| **Documentation** | Comprehensive guides with search functionality |
| **Chat System** | RAG-powered Q&A with Japanese/Korean support |

## 🔧 Configuration

### Environment Variables
```bash
# React Frontend
REACT_APP_DEFAULT_LANG=ja                               # Default language (ja/ko)
PORT=3005                                              # Application port
HOST=0.0.0.0                                          # Host binding
REACT_APP_PYTHON_CONVERTER_URL=http://localhost:3003   # Python service URL

# Python Service
FLASK_PORT=3003                                        # Python service port
CODEPAGE_BASE_PATH=/home/aspuser/app/ofasp-refactor/public/codepages

# Flask API Server (SJIS/Unicode Integration)
API_SERVER_PORT=8000                                   # SJIS conversion API port
SMED_FILES_PATH=/home/aspuser/app/volume               # SMED files directory
CATALOG_JSON_PATH=/home/aspuser/app/config/catalog.json # Resource catalog
JAVA_JAR_PATH=/home/aspuser/app/server/java_jars/ofasp.jar # ASP Java programs
```

### Core Features Configuration

#### SJIS/Unicode Conversion
```python
# Flask API encoding settings
DEFAULT_ENCODING = 'shift_jis'
ENCODING_ERRORS = 'replace'
AUTO_EXTENSION_HANDLING = False  # 확장자 자동 추가 방지
```

#### WebTerminal SMED Integration
```json
{
  "smed_pgm_mapping": "/home/aspuser/app/src/smed_pgm.json",
  "java_execution_timeout": 30,
  "function_keys": ["F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12"],
  "terminal_size": {"cols": 80, "rows": 24}
}
```

### Language Support
- **Japanese (ja)**: Default interface language
- **English (en)**: Alternative interface language  
- **Development Language**: Japanese for comments and variable names
- **Claude Communication**: Korean (for this project context)

## 📚 Documentation

### Core Features Documentation

For detailed information about the core features, please refer to:

#### SJIS to Unicode / Unicode to SJIS 변환 기능
- **Full Documentation**: [`docs/SJIS_UNICODE_CONVERSION.md`](./docs/SJIS_UNICODE_CONVERSION.md)
- **Key Topics**: 
  - Technical background and problem analysis
  - Implementation details (Flask API, React components)
  - File encoding management and extension handling
  - Testing methods and troubleshooting
  - Performance optimization and security considerations

#### WebTerminal과 SMED Maps 연동
- **Full Documentation**: [`docs/WEBTERMINAL_SMED_INTEGRATION.md`](./docs/WEBTERMINAL_SMED_INTEGRATION.md)
- **Key Topics**:
  - System architecture and component integration
  - CALL command processing and Java program execution
  - SMED map rendering and Function Key handling
  - Session management and interactive processing
  - Configuration files and testing procedures

### AI Code Agent Memory Reset Support

These documentation files are designed to help AI Code Agents understand the project state immediately after memory reset or system restart. They include:

- **Complete implementation details** with code examples
- **File locations** and directory structures  
- **Configuration requirements** and environment setup
- **Testing procedures** and validation methods
- **Troubleshooting guides** and common issues
- **Extension possibilities** and future development

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- **AS/400 Community** for legacy system insights
- **Open Source Technologies** for modern alternatives
- **TensorFlow.js** for ML-powered document processing

---

<div align="center">

**OpenASP Refactor** - Bridging Legacy and Modern

Built with ❤️ by the OpenASP Team

</div>
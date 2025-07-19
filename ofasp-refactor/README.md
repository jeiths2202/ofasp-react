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

# Start development server
npm start
```

Visit `http://localhost:3005` to access the application.

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
│   └── Interactive command mapping
├── 🔧 Backend Services
│   ├── Python conversion service (Flask, port 3003)
│   ├── File server (Express.js)
│   ├── RAG system (TensorFlow.js)
│   └── Authentication layer
└── 📚 Documentation
    ├── ASP command mapping (710 commands)
    ├── Technical implementation guides
    └── Migration best practices
```

## 🎯 Core Modules

### Code Refactoring Engine
- **COBOL Parser**: Advanced syntax analysis and conversion
- **CL Translator**: Command language to modern script migration
- **EBCDIC Converter**: Python Flask backend with SOSI handling
- **Validation System**: Real-time error detection and suggestions

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
```

### Language Support
- **Japanese (ja)**: Default interface language
- **English (en)**: Alternative interface language  
- **Development Language**: Japanese for comments and variable names
- **Claude Communication**: Korean (for this project context)

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
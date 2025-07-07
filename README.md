# 🌟 OpenASP

Complete modernization platform for migrating legacy ASP systems to cloud-native open-source technologies.

[![TypeScript](https://img.shields.io/badge/TypeScript-007ACC?style=flat&logo=typescript&logoColor=white)](https://www.typescriptlang.org/)
[![React](https://img.shields.io/badge/React-20232A?style=flat&logo=react&logoColor=61DAFB)](https://reactjs.org/)
[![Python](https://img.shields.io/badge/Python-3776AB?style=flat&logo=python&logoColor=white)](https://www.python.org/)
[![TensorFlow.js](https://img.shields.io/badge/TensorFlow.js-FF6F00?style=flat&logo=tensorflow&logoColor=white)](https://www.tensorflow.org/js)
[![Docker](https://img.shields.io/badge/Docker-2496ED?style=flat&logo=docker&logoColor=white)](https://www.docker.com/)

## 🏗️ System Architecture

```
OpenASP
├── 🖥️  OpenASP SMED Interface (Port 3000)
│   ├── Terminal-style authentication
│   ├── SMED map-based workflows
│   └── Legacy program execution
├── 🤖 ASP Manager (Port 3007)
│   ├── AI-powered chat assistant
│   ├── RAG-based documentation
│   └── Intelligent system management
├── 🔧 OpenASP Refactor (Port 3005)
│   ├── COBOL to Java conversion
│   ├── CL to Shell/JavaScript migration
│   └── 710 command mapping system
└── ⚡ API Server (Port 8000)
    ├── Multi-language program execution
    ├── Real-time system monitoring
    └── Legacy integration layer
```

## 🚀 Quick Start

### Prerequisites
- Node.js 18+ and npm
- Python 3.8+ with pip
- Git

### Installation
```bash
# Clone the repository
git clone https://github.com/jeiths2202/ofasp-react.git
cd ofasp-react

# Install frontend dependencies
cd ofasp-refactor && npm install && cd ..
cd asp-manager && npm install && cd ..

# Install Python dependencies
pip install flask psutil

# Start all services
./start-services.sh  # or start individually
```

### Individual Service Startup
```bash
# OpenASP Refactor (Port 3005)
cd ofasp-refactor && npm start

# ASP Manager (Port 3007) 
cd asp-manager && npm start

# API Server (Port 8000)
python api_server.py
```

## 🎯 Core Applications

### 🔧 [OpenASP Refactor](./ofasp-refactor) - Legacy Code Migration
- **COBOL → Java**: Advanced syntax conversion with modern patterns
- **CL → Shell/JavaScript**: Command language modernization
- **710 ASP Commands**: Complete mapping to open-source alternatives
- **Interactive Documentation**: Searchable implementation guides

### 🤖 [ASP Manager](./asp-manager) - AI-Powered Management
- **RAG Chat System**: TensorFlow.js-powered Q&A with ASP manuals
- **Multilingual Support**: Japanese/Korean auto-detection and responses
- **Real-time Monitoring**: System performance and resource tracking
- **Smart Documentation**: AI-assisted technical documentation

### 🖥️ OpenASP SMED - Legacy Interface
- **Terminal-Style UI**: Familiar AS/400-like user experience
- **SMED Map Integration**: Map-based authentication and navigation
- **Program Execution**: Support for Java, COBOL, and Shell programs
- **User Management**: Role-based access control system

## 🌐 Technology Stack

| Component | Technologies |
|-----------|-------------|
| **Frontend** | React 19, TypeScript, Tailwind CSS |
| **AI/ML** | TensorFlow.js, Universal Sentence Encoder, RAG |
| **Backend** | Node.js, Express.js, Flask (Python) |
| **Database** | PostgreSQL, Vector embeddings, JSON configs |
| **Languages** | COBOL, Java, JavaScript, Python, Shell |
| **Deployment** | Docker, nginx, PM2, ngrok |
| **Monitoring** | psutil, real-time metrics, performance tracking |

## ✨ Key Features

### 🔄 **Legacy Modernization**
- **Code Conversion**: Automated COBOL to Java transformation
- **Command Migration**: 710 ASP commands → Open source alternatives
- **Architecture Mapping**: Mainframe concepts → Cloud-native patterns
- **Compatibility Layer**: Smooth transition with minimal disruption

### 🧠 **AI-Powered Assistance**
- **Document Intelligence**: RAG system with Japanese ASP manuals
- **Contextual Help**: Chain-of-thought reasoning for technical queries
- **Multilingual Support**: Auto-detecting Japanese/Korean responses
- **Performance Optimization**: Smart caching and vector embeddings

### 🎨 **Modern User Experience**
- **Responsive Design**: Mobile-first approach with desktop optimization
- **Dark/Light Themes**: System preference matching and manual toggle
- **Accessibility**: WCAG 2.1 compliant interfaces
- **Progressive Enhancement**: Works offline with service workers

### 🔒 **Enterprise Security**
- **Session Management**: Secure authentication with role-based access
- **External Access**: ngrok integration with security controls
- **Data Privacy**: Client-side processing for sensitive documents
- **Audit Logging**: Comprehensive activity tracking

## 📊 Migration Benefits

| Aspect | Before (ASP) | After (OpenASP) |
|--------|----------------|-----------------|
| **Infrastructure** | Mainframe hardware | Cloud-native containers |
| **Development** | Green screen terminals | Modern web interfaces |
| **Languages** | COBOL, RPG, CL | Java, JavaScript, Python |
| **Database** | DB2/400 | PostgreSQL with modern ORM |
| **Scalability** | Vertical scaling | Horizontal auto-scaling |
| **Cost** | High hardware costs | Pay-as-you-use cloud |
| **Integration** | Limited APIs | RESTful APIs, microservices |
| **Maintenance** | Specialized skills | Modern development practices |

## 🔗 Service Endpoints

| Service | URL | Description |
|---------|-----|-------------|
| **OpenASP Refactor** | `http://localhost:3005` | Code migration platform |
| **ASP Manager** | `http://localhost:3007` | AI management interface |
| **SMED Interface** | `http://localhost:3000` | Legacy terminal UI |
| **API Server** | `http://localhost:8000` | Backend services |

## 📚 Documentation

- **[Migration Guide](./docs/migration-guide.md)**: Step-by-step modernization process
- **[API Reference](./docs/api-reference.md)**: Complete API documentation
- **[Command Mapping](./docs/command-mapping.md)**: ASP to OpenASP command reference
- **[Deployment Guide](./docs/deployment.md)**: Production deployment instructions

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- **ASP Community** for invaluable legacy system insights
- **Open Source Ecosystem** for providing modern alternatives
- **TensorFlow.js Team** for enabling client-side AI capabilities
- **React Community** for building amazing developer tools

---

<div align="center">

**OpenASP** - Bridging Legacy and Future

*From Mainframe to Modern: Empowering Digital Transformation*

Built with ❤️ by the OpenASP Team

</div>
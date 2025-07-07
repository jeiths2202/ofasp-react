# 🤖 ASP Manager

Intelligent AI-powered management interface for OpenASP systems with RAG-based documentation assistance.

[![TypeScript](https://img.shields.io/badge/TypeScript-007ACC?style=flat&logo=typescript&logoColor=white)](https://www.typescriptlang.org/)
[![React](https://img.shields.io/badge/React-20232A?style=flat&logo=react&logoColor=61DAFB)](https://reactjs.org/)
[![TensorFlow.js](https://img.shields.io/badge/TensorFlow.js-FF6F00?style=flat&logo=tensorflow&logoColor=white)](https://www.tensorflow.org/js)
[![AI Chat](https://img.shields.io/badge/AI_Chat-00D4AA?style=flat&logo=openai&logoColor=white)](#)

## ✨ Features

### 🧠 **AI-Powered Chat Assistant**
- **RAG System**: Real vector embeddings with TensorFlow.js Universal Sentence Encoder
- **Multilingual Support**: Auto-detects and responds in Japanese/Korean
- **PDF Processing**: Extracts and indexes ASP manuals for intelligent Q&A
- **Context-Aware**: Maintains conversation context with chain-of-thought reasoning

### 📊 **System Management**
- **Real-time Monitoring**: Live system status and performance metrics
- **Dark/Light Themes**: Automatic theme matching with system preferences
- **Responsive Design**: Optimized for desktop and mobile devices
- **Session Management**: Secure authentication with persistent sessions

### 📚 **Documentation Intelligence**
- **ASP Manual Integration**: Processes Japanese ASP system manuals
- **Semantic Search**: Advanced document retrieval with vector similarity
- **Auto-Indexing**: Automatic content extraction from PDF documents
- **Smart Caching**: Optimized performance with intelligent caching

## 🚀 Quick Start

```bash
# Clone the repository
git clone https://github.com/jeiths2202/ofasp-react.git
cd ofasp-react/asp-manager

# Install dependencies
npm install

# Start development server
npm start
```

Visit `http://localhost:3007` to access the AI management interface.

## 🏗️ Architecture

```
ASP Manager
├── 🤖 AI Chat System
│   ├── TensorFlow.js Vector DB
│   ├── RAG Implementation
│   └── Multilingual NLP
├── 📊 Management Dashboard
│   ├── System monitoring
│   ├── Performance metrics
│   └── User management
└── 📚 Knowledge Base
    ├── ASP manual processing
    ├── Vector embeddings
    └── Semantic search
```

## 🎯 Core Technologies

### RAG (Retrieval-Augmented Generation)
- **TensorFlow.js**: Client-side ML with Universal Sentence Encoder
- **Vector Database**: Float32Array-based similarity search
- **Document Chunking**: Intelligent text segmentation for optimal retrieval
- **Caching System**: Performance optimization with smart caching

### Natural Language Processing
- **Multilingual Detection**: Automatic language identification
- **Context Preservation**: Chain-of-thought reasoning for coherent responses
- **PDF Extraction**: Advanced text processing from Japanese technical documents

## 📖 Available Scripts

| Command | Description |
|---------|-------------|
| `npm start` | Start development server on port 3007 |
| `npm run build` | Build for production |
| `npm test` | Run test suite |

## 🔧 Configuration

### Environment Variables
```bash
PORT=3007                           # Application port
DANGEROUSLY_DISABLE_HOST_CHECK=true # Enable ngrok access
BROWSER=none                        # Disable auto browser opening
```

### RAG System Configuration
```javascript
// Vector DB settings
const vectorConfig = {
  model: '@tensorflow-models/universal-sentence-encoder',
  embeddingDimension: 512,
  similarityThreshold: 0.5,
  maxResults: 5
};
```

## 🌐 Technology Stack

| Layer | Technology |
|-------|------------|
| **Frontend** | React 19, TypeScript, Tailwind CSS |
| **AI/ML** | TensorFlow.js, Universal Sentence Encoder |
| **Backend** | Node.js, Express.js, pdf-parse |
| **Database** | Vector embeddings, Client-side storage |
| **Authentication** | Session-based, JWT tokens |
| **External** | ngrok support for remote access |

## 💬 Chat System Features

### Intelligent Document Retrieval
- **Semantic Search**: Vector similarity matching for relevant content
- **Context Awareness**: Maintains conversation history and context
- **Source Attribution**: Shows document sources for transparency
- **Real-time Processing**: Instant responses with optimized performance

### Multilingual Capabilities
- **Auto-Detection**: Identifies query language (Japanese/Korean)
- **Native Responses**: Responds in the detected language
- **Technical Accuracy**: Preserves technical terminology across languages
- **Context Switching**: Seamless language transitions within conversations

## 🔒 Security Features

- **Session Authentication**: Secure login with session management
- **Host Validation**: Configurable host checking for external access
- **CORS Protection**: Proper cross-origin resource sharing setup
- **Data Privacy**: Client-side processing for sensitive documents

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/ai-enhancement`)
3. Commit your changes (`git commit -m 'Add AI enhancement'`)
4. Push to the branch (`git push origin feature/ai-enhancement`)
5. Open a Pull Request

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- **TensorFlow.js Team** for client-side ML capabilities
- **Universal Sentence Encoder** for semantic embeddings
- **AS/400 Documentation** for comprehensive manual coverage
- **Open Source Community** for foundational technologies

---

<div align="center">

**ASP Manager** - AI-Powered Legacy System Management

Built with 🧠 by the OpenASP Team

</div>
# Python Conversion Service

Production-ready EBCDIC/ASCII conversion service following **CODING_RULES.md**

## ✨ Features

- **Zero Hardcoding**: All configuration via environment variables
- **Clean Architecture**: Separated concerns, no test code in production
- **High Performance**: Caching and optimized conversion algorithms
- **RESTful API**: JSON-based API with proper error handling
- **Fallback Support**: Web interface falls back to TypeScript if needed

## 🏗️ Architecture

```
python-service/
├── config/
│   └── config.py          # Configuration management
├── src/
│   ├── constants/
│   │   └── conversion.py   # All constants and validation
│   ├── utils/
│   │   └── logger.py       # Configurable logging
│   ├── converters/
│   │   └── ebcdic_converter.py  # Clean converter (no hardcoding)
│   └── api/
│       └── app.py          # Flask API service
├── tests/
│   └── test_converter.py   # Separate test files
├── .env.example           # Configuration template
├── .env                   # Actual configuration
├── requirements.txt       # Python dependencies
├── main.py               # Service entry point
└── start_service.sh      # Production startup script
```

## 🚀 Quick Start

### 1. Setup Environment

```bash
# Copy configuration template
cp .env.example .env

# Edit configuration as needed
nano .env

# Install dependencies
pip install -r requirements.txt
```

### 2. Start Service

```bash
# Production startup
./start_service.sh

# Or direct start
python main.py
```

### 3. Verify Service

```bash
# Health check
curl http://localhost:8001/health

# Service info
curl http://localhost:8001/api/v1/info

# Run integration tests
python test_simple.py
```

## 🔧 Configuration

All configuration is via environment variables (`.env` file):

```bash
# Server Configuration
FLASK_PORT=8001
FLASK_HOST=0.0.0.0
FLASK_DEBUG=false

# Code Page Configuration
CODEPAGE_BASE_PATH=../public/codepages
CODEPAGE_CACHE_SIZE=10

# Conversion Configuration
DEFAULT_ENCODING=US
DEFAULT_RECORD_LENGTH=80
MAX_INPUT_SIZE=1048576

# Security Configuration
CORS_ORIGINS=http://localhost:3000,http://localhost:3005,http://localhost:3007
API_KEY_REQUIRED=false

# Performance Configuration
WORKER_PROCESSES=4
THREAD_POOL_SIZE=10
```

## 📡 API Reference

### Health Check
```
GET /health
```

### Service Information
```
GET /api/v1/info
```

### EBCDIC to ASCII Conversion
```
POST /api/v1/convert/ebcdic-to-ascii
Content-Type: application/json

{
  "input_data": "C4C9E2D7D3C1E8",
  "encoding": "US",
  "sosi_flag": false,
  "out_sosi_flag": false,
  "rlen": 80
}
```

### ASCII to EBCDIC Conversion
```
POST /api/v1/convert/ascii-to-ebcdic
Content-Type: application/json

{
  "input_data": "DISPLAY",
  "encoding": "US",
  "sosi_flag": false,
  "out_sosi_flag": false,
  "rlen": 80
}
```

## 🔗 Integration with OpenASP AX

The OpenASP AX web interface (http://localhost:3005) automatically uses this Python service:

1. **Primary**: Calls Python service for conversion
2. **Fallback**: Falls back to TypeScript version if Python service unavailable
3. **Logging**: Shows conversion source in debug console

### Source Conversion (ソース変換)
- Uses Python service for EBCDIC file conversion
- Supports SOSI processing for Japanese characters
- Handles large files efficiently

### Dataset Conversion (データセット変換)
- Processes datasets using Python service
- Supports various layouts and formats
- Provides detailed conversion logging

## 🧪 Testing

### Unit Tests
```bash
python tests/test_converter.py
```

### Integration Tests
```bash
python test_simple.py
```

### Manual Testing
```bash
# Test EBCDIC to ASCII
curl -X POST http://localhost:8001/api/v1/convert/ebcdic-to-ascii \
  -H "Content-Type: application/json" \
  -d '{"input_data": "C4C9E2D7D3C1E8", "encoding": "US", "sosi_flag": false, "rlen": 80}'

# Test ASCII to EBCDIC
curl -X POST http://localhost:8001/api/v1/convert/ascii-to-ebcdic \
  -H "Content-Type: application/json" \
  -d '{"input_data": "DISPLAY", "encoding": "US", "sosi_flag": false, "rlen": 80}'
```

## 📋 Supported Encodings

- **US**: English (ASCII)
- **JP**: Japanese (Shift-JIS)
- **JAK**: Japanese (Alternative)
- **KEIS**: KEIS encoding
- **KR**: Korean (uses US as fallback)

## 🛡️ Security Features

- **CORS Protection**: Configurable allowed origins
- **API Key Support**: Optional API key authentication
- **Input Validation**: Strict parameter validation
- **Error Handling**: Secure error messages
- **Rate Limiting**: Configurable request limits

## 📊 Performance Features

- **Caching**: LRU cache for codepage tables
- **Async Processing**: Non-blocking request handling
- **Memory Optimization**: Efficient byte processing
- **Timeout Protection**: Configurable request timeouts

## 🔄 Integration Architecture

```
OpenASP AX (Port 3005)
    ↓
Python Service (Port 8001)
    ↓
Codepage Files (../public/codepages/)
    ↓
EBCDIC/ASCII Conversion
```

## 📝 Logs

Service logs are configurable via environment variables:

```bash
# Default log file
LOG_FILE=conversion_service.log

# Log level
LOG_LEVEL=INFO

# Log format
LOG_FORMAT=%(asctime)s - %(name)s - %(levelname)s - %(message)s
```

## 🚨 Error Handling

The service provides comprehensive error handling:

- **Input Validation**: Invalid hex strings, encodings, parameters
- **File Access**: Missing codepage files, permission issues
- **Conversion Errors**: Malformed data, unsupported characters
- **Service Errors**: Memory limits, timeout issues

## 🔧 Production Deployment

### Using Gunicorn
```bash
gunicorn --bind 0.0.0.0:8001 --workers 4 src.api.app:api.app
```

### Using Docker
```dockerfile
FROM python:3.10-slim
WORKDIR /app
COPY requirements.txt .
RUN pip install -r requirements.txt
COPY . .
CMD ["python", "main.py"]
```

## 📈 Monitoring

The service exposes metrics for monitoring:

- **Health endpoint**: `/health`
- **Service info**: `/api/v1/info`
- **Request logging**: Configurable log levels
- **Error tracking**: Detailed error messages

---

## 🎯 Key Improvements

### From Original Code
- ❌ **Removed**: All hardcoded paths and values
- ❌ **Removed**: Test code mixed with production code
- ❌ **Removed**: Print statements and debug output
- ✅ **Added**: Configuration management
- ✅ **Added**: Proper error handling
- ✅ **Added**: RESTful API
- ✅ **Added**: Comprehensive logging
- ✅ **Added**: Input validation
- ✅ **Added**: Performance optimizations

### Follows CODING_RULES.md
- ✅ **Zero hardcoding tolerance**
- ✅ **Environment variable configuration**
- ✅ **Constants files for all values**
- ✅ **Proper project structure**
- ✅ **Separated test files**
- ✅ **Configuration validation**

---

Built with ❤️ following OpenASP coding standards
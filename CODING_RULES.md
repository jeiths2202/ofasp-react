# CODING RULES - NO HARDCODING POLICY

## 🚨 CRITICAL: ZERO HARDCODING TOLERANCE

This document establishes **mandatory** coding standards to eliminate hardcoding practices and ensure maintainable, configurable code.

---

## 1. ABSOLUTE PROHIBITIONS

### ❌ NEVER HARDCODE:
- API URLs, endpoints, or base URLs
- Port numbers, IP addresses, hostnames
- Database connection strings
- API keys, tokens, secrets
- File paths (absolute paths)
- Magic numbers (numeric literals)
- String literals used multiple times
- Color codes, CSS values
- Timeout values, retry counts
- Environment-specific configurations

### ❌ EXAMPLES OF FORBIDDEN CODE:
```javascript
// FORBIDDEN - Hardcoded values
const apiUrl = "https://api.example.com/v1";
const port = 3000;
const maxRetries = 5;
const primaryColor = "#007bff";
```

---

## 2. MANDATORY REQUIREMENTS

### ✅ ALWAYS USE:

#### A. Environment Variables (.env)
```bash
# .env
API_BASE_URL=https://api.example.com/v1
PORT=3000
DATABASE_URL=postgresql://user:pass@localhost:5432/db
JWT_SECRET=your-secret-key
```

#### B. Configuration Files
```javascript
// config/config.js
export const config = {
  api: {
    baseUrl: process.env.API_BASE_URL || 'http://localhost:3000',
    timeout: parseInt(process.env.API_TIMEOUT) || 5000,
    retries: parseInt(process.env.MAX_RETRIES) || 3
  },
  server: {
    port: process.env.PORT || 3000,
    host: process.env.HOST || 'localhost'
  }
};
```

#### C. Constants Files
```javascript
// constants/index.js
export const API_ENDPOINTS = {
  USERS: '/users',
  AUTH: '/auth',
  PRODUCTS: '/products'
};

export const UI_CONSTANTS = {
  DEFAULT_PAGE_SIZE: 20,
  MAX_FILE_SIZE: 5 * 1024 * 1024, // 5MB
  DEBOUNCE_DELAY: 300
};

export const COLORS = {
  PRIMARY: 'var(--color-primary)',
  SECONDARY: 'var(--color-secondary)',
  ERROR: 'var(--color-error)'
};
```

#### D. Data Files (JSON/YAML)
```json
// data/settings.json
{
  "ui": {
    "theme": "default",
    "animations": true,
    "language": "en"
  },
  "features": {
    "enableBetaFeatures": false,
    "maxUploadSize": "5MB"
  }
}
```

---

## 3. PROJECT STRUCTURE REQUIREMENTS

### 🏗️ OpenASP Ecosystem Architecture:
```
OpenASP/
├── ofasp-refactor/ (Port 3005)
│   ├── src/
│   │   ├── components/
│   │   ├── pages/
│   │   │   ├── CobolAXPage.tsx
│   │   │   ├── ClAXPage.tsx
│   │   │   ├── AITransformPage.tsx
│   │   │   └── ToolsPage.tsx
│   │   ├── utils/
│   │   │   └── encodingConverter.ts
│   │   ├── constants/
│   │   ├── config/
│   │   └── data/
│   ├── public/
│   │   └── codepages/
│   └── server/
├── asp-manager/ (Port 3007)
│   ├── AI-powered chat assistant
│   ├── RAG-based documentation
│   └── Intelligent system management
├── smed-interface/ (Port 3000)
│   ├── Terminal-style authentication
│   ├── SMED map-based workflows
│   └── Legacy program execution
└── api-server/ (Port 8000)
    ├── Multi-language program execution
    ├── Real-time system monitoring
    └── Legacy integration layer
```

### 🏗️ Required Directory Structure:
```
project/
├── src/
│   ├── components/
│   ├── pages/
│   ├── utils/
│   ├── constants/
│   ├── config/
│   └── data/
├── public/
├── server/
├── .env.example
├── .env
└── CODING_RULES.md
```

---

## 4. CODE REVIEW CHECKLIST

### ✅ Before submitting ANY code, verify:

- [ ] **No hardcoded URLs or endpoints**
- [ ] **No hardcoded port numbers**
- [ ] **No magic numbers (use named constants)**
- [ ] **No hardcoded strings (use i18n or constants)**
- [ ] **No hardcoded file paths**
- [ ] **No hardcoded colors or CSS values**
- [ ] **All configurations use environment variables**
- [ ] **All repeated values are in constants**
- [ ] **Environment variables have fallback defaults**
- [ ] **Configuration is properly typed/validated**

---

## 5. IMPLEMENTATION PATTERNS

### ✅ CORRECT IMPLEMENTATION:

#### A. API Configuration
```javascript
// config/api.js
import { config } from './config.js';

export const apiConfig = {
  baseURL: config.api.baseUrl,
  timeout: config.api.timeout,
  headers: {
    'Content-Type': 'application/json',
    'Authorization': `Bearer ${process.env.API_TOKEN}`
  }
};
```

#### B. Database Configuration
```javascript
// config/database.js
export const dbConfig = {
  host: process.env.DB_HOST || 'localhost',
  port: process.env.DB_PORT || 5432,
  database: process.env.DB_NAME || 'myapp',
  username: process.env.DB_USER || 'user',
  password: process.env.DB_PASSWORD,
  ssl: process.env.NODE_ENV === 'production'
};
```

#### C. UI Constants
```javascript
// constants/ui.js
export const BREAKPOINTS = {
  MOBILE: '768px',
  TABLET: '1024px',
  DESKTOP: '1200px'
};

export const ANIMATION_DURATION = {
  FAST: 150,
  NORMAL: 300,
  SLOW: 500
};
```

---

## 6. ENVIRONMENT-SPECIFIC CONFIGURATIONS

### 🔧 Required .env.example:
```bash
# API Configuration
API_BASE_URL=https://api.example.com/v1
API_TOKEN=your-api-token-here
API_TIMEOUT=5000

# Server Configuration
PORT=3000
HOST=localhost
NODE_ENV=development

# Database Configuration
DB_HOST=localhost
DB_PORT=5432
DB_NAME=myapp
DB_USER=username
DB_PASSWORD=password

# Feature Flags
ENABLE_ANALYTICS=false
ENABLE_DEBUG_MODE=true
MAX_UPLOAD_SIZE=5242880
```

---

## 7. VALIDATION RULES

### 🔍 Mandatory Validation:
```javascript
// utils/envValidator.js
export function validateEnvironment() {
  const required = [
    'API_BASE_URL',
    'DATABASE_URL',
    'JWT_SECRET'
  ];
  
  const missing = required.filter(key => !process.env[key]);
  
  if (missing.length > 0) {
    throw new Error(`Missing required environment variables: ${missing.join(', ')}`);
  }
}
```

---

## 8. ENFORCEMENT MECHANISMS

### 🚫 CODE REJECTION CRITERIA:
Any code containing hardcoded values will be **immediately rejected** and must be refactored before acceptance.

### 🤖 Automated Checks:
- ESLint rules for detecting magic numbers
- Pre-commit hooks for hardcoding detection
- CI/CD pipeline validation
- Code review automated comments

---

## 9. MIGRATION GUIDE (For Existing Projects)

### 🔄 Step-by-Step Refactoring:

1. **Identify Hardcoded Values**
   ```bash
   # Search for common hardcoding patterns
   grep -r "http://" src/
   grep -r "https://" src/
   grep -r ":3000" src/
   grep -r "#[0-9a-fA-F]" src/
   ```

2. **Create Configuration Structure**
   - Set up config/ directory
   - Create constants/ directory
   - Add .env and .env.example files

3. **Extract Hardcoded Values**
   - Move URLs to config files
   - Convert magic numbers to constants
   - Extract strings to constants/i18n

4. **Validate Configuration**
   - Add environment validation
   - Test with different configurations
   - Update documentation

---

## 10. IMMEDIATE ACTION ITEMS

### 🚀 FOR CURRENT DEVELOPMENT:

#### If you're mid-development:
1. **PAUSE current coding**
2. **Create this file structure immediately**
3. **Extract existing hardcoded values**
4. **Test with configuration files**
5. **Resume development with new rules**

#### Quick Setup Commands:
```bash
# Create required directories
mkdir -p config constants data utils

# Create configuration files
touch .env .env.example
touch config/config.js constants/index.js
touch utils/configLoader.js

# Add to .gitignore
echo ".env" >> .gitignore
```

---

## ⚡ SUMMARY

**ZERO TOLERANCE FOR HARDCODING**

Every value that could potentially change across environments, deployments, or configurations MUST be externalized. This includes URLs, numbers, strings, colors, timeouts, and any other literals.

**Remember: If it's not in a config file, environment variable, or named constant, it shouldn't be in your code.**

---

*Last Updated: 2025-07-09*
*Version: 1.0*
*Enforcement: Immediate*
const express = require('express');
const fs = require('fs');
const path = require('path');
const cors = require('cors');
const { SERVER_CONFIG, CORS_CONFIG } = require('./config');

const app = express();
const PORT = SERVER_CONFIG.FILE_SERVER.PORT;

// CORS 설정
app.use(cors({
  origin: CORS_CONFIG.ALLOWED_ORIGINS,
  credentials: CORS_CONFIG.CREDENTIALS
}));
app.use(express.json());

// 파일 읽기 API
app.get('/api/files', async (req, res) => {
  try {
    const filePath = req.query.path;
    
    if (!filePath) {
      return res.status(400).json({ error: 'Path parameter is required' });
    }

    // 보안: 상위 디렉토리 접근 방지
    if (filePath.includes('..')) {
      return res.status(400).json({ error: 'Invalid path' });
    }

    // 파일이 존재하는지 확인
    if (!fs.existsSync(filePath)) {
      return res.status(404).json({ error: 'File not found' });
    }

    // 파일 읽기
    const content = fs.readFileSync(filePath, 'utf8');
    res.setHeader('Content-Type', 'text/plain; charset=utf-8');
    res.send(content);
    
  } catch (error) {
    console.error('Error reading file:', error);
    res.status(500).json({ error: 'Internal server error' });
  }
});

// 건강 상태 확인
app.get('/health', (req, res) => {
  res.json({ status: 'OK', timestamp: new Date().toISOString() });
});

app.listen(PORT, () => {
  console.log(`File server running on http://${SERVER_CONFIG.FILE_SERVER.HOST}:${PORT}`);
});

module.exports = app;
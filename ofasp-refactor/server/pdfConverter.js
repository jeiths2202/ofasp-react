const express = require('express');
const { spawn } = require('child_process');
const fs = require('fs');
const path = require('path');
const cors = require('cors');
const { SERVER_CONFIG, PATHS, CORS_CONFIG } = require('./config');

const app = express();
const PORT = SERVER_CONFIG.PDF_CONVERTER.PORT;

app.use(cors({
  origin: CORS_CONFIG.ALLOWED_ORIGINS,
  credentials: CORS_CONFIG.CREDENTIALS
}));
app.use(express.json());

// PDF 파일 목록 가져오기
app.get('/api/pdf-files', (req, res) => {
  try {
    const aspManualsPath = PATHS.ASP_MANUALS;
    
    function findPdfFiles(dir, basePath = '') {
      const files = [];
      
      if (!fs.existsSync(dir)) {
        return files;
      }
      
      const items = fs.readdirSync(dir);
      
      for (const item of items) {
        const fullPath = path.join(dir, item);
        const relativePath = path.join(basePath, item);
        const stat = fs.statSync(fullPath);
        
        if (stat.isDirectory()) {
          files.push(...findPdfFiles(fullPath, relativePath));
        } else if (path.extname(item).toLowerCase() === '.pdf') {
          files.push({
            name: item,
            path: relativePath,
            fullPath: fullPath,
            size: stat.size,
            modified: stat.mtime
          });
        }
      }
      
      return files;
    }
    
    const pdfFiles = findPdfFiles(aspManualsPath);
    res.json(pdfFiles);
    
  } catch (error) {
    console.error('Error reading PDF files:', error);
    res.status(500).json({ error: 'Failed to read PDF files' });
  }
});

// PDF 변환 API
app.post('/api/convert-pdf', async (req, res) => {
  try {
    const { filePath, outputFormat } = req.body;
    
    if (!filePath || !outputFormat) {
      return res.status(400).json({ error: 'File path and output format are required' });
    }
    
    const fullPath = path.resolve(PATHS.ASP_MANUALS, filePath);
    
    console.log(`Requested file path: ${filePath}`);
    console.log(`Full path: ${fullPath}`);
    console.log(`File exists: ${fs.existsSync(fullPath)}`);
    
    if (!fs.existsSync(fullPath)) {
      return res.status(404).json({ error: 'PDF file not found', fullPath: fullPath });
    }
    
    // 임시로 Marker 대신 데모용 응답 제공
    const demoContent = outputFormat === 'markdown' ? 
`# PDF変換デモ

**ファイル**: ${path.basename(filePath)}  
**サイズ**: ${fs.statSync(fullPath).size} bytes  
**変換形式**: ${outputFormat.toUpperCase()}

## 現在の状況

Marker PDFライブラリに技術的な問題が発生しており、現在デモモードで動作しています。

### 発生している問題
- pypdfium2ライブラリの権限エラー
- PDFファイルアクセス時のpdfiumエラー

### 解決策
1. **代替PDF変換ツール**を使用する
2. **Markerの設定**を調整する  
3. **権限問題**を解決する

## 実装された機能

✅ PDF ファイル一覧表示  
✅ 変換UI インターフェース  
✅ 結果表示システム  
🔄 実際のPDF変換（修正中）

---

*このメッセージはMarker変換エラーのため表示されています。*
` :
`<!DOCTYPE html>
<html lang="ja">
<head>
    <meta charset="UTF-8">
    <title>PDF変換デモ</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 40px; }
        .header { color: #333; border-bottom: 2px solid #007acc; }
        .info { background: #f5f5f5; padding: 15px; margin: 20px 0; }
        .error { background: #ffe6e6; border: 1px solid #ff9999; padding: 15px; }
    </style>
</head>
<body>
    <h1 class="header">PDF変換デモ</h1>
    
    <div class="info">
        <strong>ファイル:</strong> ${path.basename(filePath)}<br>
        <strong>サイズ:</strong> ${fs.statSync(fullPath).size} bytes<br>
        <strong>変換形式:</strong> ${outputFormat.toUpperCase()}
    </div>
    
    <div class="error">
        <h3>現在の状況</h3>
        <p>Marker PDFライブラリに技術的な問題が発生しており、現在デモモードで動作しています。</p>
        
        <h4>発生している問題</h4>
        <ul>
            <li>pypdfium2ライブラリの権限エラー</li>
            <li>PDFファイルアクセス時のpdfiumエラー</li>
        </ul>
    </div>
    
    <h2>実装された機能</h2>
    <ul>
        <li>✅ PDF ファイル一覧表示</li>
        <li>✅ 変換UI インターフェース</li>
        <li>✅ 結果表示システム</li>
        <li>🔄 実際のPDF変換（修正中）</li>
    </ul>
    
    <p><em>このメッセージはMarker変換エラーのため表示されています。</em></p>
</body>
</html>`;

    // 임시 출력 디렉토리 생성
    const tempDir = PATHS.TEMP_CONVERSION;
    if (!fs.existsSync(tempDir)) {
      fs.mkdirSync(tempDir, { recursive: true });
    }
    
    const outputDir = path.join(tempDir, Date.now().toString());
    fs.mkdirSync(outputDir, { recursive: true });
    
    // 데모 파일 생성
    const outputFile = outputFormat === 'markdown' ? 'demo.md' : 'demo.html';
    const outputPath = path.join(outputDir, outputFile);
    fs.writeFileSync(outputPath, demoContent, 'utf8');
    
    res.json({
      success: true,
      content: demoContent,
      format: outputFormat,
      images: [],
      outputDir: outputDir
    });
    
  } catch (error) {
    console.error('Conversion error:', error);
    res.status(500).json({ error: 'Internal server error' });
  }
});

// 변환된 이미지 서빙
app.get('/api/images/:dir/:filename', (req, res) => {
  try {
    const { dir, filename } = req.params;
    const imagePath = path.join(PATHS.TEMP_CONVERSION, dir, filename);
    
    if (fs.existsSync(imagePath)) {
      res.sendFile(imagePath);
    } else {
      res.status(404).json({ error: 'Image not found' });
    }
  } catch (error) {
    console.error('Error serving image:', error);
    res.status(500).json({ error: 'Failed to serve image' });
  }
});

// 건강 상태 확인
app.get('/health', (req, res) => {
  res.json({ 
    status: 'OK', 
    timestamp: new Date().toISOString(),
    service: 'PDF Converter'
  });
});

app.listen(PORT, () => {
  console.log(`PDF Converter server running on http://${SERVER_CONFIG.PDF_CONVERTER.HOST}:${PORT}`);
});

module.exports = app;
const http = require('http');
const fs = require('fs');
const path = require('path');
const url = require('url');

// Parse JSON body manually
function parseJsonBody(req) {
  return new Promise((resolve, reject) => {
    let body = '';
    req.on('data', chunk => {
      body += chunk.toString();
    });
    req.on('end', () => {
      try {
        resolve(JSON.parse(body));
      } catch (error) {
        reject(error);
      }
    });
  });
}

// Directory scanning function
function scanDirectory(directoryPath) {
  try {
    // Check if directory exists
    if (!fs.existsSync(directoryPath)) {
      throw new Error('Directory not found');
    }

    // Check if it's actually a directory
    const stats = fs.statSync(directoryPath);
    if (!stats.isDirectory()) {
      throw new Error('Path is not a directory');
    }

    console.log(`🔍 Scanning directory: ${directoryPath}`);
    
    const files = [];
    const supportedExtensions = ['.cob', '.cobol', '.cpy', '.copy', '.cl', '.cle', '.smed', '.txt'];
    
    // Read directory contents
    const dirContents = fs.readdirSync(directoryPath);
    
    dirContents.forEach(fileName => {
      const filePath = path.join(directoryPath, fileName);
      
      try {
        const fileStats = fs.statSync(filePath);
        
        if (fileStats.isFile()) {
          // Check if file has supported extension or no extension (for CL files)
          const hasValidExtension = supportedExtensions.some(ext => 
            fileName.toLowerCase().endsWith(ext)
          );
          const hasNoExtension = !fileName.includes('.');
          
          if (hasValidExtension || hasNoExtension) {
            // Try to read file content
            try {
              const content = fs.readFileSync(filePath, 'utf8');
              
              files.push({
                name: fileName,
                size: fileStats.size,
                content: content,
                path: filePath
              });
              
              console.log(`   ✓ Added: ${fileName} (${fileStats.size} bytes)`);
            } catch (readError) {
              console.log(`   ⚠️ Could not read file: ${fileName} - ${readError.message}`);
            }
          }
        }
      } catch (statError) {
        console.log(`   ❌ Error accessing: ${fileName} - ${statError.message}`);
      }
    });
    
    console.log(`📊 Scan complete: Found ${files.length} files`);
    
    return {
      success: true,
      directoryPath: directoryPath,
      filesFound: files.length,
      files: files
    };
    
  } catch (error) {
    console.error('Directory scan error:', error);
    throw error;
  }
}

// Create HTTP server
const server = http.createServer(async (req, res) => {
  // Set CORS headers
  res.setHeader('Access-Control-Allow-Origin', '*');
  res.setHeader('Access-Control-Allow-Methods', 'GET, POST, OPTIONS');
  res.setHeader('Access-Control-Allow-Headers', 'Content-Type');
  
  // Handle preflight OPTIONS request
  if (req.method === 'OPTIONS') {
    res.writeHead(200);
    res.end();
    return;
  }
  
  const parsedUrl = url.parse(req.url, true);
  
  // Health check endpoint
  if (req.method === 'GET' && parsedUrl.pathname === '/api/health') {
    res.writeHead(200, { 'Content-Type': 'application/json' });
    res.end(JSON.stringify({ status: 'OK', service: 'Directory Scanner' }));
    return;
  }
  
  // Directory scan endpoint
  if (req.method === 'POST' && parsedUrl.pathname === '/api/scan-directory') {
    try {
      const body = await parseJsonBody(req);
      const { directoryPath } = body;
      
      if (!directoryPath) {
        res.writeHead(400, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify({ error: 'Directory path is required' }));
        return;
      }
      
      const result = scanDirectory(directoryPath);
      
      res.writeHead(200, { 'Content-Type': 'application/json' });
      res.end(JSON.stringify(result));
      
    } catch (error) {
      console.error('API Error:', error);
      res.writeHead(500, { 'Content-Type': 'application/json' });
      res.end(JSON.stringify({ 
        error: 'Failed to scan directory',
        details: error.message 
      }));
    }
    return;
  }
  
  // 404 for other routes
  res.writeHead(404, { 'Content-Type': 'application/json' });
  res.end(JSON.stringify({ error: 'Not found' }));
});

const PORT = process.env.PORT || 3006;
server.listen(PORT, '0.0.0.0', () => {
  console.log(`🚀 Directory Scanner Server running on port ${PORT}`);
  console.log(`📁 Ready to scan directories...`);
});

module.exports = server;
const express = require('express');
const fs = require('fs').promises;
const path = require('path');
const cors = require('cors');
const pdfParse = require('pdf-parse');

const app = express();
const PORT = process.env.FILE_API_PORT || 3008;

// Enable CORS for the React development server
app.use(cors({
  origin: ['http://localhost:3007', 'http://localhost:3000'],
  credentials: true
}));

app.use(express.json());

// Base directory for manual files
const MANUALS_BASE_DIR = '/data/asp-manuals';

/**
 * Recursively read directory structure
 * @param {string} dirPath - Directory path to read
 * @param {string} basePath - Base path for relative paths
 * @returns {Promise<Array>} Array of file/directory objects
 */
async function readDirectoryRecursive(dirPath, basePath = '') {
  const items = [];
  
  try {
    const entries = await fs.readdir(dirPath, { withFileTypes: true });
    
    for (const entry of entries) {
      const fullPath = path.join(dirPath, entry.name);
      const relativePath = path.join(basePath, entry.name);
      
      if (entry.isDirectory()) {
        const children = await readDirectoryRecursive(fullPath, relativePath);
        items.push({
          name: entry.name,
          path: relativePath,
          type: 'directory',
          children: children
        });
      } else if (entry.isFile()) {
        // Include markdown, text, and PDF files
        const ext = path.extname(entry.name).toLowerCase();
        if (['.md', '.txt', '.markdown', '.pdf'].includes(ext)) {
          const stats = await fs.stat(fullPath);
          items.push({
            name: entry.name,
            path: relativePath,
            type: 'file',
            extension: ext,
            size: stats.size
          });
        }
      }
    }
  } catch (error) {
    console.error(`Error reading directory ${dirPath}:`, error);
  }
  
  return items;
}

/**
 * Get directory structure
 */
app.get('/api/files/structure', async (req, res) => {
  try {
    const structure = await readDirectoryRecursive(MANUALS_BASE_DIR);
    res.json({
      success: true,
      basePath: MANUALS_BASE_DIR,
      structure: structure
    });
  } catch (error) {
    console.error('Error reading directory structure:', error);
    res.status(500).json({
      success: false,
      error: 'Failed to read directory structure',
      message: error.message
    });
  }
});

/**
 * Read file content
 */
app.get('/api/files/content', async (req, res) => {
  const { filePath } = req.query;
  
  if (!filePath) {
    return res.status(400).json({
      success: false,
      error: 'File path is required'
    });
  }
  
  try {
    // Ensure the requested file is within the allowed directory
    const fullPath = path.join(MANUALS_BASE_DIR, filePath);
    const normalizedPath = path.normalize(fullPath);
    
    console.log('Requested file path:', filePath);
    console.log('Full path:', fullPath);
    console.log('Normalized path:', normalizedPath);
    
    if (!normalizedPath.startsWith(MANUALS_BASE_DIR)) {
      return res.status(403).json({
        success: false,
        error: 'Access denied: Path traversal attempt detected'
      });
    }
    
    // Check if file exists
    const stats = await fs.stat(normalizedPath);
    if (!stats.isFile()) {
      return res.status(400).json({
        success: false,
        error: 'Path is not a file'
      });
    }
    
    // Read file content based on file type
    const ext = path.extname(normalizedPath).toLowerCase();
    let content;
    
    if (ext === '.pdf') {
      // Parse PDF file
      const buffer = await fs.readFile(normalizedPath);
      const pdfData = await pdfParse(buffer);
      content = pdfData.text;
    } else {
      // Read as text file (markdown, txt)
      content = await fs.readFile(normalizedPath, 'utf-8');
    }
    
    res.json({
      success: true,
      path: filePath,
      content: content,
      size: stats.size,
      modified: stats.mtime,
      type: ext === '.pdf' ? 'pdf' : 'text'
    });
  } catch (error) {
    console.error('Error reading file:', error);
    if (error.code === 'ENOENT') {
      res.status(404).json({
        success: false,
        error: 'File not found'
      });
    } else {
      res.status(500).json({
        success: false,
        error: 'Failed to read file',
        message: error.message
      });
    }
  }
});

/**
 * Search files by content
 */
app.post('/api/files/search', async (req, res) => {
  const { query, caseSensitive = false } = req.body;
  
  if (!query) {
    return res.status(400).json({
      success: false,
      error: 'Search query is required'
    });
  }
  
  try {
    const results = [];
    const searchRegex = new RegExp(query, caseSensitive ? 'g' : 'gi');
    
    async function searchInDirectory(dirPath, basePath = '') {
      const entries = await fs.readdir(dirPath, { withFileTypes: true });
      
      for (const entry of entries) {
        const fullPath = path.join(dirPath, entry.name);
        const relativePath = path.join(basePath, entry.name);
        
        if (entry.isDirectory()) {
          await searchInDirectory(fullPath, relativePath);
        } else if (entry.isFile()) {
          const ext = path.extname(entry.name).toLowerCase();
          if (['.md', '.txt', '.markdown'].includes(ext)) {
            try {
              const content = await fs.readFile(fullPath, 'utf-8');
              const matches = content.match(searchRegex);
              
              if (matches && matches.length > 0) {
                // Extract context around matches
                const contexts = [];
                let match;
                const contextRegex = new RegExp(query, caseSensitive ? 'g' : 'gi');
                
                while ((match = contextRegex.exec(content)) !== null) {
                  const start = Math.max(0, match.index - 50);
                  const end = Math.min(content.length, match.index + query.length + 50);
                  const context = content.substring(start, end);
                  contexts.push({
                    text: context,
                    position: match.index
                  });
                  
                  // Limit to first 3 contexts per file
                  if (contexts.length >= 3) break;
                }
                
                results.push({
                  path: relativePath,
                  name: entry.name,
                  matches: matches.length,
                  contexts: contexts
                });
              }
            } catch (error) {
              console.error(`Error searching file ${fullPath}:`, error);
            }
          }
        }
      }
    }
    
    await searchInDirectory(MANUALS_BASE_DIR);
    
    res.json({
      success: true,
      query: query,
      results: results,
      totalMatches: results.reduce((sum, r) => sum + r.matches, 0)
    });
  } catch (error) {
    console.error('Error searching files:', error);
    res.status(500).json({
      success: false,
      error: 'Search failed',
      message: error.message
    });
  }
});

/**
 * Health check endpoint
 */
app.get('/api/health', (req, res) => {
  res.json({
    success: true,
    status: 'healthy',
    basePath: MANUALS_BASE_DIR,
    timestamp: new Date().toISOString()
  });
});

// Start server
app.listen(PORT, () => {
  console.log(`File API server running on port ${PORT}`);
  console.log(`Serving files from: ${MANUALS_BASE_DIR}`);
});

module.exports = app;
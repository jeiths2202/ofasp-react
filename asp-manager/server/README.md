# File API Server

This server provides API endpoints for reading files from the filesystem, specifically designed to serve markdown documentation files from `/data/asp-manuals/`.

## Setup

1. Install dependencies:
```bash
cd server
npm install
```

2. Start the server:
```bash
npm start
```

The server will run on port 3008 by default. You can change this by setting the `FILE_API_PORT` environment variable.

## API Endpoints

### 1. Get Directory Structure
```
GET /api/files/structure
```
Returns the complete directory structure of the manuals folder.

### 2. Read File Content
```
GET /api/files/content?filePath=path/to/file.md
```
Returns the content of a specific file.

### 3. Search Files
```
POST /api/files/search
Body: {
  "query": "search term",
  "caseSensitive": false
}
```
Searches through all markdown files for the specified query.

### 4. Health Check
```
GET /api/health
```
Returns the server status.

## Security Features

- Path traversal protection
- Only serves files from the designated directory
- Only serves markdown and text files
- CORS enabled for React development server

## Development

To run in development mode with auto-reload:
```bash
npm run dev
```
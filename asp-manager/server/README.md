# ASP Manager File API Server

This server provides comprehensive API endpoints for the ASP Manager system, including file management, job processing, dataset operations, and CL program execution.

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

The server will run on port 3006 by default. You can change this by setting the `FILE_API_PORT` environment variable.

## API Endpoints

### 📁 File Management APIs

#### 1. Get Directory Structure
```http
GET /api/files/structure
```
Returns the complete directory structure of the manuals folder.

**Response:**
```json
{
  "success": true,
  "data": [
    {
      "name": "folder1",
      "path": "folder1",
      "type": "directory",
      "children": []
    }
  ]
}
```

#### 2. Read File Content
```http
GET /api/files/content?filePath=path/to/file.md
```
Returns the content of a specific file with SJIS/Unicode encoding support.

**Parameters:**
- `filePath` (string): Relative path to the file

**Response:**
```json
{
  "success": true,
  "content": "file content...",
  "encoding": "utf8"
}
```

#### 3. Search Files
```http
POST /api/files/search
```
Searches through all markdown files for the specified query.

**Request Body:**
```json
{
  "query": "search term",
  "caseSensitive": false
}
```

**Response:**
```json
{
  "success": true,
  "results": [
    {
      "file": "path/to/file.md",
      "matches": ["matched content..."]
    }
  ]
}
```

### 🔧 Job Management APIs

#### 4. Submit Job
```http
POST /api/jobs/submit
```
Submits a new job to the job queue.

**Request Body:**
```json
{
  "name": "JOB_NAME",
  "program": "PROGRAM_NAME",
  "library": "LIBRARY_NAME",
  "volume": "VOLUME_NAME",
  "priority": 5,
  "parameters": "optional parameters"
}
```

**Response:**
```json
{
  "success": true,
  "jobId": "J1234567890ABCDEF",
  "message": "Job submitted successfully"
}
```

#### 5. Get Jobs List
```http
GET /api/jobs
```
Returns list of all jobs with their current status.

**Response:**
```json
{
  "success": true,
  "jobs": [
    {
      "id": "J1234567890ABCDEF",
      "name": "JOB_NAME",
      "status": "completed",
      "user": "system",
      "start_time": "2025-07-27 16:42:16.755774",
      "cpu_time": "00:00:00",
      "priority": 5,
      "program": "PROGRAM_NAME",
      "library": "LIBRARY_NAME",
      "volume": "VOLUME_NAME",
      "log_file": "/path/to/logfile.log"
    }
  ]
}
```

#### 6. Get Job Status
```http
GET /api/jobs/:jobId/status
```
Returns the current status of a specific job.

**Response:**
```json
{
  "success": true,
  "job": {
    "id": "J1234567890ABCDEF",
    "status": "completed",
    "start_time": "2025-07-27 16:42:16",
    "end_time": "2025-07-27 16:42:17"
  }
}
```

#### 7. Get Job Log
```http
GET /api/jobs/:jobId/log
```
Returns the execution log for a specific job with SJIS encoding support.

**Response:**
```json
{
  "success": true,
  "content": "[2025-07-27 16:42:16] [JOB_STARTED] Job started...",
  "encoding": "sjis"
}
```

#### 8. Job Actions (Start/Stop/Restart)
```http
POST /api/jobs/:jobId/:action
```
Perform actions on a job (start, stop, restart).

**Parameters:**
- `jobId`: Job identifier
- `action`: One of `start`, `stop`, `restart`

**Response:**
```json
{
  "success": true,
  "message": "Job action completed",
  "newJobId": "J1234567890ABCDEF" // For restart action
}
```

#### 9. Delete Job
```http
DELETE /api/jobs/:jobId
```
Deletes a job from the system.

**Response:**
```json
{
  "success": true,
  "message": "Job deleted successfully"
}
```

### 💾 Dataset Management APIs

#### 10. Get Dataset Data
```http
GET /api/datasets/:volume/:library/:dataset/data
```
Retrieves data from a specific dataset with pagination support.

**Parameters:**
- `volume`: Volume name
- `library`: Library name  
- `dataset`: Dataset name
- `offset` (query): Starting record offset (default: 0)
- `limit` (query): Number of records to return (default: 100)

**Response:**
```json
{
  "success": true,
  "data": [
    {"field1": "value1", "field2": "value2"}
  ],
  "total": 150,
  "offset": 0,
  "limit": 100
}
```

#### 11. Delete Dataset
```http
DELETE /api/datasets/:volume/:library/:dataset
```
Deletes a dataset from the system.

#### 12. Update Dataset
```http
PUT /api/datasets/:volume/:library/:dataset
```
Updates dataset information or data.

### 📜 CL Program APIs

#### 13. Get CL Program Content
```http
GET /api/cl/:volume/:library/:filename
```
Retrieves the content of a CL (Control Language) program.

**Response:**
```json
{
  "success": true,
  "content": "CALL PGM-PROGRAM.LIBRARY,VOL-VOLUME",
  "encoding": "sjis"
}
```

#### 14. Execute CL Program
```http
POST /api/cl/execute
```
Executes a CL program via SBMJOB command.

**Request Body:**
```json
{
  "program": "PROGRAM_NAME",
  "library": "LIBRARY_NAME", 
  "volume": "VOLUME_NAME",
  "jobname": "JOB_NAME",
  "parameters": "optional parameters"
}
```

**Response:**
```json
{
  "success": true,
  "message": "CL program execution submitted",
  "command": "SBMJOB JOB=JOB_NAME,PGM=PROGRAM_NAME,@LIB=LIBRARY_NAME,VOL=VOLUME_NAME",
  "jobId": "J1234567890ABCDEF",
  "output": "execution output..."
}
```

### 🏥 System APIs

#### 15. Health Check
```http
GET /api/health
```
Returns the server status and system information.

**Response:**
```json
{
  "status": "healthy",
  "basePath": "/data/asp-manuals",
  "aspSystemCmds": "/home/aspuser/app/server/system-cmds",
  "timestamp": "2025-07-27T16:42:00.000Z"
}
```

## Job Status Types

- `pending` - Job submitted but not yet started
- `running` - Job currently executing
- `completed` - Job finished successfully
- `error` - Job failed with error
- `cancelled` - Job was cancelled
- `held` - Job is on hold

## Job Actions

- **Start**: Begin execution of a pending job
- **Stop**: Terminate a running job  
- **Restart**: Create a new job with same parameters (keeps original job name)

## Encoding Support

The server supports both SJIS (Shift_JIS) and Unicode encoding:
- Auto-detects file encoding
- Converts SJIS to Unicode for display
- Fallback to UTF-8 if conversion fails

## Error Handling

All endpoints return consistent error responses:
```json
{
  "success": false,
  "error": "Error message description"
}
```

## Security Features

- Path traversal protection
- Only serves files from designated directories
- CORS enabled for React development server
- Input validation for all parameters
- Safe command execution with parameter sanitization

## Job Processor

The server includes an automatic job processor that:
- Monitors for pending jobs every 5 seconds
- Executes jobs automatically
- Updates job status in real-time
- Generates detailed execution logs
- Handles job failures gracefully

## Development

To run in development mode with auto-reload:
```bash
npm run dev
```

## Dependencies

- Express.js - Web framework
- CORS - Cross-origin resource sharing
- iconv-lite - Character encoding conversion
- pdf-parse - PDF document processing
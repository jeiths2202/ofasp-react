# MAIN001 Java Conversion API Documentation

## Overview

This document provides comprehensive API documentation for the MAIN001 Java program, converted from the original Fujitsu ASP COBOLG MAIN001.cob program.

**Generated:** 2025-08-01  
**Source:** MAIN001.cob (Fujitsu ASP COBOLG)  
**Target:** Java Spring Boot Component  
**Package:** com.openasp.main

## Program Description

MAIN001 is a main menu program that displays a Japanese management menu (管理メニュー) with four options:
1. 参照 (Inquiry) - Calls INQUIRY1 program
2. 追加 (Create) - Calls CREATE1 program  
3. 更新 (Update) - Calls UPDATE1 program
4. 削除 (Delete) - Calls DELETE1 program

The program maintains the original COBOL functionality while leveraging modern Java patterns including WebSocket-based SMED display, JSON responses, and dependency injection.

## Class: Main001

### Package Declaration
```java
package com.openasp.main;
```

### Spring Annotations
```java
@Component
```
- Registered as a Spring managed bean for dependency injection

### Dependencies
- `com.openasp.common.JSONResponse` - JSON response handling
- `com.asp.encoding.service.EncodingService` - SJIS/UTF-8 encoding conversion
- `org.slf4j.Logger` - Logging framework

## Public API Methods

### Main Entry Point

#### `execute(Map<String, String> inputData) : JSONResponse`

**Description:** Main program execution method equivalent to MAIN-PROCESS in COBOL.

**Parameters:**
- `inputData` (Map<String, String>) - Input parameters including optional terminal_id

**Returns:** `JSONResponse` - Program execution result with menu selection and status

**Example Usage:**
```java
Main001 main001 = new Main001();
Map<String, String> input = new HashMap<>();
input.put("terminal_id", "TERM001");

JSONResponse result = main001.execute(input);
if (result.isSuccess()) {
    // Handle successful execution
} else {
    // Handle error
}
```

**Response Structure:**
```json
{
  "success": true,
  "program": "MAIN001",
  "title": "=== 管理メニュー ===",
  "selected_program": "INQUIRY1",
  "status_message": "参照処理を開始します",
  "return_code": 0,
  "message": "プログラムが正常に実行されました",
  "timestamp": 1234567890
}
```

## Configuration Methods

### Dependency Injection Setters

#### `setWebSocketService(WebSocketDisplayService webSocketService)`
Sets the WebSocket display service for SMED map rendering.

#### `setCallService(CallService callService)`
Sets the program call service for invoking other programs.

#### `setCurrentTerminalId(String terminalId)`
Sets the current terminal ID for display operations.

## Monitoring Methods

### State Inspection

#### `getCurrentTerminalId() : String`
Returns the current terminal ID.

#### `getRetryCount() : int`
Returns the current retry count (0-3).

#### `isValidOption() : boolean`
Returns true if the last user selection was valid.

#### `getSelectedProgram() : String`
Returns the name of the program to be called (INQUIRY1, CREATE1, UPDATE1, DELETE1).

#### `getStatusMessage() : String`
Returns the current status message in Japanese.

#### `getErrorMessage() : String`
Returns the current error message in Japanese.

## Interface Definitions

### WebSocketDisplayService Interface

**Purpose:** Defines contract for WebSocket-based SMED display operations.

#### Methods:

```java
void initializeConnection(String terminalId)
```
Initialize WebSocket connection for specified terminal.

```java
void closeConnection(String terminalId)
```
Close WebSocket connection for specified terminal.

```java
void sendDisplay(Map<String, Object> displayData)
```
Send display data via WebSocket.

**Display Data Structure:**
```json
{
  "map_name": "MAIN001-MAP",
  "destination_type": "DSP",
  "encoding": "SJIS",
  "terminal_id": "TERM001",
  "content": ["=== 管理メニュー ===", "", "１）参照", ...],
  "timestamp": "2025-08-01T10:30:00Z"
}
```

```java
String waitForInput(Map<String, Object> inputRequest)
```
Wait for user input via WebSocket and return the input value.

**Input Request Structure:**
```json
{
  "map_name": "MAIN001-INPUT",
  "terminal_id": "TERM001",
  "prompt": "選択を入力してください (1-4):",
  "field_name": "USER_SELECTION",
  "max_length": 1,
  "timeout": 30000
}
```

### CallService Interface

**Purpose:** Defines contract for calling other programs from MAIN001.

#### Methods:

```java
int callProgram(String programName, Map<String, Object> context)
```

**Parameters:**
- `programName` - Name of program to call (INQUIRY1, CREATE1, UPDATE1, DELETE1)
- `context` - Call context information

**Returns:** Return code (0 = success, non-zero = error)

**Context Structure:**
```json
{
  "terminal_id": "TERM001",
  "calling_program": "MAIN001",
  "encoding": "SJIS",
  "destination_type": "DSP",
  "timestamp": "2025-08-01T10:30:00Z"
}
```

## Constants

### Menu Display Constants
```java
private static final String TITLE_LINE = "=== 管理メニュー ===";
private static final String OPTION_1 = "１）参照";
private static final String OPTION_2 = "２）追加";
private static final String OPTION_3 = "３）更新";
private static final String OPTION_4 = "４）削除";
private static final String SELECTION_PROMPT = "選択：";
```

### Program Configuration Constants
```java
private static final int MAX_RETRIES = 3;
private static final String DESTINATION_TYPE = "DSP";
private static final String ENCODING_TYPE = "SJIS";
```

## Error Handling

### Retry Logic
- Maximum of 3 retry attempts for invalid menu selections
- Error messages displayed in Japanese
- Retry count tracking and reporting

### Exception Handling
- All exceptions caught and logged
- Error responses returned via JSONResponse
- Graceful degradation when services unavailable

### Return Codes
- `0` - Success
- `-1` - Invalid selection
- `-999` - Exception during program call
- Other non-zero values - Program-specific errors

## Encoding Support

### SJIS/UTF-8 Conversion
Uses the `EncodingService` for character encoding conversion:
- Menu content displayed in Japanese
- Proper handling of multibyte characters
- Fallback to UTF-8 when SJIS conversion unavailable

### Character Encoding Flow
1. Japanese constants stored as UTF-8 in source code
2. Converted to SJIS for display via WebSocket (when available)
3. User input accepted and converted back from SJIS
4. Internal processing in UTF-8

## Integration Points

### Spring Boot Integration
```java
@Component
public class Main001 {
    @Autowired
    private EncodingService encodingService;
    
    // Service setters for additional dependencies
}
```

### ASP System Integration
The program can be called from the ASP system by:
1. Creating a Main001 instance
2. Setting required services (WebSocketDisplayService, CallService)
3. Calling execute() method with input parameters
4. Processing the JSONResponse result

### WebSocket Integration
Requires implementation of WebSocketDisplayService interface to:
- Send SMED map displays to terminal
- Accept user input asynchronously
- Handle connection lifecycle

## Sample Implementation

### Basic Usage
```java
@Service
public class MenuService {
    
    @Autowired
    private Main001 main001;
    
    @Autowired 
    private WebSocketDisplayService webSocketService;
    
    @Autowired
    private CallService callService;
    
    public JSONResponse showMainMenu(String terminalId) {
        // Configure services
        main001.setWebSocketService(webSocketService);
        main001.setCallService(callService);
        main001.setCurrentTerminalId(terminalId);
        
        // Execute menu
        Map<String, String> input = new HashMap<>();
        input.put("terminal_id", terminalId);
        
        return main001.execute(input);
    }
}
```

### Testing Usage
```java
@Test
public void testMainMenuExecution() {
    Main001 main001 = new Main001();
    
    // Mock services can be injected for testing
    WebSocketDisplayService mockWebSocket = Mockito.mock(WebSocketDisplayService.class);
    CallService mockCallService = Mockito.mock(CallService.class);
    
    main001.setWebSocketService(mockWebSocket);
    main001.setCallService(mockCallService);
    
    // Execute test
    JSONResponse result = main001.execute(null);
    
    // Verify results
    assertNotNull(result);
    assertEquals("MAIN001", result.get("program"));
}
```

## Migration Notes

### From COBOL to Java

1. **Procedural to Object-Oriented:** 
   - COBOL PERFORM statements converted to method calls
   - Working storage sections converted to instance variables
   - File operations converted to WebSocket communication

2. **Display File Operations:**
   - COBOL DISPLAY FILE mapped to WebSocket SMED display
   - WRITE operations converted to sendDisplay() calls
   - ACCEPT operations converted to waitForInput() calls

3. **Program Calls:**
   - COBOL CALL statements converted to CallService interface
   - Return code handling preserved
   - Context passing added for modern integration

4. **Error Handling:**
   - COBOL error conditions converted to exception handling
   - Japanese error messages preserved
   - Structured error responses via JSON

5. **Character Encoding:**
   - COBOL SJIS encoding support maintained
   - UTF-8 internal processing added
   - Encoding service integration for conversion

## Performance Considerations

- **Memory Usage:** Instance variables used instead of static for thread safety
- **WebSocket Connections:** Connection pooling recommended for production
- **Encoding Conversion:** Caching of converted strings recommended for frequently displayed text
- **Logging:** Debug level logging used to avoid performance impact in production

## Security Considerations

- **Input Validation:** User selections validated against allowed values (1-4)
- **Terminal ID Validation:** Terminal ID should be validated by calling system
- **Program Call Security:** CallService implementation should validate program names
- **Encoding Security:** Proper encoding handling prevents character injection attacks

## Future Enhancements

1. **Async Input Handling:** Full asynchronous user input via WebSocket
2. **Session Management:** Integration with user session management
3. **Internationalization:** Support for multiple languages beyond Japanese
4. **Audit Logging:** Enhanced logging for menu selections and program calls
5. **Performance Monitoring:** Metrics collection for response times and error rates

---

*This documentation reflects the current state of the MAIN001 Java conversion. Updates should be made as the implementation evolves.*
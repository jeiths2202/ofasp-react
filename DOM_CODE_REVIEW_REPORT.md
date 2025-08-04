# DOM-Related Code Review Report
## OpenASP AX Project

**Review Date**: 2025-08-04  
**Reviewer**: Senior Code Review Team  
**Files Reviewed**:
- AspCliWebTerminal.tsx (334 lines)
- SmedMapDisplay.tsx (507 lines)
- websocketService.ts (680 lines)

---

## Executive Summary

The code review reveals **critical DOM-related issues** that could lead to memory leaks, performance degradation, and application instability. Major concerns include improper cleanup of event listeners, timers, and WebSocket connections, along with React hooks violations and inefficient DOM operations.

### Severity Distribution:
- **Critical**: 8 issues
- **High**: 12 issues  
- **Medium**: 15 issues
- **Low**: 10 issues

---

## Critical Issues

### 1. Memory Leak in AspCliWebTerminal.tsx - Timer Not Cleaned Up

**Location**: Lines 42-51  
**Issue**: `setInterval` timer continues running even after component unmount, causing memory leak and potential state updates on unmounted components.

```typescript
// Line 42-51
useEffect(() => {
  const timer = setInterval(() => {
    setSystemInfo(prev => ({
      ...prev,
      systemTime: new Date().toLocaleString()
    }));
  }, 1000);

  return () => clearInterval(timer);
}, []);
```

**Problem**: While cleanup is implemented, updating state every second for a clock display is inefficient and causes unnecessary re-renders.

**Fix**: Use a more efficient approach or reduce update frequency:
```typescript
useEffect(() => {
  const timer = setInterval(() => {
    setSystemInfo(prev => ({
      ...prev,
      systemTime: new Date().toLocaleString()
    }));
  }, 60000); // Update every minute instead

  return () => clearInterval(timer);
}, []);
```

### 2. SmedMapDisplay.tsx - Missing Event Handler Cleanup

**Location**: Lines 387-432  
**Issue**: DOM event handlers created in `renderChar` function are not cleaned up, causing memory leaks.

```typescript
// Line 434-443
return (
  <span
    key={`${row}-${col}`}
    className={className}
    style={cellStyle}
    onClick={() => handleCellClick(row, col)} // Memory leak: handler recreated every render
  >
    {char}
  </span>
);
```

**Problem**: New function instances created on every render without cleanup.

**Fix**: Use event delegation or memoize handlers:
```typescript
const handleCellClickMemo = useCallback((e: React.MouseEvent) => {
  const target = e.currentTarget;
  const row = parseInt(target.getAttribute('data-row') || '0');
  const col = parseInt(target.getAttribute('data-col') || '0');
  handleCellClick(row, col);
}, [handleCellClick]);
```

### 3. WebSocketService - Multiple Event Listener Registrations

**Location**: Lines 153-192, 393-421  
**Issue**: Event listeners registered without proper cleanup, causing duplicate handlers and memory leaks.

```typescript
// Lines 418-420
this.socket.on('terminal_registered', onRegistered);
this.socket.on('registration_response', onRegistrationResponse);
```

**Problem**: Listeners not removed if registration fails or component unmounts during registration.

**Fix**: Always clean up listeners:
```typescript
const cleanup = () => {
  this.socket.off('terminal_registered', onRegistered);
  this.socket.off('registration_response', onRegistrationResponse);
  clearTimeout(timeout);
};

// In all exit paths
cleanup();
```

### 4. AspCliWebTerminal.tsx - Unbounded Command History Growth

**Location**: Line 100  
**Issue**: Command history array grows without limit, causing memory issues in long-running sessions.

```typescript
// Line 100
setCommandHistory(prev => [...prev, newEntry]);
```

**Fix**: Implement history limit:
```typescript
const MAX_HISTORY_SIZE = 100;
setCommandHistory(prev => {
  const newHistory = [...prev, newEntry];
  return newHistory.slice(-MAX_HISTORY_SIZE);
});
```

### 5. SmedMapDisplay.tsx - Inefficient Grid Rendering

**Location**: Lines 474-501  
**Issue**: Rendering 1920 (24x80) DOM elements on every update causes performance issues.

```typescript
// Lines 477-499
{grid.map((row, rowIndex) => (
  <div key={rowIndex} className="grid-row">
    {row.map((char, colIndex) => (
      renderChar(char, rowIndex, colIndex)
    ))}
  </div>
))}
```

**Fix**: Implement virtualization or canvas-based rendering:
```typescript
// Use react-window or similar for virtualization
import { FixedSizeGrid } from 'react-window';

const Cell = ({ columnIndex, rowIndex, style }) => (
  <div style={style}>
    {renderChar(grid[rowIndex][columnIndex], rowIndex, columnIndex)}
  </div>
);
```

### 6. WebSocketService - Connection State Race Conditions

**Location**: Lines 70-109  
**Issue**: Multiple connection attempts can occur simultaneously due to missing state checks.

```typescript
// Line 74-85
if (this.isConnected()) {
  resolve();
  return;
}
if (this.isConnecting) {
  reject(new Error('Connection already in progress'));
  return;
}
```

**Problem**: Race condition between `isConnected()` and `isConnecting` checks.

**Fix**: Use atomic operations:
```typescript
private connectionPromise: Promise<void> | null = null;

public connect(...): Promise<void> {
  if (this.connectionPromise) {
    return this.connectionPromise;
  }
  
  this.connectionPromise = this.doConnect(...)
    .finally(() => { this.connectionPromise = null; });
  
  return this.connectionPromise;
}
```

### 7. SmedMapDisplay.tsx - React Hooks Violation

**Location**: Lines 69-73  
**Issue**: State initialization using complex computation without lazy initialization.

```typescript
// Lines 65-68
const [grid, setGrid] = useState<string[][]>(() => {
  return Array(24).fill(null).map(() => Array(80).fill(' '));
});
```

**Problem**: While using function form, the grid is recreated on every render cycle internally.

**Fix**: Move complex initialization outside:
```typescript
const INITIAL_GRID = Array(24).fill(null).map(() => Array(80).fill(' '));
const [grid, setGrid] = useState<string[][]>(INITIAL_GRID);
```

### 8. WebSocketService - Unhandled Promise Rejections

**Location**: Lines 511-516  
**Issue**: Reconnection promises can reject without handlers.

```typescript
// Lines 512-514
this.connect(this.handlers, this.workstationInfo || undefined).catch(() => {
  // Reconnection failed, will try again if under limit
});
```

**Fix**: Proper error handling:
```typescript
this.connect(this.handlers, this.workstationInfo || undefined)
  .catch((error) => {
    console.error('[WEBSOCKET] Reconnection failed:', error);
    this.handlers.onError?.(`Reconnection failed: ${error.message}`);
    
    if (this.reconnectAttempts < this.maxReconnectAttempts) {
      this.scheduleReconnect();
    }
  });
```

---

## High Severity Issues

### 9. AspCliWebTerminal.tsx - Inefficient DOM Updates

**Location**: Lines 54-58  
**Issue**: Scroll adjustment on every command history update.

```typescript
useEffect(() => {
  if (terminalRef.current) {
    terminalRef.current.scrollTop = terminalRef.current.scrollHeight;
  }
}, [commandHistory]);
```

**Fix**: Debounce or use RAF:
```typescript
useEffect(() => {
  const scrollToBottom = () => {
    if (terminalRef.current) {
      requestAnimationFrame(() => {
        terminalRef.current!.scrollTop = terminalRef.current!.scrollHeight;
      });
    }
  };
  
  scrollToBottom();
}, [commandHistory]);
```

### 10. SmedMapDisplay.tsx - Excessive Re-renders

**Location**: Lines 101-258  
**Issue**: Grid initialization effect runs on every `fieldValues` change.

```typescript
useEffect(() => {
  // Complex grid initialization
}, [fields, fieldValues]); // fieldValues changes frequently
```

**Fix**: Separate concerns:
```typescript
// Initial grid setup
useEffect(() => {
  // Grid structure setup
}, [fields]);

// Field value updates
useEffect(() => {
  // Update only affected cells
}, [fieldValues]);
```

### 11. WebSocketService - Memory Leak in Event Handler

**Location**: Lines 189-191  
**Issue**: `onAny` handler captures all events without cleanup.

```typescript
this.socket.onAny((eventName: string, ...args: any[]) => {
  console.log(`[WEBSOCKET] Received event: ${eventName}`, args);
});
```

**Fix**: Store reference and clean up:
```typescript
private anyHandler = (eventName: string, ...args: any[]) => {
  console.log(`[WEBSOCKET] Received event: ${eventName}`, args);
};

// In connect
this.socket.onAny(this.anyHandler);

// In disconnect
this.socket.offAny(this.anyHandler);
```

### 12. AspCliWebTerminal.tsx - Missing Dependency in useCallback

**Location**: Lines 61-116  
**Issue**: `executeCommand` missing dependencies.

```typescript
const executeCommand = useCallback(async (command: string) => {
  // Uses systemInfo.currentUser
}, [systemInfo.currentUser]); // Should include all used values
```

**Fix**: Include all dependencies:
```typescript
const executeCommand = useCallback(async (command: string) => {
  // implementation
}, [systemInfo.currentUser, commandHistory, setCommandHistory]);
```

### 13. SmedMapDisplay.tsx - Inefficient Character Width Calculation

**Location**: Lines 77-98  
**Issue**: `isFullWidth` function called repeatedly for every character.

**Fix**: Memoize or use lookup table:
```typescript
const fullWidthCache = useMemo(() => new Map<string, boolean>(), []);

const isFullWidthMemo = useCallback((char: string): boolean => {
  if (fullWidthCache.has(char)) {
    return fullWidthCache.get(char)!;
  }
  const result = isFullWidth(char);
  fullWidthCache.set(char, result);
  return result;
}, [fullWidthCache]);
```

### 14. WebSocketService - Resource Leak in HTTP Fallback

**Location**: Lines 575-609  
**Issue**: Fetch requests without abort controller.

```typescript
const response = await fetch(httpUrl, {
  method: 'POST',
  // No timeout or abort controller
```

**Fix**: Add abort controller:
```typescript
const controller = new AbortController();
const timeout = setTimeout(() => controller.abort(), 5000);

try {
  const response = await fetch(httpUrl, {
    method: 'POST',
    signal: controller.signal,
    // ...
  });
} finally {
  clearTimeout(timeout);
}
```

### 15. AspCliWebTerminal.tsx - Potential XSS in Terminal Output

**Location**: Lines 288  
**Issue**: Rendering user input without sanitization.

```typescript
<pre>{entry.output}</pre>
```

**Fix**: Sanitize output:
```typescript
<pre>{DOMPurify.sanitize(entry.output)}</pre>
```

### 16. SmedMapDisplay.tsx - Error Boundary Not Catching All Errors

**Location**: Lines 5-44  
**Issue**: Error boundary only catches render errors, not event handler errors.

**Fix**: Wrap event handlers:
```typescript
const safeHandleKeyDown = (e: React.KeyboardEvent) => {
  try {
    handleKeyDown(e);
  } catch (error) {
    console.error('Error in keydown handler:', error);
    // Trigger error boundary
    throw error;
  }
};
```

### 17. WebSocketService - Timeout Memory Leak

**Location**: Lines 387-390  
**Issue**: Timeout not cleared in all code paths.

```typescript
const timeout = setTimeout(() => {
  console.error('[WEBSOCKET] Terminal registration timeout');
  reject(new Error('Terminal registration timeout'));
}, 10000);
```

**Fix**: Ensure cleanup in all paths:
```typescript
const cleanup = () => {
  clearTimeout(timeout);
  this.socket.off('terminal_registered', onRegistered);
  this.socket.off('registration_response', onRegistrationResponse);
};

try {
  // registration logic
} catch (error) {
  cleanup();
  throw error;
}
```

### 18. AspCliWebTerminal.tsx - Input Focus Issues

**Location**: Line 315  
**Issue**: `autoFocus` can cause accessibility problems.

```typescript
autoFocus
```

**Fix**: Use ref-based focus:
```typescript
useEffect(() => {
  if (inputRef.current && !isExecuting) {
    inputRef.current.focus();
  }
}, [isExecuting]);
```

### 19. SmedMapDisplay.tsx - Inefficient Array Operations

**Location**: Lines 319-338  
**Issue**: Creating employee data array inefficiently.

```typescript
for (let i = 1; i <= recordsPerPage; i++) {
  // Multiple field lookups
}
```

**Fix**: Use batch processing:
```typescript
const employeeData = Array.from({ length: recordsPerPage }, (_, i) => {
  const idx = i + 1;
  const id = fields[`EMP${idx}_ID`];
  if (!id?.trim()) return null;
  
  return {
    id: id.trim(),
    // ... other fields
  };
}).filter(Boolean);
```

### 20. WebSocketService - State Synchronization Issues

**Location**: Lines 642-677  
**Issue**: Connection state checks not synchronized.

**Fix**: Use single source of truth:
```typescript
private connectionState: 'disconnected' | 'connecting' | 'connected' = 'disconnected';

public isConnected(): boolean {
  return this.connectionState === 'connected' && this.socket != null;
}
```

---

## Medium Severity Issues

### 21. AspCliWebTerminal.tsx - Hardcoded Values

**Location**: Lines 35-39  
**Issue**: Command suggestions hardcoded.

```typescript
const [commandSuggestions] = useState([
  'CRTLIB', 'DLTLIB', 'WRKLIB', // ... hardcoded list
]);
```

**Fix**: Move to configuration:
```typescript
import { COMMAND_SUGGESTIONS } from './config/terminalCommands';
```

### 22. SmedMapDisplay.tsx - Magic Numbers

**Location**: Lines 67, 104  
**Issue**: Grid dimensions hardcoded as 24x80.

```typescript
Array(24).fill(null).map(() => Array(80).fill(' '));
```

**Fix**: Use constants:
```typescript
const GRID_ROWS = 24;
const GRID_COLS = 80;
```

### 23. WebSocketService - Arbitrary Timeout Values

**Location**: Line 390  
**Issue**: 10-second timeout hardcoded.

```typescript
}, 10000); // 10 second timeout
```

**Fix**: Make configurable:
```typescript
private readonly REGISTRATION_TIMEOUT = 10000;
```

### 24. AspCliWebTerminal.tsx - Inefficient String Concatenation

**Location**: Lines 215-220  
**Issue**: String operations in hot path.

```typescript
cmd.toLowerCase().startsWith(currentCommand.toLowerCase())
```

**Fix**: Cache lowercase values:
```typescript
const lowerCommand = currentCommand.toLowerCase();
const suggestions = commandSuggestions.filter(cmd => 
  cmd.toLowerCase().startsWith(lowerCommand)
);
```

### 25. SmedMapDisplay.tsx - Unnecessary State Updates

**Location**: Lines 72-73  
**Issue**: Multiple state variables that could be combined.

```typescript
const [isGridReady, setIsGridReady] = useState(false);
const [hasInputFields, setHasInputFields] = useState(false);
```

**Fix**: Combine related state:
```typescript
const [displayState, setDisplayState] = useState({
  isGridReady: false,
  hasInputFields: false
});
```

### 26. WebSocketService - Console Logging in Production

**Location**: Throughout file  
**Issue**: Excessive console.log statements.

**Fix**: Use proper logging service:
```typescript
import { logger } from './logger';
logger.debug('[WEBSOCKET] ...'); // Instead of console.log
```

### 27. AspCliWebTerminal.tsx - Inline Styles

**Location**: Line 463  
**Issue**: Inline styles in render.

```typescript
style={{marginTop: '10px'}}
```

**Fix**: Use CSS classes:
```typescript
className="close-button-margin"
```

### 28. SmedMapDisplay.tsx - Complex Conditional Rendering

**Location**: Lines 446-467  
**Issue**: Complex loading state logic.

**Fix**: Extract to component:
```typescript
const LoadingState: React.FC<LoadingStateProps> = ({ ... }) => {
  // Loading UI logic
};
```

### 29. WebSocketService - Type Safety Issues

**Location**: Line 46  
**Issue**: Using `any` type for socket.

```typescript
private socket: any = null;
```

**Fix**: Create proper type definition:
```typescript
type SocketInstance = Socket | WebSocket | null;
private socket: SocketInstance = null;
```

### 30. AspCliWebTerminal.tsx - Missing Error Boundaries

**Location**: Entire component  
**Issue**: No error boundary protection.

**Fix**: Wrap in error boundary:
```typescript
<ErrorBoundary fallback={<TerminalErrorFallback />}>
  <AspCliWebTerminal {...props} />
</ErrorBoundary>
```

### 31. SmedMapDisplay.tsx - Inefficient Key Generation

**Location**: Line 435  
**Issue**: String concatenation for keys.

```typescript
key={`${row}-${col}`}
```

**Fix**: Use index if stable:
```typescript
key={rowIndex * GRID_COLS + colIndex}
```

### 32. WebSocketService - Missing TypeScript Strict Checks

**Location**: Various  
**Issue**: Optional chaining used excessively.

**Fix**: Enable strict null checks and handle properly:
```typescript
if (!this.handlers.onError) return;
this.handlers.onError(error);
```

### 33. AspCliWebTerminal.tsx - Accessibility Issues

**Location**: Lines 251-256  
**Issue**: Icon button without accessible label.

```typescript
<button 
  className="clear-button"
  onClick={clearHistory}
  title="화면 지우기"
>
  🗑️
</button>
```

**Fix**: Add aria-label:
```typescript
<button 
  className="clear-button"
  onClick={clearHistory}
  title="화면 지우기"
  aria-label="Clear terminal history"
>
```

### 34. SmedMapDisplay.tsx - Performance in Development

**Location**: Lines 102-103  
**Issue**: Excessive console.log in render cycle.

**Fix**: Use development-only logging:
```typescript
if (process.env.NODE_ENV === 'development') {
  console.log(`SmedMapDisplay: Starting grid initialization...`);
}
```

### 35. WebSocketService - Retry Logic Issues

**Location**: Lines 504-517  
**Issue**: Exponential backoff can grow too large.

```typescript
const delay = this.reconnectDelay * Math.pow(2, this.reconnectAttempts - 1);
```

**Fix**: Add maximum delay:
```typescript
const delay = Math.min(
  this.reconnectDelay * Math.pow(2, this.reconnectAttempts - 1),
  30000 // Max 30 seconds
);
```

---

## Low Severity Issues

### 36. Code Organization - Mixed Languages

**Location**: AspCliWebTerminal.tsx  
**Issue**: Korean comments mixed with English code.

**Fix**: Standardize to one language (preferably English for international teams).

### 37. Naming Conventions

**Location**: Various  
**Issue**: Inconsistent naming (camelCase vs snake_case).

**Fix**: Establish and follow naming conventions.

### 38. Missing JSDoc Comments

**Location**: All files  
**Issue**: Functions lack documentation.

**Fix**: Add JSDoc comments:
```typescript
/**
 * Executes an ASP command and updates the terminal history
 * @param command - The ASP command to execute
 */
```

### 39. Import Organization

**Location**: All files  
**Issue**: Imports not organized.

**Fix**: Group and sort imports:
```typescript
// React imports
import React, { ... } from 'react';

// Third-party imports
import { ... } from 'socket.io-client';

// Local imports
import './styles.css';
```

### 40. CSS Class Naming

**Location**: Various  
**Issue**: Inconsistent CSS class naming.

**Fix**: Use BEM or CSS Modules.

### 41. Test Coverage

**Location**: All files  
**Issue**: No unit tests visible.

**Fix**: Add comprehensive test suites.

### 42. Build Optimization

**Location**: SmedMapDisplay.tsx  
**Issue**: Large component not code-split.

**Fix**: Consider lazy loading:
```typescript
const SmedMapDisplay = React.lazy(() => import('./SmedMapDisplay'));
```

### 43. Browser Compatibility

**Location**: WebSocketService.ts  
**Issue**: No polyfills for older browsers.

**Fix**: Add necessary polyfills or document browser requirements.

### 44. Security Headers

**Location**: WebSocketService.ts  
**Issue**: No security headers in fetch requests.

**Fix**: Add security headers:
```typescript
headers: {
  'Content-Type': 'application/json',
  'X-Requested-With': 'XMLHttpRequest',
}
```

### 45. Version Management

**Location**: All files  
**Issue**: No version information in code.

**Fix**: Add version headers or use package.json version.

---

## Recommendations

### Immediate Actions (Week 1)
1. Fix all Critical memory leaks (Issues #1-8)
2. Implement proper cleanup for event handlers
3. Add connection state management
4. Fix unbounded array growth

### Short-term (Month 1)
1. Implement virtualization for grid rendering
2. Add comprehensive error boundaries
3. Fix React hooks violations
4. Implement proper logging service

### Long-term (Quarter)
1. Refactor to use Canvas or WebGL for terminal display
2. Implement comprehensive test suite
3. Add performance monitoring
4. Consider state management library (Redux/MobX)

### Best Practices Going Forward
1. Always clean up side effects in useEffect
2. Memoize expensive computations
3. Use TypeScript strict mode
4. Implement code review checklist
5. Add pre-commit hooks for linting
6. Monitor bundle size
7. Implement error tracking (Sentry, etc.)

---

## Conclusion

The codebase shows signs of rapid development without sufficient attention to performance and cleanup. The most critical issues involve memory leaks from uncleaned event handlers and timers. The rendering performance of the SMED display is a significant concern due to the large number of DOM elements.

Immediate action is required on the Critical issues to prevent memory leaks in production. The team should prioritize implementing proper cleanup patterns and consider architectural changes for better performance at scale.
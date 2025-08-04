# Duplicate Structure Analysis Report

## Overview
The `/home/aspuser/app/ofasp-refactor` directory contains significant duplication and nested redundancy that's making the project structure confusing. This report identifies the duplications and provides recommendations for cleanup.

## Phase 1 Memory Leak Prevention APIs

### 新しいAPI追加 (New APIs Added)

#### 1. Memory Leak Detector API
**場所**: `/ofasp-refactor/src/utils/memoryLeakDetector.ts`
**機能**: Timer and event listener tracking with automatic cleanup
**使用法**:
```typescript
import { memoryLeakDetector } from '../utils/memoryLeakDetector';

// Timer registration
const timerId = memoryLeakDetector.registerInterval(() => {
  console.log('Timer tick');
}, 1000, 'Description');

// Event listener registration  
const cleanup = memoryLeakDetector.addEventListener(element, 'click', handler);

// Diagnostics
const stats = memoryLeakDetector.getDiagnostics();
```

#### 2. Timer Management Hook
**場所**: `/ofasp-refactor/src/hooks/useTimerManager.ts`
**機能**: React hook for safe timer management
**使用法**:
```typescript
import { useTimerManager } from '../hooks/useTimerManager';

const { startSystemTimer, stopSystemTimer, cleanup } = useTimerManager();

// Start system timer
startSystemTimer(() => updateTime(), 1000);

// Automatic cleanup on unmount
```

#### 3. Command History Hook  
**場所**: `/ofasp-refactor/src/hooks/useCommandHistory.ts`
**機能**: Bounded command history with memory safety
**使用法**:
```typescript
import { useCommandHistory } from '../hooks/useCommandHistory';

const { history, addCommand, clearHistory } = useCommandHistory({
  maxHistorySize: 100
});

// Add command with automatic size limiting
addCommand({ command: 'test', output: 'result', success: true });
```

#### 4. Browser Events Hook
**場所**: `/ofasp-refactor/src/hooks/useBrowserEvents.ts`  
**機能**: Safe browser event management with cleanup
**使用法**:
```typescript
import { useBrowserEvents } from '../hooks/useBrowserEvents';

const { setupBeforeUnload, cleanup } = useBrowserEvents();

// Setup page unload cleanup
setupBeforeUnload({ user: 'admin' });
```

### API設計原則
- **再利用性**: 全て独立したモジュールとして設計
- **型安全性**: 完全なTypeScript対応
- **メモリ安全**: 自動クリーンアップ機能内蔵
- **設定可能**: ハードコーディング排除、設定ベース
- **テスト可能**: 単体テスト対応設計

## Phase 2 Event Handler Management APIs

### 新しいAPI追加 (New APIs Added)

#### 1. Event Handler Registry System
**場所**: `/ofasp-refactor/src/utils/eventHandlerRegistry.ts`
**機能**: Centralized event handler management with deduplication
**使用法**:
```typescript
import { eventHandlerRegistry } from '../utils/eventHandlerRegistry';

// Single handler registration
const handlerId = eventHandlerRegistry.registerHandler('click', handler, {
  id: 'custom_id',
  description: 'Button click handler'
});

// WebSocket handler registration
const wsHandlerId = eventHandlerRegistry.registerWebSocketHandler(
  'message', handler, 'componentId', 'WebSocket message handler'
);

// Bulk registration
const handlerIds = eventHandlerRegistry.registerComponentHandlers('MyComponent', [
  { event: 'click', handler: clickHandler },
  { event: 'focus', handler: focusHandler }
]);

// Statistics
const stats = eventHandlerRegistry.getStatistics();
```

#### 2. Event Handler Hook
**場所**: `/ofasp-refactor/src/hooks/useEventHandlers.ts`
**機能**: React hook for component event handler management
**使用法**:
```typescript
import { useEventHandlers } from '../hooks/useEventHandlers';

const { registerHandler, registerWebSocketHandler, cleanup } = 
  useEventHandlers('ComponentName');

// Register handlers
const handlerId = registerHandler('click', handleClick, {
  description: 'Main button click'
});

const wsHandlerId = registerWebSocketHandler('message', handleMessage);

// Automatic cleanup on unmount
```

#### 3. Grid Event Delegation System
**場所**: `/ofasp-refactor/src/utils/gridEventDelegation.ts`
**機能**: Event delegation for large grids (24x80 = 1,920 cells)
**使用法**:
```typescript
import { GridEventDelegation } from '../utils/gridEventDelegation';

const delegator = new GridEventDelegation(containerElement, {
  gridRows: 24,
  gridCols: 80
});

// Register cell click handler
delegator.registerCellHandler('click', (position, event) => {
  console.log(`Clicked cell: ${position.row}, ${position.col}`);
});

// Multiple handlers
delegator.registerMultipleCellHandlers([
  { eventType: 'click', handler: handleCellClick },
  { eventType: 'focus', handler: handleCellFocus }
]);

// Statistics
const stats = delegator.getStatistics();
```

## 1. Duplicate MD Files

### Exact Duplicates (same MD5 hash):
- `ASP_COMMANDS_UPDATE_SUMMARY.md` - Identical in both locations
- `CLAUDE_SMED.md` - Identical in both locations  
- `CODING_RULES.md` - Identical in both locations
- `MASTER_CLAUDE.md` - Identical in both locations
- `README.md` - Identical in both locations

### Unique to ofasp-refactor:
- `WEBSOCKET_HUB_CLIENT_INTEGRATION.md`
- `WEBSOCKET_INTEGRATION_SUMMARY.md`
- `WEBSOCKET_SMED_FIXES_SUMMARY.md`

### Unique to root:
- `CLAUDE.md` (empty file in root)
- `POSITION_BASED_SMED_API.md`
- `UNIFIED_API_DOCUMENTATION.md`

## 2. Duplicate Directories

### Complete Directory Duplications:

#### asp-manager
- `/home/aspuser/app/asp-manager/` (original)
- `/home/aspuser/app/ofasp-refactor/asp-manager/` (duplicate)
- Both contain identical files and structure
- The duplicate has its own node_modules (758MB vs 743MB)

#### Nested ofasp-refactor
- `/home/aspuser/app/ofasp-refactor/ofasp-refactor/` - A nested duplicate!
- Contains different content than parent, appears to be an older version
- Has its own bore binary (7MB) and logs
- Contains Korean text report and different MD files

## 3. Server Directories
- `/home/aspuser/app/server/` - Main server (more complete)
- `/home/aspuser/app/ofasp-refactor/server/` - Partial duplicate
- The ofasp-refactor version is missing many files present in the main server:
  - Missing: cobol conversion tools, test files, demo HTML files
  - Has different api_server.py (92KB vs 176KB in main)

## 4. Volume Structure
- Main volume has complete structure: JAVA, PRODLIB, SMED, TESTLIB, XMLLIB
- ofasp-refactor volume only has: PRODLIB, TESTLIB (missing JAVA, SMED, XMLLIB)

## 5. Other Duplications
- Multiple package.json files indicating separate npm projects
- Duplicate node_modules directories consuming significant space
- Duplicate python-service directories
- Duplicate src directories with React components

## Recommendations for Cleanup

### 1. Remove Complete Duplicates
```bash
# Remove duplicate MD files in ofasp-refactor
rm /home/aspuser/app/ofasp-refactor/ASP_COMMANDS_UPDATE_SUMMARY.md
rm /home/aspuser/app/ofasp-refactor/CLAUDE_SMED.md
rm /home/aspuser/app/ofasp-refactor/CODING_RULES.md
rm /home/aspuser/app/ofasp-refactor/MASTER_CLAUDE.md
rm /home/aspuser/app/ofasp-refactor/README.md
```

### 2. Remove Nested ofasp-refactor
```bash
# This appears to be accidentally nested
rm -rf /home/aspuser/app/ofasp-refactor/ofasp-refactor/
```

### 3. Consolidate asp-manager
```bash
# Remove duplicate asp-manager
rm -rf /home/aspuser/app/ofasp-refactor/asp-manager/
```

### 4. Review and Merge Server Directories
- The main server directory is more complete
- Review any unique changes in ofasp-refactor/server/
- Consider removing the duplicate after merging any valuable changes

### 5. Clean up Volume Structure
```bash
# Remove incomplete volume structure
rm -rf /home/aspuser/app/ofasp-refactor/volume/
```

### 6. Consolidate Unique Files
Move these unique files to appropriate locations:
- WEBSOCKET_*.md files from ofasp-refactor to main docs directory
- Any unique server configurations or implementations

### 7. Remove node_modules and Rebuild
```bash
# Remove all duplicate node_modules
rm -rf /home/aspuser/app/ofasp-refactor/node_modules/
rm -rf /home/aspuser/app/ofasp-refactor/asp-manager/node_modules/
```

## Impact Analysis
- **Space Savings**: Approximately 1-2GB from removing duplicate node_modules
- **Clarity**: Removing nested directories will significantly improve navigation
- **Maintenance**: Single source of truth for each component

## Safe Cleanup Script
```bash
#!/bin/bash
# Create backup first
tar -czf ofasp-refactor-backup.tar.gz /home/aspuser/app/ofasp-refactor/

# Move unique documentation
mv /home/aspuser/app/ofasp-refactor/WEBSOCKET_*.md /home/aspuser/app/docs/

# Remove duplicates
rm -rf /home/aspuser/app/ofasp-refactor/ofasp-refactor/
rm -rf /home/aspuser/app/ofasp-refactor/asp-manager/
rm -rf /home/aspuser/app/ofasp-refactor/node_modules/
rm -rf /home/aspuser/app/ofasp-refactor/volume/

# Remove duplicate MD files
rm /home/aspuser/app/ofasp-refactor/{ASP_COMMANDS_UPDATE_SUMMARY,CLAUDE_SMED,CODING_RULES,MASTER_CLAUDE,README}.md
```

## Conclusion
The ofasp-refactor directory appears to be a partially complete attempt at refactoring that has accumulated duplicates and nested structures. Most of its content is redundant with the main project structure. After backing up any unique changes, the majority of this directory can be safely removed to clean up the project structure.
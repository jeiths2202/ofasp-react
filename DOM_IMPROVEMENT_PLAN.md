# DOM Improvements Implementation Plan

## 🎯 Phase-based Improvement Strategy

### Phase 1: Memory Leak Issues (Critical Priority)
**Target**: Fix immediate memory leaks that cause progressive performance degradation

#### 1.1 Timer Cleanup in AspCliWebTerminal.tsx
- **Location**: Line 696-705
- **Issue**: System time update timer without cleanup
- **Fix**: Add proper cleanup in useEffect return function

#### 1.2 Event Listener Cleanup
- **Location**: Line 667-693 (beforeunload)
- **Issue**: Browser event listener not properly removed
- **Fix**: Implement proper cleanup pattern

#### 1.3 Command History Growth Control
- **Location**: Line 28 (commandHistory state)
- **Issue**: Unbounded array growth
- **Fix**: Implement maximum history size with sliding window

### Phase 2: Event Handler Management Problems (High Priority)
**Target**: Eliminate duplicate event handlers and optimize handler registration

#### 2.1 WebSocket Event Handler Deduplication
- **Location**: Line 598-612 (AspCliWebTerminal.tsx)
- **Issue**: Multiple handlers for same events
- **Fix**: Implement event handler registry

#### 2.2 Grid Cell Handler Optimization  
- **Location**: Line 366-387 (SmedMapDisplay.tsx)
- **Issue**: 1,920 individual click handlers
- **Fix**: Event delegation pattern

### Phase 3: Component Complexity Analysis (Medium Priority)
**Target**: Break down monolithic components into manageable pieces

#### 3.1 AspCliWebTerminal Decomposition
- **Current**: 2,194 lines single component
- **Target**: 5-6 sub-components (~300-400 lines each)
- **Components to extract**:
  - TerminalHeader
  - TerminalBody  
  - CommandInput
  - SystemInfo
  - ConnectionStatus

#### 3.2 Custom Hooks Extraction
- **useWebSocketConnection**: WebSocket management logic
- **useTerminalState**: Terminal state management
- **useCommandHistory**: Command processing logic

### Phase 4: WebSocket Connection Management (Medium Priority)
**Target**: Simplify connection state management and eliminate race conditions

#### 4.1 Connection State Consolidation
- **Current**: Multiple boolean flags across files
- **Target**: Single state machine pattern
- **Implementation**: useReducer for connection state

#### 4.2 Event Handler Consolidation
- **Current**: Duplicate events (smed_data_received, smed_display, smed_data_direct)
- **Target**: Single event type with proper routing
- **Implementation**: Event router pattern

### Phase 5: DOM Manipulation Anti-Patterns (Low Priority)
**Target**: Replace direct DOM access with React patterns

#### 5.1 Virtual Grid Implementation
- **Current**: 1,920 individual DOM elements
- **Target**: Virtualized grid with react-window
- **Benefits**: Reduced memory usage, better performance

#### 5.2 Remove Direct DOM Queries
- **Current**: document.querySelector usage
- **Target**: Proper React refs and state management

## 🛠️ Implementation Rollback Strategy

### Git Branch Structure
```
master (baseline)
├── dom-improvements (main branch)
├── phase-1-memory-leaks
├── phase-2-event-handlers  
├── phase-3-component-split
├── phase-4-websocket-mgmt
└── phase-5-dom-patterns
```

### Rollback Points
1. **Immediate Rollback**: `git checkout master`
2. **Phase Rollback**: `git checkout phase-X-name`
3. **Feature Rollback**: `git revert <commit-hash>`

### Testing Strategy per Phase
- **Unit Tests**: Component behavior verification
- **Integration Tests**: WebSocket connection stability
- **Performance Tests**: Memory usage monitoring
- **E2E Tests**: Full user workflow validation

## 📊 Success Metrics per Phase

### Phase 1 Success Criteria
- [ ] Memory usage remains stable over 24 hours
- [ ] No timer leaks detected in dev tools
- [ ] Event listeners count remains constant
- [ ] Command history size limited to 100 entries

### Phase 2 Success Criteria  
- [ ] Event handler count reduced by >50%
- [ ] Grid interaction response time <100ms
- [ ] WebSocket reconnection success rate >95%
- [ ] No duplicate event handler registrations

### Phase 3 Success Criteria
- [ ] No components >500 lines
- [ ] Component render time <16ms (60 FPS)
- [ ] Bundle size reduced by 15%
- [ ] Maintainability index improved

### Phase 4 Success Criteria
- [ ] Connection state race conditions eliminated
- [ ] Single connection management pattern
- [ ] Event routing performance optimized
- [ ] Connection recovery time <2 seconds

### Phase 5 Success Criteria
- [ ] Grid rendering performance >60 FPS
- [ ] Memory usage for grid <10MB
- [ ] No direct DOM manipulation
- [ ] Virtual scrolling implemented

## 🔄 Rollback Procedures

### Emergency Rollback (Production Issues)
```bash
git checkout master
npm run build
# Restart services
```

### Phase Rollback (Feature Issues)
```bash
git checkout phase-N-name
git branch -D dom-improvements
git checkout -b dom-improvements
# Continue from stable phase
```

### Commit Rollback (Specific Issue)
```bash
git revert <problematic-commit-hash>
git commit -m "revert: Roll back problematic DOM change"
```

## 📝 Progress Tracking

### Current Status
- [x] Baseline established (master branch)
- [x] Analysis completed
- [ ] Phase 1 implementation
- [ ] Phase 2 implementation  
- [ ] Phase 3 implementation
- [ ] Phase 4 implementation
- [ ] Phase 5 implementation

### Implementation Schedule
- **Week 1**: Phase 1 (Memory Leaks)
- **Week 2**: Phase 2 (Event Handlers)
- **Week 3**: Phase 3 (Component Split)
- **Week 4**: Phase 4 (WebSocket Management)
- **Week 5**: Phase 5 (DOM Patterns)
- **Week 6**: Testing & Documentation

---

**Note**: Each phase will be implemented as a separate feature branch that can be individually tested and rolled back if needed.
# Advanced MapEditor Features - Implementation Guide

## Overview

The OpenASP AX MapEditor has been enhanced with advanced item manipulation features that provide precise control over SMED map field positioning and selection. This implementation introduces a sophisticated interaction system designed around the requirements:

1. **Single Item Movement**: Items only move with Ctrl + Mouse Click + Drag or Ctrl + Arrow Keys
2. **Multi-Item Group Movement**: Shift + Mouse Click selects multiple items that move together as a group

## Architecture

### Core Components

#### 1. FieldSelectionManager (`/src/utils/FieldSelectionManager.ts`)
- **Purpose**: Manages field selection state and provides selection query methods
- **Key Features**:
  - Single and multi-select state management
  - Selection validation and conflict resolution
  - Bounding box calculations for group operations
  - Movement feasibility checking

#### 2. KeyboardInteractionHandler (`/src/utils/KeyboardInteractionHandler.ts`)
- **Purpose**: Handles Ctrl+Arrow key movement operations
- **Key Features**:
  - Ctrl+Arrow key movement with collision detection
  - Configurable movement step size
  - Boundary constraint validation
  - Group movement coordination

#### 3. MouseInteractionHandler (`/src/utils/MouseInteractionHandler.ts`)
- **Purpose**: Manages Ctrl+Click selection and drag operations
- **Key Features**:
  - Ctrl+Click selection with visual feedback
  - Drag threshold detection for smooth interactions
  - Multi-field drag coordination
  - Grid-based position snapping

#### 4. GroupMovementManager (`/src/utils/GroupMovementManager.ts`)
- **Purpose**: Coordinates multi-field movement operations
- **Key Features**:
  - Advanced collision detection
  - Field alignment and distribution utilities
  - Movement constraint validation
  - Smart snap-to-grid functionality

#### 5. AdvancedFieldRenderer (`/src/components/AdvancedFieldRenderer.tsx`)
- **Purpose**: Enhanced field rendering with visual feedback
- **Key Features**:
  - Selection state visualization
  - Drag operation feedback
  - Movement guides and snap indicators
  - Collision warning indicators

## Usage Guide

### Basic Field Operations

#### Creating Fields
1. Drag field types from the toolbar to the grid
2. Fields are automatically positioned at the drop location
3. Newly created fields are automatically selected

#### Selecting Fields
- **Single Selection**: `Ctrl + Click` on a field
- **Multi-Selection**: `Ctrl + Shift + Click` to add fields to selection
- **Clear Selection**: `Ctrl + Click` on empty grid area

#### Moving Fields
- **Keyboard Movement**: Select fields, then use `Ctrl + Arrow Keys`
- **Mouse Movement**: `Ctrl + Click + Drag` selected fields
- **Group Movement**: Select multiple fields, then move with either method

### Advanced Features

#### Field Selection
```typescript
// Single field selection
Ctrl + Click → Selects individual field (yellow highlight)

// Multi-field selection  
Ctrl + Shift + Click → Adds field to selection group (orange highlight)

// Selection clearing
Ctrl + Click (empty area) → Clears all selections
```

#### Movement Controls
```typescript
// Keyboard movement
Ctrl + Arrow Keys → Moves selected fields by 1 grid unit
- Respects grid boundaries
- Prevents field collisions
- Moves all selected fields together

// Mouse movement
Ctrl + Click + Drag → Moves selected fields with mouse
- Real-time collision detection
- Visual movement guides
- Snap-to-grid positioning
```

#### Visual Feedback System
- **Primary Selection**: Yellow border with glow effect
- **Secondary Selection**: Orange border (multi-select)
- **Drag Preview**: Semi-transparent with dotted border
- **Movement Guides**: Green alignment guides during drag
- **Collision Warning**: Red border when movement blocked
- **Snap Indicators**: Corner dots showing snap positions

### Integration Points

#### Component Integration
The enhanced MapEditor integrates seamlessly with existing OpenASP components:

```typescript
// Import the enhanced component
import ASPMapEditorEnhanced from './components/ASPMapEditorEnhanced';

// Use in place of original MapEditor
<ASPMapEditorEnhanced isDarkMode={isDarkMode} />
```

#### Configuration Options
```typescript
// Keyboard interaction configuration
const keyboardConfig: KeyboardConfig = {
  enableArrowMovement: true,
  movementStep: 1,
  gridCols: 80,
  gridRows: 24,
  debug: false
};

// Mouse interaction configuration  
const mouseConfig: MouseConfig = {
  enableCtrlClick: true,
  enableDragMovement: true,
  dragThreshold: 5,
  gridCols: 80,
  gridRows: 24,
  charWidth: 10,
  charHeight: 16,
  debug: false
};

// Group movement configuration
const groupConfig: GroupMovementConfig = {
  gridCols: 80,
  gridRows: 24,
  enableCollisionDetection: true,
  enableSmartAlignment: true,
  snapToGrid: true,
  debug: false
};
```

## Technical Implementation Details

### Event Handling Strategy
The implementation uses a coordinated event handling approach:

1. **Event Capture**: Mouse and keyboard events are captured at the grid level
2. **Event Delegation**: Events are routed to appropriate interaction managers
3. **Conflict Resolution**: Panel drag operations take priority over field interactions
4. **State Synchronization**: All managers maintain synchronized field state

### Collision Detection Algorithm
```typescript
// Boundary checking
const boundsOverlap = (bounds1, bounds2) => {
  return !(bounds1.right < bounds2.left || 
           bounds2.right < bounds1.left || 
           bounds1.bottom < bounds2.top || 
           bounds2.bottom < bounds1.top);
};

// Field collision detection
const checkCollisions = (movingField, targetPosition, existingFields) => {
  const movingBounds = calculateBounds(movingField, targetPosition);
  return existingFields.some(field => 
    boundsOverlap(movingBounds, calculateBounds(field))
  );
};
```

### Performance Optimizations
- **Memoized Calculations**: Field positions and bounds are cached
- **Event Throttling**: Mouse move events are throttled to prevent excessive updates
- **Selective Re-rendering**: Only affected fields are re-rendered during operations
- **State Batching**: Multiple field updates are batched into single state updates

## Team Coordination

### Frontend Development Team
- **Primary Responsibility**: Integration and testing of interaction managers
- **Key Tasks**:
  - Component integration testing
  - Event handling verification
  - UI responsiveness optimization

### React Development Team  
- **Primary Responsibility**: Component architecture and state management
- **Key Tasks**:
  - State synchronization between managers
  - React lifecycle optimization
  - Performance profiling and optimization

### CSS Specialists
- **Primary Responsibility**: Visual feedback and animation systems
- **Key Tasks**:
  - Selection state styling
  - Animation timing and easing
  - Responsive design adaptations

### Backend Integration Team
- **Primary Responsibility**: API compatibility and data persistence
- **Key Tasks**:
  - Field position update APIs
  - SMED file format compatibility
  - Real-time collaboration features (future)

## Testing Strategy

### Unit Tests
```typescript
// FieldSelectionManager tests
describe('FieldSelectionManager', () => {
  test('handles single field selection', () => {
    // Test single selection logic
  });
  
  test('manages multi-field selection', () => {
    // Test multi-selection behavior
  });
  
  test('validates movement constraints', () => {
    // Test boundary and collision checking
  });
});
```

### Integration Tests
```typescript
// Component integration tests
describe('ASPMapEditorEnhanced', () => {
  test('coordinates between interaction managers', () => {
    // Test manager coordination
  });
  
  test('handles concurrent operations', () => {
    // Test conflict resolution
  });
  
  test('maintains state consistency', () => {
    // Test state synchronization
  });
});
```

### End-to-End Tests
- **User Interaction Flows**: Complete workflows from selection to movement
- **Error Handling**: Edge cases and error recovery
- **Performance Testing**: Large field counts and complex operations

## Deployment Considerations

### Browser Compatibility
- **Modern Browsers**: Chrome 90+, Firefox 88+, Safari 14+, Edge 90+
- **Event Support**: Requires modern pointer events and keyboard handling
- **CSS Features**: Uses modern CSS Grid and Flexbox features

### Performance Requirements
- **Field Capacity**: Optimized for up to 100 concurrent fields
- **Response Time**: Sub-100ms interaction response times
- **Memory Usage**: Efficient state management for large field sets

### Accessibility
- **Keyboard Navigation**: Full keyboard accessibility for all operations
- **Screen Readers**: ARIA labels and semantic HTML structure
- **High Contrast**: Support for high contrast display modes
- **Reduced Motion**: Respects user motion preferences

## Future Enhancements

### Planned Features
1. **Field Alignment Tools**: Automatic alignment and distribution utilities
2. **Undo/Redo System**: Operation history with rollback capabilities
3. **Real-time Collaboration**: Multi-user editing with conflict resolution
4. **Advanced Snapping**: Smart guides and magnetic field positioning
5. **Template System**: Save and reuse common field arrangements

### API Extensions
```typescript
// Future API extensions
interface EnhancedMapEditorAPI {
  // Programmatic field manipulation
  selectFields(fieldIds: string[]): void;
  moveFields(fieldIds: string[], deltaX: number, deltaY: number): void;
  alignFields(fieldIds: string[], alignment: AlignmentType): void;
  
  // Template operations
  saveTemplate(name: string): void;
  loadTemplate(name: string): void;
  
  // Collaboration features
  enableCollaboration(sessionId: string): void;
  handleRemoteOperation(operation: RemoteOperation): void;
}
```

## Support and Maintenance

### Documentation Updates
- **User Guide**: End-user documentation with screenshots and examples
- **API Reference**: Complete API documentation for developers
- **Troubleshooting Guide**: Common issues and resolution steps

### Monitoring and Analytics
- **Usage Metrics**: Track feature adoption and usage patterns
- **Performance Monitoring**: Monitor interaction response times
- **Error Tracking**: Capture and analyze user-reported issues

### Version Compatibility
- **Backward Compatibility**: Maintains compatibility with existing SMED files
- **Migration Path**: Clear upgrade path from original MapEditor
- **Feature Flags**: Gradual rollout capabilities for new features

---

## Quick Start

To get started with the enhanced MapEditor:

1. **Import the component**:
   ```typescript
   import ASPMapEditorEnhanced from './components/ASPMapEditorEnhanced';
   ```

2. **Use in your application**:
   ```tsx
   <ASPMapEditorEnhanced isDarkMode={false} />
   ```

3. **Test the features**:
   - Create fields by dragging from toolbar
   - Select fields with Ctrl+Click
   - Move fields with Ctrl+Arrow keys or drag
   - Try multi-selection with Ctrl+Shift+Click

4. **Customize configuration**:
   - Modify interaction manager configs
   - Adjust visual feedback settings
   - Configure collision detection parameters

The enhanced MapEditor provides a robust foundation for advanced SMED map editing with intuitive controls and comprehensive visual feedback.
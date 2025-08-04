/**
 * AdvancedFieldRenderer - Enhanced field rendering with interaction support
 * 
 * Provides advanced visual feedback for field selection, drag operations, and interaction states
 * Integrates with the interaction managers for coordinated field manipulation
 */

import React, { useMemo } from 'react';
import { Field } from './ASPMapEditor';
import { SelectionState } from '../utils/FieldSelectionManager';
import { DragState } from '../utils/MouseInteractionHandler';
import './AdvancedFieldRenderer.css';

export interface AdvancedFieldRendererProps {
  /** Field data to render */
  field: Field;
  /** Current selection state from FieldSelectionManager */
  selectionState: SelectionState;
  /** Current drag state from MouseInteractionHandler */
  dragState: DragState;
  /** Character dimensions for positioning */
  charWidth: number;
  charHeight: number;
  /** Zoom level for rendering */
  zoomLevel: number;
  /** Whether to show field dimensions on hover */
  showDimensions?: boolean;
  /** Whether to show snap indicators */
  showSnapIndicators?: boolean;
  /** Whether to enable visual guides */
  enableVisualGuides?: boolean;
  /** Dark mode flag */
  isDarkMode?: boolean;
  /** Event handlers */
  onMouseDown: (fieldId: string, event: React.MouseEvent) => void;
  onMouseUp: (fieldId: string, event: React.MouseEvent) => void;
  onClick: (fieldId: string, event: React.MouseEvent) => void;
  onDoubleClick?: (fieldId: string, event: React.MouseEvent) => void;
  onContextMenu?: (fieldId: string, event: React.MouseEvent) => void;
}

export interface FieldRenderState {
  /** Selection state */
  isPrimary: boolean;
  isSecondary: boolean;
  isInGroup: boolean;
  /** Interaction state */
  isDragging: boolean;
  isDragParticipant: boolean;
  isDragPreview: boolean;
  /** Movement state */
  isMovementPreview: boolean;
  isMovementBlocked: boolean;
  /** Collision state */
  hasCollisionWarning: boolean;
  hasCollisionError: boolean;
  /** UI state */
  isHovered: boolean;
  isFocused: boolean;
  isKeyboardNavigable: boolean;
}

const AdvancedFieldRenderer: React.FC<AdvancedFieldRendererProps> = ({
  field,
  selectionState,
  dragState,
  charWidth,
  charHeight,
  zoomLevel,
  showDimensions = false,
  showSnapIndicators = false,
  enableVisualGuides = true,
  isDarkMode = false,
  onMouseDown,
  onMouseUp,
  onClick,
  onDoubleClick,
  onContextMenu
}) => {
  // Memoize render state calculation to prevent unnecessary re-renders
  const renderState = useMemo((): FieldRenderState => {
    const isPrimary = selectionState.primaryField === field.id;
    const isSecondary = selectionState.selectedFields.has(field.id) && !isPrimary;
    const isInGroup = selectionState.selectedFields.size > 1 && selectionState.selectedFields.has(field.id);
    
    const isDragging = dragState.isDragging && dragState.draggedFieldId === field.id;
    const isDragParticipant = dragState.isDragging && dragState.participatingFields.has(field.id) && !isDragging;
    const isDragPreview = dragState.previewPositions.has(field.id);
    
    return {
      isPrimary,
      isSecondary,
      isInGroup,
      isDragging,
      isDragParticipant,
      isDragPreview,
      isMovementPreview: false, // Will be set by movement operations
      isMovementBlocked: false,  // Will be set by collision detection
      hasCollisionWarning: false, // Will be set by collision detection
      hasCollisionError: false,   // Will be set by collision detection
      isHovered: false,
      isFocused: false,
      isKeyboardNavigable: selectionState.selectedFields.has(field.id)
    };
  }, [field.id, selectionState, dragState]);

  // Calculate field style based on current state
  const fieldStyle = useMemo((): React.CSSProperties => {
    // Get preview position if available
    const previewPosition = dragState.previewPositions.get(field.id);
    const x = previewPosition ? previewPosition.x : field.x;
    const y = previewPosition ? previewPosition.y : field.y;
    
    const baseStyle: React.CSSProperties = {
      left: `${x * charWidth}px`,
      top: `${y * charHeight}px`,
      width: `${field.width * charWidth}px`,
      height: `${field.height * charHeight}px`,
      fontSize: `${12 * zoomLevel}px`,
      lineHeight: `${16 * zoomLevel}px`,
      padding: `${2 * zoomLevel}px`,
      zIndex: 30, // Base z-index
      position: 'absolute'
    };

    // Adjust z-index based on state
    if (renderState.isDragging) {
      baseStyle.zIndex = 100;
    } else if (renderState.isPrimary) {
      baseStyle.zIndex = 50;
    } else if (renderState.isSecondary) {
      baseStyle.zIndex = 40;
    } else if (renderState.isDragParticipant) {
      baseStyle.zIndex = 90;
    }

    // Apply field background color with appropriate opacity
    if (field.backgroundColor) {
      const opacity = renderState.isDragPreview ? '10' : '20';
      baseStyle.backgroundColor = field.backgroundColor + opacity;
    }

    // Apply text color
    if (field.textColor) {
      baseStyle.color = field.textColor;
    }

    return baseStyle;
  }, [field, dragState, renderState, charWidth, charHeight, zoomLevel]);

  // Build CSS class names based on render state
  const classNames = useMemo(() => {
    const classes = ['advanced-field'];
    
    // Field type
    classes.push(`field-type-${field.type}`);
    
    // Selection states
    if (renderState.isPrimary) classes.push('selected-primary');
    if (renderState.isSecondary) classes.push('selected-secondary');
    if (renderState.isInGroup) classes.push('selected-group');
    
    // Interaction states
    if (renderState.isDragging) classes.push('dragging');
    if (renderState.isDragParticipant) classes.push('drag-participant');
    if (renderState.isDragPreview) classes.push('drag-preview');
    
    // Movement states
    if (renderState.isMovementPreview) classes.push('movement-preview');
    if (renderState.isMovementBlocked) classes.push('movement-blocked');
    
    // Collision states
    if (renderState.hasCollisionWarning) classes.push('collision-warning');
    if (renderState.hasCollisionError) classes.push('collision-error');
    
    // UI states
    if (renderState.isFocused) classes.push('focused');
    if (renderState.isKeyboardNavigable) classes.push('keyboard-navigable');
    
    // Feature flags
    if (showDimensions) classes.push('show-dimensions');
    
    // Theme
    if (isDarkMode) classes.push('dark-mode');
    
    return classes.join(' ');
  }, [field.type, renderState, showDimensions, isDarkMode]);

  // Generate data attributes for debugging and CSS selectors
  const dataAttributes = useMemo(() => ({
    'data-field-id': field.id,
    'data-field-type': field.type,
    'data-field-name': field.name,
    'data-dimensions': `${field.width}×${field.height}`,
    'data-position': `${field.x},${field.y}`,
    'data-selected': renderState.isPrimary || renderState.isSecondary ? 'true' : 'false',
    'data-dragging': renderState.isDragging ? 'true' : 'false'
  }), [field, renderState]);

  // Handle mouse events with proper event delegation
  const handleMouseDown = (event: React.MouseEvent) => {
    event.stopPropagation();
    onMouseDown(field.id, event);
  };

  const handleMouseUp = (event: React.MouseEvent) => {
    event.stopPropagation();
    onMouseUp(field.id, event);
  };

  const handleClick = (event: React.MouseEvent) => {
    event.stopPropagation();
    onClick(field.id, event);
  };

  const handleDoubleClick = (event: React.MouseEvent) => {
    if (onDoubleClick) {
      event.stopPropagation();
      onDoubleClick(field.id, event);
    }
  };

  const handleContextMenu = (event: React.MouseEvent) => {
    if (onContextMenu) {
      event.preventDefault();
      event.stopPropagation();
      onContextMenu(field.id, event);
    }
  };

  // Generate tooltip text
  const tooltipText = useMemo(() => {
    const parts = [`Field: ${field.name}`, `Type: ${field.type}`, `Position: (${field.x}, ${field.y})`, `Size: ${field.width}×${field.height}`];
    
    if (renderState.isPrimary) parts.push('Primary Selection');
    if (renderState.isSecondary) parts.push('Secondary Selection');
    if (renderState.isDragging) parts.push('Dragging');
    if (renderState.isDragParticipant) parts.push('Drag Participant');
    
    return parts.join('\n');
  }, [field, renderState]);

  return (
    <>
      {/* Main field element */}
      <div
        className={classNames}
        style={fieldStyle}
        title={tooltipText}
        tabIndex={renderState.isKeyboardNavigable ? 0 : -1}
        onMouseDown={handleMouseDown}
        onMouseUp={handleMouseUp}
        onClick={handleClick}
        onDoubleClick={handleDoubleClick}
        onContextMenu={handleContextMenu}
        {...dataAttributes}
      >
        {/* Field content with overflow handling */}
        <span className="field-content">
          {field.value.substring(0, field.width * field.height)}
        </span>

        {/* Status overlay for selection indicators */}
        {(renderState.isPrimary || renderState.isSecondary) && (
          <div className={`field-status-overlay ${renderState.isPrimary ? 'selected' : 'secondary'}`}>
            {renderState.isPrimary ? '●' : '○'}
          </div>
        )}

        {/* Snap indicators */}
        {showSnapIndicators && renderState.isDragPreview && (
          <>
            <div className="snap-indicator" style={{ top: -4, left: -4 }} />
            <div className="snap-indicator" style={{ top: -4, right: -4 }} />
            <div className="snap-indicator" style={{ bottom: -4, left: -4 }} />
            <div className="snap-indicator" style={{ bottom: -4, right: -4 }} />
          </>
        )}
      </div>

      {/* Visual guides - rendered as separate elements to avoid z-index conflicts */}
      {enableVisualGuides && renderState.isDragging && (
        <>
          {/* Horizontal movement guide */}
          <div 
            className="movement-guide-horizontal"
            style={{
              top: `${field.y * charHeight + (field.height * charHeight) / 2}px`,
              left: 0,
              width: '100%'
            }}
          />
          {/* Vertical movement guide */}
          <div 
            className="movement-guide-vertical"
            style={{
              left: `${field.x * charWidth + (field.width * charWidth) / 2}px`,
              top: 0,
              height: '100%'
            }}
          />
        </>
      )}
    </>
  );
};

export default AdvancedFieldRenderer;
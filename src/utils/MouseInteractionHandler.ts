/**
 * MouseInteractionHandler - Advanced mouse interaction for MapEditor
 * 
 * Handles Ctrl+Click selection and enhanced drag operations
 * Integrates with FieldSelectionManager for coordinated operations
 */

import { Field } from '../components/ASPMapEditor';
import { FieldSelectionManager } from './FieldSelectionManager';

export interface MouseConfig {
  /** Enable Ctrl+Click selection */
  enableCtrlClick: boolean;
  /** Enable drag operations for selected fields */
  enableDragMovement: boolean;
  /** Drag threshold in pixels before starting drag */
  dragThreshold: number;
  /** Grid constraints */
  gridCols: number;
  gridRows: number;
  /** Character dimensions for position calculations */
  charWidth: number;
  charHeight: number;
  /** Enable debug logging */
  debug: boolean;
}

export interface DragState {
  /** Whether drag is currently active */
  isDragging: boolean;
  /** Field being dragged (for single field drags) */
  draggedFieldId: string | null;
  /** All fields participating in the drag */
  participatingFields: Set<string>;
  /** Starting position of drag */
  startPosition: { x: number; y: number };
  /** Current mouse position */
  currentPosition: { x: number; y: number };
  /** Offset from field origin to mouse */
  dragOffset: { x: number; y: number };
  /** Preview positions for fields */
  previewPositions: Map<string, { x: number; y: number }>;
}

export interface ClickResult {
  /** Whether click was handled */
  handled: boolean;
  /** Action taken */
  action: 'select' | 'deselect' | 'multi-select' | 'clear' | 'ignored';
  /** Fields affected */
  affectedFields: string[];
  /** Reason if click was ignored */
  reason?: string;
}

export class MouseInteractionHandler {
  private config: MouseConfig;
  private selectionManager: FieldSelectionManager;
  private fields: Map<string, Field>;
  private dragState: DragState;
  private onFieldsUpdate: (updatedFields: Map<string, Field>) => void;
  private onStatusMessage: (message: string) => void;
  private gridPanelRef: React.RefObject<HTMLDivElement | null>;

  constructor(
    config: MouseConfig,
    selectionManager: FieldSelectionManager,
    fields: Map<string, Field>,
    onFieldsUpdate: (updatedFields: Map<string, Field>) => void,
    onStatusMessage: (message: string) => void,
    gridPanelRef: React.RefObject<HTMLDivElement | null>
  ) {
    this.config = config;
    this.selectionManager = selectionManager;
    this.fields = fields;
    this.onFieldsUpdate = onFieldsUpdate;
    this.onStatusMessage = onStatusMessage;
    this.gridPanelRef = gridPanelRef;
    
    this.dragState = {
      isDragging: false,
      draggedFieldId: null,
      participatingFields: new Set(),
      startPosition: { x: 0, y: 0 },
      currentPosition: { x: 0, y: 0 },
      dragOffset: { x: 0, y: 0 },
      previewPositions: new Map()
    };
  }

  /**
   * Update fields reference when fields change
   */
  updateFields(fields: Map<string, Field>): void {
    this.fields = fields;
  }

  /**
   * Update configuration
   */
  updateConfig(config: Partial<MouseConfig>): void {
    this.config = { ...this.config, ...config };
  }

  /**
   * Handle mouse down events on fields
   */
  handleFieldMouseDown(fieldId: string, event: React.MouseEvent): boolean {
    if (!this.fields.has(fieldId)) {
      return false;
    }

    // Record interaction timestamp
    const now = Date.now();

    // Stop propagation to prevent grid click handling
    event.stopPropagation();
    event.nativeEvent.stopImmediatePropagation();

    if (event.ctrlKey || event.metaKey) {
      // Ctrl+Click: Handle selection
      event.preventDefault();
      
      const clickResult = this.handleFieldCtrlClick(fieldId, event.shiftKey);
      this.reportClickResult(clickResult);
      
      return true;
    } else {
      // Regular click: Check if we should start drag
      if (this.config.enableDragMovement && this.selectionManager.isSelected(fieldId)) {
        // Selected field clicked without Ctrl - prepare for potential drag
        this.initializeDragState(fieldId, event);
        return true;
      } else {
        // Non-selected field clicked without Ctrl - show hint
        this.onStatusMessage('Use Ctrl+Click to select fields, or Ctrl+Click+Drag to move selected fields');
        return false;
      }
    }
  }

  /**
   * Handle Ctrl+Click on field
   */
  private handleFieldCtrlClick(fieldId: string, withShift: boolean): ClickResult {
    if (!this.config.enableCtrlClick) {
      return {
        handled: false,
        action: 'ignored',
        affectedFields: [],
        reason: 'Ctrl+Click disabled in configuration'
      };
    }

    const wasSelected = this.selectionManager.isSelected(fieldId);
    const success = this.selectionManager.handleCtrlClick(fieldId, withShift);
    
    if (!success) {
      return {
        handled: false,
        action: 'ignored',
        affectedFields: [],
        reason: 'Selection manager rejected the operation'
      };
    }

    let action: ClickResult['action'];
    if (withShift) {
      action = wasSelected ? 'deselect' : 'multi-select';
    } else {
      action = 'select';
    }

    return {
      handled: true,
      action,
      affectedFields: [fieldId],
    };
  }

  /**
   * Initialize drag state for potential drag operation
   */
  private initializeDragState(fieldId: string, event: React.MouseEvent): void {
    if (!this.gridPanelRef.current) {
      return;
    }

    const rect = this.gridPanelRef.current.getBoundingClientRect();
    const field = this.fields.get(fieldId);
    
    if (!field) {
      return;
    }

    // Calculate field position in pixels
    const fieldLeft = field.x * this.config.charWidth;
    const fieldTop = field.y * this.config.charHeight;
    
    // Calculate offset from field origin to mouse
    const dragOffset = {
      x: event.clientX - rect.left - fieldLeft,
      y: event.clientY - rect.top - fieldTop
    };

    // Determine which fields will participate in drag
    const participatingFields = this.selectionManager.hasMultipleSelection() 
      ? new Set(this.selectionManager.getSelectedIds())
      : new Set([fieldId]);

    this.dragState = {
      isDragging: false, // Not dragging yet, just prepared
      draggedFieldId: fieldId,
      participatingFields,
      startPosition: { x: event.clientX, y: event.clientY },
      currentPosition: { x: event.clientX, y: event.clientY },
      dragOffset,
      previewPositions: new Map()
    };

    if (this.config.debug) {
      console.log('MouseInteractionHandler: Initialized drag state for', fieldId, participatingFields);
    }
  }

  /**
   * Handle mouse move events (for drag operations)
   */
  handleMouseMove(event: React.MouseEvent): boolean {
    if (!this.dragState.draggedFieldId) {
      return false;
    }

    const deltaX = event.clientX - this.dragState.startPosition.x;
    const deltaY = event.clientY - this.dragState.startPosition.y;
    const distance = Math.sqrt(deltaX * deltaX + deltaY * deltaY);

    // Check if we should start dragging
    if (!this.dragState.isDragging && distance > this.config.dragThreshold) {
      this.startDragOperation();
    }

    if (this.dragState.isDragging) {
      this.updateDragPreview(event);
      return true;
    }

    return false;
  }

  /**
   * Start the actual drag operation
   */
  private startDragOperation(): void {
    this.dragState.isDragging = true;
    
    // Update cursor and prevent text selection
    document.body.style.cursor = 'grabbing';
    document.body.style.userSelect = 'none';
    
    const fieldCount = this.dragState.participatingFields.size;
    const fieldText = fieldCount === 1 ? 'field' : 'fields';
    
    this.onStatusMessage(`Dragging ${fieldCount} ${fieldText}...`);
    
    if (this.config.debug) {
      console.log('MouseInteractionHandler: Started drag operation', this.dragState.participatingFields);
    }
  }

  /**
   * Update drag preview positions
   */
  private updateDragPreview(event: React.MouseEvent): void {
    if (!this.gridPanelRef.current) {
      return;
    }

    const rect = this.gridPanelRef.current.getBoundingClientRect();
    
    // Calculate new position for the primary dragged field
    const mouseX = event.clientX - rect.left;
    const mouseY = event.clientY - rect.top;
    
    const newFieldX = Math.round((mouseX - this.dragState.dragOffset.x) / this.config.charWidth);
    const newFieldY = Math.round((mouseY - this.dragState.dragOffset.y) / this.config.charHeight);
    
    const primaryField = this.fields.get(this.dragState.draggedFieldId!);
    if (!primaryField) {
      return;
    }

    // Calculate delta from original position
    const deltaX = newFieldX - primaryField.x;
    const deltaY = newFieldY - primaryField.y;

    // Update preview positions for all participating fields
    const newPreviewPositions = new Map<string, { x: number; y: number }>();
    let allWithinBounds = true;

    const participatingFieldsArray = Array.from(this.dragState.participatingFields);
    for (let i = 0; i < participatingFieldsArray.length; i++) {
      const fieldId = participatingFieldsArray[i];
      const field = this.fields.get(fieldId);
      if (!field) continue;

      const previewX = field.x + deltaX;
      const previewY = field.y + deltaY;

      // Check bounds
      if (previewX < 0 || previewX + field.width > this.config.gridCols ||
          previewY < 0 || previewY + field.height > this.config.gridRows) {
        allWithinBounds = false;
      }

      newPreviewPositions.set(fieldId, {
        x: Math.max(0, Math.min(previewX, this.config.gridCols - field.width)),
        y: Math.max(0, Math.min(previewY, this.config.gridRows - field.height))
      });
    }

    this.dragState.previewPositions = newPreviewPositions;
    this.dragState.currentPosition = { x: event.clientX, y: event.clientY };

    // Update status message based on bounds check
    if (!allWithinBounds) {
      this.onStatusMessage('Cannot move outside grid boundaries');
    }
  }

  /**
   * Handle mouse up events (complete drag or cancel)
   */
  handleMouseUp(event: React.MouseEvent): boolean {
    if (!this.dragState.draggedFieldId) {
      return false;
    }

    const wasDragging = this.dragState.isDragging;
    
    if (wasDragging) {
      this.completeDragOperation();
    }

    // Reset drag state
    this.resetDragState();
    
    return wasDragging;
  }

  /**
   * Complete the drag operation by updating field positions
   */
  private completeDragOperation(): void {
    const newFields = new Map(this.fields);
    const movedFields: string[] = [];

    // Apply preview positions to actual fields
    Array.from(this.dragState.previewPositions.entries()).forEach(([fieldId, previewPos]) => {
      const field = this.fields.get(fieldId);
      if (!field) return;

      // Check if position actually changed
      if (field.x !== previewPos.x || field.y !== previewPos.y) {
        const updatedField = { ...field, x: previewPos.x, y: previewPos.y };
        newFields.set(fieldId, updatedField);
        movedFields.push(fieldId);
      }
    });

    if (movedFields.length > 0) {
      this.fields = newFields;
      this.onFieldsUpdate(newFields);
      
      const fieldText = movedFields.length === 1 ? 'field' : 'fields';
      this.onStatusMessage(`Moved ${movedFields.length} ${fieldText}`);
      
      if (this.config.debug) {
        console.log('MouseInteractionHandler: Completed drag, moved fields:', movedFields);
      }
    } else {
      this.onStatusMessage('Fields returned to original positions');
    }
  }

  /**
   * Reset drag state
   */
  private resetDragState(): void {
    this.dragState = {
      isDragging: false,
      draggedFieldId: null,
      participatingFields: new Set(),
      startPosition: { x: 0, y: 0 },
      currentPosition: { x: 0, y: 0 },
      dragOffset: { x: 0, y: 0 },
      previewPositions: new Map()
    };

    // Reset document styles
    document.body.style.cursor = '';
    document.body.style.userSelect = '';
  }

  /**
   * Handle click on grid background (for clearing selection)
   */
  handleGridClick(event: React.MouseEvent): boolean {
    // Only handle if Ctrl is pressed and not in middle of drag
    if (!(event.ctrlKey || event.metaKey) || this.dragState.isDragging) {
      return false;
    }

    // Check if clicking on empty grid area
    const clickedField = this.getFieldAtPosition(event);
    if (clickedField) {
      return false; // Field click will be handled separately
    }

    // Clear selection
    this.selectionManager.clearSelection();
    this.onStatusMessage('Selection cleared');
    
    return true;
  }

  /**
   * Get field at mouse position
   */
  private getFieldAtPosition(event: React.MouseEvent): Field | null {
    if (!this.gridPanelRef.current) {
      return null;
    }

    const rect = this.gridPanelRef.current.getBoundingClientRect();
    const x = event.clientX - rect.left;
    const y = event.clientY - rect.top;
    
    const gridX = Math.floor(x / this.config.charWidth);
    const gridY = Math.floor(y / this.config.charHeight);

    // Find field that encompasses this position
    const fieldsArray = Array.from(this.fields.values());
    for (let i = 0; i < fieldsArray.length; i++) {
      const field = fieldsArray[i];
      if (gridX >= field.x && gridX < field.x + field.width &&
          gridY >= field.y && gridY < field.y + field.height) {
        return field;
      }
    }

    return null;
  }

  /**
   * Report click result to user
   */
  private reportClickResult(result: ClickResult): void {
    if (!result.handled) {
      this.onStatusMessage(result.reason || 'Click was not handled');
      return;
    }

    const fieldCount = result.affectedFields.length;
    let message = '';

    switch (result.action) {
      case 'select':
        message = `Selected field ${result.affectedFields[0]}`;
        break;
      case 'multi-select':
        message = `Added field ${result.affectedFields[0]} to selection`;
        break;
      case 'deselect':
        message = `Removed field ${result.affectedFields[0]} from selection`;
        break;
      case 'clear':
        message = 'Selection cleared';
        break;
    }

    this.onStatusMessage(message);
  }

  /**
   * Get current drag state (for external components)
   */
  getDragState(): Readonly<DragState> {
    return {
      ...this.dragState,
      participatingFields: new Set(this.dragState.participatingFields),
      previewPositions: new Map(this.dragState.previewPositions)
    };
  }

  /**
   * Check if currently dragging
   */
  isDragging(): boolean {
    return this.dragState.isDragging;
  }

  /**
   * Get field preview position during drag
   */
  getFieldPreviewPosition(fieldId: string): { x: number; y: number } | null {
    return this.dragState.previewPositions.get(fieldId) || null;
  }

  /**
   * Cancel any ongoing drag operation
   */
  cancelDrag(): void {
    if (this.dragState.isDragging) {
      this.onStatusMessage('Drag operation cancelled');
    }
    this.resetDragState();
  }
}

export default MouseInteractionHandler;
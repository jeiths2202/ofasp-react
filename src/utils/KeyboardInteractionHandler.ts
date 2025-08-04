/**
 * KeyboardInteractionHandler - Advanced keyboard interaction for MapEditor
 * 
 * Handles Ctrl+Arrow key movement for single and multi-field operations
 * Integrates with FieldSelectionManager for coordinated movement
 */

import { Field } from '../components/ASPMapEditor';
import { FieldSelectionManager } from './FieldSelectionManager';

export interface KeyboardConfig {
  /** Enable Ctrl+Arrow key movement */
  enableArrowMovement: boolean;
  /** Movement step size for arrow keys */
  movementStep: number;
  /** Grid constraints */
  gridCols: number;
  gridRows: number;
  /** Enable debug logging */
  debug: boolean;
}

export interface MovementResult {
  /** Whether movement was successful */
  success: boolean;
  /** Fields that were moved */
  movedFields: Array<{ fieldId: string; oldPos: { x: number; y: number }; newPos: { x: number; y: number } }>;
  /** Reason for failure if not successful */
  failureReason?: string;
  /** Suggested alternative action */
  suggestion?: string;
}

export class KeyboardInteractionHandler {
  private config: KeyboardConfig;
  private selectionManager: FieldSelectionManager;
  private fields: Map<string, Field>;
  private onFieldsUpdate: (updatedFields: Map<string, Field>) => void;
  private onStatusMessage: (message: string) => void;

  constructor(
    config: KeyboardConfig,
    selectionManager: FieldSelectionManager,
    fields: Map<string, Field>,
    onFieldsUpdate: (updatedFields: Map<string, Field>) => void,
    onStatusMessage: (message: string) => void
  ) {
    this.config = config;
    this.selectionManager = selectionManager;
    this.fields = fields;
    this.onFieldsUpdate = onFieldsUpdate;
    this.onStatusMessage = onStatusMessage;
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
  updateConfig(config: Partial<KeyboardConfig>): void {
    this.config = { ...this.config, ...config };
  }

  /**
   * Handle keyboard events
   */
  handleKeyDown(event: KeyboardEvent): boolean {
    if (!this.config.enableArrowMovement) {
      return false;
    }

    // Only handle Ctrl+Arrow key combinations
    if (!event.ctrlKey && !event.metaKey) {
      return false;
    }

    // Check if we have selected fields
    if (!this.selectionManager.hasSelection()) {
      if (this.isArrowKey(event.key)) {
        this.onStatusMessage('No fields selected. Use Ctrl+Click to select fields first.');
        return true; // Prevent default but indicate we handled it
      }
      return false;
    }

    let deltaX = 0;
    let deltaY = 0;
    let handled = false;

    switch (event.key) {
      case 'ArrowLeft':
        deltaX = -this.config.movementStep;
        handled = true;
        break;
      case 'ArrowRight':
        deltaX = this.config.movementStep;
        handled = true;
        break;
      case 'ArrowUp':
        deltaY = -this.config.movementStep;
        handled = true;
        break;
      case 'ArrowDown':
        deltaY = this.config.movementStep;
        handled = true;
        break;
      default:
        return false;
    }

    if (handled) {
      event.preventDefault();
      event.stopPropagation();

      const result = this.moveSelectedFields(deltaX, deltaY);
      this.handleMovementResult(result, event.key);
      
      return true;
    }

    return false;
  }

  /**
   * Move selected fields by the specified delta
   */
  private moveSelectedFields(deltaX: number, deltaY: number): MovementResult {
    const selectedFields = this.selectionManager.getSelectedFields();
    
    if (selectedFields.length === 0) {
      return {
        success: false,
        movedFields: [],
        failureReason: 'No fields selected',
        suggestion: 'Use Ctrl+Click to select fields first'
      };
    }

    // Check if movement is within bounds
    if (!this.selectionManager.canMove(deltaX, deltaY, this.config.gridCols, this.config.gridRows)) {
      return {
        success: false,
        movedFields: [],
        failureReason: 'Movement would exceed grid boundaries',
        suggestion: 'Try moving in a different direction or select fewer fields'
      };
    }

    // Get movement preview to check for collisions
    const movementPreview = this.selectionManager.getMovementPreview(deltaX, deltaY);
    const collisionCheck = this.checkForCollisions(movementPreview);
    
    if (!collisionCheck.success) {
      return {
        success: false,
        movedFields: [],
        failureReason: collisionCheck.reason,
        suggestion: 'Clear the target area or move the conflicting fields first'
      };
    }

    // Perform the movement
    const newFields = new Map(this.fields);
    const movedFields: MovementResult['movedFields'] = [];

    selectedFields.forEach(field => {
      const oldPos = { x: field.x, y: field.y };
      const newPos = { x: field.x + deltaX, y: field.y + deltaY };
      
      const updatedField = { ...field, x: newPos.x, y: newPos.y };
      newFields.set(field.id, updatedField);
      
      movedFields.push({
        fieldId: field.id,
        oldPos,
        newPos
      });
    });

    // Update fields
    this.fields = newFields;
    this.onFieldsUpdate(newFields);

    if (this.config.debug) {
      console.log('KeyboardInteractionHandler: Moved fields', movedFields);
    }

    return {
      success: true,
      movedFields
    };
  }

  /**
   * Check for collisions with non-selected fields
   */
  private checkForCollisions(movementPreview: Array<{ fieldId: string; newX: number; newY: number }>): { success: boolean; reason?: string } {
    const selectedFieldIds = new Set(this.selectionManager.getSelectedIds());
    
    // Get all non-selected fields
    const nonSelectedFields = Array.from(this.fields.values())
      .filter(field => !selectedFieldIds.has(field.id));

    // Check each moving field against all non-selected fields
    for (const preview of movementPreview) {
      const movingField = this.fields.get(preview.fieldId);
      if (!movingField) continue;

      const newBounds = {
        left: preview.newX,
        right: preview.newX + movingField.width - 1,
        top: preview.newY,
        bottom: preview.newY + movingField.height - 1
      };

      // Check against all non-selected fields
      for (const existingField of nonSelectedFields) {
        const existingBounds = {
          left: existingField.x,
          right: existingField.x + existingField.width - 1,
          top: existingField.y,
          bottom: existingField.y + existingField.height - 1
        };

        // Check for overlap
        if (this.boundsOverlap(newBounds, existingBounds)) {
          return {
            success: false,
            reason: `Field ${preview.fieldId} would collide with field ${existingField.id}`
          };
        }
      }
    }

    return { success: true };
  }

  /**
   * Check if two rectangular bounds overlap
   */
  private boundsOverlap(bounds1: { left: number; right: number; top: number; bottom: number }, 
                       bounds2: { left: number; right: number; top: number; bottom: number }): boolean {
    return !(bounds1.right < bounds2.left || 
             bounds2.right < bounds1.left || 
             bounds1.bottom < bounds2.top || 
             bounds2.bottom < bounds1.top);
  }

  /**
   * Handle the result of a movement operation
   */
  private handleMovementResult(result: MovementResult, keyPressed: string): void {
    if (result.success) {
      const direction = this.getDirectionName(keyPressed);
      const fieldCount = result.movedFields.length;
      const fieldText = fieldCount === 1 ? 'field' : 'fields';
      
      this.onStatusMessage(`Moved ${fieldCount} ${fieldText} ${direction}`);
      
      if (this.config.debug) {
        console.log(`KeyboardInteractionHandler: Successfully moved ${fieldCount} fields ${direction}`);
      }
    } else {
      this.onStatusMessage(`Movement failed: ${result.failureReason}`);
      
      if (result.suggestion) {
        setTimeout(() => {
          this.onStatusMessage(result.suggestion!);
        }, 2000);
      }
      
      if (this.config.debug) {
        console.warn('KeyboardInteractionHandler: Movement failed', result);
      }
    }
  }

  /**
   * Get human-readable direction name
   */
  private getDirectionName(key: string): string {
    switch (key) {
      case 'ArrowLeft': return 'left';
      case 'ArrowRight': return 'right';
      case 'ArrowUp': return 'up';
      case 'ArrowDown': return 'down';
      default: return 'unknown direction';
    }
  }

  /**
   * Check if key is an arrow key
   */
  private isArrowKey(key: string): boolean {
    return ['ArrowLeft', 'ArrowRight', 'ArrowUp', 'ArrowDown'].includes(key);
  }

  /**
   * Get suggested next actions based on current state
   */
  getSuggestions(): string[] {
    const suggestions: string[] = [];
    
    if (!this.selectionManager.hasSelection()) {
      suggestions.push('Use Ctrl+Click to select fields');
      suggestions.push('Use Ctrl+Shift+Click to select multiple fields');
    } else {
      suggestions.push('Use Ctrl+Arrow keys to move selected fields');
      
      if (this.selectionManager.hasMultipleSelection()) {
        suggestions.push('All selected fields will move together');
      }
    }
    
    return suggestions;
  }

  /**
   * Get keyboard shortcuts help
   */
  getKeyboardShortcuts(): Array<{ key: string; description: string }> {
    return [
      { key: 'Ctrl+Click', description: 'Select single field' },
      { key: 'Ctrl+Shift+Click', description: 'Add field to selection' },
      { key: 'Ctrl+Arrow Keys', description: 'Move selected fields' },
      { key: 'Ctrl+Click (empty area)', description: 'Clear selection' }
    ];
  }
}

export default KeyboardInteractionHandler;
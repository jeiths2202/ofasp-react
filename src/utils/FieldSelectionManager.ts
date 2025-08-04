/**
 * FieldSelectionManager - Core selection state management for MapEditor
 * 
 * Handles single and multi-select operations for SMED map fields
 * Provides clean API for selection queries and state transitions
 */

import { Field } from '../components/ASPMapEditor';

export interface SelectionState {
  /** Currently focused/primary selected field */
  primaryField: string | null;
  /** Set of all selected field IDs */
  selectedFields: Set<string>;
  /** Selection mode: single or multi */
  mode: 'single' | 'multi';
  /** Last interaction timestamp for conflict resolution */
  lastInteraction: number;
}

export interface SelectionBounds {
  minX: number;
  maxX: number;
  minY: number;
  maxY: number;
  width: number;
  height: number;
}

export class FieldSelectionManager {
  private state: SelectionState;
  private fields: Map<string, Field>;
  private onStateChange: (state: SelectionState) => void;

  constructor(
    fields: Map<string, Field>,
    onStateChange: (state: SelectionState) => void
  ) {
    this.fields = fields;
    this.onStateChange = onStateChange;
    this.state = {
      primaryField: null,
      selectedFields: new Set(),
      mode: 'single',
      lastInteraction: Date.now()
    };
  }

  /**
   * Update fields reference when fields change
   */
  updateFields(fields: Map<string, Field>): void {
    this.fields = fields;
    
    // Clean up selection if any selected fields no longer exist
    const existingFieldIds = new Set(Array.from(fields.keys()));
    const selectedFieldsArray = Array.from(this.state.selectedFields);
    const validSelectedFields = selectedFieldsArray.filter(id => existingFieldIds.has(id));
    
    if (validSelectedFields.length !== selectedFieldsArray.length) {
      this.state.selectedFields = new Set(validSelectedFields);
      
      // Update primary field if it's no longer valid
      if (this.state.primaryField && !existingFieldIds.has(this.state.primaryField)) {
        this.state.primaryField = validSelectedFields.length > 0 ? validSelectedFields[0] : null;
      }
      
      this.notifyStateChange();
    }
  }

  /**
   * Handle Ctrl+Click selection
   */
  handleCtrlClick(fieldId: string, withShift: boolean = false): boolean {
    if (!this.fields.has(fieldId)) {
      console.warn(`FieldSelectionManager: Field ${fieldId} not found`);
      return false;
    }

    this.state.lastInteraction = Date.now();

    if (withShift) {
      // Multi-select mode (Ctrl+Shift+Click)
      this.state.mode = 'multi';
      
      if (this.state.selectedFields.has(fieldId)) {
        // Deselect if already selected
        this.state.selectedFields.delete(fieldId);
        
        // Update primary field if we just deselected it
        if (this.state.primaryField === fieldId) {
          const remaining = Array.from(this.state.selectedFields);
          this.state.primaryField = remaining.length > 0 ? remaining[0] : null;
        }
      } else {
        // Add to selection
        this.state.selectedFields.add(fieldId);
        
        // Set as primary if no primary exists
        if (!this.state.primaryField) {
          this.state.primaryField = fieldId;
        }
      }
    } else {
      // Single select mode (Ctrl+Click)
      this.state.mode = 'single';
      this.state.primaryField = fieldId;
      this.state.selectedFields.clear();
      this.state.selectedFields.add(fieldId);
    }

    this.notifyStateChange();
    return true;
  }

  /**
   * Clear all selections
   */
  clearSelection(): void {
    this.state.primaryField = null;
    this.state.selectedFields.clear();
    this.state.mode = 'single';
    this.state.lastInteraction = Date.now();
    this.notifyStateChange();
  }

  /**
   * Select multiple fields by IDs
   */
  selectMultiple(fieldIds: string[]): boolean {
    const validFieldIds = fieldIds.filter(id => this.fields.has(id));
    
    if (validFieldIds.length === 0) {
      return false;
    }

    this.state.mode = validFieldIds.length > 1 ? 'multi' : 'single';
    this.state.selectedFields = new Set(validFieldIds);
    this.state.primaryField = validFieldIds[0];
    this.state.lastInteraction = Date.now();
    
    this.notifyStateChange();
    return true;
  }

  /**
   * Get current selection state
   */
  getState(): Readonly<SelectionState> {
    return { ...this.state, selectedFields: new Set(this.state.selectedFields) };
  }

  /**
   * Check if field is selected
   */
  isSelected(fieldId: string): boolean {
    return this.state.selectedFields.has(fieldId);
  }

  /**
   * Check if field is the primary selection
   */
  isPrimary(fieldId: string): boolean {
    return this.state.primaryField === fieldId;
  }

  /**
   * Get array of selected field IDs
   */
  getSelectedIds(): string[] {
    return Array.from(this.state.selectedFields);
  }

  /**
   * Get array of selected field objects
   */
  getSelectedFields(): Field[] {
    return Array.from(this.state.selectedFields)
      .map(id => this.fields.get(id))
      .filter((field): field is Field => field !== undefined);
  }

  /**
   * Get primary selected field object
   */
  getPrimaryField(): Field | null {
    return this.state.primaryField ? this.fields.get(this.state.primaryField) || null : null;
  }

  /**
   * Check if there are any selections
   */
  hasSelection(): boolean {
    return this.state.selectedFields.size > 0;
  }

  /**
   * Check if multiple fields are selected
   */
  hasMultipleSelection(): boolean {
    return this.state.selectedFields.size > 1;
  }

  /**
   * Get bounding box of all selected fields
   */
  getSelectionBounds(): SelectionBounds | null {
    const selectedFields = this.getSelectedFields();
    
    if (selectedFields.length === 0) {
      return null;
    }

    let minX = Infinity;
    let maxX = -Infinity;
    let minY = Infinity;
    let maxY = -Infinity;

    selectedFields.forEach(field => {
      minX = Math.min(minX, field.x);
      maxX = Math.max(maxX, field.x + field.width - 1);
      minY = Math.min(minY, field.y);
      maxY = Math.max(maxY, field.y + field.height - 1);
    });

    return {
      minX,
      maxX,
      minY,
      maxY,
      width: maxX - minX + 1,
      height: maxY - minY + 1
    };
  }

  /**
   * Check if selection can move in a given direction without going out of bounds
   */
  canMove(deltaX: number, deltaY: number, gridCols: number = 80, gridRows: number = 24): boolean {
    const selectedFields = this.getSelectedFields();
    
    if (selectedFields.length === 0) {
      return false;
    }

    // Check bounds for all selected fields
    return selectedFields.every(field => {
      const newX = field.x + deltaX;
      const newY = field.y + deltaY;
      
      return newX >= 0 && 
             newX + field.width <= gridCols && 
             newY >= 0 && 
             newY + field.height <= gridRows;
    });
  }

  /**
   * Get movement preview positions for selected fields
   */
  getMovementPreview(deltaX: number, deltaY: number): Array<{ fieldId: string; newX: number; newY: number }> {
    const selectedFields = this.getSelectedFields();
    
    return selectedFields.map(field => ({
      fieldId: field.id,
      newX: field.x + deltaX,
      newY: field.y + deltaY
    }));
  }

  /**
   * Check if recent interaction occurred (for conflict resolution)
   */
  hasRecentInteraction(thresholdMs: number = 200): boolean {
    return Date.now() - this.state.lastInteraction < thresholdMs;
  }

  private notifyStateChange(): void {
    this.onStateChange(this.getState());
  }
}

export default FieldSelectionManager;
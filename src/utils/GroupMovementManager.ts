/**
 * GroupMovementManager - Coordinated movement for multiple fields
 * 
 * Handles group movement operations with collision detection and constraints
 * Provides advanced movement features like alignment and distribution
 */

import { Field } from '../components/ASPMapEditor';
import { FieldSelectionManager, SelectionBounds } from './FieldSelectionManager';

export interface GroupMovementConfig {
  /** Grid constraints */
  gridCols: number;
  gridRows: number;
  /** Enable collision detection */
  enableCollisionDetection: boolean;
  /** Enable smart alignment during movement */
  enableSmartAlignment: boolean;
  /** Snap to grid during movement */
  snapToGrid: boolean;
  /** Enable debug logging */
  debug: boolean;
}

export interface MovementConstraints {
  /** Minimum allowed position */
  minPosition: { x: number; y: number };
  /** Maximum allowed position */
  maxPosition: { x: number; y: number };
  /** Fields to avoid (collision detection) */
  avoidFields: Set<string>;
  /** Force movement within bounds */
  forceBounds: boolean;
}

export interface GroupMovementResult {
  /** Whether movement was successful */
  success: boolean;
  /** Fields that were moved with their new positions */
  movedFields: Array<{
    fieldId: string;
    oldPosition: { x: number; y: number };
    newPosition: { x: number; y: number };
  }>;
  /** Constraints that prevented movement */
  constraints?: Array<{
    type: 'boundary' | 'collision' | 'custom';
    description: string;
    affectedFields: string[];
  }>;
  /** Alternative suggestions */
  suggestions?: string[];
}

export interface AlignmentOptions {
  /** Alignment type */
  type: 'left' | 'center' | 'right' | 'top' | 'middle' | 'bottom';
  /** Reference field ID for alignment (if not provided, uses selection bounds) */
  referenceFieldId?: string;
}

export interface DistributionOptions {
  /** Distribution axis */
  axis: 'horizontal' | 'vertical';
  /** Distribution type */
  type: 'space' | 'centers' | 'edges';
  /** Spacing between fields (for 'space' type) */
  spacing?: number;
}

export class GroupMovementManager {
  private config: GroupMovementConfig;
  private selectionManager: FieldSelectionManager;
  private fields: Map<string, Field>;
  private onFieldsUpdate: (updatedFields: Map<string, Field>) => void;
  private onStatusMessage: (message: string) => void;

  constructor(
    config: GroupMovementConfig,
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
  updateConfig(config: Partial<GroupMovementConfig>): void {
    this.config = { ...this.config, ...config };
  }

  /**
   * Move group of fields by delta
   */
  moveGroup(deltaX: number, deltaY: number, constraints?: Partial<MovementConstraints>): GroupMovementResult {
    const selectedFields = this.selectionManager.getSelectedFields();
    
    if (selectedFields.length === 0) {
      return {
        success: false,
        movedFields: [],
        suggestions: ['Select fields first using Ctrl+Click']
      };
    }

    // Build full constraints
    const fullConstraints = this.buildConstraints(constraints);
    
    // Calculate new positions
    const proposedMoves = selectedFields.map(field => ({
      fieldId: field.id,
      oldPosition: { x: field.x, y: field.y },
      newPosition: { 
        x: this.config.snapToGrid ? Math.round(field.x + deltaX) : field.x + deltaX,
        y: this.config.snapToGrid ? Math.round(field.y + deltaY) : field.y + deltaY
      }
    }));

    // Validate movement
    const validationResult = this.validateGroupMovement(proposedMoves, fullConstraints);
    
    if (!validationResult.success) {
      return {
        success: false,
        movedFields: [],
        constraints: validationResult.constraints,
        suggestions: this.generateMovementSuggestions(validationResult.constraints || [])
      };
    }

    // Apply movement
    return this.applyGroupMovement(proposedMoves);
  }

  /**
   * Align selected fields
   */
  alignFields(options: AlignmentOptions): GroupMovementResult {
    const selectedFields = this.selectionManager.getSelectedFields();
    
    if (selectedFields.length < 2) {
      return {
        success: false,
        movedFields: [],
        suggestions: ['Select at least 2 fields to align']
      };
    }

    const alignmentMoves = this.calculateAlignmentMoves(selectedFields, options);
    
    // Validate and apply movement
    const fullConstraints = this.buildConstraints();
    const validationResult = this.validateGroupMovement(alignmentMoves, fullConstraints);
    
    if (!validationResult.success) {
      return {
        success: false,
        movedFields: [],
        constraints: validationResult.constraints,
        suggestions: ['Cannot align fields due to constraints']
      };
    }

    const result = this.applyGroupMovement(alignmentMoves);
    
    if (result.success) {
      this.onStatusMessage(`Aligned ${result.movedFields.length} fields ${options.type}`);
    }
    
    return result;
  }

  /**
   * Distribute selected fields
   */
  distributeFields(options: DistributionOptions): GroupMovementResult {
    const selectedFields = this.selectionManager.getSelectedFields();
    
    if (selectedFields.length < 3) {
      return {
        success: false,
        movedFields: [],
        suggestions: ['Select at least 3 fields to distribute']
      };
    }

    const distributionMoves = this.calculateDistributionMoves(selectedFields, options);
    
    // Validate and apply movement
    const fullConstraints = this.buildConstraints();
    const validationResult = this.validateGroupMovement(distributionMoves, fullConstraints);
    
    if (!validationResult.success) {
      return {
        success: false,
        movedFields: [],
        constraints: validationResult.constraints,
        suggestions: ['Cannot distribute fields due to constraints']
      };
    }

    const result = this.applyGroupMovement(distributionMoves);
    
    if (result.success) {
      this.onStatusMessage(`Distributed ${result.movedFields.length} fields ${options.axis}ly`);
    }
    
    return result;
  }

  /**
   * Build movement constraints
   */
  private buildConstraints(partial?: Partial<MovementConstraints>): MovementConstraints {
    const selectedIds = new Set(this.selectionManager.getSelectedIds());
    const avoidFields = new Set<string>();
    
    // Add all non-selected fields to avoid list
    Array.from(this.fields.entries()).forEach(([fieldId, field]) => {
      if (!selectedIds.has(fieldId)) {
        avoidFields.add(fieldId);
      }
    });

    return {
      minPosition: { x: 0, y: 0 },
      maxPosition: { x: this.config.gridCols - 1, y: this.config.gridRows - 1 },
      avoidFields,
      forceBounds: true,
      ...partial
    };
  }

  /**
   * Validate group movement
   */
  private validateGroupMovement(
    proposedMoves: Array<{ fieldId: string; oldPosition: { x: number; y: number }; newPosition: { x: number; y: number } }>,
    constraints: MovementConstraints
  ): { success: boolean; constraints?: Array<{ type: 'boundary' | 'collision' | 'custom'; description: string; affectedFields: string[] }> } {
    const violations: Array<{ type: 'boundary' | 'collision' | 'custom'; description: string; affectedFields: string[] }> = [];

    for (const move of proposedMoves) {
      const field = this.fields.get(move.fieldId);
      if (!field) continue;

      // Check boundary constraints
      if (constraints.forceBounds) {
        const fieldRight = move.newPosition.x + field.width - 1;
        const fieldBottom = move.newPosition.y + field.height - 1;

        if (move.newPosition.x < constraints.minPosition.x ||
            move.newPosition.y < constraints.minPosition.y ||
            fieldRight > constraints.maxPosition.x ||
            fieldBottom > constraints.maxPosition.y) {
          violations.push({
            type: 'boundary',
            description: `Field ${move.fieldId} would exceed grid boundaries`,
            affectedFields: [move.fieldId]
          });
        }
      }

      // Check collision constraints
      if (this.config.enableCollisionDetection) {
        const collisions = this.checkFieldCollisions(move.fieldId, move.newPosition, constraints.avoidFields);
        if (collisions.length > 0) {
          violations.push({
            type: 'collision',
            description: `Field ${move.fieldId} would collide with ${collisions.join(', ')}`,
            affectedFields: [move.fieldId, ...collisions]
          });
        }
      }
    }

    return {
      success: violations.length === 0,
      constraints: violations.length > 0 ? violations : undefined
    };
  }

  /**
   * Check for field collisions at new position
   */
  private checkFieldCollisions(fieldId: string, newPosition: { x: number; y: number }, avoidFields: Set<string>): string[] {
    const field = this.fields.get(fieldId);
    if (!field) return [];

    const fieldBounds = {
      left: newPosition.x,
      right: newPosition.x + field.width - 1,
      top: newPosition.y,
      bottom: newPosition.y + field.height - 1
    };

    const collisions: string[] = [];

    Array.from(avoidFields).forEach(avoidFieldId => {
      const avoidField = this.fields.get(avoidFieldId);
      if (!avoidField) return;

      const avoidBounds = {
        left: avoidField.x,
        right: avoidField.x + avoidField.width - 1,
        top: avoidField.y,
        bottom: avoidField.y + avoidField.height - 1
      };

      if (this.boundsOverlap(fieldBounds, avoidBounds)) {
        collisions.push(avoidFieldId);
      }
    });

    return collisions;
  }

  /**
   * Check if two rectangular bounds overlap
   */
  private boundsOverlap(
    bounds1: { left: number; right: number; top: number; bottom: number },
    bounds2: { left: number; right: number; top: number; bottom: number }
  ): boolean {
    return !(bounds1.right < bounds2.left ||
             bounds2.right < bounds1.left ||
             bounds1.bottom < bounds2.top ||
             bounds2.bottom < bounds1.top);
  }

  /**
   * Apply group movement
   */
  private applyGroupMovement(
    moves: Array<{ fieldId: string; oldPosition: { x: number; y: number }; newPosition: { x: number; y: number } }>
  ): GroupMovementResult {
    const newFields = new Map(this.fields);
    const movedFields: GroupMovementResult['movedFields'] = [];

    for (const move of moves) {
      const field = this.fields.get(move.fieldId);
      if (!field) continue;

      // Only update if position actually changed
      if (field.x !== move.newPosition.x || field.y !== move.newPosition.y) {
        const updatedField = { ...field, x: move.newPosition.x, y: move.newPosition.y };
        newFields.set(move.fieldId, updatedField);
        movedFields.push(move);
      }
    }

    if (movedFields.length > 0) {
      this.fields = newFields;
      this.onFieldsUpdate(newFields);

      if (this.config.debug) {
        console.log('GroupMovementManager: Applied movement', movedFields);
      }
    }

    return {
      success: true,
      movedFields
    };
  }

  /**
   * Calculate alignment moves
   */
  private calculateAlignmentMoves(
    fields: Field[],
    options: AlignmentOptions
  ): Array<{ fieldId: string; oldPosition: { x: number; y: number }; newPosition: { x: number; y: number } }> {
    let referenceValue: number;

    // Determine reference value
    if (options.referenceFieldId) {
      const referenceField = this.fields.get(options.referenceFieldId);
      if (!referenceField) {
        throw new Error(`Reference field ${options.referenceFieldId} not found`);
      }
      referenceValue = this.getFieldAlignmentValue(referenceField, options.type);
    } else {
      // Use bounds of selected fields
      const bounds = this.getFieldsBounds(fields);
      referenceValue = this.getBoundsAlignmentValue(bounds, options.type);
    }

    // Calculate moves
    return fields.map(field => {
      const currentValue = this.getFieldAlignmentValue(field, options.type);
      const delta = referenceValue - currentValue;
      
      let newX = field.x;
      let newY = field.y;

      if (['left', 'center', 'right'].includes(options.type)) {
        newX = field.x + delta;
      } else {
        newY = field.y + delta;
      }

      return {
        fieldId: field.id,
        oldPosition: { x: field.x, y: field.y },
        newPosition: { x: newX, y: newY }
      };
    });
  }

  /**
   * Calculate distribution moves
   */
  private calculateDistributionMoves(
    fields: Field[],
    options: DistributionOptions
  ): Array<{ fieldId: string; oldPosition: { x: number; y: number }; newPosition: { x: number; y: number } }> {
    // Sort fields by position
    const sortedFields = [...fields].sort((a, b) => {
      return options.axis === 'horizontal' ? a.x - b.x : a.y - b.y;
    });

    const moves: Array<{ fieldId: string; oldPosition: { x: number; y: number }; newPosition: { x: number; y: number } }> = [];

    if (options.type === 'space' && options.spacing !== undefined) {
      // Distribute with fixed spacing
      let currentPos = options.axis === 'horizontal' ? sortedFields[0].x : sortedFields[0].y;
      
      for (let i = 0; i < sortedFields.length; i++) {
        const field = sortedFields[i];
        let newX = field.x;
        let newY = field.y;

        if (options.axis === 'horizontal') {
          newX = currentPos;
          currentPos += field.width + options.spacing;
        } else {
          newY = currentPos;
          currentPos += field.height + options.spacing;
        }

        moves.push({
          fieldId: field.id,
          oldPosition: { x: field.x, y: field.y },
          newPosition: { x: newX, y: newY }
        });
      }
    } else {
      // Distribute evenly between first and last
      const firstField = sortedFields[0];
      const lastField = sortedFields[sortedFields.length - 1];
      
      const startPos = options.axis === 'horizontal' ? firstField.x : firstField.y;
      const endPos = options.axis === 'horizontal' ? lastField.x : lastField.y;
      const totalSpace = endPos - startPos;
      const spacing = totalSpace / (sortedFields.length - 1);

      for (let i = 1; i < sortedFields.length - 1; i++) {
        const field = sortedFields[i];
        const targetPos = startPos + (spacing * i);
        
        let newX = field.x;
        let newY = field.y;

        if (options.axis === 'horizontal') {
          newX = targetPos;
        } else {
          newY = targetPos;
        }

        moves.push({
          fieldId: field.id,
          oldPosition: { x: field.x, y: field.y },
          newPosition: { x: newX, y: newY }
        });
      }
    }

    return moves;
  }

  /**
   * Get field alignment value
   */
  private getFieldAlignmentValue(field: Field, alignmentType: string): number {
    switch (alignmentType) {
      case 'left': return field.x;
      case 'center': return field.x + field.width / 2;
      case 'right': return field.x + field.width;
      case 'top': return field.y;
      case 'middle': return field.y + field.height / 2;
      case 'bottom': return field.y + field.height;
      default: return field.x;
    }
  }

  /**
   * Get bounds alignment value
   */
  private getBoundsAlignmentValue(bounds: { minX: number; maxX: number; minY: number; maxY: number }, alignmentType: string): number {
    switch (alignmentType) {
      case 'left': return bounds.minX;
      case 'center': return (bounds.minX + bounds.maxX) / 2;
      case 'right': return bounds.maxX;
      case 'top': return bounds.minY;
      case 'middle': return (bounds.minY + bounds.maxY) / 2;
      case 'bottom': return bounds.maxY;
      default: return bounds.minX;
    }
  }

  /**
   * Get bounds of multiple fields
   */
  private getFieldsBounds(fields: Field[]): { minX: number; maxX: number; minY: number; maxY: number } {
    let minX = Infinity, maxX = -Infinity, minY = Infinity, maxY = -Infinity;

    for (const field of fields) {
      minX = Math.min(minX, field.x);
      maxX = Math.max(maxX, field.x + field.width);
      minY = Math.min(minY, field.y);
      maxY = Math.max(maxY, field.y + field.height);
    }

    return { minX, maxX, minY, maxY };
  }

  /**
   * Generate movement suggestions based on constraints
   */
  private generateMovementSuggestions(constraints: Array<{ type: string; description: string }>): string[] {
    const suggestions: string[] = [];
    
    const hasBoundaryViolations = constraints.some(c => c.type === 'boundary');
    const hasCollisionViolations = constraints.some(c => c.type === 'collision');

    if (hasBoundaryViolations) {
      suggestions.push('Try moving in a different direction');
      suggestions.push('Select fewer fields to move');
    }

    if (hasCollisionViolations) {
      suggestions.push('Clear the target area first');
      suggestions.push('Move the blocking fields out of the way');
    }

    if (suggestions.length === 0) {
      suggestions.push('Check field positions and try again');
    }

    return suggestions;
  }

  /**
   * Get available group operations
   */
  getAvailableOperations(): Array<{ operation: string; description: string; requiresMultiple: boolean }> {
    return [
      { operation: 'move', description: 'Move selected fields', requiresMultiple: false },
      { operation: 'align-left', description: 'Align fields to left edge', requiresMultiple: true },
      { operation: 'align-center', description: 'Align fields to center', requiresMultiple: true },
      { operation: 'align-right', description: 'Align fields to right edge', requiresMultiple: true },
      { operation: 'align-top', description: 'Align fields to top edge', requiresMultiple: true },
      { operation: 'align-middle', description: 'Align fields to middle', requiresMultiple: true },
      { operation: 'align-bottom', description: 'Align fields to bottom edge', requiresMultiple: true },
      { operation: 'distribute-horizontal', description: 'Distribute fields horizontally', requiresMultiple: true },
      { operation: 'distribute-vertical', description: 'Distribute fields vertically', requiresMultiple: true }
    ];
  }
}

export default GroupMovementManager;
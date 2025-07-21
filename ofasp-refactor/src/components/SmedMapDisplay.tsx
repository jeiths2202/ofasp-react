import React, { useState, useEffect, useRef } from 'react';
import './SmedMapDisplay.css';

interface SmedField {
  name: string;
  row: number;
  col: number;
  length: number;
  value?: string;
  prompt?: string;
  type?: string;
}

interface SmedMapDisplayProps {
  fields: SmedField[];
  onSubmit?: (fieldValues: Record<string, string>) => void;
  mapName?: string;
}

const SmedMapDisplay: React.FC<SmedMapDisplayProps> = ({ fields, onSubmit, mapName }) => {
  const [grid, setGrid] = useState<string[][]>([]);
  const [fieldValues, setFieldValues] = useState<Record<string, string>>({});
  const [focusedField, setFocusedField] = useState<string | null>(null);
  const [cursorPosition, setCursorPosition] = useState({ row: 0, col: 0 });
  const gridRef = useRef<HTMLDivElement>(null);

  // Initialize 24x80 grid
  useEffect(() => {
    const newGrid: string[][] = Array(24).fill(null).map(() => Array(80).fill(' '));
    
    // Place fields on the grid
    fields.forEach(field => {
      const row = field.row - 1; // Convert to 0-based index
      const col = field.col - 1;
      
      // Place prompt if it exists (output field)
      if (field.prompt) {
        for (let i = 0; i < field.prompt.length && col + i < 80; i++) {
          newGrid[row][col + i] = field.prompt[i];
        }
      }
      
      // Initialize field value
      if (!field.prompt) {
        // Input field
        const value = fieldValues[field.name] || field.value || '';
        const paddedValue = value.padEnd(field.length, '_');
        for (let i = 0; i < field.length && col + i < 80; i++) {
          newGrid[row][col + i] = paddedValue[i];
        }
      }
    });
    
    setGrid(newGrid);
  }, [fields, fieldValues]);

  // Handle keyboard input
  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (!focusedField) return;
    
    const field = fields.find(f => f.name === focusedField);
    if (!field || field.prompt) return; // Only handle input fields
    
    const currentValue = fieldValues[focusedField] || '';
    
    switch (e.key) {
      case 'Enter':
        if (onSubmit) {
          onSubmit(fieldValues);
        }
        break;
        
      case 'Tab':
        e.preventDefault();
        // Find next input field
        const inputFields = fields.filter(f => !f.prompt).sort((a, b) => {
          if (a.row !== b.row) return a.row - b.row;
          return a.col - b.col;
        });
        const currentIndex = inputFields.findIndex(f => f.name === focusedField);
        const nextField = inputFields[(currentIndex + 1) % inputFields.length];
        if (nextField) {
          setFocusedField(nextField.name);
          setCursorPosition({ row: nextField.row - 1, col: nextField.col - 1 });
        }
        break;
        
      case 'Backspace':
        if (currentValue.length > 0) {
          setFieldValues({
            ...fieldValues,
            [focusedField]: currentValue.slice(0, -1)
          });
        }
        break;
        
      case 'ArrowLeft':
      case 'ArrowRight':
      case 'ArrowUp':
      case 'ArrowDown':
        // Handle cursor movement within field bounds
        break;
        
      default:
        // Handle character input
        if (e.key.length === 1 && currentValue.length < field.length) {
          setFieldValues({
            ...fieldValues,
            [focusedField]: currentValue + e.key
          });
        }
    }
  };

  // Handle click on field
  const handleCellClick = (row: number, col: number) => {
    // Find which field was clicked
    const clickedField = fields.find(field => {
      const fieldRow = field.row - 1;
      const fieldCol = field.col - 1;
      return !field.prompt && // Only input fields are clickable
             row === fieldRow && 
             col >= fieldCol && 
             col < fieldCol + field.length;
    });
    
    if (clickedField) {
      setFocusedField(clickedField.name);
      setCursorPosition({ row, col });
    }
  };

  // Render character with appropriate styling
  const renderChar = (char: string, row: number, col: number) => {
    // Check if this position is part of a field
    const field = fields.find(f => {
      const fieldRow = f.row - 1;
      const fieldCol = f.col - 1;
      return row === fieldRow && col >= fieldCol && col < fieldCol + f.length;
    });
    
    const isInputField = field && !field.prompt;
    const isFocused = field && field.name === focusedField;
    const isCursor = isFocused && row === cursorPosition.row && col === cursorPosition.col;
    
    let className = 'grid-char';
    if (isInputField) {
      className += ' input-field';
    }
    if (isFocused) {
      className += ' focused-field';
    }
    if (isCursor) {
      className += ' cursor';
    }
    
    return (
      <span
        key={`${row}-${col}`}
        className={className}
        onClick={() => handleCellClick(row, col)}
      >
        {char}
      </span>
    );
  };

  return (
    <div className="smed-map-display" onKeyDown={handleKeyDown} tabIndex={0}>
      {mapName && <div className="map-header">Map: {mapName}</div>}
      <div className="terminal-grid" ref={gridRef}>
        {grid.map((row, rowIndex) => (
          <div key={rowIndex} className="grid-row">
            {row.map((char, colIndex) => renderChar(char, rowIndex, colIndex))}
          </div>
        ))}
      </div>
      <div className="status-bar">
        Row: {cursorPosition.row + 1} Col: {cursorPosition.col + 1} | 
        {focusedField ? ` Field: ${focusedField}` : ' Ready'}
      </div>
    </div>
  );
};

export default SmedMapDisplay;
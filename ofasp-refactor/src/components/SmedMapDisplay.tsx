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
  onClose?: () => void;
  onKeyEvent?: (key: string, fieldValues: Record<string, string>) => Promise<any>;
  mapName?: string;
}

const SmedMapDisplay: React.FC<SmedMapDisplayProps> = ({ fields, onSubmit, onClose, onKeyEvent, mapName }) => {
  const [grid, setGrid] = useState<string[][]>([]);
  const [fieldValues, setFieldValues] = useState<Record<string, string>>({});
  const [focusedField, setFocusedField] = useState<string | null>(null);
  const [cursorPosition, setCursorPosition] = useState({ row: 0, col: 0 });
  const gridRef = useRef<HTMLDivElement>(null);

  // Helper function to check if character is full-width (Japanese/Chinese)
  const isFullWidth = (char: string): boolean => {
    if (!char) return false;
    const code = char.charCodeAt(0);
    return (
      (code >= 0x1100 && code <= 0x11FF) || // Hangul Jamo
      (code >= 0x2E80 && code <= 0x2FDF) || // CJK Radicals
      (code >= 0x3000 && code <= 0x303F) || // CJK Symbols and Punctuation
      (code >= 0x3040 && code <= 0x309F) || // Hiragana
      (code >= 0x30A0 && code <= 0x30FF) || // Katakana
      (code >= 0x3100 && code <= 0x31BF) || // Bopomofo
      (code >= 0x3200 && code <= 0x32FF) || // Enclosed CJK
      (code >= 0x3300 && code <= 0x33FF) || // CJK Compatibility
      (code >= 0x3400 && code <= 0x4DBF) || // CJK Extension A
      (code >= 0x4E00 && code <= 0x9FFF) || // CJK Unified Ideographs
      (code >= 0xA000 && code <= 0xA48F) || // Yi Syllables
      (code >= 0xAC00 && code <= 0xD7AF) || // Hangul Syllables
      (code >= 0xF900 && code <= 0xFAFF) || // CJK Compatibility Ideographs
      (code >= 0xFE30 && code <= 0xFE4F) || // CJK Compatibility Forms
      (code >= 0xFF00 && code <= 0xFF60) || // Fullwidth Forms
      (code >= 0xFFE0 && code <= 0xFFEF)    // Halfwidth Forms
    );
  };

  // Initialize 24x80 grid with proper full-width character handling
  useEffect(() => {
    const newGrid: string[][] = Array(24).fill(null).map(() => Array(80).fill(' '));
    
    // Place fields on the grid
    fields.forEach(field => {
      const row = field.row - 1; // Convert to 0-based index
      let col = field.col - 1;
      
      // Place prompt if it exists (output field)
      if (field.prompt) {
        let gridCol = col;
        for (let i = 0; i < field.prompt.length && gridCol < 80; i++) {
          const char = field.prompt[i];
          newGrid[row][gridCol] = char;
          
          // If this is a full-width character, skip the next column
          if (isFullWidth(char)) {
            gridCol += 2; // Full-width characters take 2 columns
            if (gridCol <= 80) {
              newGrid[row][gridCol - 1] = ''; // Mark the second half as empty
            }
          } else {
            gridCol += 1; // Half-width characters take 1 column
          }
        }
      }
      
      // Initialize field value
      if (!field.prompt) {
        // Input field
        const value = fieldValues[field.name] || field.value || '';
        const paddedValue = value.padEnd(field.length, '_');
        let gridCol = col;
        for (let i = 0; i < field.length && gridCol < 80; i++) {
          const char = paddedValue[i] || '_';
          newGrid[row][gridCol] = char;
          
          // If this is a full-width character, skip the next column
          if (isFullWidth(char)) {
            gridCol += 2;
            if (gridCol <= 80) {
              newGrid[row][gridCol - 1] = '';
            }
          } else {
            gridCol += 1;
          }
        }
      }
    });
    
    setGrid(newGrid);
  }, [fields, fieldValues]);

  // Handle keyboard input
  const handleKeyDown = async (e: React.KeyboardEvent) => {
    // Handle function keys (F1-F12)
    if (e.key.startsWith('F') && e.key.length >= 2) {
      e.preventDefault();
      
      // If onKeyEvent is provided, send the key event to the application
      if (onKeyEvent) {
        try {
          const response = await onKeyEvent(e.key, fieldValues);
          
          // Handle response from application
          if (response && response.action) {
            switch (response.action) {
              case 'close':
                if (onClose) onClose();
                break;
              case 'submit':
                if (onSubmit) onSubmit(fieldValues);
                break;
              case 'update_fields':
                if (response.fieldValues) {
                  setFieldValues(response.fieldValues);
                }
                break;
              // Add more actions as needed
            }
          }
        } catch (error) {
          console.error('Error handling key event:', error);
        }
      } else {
        // Fallback behavior if no onKeyEvent handler
        if (e.key === 'F3' && onClose) {
          onClose();
        }
      }
      return;
    }
    
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
    // Skip empty cells (used for second half of full-width characters)
    if (char === '') {
      return null;
    }
    
    // Check if this position is part of a field
    const field = fields.find(f => {
      const fieldRow = f.row - 1;
      const fieldCol = f.col - 1;
      return row === fieldRow && col >= fieldCol && col < fieldCol + f.length;
    });
    
    const isInputField = field && !field.prompt;
    const isFocused = field && field.name === focusedField;
    const isCursor = isFocused && row === cursorPosition.row && col === cursorPosition.col;
    const isFullWidthChar = isFullWidth(char);
    
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
    if (isFullWidthChar) {
      className += ' full-width';
    }
    
    const cellStyle = isFullWidthChar ? {
      width: '1.2em', // Double width for full-width characters
      textAlign: 'left' as const
    } : {};
    
    return (
      <span
        key={`${row}-${col}`}
        className={className}
        style={cellStyle}
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
        {focusedField ? ` Field: ${focusedField}` : ' Ready'} | 
        F3=Exit, Enter=Submit
      </div>
    </div>
  );
};

export default SmedMapDisplay;
/**
 * ASPMapEditorEnhanced - Enhanced version with advanced item manipulation features
 * 
 * This is the enhanced version of ASPMapEditor with the following advanced features:
 * 1. Single Item Movement: Items move only with Ctrl + Mouse Click + Drag or Ctrl + Arrow Keys
 * 2. Multi-Item Group Movement: Shift + Mouse Click to select multiple items, group moves together
 * 3. Advanced visual feedback with selection states and collision detection
 * 4. Coordinated movement with snap-to-grid and alignment features
 */

import React, { useState, useEffect, useRef, useCallback, useMemo } from 'react';
import { 
  CloudArrowUpIcon, 
  CloudArrowDownIcon, 
  EyeIcon, 
  TrashIcon,
  DocumentTextIcon,
  PlayIcon,
  PlusIcon
} from '@heroicons/react/24/outline';
import { useI18n } from '../hooks/useI18n';
import FieldSelectionManager, { SelectionState } from '../utils/FieldSelectionManager';
import KeyboardInteractionHandler, { KeyboardConfig } from '../utils/KeyboardInteractionHandler';
import MouseInteractionHandler, { MouseConfig } from '../utils/MouseInteractionHandler';
import GroupMovementManager, { GroupMovementConfig } from '../utils/GroupMovementManager';
import AdvancedFieldRenderer from './AdvancedFieldRenderer';

export interface Field {
  id: string;
  name: string;
  type: 'text' | 'input' | 'output' | 'button';
  x: number;
  y: number;
  width: number;
  height: number;
  value: string;
  cssClass: string;
  attributes: string;
  backgroundColor: string;
  textColor: string;
}

interface MapData {
  version: string;
  screenSize: { cols: number; rows: number };
  created: string;
  fieldCount: number;
  fields: Field[];
}

interface ASPMapEditorEnhancedProps {
  isDarkMode: boolean;
}

const ASPMapEditorEnhanced: React.FC<ASPMapEditorEnhancedProps> = ({ isDarkMode }) => {
  const { t } = useI18n();
  const [fields, setFields] = useState<Map<string, Field>>(new Map());
  
  // Enhanced selection state - replacing basic selection state
  const [selectionState, setSelectionState] = useState<SelectionState>({
    primaryField: null,
    selectedFields: new Set(),
    mode: 'single',
    lastInteraction: Date.now()
  });
  
  const [fieldCounter, setFieldCounter] = useState(0);
  const [draggedFieldType, setDraggedFieldType] = useState<string | null>(null);
  const [isPreviewOpen, setIsPreviewOpen] = useState(false);
  const [mapDataOutput, setMapDataOutput] = useState('');
  const [smedFiles, setSmedFiles] = useState<string[]>([]);
  const [catalogMaps, setCatalogMaps] = useState<any[]>([]);
  const [volumes, setVolumes] = useState<string[]>([]);
  const [libraries, setLibraries] = useState<string[]>([]);
  const [selectedVolume, setSelectedVolume] = useState<string>('');
  const [selectedLibrary, setSelectedLibrary] = useState<string>('');
  const [currentFileName, setCurrentFileName] = useState<string>('');
  const [showSaveDialog, setShowSaveDialog] = useState<boolean>(false);
  const [statusMessage, setStatusMessage] = useState(t('mapEditor.statusMessage'));
  
  // Floating panel states
  const [propertiesPanel, setPropertiesPanel] = useState({
    x: 20, y: 60, width: 260, height: 600, isDragging: false, isResizing: false
  });
  const [gridPanel, setGridPanel] = useState({
    x: 300, y: 60, width: 820, height: 500, isDragging: false, isResizing: false
  });
  const [dragOffset, setDragOffset] = useState({ x: 0, y: 0 });
  const [resizeHandle, setResizeHandle] = useState<string | null>(null);
  const [zoomLevel, setZoomLevel] = useState(1);
  
  const screenPanelRef = useRef<HTMLDivElement>(null);
  const [fieldProperties, setFieldProperties] = useState({
    id: '',
    name: '',
    width: 10,
    height: 1,
    value: '',
    cssClass: '',
    attributes: 'normal',
    backgroundColor: '#00ff00',
    textColor: '#ffffff'
  });

  const cols = 80;
  const rows = 24;
  
  // Calculate character size based on grid panel dimensions and zoom level
  const availableWidth = gridPanel.width - 40;
  const availableHeight = gridPanel.height - 80;
  
  const baseCharWidth = Math.max(6, availableWidth / cols);
  const baseCharHeight = Math.max(12, availableHeight / rows);
  
  const charWidth = baseCharWidth * zoomLevel;
  const charHeight = baseCharHeight * zoomLevel;

  // Advanced interaction managers
  const selectionManager = useMemo(() => {
    return new FieldSelectionManager(fields, setSelectionState);
  }, []);
  
  const keyboardHandler = useMemo(() => {
    const config: KeyboardConfig = {
      enableArrowMovement: true,
      movementStep: 1,
      gridCols: cols,
      gridRows: rows,
      debug: false
    };
    return new KeyboardInteractionHandler(
      config,
      selectionManager,
      fields,
      (updatedFields) => setFields(updatedFields),
      setStatusMessage
    );
  }, [selectionManager]);
  
  const mouseHandler = useMemo(() => {
    const config: MouseConfig = {
      enableCtrlClick: true,
      enableDragMovement: true,
      dragThreshold: 5,
      gridCols: cols,
      gridRows: rows,
      charWidth,
      charHeight,
      debug: false
    };
    return new MouseInteractionHandler(
      config,
      selectionManager,
      fields,
      (updatedFields) => setFields(updatedFields),
      setStatusMessage,
      screenPanelRef
    );
  }, [selectionManager, charWidth, charHeight]);
  
  const groupMovementManager = useMemo(() => {
    const config: GroupMovementConfig = {
      gridCols: cols,
      gridRows: rows,
      enableCollisionDetection: true,
      enableSmartAlignment: true,
      snapToGrid: true,
      debug: false
    };
    return new GroupMovementManager(
      config,
      selectionManager,
      fields,
      (updatedFields) => setFields(updatedFields),
      setStatusMessage
    );
  }, [selectionManager]);

  // Zoom functions
  const handleZoomIn = () => {
    setZoomLevel(prev => Math.min(prev + 0.25, 2));
  };

  const handleZoomOut = () => {
    setZoomLevel(prev => Math.max(prev - 0.25, 0.5));
  };

  const handleZoomReset = () => {
    setZoomLevel(1);
  };

  useEffect(() => {
    updateMapDataOutput();
    
    // Update interaction managers when fields change
    selectionManager.updateFields(fields);
    keyboardHandler.updateFields(fields);
    mouseHandler.updateFields(fields);
    groupMovementManager.updateFields(fields);
  }, [fields, selectionManager, keyboardHandler, mouseHandler, groupMovementManager]);

  useEffect(() => {
    loadSmedFiles();
    loadCatalogMaps();
    loadVolumes();
    
    // Set up keyboard event listener for advanced interactions
    const handleGlobalKeyDown = (event: KeyboardEvent) => {
      keyboardHandler.handleKeyDown(event);
    };
    
    document.addEventListener('keydown', handleGlobalKeyDown);
    
    return () => {
      document.removeEventListener('keydown', handleGlobalKeyDown);
    };
  }, [keyboardHandler]);

  // Enhanced field interaction handlers using the new interaction managers
  const handleFieldMouseDown = (fieldId: string, event: React.MouseEvent) => {
    // Check if we're in the middle of a panel drag/resize operation
    if (propertiesPanel.isDragging || gridPanel.isDragging || gridPanel.isResizing) {
      return;
    }
    
    mouseHandler.handleFieldMouseDown(fieldId, event);
  };
  
  const handleFieldMouseUp = (fieldId: string, event: React.MouseEvent) => {
    mouseHandler.handleMouseUp(event);
  };
  
  const handleFieldClick = (fieldId: string, event: React.MouseEvent) => {
    // Check if we're in the middle of a panel drag/resize operation
    if (propertiesPanel.isDragging || gridPanel.isDragging || gridPanel.isResizing) {
      return;
    }
    
    // The MouseInteractionHandler will handle the actual selection logic
    // This is mainly for backward compatibility and additional UI updates
    if (selectionState.primaryField === fieldId) {
      const field = fields.get(fieldId);
      if (field) {
        updateFieldProperties(field);
      }
    }
  };

  // Grid mouse handlers for drag operations
  const handleGridMouseMove = (event: React.MouseEvent) => {
    mouseHandler.handleMouseMove(event);
  };
  
  const handleGridMouseUp = (event: React.MouseEvent) => {
    mouseHandler.handleMouseUp(event);
  };

  const deleteSelectedField = () => {
    if (selectionState.primaryField) {
      const newFields = new Map(fields);
      newFields.delete(selectionState.primaryField);
      setFields(newFields);
      selectionManager.clearSelection();
      setStatusMessage(t('mapEditor.fieldDeleted', { selectedField: selectionState.primaryField }));
    }
  };

  const clearAllFields = () => {
    setFields(new Map());
    selectionManager.clearSelection();
    setFieldCounter(0);
    setStatusMessage(t('mapEditor.allFieldsDeleted'));
  };

  const createField = (type: string, x: number, y: number) => {
    const newCounter = fieldCounter + 1;
    setFieldCounter(newCounter);
    
    const fieldId = `FIELD_${newCounter.toString().padStart(3, '0')}`;
    
    const newField: Field = {
      id: fieldId,
      name: `${type.toUpperCase()}_${newCounter}`,
      type: type as 'text' | 'input' | 'output' | 'button',
      x: Math.floor(x / charWidth),
      y: Math.floor(y / charHeight),
      width: 10,
      height: 1,
      value: getDefaultValue(type),
      cssClass: '',
      attributes: 'normal',
      backgroundColor: '#00ff00',
      textColor: '#ffffff'
    };

    setFields(new Map(fields.set(fieldId, newField)));
    selectionManager.selectMultiple([fieldId]);
    updateFieldProperties(newField);
    setStatusMessage(t('mapEditor.fieldAdded', { fieldId }));
  };

  const getDefaultValue = (type: string) => {
    switch (type) {
      case 'text': return 'Text Label';
      case 'input': return '_'.repeat(10);
      case 'output': return 'Output';
      case 'button': return 'Button';
      default: return 'Field';
    }
  };

  const updateFieldProperties = (field: Field) => {
    setFieldProperties({
      id: field.id,
      name: field.name,
      width: field.width,
      height: field.height,
      value: field.value,
      cssClass: field.cssClass,
      attributes: field.attributes,
      backgroundColor: field.backgroundColor,
      textColor: field.textColor
    });
  };

  const handleFieldPropertyChange = (property: string, value: any) => {
    setFieldProperties(prev => ({ ...prev, [property]: value }));
    
    if (selectionState.primaryField) {
      const field = fields.get(selectionState.primaryField);
      if (field) {
        const updatedField = { ...field, [property]: value };
        setFields(new Map(fields.set(selectionState.primaryField, updatedField)));
      }
    }
  };

  const updateMapDataOutput = () => {
    const mapData: MapData = {
      version: '1.0',
      screenSize: { cols, rows },
      created: new Date().toISOString(),
      fieldCount: fields.size,
      fields: Array.from(fields.values()).map(field => ({
        id: field.id,
        name: field.name,
        type: field.type,
        x: field.x,
        y: field.y,
        width: field.width,
        height: field.height,
        value: field.value,
        cssClass: field.cssClass,
        attributes: field.attributes,
        backgroundColor: field.backgroundColor,
        textColor: field.textColor
      }))
    };
    
    setMapDataOutput(JSON.stringify(mapData, null, 2));
  };

  // Placeholder functions for file operations (keeping existing functionality)
  const loadSmedFiles = async () => {
    try {
      const response = await fetch('http://localhost:8000/api/smed/files');
      if (response.ok) {
        const data = await response.json();
        setSmedFiles(data.files || []);
      }
    } catch (error) {
      console.error('Error loading SMED files:', error);
    }
  };

  const loadCatalogMaps = async () => {
    try {
      const response = await fetch('http://localhost:8000/api/catalog/maps');
      if (response.ok) {
        const data = await response.json();
        setCatalogMaps(data.maps || []);
      }
    } catch (error) {
      console.error('Error loading catalog maps:', error);
    }
  };

  const loadVolumes = async () => {
    try {
      const response = await fetch('http://localhost:8000/api/catalog/volumes');
      if (response.ok) {
        const data = await response.json();
        setVolumes(data.volumes || []);
        if (data.volumes && data.volumes.length > 0) {
          setSelectedVolume(data.volumes[0]);
          loadLibraries(data.volumes[0]);
        }
      }
    } catch (error) {
      console.error('Error loading volumes:', error);
    }
  };

  const loadLibraries = async (volume: string) => {
    try {
      const response = await fetch(`http://localhost:8000/api/catalog/libraries/${volume}`);
      if (response.ok) {
        const data = await response.json();
        setLibraries(data.libraries || []);
        if (data.libraries && data.libraries.length > 0) {
          setSelectedLibrary(data.libraries[0]);
        }
      }
    } catch (error) {
      console.error('Error loading libraries:', error);
    }
  };

  // Drag and drop handlers for field creation
  const handleDragStart = (e: React.DragEvent, fieldType: string) => {
    e.dataTransfer.setData('text/plain', fieldType);
    setDraggedFieldType(fieldType);
  };

  const handleDragOver = (e: React.DragEvent) => {
    e.preventDefault();
  };

  const handleDrop = (e: React.DragEvent) => {
    e.preventDefault();
    e.stopPropagation();
    const fieldType = e.dataTransfer.getData('text/plain');
    
    if (screenPanelRef.current) {
      const rect = screenPanelRef.current.getBoundingClientRect();
      const x = e.clientX - rect.left;
      const y = e.clientY - rect.top;
      
      createField(fieldType, x, y);
    }
    
    setDraggedFieldType(null);
  };

  const fieldTypes = [
    { type: 'text', label: `📝 ${t('mapEditor.textField')}`, icon: DocumentTextIcon },
    { type: 'input', label: `📥 ${t('mapEditor.inputField')}`, icon: PlusIcon },
    { type: 'output', label: `📤 ${t('mapEditor.outputField')}`, icon: DocumentTextIcon },
    { type: 'button', label: `🔘 ${t('mapEditor.buttonField')}`, icon: PlayIcon }
  ];

  // Floating panel drag and resize handlers (keeping existing functionality)
  const handlePanelMouseDown = useCallback((e: React.MouseEvent, panelType: 'properties' | 'grid', action: 'drag' | 'resize', handle?: string) => {
    e.preventDefault();
    e.stopPropagation();
    
    if (action === 'drag' || action === 'resize') {
      selectionManager.clearSelection();
    }
    
    if (action === 'drag') {
      const panel = panelType === 'properties' ? propertiesPanel : gridPanel;
      const offsetX = e.clientX - panel.x;
      const offsetY = e.clientY - panel.y;
      
      setDragOffset({ x: offsetX, y: offsetY });
      
      if (panelType === 'properties') {
        setPropertiesPanel(prev => ({ ...prev, isDragging: true }));
      } else {
        setGridPanel(prev => ({ ...prev, isDragging: true }));
      }
    } else if (action === 'resize') {
      setResizeHandle(handle || '');
      if (panelType === 'properties') {
        setPropertiesPanel(prev => ({ ...prev, isResizing: true }));
      } else {
        setGridPanel(prev => ({ ...prev, isResizing: true }));
      }
    }
    
    document.body.style.userSelect = 'none';
    document.body.style.cursor = action === 'drag' ? 'grabbing' : handle || 'nw-resize';
  }, [propertiesPanel, gridPanel, selectionManager]);

  const handleMouseMove = useCallback((e: MouseEvent) => {
    if (propertiesPanel.isDragging) {
      setPropertiesPanel(prev => ({
        ...prev,
        x: Math.max(0, Math.min(window.innerWidth - prev.width, e.clientX - dragOffset.x)),
        y: Math.max(0, Math.min(window.innerHeight - prev.height, e.clientY - dragOffset.y))
      }));
    }
    
    if (gridPanel.isDragging) {
      setGridPanel(prev => ({
        ...prev,
        x: Math.max(0, Math.min(window.innerWidth - prev.width, e.clientX - dragOffset.x)),
        y: Math.max(0, Math.min(window.innerHeight - prev.height, e.clientY - dragOffset.y))
      }));
    }
    
    if (gridPanel.isResizing && resizeHandle) {
      const panel = gridPanel;
      let newWidth = panel.width;
      let newHeight = panel.height;
      let newX = panel.x;
      let newY = panel.y;
      
      if (resizeHandle.includes('e')) {
        newWidth = Math.max(300, e.clientX - panel.x);
      }
      if (resizeHandle.includes('w')) {
        const right = panel.x + panel.width;
        newX = Math.min(e.clientX, right - 300);
        newWidth = right - newX;
      }
      if (resizeHandle.includes('s')) {
        newHeight = Math.max(200, e.clientY - panel.y);
      }
      if (resizeHandle.includes('n')) {
        const bottom = panel.y + panel.height;
        newY = Math.min(e.clientY, bottom - 200);
        newHeight = bottom - newY;
      }
      
      setGridPanel(prev => ({
        ...prev,
        x: newX,
        y: newY,
        width: newWidth,
        height: newHeight
      }));
    }
  }, [propertiesPanel.isDragging, gridPanel.isDragging, gridPanel.isResizing, dragOffset, resizeHandle]);

  const handleMouseUp = useCallback(() => {
    setPropertiesPanel(prev => ({ ...prev, isDragging: false, isResizing: false }));
    setGridPanel(prev => ({ ...prev, isDragging: false, isResizing: false }));
    setResizeHandle(null);
    
    document.body.style.userSelect = '';
    document.body.style.cursor = '';
    document.body.style.pointerEvents = '';
    
    void document.body.offsetHeight;
    
    document.removeEventListener('mousemove', handleMouseMove);
    document.removeEventListener('mouseup', handleMouseUp);
    
    setTimeout(() => {
      document.body.style.userSelect = '';
      document.body.style.cursor = '';
      document.body.style.pointerEvents = '';
    }, 50);
  }, [handleMouseMove]);

  useEffect(() => {
    if (propertiesPanel.isDragging || gridPanel.isDragging || gridPanel.isResizing) {
      document.addEventListener('mousemove', handleMouseMove);
      document.addEventListener('mouseup', handleMouseUp);
      
      return () => {
        document.removeEventListener('mousemove', handleMouseMove);
        document.removeEventListener('mouseup', handleMouseUp);
        document.body.style.userSelect = '';
        document.body.style.cursor = '';
      };
    }
  }, [propertiesPanel.isDragging, gridPanel.isDragging, gridPanel.isResizing, handleMouseMove, handleMouseUp]);

  return (
    <div className="h-full bg-gray-900 text-white p-3 overflow-hidden">
      <div className="max-w-full mx-auto h-full flex flex-col">
        {/* Header */}
        <div className="mb-2 flex-shrink-0">
          <h1 className="text-lg font-bold text-white mb-1">ASP SMED Map Editor - Enhanced</h1>
          <p className="text-gray-400 text-xs">Advanced item manipulation with Ctrl+Click selection and Ctrl+Arrow movement</p>
        </div>

        {/* Toolbar */}
        <div className="bg-gray-800 rounded-lg p-2 mb-2 flex-shrink-0">
          <div className="flex flex-wrap gap-2 items-center text-sm">
            {/* Field Types */}
            <div className="flex gap-1">
              {fieldTypes.map(({ type, label }) => (
                <div
                  key={type}
                  className="px-2 py-1 text-xs bg-gray-700 hover:bg-gray-600 border border-gray-600 rounded cursor-pointer transition-colors"
                  draggable
                  onDragStart={(e) => handleDragStart(e, type)}
                >
                  {label}
                </div>
              ))}
            </div>

            {/* Action Buttons */}
            <div className="flex flex-wrap gap-1">
              <button
                onClick={clearAllFields}
                className="px-2 py-1 text-xs bg-red-600 hover:bg-red-700 rounded flex items-center gap-1 transition-colors"
              >
                <TrashIcon className="w-3 h-3" />
                Clear All
              </button>
              
              {/* Zoom Controls */}
              <div className="flex items-center gap-1 ml-2 border-l border-gray-600 pl-2">
                <button
                  onClick={handleZoomOut}
                  className="px-2 py-1 text-xs bg-gray-600 hover:bg-gray-500 rounded transition-colors"
                  title="Zoom Out (50%~200%)"
                >
                  −
                </button>
                <span className="text-xs text-gray-300 min-w-[3rem] text-center">
                  {Math.round(zoomLevel * 100)}%
                </span>
                <button
                  onClick={handleZoomIn}
                  className="px-2 py-1 text-xs bg-gray-600 hover:bg-gray-500 rounded transition-colors"
                  title="Zoom In (50%~200%)"
                >
                  +
                </button>
                <button
                  onClick={handleZoomReset}
                  className="px-2 py-1 text-xs bg-gray-600 hover:bg-gray-500 rounded transition-colors"
                  title="Reset to 100%"
                >
                  100%
                </button>
              </div>
            </div>
          </div>
        </div>

        {/* Main Content Area - Now just provides space for floating panels */}
        <div className="flex-1 relative overflow-hidden bg-gray-900">
          
          {/* Properties Panel - Floating */}
          <div 
            className="absolute bg-gray-800 rounded-lg flex flex-col shadow-2xl border border-gray-600"
            style={{ 
              left: `${propertiesPanel.x}px`,
              top: `${propertiesPanel.y}px`,
              width: `${propertiesPanel.width}px`,
              height: `${propertiesPanel.height}px`,
              zIndex: 10
            }}
          >
            {/* Panel Header - Draggable */}
            <div 
              className="flex justify-between items-center p-2 border-b border-gray-700 cursor-grab active:cursor-grabbing bg-gray-700 rounded-t-lg"
              onMouseDown={(e) => handlePanelMouseDown(e, 'properties', 'drag')}
            >
              <h3 className="text-sm font-semibold">Field Properties</h3>
              <div className="flex items-center gap-2">
                <span className="text-xs text-blue-400 font-mono">
                  {propertiesPanel.width}×{propertiesPanel.height}
                </span>
                <div className="text-gray-400 text-xs">📌</div>
              </div>
            </div>
            
            {/* Panel Content */}
            <div className="overflow-y-auto p-2 space-y-2 flex-1">
              <div>
                <label className="block text-xs font-medium mb-0.5">Field ID</label>
                <input
                  type="text"
                  value={fieldProperties.id}
                  readOnly
                  className="w-full px-2 py-1 text-xs bg-gray-700 border border-gray-600 rounded text-gray-400"
                />
              </div>

              <div>
                <label className="block text-xs font-medium mb-0.5">Field Name</label>
                <input
                  type="text"
                  value={fieldProperties.name}
                  onChange={(e) => handleFieldPropertyChange('name', e.target.value)}
                  className="w-full px-2 py-1 text-xs bg-gray-700 border border-gray-600 rounded text-white"
                />
              </div>

              <div className="grid grid-cols-2 gap-1">
                <div>
                  <label className="block text-xs font-medium mb-0.5">Width</label>
                  <input
                    type="number"
                    value={fieldProperties.width}
                    onChange={(e) => handleFieldPropertyChange('width', parseInt(e.target.value))}
                    min="1"
                    max="80"
                    className="w-full px-2 py-1 text-xs bg-gray-700 border border-gray-600 rounded text-white"
                  />
                </div>
                <div>
                  <label className="block text-xs font-medium mb-0.5">Height</label>
                  <input
                    type="number"
                    value={fieldProperties.height}
                    onChange={(e) => handleFieldPropertyChange('height', parseInt(e.target.value))}
                    min="1"
                    max="5"
                    className="w-full px-2 py-1 text-xs bg-gray-700 border border-gray-600 rounded text-white"
                  />
                </div>
              </div>

              <div>
                <label className="block text-xs font-medium mb-0.5">Default Value</label>
                <input
                  type="text"
                  value={fieldProperties.value}
                  onChange={(e) => handleFieldPropertyChange('value', e.target.value)}
                  className="w-full px-2 py-1 text-xs bg-gray-700 border border-gray-600 rounded text-white"
                />
              </div>
            </div>
            
            {/* Panel Footer */}
            <div className="p-2 border-t border-gray-700 space-y-2">
              {!selectionState.primaryField && (
                <div className="text-xs text-gray-400 text-center bg-gray-700/50 p-2 rounded">
                  <div className="mb-1 font-medium">Advanced Field Controls:</div>
                  <div>Ctrl+Click: Select field</div>
                  <div>Ctrl+Shift+Click: Multi-select</div>
                  <div>Ctrl+Arrow: Move selected</div>
                  <div>Drag: Move groups</div>
                </div>
              )}
              
              {/* Selection info */}
              {selectionState.selectedFields.size > 0 && (
                <div className="text-xs text-blue-400 mb-2 p-2 bg-blue-900/20 rounded">
                  Selected: {selectionState.selectedFields.size} field{selectionState.selectedFields.size > 1 ? 's' : ''}
                  {selectionState.selectedFields.size > 1 && (
                    <div className="mt-1 text-xs text-gray-400">
                      Multi-selection active
                    </div>
                  )}
                </div>
              )}
              
              <button
                onClick={deleteSelectedField}
                disabled={!selectionState.primaryField}
                className="w-full px-2 py-1 text-xs bg-red-600 hover:bg-red-700 disabled:bg-gray-600 rounded transition-colors"
              >
                Delete Selected
              </button>
            </div>
          </div>

          {/* Grid Panel - Floating with Resize Handles */}
          <div 
            className="absolute bg-black border-2 border-green-400 rounded-lg shadow-2xl overflow-hidden group"
            style={{ 
              left: `${gridPanel.x}px`,
              top: `${gridPanel.y}px`,
              width: `${gridPanel.width}px`,
              height: `${gridPanel.height}px`,
              zIndex: 5
            }}
          >
            {/* Panel Header - Draggable */}
            <div 
              className="flex justify-between items-center p-2 bg-gray-800 border-b border-green-400 cursor-grab active:cursor-grabbing"
              onMouseDown={(e) => handlePanelMouseDown(e, 'grid', 'drag')}
            >
              <h3 className="text-sm font-semibold text-white">Enhanced Grid Panel ({cols}×{rows})</h3>
              <div className="flex items-center gap-2">
                <span className="text-xs text-green-400 font-mono">
                  {Math.round(zoomLevel * 100)}% | {gridPanel.width}×{gridPanel.height} | Cell:{Math.round(baseCharWidth)}×{Math.round(baseCharHeight)}
                </span>
                <div className="text-green-400 text-xs">🔄</div>
              </div>
            </div>
            
            {/* Grid Content */}
            <div
              ref={screenPanelRef}
              className="relative overflow-hidden bg-black"
              style={{
                width: '100%',
                height: 'calc(100% - 40px)',
                minWidth: `${cols * charWidth}px`,
                minHeight: `${rows * charHeight}px`
              }}
              onDragOver={handleDragOver}
              onDrop={handleDrop}
              onMouseMove={handleGridMouseMove}
              onMouseUp={handleGridMouseUp}
              onClick={(e) => {
                const target = e.target as HTMLElement;
                const isGridBackground = target === e.currentTarget;
                const isGridLine = target.classList.contains('bg-gray-700') && target.classList.contains('absolute');
                const isFieldElement = target.classList.contains('cursor-pointer') || !!target.closest('.cursor-pointer');
                
                if (!propertiesPanel.isDragging && !gridPanel.isDragging && !gridPanel.isResizing) {
                  const handled = mouseHandler.handleGridClick(e);
                  
                  if (!handled && !(e.ctrlKey || e.metaKey)) {
                    setStatusMessage('Use Ctrl+Click to interact with fields');
                    setTimeout(() => {
                      setStatusMessage('Ready');
                    }, 2000);
                  }
                  
                  // Reset field properties if selection was cleared
                  if (handled && !selectionState.primaryField) {
                    setFieldProperties({
                      id: '',
                      name: '',
                      width: 10,
                      height: 1,
                      value: '',
                      cssClass: '',
                      attributes: 'normal',
                      backgroundColor: '#00ff00',
                      textColor: '#ffffff'
                    });
                  }
                }
              }}
            >
              {/* Grid */}
              <div className="absolute inset-0 opacity-30">
                {Array.from({ length: cols + 1 }, (_, i) => (
                  <div
                    key={`v-${i}`}
                    className="absolute w-px h-full bg-gray-700"
                    style={{ left: `${i * charWidth}px` }}
                  />
                ))}
                {Array.from({ length: rows + 1 }, (_, i) => (
                  <div
                    key={`h-${i}`}
                    className="absolute w-full h-px bg-gray-700"
                    style={{ top: `${i * charHeight}px` }}
                  />
                ))}
              </div>

              {/* Advanced Field Rendering */}
              {Array.from(fields.values()).map(field => (
                <AdvancedFieldRenderer
                  key={field.id}
                  field={field}
                  selectionState={selectionState}
                  dragState={mouseHandler.getDragState()}
                  charWidth={charWidth}
                  charHeight={charHeight}
                  zoomLevel={zoomLevel}
                  showDimensions={true}
                  showSnapIndicators={true}
                  enableVisualGuides={true}
                  isDarkMode={isDarkMode}
                  onMouseDown={handleFieldMouseDown}
                  onMouseUp={handleFieldMouseUp}
                  onClick={handleFieldClick}
                />
              ))}

              {/* Grid Info */}
              <div className="absolute top-2 right-2 text-xs text-gray-500 bg-black/50 px-2 py-1 rounded">
                {cols} Cols x {rows} Rows<br/>
                Cell: {Math.round(baseCharWidth)}×{Math.round(baseCharHeight)}px<br/>
                Zoom: {Math.round(zoomLevel * 100)}%<br/>
                Selected: {selectionState.selectedFields.size}
              </div>
            </div>
            
            {/* Resize Handles - Hidden by default, visible on hover */}
            <div className="absolute top-0 right-0 w-3 h-3 bg-green-400 cursor-nw-resize opacity-0 group-hover:opacity-75 hover:!opacity-100 transition-opacity"
                 onMouseDown={(e) => handlePanelMouseDown(e, 'grid', 'resize', 'ne')} />
            <div className="absolute bottom-0 right-0 w-3 h-3 bg-green-400 cursor-se-resize opacity-0 group-hover:opacity-75 hover:!opacity-100 transition-opacity"
                 onMouseDown={(e) => handlePanelMouseDown(e, 'grid', 'resize', 'se')} />
            <div className="absolute bottom-0 left-0 w-3 h-3 bg-green-400 cursor-sw-resize opacity-0 group-hover:opacity-75 hover:!opacity-100 transition-opacity"
                 onMouseDown={(e) => handlePanelMouseDown(e, 'grid', 'resize', 'sw')} />
            <div className="absolute top-0 left-0 w-3 h-3 bg-green-400 cursor-nw-resize opacity-0 group-hover:opacity-75 hover:!opacity-100 transition-opacity"
                 onMouseDown={(e) => handlePanelMouseDown(e, 'grid', 'resize', 'nw')} />
          </div>
        </div>

        {/* Status Bar */}
        <div className="mt-1 bg-gray-800 rounded px-2 py-1 text-xs text-gray-300 flex-shrink-0">
          {statusMessage}
        </div>
      </div>
    </div>
  );
};

export default ASPMapEditorEnhanced;
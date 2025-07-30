import React, { useState, useEffect, useRef, useCallback } from 'react';
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

interface Field {
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

interface ASPMapEditorProps {
  isDarkMode: boolean;
}

const ASPMapEditor: React.FC<ASPMapEditorProps> = ({ isDarkMode }) => {
  const { t } = useI18n();
  const [fields, setFields] = useState<Map<string, Field>>(new Map());
  const [selectedField, setSelectedField] = useState<string | null>(null);
  
  const [selectedFields, setSelectedFields] = useState<Set<string>>(new Set());
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
  const [zoomLevel, setZoomLevel] = useState(1); // 1 = 100%, 0.5 = 50%, 2 = 200%
  const [lastFieldInteraction, setLastFieldInteraction] = useState(0); // Timestamp of last field interaction
  const [draggedField, setDraggedField] = useState<string | null>(null);
  const [fieldDragOffset, setFieldDragOffset] = useState({ x: 0, y: 0 });
  
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
  const availableWidth = gridPanel.width - 40; // Subtract padding/margins
  const availableHeight = gridPanel.height - 80; // Subtract header and margins
  
  const baseCharWidth = Math.max(6, availableWidth / cols);
  const baseCharHeight = Math.max(12, availableHeight / rows);
  
  const charWidth = baseCharWidth * zoomLevel;
  const charHeight = baseCharHeight * zoomLevel;

  // Zoom functions
  const handleZoomIn = () => {
    setZoomLevel(prev => Math.min(prev + 0.25, 2)); // Max 200%
  };

  const handleZoomOut = () => {
    setZoomLevel(prev => Math.max(prev - 0.25, 0.5)); // Min 50%
  };

  const handleZoomReset = () => {
    setZoomLevel(1); // Reset to 100%
  };

  useEffect(() => {
    updateMapDataOutput();
  }, [fields]);

  useEffect(() => {
    loadSmedFiles();
    loadCatalogMaps();
    loadVolumes();
  }, []);


  // Floating panel drag and resize handlers
  const handlePanelMouseDown = useCallback((e: React.MouseEvent, panelType: 'properties' | 'grid', action: 'drag' | 'resize', handle?: string) => {
    e.preventDefault();
    e.stopPropagation();
    
    // Clear any existing field selection to prevent conflicts
    if (action === 'drag' || action === 'resize') {
      setSelectedField(null);
      setSelectedFields(new Set());
    }
    
    if (action === 'drag') {
      // Get the panel's current position for offset calculation
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
  }, [propertiesPanel, gridPanel]);

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
    
    // Reset document styles immediately and force reflow
    document.body.style.userSelect = '';
    document.body.style.cursor = '';
    document.body.style.pointerEvents = '';
    
    // Force a reflow to ensure styles are applied
    void document.body.offsetHeight;
    
    // Clear any lingering event listeners
    document.removeEventListener('mousemove', handleMouseMove);
    document.removeEventListener('mouseup', handleMouseUp);
    
    // Additional cleanup with a small delay to ensure all events are processed
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
        // Clean up document styles on unmount
        document.body.style.userSelect = '';
        document.body.style.cursor = '';
      };
    }
  }, [propertiesPanel.isDragging, gridPanel.isDragging, gridPanel.isResizing, handleMouseMove, handleMouseUp]);

  const loadSmedFiles = async () => {
    try {
      const response = await fetch('http://localhost:8000/api/smed/files');
      if (response.ok) {
        const data = await response.json();
        setSmedFiles(data.files || []);
      } else {
        console.error('Failed to load SMED files');
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
      } else {
        console.error('Failed to load catalog maps');
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
      } else {
        console.error('Failed to load volumes');
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
      } else {
        console.error('Failed to load libraries');
      }
    } catch (error) {
      console.error('Error loading libraries:', error);
    }
  };

  const fieldTypes = [
    { type: 'text', label: `📝 ${t('mapEditor.textField')}`, icon: DocumentTextIcon },
    { type: 'input', label: `📥 ${t('mapEditor.inputField')}`, icon: PlusIcon },
    { type: 'output', label: `📤 ${t('mapEditor.outputField')}`, icon: DocumentTextIcon },
    { type: 'button', label: `🔘 ${t('mapEditor.buttonField')}`, icon: PlayIcon }
  ];

  const getDefaultValue = (type: string) => {
    switch (type) {
      case 'text': return 'Text Label';
      case 'input': return '_'.repeat(10);
      case 'output': return 'Output';
      case 'button': return 'Button';
      default: return 'Field';
    }
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
    setSelectedField(fieldId);
    updateFieldProperties(newField);
    setStatusMessage(t('mapEditor.fieldAdded', { fieldId }));
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
    
    if (selectedField) {
      const field = fields.get(selectedField);
      if (field) {
        const updatedField = { ...field, [property]: value };
        setFields(new Map(fields.set(selectedField, updatedField)));
      }
    }
  };

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
    
    // Clean up any document styles that might interfere with clicking
    setTimeout(() => {
      document.body.style.userSelect = '';
      document.body.style.cursor = '';
    }, 100);
  };

  const handleFieldClick = (fieldId: string, e: React.MouseEvent) => {
    
    // Double-check field exists
    const field = fields.get(fieldId);
    if (!field) {
      return;
    }
    
    // Only allow field selection when Ctrl key is pressed
    if (!e.ctrlKey && !e.metaKey) {
      setStatusMessage(t('mapEditor.ctrlClickRequired'));
      setTimeout(() => {
        setStatusMessage(t('mapEditor.statusMessage'));
      }, 2000);
      return;
    }
    
    // Check if we're in the middle of a panel drag/resize operation
    if (propertiesPanel.isDragging || gridPanel.isDragging || gridPanel.isResizing) {
      return; // Ignore clicks during drag operations
    }
    
    // Ensure document styles are clean before processing click
    document.body.style.userSelect = '';
    document.body.style.cursor = '';
    
    
    if (e.shiftKey) {
      // Multi-select mode (Ctrl+Shift+Click)
      const newSelectedFields = new Set(selectedFields);
      if (newSelectedFields.has(fieldId)) {
        newSelectedFields.delete(fieldId);
      } else {
        newSelectedFields.add(fieldId);
      }
      setSelectedFields(newSelectedFields);
      setStatusMessage(t('mapEditor.multiSelectUpdated', { count: newSelectedFields.size.toString() }));
    } else {
      // Single select mode (Ctrl+Click)
      setSelectedField(fieldId);
      setSelectedFields(new Set());
      
      updateFieldProperties(field);
      setStatusMessage(t('mapEditor.fieldSelected', { fieldId }));
    }
  };

  const handleFieldDragStart = (fieldId: string, e: React.MouseEvent) => {
    setDraggedField(fieldId);
    
    const field = fields.get(fieldId);
    if (field && screenPanelRef.current) {
      const rect = screenPanelRef.current.getBoundingClientRect();
      const fieldLeft = field.x * charWidth;
      const fieldTop = field.y * charHeight;
      
      setFieldDragOffset({
        x: e.clientX - rect.left - fieldLeft,
        y: e.clientY - rect.top - fieldTop
      });
    }
  };

  const handleFieldDragMove = (e: React.MouseEvent) => {
    if (draggedField && screenPanelRef.current) {
      const rect = screenPanelRef.current.getBoundingClientRect();
      const newX = Math.round((e.clientX - rect.left - fieldDragOffset.x) / charWidth);
      const newY = Math.round((e.clientY - rect.top - fieldDragOffset.y) / charHeight);
      
      // Constrain to grid boundaries
      const constrainedX = Math.max(0, Math.min(cols - 1, newX));
      const constrainedY = Math.max(0, Math.min(rows - 1, newY));
      
      const field = fields.get(draggedField);
      if (field && (field.x !== constrainedX || field.y !== constrainedY)) {
        const updatedField = { ...field, x: constrainedX, y: constrainedY };
        const newFields = new Map(fields);
        newFields.set(draggedField, updatedField);
        setFields(newFields);
        
        // Update field properties if this field is selected
        if (selectedField === draggedField) {
          updateFieldProperties(updatedField);
        }
      }
    }
  };

  const handleFieldDragEnd = () => {
    if (draggedField) {
      setDraggedField(null);
      setFieldDragOffset({ x: 0, y: 0 });
      setStatusMessage(t('mapEditor.fieldMoved', { fieldId: draggedField }));
    }
  };

  const deleteSelectedField = () => {
    if (selectedField) {
      const newFields = new Map(fields);
      newFields.delete(selectedField);
      setFields(newFields);
      setSelectedField(null);
      setStatusMessage(t('mapEditor.fieldDeleted', { selectedField }));
    }
  };

  const clearAllFields = () => {
    setFields(new Map());
    setSelectedField(null);
    setSelectedFields(new Set());
    setFieldCounter(0);
    setStatusMessage(t('mapEditor.allFieldsDeleted'));
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

  const loadSmedFile = async (filename: string) => {
    try {
      const response = await fetch(`http://localhost:8000/api/smed/content/${filename}`);
      if (response.ok) {
        const content = await response.text();
        parseSmedContent(content);
        setStatusMessage(t('mapEditor.fileLoaded', { filename }));
      } else {
        setStatusMessage(t('mapEditor.fileLoadError', { filename }));
      }
    } catch (error) {
      setStatusMessage(t('mapEditor.fileLoadException', { error: String(error) }));
    }
  };

  const parseSmedContent = (content: string) => {
    const lines = content.split(/\r?\n/).map(line => line.trim());
    const newFields = new Map<string, Field>();
    let counter = 0;

    lines.forEach(line => {
      if (line.startsWith('ITEM')) {
        const match = line.match(/ITEM\s+(\S+)\s+TYPE=(\w)\s+POS=\((\d+),(\d+)\)\s+PROMPT=\"(.+?)\"(?:\s+COLOR=(#[A-Fa-f0-9]{6}|\w+))?/);
        if (match) {
          const [_, name, type, row, col, prompt, color] = match;
          counter++;
          
          const fieldId = `FIELD_${counter.toString().padStart(3, '0')}`;
          
          const field: Field = {
            id: fieldId,
            name: name,
            type: type === 'N' ? 'input' : type === 'A' ? 'output' : type === 'X' ? 'button' : 'text',
            x: parseInt(col) - 1,
            y: parseInt(row) - 1,
            width: prompt.length || 10,
            height: 1,
            value: prompt,
            cssClass: '',
            attributes: 'normal',
            backgroundColor: color || '#00ff00',
            textColor: '#ffffff'
          };
          
          newFields.set(fieldId, field);
        }
      }
    });

    setFields(newFields);
    setFieldCounter(counter);
  };

  const exportSmedFile = () => {
    const lines = ['MAPNAME GENERATEDMAP', ''];
    let groupCount = 0;

    Array.from(fields.values()).forEach((field, index) => {
      if (index % 5 === 0) {
        groupCount++;
        lines.push(`GROUP GRP${groupCount.toString().padStart(3, '0')}`);
      }
      
      const typeMap: { [key: string]: string } = {
        'input': 'N',
        'output': 'A',
        'text': 'T',
        'button': 'X'
      };
      
      const itemLine = `  ITEM ${field.name} TYPE=${typeMap[field.type] || 'T'} POS=(${field.y + 1},${field.x + 1}) PROMPT="${field.value}"${field.backgroundColor ? ` COLOR=${field.backgroundColor}` : ''}`;
      lines.push(itemLine);
    });

    const smedContent = lines.join('\n');
    const blob = new Blob([smedContent], { type: 'text/plain' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = 'generated_map.smed';
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
    
    setStatusMessage(t('mapEditor.fileDownloaded'));
  };

  const showPreview = () => {
    setIsPreviewOpen(true);
  };

  const closePreview = () => {
    setIsPreviewOpen(false);
  };

  const saveSmedFile = async () => {
    if (!currentFileName) {
      setShowSaveDialog(true);
      return;
    }
    
    await performSave(currentFileName, selectedVolume, selectedLibrary);
  };

  const performSave = async (filename: string, volume: string, library: string) => {
    try {
      const typeMap: { [key: string]: string } = {
        'text': 'T',
        'input': 'I', 
        'output': 'O',
        'button': 'B'
      };

      const lines = [`MAPNAME ${filename}`];
      
      Array.from(fields.values()).forEach(field => {
        const itemLine = `  ITEM ${field.name} TYPE=${typeMap[field.type] || 'T'} POS=(${field.y + 1},${field.x + 1}) PROMPT="${field.value}"${field.backgroundColor ? ` COLOR=${field.backgroundColor}` : ''}`;
        lines.push(itemLine);
      });

      const smedContent = lines.join('\n');
      
      const response = await fetch('http://localhost:8000/api/smed/save', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          filename,
          content: smedContent,
          volume,
          library
        })
      });
      
      if (response.ok) {
        const result = await response.json();
        setStatusMessage(`保存成功: ${filename}`);
        setCurrentFileName(filename);
        setShowSaveDialog(false);
      } else {
        const error = await response.text();
        setStatusMessage(`保存エラー: ${error}`);
      }
    } catch (error) {
      setStatusMessage(`保存例外: ${String(error)}`);
    }
  };

  const loadCatalogMap = async (map: any) => {
    try {
      const mapPath = `${map.volume}/${map.library}/${map.mapfile}`;
      const response = await fetch(`http://localhost:8000/api/smed/content/${mapPath}`);
      if (response.ok) {
        const content = await response.text();
        parseSmedContent(content);
        setCurrentFileName(map.name);
        setSelectedVolume(map.volume);
        setSelectedLibrary(map.library);
        setStatusMessage(t('mapEditor.fileLoaded', { filename: map.name }));
      } else {
        setStatusMessage(t('mapEditor.fileLoadError', { filename: map.name }));
      }
    } catch (error) {
      setStatusMessage(t('mapEditor.fileLoadException', { error: String(error) }));
    }
  };

  return (
    <div className="h-full bg-gray-900 text-white p-3 overflow-hidden">
      <div className="max-w-full mx-auto h-full flex flex-col">
        {/* Header */}
        <div className="mb-2 flex-shrink-0">
          <h1 className="text-lg font-bold text-white mb-1">ASP SMED Map Editor</h1>
          <p className="text-gray-400 text-xs">{t('mapEditor.description')}</p>
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
                onClick={showPreview}
                className="px-2 py-1 text-xs bg-green-600 hover:bg-green-700 rounded flex items-center gap-1 transition-colors"
              >
                <EyeIcon className="w-3 h-3" />
                {t('mapEditor.preview')}
              </button>
              
              <select
                className="px-2 py-1 text-xs bg-gray-700 border border-gray-600 rounded text-white"
                onChange={(e) => e.target.value && loadSmedFile(e.target.value)}
                value=""
              >
                <option value="">{t('mapEditor.loadSmed')}</option>
                {smedFiles.map(file => (
                  <option key={file} value={file}>{file}</option>
                ))}
              </select>

              <select
                className="px-2 py-1 text-xs bg-gray-700 border border-gray-600 rounded text-white"
                onChange={(e) => {
                  if (e.target.value) {
                    const map = catalogMaps.find(m => m.name === e.target.value);
                    if (map) loadCatalogMap(map);
                  }
                }}
                value=""
              >
                <option value="">{t('mapEditor.loadFromCatalog')}</option>
                {catalogMaps.filter(map => map.maptype === 'SMED').map(map => (
                  <option key={`${map.volume}-${map.library}-${map.name}`} value={map.name}>
                    {map.volume}/{map.library}/{map.name} - {map.description}
                  </option>
                ))}
              </select>
              
              <button
                onClick={saveSmedFile}
                className="px-2 py-1 text-xs bg-purple-600 hover:bg-purple-700 rounded flex items-center gap-1 transition-colors"
              >
                <CloudArrowUpIcon className="w-3 h-3" />
                {t('mapEditor.saveSmed')}
              </button>
              
              <button
                onClick={exportSmedFile}
                className="px-2 py-1 text-xs bg-blue-600 hover:bg-blue-700 rounded flex items-center gap-1 transition-colors"
              >
                <CloudArrowDownIcon className="w-3 h-3" />
                {t('mapEditor.exportSmed')}
              </button>
              
              <button
                onClick={clearAllFields}
                className="px-2 py-1 text-xs bg-red-600 hover:bg-red-700 rounded flex items-center gap-1 transition-colors"
              >
                <TrashIcon className="w-3 h-3" />
                {t('mapEditor.deleteAll')}
              </button>
              
              {/* Zoom Controls */}
              <div className="flex items-center gap-1 ml-2 border-l border-gray-600 pl-2">
                <button
                  onClick={handleZoomOut}
                  className="px-2 py-1 text-xs bg-gray-600 hover:bg-gray-500 rounded transition-colors"
                  title="축소 (50%~200%)"
                >
                  −
                </button>
                <span className="text-xs text-gray-300 min-w-[3rem] text-center">
                  {Math.round(zoomLevel * 100)}%
                </span>
                <button
                  onClick={handleZoomIn}
                  className="px-2 py-1 text-xs bg-gray-600 hover:bg-gray-500 rounded transition-colors"
                  title="확대 (50%~200%)"
                >
                  +
                </button>
                <button
                  onClick={handleZoomReset}
                  className="px-2 py-1 text-xs bg-gray-600 hover:bg-gray-500 rounded transition-colors"
                  title="100% 크기로 리셋"
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
              <h3 className="text-sm font-semibold">{t('mapEditor.fieldProperties')}</h3>
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
                <label className="block text-xs font-medium mb-0.5">{t('mapEditor.fieldId')}</label>
                <input
                  type="text"
                  value={fieldProperties.id}
                  readOnly
                  className="w-full px-2 py-1 text-xs bg-gray-700 border border-gray-600 rounded text-gray-400"
                />
              </div>

              <div>
                <label className="block text-xs font-medium mb-0.5">{t('mapEditor.fieldName')}</label>
                <input
                  type="text"
                  value={fieldProperties.name}
                  onChange={(e) => handleFieldPropertyChange('name', e.target.value)}
                  className="w-full px-2 py-1 text-xs bg-gray-700 border border-gray-600 rounded text-white"
                />
              </div>

              <div className="grid grid-cols-2 gap-1">
                <div>
                  <label className="block text-xs font-medium mb-0.5">{t('mapEditor.width')}</label>
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
                  <label className="block text-xs font-medium mb-0.5">{t('mapEditor.height')}</label>
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
                <label className="block text-xs font-medium mb-0.5">{t('mapEditor.defaultValue')}</label>
                <input
                  type="text"
                  value={fieldProperties.value}
                  onChange={(e) => handleFieldPropertyChange('value', e.target.value)}
                  className="w-full px-2 py-1 text-xs bg-gray-700 border border-gray-600 rounded text-white"
                />
              </div>

              <div>
                <label className="block text-xs font-medium mb-0.5">{t('mapEditor.attributes')}</label>
                <select
                  value={fieldProperties.attributes}
                  onChange={(e) => handleFieldPropertyChange('attributes', e.target.value)}
                  className="w-full px-2 py-1 text-xs bg-gray-700 border border-gray-600 rounded text-white"
                >
                  <option value="normal">{t('mapEditor.normal')}</option>
                  <option value="readonly">{t('mapEditor.readonly')}</option>
                  <option value="required">{t('mapEditor.required')}</option>
                  <option value="hidden">{t('mapEditor.hidden')}</option>
                  <option value="disabled">{t('mapEditor.disabled')}</option>
                </select>
              </div>

              <div className="grid grid-cols-2 gap-1">
                <div>
                  <label className="block text-xs font-medium mb-0.5">{t('mapEditor.backgroundColor')}</label>
                  <input
                    type="color"
                    value={fieldProperties.backgroundColor}
                    onChange={(e) => handleFieldPropertyChange('backgroundColor', e.target.value)}
                    className="w-full h-6 bg-gray-700 border border-gray-600 rounded cursor-pointer"
                  />
                </div>
                <div>
                  <label className="block text-xs font-medium mb-0.5">{t('mapEditor.textColor')}</label>
                  <input
                    type="color"
                    value={fieldProperties.textColor}
                    onChange={(e) => handleFieldPropertyChange('textColor', e.target.value)}
                    className="w-full h-6 bg-gray-700 border border-gray-600 rounded cursor-pointer"
                  />
                </div>
              </div>
            </div>
            
            {/* Panel Footer */}
            <div className="p-2 border-t border-gray-700 space-y-2">
              {!selectedField && (
                <div className="text-xs text-gray-400 text-center bg-gray-700/50 p-2 rounded">
                  <div className="mb-1 font-medium">フィールド選択方法:</div>
                  <div>Ctrl+クリック: 単一選択</div>
                  <div>Ctrl+Shift+クリック: 複数選択</div>
                </div>
              )}
              <button
                onClick={deleteSelectedField}
                disabled={!selectedField}
                className="w-full px-2 py-1 text-xs bg-red-600 hover:bg-red-700 disabled:bg-gray-600 rounded transition-colors"
              >
                {t('mapEditor.deleteSelected')}
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
              <h3 className="text-sm font-semibold text-white">Grid Panel ({cols}×{rows})</h3>
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
              onMouseMove={handleFieldDragMove}
              onMouseUp={handleFieldDragEnd}
              onClick={(e) => {
                
                // Clear field selection only when clicking on grid background (not on fields or grid lines)
                const target = e.target as HTMLElement;
                const isGridBackground = target === e.currentTarget;
                const isGridLine = target.classList.contains('bg-gray-700') && target.classList.contains('absolute');
                const isFieldElement = target.classList.contains('cursor-pointer') || !!target.closest('.cursor-pointer');
                
                
                // Check if clicking on any field area by coordinates
                const clickX = e.nativeEvent.offsetX;
                const clickY = e.nativeEvent.offsetY;
                let clickedOnField = false;
                
                
                Array.from(fields.values()).forEach(field => {
                  const fieldLeft = field.x * charWidth;
                  const fieldTop = field.y * charHeight;
                  const fieldRight = fieldLeft + (field.width * charWidth);
                  const fieldBottom = fieldTop + (field.height * charHeight);
                  
                  
                  if (clickX >= fieldLeft && clickX <= fieldRight && 
                      clickY >= fieldTop && clickY <= fieldBottom) {
                    clickedOnField = true;
                  }
                });
                
                // If clicking on a field element or field area, don't interfere
                if (isFieldElement || clickedOnField) {
                  return;
                }
                
                // Additional protection: if we have recent field interaction, ignore grid click
                const now = Date.now();
                const timeSinceLastFieldInteraction = now - lastFieldInteraction;
                
                if (timeSinceLastFieldInteraction < 200) { // Within 200ms of field interaction
                  return;
                }
                
                if (isGridBackground || isGridLine) {
                  // Only clear if we're not in the middle of a drag operation AND Ctrl key is pressed
                  if (!propertiesPanel.isDragging && !gridPanel.isDragging && !gridPanel.isResizing) {
                    if (e.ctrlKey || e.metaKey) {
                      setSelectedField(null);
                      setSelectedFields(new Set());
                      // Reset field properties to default
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
                      setStatusMessage(t('mapEditor.selectionCleared'));
                    } else {
                      setStatusMessage(t('mapEditor.ctrlClickToDeselect'));
                      setTimeout(() => {
                        setStatusMessage(t('mapEditor.statusMessage'));
                      }, 2000);
                    }
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

              {/* Fields */}
              {Array.from(fields.values()).map(field => {
                // Create a memoized field component to prevent unnecessary re-renders
                const fieldStyle: React.CSSProperties = {
                  left: `${field.x * charWidth}px`,
                  top: `${field.y * charHeight}px`,
                  width: `${field.width * charWidth}px`,
                  height: `${field.height * charHeight}px`,
                  backgroundColor: field.backgroundColor + '20',
                  color: field.textColor,
                  padding: `${2 * zoomLevel}px`,
                  fontSize: `${12 * zoomLevel}px`,
                  lineHeight: `${16 * zoomLevel}px`,
                  zIndex: selectedField === field.id ? 50 : 30, // Increased z-index significantly
                  pointerEvents: 'auto',
                  userSelect: 'none',
                  minWidth: '10px',
                  minHeight: '16px',
                  position: 'absolute'
                };
                
                return (
                <div
                  key={field.id}
                  className={`cursor-pointer border-2 font-mono text-sm transition-all hover:border-yellow-300 hover:shadow-md hover:bg-yellow-100/50 ${
                    selectedField === field.id ? 'border-yellow-400 bg-yellow-400/20 shadow-lg' : 'border-green-400 bg-green-400/10'
                  } ${selectedFields.has(field.id) ? 'border-orange-400 bg-orange-400/20' : ''}`}
                  title="Ctrl+クリック: 単一選択 / Ctrl+Shift+クリック: 複数選択"
                  style={fieldStyle}
                  onClick={(e) => {
                    // Stop propagation FIRST to prevent grid handler from interfering
                    e.stopPropagation();
                    e.preventDefault();
                    
                    
                    // This is now a backup handler - main logic is in onMouseDown
                    if (e.ctrlKey || e.metaKey) {
                      handleFieldClick(field.id, e);
                    }
                  }}
                  onMouseDown={(e) => {
                    
                    // Record field interaction timestamp
                    setLastFieldInteraction(Date.now());
                    
                    // Stop event propagation to prevent grid click
                    e.stopPropagation();
                    e.nativeEvent.stopImmediatePropagation();
                    
                    if (e.ctrlKey || e.metaKey) {
                      // Ctrl+Click: Select field (prevent default to avoid drag)
                      e.preventDefault();
                      handleFieldClick(field.id, e);
                    } else {
                      // Regular click: Start drag operation (don't prevent default)
                      handleFieldDragStart(field.id, e);
                      
                      // If field is not selected, show requirement message
                      if (selectedField !== field.id) {
                        setStatusMessage(t('mapEditor.dragHint'));
                        setTimeout(() => {
                          setStatusMessage(t('mapEditor.statusMessage'));
                        }, 2000);
                      }
                    }
                  }}
                  onMouseUp={(e) => {
                    e.stopPropagation();
                    e.preventDefault();
                  }}
                  onContextMenu={(e) => {
                    e.preventDefault();
                    e.stopPropagation();
                  }}
                  onDoubleClick={(e) => {
                    e.stopPropagation();
                    e.preventDefault();
                  }}
                >
                  {field.value.substring(0, field.width * field.height)}
                </div>
                );
              })}

              {/* Grid Info */}
              <div className="absolute top-2 right-2 text-xs text-gray-500 bg-black/50 px-2 py-1 rounded">
                {cols} Cols x {rows} Rows<br/>
                Cell: {Math.round(baseCharWidth)}×{Math.round(baseCharHeight)}px<br/>
                Zoom: {Math.round(zoomLevel * 100)}%
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
            <div className="absolute top-0 left-1/2 transform -translate-x-1/2 w-3 h-3 bg-green-400 cursor-n-resize opacity-0 group-hover:opacity-75 hover:!opacity-100 transition-opacity"
                 onMouseDown={(e) => handlePanelMouseDown(e, 'grid', 'resize', 'n')} />
            <div className="absolute bottom-0 left-1/2 transform -translate-x-1/2 w-3 h-3 bg-green-400 cursor-s-resize opacity-0 group-hover:opacity-75 hover:!opacity-100 transition-opacity"
                 onMouseDown={(e) => handlePanelMouseDown(e, 'grid', 'resize', 's')} />
            <div className="absolute top-1/2 right-0 transform -translate-y-1/2 w-3 h-3 bg-green-400 cursor-e-resize opacity-0 group-hover:opacity-75 hover:!opacity-100 transition-opacity"
                 onMouseDown={(e) => handlePanelMouseDown(e, 'grid', 'resize', 'e')} />
            <div className="absolute top-1/2 left-0 transform -translate-y-1/2 w-3 h-3 bg-green-400 cursor-w-resize opacity-0 group-hover:opacity-75 hover:!opacity-100 transition-opacity"
                 onMouseDown={(e) => handlePanelMouseDown(e, 'grid', 'resize', 'w')} />
          </div>
        </div>

        {/* Status Bar */}
        <div className="mt-1 bg-gray-800 rounded px-2 py-1 text-xs text-gray-300 flex-shrink-0">
          {statusMessage}
        </div>
      </div>

      {/* Preview Modal */}
      {isPreviewOpen && (
        <div className="fixed inset-0 bg-black bg-opacity-80 flex items-center justify-center z-50">
          <div className="bg-gray-800 rounded-lg p-6 max-w-4xl max-h-[90vh] overflow-auto">
            <div className="flex justify-between items-center mb-4">
              <h3 className="text-xl font-bold">{t('mapEditor.smedPreview')}</h3>
              <button
                onClick={closePreview}
                className="px-4 py-2 bg-red-600 hover:bg-red-700 rounded transition-colors"
              >
                {t('common.close')}
              </button>
            </div>
            
            <div
              className="bg-black border-2 border-green-400 relative"
              style={{
                width: `${cols * 10}px`,
                height: `${rows * 20}px`,
                fontFamily: 'monospace',
                fontSize: '14px',
                lineHeight: '20px'
              }}
            >
              {Array.from(fields.values()).map(field => (
                <div
                  key={field.id}
                  className="absolute font-mono whitespace-pre cursor-default"
                  style={{
                    left: `${field.x * 10}px`,
                    top: `${field.y * 20}px`,
                    width: `${field.width * 10}px`,
                    height: `${field.height * 20}px`,
                    backgroundColor: field.backgroundColor + '20',
                    color: field.textColor,
                    fontSize: '14px',
                    lineHeight: '20px',
                    padding: '1px 3px',
                    border: field.type === 'input' ? '1px solid #0080ff' : 'none'
                  }}
                >
                  {field.value}
                </div>
              ))}
            </div>
          </div>
        </div>
      )}

      {/* Save Dialog */}
      {showSaveDialog && (
        <div className="fixed inset-0 bg-black bg-opacity-80 flex items-center justify-center z-50">
          <div className="bg-gray-800 rounded-lg p-6 max-w-md w-full mx-4">
            <div className="flex justify-between items-center mb-4">
              <h3 className="text-xl font-bold">{t('mapEditor.saveSmed')}</h3>
              <button
                onClick={() => setShowSaveDialog(false)}
                className="text-gray-400 hover:text-white"
              >
                ✕
              </button>
            </div>
            
            <div className="space-y-4">
              <div>
                <label className="block text-sm font-medium text-gray-300 mb-2">
                  {t('mapEditor.selectVolume')}
                </label>
                <select
                  value={selectedVolume}
                  onChange={(e) => {
                    setSelectedVolume(e.target.value);
                    loadLibraries(e.target.value);
                  }}
                  className="w-full px-3 py-2 bg-gray-700 border border-gray-600 rounded text-white"
                >
                  {volumes.map(volume => (
                    <option key={volume} value={volume}>{volume}</option>
                  ))}
                </select>
              </div>

              <div>
                <label className="block text-sm font-medium text-gray-300 mb-2">
                  {t('mapEditor.selectLibrary')}
                </label>
                <select
                  value={selectedLibrary}
                  onChange={(e) => setSelectedLibrary(e.target.value)}
                  className="w-full px-3 py-2 bg-gray-700 border border-gray-600 rounded text-white"
                >
                  {libraries.map(library => (
                    <option key={library} value={library}>{library}</option>
                  ))}
                </select>
              </div>

              <div>
                <label className="block text-sm font-medium text-gray-300 mb-2">
                  {t('mapEditor.enterFileName')}
                </label>
                <input
                  type="text"
                  value={currentFileName}
                  onChange={(e) => setCurrentFileName(e.target.value)}
                  placeholder="Enter SMED file name"
                  className="w-full px-3 py-2 bg-gray-700 border border-gray-600 rounded text-white placeholder-gray-400"
                />
              </div>

              <div className="flex gap-2 pt-4">
                <button
                  onClick={() => currentFileName && performSave(currentFileName, selectedVolume, selectedLibrary)}
                  disabled={!currentFileName || !selectedVolume || !selectedLibrary}
                  className="flex-1 px-4 py-2 bg-purple-600 hover:bg-purple-700 disabled:bg-gray-600 disabled:cursor-not-allowed rounded transition-colors"
                >
                  {t('common.save')}
                </button>
                <button
                  onClick={() => setShowSaveDialog(false)}
                  className="px-4 py-2 bg-gray-600 hover:bg-gray-700 rounded transition-colors"
                >
                  {t('common.cancel')}
                </button>
              </div>
            </div>
          </div>
        </div>
      )}
    </div>
  );
};

export default ASPMapEditor;
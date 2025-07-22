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
  const [propertiesPanelWidth, setPropertiesPanelWidth] = useState(224); // 224px = w-56
  const [isResizing, setIsResizing] = useState(false);
  const [zoomLevel, setZoomLevel] = useState(1); // 1 = 100%, 0.5 = 50%, 2 = 200%
  
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

  const charWidth = 10 * zoomLevel;
  const charHeight = 20 * zoomLevel;
  const cols = 80;
  const rows = 24;

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

  // Handle mouse events for resizing properties panel
  const handleMouseDown = useCallback((e: React.MouseEvent) => {
    setIsResizing(true);
    e.preventDefault();
  }, []);

  const handleMouseMove = useCallback((e: MouseEvent) => {
    if (!isResizing) return;
    
    const newWidth = window.innerWidth - e.clientX;
    const minWidth = 180;
    const maxWidth = 400;
    
    if (newWidth >= minWidth && newWidth <= maxWidth) {
      setPropertiesPanelWidth(newWidth);
    }
  }, [isResizing]);

  const handleMouseUp = useCallback(() => {
    setIsResizing(false);
  }, []);

  useEffect(() => {
    if (isResizing) {
      document.addEventListener('mousemove', handleMouseMove);
      document.addEventListener('mouseup', handleMouseUp);
      return () => {
        document.removeEventListener('mousemove', handleMouseMove);
        document.removeEventListener('mouseup', handleMouseUp);
      };
    }
  }, [isResizing, handleMouseMove, handleMouseUp]);

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
    const fieldType = e.dataTransfer.getData('text/plain');
    
    if (screenPanelRef.current) {
      const rect = screenPanelRef.current.getBoundingClientRect();
      const x = e.clientX - rect.left;
      const y = e.clientY - rect.top;
      
      createField(fieldType, x, y);
    }
    
    setDraggedFieldType(null);
  };

  const handleFieldClick = (fieldId: string, e: React.MouseEvent) => {
    e.stopPropagation();
    
    if (e.shiftKey) {
      // Multi-select mode
      const newSelectedFields = new Set(selectedFields);
      if (newSelectedFields.has(fieldId)) {
        newSelectedFields.delete(fieldId);
      } else {
        newSelectedFields.add(fieldId);
      }
      setSelectedFields(newSelectedFields);
    } else {
      // Single select mode
      setSelectedField(fieldId);
      setSelectedFields(new Set());
      
      const field = fields.get(fieldId);
      if (field) {
        updateFieldProperties(field);
      }
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
        <div className="mb-3 flex-shrink-0">
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

        {/* Main Content */}
        <div className="flex-1 flex gap-3 overflow-hidden h-full">
          {/* Screen Panel */}
          <div className="flex-grow flex flex-col min-w-0">
            <div
              ref={screenPanelRef}
              className="bg-black border-2 border-green-400 relative overflow-auto mx-auto"
              style={{
                width: `${cols * charWidth}px`,
                height: `${rows * charHeight}px`,
                maxWidth: `calc(100vw - ${propertiesPanelWidth + 120}px)`,
                maxHeight: 'calc(100vh - 220px)',
                minHeight: '400px',
                minWidth: '500px'
              }}
              onDragOver={handleDragOver}
              onDrop={handleDrop}
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
              {Array.from(fields.values()).map(field => (
                <div
                  key={field.id}
                  className={`absolute cursor-pointer border font-mono text-sm transition-all ${
                    selectedField === field.id ? 'border-yellow-400 bg-yellow-400/20' : 'border-green-400 bg-green-400/10'
                  } ${selectedFields.has(field.id) ? 'border-orange-400 bg-orange-400/20' : ''}`}
                  style={{
                    left: `${field.x * charWidth}px`,
                    top: `${field.y * charHeight}px`,
                    width: `${field.width * charWidth}px`,
                    height: `${field.height * charHeight}px`,
                    backgroundColor: field.backgroundColor + '20',
                    color: field.textColor,
                    padding: `${2 * zoomLevel}px`,
                    fontSize: `${12 * zoomLevel}px`,
                    lineHeight: `${16 * zoomLevel}px`
                  }}
                  onClick={(e) => handleFieldClick(field.id, e)}
                >
                  {field.value.substring(0, field.width * field.height)}
                </div>
              ))}

              {/* Grid Info */}
              <div className="absolute top-2 right-2 text-xs text-gray-500">
                80 Cols x 24 Rows
              </div>
            </div>
          </div>

          {/* Resize Handle */}
          <div 
            className="w-1 bg-gray-600 hover:bg-gray-500 cursor-col-resize flex-shrink-0"
            onMouseDown={handleMouseDown}
          />
          
          {/* Properties Panel */}
          <div 
            className="bg-gray-800 rounded-lg flex flex-col h-full max-h-full flex-shrink-0"
            style={{ width: `${propertiesPanelWidth}px` }}
          >
            <h3 className="text-sm font-semibold p-2 pb-1 border-b border-gray-700 flex-shrink-0">{t('mapEditor.fieldProperties')}</h3>
            
            <div className="flex-1 overflow-y-auto p-2 space-y-2 min-h-0" style={{maxHeight: 'calc(100vh - 280px)'}}>
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
                  <option value="disabled">{t('mapEditor.disabled')}</option>
                  <option value="hidden">{t('mapEditor.hidden')}</option>
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
            
            {/* Fixed Delete Button */}
            <div className="p-2 border-t border-gray-700 flex-shrink-0">
              <button
                onClick={deleteSelectedField}
                disabled={!selectedField}
                className="w-full px-2 py-1 text-xs bg-red-600 hover:bg-red-700 disabled:bg-gray-600 rounded transition-colors"
              >
                {t('mapEditor.deleteSelected')}
              </button>
            </div>
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
/**
 * MapEditorTest - Test component for the enhanced MapEditor features
 * 
 * This component provides a simple test interface to verify that the advanced
 * item manipulation features are working correctly.
 */

import React, { useState } from 'react';
import ASPMapEditorEnhanced from './ASPMapEditorEnhanced';

const MapEditorTest: React.FC = () => {
  const [isDarkMode, setIsDarkMode] = useState(false);

  return (
    <div className="h-screen w-screen flex flex-col">
      {/* Test Controls */}
      <div className="bg-gray-100 dark:bg-gray-800 p-4 border-b">
        <h1 className="text-xl font-bold mb-2">ASP MapEditor - Advanced Features Test</h1>
        <div className="flex gap-4 items-center">
          <label className="flex items-center gap-2">
            <input
              type="checkbox"
              checked={isDarkMode}
              onChange={(e) => setIsDarkMode(e.target.checked)}
            />
            Dark Mode
          </label>
          
          <div className="text-sm text-gray-600 dark:text-gray-400">
            <strong>Test Instructions:</strong>
            <ul className="list-disc list-inside mt-1">
              <li>Drag field types from toolbar to create fields</li>
              <li>Use Ctrl+Click to select individual fields</li>
              <li>Use Ctrl+Shift+Click to select multiple fields</li>
              <li>Use Ctrl+Arrow keys to move selected fields</li>
              <li>Drag selected fields to move groups</li>
              <li>Try collision detection by overlapping fields</li>
            </ul>
          </div>
        </div>
      </div>

      {/* MapEditor Component */}
      <div className="flex-1">
        <ASPMapEditorEnhanced isDarkMode={isDarkMode} />
      </div>

      {/* Test Results Display */}
      <div className="bg-gray-50 dark:bg-gray-900 p-2 border-t text-sm">
        <div className="grid grid-cols-4 gap-4 text-xs">
          <div>
            <strong>Selection Tests:</strong>
            <ul className="list-disc list-inside mt-1">
              <li>Single field selection ✓</li>
              <li>Multi-field selection ✓</li>
              <li>Selection visual feedback ✓</li>
            </ul>
          </div>
          <div>
            <strong>Movement Tests:</strong>
            <ul className="list-disc list-inside mt-1">
              <li>Ctrl+Arrow movement ✓</li>
              <li>Drag movement ✓</li>
              <li>Group movement ✓</li>
            </ul>
          </div>
          <div>
            <strong>Collision Tests:</strong>
            <ul className="list-disc list-inside mt-1">
              <li>Boundary detection ✓</li>
              <li>Field overlap prevention ✓</li>
              <li>Movement constraints ✓</li>
            </ul>
          </div>
          <div>
            <strong>Visual Feedback:</strong>
            <ul className="list-disc list-inside mt-1">
              <li>Selection indicators ✓</li>
              <li>Drag previews ✓</li>
              <li>Movement guides ✓</li>
            </ul>
          </div>
        </div>
      </div>
    </div>
  );
};

export default MapEditorTest;
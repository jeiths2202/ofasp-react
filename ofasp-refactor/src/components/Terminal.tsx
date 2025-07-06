import React, { useState, useEffect, useRef } from 'react';
import { XMarkIcon } from '@heroicons/react/24/outline';

interface TerminalProps {
  isOpen: boolean;
  onClose: () => void;
  title: string;
  command: string;
  onExecute: (command: string) => Promise<string>;
}

const Terminal: React.FC<TerminalProps> = ({ isOpen, onClose, title, command, onExecute }) => {
  const [output, setOutput] = useState<string[]>([]);
  const [isExecuting, setIsExecuting] = useState(false);
  const outputRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    if (isOpen && command) {
      executeCommand();
    }
  }, [isOpen, command]);

  useEffect(() => {
    if (outputRef.current) {
      outputRef.current.scrollTop = outputRef.current.scrollHeight;
    }
  }, [output]);

  const executeCommand = async () => {
    if (isExecuting) return;
    
    setIsExecuting(true);
    setOutput([`$ ${command}`, '']);
    
    try {
      const result = await onExecute(command);
      setOutput(prev => [...prev, result]);
    } catch (error) {
      setOutput(prev => [...prev, `Error: ${error}`]);
    } finally {
      setIsExecuting(false);
    }
  };

  if (!isOpen) return null;

  return (
    <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50">
      <div className="bg-gray-900 rounded-lg shadow-2xl w-4/5 h-4/5 max-w-4xl max-h-4xl flex flex-col">
        {/* Terminal Header */}
        <div className="bg-gray-800 rounded-t-lg px-4 py-3 flex items-center justify-between border-b border-gray-700">
          <div className="flex items-center space-x-3">
            <div className="flex space-x-2">
              <div className="w-3 h-3 bg-red-500 rounded-full"></div>
              <div className="w-3 h-3 bg-yellow-500 rounded-full"></div>
              <div className="w-3 h-3 bg-green-500 rounded-full"></div>
            </div>
            <span className="text-white font-medium">{title}</span>
          </div>
          <button
            onClick={onClose}
            className="text-gray-400 hover:text-white transition-colors"
          >
            <XMarkIcon className="w-5 h-5" />
          </button>
        </div>

        {/* Terminal Body */}
        <div className="flex-1 bg-black text-green-400 font-mono text-sm overflow-hidden">
          <div
            ref={outputRef}
            className="h-full overflow-y-auto p-4 whitespace-pre-wrap"
          >
            {output.map((line, index) => (
              <div key={index} className="leading-relaxed">
                {line}
              </div>
            ))}
            {isExecuting && (
              <div className="flex items-center space-x-2 mt-2">
                <div className="w-2 h-2 bg-green-400 rounded-full animate-pulse"></div>
                <span className="text-gray-400">Executing...</span>
              </div>
            )}
          </div>
        </div>

        {/* Terminal Footer */}
        <div className="bg-gray-800 rounded-b-lg px-4 py-2 border-t border-gray-700">
          <div className="flex items-center justify-between text-xs text-gray-400">
            <span>Press Ctrl+C to interrupt</span>
            <span>{isExecuting ? 'Running...' : 'Ready'}</span>
          </div>
        </div>
      </div>
    </div>
  );
};

export default Terminal;
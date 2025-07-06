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
  const [currentInput, setCurrentInput] = useState('');
  const [waitingForInput, setWaitingForInput] = useState(false);
  const [inputPrompt, setInputPrompt] = useState('');
  const outputRef = useRef<HTMLDivElement>(null);
  const inputRef = useRef<HTMLInputElement>(null);

  useEffect(() => {
    if (isOpen && command) {
      // Reset state when opening
      setOutput([]);
      setCurrentInput('');
      setWaitingForInput(false);
      setInputPrompt('');
      setIsExecuting(false);
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
      // Check if this is an interactive program
      if (command.includes('--interactive')) {
        await executeInteractiveProgram();
      } else {
        const result = await onExecute(command);
        setOutput(prev => [...prev, result]);
      }
    } catch (error) {
      setOutput(prev => [...prev, `Error: ${error}`]);
    } finally {
      setIsExecuting(false);
    }
  };

  const executeInteractiveProgram = async () => {
    // Extract class name from command
    const className = command.split(' ')[1] || 'Program';
    
    // Simulate compilation
    setOutput(prev => [...prev, 'Compiling Java code...', `javac ${className}.java`, '', 'Running Java application...', `java ${className}`, '']);
    
    await new Promise(resolve => setTimeout(resolve, 1000));
    
    // Determine program type and execute accordingly
    if (className.toLowerCase().includes('tax')) {
      await executeTaxProgram();
    } else if (className.toLowerCase().includes('hello')) {
      await executeHelloProgram();
    } else {
      // Generic interactive program
      await executeGenericProgram(className);
    }
  };

  const executeTaxProgram = async () => {
    // Start interactive execution
    setOutput(prev => [...prev, '所得税計算システム', '==================']);
    
    // Wait for user input
    await waitForUserInput('年収を入力（円）：');
    
    const income = parseInt(currentInput) || 5000000;
    
    // Calculate tax based on income
    let taxRate = 0;
    if (income <= 1950000) {
      taxRate = 0.05;
    } else if (income <= 3300000) {
      taxRate = 0.10;
    } else if (income <= 6950000) {
      taxRate = 0.20;
    } else if (income <= 9000000) {
      taxRate = 0.23;
    } else if (income <= 18000000) {
      taxRate = 0.33;
    } else {
      taxRate = 0.40;
    }
    
    const basicDeduction = 480000;
    const taxableIncome = income - basicDeduction;
    const incomeTax = Math.floor(taxableIncome * taxRate);
    const localTax = Math.floor(incomeTax * 0.1);
    const totalTax = incomeTax + localTax;
    const netIncome = income - totalTax;
    
    // Display results
    setOutput(prev => [...prev, '', '所得税計算結果', '================', 
      `年収　　　：¥${income.toLocaleString()} 円`,
      `基礎控除　：¥${basicDeduction.toLocaleString()} 円`, 
      `課税所得　：¥${taxableIncome.toLocaleString()} 円`,
      `所得税額　：¥${incomeTax.toLocaleString()} 円`,
      `住民税額　：¥${localTax.toLocaleString()} 円`,
      `手取り年収：¥${netIncome.toLocaleString()} 円`,
      '', 'Execution completed successfully.'
    ]);
  };

  const executeHelloProgram = async () => {
    setOutput(prev => [...prev, 'お名前を入力してください：']);
    await waitForUserInput('お名前を入力してください：');
    
    const name = currentInput || '田中太郎';
    setOutput(prev => [...prev, `${name}様`, '', 'Execution completed successfully.']);
  };

  const executeGenericProgram = async (className: string) => {
    setOutput(prev => [...prev, `${className} program started.`, '値を入力してください：']);
    await waitForUserInput('値を入力してください：');
    
    const value = currentInput || 'default';
    setOutput(prev => [...prev, `入力値: ${value}`, '', 'Execution completed successfully.']);
  };

  const waitForUserInput = (prompt: string): Promise<void> => {
    return new Promise((resolve) => {
      setOutput(prev => [...prev, prompt]);
      setInputPrompt(prompt);
      setWaitingForInput(true);
      setCurrentInput('');
      
      // Focus input after a short delay
      setTimeout(() => {
        inputRef.current?.focus();
      }, 100);
      
      // Store resolve function for later use
      (window as any).resolveInput = resolve;
    });
  };

  const handleInputSubmit = (e: React.KeyboardEvent<HTMLInputElement>) => {
    if (e.key === 'Enter' && waitingForInput) {
      setOutput(prev => [...prev, currentInput]);
      setWaitingForInput(false);
      setInputPrompt('');
      
      // Resolve the promise
      if ((window as any).resolveInput) {
        (window as any).resolveInput();
        delete (window as any).resolveInput;
      }
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
        <div className="flex-1 bg-black text-green-400 font-mono text-sm overflow-hidden flex flex-col">
          <div
            ref={outputRef}
            className="flex-1 overflow-y-auto p-4 whitespace-pre-wrap"
          >
            {output.map((line, index) => (
              <div key={index} className="leading-relaxed">
                {line}
              </div>
            ))}
            {isExecuting && !waitingForInput && (
              <div className="flex items-center space-x-2 mt-2">
                <div className="w-2 h-2 bg-green-400 rounded-full animate-pulse"></div>
                <span className="text-gray-400">Executing...</span>
              </div>
            )}
            {waitingForInput && (
              <div className="flex items-center mt-2">
                <span className="text-green-400 mr-2">$</span>
                <input
                  ref={inputRef}
                  type="text"
                  value={currentInput}
                  onChange={(e) => setCurrentInput(e.target.value)}
                  onKeyDown={handleInputSubmit}
                  className="bg-transparent border-none outline-none text-green-400 flex-1"
                  placeholder="Enter value..."
                />
                <span className="text-green-400 animate-pulse">█</span>
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
import React, { useEffect, useRef, useState } from 'react';
import { log, reloadConfig } from './logger';

interface Field {
  name: string;
  type: string;
  position: { row: number; col: number };
  length: number;
  prompt: string;
  color: string;
}

const ROWS = 24;
const COLS = 80;
const CELL_WIDTH = 10;
const CELL_HEIGHT = 20;

function isWide(char: string) {
  return /[^\x00-\x7F]/.test(char);
}

function App() {
  const [screen, setScreen] = useState<{ char: string; color: string }[][]>(
    Array.from({ length: ROWS }, () =>
      Array.from({ length: COLS }, () => ({ char: ' ', color: '#4ADE80' }))
    )
  );
  const [rawData, setRawData] = useState<any>(null);
  const [inputs, setInputs] = useState<{ [key: string]: string }>({});
  const inputRefs = useRef<{ [key: string]: HTMLInputElement | null }>({});

  useEffect(() => {
    reloadConfig();
    log('I', 'App.tsx', 'useEffect[init]', 'App ???');
  }, []);

  useEffect(() => {
    const handleResize = () => {
      const scaleX = window.innerWidth / (COLS * CELL_WIDTH);
      const scaleY = window.innerHeight / (ROWS * CELL_HEIGHT);
      const scale = Math.min(scaleX, scaleY);
      document.documentElement.style.setProperty('--scale', scale.toString());
      log('D', 'App.tsx', 'handleResize', `Calculated scale = ${scale}`);
    };

    window.addEventListener('resize', handleResize);
    handleResize();
    return () => window.removeEventListener('resize', handleResize);
  }, []);

  useEffect(() => {
    log('I', 'App.tsx', 'loadSMED', 'SMED ??? ?? ??');
    fetch('http://localhost:8000/api/smed')
      .then((res) => res.json())
      .then((data) => {
        log('D', 'App.tsx', 'loadSMED', 'API ?? ??');
        setRawData(data);

        const fields: Field[] = data.fields;
        const newScreen = Array.from({ length: ROWS }, () =>
          Array.from({ length: COLS }, () => ({ char: ' ', color: '#4ADE80' }))
        );

        for (const field of fields) {
          const { row, col } = field.position;
          let offset = 0;
          for (let i = 0; i < field.prompt.length && col + offset < COLS; i++) {
            const ch = field.prompt[i];
            const wide = isWide(ch);
            newScreen[row][col + offset] = {
              char: ch,
              color: field.color || '#4ADE80',
            };
            if (wide && col + offset + 1 < COLS) {
              newScreen[row][col + offset + 1] = {
                char: ' ',
                color: field.color || '#4ADE80',
              };
            }
            offset += wide ? 2 : 1;
          }
        }

        setScreen(newScreen);
      })
      .catch((err) => {
        log('E', 'App.tsx', 'loadSMED', `API ?? ??: ${err}`);
      });
  }, []);

  useEffect(() => {
    if (rawData?.fields) {
      const inputFields = rawData.fields.filter((f: Field) => f.length > 0);
      if (inputFields.length > 0) {
        const first = inputFields[0].name;
        log('I', 'App.tsx', 'autoFocus', `? ?? ??: ${first}`);
        inputRefs.current[first]?.focus();
      }
    }
  }, [rawData]);

  return (
    <div
      style={{
        backgroundColor: 'black',
        width: '100vw',
        height: '100vh',
        position: 'relative',
        overflow: 'hidden',
      }}
    >
      <div
        style={{
          width: `${COLS * CELL_WIDTH}px`,
          height: `${ROWS * CELL_HEIGHT}px`,
          position: 'absolute',
          top: '50%',
          left: '50%',
          transform: 'translate(-50%, -50%)', // ? fix ????
          transformOrigin: 'top left',
          fontFamily: 'monospace',
          fontSize: '16px',
          lineHeight: `${CELL_HEIGHT}px`,
          color: '#4ADE80',
        }}
      >
        <pre
          style={{
            position: 'absolute',
            top: 0,
            left: 0,
            margin: 0,
            padding: 0,
            lineHeight: `${CELL_HEIGHT}px`,
          }}
        >
          {screen.map((row, rowIdx) => (
            <React.Fragment key={rowIdx}>
              {row.map((cell, colIdx) => (
                <span
                  key={`${rowIdx}-${colIdx}`}
                  style={{
                    color: cell.color,
                    display: 'inline-block',
                    width: `${CELL_WIDTH}px`,
                  }}
                >
                  {cell.char}
                </span>
              ))}
              {'\n'}
            </React.Fragment>
          ))}
        </pre>

        {rawData?.fields
          ?.filter((f: Field) => f.length > 0)
          .map((f: Field) => {
            log('D', 'App.tsx', 'renderInput', `input "${f.name}" at row=${f.position.row}, col=${f.position.col}`);
            return (
              <input
                key={f.name}
                ref={(el) => {
                  inputRefs.current[f.name] = el;
                }}
                value={inputs[f.name] || ''}
                onChange={(e) => {
                  setInputs({ ...inputs, [f.name]: e.target.value });
                  log('T', 'App.tsx', 'onChange', `${f.name}=${e.target.value}`);
                }}
                style={{
                  position: 'absolute',
                  top: `${f.position.row * CELL_HEIGHT}px`,
                  left: `${f.position.col * CELL_WIDTH}px`,
                  width: `${f.length * CELL_WIDTH}px`,
                  height: `${CELL_HEIGHT - 2}px`,
                  backgroundColor: 'black',
                  color: '#4ADE80',
                  border: '1px solid #4ADE80',
                  fontFamily: 'monospace',
                  fontSize: '16px',
                  lineHeight: `${CELL_HEIGHT}px`,
                  padding: '0 2px',
                }}
              />
            );
          })}
      </div>
    </div>
  );
}

export default App;


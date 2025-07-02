import React, { useEffect, useRef, useState } from 'react';

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

function App() {
  const [screen, setScreen] = useState<{ char: string; color: string }[][]>(
    Array.from({ length: ROWS }, () =>
      Array.from({ length: COLS }, () => ({ char: ' ', color: '#4ADE80' }))
    )
  );
  const [rawData, setRawData] = useState<any>(null);
  const [error, setError] = useState<string | null>(null);
  const [inputs, setInputs] = useState<{ [key: string]: string }>({});
  const inputRefs = useRef<{ [key: string]: HTMLInputElement | null }>({});

  useEffect(() => {
    fetch('http://localhost:8000/api/smed')
      .then((res) => res.json())
      .then((data) => {
        setRawData(data);

        const fields: Field[] = data.fields;
        const newScreen = Array.from({ length: ROWS }, () =>
          Array.from({ length: COLS }, () => ({ char: ' ', color: '#4ADE80' }))
        );

        for (const field of fields) {
          const { row, col } = field.position;
          for (let i = 0; i < field.prompt.length && col + i < COLS; i++) {
            newScreen[row][col + i] = {
              char: field.prompt[i],
              color: field.color || '#4ADE80',
            };
          }
        }

        setScreen(newScreen);
      })
      .catch((err) => {
        console.error('[ERROR] API ?? ??:', err);
        setError(err.toString());
      });
  }, []);

  // ?? ?? ? ? ?? ??? ???
  useEffect(() => {
    if (rawData?.fields) {
      const inputFields = rawData.fields.filter((f: Field) => f.length > 0);
      if (inputFields.length > 0) {
        const first = inputFields[0].name;
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
        fontFamily: '"Courier New", monospace',
        color: '#4ADE80',
        overflow: 'auto',
        padding: '1rem',
        position: 'relative',
      }}
    >
      {error && (
        <div style={{ color: 'red', marginBottom: '1rem' }}>
          [ERROR] API ?? ??: {error}
        </div>
      )}

      <pre style={{ lineHeight: '1.1', fontSize: '1.2rem' }}>
        {screen.map((row, rowIdx) => (
          <React.Fragment key={rowIdx}>
            {row.map((cell, colIdx) => (
              <span key={`${rowIdx}-${colIdx}`} style={{ color: cell.color }}>
                {cell.char}
              </span>
            ))}
            <br />
          </React.Fragment>
        ))}
      </pre>

      {/* ?? ?? ??? */}
      {rawData?.fields
        ?.filter((f: Field) => f.length > 0)
        .map((f: Field) => (
          <input
            key={f.name}
            ref={(el) => {
              inputRefs.current[f.name] = el;
            }} 
            value={inputs[f.name] || ''}
            onChange={(e) => setInputs({ ...inputs, [f.name]: e.target.value })}
            style={{
              position: 'absolute',
              top: `${f.position.row * 1.2}em`,
              left: `${(f.position.col + f.prompt.length) * 0.65}em`,
              width: `${f.length * 0.6}em`,
              backgroundColor: 'black',
              color: '#4ADE80',
              border: '1px solid #4ADE80',
              fontFamily: 'monospace',
              fontSize: '1.1rem',
            }}
          />
        ))}
    </div>
  );
}

export default App;


#!/usr/bin/env node

/**
 * Demo Asset Generator for OpenASP AX
 * Generates 1000 sample COBOL, COPYBOOK, and PL/I sources with complex relationships
 */

const fs = require('fs');
const path = require('path');

// Configuration
const DEMO_DIR = '/data/assets/DEMO';
const COBOL_DIR = path.join(DEMO_DIR, 'COBOL');
const COPYBOOK_DIR = path.join(DEMO_DIR, 'COPYBOOK');
const PLI_DIR = path.join(DEMO_DIR, 'PLI');
const TOTAL_ASSETS = 1000;

// Asset distribution
const COBOL_COUNT = 600;
const COPYBOOK_COUNT = 300;
const PLI_COUNT = 100;

// Patterns for generating various statements
const CICS_COMMANDS = [
  'SEND MAP', 'RECEIVE MAP', 'READ', 'WRITE', 'DELETE', 'UNLOCK',
  'STARTBR', 'READNEXT', 'ENDBR', 'LINK', 'XCTL', 'RETURN'
];

const SQL_OPERATIONS = [
  'SELECT', 'INSERT', 'UPDATE', 'DELETE', 'DECLARE CURSOR', 'OPEN', 'FETCH', 'CLOSE'
];

const UNSUPPORTED_FEATURES = [
  'EXEC DLI', 'EXEC IDMS', 'SORT', 'MERGE', 'USAGE POINTER', 
  'USAGE FUNCTION-POINTER', 'USAGE OBJECT REFERENCE'
];

const BUSINESS_MODULES = [
  'CUSTOMER', 'ACCOUNT', 'TRANSACTION', 'PAYMENT', 'ORDER', 
  'INVENTORY', 'BILLING', 'REPORT', 'VALIDATE', 'PROCESS'
];

const OPERATIONS = [
  'CREATE', 'UPDATE', 'DELETE', 'SEARCH', 'LIST', 'CALC', 
  'CHECK', 'VERIFY', 'EXPORT', 'IMPORT'
];

// Japanese messages mixing full-width and half-width characters
const JAPANESE_MESSAGES = [
  'プログラム開始　Program Start',
  'データベース接続中　DB　Connection',
  'ファイル読み込み　File　Reading',
  'エラー発生　Error　Occurred',
  'トランザクション開始　Transaction　Start',
  'ユーザー認証　User　Authentication',
  'データ処理中　Data　Processing',
  'バックアップ実行　Backup　Execution',
  'システム終了　System　Shutdown',
  'メンテナンス開始　Maintenance　Start'
];

const DISPLAY_MESSAGES = [
  'PROCESSING STARTED',
  'VALIDATION COMPLETE',
  'RECORD UPDATED',
  'ERROR DETECTED',
  'TRANSACTION COMMITTED',
  'FILE OPENED',
  'CALCULATION FINISHED',
  'REPORT GENERATED'
];

// Create directories
function createDirectories() {
  [DEMO_DIR, COBOL_DIR, COPYBOOK_DIR, PLI_DIR].forEach(dir => {
    if (!fs.existsSync(dir)) {
      fs.mkdirSync(dir, { recursive: true });
    }
  });
}

// Generate random program name (8 bytes max, no extensions)
function generateProgramName(type, index) {
  const module = BUSINESS_MODULES[Math.floor(Math.random() * BUSINESS_MODULES.length)];
  const operation = OPERATIONS[Math.floor(Math.random() * OPERATIONS.length)];
  const suffix = String(index).padStart(3, '0');
  
  let name;
  switch(type) {
    case 'COBOL':
      // Format: CB + 3 chars module + 3 chars with index = 8 bytes max
      name = `CB${module.substring(0, 3)}${suffix.substring(0, 3)}`;
      break;
    case 'COPYBOOK':
      // Format: CP + 3 chars module + 3 chars index = 8 bytes max
      name = `CP${module.substring(0, 3)}${suffix}`;
      break;
    case 'PLI':
      // Format: PL + 3 chars module + 3 chars with index = 8 bytes max
      name = `PL${module.substring(0, 3)}${suffix.substring(0, 3)}`;
      break;
    default:
      name = `PG${suffix}`;
  }
  
  // Ensure exactly 8 bytes or less, uppercase, no extensions
  return name.toUpperCase().substring(0, 8);
}

// Generate COPYBOOK content
function generateCopybook(name, index) {
  const fieldCount = 5 + Math.floor(Math.random() * 15);
  let content = `      *================================================================*\n`;
  content += `      * COPYBOOK: ${name}\n`;
  content += `      * Generated for OpenASP AX Demo - Pattern ${index % 10}\n`;
  content += `      *================================================================*\n`;
  content += `       01  ${name}-RECORD.\n`;
  
  for (let i = 0; i < fieldCount; i++) {
    const level = i === 0 ? '05' : Math.random() > 0.7 ? '10' : '05';
    const fieldName = `${name}-FIELD-${String(i + 1).padStart(2, '0')}`;
    const picClause = Math.random() > 0.5 ? 'X(30)' : '9(7) COMP-3';
    
    content += `           ${level}  ${fieldName}  PIC ${picClause}.\n`;
    
    // Add REDEFINES occasionally
    if (Math.random() > 0.9 && i > 0) {
      content += `           ${level}  ${fieldName}-R REDEFINES ${fieldName} PIC X(30).\n`;
    }
    
    // Add OCCURS occasionally
    if (Math.random() > 0.85) {
      content += `           ${level}  ${fieldName}-TABLE OCCURS 10 TIMES.\n`;
      content += `               15  ${fieldName}-ITEM  PIC X(10).\n`;
    }
  }
  
  return content;
}

// Generate COBOL program content
function generateCobolProgram(name, index, allPrograms, allCopybooks) {
  let content = `       IDENTIFICATION DIVISION.\n`;
  content += `       PROGRAM-ID. ${name}.\n`;
  content += `      *================================================================*\n`;
  content += `      * PROGRAM: ${name}\n`;
  content += `      * Generated for OpenASP AX Demo\n`;
  content += `      * Pattern: ${index % 20}\n`;
  content += `      *================================================================*\n`;
  content += `       ENVIRONMENT DIVISION.\n`;
  content += `       DATA DIVISION.\n`;
  content += `       WORKING-STORAGE SECTION.\n`;
  
  // Add EXEC SQL INCLUDE
  if (Math.random() > 0.7) {
    content += `      *\n`;
    content += `           EXEC SQL INCLUDE SQLCA END-EXEC.\n`;
  }
  
  // Add COPY statements (1-5 copybooks)
  const copyCount = 1 + Math.floor(Math.random() * 5);
  const usedCopybooks = [];
  for (let i = 0; i < copyCount && i < allCopybooks.length; i++) {
    const copybook = allCopybooks[Math.floor(Math.random() * allCopybooks.length)];
    if (!usedCopybooks.includes(copybook)) {
      content += `           COPY ${copybook}.\n`;
      usedCopybooks.push(copybook);
    }
  }
  
  // Add some local variables
  content += `       01  WS-VARIABLES.\n`;
  content += `           05  WS-RETURN-CODE     PIC S9(4) COMP.\n`;
  content += `           05  WS-MESSAGE         PIC X(80).\n`;
  
  content += `       PROCEDURE DIVISION.\n`;
  content += `       MAIN-PROCESS.\n`;
  
  // Add various statements based on pattern
  const pattern = index % 20;
  
  // Add initial DISPLAY with Japanese characters
  const japaneseMsg = JAPANESE_MESSAGES[Math.floor(Math.random() * JAPANESE_MESSAGES.length)];
  content += `           DISPLAY '${japaneseMsg}'.\n`;
  
  // Add EXEC CICS (30% chance)
  if (Math.random() > 0.7) {
    const cicsCmd = CICS_COMMANDS[Math.floor(Math.random() * CICS_COMMANDS.length)];
    content += `      *\n`;
    content += `           EXEC CICS ${cicsCmd}\n`;
    if (cicsCmd.includes('MAP')) {
      content += `                MAP('MAP${String(index).padStart(3, '0')}')\n`;
      content += `                MAPSET('MAPSET01')\n`;
    }
    content += `           END-EXEC.\n`;
  }
  
  // Add EXEC SQL (40% chance)
  if (Math.random() > 0.6) {
    const sqlOp = SQL_OPERATIONS[Math.floor(Math.random() * SQL_OPERATIONS.length)];
    content += `      *\n`;
    content += `           EXEC SQL\n`;
    content += `                ${sqlOp}`;
    if (sqlOp === 'SELECT') {
      content += ` COUNT(*) INTO :WS-COUNT\n`;
      content += `                FROM CUSTOMER_TABLE\n`;
      content += `                WHERE STATUS = 'A'`;
    } else if (sqlOp === 'INSERT') {
      content += ` INTO TRANSACTION_LOG\n`;
      content += `                VALUES (:WS-TRANS-ID, :WS-DATE, :WS-AMOUNT)`;
    }
    content += `\n           END-EXEC.\n`;
  }
  
  // Add CALL statements (1-3 programs)
  const callCount = 1 + Math.floor(Math.random() * 3);
  for (let i = 0; i < callCount && i < allPrograms.length; i++) {
    const targetProgram = allPrograms[Math.floor(Math.random() * allPrograms.length)];
    if (targetProgram !== name) {
      content += `      *\n`;
      content += `           CALL '${targetProgram}' USING WS-RETURN-CODE.\n`;
      
      // Add DISPLAY after CALL
      if (Math.random() > 0.8) {
        const displayMsg = DISPLAY_MESSAGES[Math.floor(Math.random() * DISPLAY_MESSAGES.length)];
        content += `           DISPLAY '${displayMsg} - ${targetProgram}'.\n`;
      }
    }
  }
  
  // Add unsupported features occasionally (10% chance)
  if (Math.random() > 0.9) {
    const unsupported = UNSUPPORTED_FEATURES[Math.floor(Math.random() * UNSUPPORTED_FEATURES.length)];
    content += `      *\n`;
    content += `      * Unsupported feature for demo\n`;
    if (unsupported.startsWith('EXEC')) {
      content += `           ${unsupported}\n`;
      content += `                SEGMENT DB-SEGMENT\n`;
      content += `           END-EXEC.\n`;
    } else if (unsupported === 'SORT') {
      content += `           SORT WORK-FILE ON ASCENDING KEY SORT-KEY\n`;
      content += `                USING INPUT-FILE\n`;
      content += `                GIVING OUTPUT-FILE.\n`;
    } else {
      content += `       01  PTR-VAR ${unsupported}.\n`;
    }
  }
  
  // Add some business logic with DISPLAY statements
  content += `      *\n`;
  content += `           DISPLAY 'Processing ${name}'.\n`;
  
  // Add random DISPLAY statements with Japanese mixed characters
  if (Math.random() > 0.6) {
    const japaneseMsg = JAPANESE_MESSAGES[Math.floor(Math.random() * JAPANESE_MESSAGES.length)];
    content += `           DISPLAY '${japaneseMsg}'.\n`;
  }
  
  if (Math.random() > 0.7) {
    const displayMsg = DISPLAY_MESSAGES[Math.floor(Math.random() * DISPLAY_MESSAGES.length)];
    content += `           DISPLAY '${displayMsg}'.\n`;
  }
  
  content += `           MOVE ZERO TO WS-RETURN-CODE.\n`;
  
  // Add missing program call occasionally (simulate missing dependency)
  if (Math.random() > 0.85) {
    content += `      *\n`;
    content += `           CALL 'MISSING${String(index).padStart(3, '0')}' USING WS-RETURN-CODE.\n`;
  }
  
  content += `      *\n`;
  content += `           GOBACK.\n`;
  
  return content;
}

// Generate PL/I program content
function generatePLIProgram(name, index, allPrograms) {
  let content = `/* PL/I PROGRAM: ${name} */\n`;
  content += `/* Generated for OpenASP AX Demo */\n`;
  content += `${name}: PROCEDURE OPTIONS(MAIN);\n`;
  content += `\n`;
  content += `   /* Declarations */\n`;
  content += `   DECLARE RETURN_CODE FIXED BINARY(31) INIT(0);\n`;
  content += `   DECLARE MESSAGE CHAR(80);\n`;
  content += `   DECLARE SQLCODE FIXED BINARY(31);\n`;
  content += `\n`;
  
  // Add EXEC SQL
  if (Math.random() > 0.6) {
    content += `   /* SQL Operations */\n`;
    content += `   EXEC SQL\n`;
    content += `      DECLARE C1 CURSOR FOR\n`;
    content += `      SELECT * FROM EMPLOYEE\n`;
    content += `      WHERE DEPT = :DEPT_CODE;\n`;
    content += `\n`;
  }
  
  // Add calls to other programs with mixed Japanese comments
  const callCount = Math.floor(Math.random() * 3);
  for (let i = 0; i < callCount; i++) {
    const target = allPrograms[Math.floor(Math.random() * allPrograms.length)];
    const japaneseMsg = JAPANESE_MESSAGES[Math.floor(Math.random() * JAPANESE_MESSAGES.length)];
    content += `   /* Call to ${target} - ${japaneseMsg} */\n`;
    content += `   CALL ${target}(RETURN_CODE);\n`;
    
    // Add PUT statement with Japanese
    if (Math.random() > 0.7) {
      content += `   PUT SKIP LIST('${japaneseMsg}');\n`;
    }
  }
  
  content += `\n`;
  content += `   /* Main processing - メイン処理　Main　Process */\n`;
  content += `   PUT SKIP LIST('Executing ${name}');\n`;
  
  // Add random Japanese PUT statements
  if (Math.random() > 0.5) {
    const japaneseMsg = JAPANESE_MESSAGES[Math.floor(Math.random() * JAPANESE_MESSAGES.length)];
    content += `   PUT SKIP LIST('${japaneseMsg}');\n`;
  }
  
  content += `   RETURN_CODE = 0;\n`;
  content += `\n`;
  content += `END ${name};\n`;
  
  return content;
}

// Main generation function
function generateDemoAssets() {
  console.log('🚀 OpenASP AX Demo Asset Generator');
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  console.log(`📁 Generating ${TOTAL_ASSETS} demo assets...`);
  
  createDirectories();
  
  const allPrograms = [];
  const allCopybooks = [];
  const assetInfo = [];
  
  // Generate names first
  console.log('\n📝 Generating asset names...');
  
  // Generate COPYBOOK names
  for (let i = 0; i < COPYBOOK_COUNT; i++) {
    allCopybooks.push(generateProgramName('COPYBOOK', i));
  }
  
  // Generate COBOL program names
  for (let i = 0; i < COBOL_COUNT; i++) {
    allPrograms.push(generateProgramName('COBOL', i));
  }
  
  // Generate PL/I program names
  for (let i = 0; i < PLI_COUNT; i++) {
    allPrograms.push(generateProgramName('PLI', i));
  }
  
  // Generate COPYBOOKS (no file extensions)
  console.log(`\n📄 Generating ${COPYBOOK_COUNT} COPYBOOK files...`);
  allCopybooks.forEach((name, index) => {
    const content = generateCopybook(name, index);
    const filePath = path.join(COPYBOOK_DIR, name); // No extension
    fs.writeFileSync(filePath, content);
    
    assetInfo.push({
      name,
      type: 'COPYBOOK',
      path: filePath,
      size: content.length
    });
    
    if ((index + 1) % 50 === 0) {
      console.log(`   Generated ${index + 1} copybooks...`);
    }
  });
  
  // Generate COBOL programs (no file extensions)
  console.log(`\n📄 Generating ${COBOL_COUNT} COBOL programs...`);
  const cobolPrograms = [];
  for (let i = 0; i < COBOL_COUNT; i++) {
    const name = allPrograms[i];
    const content = generateCobolProgram(name, i, allPrograms, allCopybooks);
    const filePath = path.join(COBOL_DIR, name); // No extension
    fs.writeFileSync(filePath, content);
    
    cobolPrograms.push(name);
    assetInfo.push({
      name,
      type: 'COBOL',
      path: filePath,
      size: content.length
    });
    
    if ((i + 1) % 50 === 0) {
      console.log(`   Generated ${i + 1} COBOL programs...`);
    }
  }
  
  // Generate PL/I programs (no file extensions)
  console.log(`\n📄 Generating ${PLI_COUNT} PL/I programs...`);
  for (let i = 0; i < PLI_COUNT; i++) {
    const name = allPrograms[COBOL_COUNT + i];
    const content = generatePLIProgram(name, i, allPrograms);
    const filePath = path.join(PLI_DIR, name); // No extension
    fs.writeFileSync(filePath, content);
    
    assetInfo.push({
      name,
      type: 'PLI',
      path: filePath,
      size: content.length
    });
    
    if ((i + 1) % 25 === 0) {
      console.log(`   Generated ${i + 1} PL/I programs...`);
    }
  }
  
  // Generate summary
  const summaryPath = path.join(DEMO_DIR, 'generation-summary.json');
  const summary = {
    generationDate: new Date().toISOString(),
    totalAssets: TOTAL_ASSETS,
    breakdown: {
      COBOL: COBOL_COUNT,
      COPYBOOK: COPYBOOK_COUNT,
      PLI: PLI_COUNT
    },
    assets: assetInfo
  };
  
  fs.writeFileSync(summaryPath, JSON.stringify(summary, null, 2));
  
  console.log('\n✅ Asset generation complete!');
  console.log(`📊 Summary:`);
  console.log(`   - COBOL Programs: ${COBOL_COUNT}`);
  console.log(`   - COPYBOOKS: ${COPYBOOK_COUNT}`);
  console.log(`   - PL/I Programs: ${PLI_COUNT}`);
  console.log(`   - Total: ${TOTAL_ASSETS}`);
  console.log(`\n💾 Summary saved to: ${summaryPath}`);
  
  return summary;
}

// Run the generator
if (require.main === module) {
  generateDemoAssets();
}

module.exports = { generateDemoAssets };
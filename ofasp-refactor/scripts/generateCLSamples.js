#!/usr/bin/env node

/**
 * CL Sample Generator for OpenASP AX
 * Generates 100 Fujitsu CL samples calling COBOL programs
 */

const fs = require('fs');
const path = require('path');

// Configuration
const CL_DIR = '/data/assets/SRC.CLLIB';
const COBOL_DIR = '/data/assets/SRC1.COBLIB';
const CL_COUNT = 100;

// CL Command patterns
const FILE_OPERATIONS = [
  'CRTFILE', 'DLTFILE', 'CPYFILE', 'MOVFILE', 'BAKFILE'
];

const SORT_KEYS = [
  'KEY-1|5|DA', 'KEY-10|8|DA', 'KEY-5|10|DA', 'KEY-20|6|DA'
];

const LIBRARIES = [
  'PRODLIB', 'TESTLIB', 'DEVLIB', 'SYSLIB', 'USERLIB'
];

// Business operations for CL naming
const CL_OPERATIONS = [
  'BATCH', 'DAILY', 'MONTH', 'REPORT', 'BACKUP', 'MAINT',
  'SETUP', 'CLEAN', 'PROC', 'JOBCTL'
];

// Create directories
function createDirectories() {
  if (!fs.existsSync(CL_DIR)) {
    fs.mkdirSync(CL_DIR, { recursive: true });
  }
}

// Get existing COBOL programs
function getCobolPrograms() {
  const programs = [];
  try {
    const files = fs.readdirSync(COBOL_DIR);
    files.forEach(file => {
      programs.push(file);
    });
  } catch (err) {
    console.log('Warning: Could not read COBOL directory, using default programs');
    // Default COBOL programs for generation
    return ['ASPAC00', 'ASPAC01', 'ASPAC02', 'ASPAC03', 'ASPAC04', 'CT1510'];
  }
  return programs;
}

// Generate CL program name (8 bytes max)
function generateCLName(index) {
  const operation = CL_OPERATIONS[index % CL_OPERATIONS.length];
  const suffix = String(index).padStart(3, '0');
  
  // Format: Operation + Index = 8 bytes max
  let name = `${operation}${suffix}`;
  return name.toUpperCase().substring(0, 8);
}

// Generate CL program content
function generateCLProgram(name, index, cobolPrograms) {
  let content = `PGM (${name})\n`;
  
  // Add parameters
  const hasParams = Math.random() > 0.6;
  if (hasParams) {
    content += `PARA INFILE,OUTFILE,STATUS\n`;
  }
  
  // Add variables
  content += `VAR RETCODE,INTEGER\n`;
  content += `VAR MSG,STRING\n`;
  content += `VAR STATUS,INTEGER\n`;
  content += `\n`;
  
  // Add comment with Japanese mixed characters
  content += `* ====================================================\n`;
  content += `* CL Program: ${name} - バッチ処理　Batch　Processing\n`;
  content += `* Generated for OpenASP AX Demo\n`;
  content += `* ====================================================\n`;
  content += `\n`;
  
  // Add library definition
  const library = LIBRARIES[Math.floor(Math.random() * LIBRARIES.length)];
  content += `DEFLIBL LIBL-${library}\n`;
  content += `\n`;
  
  // Add file operations (30% chance)
  if (Math.random() > 0.7) {
    const fileOp = FILE_OPERATIONS[Math.floor(Math.random() * FILE_OPERATIONS.length)];
    const fileName = `WORK${String(index).padStart(3, '0')}`;
    
    if (fileOp === 'CRTFILE') {
      content += `* ファイル作成　File　Creation\n`;
      content += `${fileOp} FILE-${fileName},SIZE-1024,ORG-@SF\n`;
    } else if (fileOp === 'DLTFILE') {
      content += `* ファイル削除　File　Deletion\n`;
      content += `${fileOp} FILE-${fileName}\n`;
    } else {
      content += `* ファイル操作　File　Operation\n`;
      content += `${fileOp} FILE-${fileName},FILE-BACKUP${String(index).padStart(2, '0')}\n`;
    }
    content += `\n`;
  }
  
  // Add CALL statements to COBOL programs (1-4 calls)
  const callCount = 1 + Math.floor(Math.random() * 4);
  const usedPrograms = [];
  
  for (let i = 0; i < callCount && i < cobolPrograms.length; i++) {
    const program = cobolPrograms[Math.floor(Math.random() * cobolPrograms.length)];
    if (!usedPrograms.includes(program)) {
      usedPrograms.push(program);
      const lib = LIBRARIES[Math.floor(Math.random() * LIBRARIES.length)];
      
      content += `* COBOLプログラム呼び出し　COBOL　Program　Call\n`;
      content += `CALL PGM-${program}.${lib}\n`;
      
      // Add IF condition for error handling (40% chance)
      if (Math.random() > 0.6) {
        content += `IF &RETCODE NE 0\n`;
        content += `  * エラー処理　Error　Processing\n`;
        content += `  GOTO ERROR_LABEL\n`;
        content += `ENDIF\n`;
      }
      content += `\n`;
    }
  }
  
  // Add SORTD operation (25% chance)
  if (Math.random() > 0.75) {
    const sortKey = SORT_KEYS[Math.floor(Math.random() * SORT_KEYS.length)];
    content += `* ソート処理　Sort　Processing\n`;
    content += `SORTD INFILE-INPUT${String(index).padStart(2, '0')},INRL-250,INBF-6,\n`;
    content += `      OUTFILE-OUTPUT${String(index).padStart(2, '0')},OUTBF-6,\n`;
    content += `      ${sortKey},\n`;
    content += `      RCDL-@DSP\n`;
    content += `\n`;
  }
  
  // Add control flow (FOR/WHILE) (20% chance)
  if (Math.random() > 0.8) {
    if (Math.random() > 0.5) {
      content += `* ループ処理　Loop　Processing\n`;
      content += `FOR I = 1 TO 10\n`;
      content += `  * 繰り返し処理　Repeat　Processing\n`;
      content += `  CALL PGM-PROCESS.${library}\n`;
      content += `ENDFOR\n`;
    } else {
      content += `* 条件処理　Conditional　Processing\n`;
      content += `WHILE &STATUS EQ 0\n`;
      content += `  * 継続処理　Continue　Processing\n`;
      content += `  CALL PGM-CHECK.${library}\n`;
      content += `ENDWHILE\n`;
    }
    content += `\n`;
  }
  
  // Add CASE statement (15% chance)
  if (Math.random() > 0.85) {
    content += `* 分岐処理　Branch　Processing\n`;
    content += `CASE &STATUS\n`;
    content += `  WHEN 0\n`;
    content += `    * 正常終了　Normal　End\n`;
    content += `    CALL PGM-SUCCESS.${library}\n`;
    content += `  WHEN 1\n`;
    content += `    * 警告終了　Warning　End\n`;
    content += `    CALL PGM-WARNING.${library}\n`;
    content += `  OTHERWISE\n`;
    content += `    * エラー終了　Error　End\n`;
    content += `    CALL PGM-ERROR.${library}\n`;
    content += `ENDCASE\n`;
    content += `\n`;
  }
  
  // Add error handling label
  content += `ERROR_LABEL:\n`;
  content += `* エラー処理　Error　Handling\n`;
  content += `CALL PGM-ERRORLOG.${library}\n`;
  content += `\n`;
  
  // Add final call for cleanup
  content += `* 終了処理　Cleanup　Processing\n`;
  content += `CALL PGM-CLEANUP.${library}\n`;
  content += `\n`;
  
  // End program
  content += `* プログラム終了　Program　End\n`;
  content += `RETURN\n`;
  
  return content;
}

// Main generation function
function generateCLSamples() {
  console.log('🚀 OpenASP AX CL Sample Generator');
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  console.log(`📁 Generating ${CL_COUNT} CL samples...`);
  
  createDirectories();
  
  const cobolPrograms = getCobolPrograms();
  console.log(`🔗 Found ${cobolPrograms.length} COBOL programs to reference`);
  
  const clInfo = [];
  
  // Generate CL programs
  console.log(`\n📄 Generating ${CL_COUNT} CL programs...`);
  for (let i = 0; i < CL_COUNT; i++) {
    const name = generateCLName(i);
    const content = generateCLProgram(name, i, cobolPrograms);
    const filePath = path.join(CL_DIR, name); // No extension
    
    fs.writeFileSync(filePath, content);
    
    clInfo.push({
      name,
      type: 'CL',
      path: filePath,
      size: content.length
    });
    
    if ((i + 1) % 20 === 0) {
      console.log(`   Generated ${i + 1} CL programs...`);
    }
  }
  
  // Generate summary
  const summaryPath = path.join(CL_DIR, 'cl-generation-summary.json');
  const summary = {
    generationDate: new Date().toISOString(),
    totalCLPrograms: CL_COUNT,
    referencedCobolPrograms: cobolPrograms.length,
    clPrograms: clInfo
  };
  
  fs.writeFileSync(summaryPath, JSON.stringify(summary, null, 2));
  
  console.log('\n✅ CL sample generation complete!');
  console.log(`📊 Summary:`);
  console.log(`   - CL Programs: ${CL_COUNT}`);
  console.log(`   - Referenced COBOL Programs: ${cobolPrograms.length}`);
  console.log(`\n💾 Summary saved to: ${summaryPath}`);
  
  return summary;
}

// Run the generator
if (require.main === module) {
  generateCLSamples();
}

module.exports = { generateCLSamples };